//! Sound Processing Unit
//!
//! Most of the code is based on Mednafen's implementation

mod fifo;
mod fir;

use super::{cdrom, cpu, sync, AccessWidth, Addressable, CycleCount, Psx};
use fifo::DecoderFifo;
use std::ops::{Index, IndexMut};

const SPUSYNC: sync::SyncToken = sync::SyncToken::Spu;

/// Offset into the SPU internal ram
type RamIndex = u32;

pub struct Spu {
    /// RAM index, used for read/writes using CPU or DMA.
    ram_index: RamIndex,
    /// Write index in the capture buffers. There's only one index used for all 4 buffers at any
    /// given time
    capture_index: RamIndex,
    /// If the IRQ is enabled in the control register and the SPU memory is accessed at `irq_addr`
    /// (read *or* write) the interrupt is triggered.
    irq_addr: RamIndex,
    /// True if the interrupt has been triggered and not yet ack'ed
    irq: bool,
    /// Main volume, left
    main_volume_left: Volume,
    /// Main volume, right
    main_volume_right: Volume,
    /// The 24 individual voices
    voices: [Voice; 24],
    /// Which voices should be started (bitfield, one bit per voice)
    voice_start: u32,
    /// Which voices should be stopped (bitfield, one bit per voice)
    voice_stop: u32,
    /// Configures which voices output LFSR noise (bitfield, one bit per voice)
    voice_noise: u32,
    /// Configures which voices are fed to the reverberation module (bitfield, one bit per voice)
    voice_reverb: u32,
    /// Configures which voices are frequency modulated (bitfield, one bit per voice)
    voice_frequency_modulated: u32,
    /// Status bits, cleared on start, set to 1 when loop_end is reached (bitfield, one bit per
    /// voice)
    voice_looped: u32,
    /// Most of the SPU's register behave like a R/W RAM, so to simplify the emulation we just
    /// store most registers in a big buffer
    regs: [u16; 320],
    /// SPU internal RAM, 16bit wide
    ram: [u16; SPU_RAM_SIZE],
    /// Output audio buffer. Sent to the frontend after each frame, so should be large enough to
    /// store one frame worth of audio samples. Assuming a 50Hz refresh rate @ 44.1kHz that should
    /// be about ~1800 samples per frame at most.
    audio_buffer: [i16; 2048],
    /// Write pointer into the audio_buffer
    audio_buffer_index: u32,
}

impl Spu {
    pub fn new() -> Spu {
        Spu {
            ram_index: 0,
            capture_index: 0,
            irq_addr: 0,
            irq: false,
            main_volume_left: Volume::new(),
            main_volume_right: Volume::new(),
            voices: [
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
                Voice::new(),
            ],
            voice_start: 0,
            voice_stop: 0,
            voice_noise: 0,
            voice_reverb: 0,
            voice_frequency_modulated: 0,
            voice_looped: 0,
            regs: [0; 320],
            ram: [0; SPU_RAM_SIZE],
            audio_buffer: [0; 2048],
            audio_buffer_index: 0,
        }
    }

    /// Returns the value of the control register
    fn control(&self) -> u16 {
        self.regs[regmap::CONTROL]
    }

    /// True if the "SPU enable" bit is set in the control register
    fn enabled(&self) -> bool {
        self.control() & (1 << 15) != 0
    }

    fn irq_enabled(&self) -> bool {
        // No$ says that the bit 6 (IRQ9) is "only when bit15=1", I'm not sure what that means.
        // Mednafen doesn't appear to put any condition on the interrupt bit.
        self.control() & (1 << 6) != 0
    }

    /// True if the SPU is muted in the configuration register
    fn muted(&self) -> bool {
        self.control() & (1 << 14) == 0
    }

    /// True if the SPU plays the audio coming from the CD
    fn cd_audio_enabled(&self) -> bool {
        self.control() & 1 != 0
    }

    /// Update the status register
    fn update_status(&mut self) {
        let mut status = 0;

        status |= self.control() & 0x3f;
        status |= (self.irq as u16) << 6;

        // Not sure what that's about, copied straight from mednafen. `TRANSFER_CONTROL` is the
        // mystery register that mangles the memory writes if it's not set to 4 (cf. No$)
        if self.regs[regmap::TRANSFER_CONTROL] == 4 {
            // Bit set to true if the capture index targets the high half of the capture buffers
            let capture_high = self.capture_index & 0x100 != 0;

            status |= (capture_high as u16) << 11;
        }

        self.regs[regmap::STATUS] = status;
    }

    /// Returns true if `voice` is configured to output LFSR noise
    fn is_noise(&self, voice: u8) -> bool {
        self.voice_noise & (1 << voice) != 0
    }

    /// Returns true if frequency modulation is enabled for `voice`
    fn is_frequency_modulated(&self, voice: u8) -> bool {
        self.voice_frequency_modulated & (1 << voice) != 0
    }

    /// Returns true if voice should be started
    fn is_voice_started(&self, voice: u8) -> bool {
        self.voice_start & (1 << voice) != 0
    }

    /// Returns true if voice should be stopped
    fn is_voice_stopped(&self, voice: u8) -> bool {
        self.voice_stop & (1 << voice) != 0
    }

    /// Returns true if voice should be fed to the reverberation module
    fn is_voice_reverberated(&self, voice: u8) -> bool {
        self.voice_reverb & (1 << voice) != 0
    }
}

impl Index<u8> for Spu {
    type Output = Voice;

    fn index(&self, port: u8) -> &Self::Output {
        &self.voices[port as usize]
    }
}

impl IndexMut<u8> for Spu {
    fn index_mut(&mut self, port: u8) -> &mut Self::Output {
        &mut self.voices[port as usize]
    }
}

/// Run the SPU until it's caught up with the CPU
pub fn run(psx: &mut Psx) {
    let mut elapsed = sync::resync(psx, SPUSYNC);

    while elapsed >= SPU_FREQ_DIVIDER {
        elapsed -= SPU_FREQ_DIVIDER;
        run_cycle(psx);
    }

    // If we have some leftover cycles we can just return them to the synchronization module, we'll
    // get them back on the next call to resync
    sync::rewind(psx, SPUSYNC, elapsed);

    // For now force a sync at the next cycle
    sync::next_event(psx, SPUSYNC, SPU_FREQ_DIVIDER - elapsed);
}

/// Get the contents of the sample buffer
pub fn get_samples(psx: &mut Psx) -> &[i16] {
    let end = psx.spu.audio_buffer_index as usize;

    &psx.spu.audio_buffer[..end]
}

/// Clear the sample buffer
pub fn clear_samples(psx: &mut Psx) {
    psx.spu.audio_buffer_index = 0;
}

/// Put the provided stereo pair in the output buffer and flush it if necessary
fn output_samples(psx: &mut Psx, left: i16, right: i16) {
    let idx = psx.spu.audio_buffer_index as usize;

    // If this overflows the frontend isn't reading the samples fast enough
    psx.spu.audio_buffer[idx] = left;
    psx.spu.audio_buffer[idx + 1] = right;

    psx.spu.audio_buffer_index += 2;
}

/// Emulate one cycle of the SPU
fn run_cycle(psx: &mut Psx) {
    psx.spu.update_status();

    // Sum of the left and right voice volume levels
    let mut left_mix = 0;
    let mut right_mix = 0;

    for voice in 0..24 {
        let (left, right) = run_voice_cycle(psx, voice);

        left_mix += left;
        right_mix += right;

        if psx.spu.is_voice_reverberated(voice) {
            unimplemented!()
        }
    }

    // Voice start/stop should've been processed by `run_voice_cycle`
    psx.spu.voice_start = 0;
    psx.spu.voice_stop = 0;

    if psx.spu.muted() {
        // Mute bit doesn't actually mute CD audio, just the SPU voices.
        left_mix = 0;
        right_mix = 0;
    }

    if psx.spu.cd_audio_enabled() {
        let (cd_left, cd_right) = cdrom::run_audio_cycle(psx, true);

        left_mix += i32::from(cd_left);
        right_mix += i32::from(cd_right);
    } else {
        cdrom::run_audio_cycle(psx, false);
    }

    left_mix = saturate_to_i16(left_mix) as i32;
    right_mix = saturate_to_i16(right_mix) as i32;

    left_mix = psx.spu.main_volume_left.apply_level(left_mix);
    right_mix = psx.spu.main_volume_right.apply_level(right_mix);

    psx.spu.main_volume_left.run_sweep_cycle();
    psx.spu.main_volume_right.run_sweep_cycle();

    psx.spu.capture_index += 1;
    psx.spu.capture_index &= 0x1ff;

    output_samples(psx, saturate_to_i16(left_mix), saturate_to_i16(right_mix));
}

/// Run `voice` for one cycle and return a pair of stereo samples
fn run_voice_cycle(psx: &mut Psx, voice: u8) -> (i32, i32) {
    // There's no "enable" flag for the voices, they're effectively always running. Unused voices
    // are just muted. Beyond that the ADPCM decoder is always running, even when the voice is in
    // "noise" mode and the output isn't used. This is important when the SPU interrupt is enabled.
    run_voice_decoder(psx, voice);

    let raw_sample = if psx.spu.is_noise(voice) {
        // TODO: implement noise LFSR
        unimplemented!()
    } else {
        psx.spu[voice].next_raw_sample()
    };

    let sample = psx.spu[voice].apply_enveloppe(raw_sample);

    // Voices 1 and 3 write their samples back into SPU RAM (what No$ refers to as "capture")
    if voice == 1 {
        ram_write(psx, 0x400 | psx.spu.capture_index, sample as u16);
    } else if voice == 3 {
        ram_write(psx, 0x600 | psx.spu.capture_index, sample as u16);
    }

    let (left, right) = psx.spu[voice].apply_stereo(sample);

    psx.spu[voice].run_sweep_cycle();

    if psx.spu[voice].start_delay > 0 {
        // We're still in the start delay, we don't run the envelope or frequency sweep yet
        psx.spu[voice].start_delay -= 1;
    } else {
        psx.spu[voice].run_envelope_cycle();

        let step = u32::from(psx.spu[voice].step_length);

        if psx.spu.is_frequency_modulated(voice) {
            // Voice 0 cannot be frequency modulated
            debug_assert!(voice != 0);

            unimplemented!();
        }

        let step = if step > 0x3fff { 0x3fff } else { step as u16 };

        psx.spu[voice].consume_samples(step);
    }

    if psx.spu.is_voice_stopped(voice) {
        psx.spu[voice].release();
    }

    if psx.spu.is_voice_started(voice) {
        psx.spu[voice].restart();
        psx.spu.voice_looped &= !(1 << voice);
    }

    if !psx.spu.enabled() {
        // XXX Mednafen doesn't reset the ADSR divider in this situation
        psx.spu[voice].release();
        psx.spu[voice].mute();
    }

    (left, right)
}

/// Run the ADPCM decoder for one cycle
fn run_voice_decoder(psx: &mut Psx, voice: u8) {
    // XXX This value of 11 is taken from Mednafen. Technically we only consume 4 samples (at most)
    // per cycle so >= 4 would do the trick but apparently the original hardware decodes ahead.
    // This is important if the IRQ is enabled since it means that it would trigger a bit earlier
    // when the block is read.
    //
    // This is still not entirely cycle accurate, so we could be improved further with more
    // testing. Mednafen's codebase has a few comments giving hints on what could be done. More
    // testing required.
    if psx.spu[voice].decoder_fifo.len() >= 11 {
        // We have enough data in the decoder FIFO, no need to decode more
        if psx.spu.irq_enabled() {
            // Test prev address
            unimplemented!();
        }
    } else {
        // True if we're starting a new ADPCM block
        let new_block = psx.spu[voice].cur_index % 8 == 0;

        if new_block {
            // Check if looping has been requested in the previous block
            if psx.spu[voice].maybe_loop() {
                psx.spu.voice_looped |= 1 << voice;

                // Mednafen doesn't apply the "release and mute" block flag if we're in noise
                // mode. No$ doesn't seem to mention this corner case, but I suppose that it makes
                // sense to ignore decoder envelope changes if we don't use the data.
                if !psx.spu.is_noise(voice) {
                    psx.spu[voice].maybe_release();
                }
            }
        }

        if psx.spu.irq_enabled() {
            // Test current address
            unimplemented!();
        }

        if new_block {
            // We're starting a new block
            let header = ram_read(psx, psx.spu[voice].cur_index);

            psx.spu[voice].set_block_header(header);
            psx.spu[voice].next_index();
        }

        // Decode 4 samples
        let encoded = ram_read(psx, psx.spu[voice].cur_index);
        psx.spu[voice].next_index();
        psx.spu[voice].decode(encoded);
    }
}

/// Handle DMA writes
pub fn dma_store(psx: &mut Psx, v: u32) {
    let w1 = v as u16;
    let w2 = (v >> 16) as u16;

    // XXX Mednafen only checks for IRQ after the 2nd word.
    transfer(psx, w1);
    transfer(psx, w2);
}

/// Handle DMA reads
pub fn dma_load(psx: &mut Psx) -> u32 {
    let w1 = ram_read(psx, psx.spu.ram_index) as u32;
    psx.spu.ram_index = (psx.spu.ram_index + 1) & 0x3_ffff;
    let w2 = ram_read(psx, psx.spu.ram_index) as u32;
    psx.spu.ram_index = (psx.spu.ram_index + 1) & 0x3_ffff;

    check_for_irq(psx, psx.spu.ram_index);

    w1 | (w2 << 16)
}

pub fn store<T: Addressable>(psx: &mut Psx, off: u32, val: T) {
    if T::width() != AccessWidth::HalfWord {
        panic!("Unhandled {:?} SPU store", T::width());
    }

    let val = val.as_u16();

    let index = (off >> 1) as usize;

    psx.spu.regs[index] = val;

    if index < 0xc0 {
        // Voice configuration
        let voice = &mut psx.spu.voices[index >> 3];

        match index & 7 {
            regmap::voice::VOLUME_LEFT => voice.volume_left.set_config(val),
            regmap::voice::VOLUME_RIGHT => voice.volume_right.set_config(val),
            regmap::voice::ADPCM_STEP_LENGTH => voice.step_length = val,
            regmap::voice::ADPCM_START_INDEX => voice.set_start_index(to_ram_index(val)),
            regmap::voice::ADPCM_ADSR_LO => voice.adsr.set_conf_lo(val),
            regmap::voice::ADPCM_ADSR_HI => voice.adsr.set_conf_hi(val),
            regmap::voice::CURRENT_ADSR_VOLUME => voice.set_level(val as i16),
            regmap::voice::ADPCM_REPEAT_INDEX => {
                let loop_index = (RamIndex::from(val) << 2) & 0x3_ffff;
                voice.set_loop_index(loop_index);
            }
            _ => (),
        }
    } else if index < 0x100 {
        match index {
            regmap::MAIN_VOLUME_LEFT => psx.spu.main_volume_left.set_config(val),
            regmap::MAIN_VOLUME_RIGHT => psx.spu.main_volume_right.set_config(val),
            regmap::REVERB_VOLUME_LEFT => (),
            regmap::REVERB_VOLUME_RIGHT => (),
            regmap::VOICE_ON_LO => to_lo(&mut psx.spu.voice_start, val),
            regmap::VOICE_ON_HI => to_hi(&mut psx.spu.voice_start, val),
            regmap::VOICE_OFF_LO => to_lo(&mut psx.spu.voice_stop, val),
            regmap::VOICE_OFF_HI => to_hi(&mut psx.spu.voice_stop, val),
            regmap::VOICE_FM_MOD_EN_LO => {
                // Voice 0 cannot be frequency modulated
                to_lo(&mut psx.spu.voice_frequency_modulated, val & !1);
            }
            regmap::VOICE_FM_MOD_EN_HI => to_hi(&mut psx.spu.voice_frequency_modulated, val),
            regmap::VOICE_NOISE_EN_LO => to_lo(&mut psx.spu.voice_noise, val),
            regmap::VOICE_NOISE_EN_HI => to_hi(&mut psx.spu.voice_noise, val),
            regmap::VOICE_REVERB_EN_LO => (),
            regmap::VOICE_REVERB_EN_HI => (),
            regmap::VOICE_STATUS_LO => to_lo(&mut psx.spu.voice_looped, val),
            regmap::VOICE_STATUS_HI => to_hi(&mut psx.spu.voice_looped, val),
            regmap::REVERB_BASE => (),
            regmap::TRANSFER_START_INDEX => psx.spu.ram_index = to_ram_index(val),
            regmap::TRANSFER_FIFO => transfer(psx, val),
            regmap::CONTROL => {
                if psx.spu.irq_enabled() {
                    check_for_irq(psx, psx.spu.ram_index);
                } else {
                    // IRQ is acknowledged
                    psx.spu.irq = false;
                }
            }
            regmap::TRANSFER_CONTROL => {
                if val != 4 {
                    // According to No$ this register controls the way the data is transferred to
                    // the sound ram and the only value that makes sense is 4 (or more
                    // specifically, bits [3:1] should be 2), otherwise bytes get repeated using
                    // various patterns.
                    warn!("SPU TRANSFER_CONTROL set to 0x{:x}", val);
                }
            }
            regmap::CD_VOLUME_LEFT => (),
            regmap::CD_VOLUME_RIGHT => (),
            regmap::EXT_VOLUME_LEFT => (),
            regmap::EXT_VOLUME_RIGHT => (),
            // Reverb configuration
            regmap::REVERB_APF_OFFSET1..=regmap::REVERB_INPUT_VOLUME_RIGHT => (),
            _ => unimplemented!(
                "SPU store index {:x} (off = {:x}, abs = {:x})",
                index,
                off,
                0x1f80_1c00 + off
            ),
        }
    } else {
        unimplemented!("Internal SPU register store");
    }
}

pub fn load<T: Addressable>(psx: &mut Psx, off: u32) -> T {
    // This is probably very heavy handed, mednafen only syncs from the CD code and never on
    // register access
    run(psx);

    if T::width() != AccessWidth::HalfWord {
        panic!("Unhandled {:?} SPU load", T::width());
    }

    let index = (off >> 1) as usize;

    let reg_v = psx.spu.regs[index];

    let v = if index < 0xc0 {
        let voice = &psx.spu.voices[index >> 3];

        match index & 7 {
            regmap::voice::CURRENT_ADSR_VOLUME => voice.level() as u16,
            regmap::voice::ADPCM_REPEAT_INDEX => (voice.loop_index >> 2) as u16,
            _ => reg_v,
        }
    } else if index < 0x100 {
        match index {
            regmap::VOICE_STATUS_LO => psx.spu.voice_looped as u16,
            regmap::VOICE_STATUS_HI => (psx.spu.voice_looped >> 16) as u16,
            regmap::TRANSFER_FIFO => unimplemented!(),
            regmap::CURRENT_VOLUME_LEFT => psx.spu.main_volume_left.level() as u16,
            regmap::CURRENT_VOLUME_RIGHT => psx.spu.main_volume_right.level() as u16,
            // Nobody seems to know what this register is for, but mednafen returns 0
            regmap::UNKNOWN => return T::from_u32(0),
            _ => reg_v,
        }
    } else {
        unimplemented!("Internal SPU register load");
    };

    T::from_u32(u32::from(v))
}

/// Write the SPU ram at the `ram_index` an increment it.
fn transfer(psx: &mut Psx, val: u16) {
    let i = psx.spu.ram_index;

    ram_write(psx, i, val);

    psx.spu.ram_index = (i + 1) & 0x3_ffff;

    // `ram_write` already checks for interrupt before the write but mednafen immediately rechecks
    // the incremented address after that. Sounds weird but let's go with it for now.
    check_for_irq(psx, psx.spu.ram_index);
}

fn ram_write(psx: &mut Psx, index: RamIndex, val: u16) {
    check_for_irq(psx, index);

    let index = index as usize;

    debug_assert!(index < psx.spu.ram.len());

    psx.spu.ram[index] = val;
}

fn ram_read(psx: &mut Psx, index: RamIndex) -> u16 {
    let index = index as usize;

    debug_assert!(index < psx.spu.ram.len());

    psx.spu.ram[index]
}

/// Trigger an IRQ if it's enabled in the control register and `addr` is equal to the `irq_addr`
fn check_for_irq(psx: &mut Psx, index: RamIndex) {
    if psx.spu.irq_enabled() && index == psx.spu.irq_addr {
        psx.spu.irq = true;
        panic!("Trigger SPU IRQ!");
    }
}

pub struct Voice {
    /// Voice volume left
    volume_left: Volume,
    /// Voice volume right
    volume_right: Volume,
    /// Attack Decay Sustain Release envelope
    adsr: Adsr,
    /// This value configures how fast the samples are played on this voice, which effectively
    /// changes the frequency of the output audio.
    ///
    /// The value is a 14 bit fixed point integer with 12 fractional bits
    step_length: u16,
    /// Remaining fractional steps carried between cycles, giving up the effective phase of the
    /// voice. 12 fractional bits.
    phase: u16,
    /// Value `cur_index` will take upon voice start
    start_index: RamIndex,
    /// Current index in SPU RAM for this voice
    cur_index: RamIndex,
    /// Target address for `cur_index` when an ADPCM block requests looping
    loop_index: RamIndex,
    /// True if `loop_index` has been configured through the register interface and any ADPCM loop
    /// block should be ignored.
    loop_index_force: bool,
    /// Header for the current ADPCM block
    block_header: AdpcmHeader,
    /// Last two ADPCM-decoded samples, used to extrapolate the next one
    last_samples: [i16; 2],
    /// FIFO containing the samples that have been decoded but not yet output
    decoder_fifo: DecoderFifo,
    /// Delay (in SPU cycles) between the moment a voice is enabled and the moment the envelope
    /// and frequency functions start running
    start_delay: u8,
}

impl Voice {
    fn new() -> Voice {
        Voice {
            volume_left: Volume::new(),
            volume_right: Volume::new(),
            adsr: Adsr::new(),
            step_length: 0,
            phase: 0,
            start_index: 0,
            cur_index: 0,
            loop_index: 0,
            loop_index_force: false,
            block_header: AdpcmHeader(0),
            last_samples: [0; 2],
            decoder_fifo: DecoderFifo::new(),
            start_delay: 0,
        }
    }

    /// Perform a loop if it was requested by the previously decoded block. Returns `true` if a
    /// loop has taken place
    fn maybe_loop(&mut self) -> bool {
        let do_loop = self.block_header.loop_end();

        if do_loop {
            self.cur_index = self.loop_index & !7;
        }

        do_loop
    }

    /// Release if it was requested by the previously decoded block. Should only be called if the
    /// block also requested looping.
    fn maybe_release(&mut self) {
        debug_assert!(self.block_header.loop_end());
        if self.block_header.loop_release_and_mute() {
            // XXX Mednafen only change the ADSR step and doesn't reset the divider but there's
            // a comment wondering if it should be reset too. To keep the code simpler here I
            // simply call the same function used when a voice is stopped.
            self.adsr.release();
            self.adsr.level = 0;
        }
    }

    /// Increment `cur_index`, wrapping to 0 if we've reached the end of the SPU RAM
    fn next_index(&mut self) {
        self.cur_index = (self.cur_index + 1) % SPU_RAM_SIZE as u32;
    }

    fn set_start_index(&mut self, addr: RamIndex) {
        // From mednafen: apparently the start index is aligned to a multiple of 8 samples
        self.start_index = addr & !7;
    }

    fn set_level(&mut self, level: i16) {
        self.adsr.set_level(level)
    }

    fn level(&self) -> i16 {
        self.adsr.level
    }

    fn set_block_header(&mut self, header: u16) {
        self.block_header = AdpcmHeader(header);

        if !self.loop_index_force && self.block_header.loop_start() {
            self.loop_index = self.cur_index;
        }
    }

    fn set_loop_index(&mut self, loop_index: RamIndex) {
        self.loop_index = loop_index;
        self.loop_index_force = true;
    }

    /// Decode 4 samples from an ADPCM block
    fn decode(&mut self, mut encoded: u16) {
        let (wp, wn) = self.block_header.weights();
        let mut shift = self.block_header.shift();

        // Taken from Mednafen: normally the shift value should be between 0 and 12 since otherwise
        // you lose precision. Apparently when that happens we only keep the sign bit and extend it
        // 8 times.
        //
        // XXX Should probably be tested on real hardware and added as a unit test.
        if shift > 12 {
            encoded &= 0x8888;
            shift = 8;
        }

        // Decode the four 4bit samples
        for i in 0..4 {
            // Extract the 4 bits and convert to signed to get proper sign extension when shifting
            let mut sample = (encoded << (12 - i * 4) & 0xf000) as i16;

            sample >>= shift;

            let mut sample = i32::from(sample);

            // Previous sample
            let sample_1 = i32::from(self.last_samples[0]);
            // Antepenultimate sample
            let sample_2 = i32::from(self.last_samples[1]);

            // Extrapolate with sample -1 using the positive weight
            sample += (sample_1 * wp) >> 6;
            // Extrapolate with sample -2 using the negative weight
            sample += (sample_2 * wn) >> 6;

            let sample = saturate_to_i16(sample);
            self.decoder_fifo.push(sample);

            // Shift `last_samples` for the next sample
            self.last_samples[1] = self.last_samples[0];
            self.last_samples[0] = sample;
        }
    }

    /// Returns the next "raw" decoded sample for this voice, meaning the post-ADPCM decode and
    /// resampling but pre-ADSR.
    fn next_raw_sample(&self) -> i32 {
        let phase = (self.phase >> 4) as u8;
        let samples = [
            self.decoder_fifo[0],
            self.decoder_fifo[1],
            self.decoder_fifo[2],
            self.decoder_fifo[3],
        ];

        fir::filter(phase, samples)
    }

    /// Run one cycle for the ADSR envelope function
    fn run_envelope_cycle(&mut self) {
        self.adsr.run_cycle();
    }

    fn run_sweep_cycle(&mut self) {
        self.volume_left.run_sweep_cycle();
        self.volume_right.run_sweep_cycle();
    }

    /// Apply the Attack Decay Sustain Release envelope to a sample
    fn apply_enveloppe(&self, sample: i32) -> i32 {
        let level = i32::from(self.adsr.level);

        (sample * level) >> 15
    }

    /// Apply left and right volume levels
    fn apply_stereo(&self, sample: i32) -> (i32, i32) {
        (
            self.volume_left.apply_level(sample),
            self.volume_right.apply_level(sample),
        )
    }

    /// Reinitialize voice
    fn restart(&mut self) {
        self.adsr.attack();
        self.phase = 0;
        self.cur_index = self.start_index & !7;
        self.block_header = AdpcmHeader(0);
        self.last_samples = [0; 2];
        self.decoder_fifo.clear();
        self.start_delay = 4;
        self.loop_index_force = false;
    }

    /// Put the ADSR enveloppe in "release" state if it's not already
    fn release(&mut self) {
        self.adsr.release();
    }

    /// Set the envelope's volume to 0
    fn mute(&mut self) {
        self.adsr.level = 0;
    }

    fn consume_samples(&mut self, step: u16) {
        let step = self.phase + step;

        // Update phase with the remaining fractional part
        self.phase = step & 0xfff;

        // Consume samples as needed
        let consumed = step >> 12;
        self.decoder_fifo.discard(consumed as usize);
    }
}

/// Saturating cast from i32 to i16
fn saturate_to_i16(v: i32) -> i16 {
    if v < i16::min_value() as i32 {
        i16::min_value()
    } else if v > i16::max_value() as i32 {
        i16::max_value()
    } else {
        v as i16
    }
}

struct Volume {
    level: i16,
    config: VolumeConfig,
}

impl Volume {
    fn new() -> Volume {
        Volume {
            level: 0,
            config: VolumeConfig::Fixed(0),
        }
    }

    fn set_config(&mut self, conf: u16) {
        let fixed = conf & 0x8000 == 0;

        self.config = if fixed {
            let level = (conf << 1) as i16;

            VolumeConfig::Fixed(level)
        } else {
            // XXX TODO
            VolumeConfig::Sweep(EnvelopeParams::new())
        };
        // XXX should we update self.level right now? Mefnaden waits for the next call to
        // run_sweep_cycle but that takes place after the level is read.
    }

    fn level(&self) -> i16 {
        self.level
    }

    /// Apply current level to a sound sample
    fn apply_level(&self, sample: i32) -> i32 {
        let level = self.level as i32;

        (sample * level) >> 15
    }

    fn run_sweep_cycle(&mut self) {
        self.level = match self.config {
            VolumeConfig::Fixed(l) => l,
            VolumeConfig::Sweep(_) => unimplemented!(),
        };
    }
}

/// Volume configuration, either fixed or a sweep
enum VolumeConfig {
    /// Fixed volume
    Fixed(i16),
    /// Sweep
    Sweep(EnvelopeParams),
}

/// Attack Decay Sustain Release envelope
struct Adsr {
    state: AdsrState,
    /// Current audio level for this envelope
    level: i16,
    /// Divider used to count until the next envelope step
    divider: u16,
    /// Pre-computed envelope parameters for all 4 ADSR states
    params: [EnvelopeParams; 4],
    /// Volume level used to trigger the switch from Decay to Sustain mode
    sustain_level: i16,
    /// Config register value
    config: AdsrConfig,
}

impl Adsr {
    fn new() -> Adsr {
        let mut adsr = Adsr {
            state: AdsrState::Attack,
            level: 0,
            divider: 0,
            params: [
                EnvelopeParams::new(),
                EnvelopeParams::new(),
                EnvelopeParams::new(),
                EnvelopeParams::new(),
            ],
            sustain_level: 0,
            config: AdsrConfig::new(),
        };

        // Not really needed but it's probably cleaner to make sure that `params` and `config`
        // remain always in sync
        adsr.refresh_params();

        adsr
    }

    fn set_level(&mut self, level: i16) {
        self.level = level;
    }

    fn run_cycle(&mut self) {
        let params = &self.params[self.state as usize];

        let div_step = params.compute_divider_step(self.level);
        debug_assert!(div_step > 0);

        // `div_step`'s max value should be 0x8000, so the addition should never overflow
        debug_assert!(div_step <= 0x8000);
        self.divider += div_step;

        if self.divider < 0x8000 {
            // We haven't reached the next step yet.
            return;
        }

        // Next step reached
        self.divider = 0;

        let level_step = params.compute_level_step(self.level);

        // According to Mednafen's code negative audio levels (normally only possible through a
        // manual write to the register) are treated as underflows *except* during the attack.
        // It's unlikely to occur in practice because the level is reset to 0 at the start of
        // the attack, so the only way this can happen is if the level is rewritten while the
        // attack is in progress.
        //
        // XXX That's probably worth a double-check on the real hardware
        if self.state == AdsrState::Attack {
            self.level = match self.level.checked_add(level_step) {
                Some(l) => l,
                None => {
                    // Overflow
                    self.state = AdsrState::Decay;
                    i16::max_value()
                }
            }
        } else {
            self.level = self.level.wrapping_add(level_step);

            if self.level < 0 {
                // Overflow or underflow
                self.level = if level_step > 0 { i16::max_value() } else { 0 };
            }
        }

        if self.state == AdsrState::Decay && self.level <= self.sustain_level {
            self.state = AdsrState::Sustain;
        }
    }

    /// Refresh the pre-computed `params`
    fn refresh_params(&mut self) {
        self.sustain_level = self.config.sustain_level();
        self.params[AdsrState::Attack as usize] = self.config.attack_params();
        self.params[AdsrState::Decay as usize] = self.config.decay_params();
        self.params[AdsrState::Sustain as usize] = self.config.sustain_params();
        self.params[AdsrState::Release as usize] = self.config.release_params();
    }

    fn set_conf_lo(&mut self, v: u16) {
        self.config.set_lo(v);
        self.refresh_params();
    }

    fn set_conf_hi(&mut self, v: u16) {
        self.config.set_hi(v);
        self.refresh_params();
    }

    fn release(&mut self) {
        self.divider = 0;
        self.state = AdsrState::Release;
    }

    fn attack(&mut self) {
        self.divider = 0;
        self.state = AdsrState::Attack;
        self.level = 0;
    }
}

/// Parameters used to
struct EnvelopeParams {
    /// Base divider step value (how fast do we reach the next step).
    divider_step: u16,
    /// Base level step value
    level_step: i16,
    /// Envelope mode that modifies the way the steps are calculated
    mode: EnvelopeMode,
}

impl EnvelopeParams {
    fn new() -> EnvelopeParams {
        EnvelopeParams {
            divider_step: 0,
            level_step: 0,
            mode: EnvelopeMode::Linear,
        }
    }

    /// Compute (divider_step, level_step) for the given `shift` and `step` values
    fn steps(shift: u32, step: i8) -> (u16, i16) {
        let step = step as i16;

        if shift < 11 {
            (0x8000, step << (11 - shift))
        } else {
            let div_shift = shift - 11;

            if div_shift <= 15 {
                (0x8000 >> div_shift, step)
            } else {
                (1, step)
            }
        }
    }

    /// Compute the parameters for smooth mode
    fn smooth_mode(step: u32, base_divider: u16, base_level: i16) -> EnvelopeMode {
        let mut smooth_divider = if step > 10 && base_divider > 3 {
            base_divider >> 2
        } else if step >= 10 && base_divider > 1 {
            base_divider >> 1
        } else {
            base_divider
        };

        if smooth_divider == 0 {
            smooth_divider = 1;
        }

        let smooth_level = if step < 10 {
            base_level >> 2
        } else if step == 10 {
            base_level >> 1
        } else {
            base_level
        };

        EnvelopeMode::SmoothUp(smooth_divider, smooth_level)
    }

    fn compute_divider_step(&self, cur_level: i16) -> u16 {
        if let EnvelopeMode::SmoothUp(smooth_divider_step, _) = self.mode {
            if cur_level >= 0x6000 {
                return smooth_divider_step;
            }
        }

        self.divider_step
    }

    fn compute_level_step(&self, cur_level: i16) -> i16 {
        match self.mode {
            EnvelopeMode::Linear => self.level_step,
            EnvelopeMode::Exponential => {
                let ls = self.level_step as i32;
                let cl = cur_level as i32;

                ((ls * cl) >> 15) as i16
            }
            EnvelopeMode::SmoothUp(_, smooth_level_step) => {
                if cur_level >= 0x6000 {
                    smooth_level_step
                } else {
                    self.level_step
                }
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
enum EnvelopeMode {
    /// Divider and Volume steps remain the same throughout
    Linear,
    /// Behaves linearly up until volume reaches 0x6000, then the divider_step is replaced by the
    /// first tuple param and the level_step is replaced by the 2nd parameter
    SmoothUp(u16, i16),
    /// Volume steps are multiplied by the current value of the volume, resulting in
    /// exponentially bigger steps (in absolute value)
    Exponential,
}

#[derive(Copy, Clone)]
struct AdsrConfig(u32);

impl AdsrConfig {
    fn new() -> AdsrConfig {
        AdsrConfig(0)
    }

    fn sustain_level(self) -> i16 {
        let sl = self.0 & 0xf;

        let sl = ((sl + 1) << 11) - 1;

        debug_assert!(sl < 0x8000);

        sl as i16
    }

    fn attack_params(self) -> EnvelopeParams {
        let shift = (self.0 >> 10) & 0x1f;
        let step = 7 - ((self.0 >> 8) & 3);
        let exp = (self.0 >> 15) & 1 != 0;

        let (div_step, lvl_step) = EnvelopeParams::steps(shift, step as i8);

        let mode = if exp {
            EnvelopeParams::smooth_mode(step, div_step, lvl_step)
        } else {
            EnvelopeMode::Linear
        };

        EnvelopeParams {
            divider_step: div_step,
            level_step: lvl_step,
            mode,
        }
    }

    fn decay_params(self) -> EnvelopeParams {
        let shift = (self.0 >> 4) & 0xf;
        let step = -8;

        let (div_step, ls) = EnvelopeParams::steps(shift, step);

        EnvelopeParams {
            divider_step: div_step,
            level_step: ls,
            mode: EnvelopeMode::Exponential,
        }
    }

    fn sustain_params(self) -> EnvelopeParams {
        let shift = (self.0 >> 24) & 0x1f;
        let raw_step = 7 - ((self.0 >> 22) & 3);
        let exp = (self.0 >> 31) & 1 != 0;
        let inv_step = (self.0 >> 30) & 1 != 0;

        let step = if inv_step { !raw_step } else { raw_step };

        let (div_step, lvl_step) = EnvelopeParams::steps(shift, step as i8);

        let mode = if exp {
            if inv_step {
                EnvelopeMode::Exponential
            } else {
                EnvelopeParams::smooth_mode(raw_step, div_step, lvl_step)
            }
        } else {
            EnvelopeMode::Linear
        };

        EnvelopeParams {
            divider_step: div_step,
            level_step: lvl_step,
            mode,
        }
    }

    fn release_params(self) -> EnvelopeParams {
        let shift = (self.0 >> 16) & 0x1f;
        let step = -8;
        let exp = (self.0 >> 21) & 1 != 0;

        let (div_step, lvl_step) = EnvelopeParams::steps(shift, step as i8);

        let mode = if exp {
            EnvelopeMode::Exponential
        } else {
            EnvelopeMode::Linear
        };

        EnvelopeParams {
            divider_step: div_step,
            level_step: lvl_step,
            mode,
        }
    }

    fn set_lo(&mut self, v: u16) {
        to_lo(&mut self.0, v);
    }

    fn set_hi(&mut self, v: u16) {
        to_hi(&mut self.0, v);
    }
}

/// Possible ADSR states
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum AdsrState {
    Attack,
    Decay,
    Sustain,
    Release,
}

/// The first two bytes of a 16-byte ADPCM block
#[derive(Copy, Clone)]
struct AdpcmHeader(u16);

impl AdpcmHeader {
    /// If true the current block is the last one of the loop sequence
    fn loop_end(self) -> bool {
        self.0 & (1 << 8) != 0
    }

    /// If true (and loop_end() is also true) we must release the envelope and set the volume
    /// to 0
    fn loop_release_and_mute(self) -> bool {
        // Shouldn't be called if `loop_end` is false
        debug_assert!(self.loop_end());
        self.0 & (1 << 9) == 0
    }

    /// If true the current block is the target for a subsequent loop_end block.
    fn loop_start(self) -> bool {
        self.0 & (1 << 10) != 0
    }

    /// Returns the pair of positive and negative weights described in the header
    fn weights(self) -> (i32, i32) {
        // Weights taken from No$, Mednafen use the same values.
        let w: [(i32, i32); 16] = [
            (0, 0),
            (60, 0),
            (115, -52),
            (98, -55),
            (122, -60),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
        ];

        let off = (self.0 >> 4) & 0xf;

        w[off as usize]
    }

    /// Right shift value to apply to extended encoded samples
    fn shift(self) -> u8 {
        (self.0 & 0xf) as u8
    }
}

/// Convert a register value to a ram index
fn to_ram_index(v: u16) -> RamIndex {
    RamIndex::from(v) << 2
}

fn to_hi(r: &mut u32, v: u16) {
    let v = u32::from(v);

    *r &= 0xffff;
    *r |= v << 16;
}

fn to_lo(r: &mut u32, v: u16) {
    let v = u32::from(v);

    *r &= 0xffff_0000;
    *r |= v;
}

#[allow(dead_code)]
mod regmap {
    //! SPU register map: offset from the base in number of *halfwords*

    pub mod voice {
        //! Per-voice regmap, repeated 24 times

        pub const VOLUME_LEFT: usize = 0x0;
        pub const VOLUME_RIGHT: usize = 0x1;
        pub const ADPCM_STEP_LENGTH: usize = 0x2;
        pub const ADPCM_START_INDEX: usize = 0x3;
        pub const ADPCM_ADSR_LO: usize = 0x4;
        pub const ADPCM_ADSR_HI: usize = 0x5;
        pub const CURRENT_ADSR_VOLUME: usize = 0x6;
        pub const ADPCM_REPEAT_INDEX: usize = 0x7;
    }

    pub const MAIN_VOLUME_LEFT: usize = 0xc0;
    pub const MAIN_VOLUME_RIGHT: usize = 0xc1;
    pub const REVERB_VOLUME_LEFT: usize = 0xc2;
    pub const REVERB_VOLUME_RIGHT: usize = 0xc3;
    pub const VOICE_ON_LO: usize = 0xc4;
    pub const VOICE_ON_HI: usize = 0xc5;
    pub const VOICE_OFF_LO: usize = 0xc6;
    pub const VOICE_OFF_HI: usize = 0xc7;
    pub const VOICE_FM_MOD_EN_LO: usize = 0xc8;
    pub const VOICE_FM_MOD_EN_HI: usize = 0xc9;
    pub const VOICE_NOISE_EN_LO: usize = 0xca;
    pub const VOICE_NOISE_EN_HI: usize = 0xcb;
    pub const VOICE_REVERB_EN_LO: usize = 0xcc;
    pub const VOICE_REVERB_EN_HI: usize = 0xcd;
    pub const VOICE_STATUS_LO: usize = 0xce;
    pub const VOICE_STATUS_HI: usize = 0xcf;

    pub const REVERB_BASE: usize = 0xd1;
    pub const TRANSFER_START_INDEX: usize = 0xd3;
    pub const TRANSFER_FIFO: usize = 0xd4;
    pub const CONTROL: usize = 0xd5;
    pub const TRANSFER_CONTROL: usize = 0xd6;
    pub const STATUS: usize = 0xd7;
    pub const CD_VOLUME_LEFT: usize = 0xd8;
    pub const CD_VOLUME_RIGHT: usize = 0xd9;
    pub const EXT_VOLUME_LEFT: usize = 0xda;
    pub const EXT_VOLUME_RIGHT: usize = 0xdb;
    pub const CURRENT_VOLUME_LEFT: usize = 0xdc;
    pub const CURRENT_VOLUME_RIGHT: usize = 0xdd;
    pub const UNKNOWN: usize = 0xde;

    pub const REVERB_APF_OFFSET1: usize = 0xe0;
    pub const REVERB_APF_OFFSET2: usize = 0xe1;
    pub const REVERB_REFLECT_VOLUME1: usize = 0xe2;
    pub const REVERB_COMB_VOLUME1: usize = 0xe3;
    pub const REVERB_COMB_VOLUME2: usize = 0xe4;
    pub const REVERB_COMB_VOLUME3: usize = 0xe5;
    pub const REVERB_COMB_VOLUME4: usize = 0xe6;
    pub const REVERB_REFLECT_VOLUME2: usize = 0xe7;
    pub const REVERB_APF_VOLUME1: usize = 0xe8;
    pub const REVERB_APF_VOLUME2: usize = 0xe9;
    pub const REVERB_REFLECT_SAME_LEFT1: usize = 0xea;
    pub const REVERB_REFLECT_SAME_RIGHT1: usize = 0xeb;
    pub const REVERB_COMB_LEFT1: usize = 0xec;
    pub const REVERB_COMB_RIGHT1: usize = 0xed;
    pub const REVERB_COMB_LEFT2: usize = 0xee;
    pub const REVERB_COMB_RIGHT2: usize = 0xef;
    pub const REVERB_REFLECT_SAME_LEFT2: usize = 0xf0;
    pub const REVERB_REFLECT_SAME_RIGHT2: usize = 0xf1;
    pub const REVERB_REFLECT_DIFF_LEFT1: usize = 0xf2;
    pub const REVERB_REFLECT_DIFF_RIGHT1: usize = 0xf3;
    pub const REVERB_COMB_LEFT3: usize = 0xf4;
    pub const REVERB_COMB_RIGHT3: usize = 0xf5;
    pub const REVERB_COMB_LEFT4: usize = 0xf6;
    pub const REVERB_COMB_RIGHT4: usize = 0xf7;
    pub const REVERB_REFLECT_DIFF_LEFT2: usize = 0xf8;
    pub const REVERB_REFLECT_DIFF_RIGHT2: usize = 0xf9;
    pub const REVERB_APF_LEFT1: usize = 0xfa;
    pub const REVERB_APF_RIGHT1: usize = 0xfb;
    pub const REVERB_APF_LEFT2: usize = 0xfc;
    pub const REVERB_APF_RIGHT2: usize = 0xfd;
    pub const REVERB_INPUT_VOLUME_LEFT: usize = 0xfe;
    pub const REVERB_INPUT_VOLUME_RIGHT: usize = 0xff;
}

/// SPU RAM size in multiple of 16bit words
const SPU_RAM_SIZE: usize = 256 * 1024;

/// The SPU runs at 44.1kHz, the CD audio frequency, this way no resampling is required
const AUDIO_FREQ_HZ: CycleCount = 44_100;

/// The CPU frequency is an exact multiple of the audio frequency, so the divider is always an
/// integer (0x300 normally)
const SPU_FREQ_DIVIDER: CycleCount = cpu::CPU_FREQ_HZ / AUDIO_FREQ_HZ;
