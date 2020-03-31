//! Sound Processing Unit

use super::{cpu, sync, AccessWidth, Addressable, CycleCount, Psx};

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
    /// Main volume, left and right
    main_volume: [VolumeSweep; 2],
    /// The 24 individual voices
    voices: [Voice; 24],
    /// Most of the SPU's register behave like a R/W RAM, so to simplify the emulation we just
    /// store most registers in a big buffer
    regs: [u16; 320],
    /// SPU internal RAM, 16bit wide
    ram: [u16; SPU_RAM_SIZE],
}

impl Spu {
    pub fn new() -> Spu {
        Spu {
            ram_index: 0,
            capture_index: 0,
            irq_addr: 0,
            irq: false,
            main_volume: [VolumeSweep::new(), VolumeSweep::new()],
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
            regs: [0; 320],
            ram: [0; SPU_RAM_SIZE],
        }
    }

    /// Returns the value of the control register
    fn control(&self) -> u16 {
        self.regs[regmap::CONTROL]
    }

    fn irq_enabled(&self) -> bool {
        let control = self.control();

        // No$ says that the bit 6 (IRQ9) is "only when bit15=1", I'm not sure what that means.
        // Mednafen doesn't appear to put any condition on the interrupt bit.
        control & (1 << 6) != 0
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

/// Emulate one cycle of the SPU
fn run_cycle(psx: &mut Psx) {
    psx.spu.update_status();

    psx.spu.capture_index += 1;
    psx.spu.capture_index &= 0x1ff;
}

pub fn store<T: Addressable>(psx: &mut Psx, off: u32, val: T) {
    // This is probably very heavy handed, mednafen only syncs from the CD code and never on
    // register access
    run(psx);

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
            regmap::voice::VOLUME_LEFT => voice.volume[0].set_config(val),
            regmap::voice::VOLUME_RIGHT => voice.volume[1].set_config(val),
            regmap::voice::ADPCM_STEP_LENGTH => voice.step_length = val,
            regmap::voice::ADPCM_START_INDEX => voice.set_start_index(to_ram_index(val)),
            regmap::voice::ADPCM_ADSR_LO => voice.adsr.set_lo(val),
            regmap::voice::ADPCM_ADSR_HI => voice.adsr.set_hi(val),
            _ => (),
        }
    } else {
        match index {
            regmap::MAIN_VOLUME_LEFT => psx.spu.main_volume[0].set_config(val),
            regmap::MAIN_VOLUME_RIGHT => psx.spu.main_volume[1].set_config(val),
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
            _ => (),
        }
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

    if index > 0x100 {
        unimplemented!();
    }

    let reg_v = psx.spu.regs[index];

    let v = if index < 0xc0 {
        let voice = &psx.spu.voices[index >> 3];

        match index & 7 {
            regmap::voice::CURRENT_ADSR_VOLUME => voice.level(),
            regmap::voice::ADPCM_REPEAT_INDEX => unimplemented!(),
            _ => reg_v,
        }
    } else {
        match index {
            regmap::VOICE_STATUS_LO => unimplemented!(),
            regmap::VOICE_STATUS_HI => unimplemented!(),
            regmap::TRANSFER_FIFO => unimplemented!(),
            regmap::CURRENT_VOLUME_LEFT => unimplemented!(),
            regmap::CURRENT_VOLUME_RIGHT => unimplemented!(),
            // Nobody seems to know what this register is for, but mednafen returns 0
            regmap::UNKNOWN => return T::from_u32(0),
            _ => reg_v,
        }
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

/// Trigger an IRQ if it's enabled in the control register and `addr` is equal to the `irq_addr`
fn check_for_irq(psx: &mut Psx, index: RamIndex) {
    if psx.spu.irq_enabled() && index == psx.spu.irq_addr {
        psx.spu.irq = true;
        panic!("Trigger SPU IRQ!");
    }
}

struct Voice {
    /// Voice volume, left and riht
    volume: [VolumeSweep; 2],
    adsr: Adsr,
    /// This value configures how fast the samples are played on this voice, which effectively
    /// changes the frequency of the output audio.
    step_length: u16,
    start_index: RamIndex,
}

impl Voice {
    fn new() -> Voice {
        Voice {
            volume: [VolumeSweep::new(), VolumeSweep::new()],
            adsr: Adsr::new(),
            step_length: 0,
            start_index: 0,
        }
    }

    fn set_start_index(&mut self, addr: RamIndex) {
        // From mednafen: apparently the start index is aligned to a multiple of 8 samples
        self.start_index = addr & !7;
    }

    fn level(&self) -> u16 {
        // TODO
        0
    }
}

/// Volume configuration, either fixed or a sweep
struct VolumeSweep(u16);

impl VolumeSweep {
    fn new() -> VolumeSweep {
        VolumeSweep(0)
    }

    fn set_config(&mut self, conf: u16) {
        self.0 = conf
    }
}

/// Attack Delay Sustain Release envelope configuration
struct Adsr(u32);

impl Adsr {
    fn new() -> Adsr {
        Adsr(0)
    }

    fn set_lo(&mut self, v: u16) {
        to_lo(&mut self.0, v);
    }

    fn set_hi(&mut self, v: u16) {
        to_hi(&mut self.0, v);
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
