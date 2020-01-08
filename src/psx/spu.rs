//! Sound Processing Unit

use super::{AccessWidth, Addressable, Psx};

pub struct Spu {
    /// Control register
    control: Control,
    /// RAM index, used for read/writes using CPU or DMA.
    ram_index: u32,
    /// If the IRQ is enabled in the control register and the SPU memory is accessed at `irq_addr`
    /// (read *or* write) the interrupt is triggered.
    irq_addr: u32,
    /// Main volume, left and right
    main_volume: [Volume; 2],
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
            control: Control::new(),
            ram_index: 0,
            irq_addr: 0,
            main_volume: [Volume::new(), Volume::new()],
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
            regmap::voice::VOLUME_LEFT => voice.volume[0].set_config(val),
            regmap::voice::VOLUME_RIGHT => voice.volume[1].set_config(val),
            regmap::voice::ADPCM_STEP_LENGTH => voice.step_length = val,
            _ => panic!("Unhandled SPU voice configuration {:x} @ {:x}", val, index),
        }
    } else {
        match index {
            regmap::MAIN_VOLUME_LEFT => psx.spu.main_volume[0].set_config(val),
            regmap::MAIN_VOLUME_RIGHT => psx.spu.main_volume[1].set_config(val),
            regmap::REVERB_VOLUME_LEFT => (),
            regmap::REVERB_VOLUME_RIGHT => (),
            regmap::VOICE_ON_LO => (),
            regmap::VOICE_ON_HI => (),
            regmap::VOICE_OFF_LO => (),
            regmap::VOICE_OFF_HI => (),
            regmap::VOICE_FM_MOD_EN_LO => (),
            regmap::VOICE_FM_MOD_EN_HI => (),
            regmap::VOICE_NOISE_EN_LO => (),
            regmap::VOICE_NOISE_EN_HI => (),
            regmap::VOICE_REVERB_EN_LO => (),
            regmap::VOICE_REVERB_EN_HI => (),
            regmap::TRANSFER_START_INDEX => psx.spu.ram_index = u32::from(val) << 2,
            regmap::TRANSFER_FIFO => transfer(psx, val),
            regmap::CONTROL => psx.spu.control.set(val),
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
            _ => panic!("Unhandled SPU write {:x} @ {:x}", val, index),
        }
    }
}

pub fn load<T: Addressable>(psx: &mut Psx, off: u32) -> T {
    if T::width() != AccessWidth::HalfWord {
        panic!("Unhandled {:?} SPU load", T::width());
    }

    let index = (off >> 1) as usize;

    T::from_u32(u32::from(psx.spu.regs[index]))
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

fn ram_write(psx: &mut Psx, index: u32, val: u16) {
    check_for_irq(psx, index);

    let index = index as usize;

    debug_assert!(index < psx.spu.ram.len());

    psx.spu.ram[index] = val;
}

/// Trigger an IRQ if it's enabled in the control register and `addr` is equal to the `irq_addr`
fn check_for_irq(psx: &mut Psx, index: u32) {
    if psx.spu.control.irq_enabled() && index == psx.spu.irq_addr {
        panic!("Trigger SPU IRQ!");
    }
}

/// SPU Control register
struct Control(u16);

impl Control {
    fn new() -> Control {
        Control(0)
    }

    fn set(&mut self, c: u16) {
        self.0 = c;

        if self.irq_enabled() {
            panic!("SPU IRQ enabled!");
        }
    }

    fn irq_enabled(&self) -> bool {
        // No$ says that the bit 6 (IRQ9) is "only when bit15=1", I'm not sure what that means.
        // Mednafen doesn't appear to put any condition on the interrupt bit.
        self.0 & (1 << 6) != 0
    }
}

/// Structure representing the state of one of the PSX SPU's 24voices
struct Voice {
    /// Voice volume, left and riht
    volume: [Volume; 2],
    /// This value configures how fast the samples are played on this voice, which effectively
    /// changes the frequency of the output audio.
    step_length: u16,
}

impl Voice {
    fn new() -> Voice {
        Voice {
            volume: [Volume::new(), Volume::new()],
            step_length: 0,
        }
    }
}

struct Volume {
    config: u16,
}

impl Volume {
    fn new() -> Volume {
        Volume { config: 0 }
    }

    fn set_config(&mut self, conf: u16) {
        self.config = conf;
    }
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
