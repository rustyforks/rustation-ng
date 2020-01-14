//! The PlayStation DMA, that can be used to copy data between the RAM and various devices (GPU,
//! CD drive, MDEC, SPU etc...)

use super::{AccessWidth, Addressable, Psx};

pub struct Dma {
    control: Control,
    irq_config: IrqConfig,
}

impl Dma {
    pub fn new() -> Dma {
        Dma {
            control: Control::new(),
            irq_config: IrqConfig::new(),
        }
    }
}

pub fn load<T: Addressable>(psx: &mut Psx, offset: u32) -> T {
    if T::width() != AccessWidth::Word {
        panic!("Unhandled DMA {:?} access", T::width());
    }

    let _ = psx;

    let channel = (offset & 0x70) >> 4;
    let reg = offset & 0xf;

    let v = match channel {
        0..=6 => {
            // Channel configuration
            unimplemented!("Read from channel {} register {:x}", channel, reg);
        }
        7 => match reg {
            0 => psx.dma.control.get(),
            4 => psx.dma.irq_config.get(),
            _ => unimplemented!("DMA register read from {:x}", reg),
        },
        _ => unreachable!(),
    };

    T::from_u32(v)
}

pub fn store<T: Addressable>(psx: &mut Psx, offset: u32, val: T) {
    if T::width() != AccessWidth::Word {
        panic!("Unhandled DMA {:?} access", T::width());
    }

    let channel = (offset & 0x70) >> 4;
    let reg = offset & 0xf;
    let val = val.as_u32();

    match channel {
        0..=6 => {
            // Channel configuration
            unimplemented!("Write to channel {} register {:x}", channel, reg);
        }
        7 => match reg {
            0 => psx.dma.control.set(val),
            4 => psx.dma.irq_config.set(val),
            _ => unimplemented!("DMA register write to {:x}", reg),
        },
        _ => unreachable!(),
    }
}

/// DMA control register
struct Control(u32);

impl Control {
    fn new() -> Control {
        Control(0)
    }

    fn set(&mut self, conf: u32) {
        self.0 = conf;
    }

    fn get(&self) -> u32 {
        self.0
    }
}

/// DMA IRQ config register
struct IrqConfig(u32);

impl IrqConfig {
    fn new() -> IrqConfig {
        IrqConfig(0)
    }

    fn set(&mut self, conf: u32) {
        // Not all bits are writeable
        self.0 = conf & 0x00ff_803f;
    }

    fn get(&self) -> u32 {
        self.0
    }
}
