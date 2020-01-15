//! The PlayStation DMA, that can be used to copy data between the RAM and various devices (GPU,
//! CD drive, MDEC, SPU etc...)

use super::{AccessWidth, Addressable, Psx};
use std::ops::{Index, IndexMut};

pub struct Dma {
    control: Control,
    irq_config: IrqConfig,
    /// The 7 DMA channels
    channels: [Channel; 7],
}

impl Dma {
    pub fn new() -> Dma {
        Dma {
            control: Control::new(),
            irq_config: IrqConfig::new(),
            channels: [
                Channel::new(),
                Channel::new(),
                Channel::new(),
                Channel::new(),
                Channel::new(),
                Channel::new(),
                Channel::new(),
            ],
        }
    }
}

impl Index<Port> for Dma {
    type Output = Channel;

    fn index(&self, port: Port) -> &Self::Output {
        &self.channels[port as usize]
    }
}

impl IndexMut<Port> for Dma {
    fn index_mut(&mut self, port: Port) -> &mut Self::Output {
        &mut self.channels[port as usize]
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
            let port = Port::from_index(channel);

            match reg {
                8 => set_channel_control(psx, port, val),
                _ => unimplemented!("Write to channel {:?} register {:x}: {:x}", port, reg, val),
            }
        }
        7 => match reg {
            0 => psx.dma.control.set(val),
            4 => psx.dma.irq_config.set(val),
            _ => unimplemented!("DMA register write to {:x}", reg),
        },
        _ => unreachable!(),
    }
}

fn set_channel_control(psx: &mut Psx, port: Port, ctrl: u32) {
    if port == Port::Otc {
        unimplemented!("Handle OTC channel configuration");
    }

    let was_enabled = psx.dma[port].control.is_enabled();

    psx.dma[port].control.set(ctrl);

    let is_enabled = psx.dma[port].control.is_enabled();

    if was_enabled ^ is_enabled {
        // Channel was started or stopped
        if is_enabled {
            unimplemented!("Start DMA channel {:?}!", port);
        } else {
            unimplemented!("Force stop DMA channel {:?}", port);
        }
    }
}

/// The 7 DMA channels
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Port {
    /// Macroblock decoder input
    MDecIn = 0,
    /// Macroblock decoder output
    MDecOut = 1,
    /// Graphics Processing Unit
    Gpu = 2,
    /// CD-ROM drive
    CdRom = 3,
    /// Sound Processing Unit
    Spu = 4,
    /// Extension port
    Pio = 5,
    /// Used to clear the ordering table in RAM
    Otc = 6,
}

impl Port {
    pub fn from_index(index: u32) -> Port {
        match index {
            0 => Port::MDecIn,
            1 => Port::MDecOut,
            2 => Port::Gpu,
            3 => Port::CdRom,
            4 => Port::Spu,
            5 => Port::Pio,
            6 => Port::Otc,
            n => panic!("Invalid DMA channel {}", n),
        }
    }
}

struct Channel {
    control: ChannelControl,
}

impl Channel {
    fn new() -> Channel {
        Channel {
            control: ChannelControl::new(),
        }
    }
}

/// DMA channel control register
struct ChannelControl(u32);

impl ChannelControl {
    fn new() -> ChannelControl {
        ChannelControl(0)
    }

    fn set(&mut self, v: u32) {
        // Many bits are RO
        self.0 = v & 0x7177_0703
    }

    /// Returns true if the 'enable' bit is set
    fn is_enabled(&self) -> bool {
        self.0 & (1 << 24) != 0
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
