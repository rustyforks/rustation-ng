//! IRQ handling

use super::Psx;

/// The PlayStation supports 10 interrupts
#[derive(Clone, Copy, Debug)]
#[allow(unused)]
pub enum Interrupt {
    /// Display in vertical blanking
    VBlank = 0,
    /// CDROM controller
    CdRom = 2,
    /// DMA transfer done
    Dma = 3,
    /// Timer0 interrupt
    Timer0 = 4,
    /// Timer1 interrupt
    Timer1 = 5,
    /// Timer2 interrupt
    Timer2 = 6,
    /// Gamepad and Memory Card controller interrupt
    PadMemCard = 7,
}

#[derive(Clone, Copy)]
pub struct InterruptState {
    /// Interrupt status
    status: u16,
    /// Interrupt mask
    mask: u16,
}

impl InterruptState {
    pub fn new() -> InterruptState {
        InterruptState { status: 0, mask: 0 }
    }
}

pub fn status(psx: &mut Psx) -> u16 {
    psx.irq.status
}

pub fn mask(psx: &mut Psx) -> u16 {
    psx.irq.mask
}

pub fn set_mask(psx: &mut Psx, mask: u16) {
    // Temporary hack: trigger an error if a non-implemented interrupt is requested
    let supported: [Interrupt; 0] = [];

    let rem = supported
        .iter()
        .fold(mask, |mask, &it| mask & !(1 << it as u16));

    if rem != 0 {
        panic!("Unsupported interrupt: {:04x}", rem);
    }

    psx.irq.mask = mask;
}

/// Acknowledge interrupts by writing 0 to the corresponding bit
pub fn ack(psx: &mut Psx, ack: u16) {
    psx.irq.status &= ack;
}
