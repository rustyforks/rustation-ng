//! IRQ handling

use super::{cpu, Psx};

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
    /// SPU interrupt
    Spu = 9,
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

pub fn status(psx: &Psx) -> u16 {
    psx.irq.status
}

pub fn mask(psx: &Psx) -> u16 {
    psx.irq.mask
}

pub fn trigger(psx: &mut Psx, which: Interrupt) {
    psx.irq.status |= 1 << which as usize;

    cpu::irq_changed(psx);
}

pub fn set_mask(psx: &mut Psx, mask: u16) {
    // Temporary hack: trigger an error if a non-implemented interrupt is requested
    let supported: [Interrupt; 8] = [
        Interrupt::VBlank,
        Interrupt::CdRom,
        Interrupt::Dma,
        Interrupt::Timer0,
        Interrupt::Timer1,
        Interrupt::Timer2,
        Interrupt::PadMemCard,
        Interrupt::Spu,
    ];

    let rem = supported
        .iter()
        .fold(mask, |mask, &it| mask & !(1 << it as u16));

    debug_assert!(rem == 0, "Unsupported interrupt: {:04x}", rem);

    psx.irq.mask = mask;

    cpu::irq_changed(psx);
}

/// Acknowledge interrupts by writing 0 to the corresponding bit
pub fn ack(psx: &mut Psx, ack: u16) {
    psx.irq.status &= ack;

    cpu::irq_changed(psx);
}

/// Returns true if we currently have at least one active and unmasked interrupt
pub fn active(psx: &Psx) -> bool {
    status(psx) & mask(psx) != 0
}

/// Basic helper enum that can be used for return values of functions and method that can trigger
/// an interrupt. Generates a compiler warning if it's not checked, which should help avoid
/// "losing" interrupts
#[must_use]
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum IrqState {
    Idle,
    Triggered,
}

impl IrqState {
    pub fn is_triggered(self) -> bool {
        self == IrqState::Triggered
    }
}
