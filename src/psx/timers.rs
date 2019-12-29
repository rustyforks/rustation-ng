//! The PlayStation has three timers. They're mostly identical except that they can each select a
//! different clock source besides the regular system clock:
//!
//! - Timer 0: GPU pixel clock
//! - Timer 1: GPU horizontal blanking
//! - Timer 2: System clock / 8

use super::{Addressable, Psx};

pub struct Timers {
    /// For now we don't implement the timers, so just expose the registers as R/W
    regs: [u16; 12],
}

impl Timers {
    pub fn new() -> Timers {
        Timers { regs: [0; 12] }
    }
}

pub fn load<T: Addressable>(psx: &mut Psx, offset: u32) -> T {
    let index = (offset >> 2) as usize;

    T::from_u32(u32::from(psx.timers.regs[index]))
}

pub fn store<T: Addressable>(psx: &mut Psx, offset: u32, val: T) {
    let index = (offset >> 2) as usize;

    psx.timers.regs[index] = val.as_u32() as u16;
}
