//! Debugger interface

use super::Psx;
use std::cell::RefCell;

/// Trait defining the debugger interface
pub trait Debugger {
    /// Signal a "break" which will put the emulator in debug mode at the next instruction
    fn trigger_break(&mut self, psx: &mut Psx);

    /// Called by the CPU when it's about to execute a new instruction. This function is called
    /// before *all* CPU instructions so it needs to be as fast as possible.
    fn pc_change(&mut self, psx: &mut Psx);

    /// Called by the CPU when it's about to load a value from memory.
    fn memory_read(&mut self, psx: &mut Psx, addr: u32);

    /// Called by the CPU when it's about to write a value to memory.
    fn memory_write(&mut self, psx: &mut Psx, addr: u32);
}

/// Dummy debugger implementation that does nothing. Can be used when debugging is disabled.
impl Debugger for () {
    fn trigger_break(&mut self, _: &mut Psx) {}

    fn pc_change(&mut self, _: &mut Psx) {}

    fn memory_read(&mut self, _: &mut Psx, _: u32) {}

    fn memory_write(&mut self, _: &mut Psx, _: u32) {}
}

#[cfg(feature = "debugger")]
thread_local! {
    pub static DEBUGGER: RefCell<Box<dyn Debugger>> = RefCell::new(Box::new(()));
}

#[cfg(feature = "debugger")]
pub fn swap_debugger(new_debugger: Box<dyn Debugger>) {
    DEBUGGER.with(|d| {
        d.replace(new_debugger);
    });
}

#[cfg(not(feature = "debugger"))]
pub fn swap_debugger(_: Box<dyn Debugger>) {}

#[cfg(feature = "debugger")]
pub fn trigger_break(psx: &mut Psx) {
    DEBUGGER.with(|d| {
        d.borrow_mut().trigger_break(psx);
    });
}

#[cfg(not(feature = "debugger"))]
pub fn trigger_break(_: &mut Psx) {}

#[cfg(feature = "debugger")]
pub fn pc_change(psx: &mut Psx) {
    DEBUGGER.with(|d| {
        d.borrow_mut().pc_change(psx);
    });
}

thread_local! {
    pub static PREV_REGS: RefCell<[u32; 32]> = RefCell::new([0; 32]);
}

#[cfg(not(feature = "debugger"))]
pub fn pc_change(psx: &mut Psx) {
    let delta = psx.cycle_counter - psx.prev_cycle_counter;
    psx.prev_cycle_counter = psx.cycle_counter;

    print!("?P {:x} {}", psx.cpu.current_pc(), delta);

    PREV_REGS.with(|r| {
        let mut prev = r.borrow_mut();
        let cur = psx.cpu.regs();

        for i in 0..32 {
            if prev[i] != cur[i] {
                print!(" {} {:x}", crate::psx::cpu::REGISTER_NAMES[i], cur[i]);
                prev[i] = cur[i];
            }
        }
    });

    println!();
}

#[cfg(feature = "debugger")]
pub fn memory_read(psx: &mut Psx, addr: u32) {
    DEBUGGER.with(|d| {
        d.borrow_mut().memory_read(psx, addr);
    });
}

#[cfg(not(feature = "debugger"))]
pub fn memory_read(_: &mut Psx, _: u32) {}

#[cfg(feature = "debugger")]
pub fn memory_write(psx: &mut Psx, addr: u32) {
    DEBUGGER.with(|d| {
        d.borrow_mut().memory_write(psx, addr);
    });
}

#[cfg(not(feature = "debugger"))]
pub fn memory_write(_: &mut Psx, _: u32) {}
