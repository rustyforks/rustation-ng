//! MIPS Coprocessor 0
//!
//! Unlike the GTE (Coprocessor 2) this one is part of standard (and required) MIPS. On the
//! PlayStation it mainly deals with exceptions (generated by interrupts or software). It also
//! provide facilities for cache management (used to clean the instruction cache for instance) and
//! breakpoint registers.
//!
//! It's also the coprocessor that's supposed to manage virtual memory but there's no such thing on
//! the PSX.

use super::cpu::RegisterIndex;
use super::Psx;

pub fn mtc0(_psx: &mut Psx, cop0_r: RegisterIndex, v: u32) {
    panic!("Unhandled MTC0 {} <- 0x{:x}", cop0_r.0, v);
}
