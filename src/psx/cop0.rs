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

/// Coprocessor 0: System control
pub struct Cop0 {
    /// Cop0 register 12: Status register
    sr: u32,
}

impl Cop0 {
    pub fn new() -> Cop0 {
        Cop0 { sr: 0 }
    }
}

/// Move To Coprocessor 0
pub fn mtc0(psx: &mut Psx, cop_r: RegisterIndex, v: u32) {
    match cop_r.0 {
        // Breakpoints registers
        3 | 5 | 6 | 7 | 9 | 11 => {
            if v != 0 {
                panic!("Unhandled write to cop0r{}: {:08x}", cop_r.0, v)
            }
        }
        12 => psx.cop0.sr = v,
        // Cause register
        13 => {
            if v != 0 {
                panic!("Unhandled write to CAUSE register: {:08x}", v)
            }
        }
        _ => panic!("Unhandled COP0 register {}", cop_r.0),
    }
}
