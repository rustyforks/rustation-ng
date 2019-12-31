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
    /// Cop0 register 13: Cause register
    cause: u32,
    /// Cop0 register 14: Exception PC
    epc: u32,
}

impl Cop0 {
    pub fn new() -> Cop0 {
        Cop0 {
            sr: 0,
            cause: 0,
            epc: 0,
        }
    }

    /// Return the value of the SR register
    pub fn sr(&self) -> u32 {
        self.sr
    }

    /// Returns true if the cache is isolated.
    pub fn cache_isolated(&self) -> bool {
        self.sr & 0x10000 != 0
    }

    /// Return the value of the BAD register
    pub fn bad(&self) -> u32 {
        // XXX we don't emulate the "BAD" cop0 register yet. It's almost useless in the PSX anyway
        // since there's no MMU.
        0
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

/// Move From Coprocessor 0
pub fn mfc0(psx: &mut Psx, cop_r: RegisterIndex) -> u32 {
    match cop_r.0 {
        6 => {
            // No$ says this register "randomly" memorizes a jump target after certain exceptions
            // occur. Doesn't seem very useful and would require a lot more testing to implement
            // accurately.
            warn!("Unhandled read from JUMP_DEST (cop0r6)");
            0
        }
        7 => {
            // DCIC: breakpoint control
            warn!("Unhandled read from DCIC (cop0r7)");
            0
        }
        8 => {
            // This register should be mostly useless on the PlayStation since it doesn't have
            // virtual memory, however some exceptions do write to this register so maybe it's
            // worth implementing better
            warn!("Unhandled read from BAD_VADDR (cop0r8)");
            0
        }
        12 => psx.cop0.sr,
        13 => cause(psx),
        14 => psx.cop0.epc,
        15 => PROCESSOR_ID,
        _ => {
            warn!("Unhandled read from COP0 register {}", cop_r.0);
            0
        }
    }
}

/// Called when the CPU is about to enter an exception handler. Returns the address of the handler
/// that should be used.
pub fn enter_exception(psx: &mut Psx, cause: Exception) -> u32 {
    // Shift bits [5:0] of `SR` two places to the left. Those bits
    // are three pairs of Interrupt Enable/User Mode bits behaving
    // like a stack 3 entries deep. Entering an exception pushes a
    // pair of zeroes by left shifting the stack which disables
    // interrupts and puts the CPU in kernel mode. The original
    // third entry is discarded (it's up to the kernel to handle
    // more than two recursive exception levels).
    let cop0 = &mut psx.cop0;
    let pc = psx.cpu.current_pc();

    let mode = cop0.sr & 0x3f;

    cop0.sr &= !0x3f;
    cop0.sr |= (mode << 2) & 0x3f;

    // Update `CAUSE` register with the exception code (bits
    // [6:2])
    cop0.cause &= !0x7c;
    cop0.cause |= (cause as u32) << 2;

    if psx.cpu.in_delay_slot() {
        // When an exception occurs in a delay slot `EPC` points
        // to the branch instruction and bit 31 of `CAUSE` is set.
        cop0.epc = pc.wrapping_sub(4);
        cop0.cause |= 1 << 31;
    } else {
        cop0.epc = pc;
        cop0.cause &= !(1 << 31);
    }

    // The address of the exception handler address depends on the
    // value of the BEV bit in SR
    if (cop0.sr & (1 << 22)) != 0 {
        0xbfc0_0180
    } else {
        0x8000_0080
    }
}

pub fn cause(psx: &mut Psx) -> u32 {
    psx.cop0.cause
}

/// Exception types (as stored in the `CAUSE` register)
#[derive(Clone, Copy, Debug)]
#[allow(unused)]
pub enum Exception {
    /// Interrupt Request
    Interrupt = 0x0,
    /// Address error on load
    LoadAddressError = 0x4,
    /// Address error on store
    StoreAddressError = 0x5,
    /// System call (caused by the SYSCALL opcode)
    SysCall = 0x8,
    /// Breakpoint (caused by the BREAK opcode)
    Break = 0x9,
    /// CPU encountered an unknown instruction
    IllegalInstruction = 0xa,
    /// Unsupported coprocessor operation
    CoprocessorError = 0xb,
    /// Arithmetic overflow
    Overflow = 0xc,
}

/// Value of the "Processor ID" register (Cop0r15). This is the value
/// returned by my SCPH-7502.
pub const PROCESSOR_ID: u32 = 0x0000_0002;
