use super::Psx;

use std::fmt;

pub struct Cpu {
    /// The Program Counter register: points to the next instruction
    pc: u32,
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            // Reset value for the PC: beginning of BIOS ROM
            pc: 0xbfc0_0000,
        }
    }
}

pub fn run_next_instruction(psx: &mut Psx) {
    let pc = psx.cpu.pc;

    // Point to the next instruction. MIPS instructions are always exactly 4bytes long.
    psx.cpu.pc += 4;

    // Unaligned PC should trigger an exception
    assert!(pc % 4 == 0);

    let instruction = Instruction(psx.load(pc));

    let handler = OPCODE_HANDLERS[instruction.opcode()];

    handler(psx, instruction);
}

/// A single MIPS instruction wrapper to make decoding easier
#[derive(Clone, Copy)]
pub struct Instruction(u32);

impl Instruction {
    /// Return bits [31:26] of the instruction
    fn opcode(self) -> usize {
        let Instruction(op) = self;

        (op >> 26) as usize
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0x{:08x}", self.0)
    }
}

/// Placeholder while we haven't implemented all opcodes
fn op_unimplemented(_psx: &mut Psx, instruction: Instruction) {
    panic!(
        "Encountered unimplemented instruction {} (opcode: 0x{:x})",
        instruction,
        instruction.opcode()
    );
}

/// Handler table for the main opcodes (instruction bits [31:26])
const OPCODE_HANDLERS: [fn(&mut Psx, Instruction); 64] = [
    // 0x00
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    // 0x10
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    // 0x20
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    // 0x30
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
];
