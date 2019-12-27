use super::Psx;

use std::fmt;

pub struct Cpu {
    /// The Program Counter register: points to the next instruction
    pc: u32,
    /// General Purpose Registers. The first entry (R0) must always contain 0
    regs: [u32; 32],
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            // Reset value for the PC: beginning of BIOS ROM
            pc: 0xbfc0_0000,
            // Not sure what the reset values of the general purpose registers is but it shouldn't
            // matter since the BIOS doesn't read them. R0 is always 0 however, so that shoudn't be
            // changed.
            regs: [0; 32],
        }
    }

    /// Return the current value of register `index`
    fn reg(&self, index: RegisterIndex) -> u32 {
        self.regs[index.0 as usize]
    }

    /// Put `val` into register `index`. If `index` is 0 nothing happens as R0 always contains 0.
    fn set_reg(&mut self, index: RegisterIndex, val: u32) {
        self.regs[index.0 as usize] = val;

        // R0 always contains 0
        self.regs[0] = 0;
    }
}

impl fmt::Debug for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f)?;
        writeln!(f, "PC: 0x{:08x}", self.pc)?;

        for i in 0..16 {
            writeln!(
                f,
                "{}: 0x{:08x}    {}: 0x{:08x}",
                REGISTER_NAMES[i],
                self.regs[i],
                REGISTER_NAMES[i + 16],
                self.regs[i + 16]
            )?;
        }

        Ok(())
    }
}

pub fn run_next_instruction(psx: &mut Psx) {
    let pc = psx.cpu.pc;

    // Point to the next instruction. MIPS instructions are always exactly 4bytes long.
    psx.cpu.pc += 4;

    // Unaligned PC should trigger an exception
    assert!(pc % 4 == 0);

    let instruction = Instruction(psx.load(pc));

    println!("{:?}", psx.cpu);
    println!("About to execute {}...", instruction);

    let handler = OPCODE_HANDLERS[instruction.opcode()];

    handler(psx, instruction);
}

/// When the main opcode is 0 we need to dispatch through a secondary table based on bits [5:0] of
/// the instruction
fn op_function(psx: &mut Psx, instruction: Instruction) {
    let handler = FUNCTION_HANDLERS[instruction.function()];

    handler(psx, instruction);
}

/// Shift Left Logical
///
/// `SLL $r0, $r0, 0` (machine code 0x0000_0000) is the idiomatic way of encoding a NOP
fn op_sll(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.shift();
    let t = instruction.t();
    let d = instruction.d();

    let v = psx.cpu.reg(t) << i;

    psx.cpu.set_reg(d, v);
}

/// Add Immediate Unsigned
fn op_addiu(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = instruction.t();
    let s = instruction.s();

    let v = psx.cpu.reg(s).wrapping_add(i);

    psx.cpu.set_reg(t, v);
}

/// Bitwise Or Immediate
fn op_ori(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm();
    let t = instruction.t();
    let s = instruction.s();

    let v = psx.cpu.reg(s) | i;

    psx.cpu.set_reg(t, v);
}

/// Load Upper Immediate
fn op_lui(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm();
    let t = instruction.t();

    // Low 16bits are set to 0
    let v = i << 16;

    psx.cpu.set_reg(t, v);
}

/// Store Word
fn op_sw(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = instruction.t();
    let s = instruction.s();

    let addr = psx.cpu.reg(s).wrapping_add(i);
    let v = psx.cpu.reg(t);

    // Address must be 32bit aligned
    if addr % 4 == 0 {
        psx.store(addr, v);
    } else {
        // XXX Should trigger an exception
        panic!("Misaligned sw!");
    }
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

    /// Return bits [5:0] of the instruction
    fn function(self) -> usize {
        let Instruction(op) = self;

        (op & 0x3f) as usize
    }

    /// Return immediate value in bits [16:0]
    fn imm(self) -> u32 {
        let Instruction(op) = self;

        op & 0xffff
    }

    /// Return immediate value in bits [16:0] as a sign-extended 32bit
    /// value
    fn imm_se(self) -> u32 {
        let Instruction(op) = self;

        let v = (op & 0xffff) as i16;

        v as u32
    }

    /// Shift Immediate values are stored in bits [10:6]
    fn shift(self) -> u32 {
        let Instruction(op) = self;

        (op >> 6) & 0x1f
    }

    /// Return register index in bits [25:21]
    fn s(self) -> RegisterIndex {
        let Instruction(op) = self;

        RegisterIndex((op >> 21) & 0x1f)
    }

    /// Return register index in bits [20:16]
    fn t(self) -> RegisterIndex {
        let Instruction(op) = self;

        RegisterIndex((op >> 16) & 0x1f)
    }

    /// Return register index in bits [15:11]
    fn d(self) -> RegisterIndex {
        let Instruction(op) = self;

        RegisterIndex((op >> 11) & 0x1f)
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0x{:08x}", self.0)
    }
}

/// A simple wrapper around a register index to avoid coding errors where the register index could
/// be used instead of its value
#[derive(Clone, Copy)]
struct RegisterIndex(u32);

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
    op_function,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_addiu,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_ori,
    op_unimplemented,
    op_lui,
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
    op_sw,
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

/// Placeholder while we haven't implemented all opcodes
fn op_unimplemented_function(_psx: &mut Psx, instruction: Instruction) {
    panic!(
        "Encountered unimplemented instruction {} (function: 0x{:x})",
        instruction,
        instruction.function()
    );
}

/// Handler table for the function codes (instruction bits [31:26] when opcode is 0)
const FUNCTION_HANDLERS: [fn(&mut Psx, Instruction); 64] = [
    // 0x00
    op_sll,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    // 0x10
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    // 0x20
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    // 0x30
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
];

/// Conventional names given to the MIPS registers
const REGISTER_NAMES: [&str; 32] = [
    "r0", // Hardwired to be always 0
    "at", // Assembler Temporary (reserved for the assembler)
    "v0", "v1", // First and second return values
    "a0", "a1", "a2", "a3", // First four function arguments
    "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", // Temporary registers
    "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", // Saved registers
    "t8", "t9", // Temporary registers
    "k0", "k1", // Reserved for kernel use
    "gp", // Global pointer (not normally used on the PSX)
    "sp", // Stack Pointer
    "fp", // Frame Pointer
    "ra", // Return address
];
