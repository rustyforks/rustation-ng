use super::{cop0, Addressable, Psx};

use std::fmt;

pub struct Cpu {
    /// The Program Counter register: points to the next instruction
    pc: u32,
    /// Next value for the PC, used to emulate the branch delay slot
    next_pc: u32,
    /// General Purpose Registers. The first entry (R0) must always contain 0
    regs: [u32; 32],
    /// Load initiated by the current instruction (will take effect after the load delay slot)
    load: (RegisterIndex, u32),
}

impl Cpu {
    pub fn new() -> Cpu {
        // Reset value for the PC: beginning of BIOS ROM
        let reset_pc = 0xbfc0_0000;

        Cpu {
            pc: reset_pc,
            next_pc: reset_pc.wrapping_add(4),
            // Not sure what the reset values of the general purpose registers is but it shouldn't
            // matter since the BIOS doesn't read them. R0 is always 0 however, so that shoudn't be
            // changed.
            regs: [0; 32],
            load: (RegisterIndex(0), 0),
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

    /// Branch to immediate value `offset`.
    fn branch(&mut self, offset: u32) {
        // Offset immediates are always shifted two places to the
        // right since `PC` addresses have to be aligned on 32bits at
        // all times.
        let offset = offset << 2;

        self.next_pc = self.pc.wrapping_add(offset);
    }

    /// Execute and clear any pending load
    fn delayed_load(&mut self) {
        // Execute the pending load (if any, otherwise it will load `R0` which is a NOP).
        let (reg, val) = self.load;

        self.set_reg(reg, val);

        // We reset the load to target register 0 for the next instruction
        self.load = (RegisterIndex(0), 0);
    }

    /// Execute the pending delayed and setup the next one. If the new load targets the same
    /// register as the current one then the older one is cancelled (i.e. it never makes it to the
    /// register).
    ///
    /// This method should be used instead of `delayed_load` for instructions that setup a delayed
    /// load.
    fn delayed_load_chain(&mut self, reg: RegisterIndex, val: u32) {
        let (pending_reg, pending_val) = self.load;

        // This takes care of the following situation:
        //
        //    lw   $t0, 0($s0)
        //    lw   $t0, 0($s1)
        //    move $t0, $s1
        //
        // In this situation the 2nd LW targets the same register an the one just before. In this
        // scenario the first load never completes and the value of T0 in the move won't have been
        // modified by either LW (the first one being interrupted by the second one, and the second
        // one not having yet finished since we're in the delay slot).
        if pending_reg != reg {
            self.set_reg(pending_reg, pending_val);
        }

        self.load = (reg, val);
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
    println!("{:?}", psx.cpu);

    // Explanation of the various *pc variables:
    //
    // * `current_pc`: Pointer to the instruction about to be executed.
    //
    // * `psx.cpu.pc`: Pointer to the next instruction to be executed. It's possible for this value
    //                 to change before the next instruction is reached if an exception occurs
    //                 (exceptions have no delay slot).
    //
    // * `psx.cpu.next_pc`: Value `psx.cpu.pc` will take on the *next* cycle, so effectively a
    //                      pointer to the next next instruction being executed. It's possible for
    //                      this value to change before the next instruction is reached if an
    //                      exception *or* a branch/jump occurs. We can't change `psx.cpu.pc`
    //                      directly in case of a branch because we need to emulate the branch
    //                      delay slot.
    //
    // So basically when a branch/jump is executed only `psx.cpu.next_pc` is modified, which means
    // that the value of the next instruction to be executed (pointed at by `psx.cpu.pc`) remains
    // in the pipeline. Thus the branch delay slot is emulated accurately.
    let current_pc = psx.cpu.pc;
    psx.cpu.pc = psx.cpu.next_pc;
    psx.cpu.next_pc = psx.cpu.pc.wrapping_add(4);

    // Unaligned PC should trigger an exception
    assert!(current_pc % 4 == 0);

    let instruction = Instruction(psx.load(current_pc));

    println!("About to execute {}...", instruction);

    let handler = OPCODE_HANDLERS[instruction.opcode()];

    handler(psx, instruction);
}

/// Execute a memory write
fn store<T: Addressable>(psx: &mut Psx, addr: u32, v: T) {
    if psx.cop0.cache_isolated() {
        // If the cache is isolated then the write should go to cache maintenance instead of going
        // to the other modules. Since we don't yet implement the instruction cache we can just
        // ignore it and return.
        return;
    }

    psx.store(addr, v);
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

    psx.cpu.delayed_load();

    psx.cpu.set_reg(d, v);
}

/// Add Unsigned
fn op_addu(psx: &mut Psx, instruction: Instruction) {
    let s = instruction.s();
    let t = instruction.t();
    let d = instruction.d();

    let v = psx.cpu.reg(s).wrapping_add(psx.cpu.reg(t));

    psx.cpu.delayed_load();

    psx.cpu.set_reg(d, v);
}

/// Bitwise Or
fn op_or(psx: &mut Psx, instruction: Instruction) {
    let d = instruction.d();
    let s = instruction.s();
    let t = instruction.t();

    let v = psx.cpu.reg(s) | psx.cpu.reg(t);

    psx.cpu.delayed_load();

    psx.cpu.set_reg(d, v);
}

/// Set on Less Than Unsigned
fn op_sltu(psx: &mut Psx, instruction: Instruction) {
    let d = instruction.d();
    let s = instruction.s();
    let t = instruction.t();

    let v = psx.cpu.reg(s) < psx.cpu.reg(t);

    psx.cpu.delayed_load();

    psx.cpu.set_reg(d, v as u32);
}

/// Jump
fn op_j(psx: &mut Psx, instruction: Instruction) {
    let target = instruction.imm_jump();

    // In order to fit the immediate target in the instruction the bottom two bits are stripped
    // (see the implementation of `imm_jump`) but that still only leaves 26 bits to store 30 bits.
    // As a workaround the 4 MSBs are simply copied from the PC. That means that the effective
    // range of this instruction is limited and it can't reach any location in memory, in
    // particular it can't be used to switch from one area to an other (like, say, from KUSEG to
    // KSEG0).
    psx.cpu.next_pc = (psx.cpu.pc & 0xf000_0000) | target;

    psx.cpu.delayed_load();
}

/// Branch if Not Equal
fn op_bne(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se();
    let s = instruction.s();
    let t = instruction.t();

    if psx.cpu.reg(s) != psx.cpu.reg(t) {
        psx.cpu.branch(i);
    }

    psx.cpu.delayed_load();
}

/// Add Immediate and check for signed overflow
fn op_addi(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se() as i32;
    let t = instruction.t();
    let s = instruction.s();

    let s = psx.cpu.reg(s) as i32;

    psx.cpu.delayed_load();

    match s.checked_add(i) {
        Some(v) => psx.cpu.set_reg(t, v as u32),
        None => panic!("ADDI overflowed!"),
    }
}

/// Add Immediate Unsigned
fn op_addiu(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = instruction.t();
    let s = instruction.s();

    let v = psx.cpu.reg(s).wrapping_add(i);

    psx.cpu.delayed_load();

    psx.cpu.set_reg(t, v);
}

/// Bitwise Or Immediate
fn op_ori(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm();
    let t = instruction.t();
    let s = instruction.s();

    let v = psx.cpu.reg(s) | i;

    psx.cpu.delayed_load();

    psx.cpu.set_reg(t, v);
}

/// Load Upper Immediate
fn op_lui(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm();
    let t = instruction.t();

    // Low 16bits are set to 0
    let v = i << 16;

    psx.cpu.delayed_load();

    psx.cpu.set_reg(t, v);
}

/// Coprocessor 0 opcode
fn op_cop0(psx: &mut Psx, instruction: Instruction) {
    match instruction.cop_opcode() {
        0b00000 => panic!("Implement MFC0"),
        0b00100 => op_mtc0(psx, instruction),
        0b10000 => panic!("Implement RFE"),
        _ => panic!("Unhandled cop0 instruction {}", instruction),
    }
}

/// Move To Coprocessor 0
fn op_mtc0(psx: &mut Psx, instruction: Instruction) {
    let cpu_r = instruction.t();
    let cop_r = instruction.d();

    let v = psx.cpu.reg(cpu_r);

    psx.cpu.delayed_load();

    cop0::mtc0(psx, cop_r, v);
}

/// Load Word
fn op_lw(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = instruction.t();
    let s = instruction.s();

    let addr = psx.cpu.reg(s).wrapping_add(i);

    // Address must be 32bit aligned
    if addr % 4 == 0 {
        let v = psx.load(addr);

        psx.cpu.delayed_load_chain(t, v);
    } else {
        psx.cpu.delayed_load();
        panic!("Misaligned lw!");
    }
}

/// Store Word
fn op_sw(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = instruction.t();
    let s = instruction.s();

    let addr = psx.cpu.reg(s).wrapping_add(i);
    let v = psx.cpu.reg(t);

    psx.cpu.delayed_load();

    // Address must be 32bit aligned
    if addr % 4 == 0 {
        store(psx, addr, v);
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

    /// Return coprocessor opcode in bits [25:21]
    fn cop_opcode(self) -> u32 {
        let Instruction(op) = self;

        (op >> 21) & 0x1f
    }

    /// Return immediate value in bits [16:0]
    fn imm(self) -> u32 {
        let Instruction(op) = self;

        op & 0xffff
    }

    /// Jump target stored in bits [25:0].
    fn imm_jump(self) -> u32 {
        let Instruction(op) = self;

        // The two LSBs aren't stored since (due to alignment constraints) they're assumed to be 0.
        (op & 0x3ff_ffff) << 2
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
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct RegisterIndex(pub u32);

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
    op_j,
    op_unimplemented,
    op_unimplemented,
    op_bne,
    op_unimplemented,
    op_unimplemented,
    op_addi,
    op_addiu,
    op_unimplemented,
    op_unimplemented,
    op_unimplemented,
    op_ori,
    op_unimplemented,
    op_lui,
    // 0x10
    op_cop0,
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
    op_lw,
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
    op_addu,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_or,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_sltu,
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
