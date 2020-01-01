use super::cop0::Exception;
use super::{cop0, debugger, Addressable, Psx};

use std::fmt;

pub struct Cpu {
    /// Address of the instruction currently being executed. Used for
    /// setting the EPC in exceptions.
    current_pc: u32,
    /// The Program Counter register: points to the next instruction
    pc: u32,
    /// Next value for the PC, used to emulate the branch delay slot
    next_pc: u32,
    /// General Purpose Registers. The first entry (R0) must always contain 0
    regs: [u32; 32],
    /// HI register for division remainder and multiplication MSBs
    hi: u32,
    /// LO register for division quotient and multiplication LSBs
    lo: u32,
    /// Load initiated by the current instruction (will take effect after the load delay slot)
    load: (RegisterIndex, u32),
    /// Set by the current instruction if a branch occurred and the next instruction will be in the
    /// delay slot.
    branch: bool,
    /// Set if the current instruction executes in the delay slot
    delay_slot: bool,
    /// If true BREAK instructions trigged the debugger instead of generating an exception
    debug_on_break: bool,
}

impl Cpu {
    pub fn new() -> Cpu {
        // Reset value for the PC: beginning of BIOS ROM
        let reset_pc = 0xbfc0_0000;

        Cpu {
            current_pc: reset_pc,
            pc: reset_pc,
            next_pc: reset_pc.wrapping_add(4),
            // Not sure what the reset values of the general purpose registers is but it shouldn't
            // matter since the BIOS doesn't read them. R0 is always 0 however, so that shoudn't be
            // changed.
            regs: [0; 32],
            hi: 0,
            lo: 0,
            load: (RegisterIndex(0), 0),
            branch: false,
            delay_slot: false,
            debug_on_break: false,
        }
    }

    /// Returns the address of the instruction currently being executed
    pub fn current_pc(&self) -> u32 {
        self.current_pc
    }

    /// Force PC address. Meant to be used from the debugger. Use at your own risk.
    pub fn force_pc(&mut self, pc: u32) {
        self.pc = pc;
        self.next_pc = self.pc.wrapping_add(4);
        self.delay_slot = false;
    }

    /// Returns true if the instruction currently being executed is in a delay slot
    pub fn in_delay_slot(&self) -> bool {
        self.delay_slot
    }

    /// Get the value of all general purpose registers
    pub fn regs(&self) -> &[u32] {
        &self.regs
    }

    /// Get the value of the LO register
    pub fn lo(&self) -> u32 {
        self.lo
    }

    /// Get the value of the HI register
    pub fn hi(&self) -> u32 {
        self.hi
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
        self.branch = true;
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
    // Explanation of the various *pc variables:
    //
    // * `psx.cpucurrent_pc`: Pointer to the instruction about to be executed.
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
    psx.cpu.current_pc = psx.cpu.pc;
    psx.cpu.pc = psx.cpu.next_pc;
    psx.cpu.next_pc = psx.cpu.pc.wrapping_add(4);

    // If the last instruction was a branch then we're in the delay slot
    psx.cpu.delay_slot = psx.cpu.branch;
    psx.cpu.branch = false;

    // Debugger entrypoint: used for code breakpoints and stepping
    debugger::pc_change(psx);

    if psx.cpu.current_pc % 4 != 0 {
        // PC is not correctly aligned!
        exception(psx, Exception::LoadAddressError);
        return;
    }

    let instruction = Instruction(psx.load(psx.cpu.current_pc));

    let handler = OPCODE_HANDLERS[instruction.opcode()];

    handler(psx, instruction);
}

/// Trigger an exception
fn exception(psx: &mut Psx, cause: Exception) {
    // Update the status register
    let handler_addr = cop0::enter_exception(psx, cause);

    // Exceptions don't have a branch delay, we jump directly into
    // the handler
    psx.cpu.pc = handler_addr;
    psx.cpu.next_pc = handler_addr.wrapping_add(4);
}

/// Execute a memory write
fn store<T: Addressable>(psx: &mut Psx, addr: u32, v: T) {
    if psx.cop0.cache_isolated() {
        // If the cache is isolated then the write should go to cache maintenance instead of going
        // to the other modules. Since we don't yet implement the instruction cache we can just
        // ignore it and return.
        return;
    }

    debugger::memory_write(psx, addr);

    psx.store(addr, v);
}

/// Execute a memory read
fn load<T: Addressable>(psx: &mut Psx, addr: u32) -> T {
    debugger::memory_read(psx, addr);

    psx.load(addr)
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

/// Shift Right Logical
fn op_srl(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.shift();
    let t = instruction.t();
    let d = instruction.d();

    let v = psx.cpu.reg(t) >> i;

    psx.cpu.delayed_load();

    psx.cpu.set_reg(d, v);
}

/// Shift Right Arithmetic
fn op_sra(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.shift();
    let t = instruction.t();
    let d = instruction.d();

    let v = (psx.cpu.reg(t) as i32) >> i;

    psx.cpu.delayed_load();

    psx.cpu.set_reg(d, v as u32);
}

/// Shift Left Logical Variable
fn op_sllv(psx: &mut Psx, instruction: Instruction) {
    let d = instruction.d();
    let s = instruction.s();
    let t = instruction.t();

    // Shift amount is truncated to 5 bits
    let v = psx.cpu.reg(t) << (psx.cpu.reg(s) & 0x1f);

    psx.cpu.delayed_load();

    psx.cpu.set_reg(d, v);
}

/// Shift Right Logical Variable
fn op_srlv(psx: &mut Psx, instruction: Instruction) {
    let d = instruction.d();
    let s = instruction.s();
    let t = instruction.t();

    // Shift amount is truncated to 5 bits
    let v = psx.cpu.reg(t) >> (psx.cpu.reg(s) & 0x1f);

    psx.cpu.delayed_load();

    psx.cpu.set_reg(d, v);
}

/// Shift Right Arithmetic Variable
fn op_srav(psx: &mut Psx, instruction: Instruction) {
    let d = instruction.d();
    let s = instruction.s();
    let t = instruction.t();

    // Shift amount is truncated to 5 bits
    let v = (psx.cpu.reg(t) as i32) >> (psx.cpu.reg(s) & 0x1f);

    psx.cpu.delayed_load();

    psx.cpu.set_reg(d, v as u32);
}

/// Jump Register
fn op_jr(psx: &mut Psx, instruction: Instruction) {
    let s = instruction.s();

    psx.cpu.next_pc = psx.cpu.reg(s);
    psx.cpu.branch = true;

    psx.cpu.delayed_load();
}

/// Jump And Link Register
fn op_jalr(psx: &mut Psx, instruction: Instruction) {
    let s = instruction.s();
    let d = instruction.d();

    let ra = psx.cpu.next_pc;

    psx.cpu.next_pc = psx.cpu.reg(s);
    psx.cpu.branch = true;

    psx.cpu.delayed_load();

    // Store return address in `d`
    psx.cpu.set_reg(d, ra);
}

/// System Call
fn op_syscall(psx: &mut Psx, _: Instruction) {
    exception(psx, Exception::SysCall);
}

/// Break
fn op_break(psx: &mut Psx, _: Instruction) {
    if psx.cpu.debug_on_break {
        info!("BREAK instruction while debug_on_break is active");
        debugger::trigger_break(psx);
    } else {
        exception(psx, Exception::Break);
    }
}

/// Move From HI
fn op_mfhi(psx: &mut Psx, instruction: Instruction) {
    let d = instruction.d();

    let hi = psx.cpu.hi;

    psx.cpu.delayed_load();

    psx.cpu.set_reg(d, hi);
}

/// Move to HI
fn op_mthi(psx: &mut Psx, instruction: Instruction) {
    let s = instruction.s();

    psx.cpu.hi = psx.cpu.reg(s);

    psx.cpu.delayed_load();
}

/// Move From LO
fn op_mflo(psx: &mut Psx, instruction: Instruction) {
    let d = instruction.d();

    let lo = psx.cpu.lo;

    psx.cpu.delayed_load();

    psx.cpu.set_reg(d, lo);
}

/// Move to LO
fn op_mtlo(psx: &mut Psx, instruction: Instruction) {
    let s = instruction.s();

    psx.cpu.lo = psx.cpu.reg(s);

    psx.cpu.delayed_load();
}

/// Multiply (signed)
fn op_mult(psx: &mut Psx, instruction: Instruction) {
    let s = instruction.s();
    let t = instruction.t();

    let a = i64::from(psx.cpu.reg(s) as i32);
    let b = i64::from(psx.cpu.reg(t) as i32);

    let res = (a * b) as u64;

    psx.cpu.delayed_load();

    psx.cpu.hi = (res >> 32) as u32;
    psx.cpu.lo = res as u32;
}

/// Multiply Unsigned
fn op_multu(psx: &mut Psx, instruction: Instruction) {
    let s = instruction.s();
    let t = instruction.t();

    let a = u64::from(psx.cpu.reg(s));
    let b = u64::from(psx.cpu.reg(t));

    let res = a * b;

    psx.cpu.delayed_load();

    psx.cpu.hi = (res >> 32) as u32;
    psx.cpu.lo = res as u32;
}

/// Divide (signed)
fn op_div(psx: &mut Psx, instruction: Instruction) {
    let s = instruction.s();
    let t = instruction.t();

    let n = psx.cpu.reg(s) as i32;
    let d = psx.cpu.reg(t) as i32;

    psx.cpu.delayed_load();

    if d == 0 {
        // Division by zero, results are bogus
        psx.cpu.hi = n as u32;

        if n >= 0 {
            psx.cpu.lo = 0xffff_ffff;
        } else {
            psx.cpu.lo = 1;
        }
    } else if n as u32 == 0x8000_0000 && d == -1 {
        // Result is not representable in a 32bit signed integer
        psx.cpu.hi = 0;
        psx.cpu.lo = 0x8000_0000;
    } else {
        psx.cpu.hi = (n % d) as u32;
        psx.cpu.lo = (n / d) as u32;
    }
}

/// Divide Unsigned
fn op_divu(psx: &mut Psx, instruction: Instruction) {
    let s = instruction.s();
    let t = instruction.t();

    let n = psx.cpu.reg(s);
    let d = psx.cpu.reg(t);

    psx.cpu.delayed_load();

    if d == 0 {
        // Division by zero, results are bogus
        psx.cpu.hi = n;
        psx.cpu.lo = 0xffff_ffff;
    } else {
        psx.cpu.hi = n % d;
        psx.cpu.lo = n / d;
    }
}

/// Add and check for signed overflow
fn op_add(psx: &mut Psx, instruction: Instruction) {
    let s = instruction.s();
    let t = instruction.t();
    let d = instruction.d();

    let s = psx.cpu.reg(s) as i32;
    let t = psx.cpu.reg(t) as i32;

    psx.cpu.delayed_load();

    match s.checked_add(t) {
        Some(v) => psx.cpu.set_reg(d, v as u32),
        None => exception(psx, Exception::Overflow),
    }
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

/// Subtract and check for signed overflow
fn op_sub(psx: &mut Psx, instruction: Instruction) {
    let s = instruction.s();
    let t = instruction.t();
    let d = instruction.d();

    let s = psx.cpu.reg(s) as i32;
    let t = psx.cpu.reg(t) as i32;

    psx.cpu.delayed_load();

    match s.checked_sub(t) {
        Some(v) => psx.cpu.set_reg(d, v as u32),
        None => exception(psx, Exception::Overflow),
    }
}

/// Subtract Unsigned
fn op_subu(psx: &mut Psx, instruction: Instruction) {
    let s = instruction.s();
    let t = instruction.t();
    let d = instruction.d();

    let v = psx.cpu.reg(s).wrapping_sub(psx.cpu.reg(t));

    psx.cpu.delayed_load();

    psx.cpu.set_reg(d, v);
}

/// Bitwise And
fn op_and(psx: &mut Psx, instruction: Instruction) {
    let d = instruction.d();
    let s = instruction.s();
    let t = instruction.t();

    let v = psx.cpu.reg(s) & psx.cpu.reg(t);

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

/// Bitwise Exclusive Or
fn op_xor(psx: &mut Psx, instruction: Instruction) {
    let d = instruction.d();
    let s = instruction.s();
    let t = instruction.t();

    let v = psx.cpu.reg(s) ^ psx.cpu.reg(t);

    psx.cpu.delayed_load();

    psx.cpu.set_reg(d, v);
}

/// Bitwise Not Or
fn op_nor(psx: &mut Psx, instruction: Instruction) {
    let d = instruction.d();
    let s = instruction.s();
    let t = instruction.t();

    let v = !(psx.cpu.reg(s) | psx.cpu.reg(t));

    psx.cpu.delayed_load();

    psx.cpu.set_reg(d, v);
}

/// Set on Less Than (signed)
fn op_slt(psx: &mut Psx, instruction: Instruction) {
    let d = instruction.d();
    let s = instruction.s();
    let t = instruction.t();

    let s = psx.cpu.reg(s) as i32;
    let t = psx.cpu.reg(t) as i32;

    let v = s < t;

    psx.cpu.delayed_load();

    psx.cpu.set_reg(d, v as u32);
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

/// Various branch instructions: BGEZ, BLTZ, BGEZAL, BLTZAL. Bits [20:16] are used to figure out
/// which one to use
fn op_bxx(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se();
    let s = instruction.s();

    let instruction = instruction.0;

    let is_bgez = (instruction >> 16) & 1;
    // It's not enough to test for bit 20 to see if we're supposed
    // to link, if any bit in the range [19:17] is set the link
    // doesn't take place and RA is left untouched.
    let is_link = (instruction >> 17) & 0xf == 0x8;

    let v = psx.cpu.reg(s) as i32;

    // Test "less than zero"
    let test = (v < 0) as u32;

    // If the test is "greater than or equal to zero" we need to
    // negate the comparison above ("a >= 0" <=> "!(a < 0)"). The
    // xor takes care of that.
    let test = test ^ is_bgez;

    psx.cpu.delayed_load();

    // If linking is requested it occurs unconditionally, even if
    // the branch is not taken
    if is_link {
        let ra = psx.cpu.next_pc;

        // Store return address in R31
        psx.cpu.set_reg(RegisterIndex(31), ra);
    }

    if test != 0 {
        psx.cpu.branch(i);
    }
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
    psx.cpu.branch = true;

    psx.cpu.delayed_load();
}

/// Jump And Link
fn op_jal(psx: &mut Psx, instruction: Instruction) {
    let ra = psx.cpu.next_pc;
    let target = instruction.imm_jump();

    psx.cpu.next_pc = (psx.cpu.pc & 0xf000_0000) | target;
    psx.cpu.branch = true;

    psx.cpu.delayed_load();

    // Store return address in R31
    psx.cpu.set_reg(RegisterIndex(31), ra);
}

/// Branch if Equal
fn op_beq(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se();
    let s = instruction.s();
    let t = instruction.t();

    if psx.cpu.reg(s) == psx.cpu.reg(t) {
        psx.cpu.branch(i);
    }

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

/// Branch if Less than or Equal to Zero
fn op_blez(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se();
    let s = instruction.s();

    let v = psx.cpu.reg(s) as i32;

    if v <= 0 {
        psx.cpu.branch(i);
    }

    psx.cpu.delayed_load();
}

/// Branch if Greater Than Zero
fn op_bgtz(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se();
    let s = instruction.s();

    let v = psx.cpu.reg(s) as i32;

    if v > 0 {
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
        None => exception(psx, Exception::Overflow),
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

/// Set if Less Than Immediate (signed)
fn op_slti(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se() as i32;
    let s = instruction.s();
    let t = instruction.t();

    let v = (psx.cpu.reg(s) as i32) < i;

    psx.cpu.delayed_load();

    psx.cpu.set_reg(t, v as u32);
}

/// Set if Less Than Immediate Unsigned
fn op_sltiu(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se();
    let s = instruction.s();
    let t = instruction.t();

    let v = psx.cpu.reg(s) < i;

    psx.cpu.delayed_load();

    psx.cpu.set_reg(t, v as u32);
}

/// Bitwise And Immediate
fn op_andi(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm();
    let t = instruction.t();
    let s = instruction.s();

    let v = psx.cpu.reg(s) & i;

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

/// Bitwise eXclusive Or Immediate
fn op_xori(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm();
    let t = instruction.t();
    let s = instruction.s();

    let v = psx.cpu.reg(s) ^ i;

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
        0b00000 => op_mfc0(psx, instruction),
        0b00100 => op_mtc0(psx, instruction),
        0b10000 => op_rfe(psx, instruction),
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

/// Move From Coprocessor 0
fn op_mfc0(psx: &mut Psx, instruction: Instruction) {
    let cpu_r = instruction.t();
    let cop_r = instruction.d();

    let v = cop0::mfc0(psx, cop_r);

    psx.cpu.delayed_load_chain(cpu_r, v);
}

/// Return From Exception. Doesn't actually jump anywhere but tells the coprocessor to return to
/// the mode it was in when the exception occurred.
fn op_rfe(psx: &mut Psx, instruction: Instruction) {
    psx.cpu.delayed_load();

    // There are other instructions with the same encoding but all are virtual memory related and
    // the PlayStation doesn't implement them. Still, let's make sure we're not running buggy code.
    if instruction.0 & 0x3f != 0b01_0000 {
        panic!("Invalid cop0 instruction: {}", instruction);
    }

    cop0::return_from_exception(psx);
}

/// Coprocessor 1 opcode (does not exist on the PlayStation)
fn op_cop1(psx: &mut Psx, _: Instruction) {
    psx.cpu.delayed_load();

    warn!("Encountered Cop1 instruction");

    exception(psx, Exception::CoprocessorError);
}

/// Coprocessor 2 opcode (GTE)
fn op_cop2(_psx: &mut Psx, _instruction: Instruction) {
    panic!("Encountered GTE instruction");
}

/// Coprocessor 3 opcode (does not exist on the PlayStation)
fn op_cop3(psx: &mut Psx, _: Instruction) {
    psx.cpu.delayed_load();

    warn!("Encountered Cop3 instruction");

    exception(psx, Exception::CoprocessorError);
}

/// Load Byte (signed)
fn op_lb(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = instruction.t();
    let s = instruction.s();

    let addr = psx.cpu.reg(s).wrapping_add(i);

    // Cast as i8 to force sign extension
    let v = load::<u8>(psx, addr) as i8;

    // Put the load in the delay slot
    psx.cpu.delayed_load_chain(t, v as u32);
}

/// Load Halfword (signed)
fn op_lh(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = instruction.t();
    let s = instruction.s();

    let addr = psx.cpu.reg(s).wrapping_add(i);

    if addr % 2 == 0 {
        // Cast as i16 to force sign extension
        let v = load::<u16>(psx, addr) as i16;

        // Put the load in the delay slot
        psx.cpu.delayed_load_chain(t, v as u32);
    } else {
        psx.cpu.delayed_load();
        exception(psx, Exception::LoadAddressError);
    }
}

/// Load Word Left
fn op_lwl(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = instruction.t();
    let s = instruction.s();

    let addr = psx.cpu.reg(s).wrapping_add(i);

    // This instruction bypasses the load delay restriction: this instruction will merge the new
    // contents with the value currently being loaded if need be.
    let (pending_reg, pending_value) = psx.cpu.load;

    let cur_v = if pending_reg == t {
        pending_value
    } else {
        psx.cpu.reg(t)
    };

    // Next we load the *aligned* word containing the first addressed byte
    let aligned_addr = addr & !3;
    let aligned_word: u32 = load(psx, aligned_addr);

    // Depending on the address alignment we fetch the 1, 2, 3 or 4 *most* significant bytes and
    // put them in the target register.
    let v = match addr & 3 {
        0 => (cur_v & 0x00ff_ffff) | (aligned_word << 24),
        1 => (cur_v & 0x0000_ffff) | (aligned_word << 16),
        2 => (cur_v & 0x0000_00ff) | (aligned_word << 8),
        3 => aligned_word,
        _ => unreachable!(),
    };

    // Put the load in the delay slot
    psx.cpu.delayed_load_chain(t, v);
}

/// Load Word
fn op_lw(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = instruction.t();
    let s = instruction.s();

    let addr = psx.cpu.reg(s).wrapping_add(i);

    // Address must be 32bit aligned
    if addr % 4 == 0 {
        let v = load(psx, addr);

        psx.cpu.delayed_load_chain(t, v);
    } else {
        psx.cpu.delayed_load();
        exception(psx, Exception::LoadAddressError);
    }
}

/// Load Byte Unsigned
fn op_lbu(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = instruction.t();
    let s = instruction.s();

    let addr = psx.cpu.reg(s).wrapping_add(i);

    let v = load::<u8>(psx, addr);

    // Put the load in the delay slot
    psx.cpu.delayed_load_chain(t, u32::from(v));
}

/// Load Halfword Unsigned
fn op_lhu(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = instruction.t();
    let s = instruction.s();

    let addr = psx.cpu.reg(s).wrapping_add(i);

    // Address must be 16bit aligned
    if addr % 2 == 0 {
        let v = load::<u16>(psx, addr);

        // Put the load in the delay slot
        psx.cpu.delayed_load_chain(t, u32::from(v));
    } else {
        psx.cpu.delayed_load();
        exception(psx, Exception::LoadAddressError);
    }
}

/// Load Word Right
fn op_lwr(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = instruction.t();
    let s = instruction.s();

    let addr = psx.cpu.reg(s).wrapping_add(i);

    // This instruction bypasses the load delay restriction: this instruction will merge the new
    // contents with the value currently being loaded if need be.
    let (pending_reg, pending_value) = psx.cpu.load;

    let cur_v = if pending_reg == t {
        pending_value
    } else {
        psx.cpu.reg(t)
    };

    // Next we load the *aligned* word containing the first addressed byte
    let aligned_addr = addr & !3;
    let aligned_word: u32 = load(psx, aligned_addr);

    // Depending on the address alignment we fetch the 1, 2, 3 or 4 *least* significant bytes and
    // put them in the target register.
    let v = match addr & 3 {
        0 => aligned_word,
        1 => (cur_v & 0xff00_0000) | (aligned_word >> 8),
        2 => (cur_v & 0xffff_0000) | (aligned_word >> 16),
        3 => (cur_v & 0xffff_ff00) | (aligned_word >> 24),
        _ => unreachable!(),
    };

    // Put the load in the delay slot
    psx.cpu.delayed_load_chain(t, v);
}

/// Store Byte
fn op_sb(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = instruction.t();
    let s = instruction.s();

    let addr = psx.cpu.reg(s).wrapping_add(i);
    let v = psx.cpu.reg(t);

    psx.cpu.delayed_load();

    store(psx, addr, v as u8);
}

/// Store Halfword
fn op_sh(psx: &mut Psx, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = instruction.t();
    let s = instruction.s();

    let addr = psx.cpu.reg(s).wrapping_add(i);
    let v = psx.cpu.reg(t);

    psx.cpu.delayed_load();

    // Address must be 16bit aligned
    if addr % 2 == 0 {
        store(psx, addr, v as u16);
    } else {
        exception(psx, Exception::StoreAddressError);
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
        exception(psx, Exception::StoreAddressError);
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
    op_bxx,
    op_j,
    op_jal,
    op_beq,
    op_bne,
    op_blez,
    op_bgtz,
    op_addi,
    op_addiu,
    op_slti,
    op_sltiu,
    op_andi,
    op_ori,
    op_xori,
    op_lui,
    // 0x10
    op_cop0,
    op_cop1,
    op_cop2,
    op_cop3,
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
    op_lb,
    op_lh,
    op_lwl,
    op_lw,
    op_lbu,
    op_lhu,
    op_lwr,
    op_unimplemented,
    op_sb,
    op_sh,
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
    op_srl,
    op_sra,
    op_sllv,
    op_unimplemented_function,
    op_srlv,
    op_srav,
    op_jr,
    op_jalr,
    op_unimplemented_function,
    op_unimplemented_function,
    op_syscall,
    op_break,
    op_unimplemented_function,
    op_unimplemented_function,
    // 0x10
    op_mfhi,
    op_mthi,
    op_mflo,
    op_mtlo,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_mult,
    op_multu,
    op_div,
    op_divu,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    op_unimplemented_function,
    // 0x20
    op_add,
    op_addu,
    op_sub,
    op_subu,
    op_and,
    op_or,
    op_xor,
    op_nor,
    op_unimplemented_function,
    op_unimplemented_function,
    op_slt,
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
