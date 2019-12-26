use super::Psx;

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

    let instruction: u32 = psx.load(pc);

    panic!("Execute instruction {:x}", instruction);
}
