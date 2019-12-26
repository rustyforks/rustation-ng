mod bios;
mod cpu;
pub mod error;

use std::path::Path;

use self::bios::Bios;
use self::cpu::Cpu;
use self::error::Result;

/// Current state of the emulator
pub struct Psx {
    cpu: Cpu,
    bios: Bios,
}

impl Psx {
    pub fn new(bios_path: &Path) -> Result<Psx> {
        let psx = Psx {
            cpu: Cpu::new(),
            bios: Bios::new(bios_path)?,
        };

        Ok(psx)
    }

    pub fn run(&mut self) {
        loop {
            cpu::run_next_instruction(self);
        }
    }

    pub fn load<T: Addressable>(&mut self, address: u32) -> T {
        let abs_addr = map::mask_region(address);

        if let Some(offset) = map::BIOS.contains(abs_addr) {
            return self.bios.load(offset);
        }

        panic!("Unhandled load at address {:08x}", abs_addr);
    }
}

/// Types of access supported by the PlayStation architecture
#[derive(PartialEq, Eq, Debug)]
pub enum AccessWidth {
    Byte = 1,
    HalfWord = 2,
    Word = 4,
}

/// rait representing the attributes of a primitive addressable
/// memory location.
pub trait Addressable {
    /// Retrieve the width of the access
    fn width() -> AccessWidth;
    /// Build an Addressable value from an u32. If the Addressable is 8 or 16bits wide the MSBs are
    /// discarded to fit.
    fn from_u32(i: u32) -> Self;
    /// Retrieve the value of the Addressable as an u32. If the Addressable is 8 or 16bits wide the
    /// MSBs are padded with 0s.
    fn as_u32(&self) -> u32;
    /// Retrieve the value of the Addressable as an u16. If the Addressable was 8 bit wide the MSBs
    /// are padded with 0s, if it was 32bit wide the MSBs are truncated.
    fn as_u16(&self) -> u16 {
        self.as_u32() as u16
    }
    /// Retrieve the value of the Addressable as an u8. If the Addressable was 16 or 32bit wide the
    /// MSBs are truncated.
    fn as_u8(&self) -> u8 {
        self.as_u32() as u8
    }
}

impl Addressable for u8 {
    fn width() -> AccessWidth {
        AccessWidth::Byte
    }

    fn from_u32(v: u32) -> u8 {
        v as u8
    }

    fn as_u32(&self) -> u32 {
        u32::from(*self)
    }
}

impl Addressable for u16 {
    fn width() -> AccessWidth {
        AccessWidth::HalfWord
    }

    fn from_u32(v: u32) -> u16 {
        v as u16
    }

    fn as_u32(&self) -> u32 {
        u32::from(*self)
    }
}

impl Addressable for u32 {
    fn width() -> AccessWidth {
        AccessWidth::Word
    }

    fn from_u32(v: u32) -> u32 {
        v
    }

    fn as_u32(&self) -> u32 {
        *self
    }
}

mod map {
    /// Mask array used to strip the region bits of the address. The mask is selected using the 3
    /// MSBs of the address so each entry effectively matches 512kB of the address space. KSEG2 is
    /// not touched since it doesn't share anything with the other regions.
    const REGION_MASK: [u32; 8] = [
        // KUSEG: 2048MB
        0xffff_ffff,
        0xffff_ffff,
        0xffff_ffff,
        0xffff_ffff,
        // KSEG0:  512MB
        0x7fff_ffff,
        // KSEG1:  512MB
        0x1fff_ffff,
        // KSEG2: 1024MB
        0xffff_ffff,
        0xffff_ffff,
    ];

    /// Mask a CPU address to remove the region bits.
    pub fn mask_region(addr: u32) -> u32 {
        // Index address space in 512MB chunks
        let index = (addr >> 29) as usize;

        addr & REGION_MASK[index]
    }

    pub struct Range(u32, u32);

    impl Range {
        /// Return `Some(offset)` if addr is contained in `self`
        pub fn contains(self, addr: u32) -> Option<u32> {
            let Range(start, length) = self;

            if addr >= start && addr < start + length {
                Some(addr - start)
            } else {
                None
            }
        }
    }

    /// BIOS ROM. Read-only, significantly slower to access than system RAM
    pub const BIOS: Range = Range(0x1fc0_0000, 512 * 1024);
}
