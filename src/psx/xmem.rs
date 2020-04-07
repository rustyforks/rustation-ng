use super::bios::BIOS_SIZE;
/// Optimized data structure holding the parts of the PSX address space that can contain executable
/// code.
use super::{cpu, Addressable};

/// This structure manages all executable portions of memory and offers fast lookup to speed up
/// instruction fetches.
///
/// This assumes that these regions behave like actual memory and not hardware register: reading
/// these locations has no side-effect. This is obviously true for the RAM and BIOS but may not be
/// the case for Expansion memory, so that may cause compatibility issues if we ever need to
/// implement support for extensions that associate side-effects to instruction fetches. I don't
/// know if such extensions exist.
pub struct XMemory {
    /// We currently only support executing from RAM and BIOS so we need three concrete pages:
    ///
    /// * The RAM page
    /// * The BIOS page (the first 512KiB contain the BIOS, the rest is padded with 0xff)
    /// * The "bad" page that's filled with 0xff and used as placeholder for all pages that are not
    ///   executable.
    memory: Box<[u8; PAGE_SIZE_BYTES * 3]>,
    /// Look up table containing PAGE_SIZE offsets in `memory` for all pages in the system
    offset_lut: [u8; PAGE_COUNT],
}

impl XMemory {
    pub fn new() -> XMemory {
        let mut xmem = XMemory {
            memory: box_array![0xff; PAGE_SIZE_BYTES * 3],
            offset_lut: [MemoryPage::Bad as u8; PAGE_COUNT],
        };

        // Remap executable pages
        for &region in &REGION_OFFSETS {
            // RAM: mirrored 4 times
            for i in 0..4 {
                xmem.remap(region + i * RAM_SIZE as u32, MemoryPage::Ram);
            }
            // BIOS
            xmem.remap(region + 0x1fc0_0000, MemoryPage::Bios);
        }

        xmem
    }

    /// Make `offset_lut` point at `target` for memory address `addr`
    fn remap(&mut self, addr: u32, target: MemoryPage) {
        let page = (addr >> PAGE_SHIFT) as usize;

        self.offset_lut[page] = target as u8;
    }

    /// Set the contents of the BIOS
    pub fn set_bios(&mut self, bios: &[u8; BIOS_SIZE]) {
        let bios_base = (MemoryPage::Bios as usize) << PAGE_SHIFT;
        for i in 0..BIOS_SIZE {
            self.memory[bios_base + i] = bios[i];
        }
    }

    /// Fetch data from memory at `offset`
    fn load<T: Addressable>(&self, offset: u32) -> T {
        let offset = offset as usize;

        let mut v = 0;

        for i in 0..T::width() as usize {
            let b = u32::from(self.memory[offset + i]);

            v |= b << (i * 8)
        }

        Addressable::from_u32(v)
    }

    /// Store data to memory at `offset`
    fn store<T: Addressable>(&mut self, offset: u32, val: T) {
        let offset = offset as usize;

        let val = val.as_u32();

        for i in 0..T::width() as usize {
            self.memory[offset + i] = (val >> (i * 8)) as u8;
        }
    }

    /// Read from RAM at `offset`
    pub fn ram_load<T: Addressable>(&self, offset: u32) -> T {
        let ram_base = (MemoryPage::Ram as u32) << PAGE_SHIFT;

        // The two MSBs are ignored, the 2MB RAM is mirrored four times over the first 8MB of
        // address space
        let offset = offset & 0x1f_ffff;

        self.load(ram_base + offset)
    }

    /// Write `val` to RAM at `offset`
    pub fn ram_store<T: Addressable>(&mut self, offset: u32, val: T) {
        let ram_base = (MemoryPage::Ram as u32) << PAGE_SHIFT;

        // The two MSBs are ignored, the 2MB RAM is mirrored four times over the first 8MB of
        // address space
        let offset = offset & 0x1f_ffff;

        self.store(ram_base + offset, val);
    }

    /// Read from BIOS at `offset`
    pub fn bios_load<T: Addressable>(&self, offset: u32) -> T {
        let bios_base = (MemoryPage::Bios as u32) << PAGE_SHIFT;

        self.load(bios_base + offset)
    }

    /// Fetch instruction at absolute address `addr`
    pub fn load_instruction(&self, addr: u32) -> cpu::Instruction {
        let page = addr >> PAGE_SHIFT;

        let mem_page = self.offset_lut[page as usize] as u32;

        let mut offset = mem_page << PAGE_SHIFT;

        offset += addr & ((1 << PAGE_SHIFT) - 1);

        let mut v = 0;
        for i in 0..4 {
            let b = u32::from(self.memory[(offset + i) as usize]);

            v |= b << (i * 8)
        }

        cpu::Instruction::new(v)
    }
}

/// Order of the pages in `XMemory::memory`
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum MemoryPage {
    Ram = 0,
    Bios = 1,
    Bad = 2,
}

/// Defines how big each cache page will be (log2 since it's a shift value).
///
/// I use 2MB since it's the largest usable size: anything bigger won't let us address the RAM
/// properly since it's supposed to be mirrored 4 times contiguously.
///
/// This means that pages are actually larger than the full BIOS but that's not a problem since
/// there are no other executable regions within 2MB of the BIOS address space, so we can just pad
/// with dummy bytes.
const PAGE_SHIFT: u8 = 21;

/// Page size in bytes
const PAGE_SIZE_BYTES: usize = 1 << PAGE_SHIFT;

/// Total number of pages to cover the full 32bit address space
const PAGE_COUNT: usize = 1 << (32 - PAGE_SHIFT);

/// Offsets for the three memory regions containing executable code: KUSEG, KSEG0 and KSEG1
const REGION_OFFSETS: [u32; 3] = [0x0000_0000, 0x8000_0000, 0xa000_0000];

/// System RAM: 2MB
const RAM_SIZE: usize = 2 * 1024 * 1024;
