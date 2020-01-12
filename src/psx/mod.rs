mod bios;
pub mod cop0;
pub mod cpu;
pub mod debugger;
mod dma;
pub mod error;
mod gpu;
mod irq;
mod spu;
mod sync;
mod timers;

use std::path::Path;

use self::error::Result;

pub type CycleCount = i32;

/// Current state of the emulator
pub struct Psx {
    /// Counter of the number of CPU cycles elapsed since an arbitrary point in time. Used as the
    /// reference to synchronize the other modules
    pub cycle_counter: CycleCount,
    /// Set to true when the GPU is done drawing one frame (or one field in interlaced mode)
    pub frame_done: bool,
    pub sync: sync::Synchronizer,
    pub cpu: cpu::Cpu,
    pub cop0: cop0::Cop0,
    pub irq: irq::InterruptState,
    pub ram: Ram,
    pub bios: bios::Bios,
    pub spu: spu::Spu,
    pub dma: dma::Dma,
    pub timers: timers::Timers,
    pub gpu: gpu::Gpu,
    /// Memory control registers
    pub mem_control: [u32; 9],
    /// Contents of the RAM_SIZE register which is probably a configuration register for the memory
    /// controller.
    pub ram_size: u32,
    /// Contents of the CACHE_CONTROL register
    pub cache_control: u32,
}

impl Psx {
    pub fn new(bios_path: &Path) -> Result<Psx> {
        // TODO should be based on the game being run.
        let standard = if true {
            gpu::VideoStandard::Pal
        } else {
            gpu::VideoStandard::Ntsc
        };

        let psx = Psx {
            cycle_counter: 0,
            frame_done: false,
            sync: sync::Synchronizer::new(),
            cpu: cpu::Cpu::new(),
            cop0: cop0::Cop0::new(),
            irq: irq::InterruptState::new(),
            ram: Ram::new(),
            bios: bios::Bios::new(bios_path)?,
            spu: spu::Spu::new(),
            dma: dma::Dma::new(),
            timers: timers::Timers::new(),
            gpu: gpu::Gpu::new(standard),
            mem_control: [0; 9],
            ram_size: 0,
            cache_control: 0,
        };

        Ok(psx)
    }

    pub fn run(&mut self) {
        loop {
            while !sync::is_event_pending(self) {
                cpu::run_next_instruction(self);
            }

            sync::handle_events(self);
        }
    }

    /// Advance the CPU cycle counter by the given number of ticks
    pub fn tick(&mut self, cycles: CycleCount) {
        self.cycle_counter += cycles;
    }

    /// Like load, but tries to minimizes side-effects. Used for debugging.
    pub fn examine<T: Addressable>(&mut self, address: u32) -> T {
        // A bit heavy handed but that shouldn't pose much of a problem since this function should
        // only be used for debugging. Catching unwinds means that it will be harder for the
        // debugger to crash the emulator if it triggers an unhandled edge case (in particular if
        // it attempts to read from some unimplemented memory location).
        use ::std::panic;

        let cc = self.cycle_counter;

        let result = panic::catch_unwind(panic::AssertUnwindSafe(|| self.load(address)));

        // Restore the previous counter to avoid advancing the emulation state with debug reads
        self.cycle_counter = cc;

        let bad_value = Addressable::from_u32(0xfbad_c0de);

        result.unwrap_or(bad_value)
    }

    /// Specialized version of load made specifically to fetch CPU instructions. This is supposed
    /// to be a more streamlined version of `load` for performance reasons.
    pub fn load_instruction(&mut self, address: u32) -> cpu::Instruction {
        let abs_addr = map::mask_region(address);

        let i = {
            if let Some(offset) = map::RAM.contains(abs_addr) {
                self.ram.load(offset)
            } else if let Some(offset) = map::BIOS.contains(abs_addr) {
                self.bios.load(offset)
            } else {
                panic!("Unimplemented instruction fetch from 0x{:x}", address);
            }
        };

        cpu::Instruction::new(i)
    }

    /// Decode `address` and perform the load from the target module. `cc` contains the value of
    /// the CPU cycle counter when the transfer begins and should be updated to contain the value
    /// of the cycle counter when it'll complete.
    pub fn load<T: Addressable>(&mut self, address: u32) -> T {
        let abs_addr = map::mask_region(address);

        if let Some(offset) = map::RAM.contains(abs_addr) {
            self.tick(3);
            return self.ram.load(offset);
        }

        if let Some(offset) = map::BIOS.contains(abs_addr) {
            // XXX Mednafen doesn't add any penalty for BIOS read, which sounds wrong. It's
            // probably not a common-enough occurence to matter
            return self.bios.load(offset);
        }

        if let Some(offset) = map::SPU.contains(abs_addr) {
            if T::width() == AccessWidth::Word {
                self.tick(36);
            } else {
                self.tick(16);
            }

            return spu::load(self, offset);
        }

        if let Some(offset) = map::DMA.contains(abs_addr) {
            self.tick(1);
            return dma::load(self, offset);
        }

        if let Some(offset) = map::TIMERS.contains(abs_addr) {
            self.tick(1);
            return timers::load(self, offset);
        }

        if let Some(offset) = map::GPU.contains(abs_addr) {
            self.tick(1);
            return gpu::load(self, offset);
        }

        if let Some(off) = map::IRQ_CONTROL.contains(abs_addr) {
            self.tick(1);

            let v = match off {
                0 => u32::from(irq::status(self)),
                4 => u32::from(irq::mask(self)),
                _ => panic!("Unhandled IRQ load at address {:08x}", abs_addr),
            };

            // Since the IRQ registers are only 16bit wide the high 32bits are undefined. In
            // practice the high bits appear to maintain the value of the previous load.
            //
            // We could try to emulate this behaviour by keeping track of the previously loaded
            // value and use that but it's unclear if any game relies on this edge case.
            //
            // Mednafen sets the high bits to 0x1f80_0000. It's unclear where that comes from, I
            // assume that some game relies on this value somehow? Let's err on the side of caution
            // and do the same thing.
            return Addressable::from_u32(v | 0x1f80_0000);
        }

        if map::EXPANSION_1.contains(abs_addr).is_some() {
            // No expansion implemented. Returns full ones when no
            // expansion is present
            return Addressable::from_u32(!0);
        }

        if map::CACHE_CONTROL.contains(abs_addr).is_some() {
            if T::width() != AccessWidth::Word {
                panic!("Unhandled cache control access");
            }

            return Addressable::from_u32(self.cache_control);
        }

        if let Some(offset) = map::MEM_CONTROL.contains(abs_addr) {
            if T::width() != AccessWidth::Word {
                panic!("Unhandled MEM_CONTROL {:?} access", T::width());
            }

            let index = (offset >> 2) as usize;

            return Addressable::from_u32(self.mem_control[index]);
        }

        if map::RAM_SIZE.contains(abs_addr).is_some() {
            if T::width() != AccessWidth::Word {
                panic!("Unhandled RAM_SIZE access");
            }

            return Addressable::from_u32(self.ram_size);
        }

        panic!("Unhandled load at address {:08x}", abs_addr);
    }

    /// Decode `address` and perform the store to the target module
    pub fn store<T: Addressable>(&mut self, address: u32, val: T) {
        let abs_addr = map::mask_region(address);

        if let Some(offset) = map::RAM.contains(abs_addr) {
            self.ram.store(offset, val);
            return;
        }

        if let Some(offset) = map::SPU.contains(abs_addr) {
            spu::store(self, offset, val);
            return;
        }

        if let Some(offset) = map::DMA.contains(abs_addr) {
            dma::store(self, offset, val);
            return;
        }

        if let Some(offset) = map::TIMERS.contains(abs_addr) {
            timers::store(self, offset, val);
            return;
        }

        if let Some(offset) = map::GPU.contains(abs_addr) {
            gpu::store(self, offset, val);
            return;
        }

        if let Some(offset) = map::IRQ_CONTROL.contains(abs_addr) {
            match offset {
                0 => irq::ack(self, val.as_u32() as u16),
                4 => irq::set_mask(self, val.as_u32() as u16),
                _ => panic!("Unhandled IRQ store at address {:08x}", abs_addr),
            }

            return;
        }

        if let Some(offset) = map::EXPANSION_1.contains(abs_addr) {
            warn!("Unhandled write to expansion 1 register {:x}", offset);
            return;
        }

        if let Some(offset) = map::MEM_CONTROL.contains(abs_addr) {
            if T::width() != AccessWidth::Word {
                panic!("Unhandled MEM_CONTROL {:?} access", T::width());
            }

            let val = val.as_u32();

            // We don't actually implement those registers, we assume that all BIOSes and games are
            // going to use the default memory configuration. I'm not aware of any game that breaks
            // this assumption. Still, we can catch any attempt at using a non-standard
            // configuration and report an error.
            match offset {
                // Expansion 1 base address
                0 => {
                    if val != 0x1f00_0000 {
                        panic!("Bad expansion 1 base address: 0x{:08x}", val);
                    }
                }
                // Expansion 2 base address
                4 => {
                    if val != 0x1f80_2000 {
                        panic!("Bad expansion 2 base address: 0x{:08x}", val);
                    }
                }
                _ => warn!(
                    "Unhandled write to MEM_CONTROL register {:x}: 0x{:08x}",
                    offset, val
                ),
            }

            let index = (offset >> 2) as usize;
            self.mem_control[index] = val;
            return;
        }

        if map::CACHE_CONTROL.contains(abs_addr).is_some() {
            if T::width() != AccessWidth::Word {
                panic!("Unhandled cache control access");
            }

            self.cache_control = val.as_u32();

            return;
        }

        if map::RAM_SIZE.contains(abs_addr).is_some() {
            if T::width() != AccessWidth::Word {
                panic!("Unhandled RAM_SIZE access");
            }

            self.ram_size = val.as_u32();
            return;
        }

        if let Some(offset) = map::EXPANSION_2.contains(abs_addr) {
            warn!("Unhandled write to expansion 2 register {:x}", offset);
            return;
        }

        panic!(
            "Unhandled store at address {:08x} (val=0x{:08x})",
            abs_addr,
            val.as_u32()
        );
    }

    /// Returns true if the instruction cache is enabled in the CACHE_CONTROL register
    pub fn icache_enabled(&self) -> bool {
        self.cache_control & 0x800 != 0
    }

    /// Returns true if the cache is in "tag test mode"
    pub fn tag_test_mode(&self) -> bool {
        self.cache_control & 4 != 0
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

/// RAM
pub struct Ram {
    data: Box<[u8; RAM_SIZE]>,
}

impl Ram {
    /// Instantiate main RAM
    pub fn new() -> Ram {
        Ram {
            data: Box::new([0; RAM_SIZE]),
        }
    }

    /// Fetch the little endian value at `offset`
    pub fn load<T: Addressable>(&self, offset: u32) -> T {
        // The two MSBs are ignored, the 2MB RAM is mirrored four times over the first 8MB of
        // address space
        let offset = (offset & 0x1f_ffff) as usize;

        let mut v = 0;

        for i in 0..T::width() as usize {
            let b = u32::from(self.data[offset + i]);

            v |= b << (i * 8)
        }

        Addressable::from_u32(v)
    }

    /// Store the 32bit little endian word `val` into `offset`
    pub fn store<T: Addressable>(&mut self, offset: u32, val: T) {
        // The two MSBs are ignored, the 2MB RAM is mirrored four times over the first 8MB of
        // address space
        let offset = (offset & 0x1f_ffff) as usize;

        let val = val.as_u32();

        for i in 0..T::width() as usize {
            self.data[offset + i] = (val >> (i * 8)) as u8;
        }
    }
}

/// System RAM: 2MB
const RAM_SIZE: usize = 2 * 1024 * 1024;

pub mod map {
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

    /// Main RAM: 2MB mirrored four times over the first 8MB
    pub const RAM: Range = Range(0x0000_0000, 8 * 1024 * 1024);

    /// Expansion region 1
    pub const EXPANSION_1: Range = Range(0x1f00_0000, 512 * 1024);

    /// BIOS ROM. Read-only, significantly slower to access than system RAM
    pub const BIOS: Range = Range(0x1fc0_0000, 512 * 1024);

    /// Memory latency and expansion mapping
    pub const MEM_CONTROL: Range = Range(0x1f80_1000, 36);

    /// Register that has something to do with RAM configuration, configured by the BIOS
    pub const RAM_SIZE: Range = Range(0x1f80_1060, 4);

    /// Interrupt Control registers (status and mask)
    pub const IRQ_CONTROL: Range = Range(0x1f80_1070, 8);

    /// Direct Memory Access registers
    pub const DMA: Range = Range(0x1f80_1080, 0x80);

    /// Timer registers
    pub const TIMERS: Range = Range(0x1f80_1100, 0x30);

    /// GPU Registers
    pub const GPU: Range = Range(0x1f80_1810, 8);

    /// SPU registers
    pub const SPU: Range = Range(0x1f80_1c00, 640);

    /// Expansion region 2
    pub const EXPANSION_2: Range = Range(0x1f80_2000, 66);

    /// Cache control register. Full address since it's in KSEG2
    pub const CACHE_CONTROL: Range = Range(0xfffe_0130, 4);
}
