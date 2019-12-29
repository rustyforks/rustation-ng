//! Sound Processing Unit

use super::{AccessWidth, Addressable, Psx};

pub struct Spu {
    /// As a placeholder before the SPU is implemented the register interface is treated like a RAM
    regs: [u16; 320],
}

impl Spu {
    pub fn new() -> Spu {
        Spu { regs: [0; 320] }
    }
}

pub fn store<T: Addressable>(psx: &mut Psx, off: u32, val: T) {
    if T::width() != AccessWidth::HalfWord {
        panic!("Unhandled {:?} SPU store", T::width());
    }

    let index = (off >> 1) as usize;

    psx.spu.regs[index] = val.as_u16();
}

pub fn load<T: Addressable>(psx: &mut Psx, off: u32) -> T {
    if T::width() != AccessWidth::HalfWord {
        panic!("Unhandled {:?} SPU load", T::width());
    }

    let index = (off >> 1) as usize;

    T::from_u32(u32::from(psx.spu.regs[index]))
}
