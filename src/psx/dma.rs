//! The PlayStation DMA, that can be used to copy data between the RAM and various devices (GPU,
//! CD drive, MDEC, SPU etc...)

use super::{AccessWidth, Addressable, Psx};

pub struct Dma {
    /// For now we don't implement the DMA, so just expose the registers as R/W
    regs: [u32; 0x20],
}

impl Dma {
    pub fn new() -> Dma {
        Dma { regs: [0; 0x20] }
    }
}

pub fn load<T: Addressable>(psx: &mut Psx, offset: u32) -> T {
    if T::width() != AccessWidth::Word {
        panic!("Unhandled DMA {:?} access", T::width());
    }

    let index = (offset >> 4) as usize;

    T::from_u32(psx.dma.regs[index])
}

pub fn store<T: Addressable>(psx: &mut Psx, offset: u32, val: T) {
    if T::width() != AccessWidth::Word {
        panic!("Unhandled DMA {:?} access", T::width());
    }

    let index = (offset >> 4) as usize;

    psx.dma.regs[index] = val.as_u32();
}
