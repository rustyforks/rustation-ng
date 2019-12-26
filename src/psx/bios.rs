use std::fs::File;
use std::io::{Error, ErrorKind, Read, Result};
use std::path::Path;

use super::Addressable;

pub struct Bios {
    rom: Box<[u8; BIOS_SIZE]>,
}

impl Bios {
    pub fn new(path: &Path) -> Result<Bios> {
        let mut file = File::open(path)?;

        // Load the BIOS
        let mut rom = Box::new([0; BIOS_SIZE]);
        let mut nread = 0;

        while nread < BIOS_SIZE {
            nread += match file.read(&mut rom[nread..])? {
                0 => {
                    return Err(Error::new(
                        ErrorKind::InvalidInput,
                        "BIOS file is too small",
                    ))
                }
                n => n,
            };
        }

        // Make sure the BIOS file is not too big, it's probably not a
        // good dump otherwise.
        if file.read(&mut [0; 1])? != 0 {
            return Err(Error::new(ErrorKind::InvalidInput, "BIOS file is too big"));
        }

        Ok(Bios { rom })
    }

    /// Read `T::width()` bytes from the BIOS at the given `offset`
    pub fn load<T: Addressable>(&self, offset: u32) -> T {
        let offset = offset as usize;

        let mut r = 0;

        for i in 0..T::width() as usize {
            let b = u32::from(self.rom[offset + i]);

            r |= b << (8 * i)
        }

        Addressable::from_u32(r)
    }
}

/// BIOS images are always 512KB in length
pub const BIOS_SIZE: usize = 512 * 1024;
