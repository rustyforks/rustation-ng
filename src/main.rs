#[macro_use]
extern crate log;
extern crate simple_logger;

mod psx;

use std::path::Path;

use psx::error::{Error, Result};

fn main() -> Result<()> {
    simple_logger::init().unwrap();

    let args: Vec<_> = std::env::args().collect();

    let bios_path = match args.get(1) {
        Some(b) => b,
        None => {
            error!("Usage: rustation-ng <BIOS file>");
            return Err(Error::LogicError("Missing BIOS".into()));
        }
    };

    let mut psx = psx::Psx::new(Path::new(bios_path))?;

    psx.run();

    Ok(())
}
