use crate::psx::iso9660;
use cdimage::CdError;
use std::io;

pub type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    IoError(io::Error),
    /// We couldn't find the disc's serial number
    NoSerialNumber,
    /// The provided BIOS is unknown
    UnknownBios,
    /// We couldn't find a suitable BIOS
    NoBiosFound,
    /// Invalid BIOS file
    BadBios(String),
    /// Something went wrong while communicating with the frontend
    FrontendError(String),
    CdError(CdError),
    /// The disc format was incorrect (i.e. probably not a valid PSX disc image)
    BadDiscFormat(String),
    /// CD ISO filesystem error
    IsoError(iso9660::Error),
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::IoError(e)
    }
}

impl From<CdError> for Error {
    fn from(e: CdError) -> Self {
        Error::CdError(e)
    }
}

impl From<iso9660::Error> for Error {
    fn from(e: iso9660::Error) -> Self {
        Error::IsoError(e)
    }
}
