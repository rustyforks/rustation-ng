//! Very partial implementation of the ISO 9660 filesystem

use cdimage::bcd::Bcd;
use cdimage::msf::Msf;
use cdimage::{CdError, Image};

/// Structure representing an ISO9660 directory
pub struct Directory {
    /// Contents of the directory
    entries: Vec<Entry>,
}

impl Directory {
    pub fn new(image: &mut dyn Image, entry: &Entry) -> Result<Directory, Error> {
        if !entry.is_dir() {
            return Err(Error::NotADirectory);
        }

        let mut dir = Directory {
            entries: Vec::new(),
        };

        // Directory entries cannot span multiple sectors so it's safe to handle them one by one
        let mut extent_len = entry.extent_len() as usize;
        let extent_location = entry.extent_location();

        let track_msf = match Msf::from_sector_index(extent_location) {
            Some(m) => m,
            None => return Err(Error::BadExtent(extent_location)),
        };

        let toc = image.toc();
        let track = toc.track(Bcd::one())?;
        let mut msf = track.absolute_msf(track_msf)?;

        while extent_len > 0 {
            let sector = image.read_sector(msf)?;

            let data = sector.mode2_xa_payload()?;

            let len = if extent_len > 2048 { 2048 } else { extent_len };

            dir.parse_entries(&data[0..len])?;

            extent_len -= len;
            msf = msf.next().unwrap();
        }

        Ok(dir)
    }

    fn parse_entries(&mut self, mut raw: &[u8]) -> Result<(), Error> {
        while !raw.is_empty() {
            let dir_len = raw[0] as usize;

            if dir_len == 0 {
                // It seems we've reached the last directory. Or at least I think so? I'm not
                // entirely sure how directories which span several sectors are handled, if the
                // padding is not part of any entry then we should skip ahead to the next sector.
                // Needs more testing.
                break;
            }

            if dir_len < 34 {
                let desc = format!("Directory entry too short ({})", dir_len);
                return Err(Error::BadFormat(desc));
            }

            let name_len = raw[32] as usize;

            let name_end = 33 + name_len;

            if name_end > dir_len {
                return Err(Error::BadFormat("Entry name too long".into()));
            }

            self.entries.push(Entry::new(&raw[0..dir_len]));

            raw = &raw[dir_len..];
        }

        Ok(())
    }

    /// Attempt to "cd" to a subdirectory, returning a new `Directory`
    /// instance
    #[allow(dead_code)]
    pub fn cd(&self, image: &mut dyn Image, name: &[u8]) -> Result<Directory, Error> {
        let entry = self.entry_by_name(name)?;

        Directory::new(image, entry)
    }

    pub fn entry_by_name(&self, name: &[u8]) -> Result<&Entry, Error> {
        match self.entries.iter().find(|e| e.name() == name) {
            Some(e) => Ok(e),
            None => {
                let n = String::from_utf8_lossy(name);
                Err(Error::EntryNotFound(n.to_string()))
            }
        }
    }

    /// Retreive a list of all the entries in this directory
    #[allow(dead_code)]
    pub fn ls(&self) -> &[Entry] {
        &self.entries
    }
}

/// A single directory entry
pub struct Entry(Vec<u8>);

impl Entry {
    fn new(entry: &[u8]) -> Entry {
        Entry(entry.into())
    }

    pub fn name(&self) -> &[u8] {
        let name_len = self.0[32] as usize;

        let name_end = 33 + name_len;

        // No need to validate the len, it should've been done on
        // entry creation
        &self.0[33..name_end]
    }

    pub fn is_dir(&self) -> bool {
        let flags = self.0[25];

        (flags & 0x2) != 0
    }

    pub fn extent_location(&self) -> u32 {
        read_u32(&self.0[2..10])
    }

    pub fn extent_len(&self) -> u32 {
        read_u32(&self.0[10..18])
    }

    pub fn read_file(&self, image: &mut dyn Image) -> Result<Vec<u8>, Error> {
        if self.is_dir() {
            return Err(Error::NotAFile);
        }

        let mut extent_len = self.extent_len() as usize;
        let extent_location = self.extent_location();

        let mut contents = Vec::with_capacity(extent_len);

        let track_msf = match Msf::from_sector_index(extent_location) {
            Some(m) => m,
            None => return Err(Error::BadExtent(extent_location)),
        };

        let toc = image.toc();
        let track = toc.track(Bcd::one())?;
        let mut msf = track.absolute_msf(track_msf)?;

        while extent_len > 0 {
            let sector = image.read_sector(msf)?;

            let data = sector.mode2_xa_payload()?;

            let len = if extent_len > 2048 { 2048 } else { extent_len };

            for &b in &data[0..len] {
                contents.push(b);
            }

            extent_len -= len;
            msf = msf.next().unwrap();
        }

        Ok(contents)
    }
}

#[derive(Debug)]
pub enum Error {
    /// Cdimage access error
    CdError(CdError),
    /// Couldn't find the ISO9660 magic "CD0001"
    BadMagic,
    /// Couldn't find the Primary Volume Descriptor
    MissingPrimaryVolumeDescriptor,
    /// Unexpected Volume Descriptor version
    BadVolumDescriptorVersion,
    /// Encountered an invalid extent location
    BadExtent(u32),
    /// Miscellaneous ISO9660 format error containing a description of the problem
    BadFormat(String),
    /// The requested entry couldn't be found
    EntryNotFound(String),
    /// We expected a directory and got a file
    NotADirectory,
    /// We expected a file and got a directory
    NotAFile,
}

impl From<CdError> for Error {
    fn from(e: CdError) -> Error {
        Error::CdError(e)
    }
}

pub fn open_image(image: &mut dyn Image) -> Result<Directory, Error> {
    // The first 16 sectors are the "system area" which is ignored by the ISO filesystem. The
    // Volume Descriptor Set should start at 00:00:16 in track 01
    let toc = image.toc();
    let track = toc.track(Bcd::one())?;
    let mut msf = track.absolute_msf(Msf::from_bcd(0, 0, 0x16).unwrap())?;

    let mut sector;

    // Look for the primary volume descriptor
    loop {
        sector = image.read_sector(msf)?;

        let volume_descriptor = sector.mode2_xa_payload()?;

        // Check volume descriptor "standard identifier"
        if &volume_descriptor[1..6] != b"CD001" {
            return Err(Error::BadMagic);
        }

        // Byte 0 contains the "volume descriptor type".
        match volume_descriptor[0] {
            // Primary Volume Descriptor
            0x01 => break,
            // Volume Descriptor Set Terminator
            0xff => return Err(Error::MissingPrimaryVolumeDescriptor),
            // Unhandled volume descriptor type, ignore
            _ => (),
        }

        // Not the primary volume descriptor, move on to the next sector
        msf = msf.next().unwrap();
    }

    let volume_descriptor = sector.mode2_xa_payload()?;

    // Volume Descriptor Version
    if volume_descriptor[6] != 0x01 {
        return Err(Error::BadVolumDescriptorVersion);
    }

    // We can now open the root directory descriptor
    let root_dir_descriptor = &volume_descriptor[156..190];

    let root_dir = Entry::new(root_dir_descriptor);
    Directory::new(image, &root_dir)
}

/// Read a 32bit number stored in "both byte order" format
fn read_u32(v: &[u8]) -> u32 {
    // Only use the little endian representation. Should we bother validating that the BE version
    // is coherent?
    v[0] as u32 | ((v[1] as u32) << 8) | ((v[2] as u32) << 16) | ((v[3] as u32) << 24)
}
