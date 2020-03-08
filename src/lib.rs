//! Rustation libretro core

// This warning is a bit overkill at times, for instance it wants we to rewrite a simple
//
// ```
// for i in 1..len {
// ```
//
// into:
// ```
// for <item> in params.iter_mut().take(len).skip(1) {
// ```
#![allow(clippy::needless_range_loop)]
// This one is not too terrible but I find it not very useful when writing an emulator because
// the vast majority of the time our types are actually constrained by the original
// hardware, so using "as" casts is not a problem the vast majority of the time.
#![allow(clippy::cast_lossless)]

extern crate libc;
#[macro_use]
extern crate log;
extern crate cdimage;
extern crate fnv;
extern crate shaman;

mod assembler;
mod error;

use cdimage::cue::Cue;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};

use error::{Error, Result};
use psx::bios::Metadata;
use psx::bios::{Bios, BIOS_SIZE};
use psx::disc::Disc;

mod version {
    // VERSION and VERSION_CSTR are generated by build.rs
    include!(concat!(env!("OUT_DIR"), "/version.rs"));
}

#[macro_use]
pub mod libretro;
mod psx;
mod retrolog;

/// Static system information sent to the frontend on request
const SYSTEM_INFO: libretro::SystemInfo = libretro::SystemInfo {
    library_name: cstring!("rustation-ng"),
    library_version: version::VERSION_CSTR as *const _ as *const libc::c_char,
    valid_extensions: cstring!("cue"),
    need_fullpath: false,
    block_extract: false,
};

/// Emulation context containing the emulator state
struct Context {
    psx: psx::Psx,
    disc_path: PathBuf,
}

impl Context {
    fn new(disc: &Path) -> Result<Context> {
        let psx = Context::load_disc(disc)?;

        let mut ctx = Context {
            psx,
            disc_path: disc.to_path_buf(),
        };

        libretro::Context::refresh_variables(&mut ctx);

        Ok(ctx)
    }

    fn load_disc(disc: &Path) -> Result<psx::Psx> {
        let image = Cue::new(disc)?;

        let disc = Disc::new(Box::new(image))?;

        let serial = disc.serial_number();
        let region = disc.region();

        info!("Disc serial number: {}", serial);
        info!("Detected disc region: {:?}", region);

        let bios = find_bios(|md| md.region == region)?;

        let psx = psx::Psx::new_with_disc(disc, bios)?;

        Ok(psx)
    }

    // Precise FPS values for the video output for the given VideoClock. It's actually possible to
    // configure the PlayStation GPU to output with NTSC timings with the PAL clock (and vice-versa)
    // which would make this code invalid but it wouldn't make a lot of sense for a game to do that.
    fn video_output_framerate(&self) -> f32 {
        match self.psx.video_standard() {
            // 53.690MHz GPU clock frequency, 263 lines per field, 3413 cycles per line
            psx::VideoStandard::Ntsc => 59.81,
            // 53.222MHz GPU clock frequency, 314 lines per field, 3406 cycles per line
            psx::VideoStandard::Pal => 49.76,
        }
    }
}

impl libretro::Context for Context {
    fn render_frame(&mut self) {
        self.psx.run_frame();

        let frame = self.psx.last_frame();

        libretro::frame_done(&frame.pixels, frame.width, frame.height);
    }

    fn get_system_av_info(&self) -> libretro::SystemAvInfo {
        let upscaling = options::CoreOptions::internal_upscale_factor();

        // XXX For now we only support displaying the full VRAM, fix me later
        let _full_vram = options::CoreOptions::display_full_vram();
        let full_vram = true;

        let (w, h) = if full_vram {
            (1024, 512)
        } else {
            // Maximum resolution supported by the PlayStation video output is 640x480
            (640, 480)
        };

        let max_width = (w * upscaling) as libc::c_uint;
        let max_height = (h * upscaling) as libc::c_uint;

        libretro::SystemAvInfo {
            geometry: libretro::GameGeometry {
                // The base resolution will be overriden using ENVIRONMENT_SET_GEOMETRY before
                // rendering a frame so this base value is not really important
                base_width: max_width,
                base_height: max_height,
                max_width,
                max_height,
                aspect_ratio: 2. / 1.,
            },
            timing: libretro::SystemTiming {
                fps: self.video_output_framerate() as f64,
                sample_rate: 44_100.,
            },
        }
    }

    fn refresh_variables(&mut self) {}

    fn reset(&mut self) {
        match Context::load_disc(&self.disc_path) {
            Ok(psx) => {
                info!("Game reset");
                self.psx = psx;
            }
            Err(_) => warn!("Couldn't reset game"),
        }
    }

    fn gl_context_reset(&mut self) {}

    fn gl_context_destroy(&mut self) {}

    fn serialize_size(&self) -> usize {
        0
    }

    fn serialize(&self, _buf: &mut [u8]) -> ::std::result::Result<(), ()> {
        Ok(())
    }

    fn unserialize(&mut self, _buf: &[u8]) -> ::std::result::Result<(), ()> {
        Ok(())
    }
}

/// Attempt to find a BIOS for `region` in the system directory
fn find_bios<F>(predicate: F) -> Result<Bios>
where
    F: Fn(&Metadata) -> bool,
{
    let system_directory = match libretro::get_system_directory() {
        Some(dir) => dir,
        // libretro.h says that when the system directory is not provided "it's up to the
        // implementation to find a suitable directory" but I'm not sure what to put here.
        // Maybe "."? I'd rather give an explicit error message instead.
        None => {
            let m = "The frontend didn't give us a system directory, no BIOS can be loaded";
            return Err(Error::FrontendError(m.to_string()));
        }
    };

    info!("Looking for a suitable BIOS in {:?}", system_directory);

    let dir = ::std::fs::read_dir(&system_directory)?;

    for entry in dir {
        match entry {
            Ok(entry) => {
                let path = entry.path();

                match entry.metadata() {
                    Ok(md) => {
                        if !md.is_file() {
                            debug!("Ignoring {:?}: not a file", path);
                        } else if md.len() != BIOS_SIZE as u64 {
                            debug!("Ignoring {:?}: bad size", path);
                        } else {
                            match try_bios(&predicate, &path) {
                                Ok(bios) => {
                                    info!("Using BIOS {:?} ({:?})", path, bios.metadata());
                                    return Ok(bios);
                                }
                                Err(e) => {
                                    warn!("Ignoring {:?}: {:?}", path, e);
                                }
                            }
                        }
                    }
                    Err(e) => warn!("Ignoring {:?}: can't get file metadata: {}", path, e),
                }
            }
            Err(e) => warn!("Error while reading directory: {}", e),
        }
    }

    error!("Couldn't find a suitable BIOS image");

    Err(Error::NoBiosFound)
}

/// Attempt to read and load the BIOS at `path`
fn try_bios<F>(predicate: F, path: &Path) -> Result<Bios>
where
    F: Fn(&Metadata) -> bool,
{
    let mut file = File::open(&path)?;

    // Load the BIOS
    let mut data = Box::new([0; BIOS_SIZE]);
    let mut nread = 0;

    while nread < BIOS_SIZE {
        nread += match file.read(&mut data[nread..])? {
            0 => {
                let m = format!("Ignoring {:?}: file too small", path);
                return Err(Error::BadBios(m));
            }
            n => n,
        };
    }

    let bios = Bios::new(data)?;
    let md = bios.metadata();

    info!("Found BIOS DB entry for {:?}: {:?}", path, md);

    if md.known_bad {
        let m = format!("Ignoring {:?}: known bad dump", path);
        Err(Error::BadBios(m))
    } else if !predicate(md) {
        let m = format!("Ignoring {:?}: rejected by predicate", path);
        Err(Error::BadBios(m))
    } else {
        Ok(bios)
    }
}

mod options {
    //! Core options

    use std::str::FromStr;

    libretro_variables!(
        pub struct CoreOptions (prefix = "rustation") {
            internal_upscale_factor: u32, parse_upscale
                => "Internal upscaling factor; 1x (native)|2x|3x|4x|5x|6x|7x|8x";
            internal_color_depth: u8, parse_color_depth
                => "Internal color depth; dithered 16bpp (native)|32bpp";
            display_full_vram: bool, parse_bool
                => "Display full VRAM; disabled|enabled";
        });

    fn parse_upscale(opt: &str) -> Result<u32, <u32 as FromStr>::Err> {
        let num = opt.trim_matches(|c: char| !c.is_numeric());

        num.parse()
    }

    fn parse_color_depth(opt: &str) -> Result<u8, <u8 as FromStr>::Err> {
        let num = opt.trim_matches(|c: char| !c.is_numeric());

        num.parse()
    }

    fn parse_bool(opt: &str) -> Result<bool, ()> {
        match opt {
            "true" | "enabled" | "on" => Ok(true),
            "false" | "disabled" | "off" => Ok(false),
            _ => Err(()),
        }
    }
}

fn init() {
    retrolog::init();

    if !libretro::set_pixel_format(libretro::PixelFormat::Xrgb8888) {
        error!("Can't set pixel format to XRGB 8888!");
    }
}

fn init_variables() {
    options::CoreOptions::register();
}

/// Called when a game is loaded and a new context must be built
fn load_game(disc: PathBuf) -> Option<Box<dyn libretro::Context>> {
    info!("Loading {:?}", disc);

    Context::new(&disc)
        .ok()
        .map(|c| Box::new(c) as Box<dyn libretro::Context>)
}
