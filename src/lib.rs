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
// I seem to get weird false positive in the GTE code for this one
#![allow(clippy::redundant_closure_call)]
// Wants to rewrite some numeric comparison chains as match when it doesn't make a lot of sense
// IMO.
#![allow(clippy::comparison_chain)]
// Doesn't let me use r, g, b and y, u, v in the same function!
#![allow(clippy::many_single_char_names)]

extern crate libc;
#[macro_use]
extern crate log;
extern crate cdimage;
extern crate fnv;
extern crate shaman;

mod assembler;
mod error;
mod memory_card;
#[macro_use]
pub mod libretro;
mod psx;
mod retrolog;

use cdimage::cue::Cue;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};

use error::{Error, Result};
use memory_card::MemoryCardFile;
use psx::bios::Metadata;
use psx::bios::{Bios, BIOS_SIZE};
use psx::disc::Disc;
use psx::gpu::RasterizerOption;
use psx::pad_memcard::devices::gamepad::{Button, ButtonState, DigitalPad, DualShock, GamePad};
use psx::pad_memcard::devices::DisconnectedDevice;

mod version {
    // VERSION and VERSION_CSTR are generated by build.rs
    include!(concat!(env!("OUT_DIR"), "/version.rs"));
}

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
    /// The type of controller connected on the two input ports
    controller_type: [libretro::InputDevice; 2],
    /// Objects used to deal with reading/storing the memory card images to files
    memcard_files: [MemoryCardFile; 2],
    /// Internal frame width
    internal_width: u32,
    /// Internal frame height
    internal_height: u32,
}

impl Context {
    fn new(disc: &Path) -> Result<Context> {
        let psx = Context::load_disc(disc)?;

        let mut ctx = Context {
            psx,
            disc_path: disc.to_path_buf(),
            // Start with both port disconnected and wait for the frontend to tell us what to use
            // in `set_controller`
            controller_type: [libretro::InputDevice::None; 2],
            memcard_files: [MemoryCardFile::dummy(), MemoryCardFile::dummy()],
            internal_width: 640,
            internal_height: 480,
        };

        libretro::Context::refresh_variables(&mut ctx);

        ctx.setup_memory_cards();

        Ok(ctx)
    }

    fn poll_controllers(&mut self) {
        let gamepads = self.psx.pad_memcard.gamepads_mut();

        // Update buttons
        for port in 0..2 {
            let ty = self.controller_type[port];
            let has_buttons =
                ty == libretro::InputDevice::JoyPad || ty == libretro::InputDevice::Analog;

            if !has_buttons {
                // Nothing to do
                continue;
            }

            let device = gamepads[port].device_mut();

            for &(retrobutton, psxbutton) in &BUTTON_MAP {
                let state = if libretro::button_pressed(port, retrobutton) {
                    ButtonState::Pressed
                } else {
                    ButtonState::Released
                };

                device.set_button_state(psxbutton, state);
            }
        }

        // Update analog sticks
        for port in 0..2 {
            let ty = self.controller_type[port];
            let has_sticks = ty == libretro::InputDevice::Analog;

            if !has_sticks {
                // Nothing to do
                continue;
            }

            let device = gamepads[port].device_mut();

            let left_x =
                libretro::axis_state(port, libretro::AnalogInput::Left, libretro::AnalogAxis::X);
            let left_y =
                libretro::axis_state(port, libretro::AnalogInput::Left, libretro::AnalogAxis::Y);
            let right_x =
                libretro::axis_state(port, libretro::AnalogInput::Right, libretro::AnalogAxis::X);
            let right_y =
                libretro::axis_state(port, libretro::AnalogInput::Right, libretro::AnalogAxis::Y);

            device.set_axis_state((left_x, left_y), (right_x, right_y));
        }
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

    /// Initialize the memory cards connected to the emulated console
    fn setup_memory_cards(&mut self) {
        let mut memory_cards = self.psx.pad_memcard.memory_cards_mut();

        // Start by disconnecting everything to make sure we never half-configure the memory card.
        // We want to make it hard for the user to lose their saves...
        for m in &mut memory_cards {
            let device = Box::new(DisconnectedDevice);
            m.connect_device(device);
        }
        for m in self.memcard_files.iter_mut() {
            *m = MemoryCardFile::dummy();
        }

        let save_path = match libretro::get_save_directory() {
            Some(p) => p,
            None => {
                warn!("No save directory defined, disabling memory cards");
                return;
            }
        };

        // Memory Card 1: game specific
        //
        // XXX Make this configurable at some point.
        let mc1_filename = self.disc_path.file_stem().map(|f| {
            let p: &Path = f.as_ref();
            p.with_extension("0.mcr")
        });

        match mc1_filename {
            Some(f) => {
                let full_path = save_path.join(f);
                match MemoryCardFile::load_or_create(&full_path) {
                    Ok((mcf, mc)) => {
                        // Success
                        info!("Memory Card 1 is {}", mcf.path().display());
                        memory_cards[0].connect_device(Box::new(mc));
                        self.memcard_files[0] = mcf;
                    }
                    Err(e) => {
                        error!(
                            "Can't load or create memory card '{}': {}",
                            full_path.display(),
                            e
                        );
                    }
                }
            }
            None => error!("Can't generate a file name for memory card 1, disabling it"),
        }

        // Memory Card 2: same for all games
        //
        // XXX Make this configurable at some point.
        let full_path = save_path.join("rustation_common.1.mcr");
        match MemoryCardFile::load_or_create(&full_path) {
            Ok((mcf, mc)) => {
                // Success
                info!("Memory Card 2 is {}", mcf.path().display());
                memory_cards[1].connect_device(Box::new(mc));
                self.memcard_files[1] = mcf;
            }
            Err(e) => {
                error!(
                    "Can't load or create memory card '{}': {}",
                    full_path.display(),
                    e
                );
            }
        }
    }

    /// Called once per frame in order to save the Memory Card to disc if necessary
    fn check_memory_cards(&mut self) {
        let memory_cards = self.psx.pad_memcard.memory_cards();

        self.memcard_files[0].maybe_dump(memory_cards[0].device());
        self.memcard_files[1].maybe_dump(memory_cards[1].device());
    }

    /// Called when we're about to quit to force-flush any pending Memory Card write
    fn flush_memory_cards(&mut self) {
        let memory_cards = self.psx.pad_memcard.memory_cards();

        self.memcard_files[0].force_dump(memory_cards[0].device());
        self.memcard_files[1].force_dump(memory_cards[1].device());
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

    fn get_geometry(&self) -> libretro::GameGeometry {
        let upscaling = options::CoreOptions::internal_upscale_factor();

        // XXX For now we only support displaying the full VRAM, fix me later
        let full_vram = options::CoreOptions::display_full_vram();

        let w;
        let h;
        let aspect_ratio;

        if full_vram {
            w = 1024;
            h = 512;
            aspect_ratio = 2. / 1.
        } else {
            // Maximum resolution supported by the PlayStation video output is 640x576. That high a
            // vertical resolution would mean no blanking however, so it doesn't make a lot of
            // sense.
            w = 640;
            h = 480;
            aspect_ratio = 4. / 3.
        }

        let max_width = (w * upscaling) as libc::c_uint;
        let max_height = (h * upscaling) as libc::c_uint;

        libretro::GameGeometry {
            base_width: self.internal_width,
            base_height: self.internal_height,
            max_width,
            max_height,
            aspect_ratio,
        }
    }

    fn output_frame(&mut self) {
        let mut frame = self.psx.last_frame();

        if frame.width != self.internal_width || frame.height != self.internal_height {
            // Internal resolution changed
            self.internal_width = frame.width;
            self.internal_height = frame.height;

            let geom = self.get_geometry();
            libretro::set_geometry(&geom);

            frame = self.psx.last_frame();
        }

        libretro::frame_done(&frame.pixels, frame.width, frame.height);
    }
}

impl ::std::ops::Drop for Context {
    fn drop(&mut self) {
        info!("Shutting down");
        self.flush_memory_cards();
    }
}

impl libretro::Context for Context {
    fn set_controller(&mut self, port: usize, mut device: libretro::InputDevice, subclass: u32) {
        if port > 1 {
            warn!(
                "Can't configure controller for port {}, only 2 supported",
                port + 1
            );
            return;
        }

        let gamepads = self.psx.pad_memcard.gamepads_mut();

        let new_pad: Box<dyn GamePad> = match subclass {
            PSX_CONTROLLER_DIGITAL => Box::new(DigitalPad::new()),
            PSX_CONTROLLER_DUALSHOCK => Box::new(DualShock::new()),
            _ => {
                // Try to match the generic class instead
                match device {
                    libretro::InputDevice::None => Box::new(DisconnectedDevice),
                    libretro::InputDevice::JoyPad => Box::new(DigitalPad::new()),
                    libretro::InputDevice::Analog => Box::new(DualShock::new()),
                    _ => {
                        error!(
                            "Received bogus controller config for port {}: {:?}.0x{:x}.\
                               Disconnecting it",
                            port, device, subclass
                        );
                        device = libretro::InputDevice::None;
                        Box::new(DisconnectedDevice)
                    }
                }
            }
        };

        info!("New controller on port {}: {}", port, new_pad.description());

        self.controller_type[port] = device;
        gamepads[port].connect_device(new_pad);
    }

    fn render_frame(&mut self) {
        self.poll_controllers();

        self.psx.run_frame();

        self.output_frame();

        // Send sound samples
        let samples = self.psx.get_audio_samples();
        libretro::send_audio_samples(samples);
        // Clear the emulator's buffer for next frame
        self.psx.clear_audio_samples();

        self.check_memory_cards();
    }

    fn get_system_av_info(&self) -> libretro::SystemAvInfo {
        libretro::SystemAvInfo {
            geometry: self.get_geometry(),
            timing: libretro::SystemTiming {
                fps: self.video_output_framerate() as f64,
                sample_rate: 44_100.,
            },
        }
    }

    fn refresh_variables(&mut self) {
        let full_vram = options::CoreOptions::display_full_vram();

        self.psx
            .gpu
            .set_rasterizer_option(RasterizerOption::DisplayFullVRam(full_vram));
    }

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

    file.read_exact(&mut *data)?;

    let bios = Bios::new(data)?;
    let md = bios.metadata();

    info!("Found BIOS DB entry for {:?}: {:?}", path, md);

    if md.known_bad {
        let m = format!("{:?}: known bad dump", path);
        Err(Error::BadBios(m))
    } else if !predicate(md) {
        let m = format!("{:?}: rejected by predicate", path);
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

/// Libretro to PlayStation button mapping. Libretro's mapping is based on the SNES controller so
/// libretro's A button matches the PlayStation's Circle button.
static BUTTON_MAP: [(libretro::JoyPadButton, Button); 16] = [
    (libretro::JoyPadButton::Up, Button::DUp),
    (libretro::JoyPadButton::Down, Button::DDown),
    (libretro::JoyPadButton::Left, Button::DLeft),
    (libretro::JoyPadButton::Right, Button::DRight),
    (libretro::JoyPadButton::Start, Button::Start),
    (libretro::JoyPadButton::Select, Button::Select),
    (libretro::JoyPadButton::A, Button::Circle),
    (libretro::JoyPadButton::B, Button::Cross),
    (libretro::JoyPadButton::Y, Button::Square),
    (libretro::JoyPadButton::X, Button::Triangle),
    (libretro::JoyPadButton::L, Button::L1),
    (libretro::JoyPadButton::R, Button::R1),
    (libretro::JoyPadButton::L2, Button::L2),
    (libretro::JoyPadButton::R2, Button::R2),
    (libretro::JoyPadButton::L3, Button::L3),
    (libretro::JoyPadButton::R3, Button::R3),
];

static INPUT_DESCRIPTORS: [libretro::InputDescriptor; 41] = [
    // Port 0
    libretro::InputDescriptor::joypad_button(
        0,
        libretro::JoyPadButton::Left,
        cstring!("D-Pad Left"),
    ),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::Up, cstring!("D-Pad Up")),
    libretro::InputDescriptor::joypad_button(
        0,
        libretro::JoyPadButton::Down,
        cstring!("D-Pad Down"),
    ),
    libretro::InputDescriptor::joypad_button(
        0,
        libretro::JoyPadButton::Right,
        cstring!("D-Pad Right"),
    ),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::B, cstring!("Cross")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::A, cstring!("Circle")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::X, cstring!("Triangle")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::Y, cstring!("Square")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::L, cstring!("L1")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::L2, cstring!("L2")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::L3, cstring!("L3")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::R, cstring!("R1")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::R2, cstring!("R2")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::R3, cstring!("R3")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::Select, cstring!("Select")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::Start, cstring!("Start")),
    libretro::InputDescriptor::analog_axis(
        0,
        libretro::AnalogInput::Left,
        libretro::AnalogAxis::X,
        cstring!("Left Analog X"),
    ),
    libretro::InputDescriptor::analog_axis(
        0,
        libretro::AnalogInput::Left,
        libretro::AnalogAxis::Y,
        cstring!("Left Analog Y"),
    ),
    libretro::InputDescriptor::analog_axis(
        0,
        libretro::AnalogInput::Right,
        libretro::AnalogAxis::X,
        cstring!("Right Analog X"),
    ),
    libretro::InputDescriptor::analog_axis(
        0,
        libretro::AnalogInput::Right,
        libretro::AnalogAxis::Y,
        cstring!("Right Analog Y"),
    ),
    // Port 1
    libretro::InputDescriptor::joypad_button(
        1,
        libretro::JoyPadButton::Left,
        cstring!("D-Pad Left"),
    ),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::Up, cstring!("D-Pad Up")),
    libretro::InputDescriptor::joypad_button(
        1,
        libretro::JoyPadButton::Down,
        cstring!("D-Pad Down"),
    ),
    libretro::InputDescriptor::joypad_button(
        1,
        libretro::JoyPadButton::Right,
        cstring!("D-Pad Right"),
    ),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::B, cstring!("Cross")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::A, cstring!("Circle")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::X, cstring!("Triangle")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::Y, cstring!("Square")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::L, cstring!("L1")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::L2, cstring!("L2")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::L3, cstring!("L3")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::R, cstring!("R1")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::R2, cstring!("R2")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::R3, cstring!("R3")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::Select, cstring!("Select")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::Start, cstring!("Start")),
    libretro::InputDescriptor::analog_axis(
        1,
        libretro::AnalogInput::Left,
        libretro::AnalogAxis::X,
        cstring!("Left Analog X"),
    ),
    libretro::InputDescriptor::analog_axis(
        1,
        libretro::AnalogInput::Left,
        libretro::AnalogAxis::Y,
        cstring!("Left Analog Y"),
    ),
    libretro::InputDescriptor::analog_axis(
        1,
        libretro::AnalogInput::Right,
        libretro::AnalogAxis::X,
        cstring!("Right Analog X"),
    ),
    libretro::InputDescriptor::analog_axis(
        1,
        libretro::AnalogInput::Right,
        libretro::AnalogAxis::Y,
        cstring!("Right Analog Y"),
    ),
    // End of table
    libretro::InputDescriptor::end_of_table(),
];

/// Standard, digital-only controller (SCPH-1080)
const PSX_CONTROLLER_DIGITAL: libc::c_uint = libretro::InputDevice::JoyPad.subclass(0);
/// DualShock analog controller (SCPH-1200)
const PSX_CONTROLLER_DUALSHOCK: libc::c_uint = libretro::InputDevice::Analog.subclass(0);

/// All supported controller types
static CONTROLLER_DESCRIPTIONS: [libretro::ControllerDescription; 2] = [
    libretro::ControllerDescription {
        desc: cstring!("PlayStation Digital Controller"),
        id: PSX_CONTROLLER_DIGITAL,
    },
    libretro::ControllerDescription {
        desc: cstring!("PlayStation DualShock Analog Controller"),
        id: PSX_CONTROLLER_DUALSHOCK,
    },
];

/// Controller settings for two player mode (no multitap)
static CONTROLLER_INFO_2P: [libretro::ControllerInfo; 3] = [
    // Player 1
    libretro::ControllerInfo {
        types: &CONTROLLER_DESCRIPTIONS as *const _,
        num_types: CONTROLLER_DESCRIPTIONS.len() as _,
    },
    // Player 2
    libretro::ControllerInfo {
        types: &CONTROLLER_DESCRIPTIONS as *const _,
        num_types: CONTROLLER_DESCRIPTIONS.len() as _,
    },
    // End of table marker
    libretro::ControllerInfo::end_of_table(),
];

fn init_controllers() {
    libretro::set_controller_info(&CONTROLLER_INFO_2P);
    libretro::set_input_descriptors(&INPUT_DESCRIPTORS);
}
