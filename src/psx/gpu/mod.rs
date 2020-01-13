mod commands;
mod fifo;

use super::cpu::CPU_FREQ_HZ;
use super::{irq, sync, AccessWidth, Addressable, CycleCount, Psx};
use commands::Command;

const GPUSYNC: sync::SyncToken = sync::SyncToken::Gpu;

pub struct Gpu {
    video_standard: VideoStandard,
    /// Current value of the display mode
    display_mode: DisplayMode,
    /// Number of the first line displayed on the screen
    display_line_start: u16,
    /// Number of the first line *not* displayed on the screen
    display_line_end: u16,
    /// True when we're between [display_line_start; display_line_end[
    display_active: bool,
    /// First line of the display area in VRAM
    display_vram_y_start: u16,
    /// Current value of the draw mode
    draw_mode: DrawMode,
    /// GP0 command FIFO
    command_fifo: fifo::CommandFifo,
    /// Variable used to simulate the time taken by draw commands. Taken from mednafen. This value
    /// can become negative (we don't start a new draw command if it's negative, but a command
    /// already started won't stop using cycles even if this goes below 0).
    draw_time_budget: CycleCount,
    /// Since the ratio of GPU-to-CPU frequency isn't an integer we can end up with fractional
    /// cycles in `run`. We store them here (this is a fixed point value with 16 fractional bits).
    remaining_fractional_cycles: u16,
    /// True if we're in the current line's HSYNC, false otherwise
    in_hsync: bool,
    /// If we're in the current line's HSYNC then this is the time to the end of line. Otherwise
    /// it's the time until we reach the HSYNC.
    cycles_to_line_event: CycleCount,
    /// Number of the line currently being displayed (from 0 to `lines_per_field`)
    cur_line: u16,
    /// Offset of the currently displayed line in the VRAM (relative to `display_vram_y_start`)
    cur_line_vram_offset: u16,
    /// Actual Y coordinate in VRAM of the currently displayed line
    cur_line_vram_y: u16,
    /// This variable toggles at each new line.
    line_phase: bool,
    /// When interlaced this variable is true when we switch to the bottom field. When progressive
    /// this variable is always false.
    bottom_field: bool,
    /// When interlaced this variable is true when we send the bottom field to the TV
    read_bottom_field: bool,
    /// Total number of lines (including blanking) per field. If the display is progressive there's
    /// only one field per frame
    lines_per_field: u16,
    /// Set to `false` on the beginning of a new line and set to `true` when the line has been
    /// drawn
    frame_drawn: bool,
}

impl Gpu {
    pub fn new(video_standard: VideoStandard) -> Gpu {
        let mut gpu = Gpu {
            video_standard,
            draw_mode: DrawMode::new(),
            display_mode: DisplayMode::new(),
            display_line_start: 0x10,
            display_line_end: 0x100,
            display_active: false,
            display_vram_y_start: 0,
            command_fifo: fifo::CommandFifo::new(),
            // XXX for now let's start with a huge budget since we don't have proper timings yet
            draw_time_budget: 0x1000_0000,
            remaining_fractional_cycles: 0,
            // This is what Mednafen uses, I have no idea where that comes from. Surely having
            // extremely precise timings so early on doesn't matter? After all the BIOS will resync
            // on VSync anyway.
            cycles_to_line_event: 3412 - 200,
            in_hsync: false,
            cur_line: 0,
            cur_line_vram_offset: 0,
            cur_line_vram_y: 0,
            line_phase: true,
            bottom_field: false,
            read_bottom_field: false,
            lines_per_field: 0,
            frame_drawn: false,
        };

        gpu.refresh_lines_per_field();

        gpu
    }

    fn status(&self) -> u32 {
        let mut s = 0;

        s |= self.draw_mode.0 & 0x7ff;

        // TODO: bit 11 and 12 (GP0[0xe6])

        s |= ((!self.bottom_field) as u32) << 13;

        // TODO: No$ says that bit 14 is `display_mode` bit 7, need to check. Mednafen doesn't set
        // it.

        s |= (self.draw_mode.texture_disable() as u32) << 15;

        s |= ((self.display_mode.0 >> 6) & 1) << 16;
        s |= (self.display_mode.0 & 0x3f) << 17;

        // TODO: bit 23 - Display Enable (GP1[0x03])
        // TODO: bit 24 - IRQ1 (*not* VSync)
        // TODO: bit 25 - DMA data request

        s |= (self.is_idle() as u32) << 26;

        // TODO: bit 27: Data available
        // TODO: bit 28: DMA ready
        s |= 1 << 28;
        // TODO: bits [29:30]: DMA direction

        let display_line_even_odd = u32::from(self.cur_line_vram_y & 1);
        s |= display_line_even_odd << 31;

        s
    }

    /// Computes the value of the status register's "idle" bit
    fn is_idle(&self) -> bool {
        // TODO: add "InCmd" when we implement it
        self.draw_time_budget >= 0 && self.command_fifo.is_empty()
    }

    /// Attempt to write `command` to the command FIFO, returns `true` if successful, `false` if
    /// the FIFO overflowed and the command was dropped
    fn try_write_command(&mut self, command: u32) -> bool {
        // This logic was taken from mednafen: normally we should have a `command_fifo` of the same
        // depth as the real PSX (0x10 entries) and reject the command if it's empty. The problem
        // is that this requires very accurate GPU pipeline emulation, if a game really pushes the
        // FIFO to the limit and we end up slightly slower than normal we'll drop some data that we
        // shouldn't. As a workaround we use a deeper FIFO and we allow one extra command in it.
        let cur_fifo_len = self.command_fifo.len();

        if cur_fifo_len >= PSX_COMMAND_FIFO_DEPTH {
            // We have more data than a real PSX would allow but if it fits with the next command
            // removed we allow it anyway
            let next_command = self.next_command();

            let fifo_max = PSX_COMMAND_FIFO_DEPTH + next_command.fifo_len();

            if cur_fifo_len >= fifo_max {
                // Nope, the FIFO is still too full, drop the command
                warn!("GPU FIFO full, dropping 0x{:x}", command);
                return false;
            }
        }

        self.command_fifo.push(command);
        true
    }

    /// Peek into the command FIFO and retrieve the next Command to be executed. It's a mistake to
    /// call this method if the FIFO is empty.
    fn next_command(&self) -> &'static Command {
        debug_assert!(!self.command_fifo.is_empty());

        let opcode = self.command_fifo.peek() >> 24;

        &commands::GP0_COMMANDS[opcode as usize]
    }

    fn add_draw_time(&mut self, elapsed_cpu_cycles: CycleCount) {
        // No idea what's the rationale behind this cycle twiddling, it's copied from mednafen
        self.draw_time_budget += elapsed_cpu_cycles << 1;

        if self.draw_time_budget > 256 {
            self.draw_time_budget = 256;
        }
    }

    /// Returns the number of GPU cycles elapsed while the CPU ran `cpu_cyles`. Any fractional
    /// leftover cycle will be stored in `remaining_fractional_cycles`
    fn tick(&mut self, cpu_cycles: CycleCount) -> CycleCount {
        let clock_ratio = match self.video_standard {
            VideoStandard::Ntsc => GPU_CYCLES_PER_CPU_CYCLES_NTSC,
            VideoStandard::Pal => GPU_CYCLES_PER_CPU_CYCLES_PAL,
        };

        let mut gpu_cycles = u64::from(self.remaining_fractional_cycles);
        gpu_cycles += (cpu_cycles as u64) * clock_ratio;

        // FRACTIONAL_FACTOR should be a power of two so that the following modulo/division
        // optimize as simple bitops
        self.remaining_fractional_cycles = (gpu_cycles % FRACTIONAL_FACTOR) as u16;

        (gpu_cycles / FRACTIONAL_FACTOR) as CycleCount
    }

    /// Returns the total length of a line (including horizontal blanking)
    fn line_length(&self) -> u16 {
        match self.display_mode.standard() {
            // I'm not really sure what justifies this `line_phase` business but that's what
            // mednafen does. Maybe the real value is close to 3412.5 and therefore we have close
            // to one full cycle added every other cycle?
            VideoStandard::Ntsc => 3412 + self.line_phase as u16,
            VideoStandard::Pal => 3405,
        }
    }

    /// Refresh the value of `lines_per_field` based on the display mode
    fn refresh_lines_per_field(&mut self) {
        self.lines_per_field = if self.display_mode.is_interlaced() {
            let l = match self.display_mode.standard() {
                VideoStandard::Ntsc => 263,
                VideoStandard::Pal => 313,
            };

            l - self.bottom_field as u16
        } else {
            match self.display_mode.standard() {
                VideoStandard::Ntsc => 263,
                VideoStandard::Pal => 314,
            }
        };
    }

    /// Called when we switch to a new line
    fn new_line(&mut self) {
        self.line_phase = !self.line_phase;

        self.cur_line = (self.cur_line + 1) % self.lines_per_field;
    }

    /// Called when we're about to finish the current field
    fn new_field(&mut self) {
        self.bottom_field = if self.display_mode.is_interlaced() {
            !self.bottom_field
        } else {
            false
        };
    }
}

pub fn run(psx: &mut Psx) {
    let elapsed = sync::resync(psx, GPUSYNC);

    psx.gpu.add_draw_time(elapsed);

    process_commands(psx);

    let mut elapsed_gpu_cycles = psx.gpu.tick(elapsed);

    while elapsed_gpu_cycles >= psx.gpu.cycles_to_line_event {
        elapsed_gpu_cycles -= psx.gpu.cycles_to_line_event;

        // We either reached hsync or left it
        psx.gpu.in_hsync = !psx.gpu.in_hsync;

        if psx.gpu.in_hsync {
            handle_hsync(psx);
        } else {
            // We reached the EOL
            handle_eol(psx);
        }
    }

    psx.gpu.cycles_to_line_event -= elapsed_gpu_cycles;

    // New we need to program the next sync at `cycles_to_line_event`. Where it gets tricky is that
    // we program sync events based on the CPU clock, so we need to do the conversion
    let mut delta = psx.gpu.cycles_to_line_event as u64 * FRACTIONAL_FACTOR;
    // Don't forget the fractional cycle we have leftover
    delta -= u64::from(psx.gpu.remaining_fractional_cycles);

    // Finally divide by the frequency factor, rounding *up* (we want to be called when the event
    // has occurred, not just before). Remember that in order to divide `x` by `y` rounding up you
    // need to do `(x + y - 1) / y`.
    let clock_ratio = match psx.gpu.video_standard {
        VideoStandard::Ntsc => GPU_CYCLES_PER_CPU_CYCLES_NTSC,
        VideoStandard::Pal => GPU_CYCLES_PER_CPU_CYCLES_PAL,
    };

    delta = (delta + clock_ratio - 1) / clock_ratio;

    if delta < 1 {
        delta = 1;
    } else if delta > 128 {
        // I believe that mednafen does that in order to trigger a call to `process_commands` very
        // often and keep the GPU working. We could relax this when we don't care for very accurate
        // timings
        delta = 128;
    }

    sync::next_event(psx, GPUSYNC, delta as i32);
}

/// Called when we reach the hsync
fn handle_hsync(psx: &mut Psx) {
    // Next event when we reach the EOL
    psx.gpu.cycles_to_line_event = HSYNC_LEN_CYCLES;
}

/// Called when we reach the end of line (just after the HSYNC)
fn handle_eol(psx: &mut Psx) {
    psx.gpu.new_line();

    // Next line event will be when we reach the hsync
    psx.gpu.cycles_to_line_event = CycleCount::from(psx.gpu.line_length()) - HSYNC_LEN_CYCLES;
    let cur_line = psx.gpu.cur_line;

    // Taken from mednafen but I'm not sure if that's necessary or even useful. Normally we'll draw
    // the frame when we reach `display_line_end` below. If we miss that we have the code running
    // on the last field line below that'll always catch it. So what's the point of checking once
    // again here? Especially since at this point the line number seems fairly arbitrary. I guess
    // it can trigger early if `display_line_end` is set to some silly value but is it really worth
    // a special case?
    if !psx.gpu.frame_drawn {
        let draw_line = match psx.gpu.video_standard {
            VideoStandard::Ntsc => 256,
            VideoStandard::Pal => 308,
        };

        if cur_line == draw_line {
            // We reached the end of active video, we can tell the frontend to render the frame
            draw_frame(psx);
        }
    }

    let is_first_line = cur_line == 0;
    let is_last_line = cur_line == psx.gpu.lines_per_field - 1;

    if is_last_line {
        if !psx.gpu.frame_drawn {
            // Normally we should've drawn the frame by now but we might have missed it if the
            // video standard changed mid-frame. In this case we can just draw it now in order not
            // to skip the frame entirely and it'll return to normal next frame
            draw_frame(psx);
        }

        // I'm not sure why Mednafen changes the field on the last line of the field instead of the
        // first line of the next one.
        psx.gpu.new_field();
    } else if is_first_line {
        debug_assert!(psx.gpu.frame_drawn, "Last frame wasn't drawn!");

        psx.gpu.frame_drawn = false;
        psx.gpu.refresh_lines_per_field();
    }

    if cur_line == psx.gpu.display_line_end && psx.gpu.display_active {
        // We're leaving the active display area.
        psx.gpu.display_active = false;
        psx.gpu.cur_line_vram_offset = 0;

        irq::trigger(psx, irq::Interrupt::VBlank);

        if psx.gpu.display_mode.is_true_interlaced() {
            // Prepare for the next frame, if we're currently sending the bottom field it means
            // that we're going to switch to the top
            psx.gpu.read_bottom_field = !psx.gpu.bottom_field;
        }

        if !psx.gpu.frame_drawn {
            // More magic from mednafen. I assume that the logic is to try to refresh the display
            // as early as possible to minimize latency, although I'm not really sure I understand
            // the logic behind putting a minimum line value here. The comment in mednafen mentions
            // Descent(NTSC) which reaches the end of frame at line 236 and Mikagura Shoujo
            // Tanteidan which sets it to 192 during the intro FMV.
            let line_min = match psx.gpu.video_standard {
                VideoStandard::Ntsc => 232,
                VideoStandard::Pal => 260,
            };

            if cur_line >= line_min {
                draw_frame(psx);
            }
        }
    }

    if cur_line == psx.gpu.display_line_start && !psx.gpu.display_active {
        // We're entering the active display area
        psx.gpu.display_active = true;
    }

    // Figure out which VRAM line is being displayed
    psx.gpu.cur_line_vram_y = psx.gpu.display_vram_y_start;
    psx.gpu.cur_line_vram_y += if psx.gpu.display_mode.is_true_interlaced() {
        psx.gpu.cur_line_vram_offset * 2 + psx.gpu.read_bottom_field as u16
    } else {
        psx.gpu.cur_line_vram_offset
    };
    psx.gpu.cur_line_vram_y %= VRAM_HEIGHT;

    // XXX actually copy the line to the output framebuffer here

    if psx.gpu.display_active {
        psx.gpu.cur_line_vram_offset += 1;
    }
}

/// Called when a frame is done rendering and should be displayed
fn draw_frame(psx: &mut Psx) {
    psx.gpu.frame_drawn = true;
}

pub fn store<T: Addressable>(psx: &mut Psx, off: u32, val: T) {
    if T::width() != AccessWidth::Word {
        panic!("Unhandled GPU store ({:?})", T::width());
    }

    let val = val.as_u32();

    match off {
        0 => gp0(psx, val),
        4 => unimplemented!(),
        _ => unreachable!(),
    };
}

pub fn load<T: Addressable>(psx: &mut Psx, off: u32) -> T {
    if T::width() != AccessWidth::Word {
        panic!("Unhandled GPU load ({:?})", T::width());
    }

    let v = match off {
        0 => unimplemented!(),
        4 => psx.gpu.status(),
        _ => unreachable!(),
    };

    T::from_u32(v)
}

/// Handle GP0 commands
fn gp0(psx: &mut Psx, val: u32) {
    if psx.gpu.try_write_command(val) {
        process_commands(psx);
    }
}

/// Attempt to execute a command from the `command_fifo`
fn process_commands(psx: &mut Psx) {
    if psx.gpu.command_fifo.is_empty() {
        // We have nothing to do if the FIFO is empty
        return;
    }

    let command = psx.gpu.next_command();

    if command.len() > psx.gpu.command_fifo.len() {
        // We still haven't received the entire command, wait longer
        return;
    }

    // Apparently there are a handful of commands that aren't executed like the rest. Both mednafen
    // and No$ agree on that, however the specifics are unclear. I tag these commands as "out of
    // band" here (mednafen calls them "ss_cmd", not sure what that means).
    //
    // No$ says that these commands "do not take up space in the FIFO" and are "probably executed
    // immediately (even if there're still other commands in the FIFO)". If that's true it means
    // that we should run these commands directly from `gp0` without touching the FIFO.
    //
    // Mednafen on the other hands puts these commands through the FIFO as usual and executes them
    // in order, just with no draw time overhead.
    if psx.gpu.draw_time_budget < 0 && !command.out_of_band {
        // We don't have enough time budget to execute this command
        return;
    }

    if !command.out_of_band {
        psx.gpu.draw_time_budget -= 2;
    }

    // Invoke the callback to actually implement the command
    (command.handler)(psx);
}

/// Wrapper around the Draw Mode register value (set by GP0[0xe1])
struct DrawMode(u32);

impl DrawMode {
    fn new() -> DrawMode {
        DrawMode(0)
    }

    fn set(&mut self, mode: u32) {
        self.0 = mode
    }

    fn texture_disable(&self) -> bool {
        self.0 & (1 << 11) != 0
    }
}

/// Wrapper around the Display Mode register value (set by GP1[0x08])
struct DisplayMode(u32);

impl DisplayMode {
    fn new() -> DisplayMode {
        DisplayMode(0)
    }

    fn standard(&self) -> VideoStandard {
        if self.0 & (1 << 3) != 0 {
            VideoStandard::Pal
        } else {
            VideoStandard::Ntsc
        }
    }

    fn is_interlaced(&self) -> bool {
        self.0 & (1 << 5) != 0
    }

    /// To actually have the console output in interlaced (having two interlaced fields in VRAM and
    /// the console displays one after the other) it's not enough to set the `is_interlaced` bit,
    /// you also need to set bit 2 in Display Mode to actually tell the console to use two fields
    /// in VRAM. Without it the console sends the same data for the top and bottom fields, which is
    /// fairly useless.
    fn is_true_interlaced(&self) -> bool {
        let two_fields = self.0 & (1 << 2) != 0;

        self.is_interlaced() && two_fields
    }
}

/// Real PSX command FIFO depth
const PSX_COMMAND_FIFO_DEPTH: usize = 0x10;

/// Emulated command FIFO depth. This is different from `PSX_COMMAND_FIFO_DEPTH` to work around
/// emulation inaccuracies, see `try_write_command` above for more infos. Needs to be a power of
/// two for the FIFO code to work correctly.
const COMMAND_FIFO_DEPTH: usize = 0x20;

/// The are a few hardware differences between PAL and NTSC consoles, in particular the pixelclock
/// runs slightly slower on PAL consoles.
#[derive(Clone, Copy)]
pub enum VideoStandard {
    Ntsc,
    Pal,
}

/// Total number of lines in the VRAM
const VRAM_HEIGHT: u16 = 512;

/// Duration of the HSYNC in GPU cycles
const HSYNC_LEN_CYCLES: CycleCount = 200;

/// Scaling factor used in fixed point GPU cycle arithmetics
const FRACTIONAL_FACTOR: u64 = 1 << 16;

/// Ratio of GPU_FREQ_NTSC_HZ / CPU_FREQ_HZ multiplied by 0x1000 to use fixed point arithmetics
/// instead of floating point
const GPU_CYCLES_PER_CPU_CYCLES_NTSC: u64 =
    (GPU_FREQ_NTSC_HZ * (FRACTIONAL_FACTOR as f64) / (CPU_FREQ_HZ as f64)) as u64;

/// Ratio of GPU_FREQ_PAL_HZ / CPU_FREQ_HZ multiplied by 0x1000 to use fixed point arithmetics
/// instead of floating point
const GPU_CYCLES_PER_CPU_CYCLES_PAL: u64 =
    (GPU_FREQ_PAL_HZ * (FRACTIONAL_FACTOR as f64) / (CPU_FREQ_HZ as f64)) as u64;

/// GPU frequency for NTSC consoles (Japan + North America)
const GPU_FREQ_NTSC_HZ: f64 = 53_693_181.818;
/// GPU frequency for PAL consoles (Europe)
const GPU_FREQ_PAL_HZ: f64 = 53_203_425.;
