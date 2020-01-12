mod commands;
mod fifo;

use super::cpu::CPU_FREQ_HZ;
use super::{sync, AccessWidth, Addressable, CycleCount, Psx};
use commands::Command;

const GPUSYNC: sync::SyncToken = sync::SyncToken::Gpu;

pub struct Gpu {
    video_standard: VideoStandard,
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
}

impl Gpu {
    pub fn new(video_standard: VideoStandard) -> Gpu {
        Gpu {
            video_standard,
            draw_mode: DrawMode::new(),
            command_fifo: fifo::CommandFifo::new(),
            // XXX for now let's start with a huge budget since we don't have proper timings yet
            draw_time_budget: 0x1000_0000,
            remaining_fractional_cycles: 0,
        }
    }

    fn status(&self) -> u32 {
        let mut s = 0;

        s |= self.draw_mode.0 & 0x7ff;

        s |= (self.draw_mode.texture_disable() as u32) << 15;

        s
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
}

pub fn run(psx: &mut Psx) {
    let elapsed = sync::resync(psx, GPUSYNC);

    psx.gpu.add_draw_time(elapsed);

    process_commands(psx);

    let _elapsed_gpu_cycles = psx.gpu.tick(elapsed);

    // Placeholder code to avoid an infinite loop
    sync::next_event(psx, GPUSYNC, 512);
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
