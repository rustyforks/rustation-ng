mod commands;

use super::{AccessWidth, Addressable, CycleCount, Psx};
use commands::Command;

pub struct Gpu {
    /// GP0 command FIFO
    command_fifo: CommandFifo,
    /// Variable used to simulate the time taken by draw commands. Taken from mednafen. This value
    /// can become negative (we don't start a new draw command if it's negative, but a command
    /// already started won't stop using cycles even if this goes below 0).
    draw_time_budget: CycleCount,
}

impl Gpu {
    pub fn new() -> Gpu {
        Gpu {
            command_fifo: CommandFifo::new(),
            // XXX for now let's start with a huge budget since we don't have proper timings yet
            draw_time_budget: 0x1000_0000,
        }
    }

    fn status(&self) -> u32 {
        0
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

/// GP0 command FIFO
struct CommandFifo {
    buffer: [u32; COMMAND_FIFO_DEPTH],
    /// Read index in buffer. One bit wider that COMMAND_FIFO_DEPTH to differentiate FIFO full and
    /// FIFO empty.
    read_index: u8,
    /// Write index in buffer. One bit wider that COMMAND_FIFO_DEPTH to differentiate FIFO full and
    /// FIFO empty.
    write_index: u8,
}

impl CommandFifo {
    fn new() -> CommandFifo {
        CommandFifo {
            buffer: [0; COMMAND_FIFO_DEPTH],
            read_index: 0,
            write_index: 0,
        }
    }

    fn is_empty(&self) -> bool {
        self.read_index == self.write_index
    }

    fn is_full(&self) -> bool {
        self.read_index ^ self.write_index ^ COMMAND_FIFO_DEPTH as u8 == 0
    }

    fn len(&self) -> usize {
        let l = self.write_index - self.read_index;

        l as usize
    }

    /// Push an entry in the FIFO. Should *not* be called when the FIFO is full!
    fn push(&mut self, val: u32) {
        debug_assert!(!self.is_full());

        let i = self.write_index % COMMAND_FIFO_DEPTH as u8;

        self.write_index = self.write_index.wrapping_add(1);

        self.buffer[i as usize] = val;
    }

    /// Pop an entry from the FIFO. Should *not* be called when the FIFO is empty!
    fn pop(&mut self) -> u32 {
        debug_assert!(!self.is_empty());

        let i = self.read_index % COMMAND_FIFO_DEPTH as u8;

        self.read_index = self.read_index.wrapping_add(1);

        self.buffer[i as usize]
    }

    /// Returns the element at the top of the FIFO but doesn't pop it. Should *not* be called when
    /// the FIFO is empty!
    fn peek(&self) -> u32 {
        debug_assert!(!self.is_empty());

        let i = self.read_index % COMMAND_FIFO_DEPTH as u8;

        self.buffer[i as usize]
    }
}

/// Real PSX command FIFO depth
const PSX_COMMAND_FIFO_DEPTH: usize = 0x10;

/// Emulated command FIFO depth. This is different from `PSX_COMMAND_FIFO_DEPTH` to work around
/// emulation inaccuracies, see `try_write_command` above for more infos. Needs to be a power of
/// two for the FIFO code to work correctly.
const COMMAND_FIFO_DEPTH: usize = 0x20;

#[test]
fn test_command_fifo() {
    let mut fifo = CommandFifo::new();

    // Empty FIFO
    assert!(fifo.is_empty());
    assert!(!fifo.is_full());
    assert_eq!(fifo.len(), 0);

    // Push element
    fifo.push(1);
    assert!(!fifo.is_empty());
    assert!(!fifo.is_full());
    assert_eq!(fifo.len(), 1);

    // Peek
    assert_eq!(fifo.peek(), 1);
    assert!(!fifo.is_empty());
    assert!(!fifo.is_full());
    assert_eq!(fifo.len(), 1);

    // Pop
    assert_eq!(fifo.pop(), 1);
    assert!(fifo.is_empty());
    assert!(!fifo.is_full());
    assert_eq!(fifo.len(), 0);

    // Fill
    for i in 0..COMMAND_FIFO_DEPTH {
        assert!(!fifo.is_full());
        assert!(fifo.len() == i);
        fifo.push(i as u32);
    }

    assert!(fifo.is_full());
    assert!(!fifo.is_empty());
    assert_eq!(fifo.len(), COMMAND_FIFO_DEPTH);

    // Empty
    for i in 0..COMMAND_FIFO_DEPTH {
        assert!(!fifo.is_empty());
        assert_eq!(fifo.pop(), i as u32);
    }
    assert!(fifo.is_empty());
    assert!(!fifo.is_full());
    assert_eq!(fifo.len(), 0);

    // Fill interleaved
    for i in 0..(COMMAND_FIFO_DEPTH - 1) {
        assert!(!fifo.is_full());

        let v = i as u32;

        fifo.push(v);
        fifo.push(v);
        fifo.pop();
    }

    assert!(!fifo.is_empty());
    assert!(!fifo.is_full());
    assert_eq!(fifo.len(), COMMAND_FIFO_DEPTH - 1);

    fifo.push(0xc0_ffee);
    assert!(fifo.is_full());
    assert!(!fifo.is_empty());
    assert_eq!(fifo.len(), COMMAND_FIFO_DEPTH);

    // Empty interleaved
    for i in 0..COMMAND_FIFO_DEPTH {
        assert!(!fifo.is_empty());
        fifo.pop();
        fifo.push(i as u32);
        fifo.pop();
    }

    assert!(fifo.is_empty());
    assert!(!fifo.is_full());
    assert_eq!(fifo.len(), 0);
}
