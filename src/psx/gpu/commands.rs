//! Implementation of the various GP0 commands.

use super::Psx;

/// Description of the various GP0 commands
pub struct Command {
    /// Callback function to actually perform the command
    pub handler: fn(&mut Psx),
    /// Actual length of the command, in number of FIFO words
    pub len: u8,
    /// This is the amount of overhead bytes accepted on the FIFO when this command is the next to
    /// be executed (see `try_write_command` for more details). These values are taken from
    /// mednafen, I'm not sure how they've been established. I assume it has something to do with
    /// the amount of time taken by these instructions to process?
    pub fifo_len: u8,
    /// Marker for commands that aren't executed with normal PSX timings.
    pub out_of_band: bool,
}

impl Command {
    pub fn len(&self) -> usize {
        self.len as usize
    }

    pub fn fifo_len(&self) -> usize {
        self.fifo_len as usize
    }
}

fn cmd_draw_mode(psx: &mut Psx) {
    let mode = psx.gpu.command_fifo.pop();

    psx.gpu.draw_mode.set(mode);
}

fn cmd_tex_window(psx: &mut Psx) {
    psx.gpu.tex_window = psx.gpu.command_fifo.pop() & 0xf_ffff;
}

fn cmd_clip_top_left(psx: &mut Psx) {
    psx.gpu.clip_top_left = psx.gpu.command_fifo.pop() & 0xf_ffff;
}

fn cmd_clip_bot_right(psx: &mut Psx) {
    psx.gpu.clip_bot_right = psx.gpu.command_fifo.pop() & 0xf_ffff;
}

fn cmd_draw_offset(psx: &mut Psx) {
    psx.gpu.draw_offset = psx.gpu.command_fifo.pop() & 0x3f_ffff;
}

fn cmd_mask_settings(psx: &mut Psx) {
    let mask_settings = psx.gpu.command_fifo.pop() & 0x3f_ffff;

    psx.gpu.mask_settings.set(mask_settings)
}

/// Does nothing, but with style
fn cmd_nop(psx: &mut Psx) {
    // Pop the FIFO
    let w = psx.gpu.command_fifo.pop();

    warn!("Encountered GPU NOP command: 0x{:08x}", w);
}

/// Placeholder function
fn cmd_unimplemented(psx: &mut Psx) {
    unimplemented!("GPU command {:08x}", psx.gpu.command_fifo.pop());
}

/// LUT for all GP0 commands (indexed by opcode, bits[31:24] of the first command word)
pub const GP0_COMMANDS: [Command; 0x100] = [
    // 0x00
    Command {
        handler: cmd_nop,
        len: 1,
        fifo_len: 1,
        out_of_band: true,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    // 0x10
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    // 0x20
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    // 0x30
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    // 0x40
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    // 0x50
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    // 0x60
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    // 0x70
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    // 0x80
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    // 0x90
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    // 0xa0
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    // 0xb0
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    // 0xc0
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    // 0xd0
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    // 0xe0
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_draw_mode,
        len: 1,
        fifo_len: 2,
        out_of_band: false,
    },
    Command {
        handler: cmd_tex_window,
        len: 1,
        fifo_len: 2,
        out_of_band: false,
    },
    Command {
        handler: cmd_clip_top_left,
        len: 1,
        fifo_len: 1,
        out_of_band: true,
    },
    Command {
        handler: cmd_clip_bot_right,
        len: 1,
        fifo_len: 1,
        out_of_band: true,
    },
    Command {
        handler: cmd_draw_offset,
        len: 1,
        fifo_len: 1,
        out_of_band: true,
    },
    Command {
        handler: cmd_mask_settings,
        len: 1,
        fifo_len: 2,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    // 0xf0
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
    Command {
        handler: cmd_unimplemented,
        len: 1,
        fifo_len: 1,
        out_of_band: false,
    },
];
