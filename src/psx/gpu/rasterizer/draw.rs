use std::sync::mpsc;

use super::{Command, CommandBuffer, Frame, Special};
use crate::psx::gpu::{DrawMode, MaskSettings};

pub struct Rasterizer {
    /// Frame currently being drawn
    cur_frame: Frame,
    /// Channel used to receive commands
    command_channel: mpsc::Receiver<CommandBuffer>,
    /// Channel used to send completed frames back
    frame_channel: mpsc::Sender<Frame>,
    /// Draw mode configuration
    draw_mode: DrawMode,
    /// Left edge of the clipping area
    clip_x_min: i32,
    /// Top edge of the clipping area
    clip_y_min: i32,
    /// Right edge of the clipping area
    clip_x_max: i32,
    /// Bottom edge of the clipping area
    clip_y_max: i32,
    /// Horizontal drawing offset
    draw_offset_x: i32,
    /// Vertical drawing offset
    draw_offset_y: i32,
    /// Texture window settings
    tex_window: u32,
    /// Mask bit settings
    mask_settings: MaskSettings,
}

impl Rasterizer {
    pub fn new(
        command_channel: mpsc::Receiver<CommandBuffer>,
        frame_channel: mpsc::Sender<Frame>,
    ) -> Rasterizer {
        Rasterizer {
            cur_frame: Frame::new(1024, 512),
            command_channel,
            frame_channel,
            draw_mode: DrawMode::new(),
            clip_x_min: 0,
            clip_y_min: 0,
            clip_x_max: 1,
            clip_y_max: 0,
            draw_offset_x: 0,
            draw_offset_y: 0,
            tex_window: 0,
            mask_settings: MaskSettings::new(),
        }
    }

    pub fn run(&mut self) {
        loop {
            let commands = self.command_channel.recv().unwrap();

            let mut command_i = commands.iter();

            while let Some(cmd) = command_i.next() {
                match cmd {
                    Command::Gp0(v) => {
                        let op = v >> 24;
                        let h = &GP0_COMMANDS[op as usize];
                        // The longest possible draw command is 12 word long (shaded and textured
                        // quad)
                        let mut params = [0; 12];

                        params[0] = *v;

                        let len = h.len as usize;

                        for i in 1..len {
                            // The main GPU code is supposed to send us complete draw commands so
                            // it should be safe to expect the right number of parameters here.
                            match command_i.next() {
                                Some(Command::Gp0(v)) => params[i] = *v,
                                other => panic!("Expected GP0 command, got {:?}", other),
                            }
                        }

                        (h.handler)(self, &params[..len]);
                    }
                    Command::Gp1(v) => self.gp1(*v),
                    Command::Special(Special::Quit) => return,
                    // XXX draw one line at a time
                    Command::Special(Special::EndOfLine(_)) => (),
                    Command::Special(Special::EndOfFrame) => self.send_frame(),
                }
            }
        }
    }

    fn gp1(&mut self, val: u32) {
        let op = val >> 24;

        match op {
            0x00 => self.reset(),
            // Reset command FIFO
            0x01 => (),
            0x02 => debug!("IRQ1 ack"),
            _ => warn!("Unimplemented GP1 {:x}", val),
        }
    }

    fn send_frame(&mut self) {
        let mut new_frame = Frame::new(1024, 512);

        ::std::mem::swap(&mut new_frame, &mut self.cur_frame);

        self.frame_channel.send(new_frame).unwrap();
    }

    fn reset(&mut self) {
        self.draw_mode.set(0);

        self.clip_x_min = 0;
        self.clip_y_min = 0;
        self.clip_x_max = 1;
        self.clip_y_max = 0;
        self.draw_offset_x = 0;
        self.draw_offset_y = 0;
        self.draw_offset_x = 0;
        self.draw_offset_y = 0;
        self.tex_window = 0;
        self.mask_settings.set(0);
    }
}

/// Extend a signed value on `n` bit to an i32
fn extend_to_i32(val: u32, n: usize) -> i32 {
    let shift = 32 - n;

    ((val << shift) as i32) >> shift
}

/// Description of the various GP0 commands
pub struct CommandHandler {
    pub handler: fn(&mut Rasterizer, params: &[u32]),
    /// Actual length of the command, in number of FIFO words
    pub len: u8,
}

fn cmd_tex_window(rasterizer: &mut Rasterizer, params: &[u32]) {
    rasterizer.tex_window = params[0] & 0xf_ffff;
}

fn cmd_clip_top_left(rasterizer: &mut Rasterizer, params: &[u32]) {
    let clip = params[0];

    rasterizer.clip_x_min = (clip & 0x3ff) as i32;
    rasterizer.clip_y_min = ((clip >> 10) & 0x3ff) as i32;
}

fn cmd_clip_bot_right(rasterizer: &mut Rasterizer, params: &[u32]) {
    let clip = params[0];

    rasterizer.clip_x_max = (clip & 0x3ff) as i32;
    rasterizer.clip_y_max = ((clip >> 10) & 0x3ff) as i32;

    // XXX double check but apparently the real X clip is one more pixel than configured in the
    // command.
    rasterizer.clip_x_max += 1;
}

fn cmd_draw_mode(rasterizer: &mut Rasterizer, params: &[u32]) {
    let mode = params[0];

    rasterizer.draw_mode.set(mode);
}

fn cmd_draw_offset(rasterizer: &mut Rasterizer, params: &[u32]) {
    let off = params[0];

    let off_x = off & 0x7ff;
    let off_y = (off >> 11) & 0x7ff;

    // Sign-extend
    rasterizer.draw_offset_x = extend_to_i32(off_x, 11);
    rasterizer.draw_offset_y = extend_to_i32(off_y, 11);
}

fn cmd_mask_settings(rasterizer: &mut Rasterizer, params: &[u32]) {
    let mask_settings = params[0] & 0x3f_ffff;

    rasterizer.mask_settings.set(mask_settings)
}

/// Placeholder function
fn cmd_unimplemented(_rasterizer: &mut Rasterizer, params: &[u32]) {
    unimplemented!("GPU command {:08x}", params[0]);
}

/// LUT for all GP0 commands (indexed by opcode, bits[31:24] of the first command word)
pub static GP0_COMMANDS: [CommandHandler; 0x100] = [
    // 0x00
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    // 0x10
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    // 0x20
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 5,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 5,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    // 0x30
    CommandHandler {
        handler: cmd_unimplemented,
        len: 6,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 8,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    // 0x40
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    // 0x50
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    // 0x60
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 4,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    // 0x70
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    // 0x80
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    // 0x90
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    // 0xa0
    CommandHandler {
        handler: cmd_unimplemented,
        len: 3,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    // 0xb0
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    // 0xc0
    CommandHandler {
        handler: cmd_unimplemented,
        len: 3,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    // 0xd0
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    // 0xe0
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_draw_mode,
        len: 1,
    },
    CommandHandler {
        handler: cmd_tex_window,
        len: 1,
    },
    CommandHandler {
        handler: cmd_clip_top_left,
        len: 1,
    },
    CommandHandler {
        handler: cmd_clip_bot_right,
        len: 1,
    },
    CommandHandler {
        handler: cmd_draw_offset,
        len: 1,
    },
    CommandHandler {
        handler: cmd_mask_settings,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    // 0xf0
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
];
