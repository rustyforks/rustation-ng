use std::sync::mpsc;

use super::{Command, CommandBuffer, Frame, Special};
use crate::psx::gpu::commands::{NoShading, Position};
use crate::psx::gpu::commands::{NoTexture, Opaque, ShadingMode, TextureMode, TransparencyMode};
use crate::psx::gpu::{DrawMode, MaskSettings};

pub struct Rasterizer {
    vram: Box<[VRamPixel; 1024 * 512]>,
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
            vram: box_array![VRamPixel::new(); 1024 * 512],
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
        // Full VRAM display
        for i in 0..(1024 * 512) {
            self.cur_frame.pixels[i] = self.vram[i].to_rgb888();
        }

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

    fn draw_solid_pixel<Transparency>(&mut self, x: i32, y: i32, color: Bgr888)
    where
        Transparency: TransparencyMode,
    {
        debug_assert!(x >= 0 && x < 1024);
        debug_assert!(y >= 0 && y < 512);

        let vram_off = y * 1024 + x;

        let vram_pixel = &mut self.vram[vram_off as usize];

        // XXX implement masking
        if Transparency::is_transparent() {
            unimplemented!();
        } else {
            *vram_pixel = VRamPixel::from_bgr888(color);
        };
    }

    /// Draw a rectangle of a single solid color
    fn draw_solid_rect<Transparency>(
        &mut self,
        mut top_left: Position,
        mut width: i32,
        mut height: i32,
        color: Bgr888,
    ) where
        Transparency: TransparencyMode,
    {
        // Clip to drawing area
        if top_left.x < self.clip_x_min {
            width -= self.clip_x_min - top_left.x;
            top_left.x = self.clip_x_min;
        }

        if top_left.y < self.clip_y_min {
            height -= self.clip_y_min - top_left.y;
            top_left.y = self.clip_y_min;
        }

        if top_left.x + width > self.clip_x_max {
            width = self.clip_x_max - top_left.x;
        }

        if top_left.y + height > self.clip_y_max {
            height = self.clip_y_max - top_left.y;
        }

        if width <= 0 || height <= 0 {
            // Nothing to do
            return;
        }

        for row in 0..height {
            let y = top_left.y + row;

            for col in 0..width {
                let x = top_left.x + col;

                self.draw_solid_pixel::<Transparency>(x, y, color);
            }
        }
    }
}

/// A single BGR1555 VRAM pixel. In order to make the emulation code simpler and to support
/// increased color depth we always use xBGR 1888 internally
#[derive(Copy, Clone)]
struct VRamPixel(u32);

impl VRamPixel {
    fn new() -> VRamPixel {
        VRamPixel(0x7)
    }

    fn from_bgr888(bgr: Bgr888) -> VRamPixel {
        let r = bgr.red();
        let g = bgr.green();
        let b = bgr.blue();
        let mask = bgr.mask();

        let r5 = r >> 3;
        let g5 = g >> 3;
        let b5 = b >> 3;

        let mut p = r5 | (g5 << 5) | (b5 << 10) | (mask << 15);

        // Store the extended precision (not available on real hardware) bits in the top 16 bits
        let rl = r & 0x7;
        let gl = g & 0x7;
        let bl = b & 0x7;

        p |= (rl << 16) | (gl << 19) | (bl << 22);

        VRamPixel(p)
    }

    fn to_rgb888(self) -> u32 {
        let p = self.0;

        let mut r = (p & 0x1f) << 3;
        let mut g = ((p >> 5) & 0x1f) << 3;
        let mut b = ((p >> 10) & 0x1f) << 3;

        r |= (p >> 16) & 0x7;
        g |= (p >> 19) & 0x7;
        b |= (p >> 22) & 0x7;

        // Do we want to return the mask bit here? I can't really see how it would ever be useful.

        (r << 16) | (g << 8) | b
    }
}

#[derive(Copy, Clone, Debug)]
struct Bgr888(u32);

impl Bgr888 {
    fn from_command(c: u32) -> Bgr888 {
        Bgr888(c & 0xff_ffff)
    }

    fn red(self) -> u32 {
        self.0 & 0xff
    }

    fn green(self) -> u32 {
        (self.0 >> 8) & 0xff
    }

    fn blue(self) -> u32 {
        (self.0 >> 16) & 0xff
    }

    fn mask(self) -> u32 {
        (self.0 >> 24) & 1
    }
}

/// Description of a vertex with position, texture and shading (depending on the command)
#[derive(Debug)]
struct Vertex {
    position: Position,
    color: Bgr888,
    texture: u32,
}

impl Vertex {
    fn new() -> Vertex {
        Vertex {
            position: Position::new(0, 0),
            color: Bgr888::from_command(0),
            texture: 0,
        }
    }

    fn set_color(&mut self, c: u32) {
        self.color = Bgr888::from_command(c);
    }

    fn set_position(&mut self, p: u32) {
        self.position = Position::from_command(p);
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

fn cmd_handle_poly_quad<Transparency, Texture, Shading>(rasterizer: &mut Rasterizer, params: &[u32])
where
    Transparency: TransparencyMode,
    Texture: TextureMode,
    Shading: ShadingMode,
{
    let mut vertices = [Vertex::new(), Vertex::new(), Vertex::new(), Vertex::new()];

    let mut index = 0;

    // Load the vertex data from the command
    for (v, vertex) in vertices.iter_mut().enumerate() {
        if v == 0 || Shading::is_shaded() {
            vertex.set_color(params[index]);
            index += 1;
        }

        vertex.set_position(params[index]);
        index += 1;

        // Add the draw offset
        vertex.position.x += rasterizer.draw_offset_x;
        vertex.position.y += rasterizer.draw_offset_y;

        if Texture::is_textured() {
            vertex.texture = params[index];
            index += 1;
        }
    }

    let ax = vertices[0].position.x;
    let bx = vertices[1].position.x;
    let cx = vertices[2].position.x;
    let dx = vertices[3].position.x;
    let ay = vertices[0].position.y;
    let by = vertices[1].position.y;
    let cy = vertices[2].position.y;
    let dy = vertices[3].position.y;

    // See if the quad is really a rect (i.e. the top and bottom sides are perfectly horizontal and
    // the left and right sides are perfectly vertical). It's fairly common for PSX games to draw
    // rects using quads for a variety of reasons, so it's probably worth optimizing this special
    // case.
    let is_rect = if ax == bx {
        // We have a rect if we're in the following configuration:
        //    A -- C
        //    |    |
        //    B -- D
        // Potentially mirrored vertically and/or horizontally
        (ay == cy) && (cx == dx) && (dy == by)
    } else if ay == by {
        // We have a rect if we're in the following configuration:
        //    A -- B
        //    |    |
        //    C -- D
        // Potentially mirrored vertically and/or horizontally
        (ax == cx) && (cy == dy) && (dx == bx)
    } else {
        false
    };

    if is_rect {
        // Order coords so that the vertices end up in this order:
        //         A -- B
        //         |    |
        //         C -- D
        vertices.sort_by(|a, b| {
            let ay = a.position.y;
            let by = b.position.y;

            if ay != by {
                ay.cmp(&by)
            } else {
                a.position.x.cmp(&b.position.x)
            }
        });

        let width = vertices[1].position.x - vertices[0].position.x;
        let left = vertices[0].position.x;
        let height = vertices[2].position.y - vertices[0].position.y;
        let top = vertices[0].position.y;
        let top_left = Position::new(left, top);

        if Texture::is_textured() || Shading::is_shaded() {
            // We should check if the texture is "square" as well, otherwise it might be simpler to
            // use the normal triangle-drawing code?
            unimplemented!();
        } else {
            let color = vertices[0].color;

            rasterizer.draw_solid_rect::<Transparency>(top_left, width, height, color);
            return;
        }
    }

    unimplemented!("Non-rect quad {:?}", vertices);
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

fn cmd_clear_cache(_rasterizer: &mut Rasterizer, _params: &[u32]) {
    // TODO: no cache support for now
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
        handler: cmd_clear_cache,
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
        handler: cmd_handle_poly_quad::<Opaque, NoTexture, NoShading>,
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
