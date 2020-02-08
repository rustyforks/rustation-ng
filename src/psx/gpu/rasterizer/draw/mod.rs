mod fixed_point;

#[cfg(test)]
mod tests;

use std::sync::mpsc;

use super::{Command, CommandBuffer, Frame, Special};
use crate::psx::gpu::commands::Shaded;
use crate::psx::gpu::commands::{NoShading, Position};
use crate::psx::gpu::commands::{NoTexture, Opaque, ShadingMode, TextureMode, TransparencyMode};
use crate::psx::gpu::{DrawMode, MaskSettings};
use fixed_point::FixedPoint;
use std::cmp::{max, min};
use std::fmt;

#[derive(Debug)]
enum State {
    /// We're waiting for the next command
    WaitingForCommand,
    /// We're uploading data to the VRAM.
    VRamStore(VRamStore),
}

pub struct Rasterizer {
    vram: Box<[VRamPixel; 1024 * 512]>,
    state: State,
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
            state: State::WaitingForCommand,
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
                        match self.state {
                            State::WaitingForCommand => {
                                let op = v >> 24;
                                let h = &GP0_COMMANDS[op as usize];
                                // The longest possible draw command is 12 word long (shaded and
                                // textured quad)
                                let mut params = [0; 12];

                                params[0] = *v;

                                let len = h.len as usize;

                                for i in 1..len {
                                    // The main GPU code is supposed to send us complete draw
                                    // commands so it should be safe to expect the right number of
                                    // parameters here.
                                    match command_i.next() {
                                        Some(Command::Gp0(v)) => params[i] = *v,
                                        other => panic!("Expected GP0 command, got {:?}", other),
                                    }
                                }

                                (h.handler)(self, &params[..len]);
                            }
                            State::VRamStore(ref mut store) => {
                                let p0 = VRamPixel::from_mbgr1555(*v as u16);
                                let p1 = VRamPixel::from_mbgr1555((*v >> 16) as u16);

                                for &p in [p0, p1].iter() {
                                    let vram_off = store.target_vram_offset();

                                    // XXX handle mask bit
                                    self.vram[vram_off] = p;

                                    if store.next().is_none() {
                                        // End of store
                                        self.state = State::WaitingForCommand;
                                        break;
                                    }
                                }
                            }
                        }
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

    fn draw_triangle<Transparency, Texture, Shading>(&mut self, mut vertices: [Vertex; 3])
    where
        Transparency: TransparencyMode,
        Texture: TextureMode,
        Shading: ShadingMode,
    {
        // Order the vertices by y
        vertices.sort_by(|a, b| a.position.y.cmp(&b.position.y));

        let a = &vertices[0];
        let b = &vertices[1];
        let c = &vertices[2];

        // We need to draw split the triangle in two sub-triangles. Consider the following
        // triangle:
        //
        //    A
        //    +
        //    |\
        //    | \
        //    |  \
        //  H +   + B <-- Need to cut horizontally here.
        //    |  /
        //    | /
        //    |/
        //    +
        //    C
        //
        // Note that since we order A, B and C by Y coordinate it's possible for B to be on either
        // side of the triangle (see `ac_is_left` below)
        //
        // In order to draw it simply we need to break it into two triangles by drawing an
        // horizontal line at B. Then we can simply iterate on each line from A to B, with the line
        // width increasing by a constant amount at every step. Then we can do the same thing from
        // B to C
        //
        // Of course in some situations we'll end up with "flat" triangles, where one edge is
        // perfectly horizontal and A.x == B.x or C.x == B.x, in which case one of these
        // sub-triangles will effectively have 0 height.

        let y_min = a.position.y;
        let y_max = c.position.y;

        if y_max - y_min >= 512 {
            // Triangle is too tall, give up
            return;
        }

        if y_max < self.clip_y_min || y_min > self.clip_y_max {
            // The triangle is fully above or below the clip area, we don't have anything to draw
            return;
        }

        // Find the left side of the bounding box and the index of the core vertex. The core vertex
        // is the one we'll start drawing from.
        let (x_min, core_index) = vertices
            .iter()
            .min_by(|v0, v1| {
                // Here's the trick: the "core" vertex is the leftmost one. If two vertices are
                // lined up vertically on the left they'll both be equally leftmost, in this case
                // we take the one that comes *last* in the command.
                v0.position
                    .x
                    .cmp(&v1.position.x)
                    .then_with(|| v1.index.cmp(&v0.index))
            })
            .map(|core_vertex| (core_vertex.position.x, core_vertex.index))
            .unwrap();

        let x_max = vertices.iter().map(|v| v.position.x).max().unwrap();

        if x_max - x_min >= 1024 {
            // Triangle is too large, give up
            return;
        }

        if x_max < self.clip_x_min || x_min > self.clip_x_max {
            // The triangle is fully to the left or right of the draw area, we don't have anything
            // to draw
            return;
        }

        let xproduct = cross_product(a.position, b.position, c.position);

        if xproduct == 0 {
            // All three vertices are aligned, the triangle is perfectly flat and we have nothing
            // to draw
            return;
        }

        // True if AC is the left edge and AB + BC are the right edges, false if it's the other way
        // around
        let ac_is_left = xproduct > 0;

        let a_x = a.position.x;
        let b_x = b.position.x;
        let c_x = c.position.x;

        let a_y = a.position.y;
        let b_y = b.position.y;
        let c_y = c.position.y;

        // Slope of AC. We've already checked that the triangle had non-0 screen height, so we know
        // that this can't be a division by 0
        let ac_dxdy = FixedPoint::new_dxdy(c_x - a_x, c_y - a_y);

        // Slope of AB
        let ab_dxdy = if a_y != b_y {
            FixedPoint::new_dxdy(b_x - a_x, b_y - a_y)
        } else {
            // AB is horizontal, we won't have to use this variable
            FixedPoint::new(0)
        };

        // Slope of BC
        let bc_dxdy = if b_y != c_y {
            FixedPoint::new_dxdy(c_x - b_x, c_y - b_y)
        } else {
            // BC is horizontal, we won't have to use this variable
            FixedPoint::new(0)
        };

        let a_fpx = FixedPoint::new_saturated(a_x);
        let b_fpx = FixedPoint::new_saturated(b_x);
        let c_fpx = FixedPoint::new_saturated(c_x);
        // Coordinate of the point on AC that has the same y as B
        let h_fpx = a_fpx + ac_dxdy * (b_y - a_y);

        // The draw order depends on the core_vertex.
        if core_index == a.index {
            // We draw AB then BC

            if a_y != b_y {
                // Draw AB
                let (left_dxdy, right_dxdy) = if ac_is_left {
                    (ac_dxdy, ab_dxdy)
                } else {
                    (ab_dxdy, ac_dxdy)
                };

                let rc = RasterCoords {
                    start_y: a_y,
                    end_y: b_y,
                    left_x: a_fpx,
                    right_x: a_fpx,
                    left_dxdy,
                    right_dxdy,
                };

                self.rasterize::<Transparency, Texture, Shading>(rc, RasterDir::Down);
            }

            if b_y != c_y {
                // Draw BC
                let (left_x, left_dxdy, right_x, right_dxdy) = if ac_is_left {
                    (h_fpx, ac_dxdy, b_fpx, bc_dxdy)
                } else {
                    (b_fpx, bc_dxdy, h_fpx, ac_dxdy)
                };

                let rc = RasterCoords {
                    start_y: b_y,
                    end_y: c_y,
                    left_x,
                    right_x,
                    left_dxdy,
                    right_dxdy,
                };

                self.rasterize::<Transparency, Texture, Shading>(rc, RasterDir::Down);
            }
        } else {
            // Core vertex is B or C

            if b_y != c_y {
                if core_index == b.index {
                    // Draw BC
                    let (left_x, left_dxdy, right_x, right_dxdy) = if ac_is_left {
                        (h_fpx, ac_dxdy, b_fpx, bc_dxdy)
                    } else {
                        (b_fpx, bc_dxdy, h_fpx, ac_dxdy)
                    };

                    let rc = RasterCoords {
                        start_y: b_y,
                        end_y: c_y,
                        left_x,
                        right_x,
                        left_dxdy,
                        right_dxdy,
                    };

                    self.rasterize::<Transparency, Texture, Shading>(rc, RasterDir::Down);
                } else {
                    // Core vertex is C. Draw CB.
                    let (left_dxdy, right_dxdy) = if ac_is_left {
                        (ac_dxdy, bc_dxdy)
                    } else {
                        (bc_dxdy, ac_dxdy)
                    };

                    let rc = RasterCoords {
                        start_y: c_y,
                        end_y: b_y,
                        left_x: c_fpx,
                        right_x: c_fpx,
                        left_dxdy,
                        right_dxdy,
                    };

                    self.rasterize::<Transparency, Texture, Shading>(rc, RasterDir::Up);
                }
            }

            // If the core vertex is B or C we always end up by drawing BA
            if a_y != b_y {
                let (left_x, left_dxdy, right_x, right_dxdy) = if ac_is_left {
                    (h_fpx, ac_dxdy, b_fpx, ab_dxdy)
                } else {
                    (b_fpx, ab_dxdy, h_fpx, ac_dxdy)
                };

                let rc = RasterCoords {
                    start_y: b_y,
                    end_y: a_y,
                    left_x,
                    right_x,
                    left_dxdy,
                    right_dxdy,
                };

                self.rasterize::<Transparency, Texture, Shading>(rc, RasterDir::Up);
            }
        }
    }

    fn rasterize<Transparency, Texture, Shading>(&mut self, rc: RasterCoords, dir: RasterDir)
    where
        Transparency: TransparencyMode,
        Texture: TextureMode,
        Shading: ShadingMode,
    {
        let mut y = rc.start_y;
        let mut left_x = rc.left_x;
        let mut right_x = rc.right_x;

        if dir == RasterDir::Up {
            while y != rc.end_y {
                // We move first, then we draw.
                y -= 1;
                left_x -= rc.left_dxdy;
                right_x -= rc.right_dxdy;

                if y < self.clip_y_min {
                    // We left the drawing area
                    break;
                }

                if y <= self.clip_y_max {
                    self.rasterize_line::<Transparency, Texture, Shading>(
                        y,
                        left_x.truncate(),
                        right_x.truncate(),
                    );
                }
            }
        } else {
            while y != rc.end_y {
                if y > self.clip_y_max {
                    // We left the drawing area
                    break;
                }

                if y >= self.clip_y_min {
                    self.rasterize_line::<Transparency, Texture, Shading>(
                        y,
                        left_x.truncate(),
                        right_x.truncate(),
                    );
                }

                y += 1;
                left_x += rc.left_dxdy;
                right_x += rc.right_dxdy;
            }
        }
    }

    fn rasterize_line<Transparency, Texture, Shading>(&mut self, y: i32, left_x: i32, right_x: i32)
    where
        Transparency: TransparencyMode,
        Texture: TextureMode,
        Shading: ShadingMode,
    {
        let left_x = max(left_x, self.clip_x_min);
        let right_x = min(right_x, self.clip_x_max);

        if left_x >= right_x {
            // Line is either 0-length or clipped
            return;
        }

        // XXX fixme
        let color = Bgr888::from_command(0x00_00ff);
        for x in left_x..right_x {
            self.draw_solid_pixel::<Transparency>(x, y, color);
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum RasterDir {
    /// We drawing lines from top to bottom
    Down,
    /// We drawing lines from bottom to top
    Up,
}

/// Structure containing the definition of the various coordinates necessary to rasterize a
/// triangle
struct RasterCoords {
    /// Y coordinate of the first line to be drawn
    start_y: i32,
    /// Y coordinate of the first line *not* to be drawn
    end_y: i32,
    /// X coordinate of the left side of the first line to be drawn
    left_x: FixedPoint,
    /// X coordinate of the right side of the first line to be drawn
    right_x: FixedPoint,
    /// Value added or subtracted to left_x every time we move down or up one line (respectively)
    left_dxdy: FixedPoint,
    /// Value added or subtracted to right_x every time we move down or up one line (respectively)
    right_dxdy: FixedPoint,
}

/// Compute the cross-product of (AB) x (AC)
fn cross_product(a: Position, b: Position, c: Position) -> i32 {
    (b.x - a.x) * (c.y - a.y) - (c.x - a.x) * (b.y - a.y)
}

/// A single BGR1555 VRAM pixel. In order to make the emulation code simpler and to support
/// increased color depth we always use xRGB 1888 internally
#[derive(Copy, Clone, PartialEq, Eq)]
struct VRamPixel(u32);

impl VRamPixel {
    fn new() -> VRamPixel {
        VRamPixel(0)
    }

    fn from_bgr888(bgr: Bgr888) -> VRamPixel {
        VRamPixel((bgr.red() << 16) | (bgr.green() << 8) | bgr.blue())
    }

    fn from_mbgr1555(mbgr: u16) -> VRamPixel {
        let r = (mbgr & 0x1f) as u32;
        let g = ((mbgr >> 5) & 0x1f) as u32;
        let b = ((mbgr >> 10) & 0x1f) as u32;
        let m = ((mbgr >> 15) & 1) as u32;

        // We want to extend to RGB888 so we copy the 3 MBS to the LSBs (this way black remains
        // black and white remains white)
        let r = (r << 3) | (r >> 2);
        let g = (g << 3) | (g >> 2);
        let b = (b << 3) | (b >> 2);

        VRamPixel(b | (g << 8) | (r << 16) | (m << 24))
    }

    fn to_rgb888(self) -> u32 {
        self.0 & 0xff_ff_ff
    }

    fn red(self) -> u8 {
        (self.0 & 0xff) as u8
    }

    fn green(self) -> u8 {
        (self.0 >> 8) as u8
    }

    fn blue(self) -> u8 {
        (self.0 >> 16) as u8
    }
}

impl fmt::Display for VRamPixel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "#{:02x}{:02x}{:02x}",
            self.red(),
            self.green(),
            self.blue()
        )
    }
}

impl fmt::Debug for VRamPixel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Copy, Clone, Debug)]
struct Bgr888(u32);

impl Bgr888 {
    fn black() -> Bgr888 {
        Bgr888(0)
    }

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
}

/// Description of a vertex with position, texture and shading (depending on the command)
#[derive(Debug, Clone)]
struct Vertex {
    position: Position,
    color: Bgr888,
    texture: u32,
    /// The order in which the vertices are received is sometimes important, so we keep track of
    /// the index in the original command here.
    index: u8,
}

impl Vertex {
    fn new(index: u8) -> Vertex {
        Vertex {
            position: Position::new(0, 0),
            color: Bgr888::from_command(0),
            texture: 0,
            index,
        }
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
    let mut vertices = [
        Vertex::new(0),
        Vertex::new(1),
        Vertex::new(2),
        Vertex::new(3),
    ];

    let mut index = 0;
    let mut cur_color = Bgr888::black();

    // Load the vertex data from the command
    for (v, vertex) in vertices.iter_mut().enumerate() {
        if v == 0 || Shading::is_shaded() {
            cur_color = Bgr888::from_command(params[index]);
            index += 1;
        }

        vertex.color = cur_color;

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

    let triangle = [
        vertices[0].clone(),
        vertices[1].clone(),
        vertices[2].clone(),
    ];
    rasterizer.draw_triangle::<Transparency, Texture, Shading>(triangle);

    // Clippy wants us to remove the last clone here, but doing so generates a compilation error
    #[allow(clippy::redundant_clone)]
    let triangle = [
        vertices[1].clone(),
        vertices[2].clone(),
        vertices[3].clone(),
    ];
    rasterizer.draw_triangle::<Transparency, Texture, Shading>(triangle);
}

fn cmd_handle_poly_tri<Transparency, Texture, Shading>(rasterizer: &mut Rasterizer, params: &[u32])
where
    Transparency: TransparencyMode,
    Texture: TextureMode,
    Shading: ShadingMode,
{
    let mut vertices = [Vertex::new(0), Vertex::new(1), Vertex::new(2)];

    let mut index = 0;
    let mut cur_color = Bgr888::black();

    // Load the vertex data from the command
    for (v, vertex) in vertices.iter_mut().enumerate() {
        if v == 0 || Shading::is_shaded() {
            cur_color = Bgr888::from_command(params[index]);
            index += 1;
        }
        vertex.color = cur_color;

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

    rasterizer.draw_triangle::<Transparency, Texture, Shading>(vertices);
}

#[derive(Debug)]
struct VRamStore {
    x_min: u16,
    x_max: u16,
    y_min: u16,
    y_max: u16,
    /// Current X coordinate, from x_min to x_max
    x: u16,
    /// Current Y coordinate, from y_min to y_max
    y: u16,
}

impl VRamStore {
    fn new(left: u16, top: u16, width: u16, height: u16) -> VRamStore {
        debug_assert!(width > 0);
        debug_assert!(height > 0);

        VRamStore {
            x_min: left,
            x_max: left + width,
            y_min: top,
            y_max: top + height,
            x: left,
            y: top,
        }
    }

    fn target_vram_offset(&self) -> usize {
        let x = (self.x % 1024) as usize;
        let y = (self.y % 512) as usize;

        y * 1024 + x
    }

    fn next(&mut self) -> Option<()> {
        self.x += 1;

        if self.x == self.x_max {
            self.x = self.x_min;
            self.y += 1;

            if self.y == self.y_max {
                // End of transfer
                return None;
            }
        }

        Some(())
    }
}

fn cmd_vram_store(rasterizer: &mut Rasterizer, params: &[u32]) {
    let pos = params[1];
    let dim = params[2];

    let left = (pos & 0x3ff) as u16;
    let top = ((pos >> 16) & 0x3ff) as u16;

    // Width is in GPU pixels, i.e. 16bits per pixel
    let mut width = (dim & 0x3ff) as u16;
    let mut height = ((dim >> 16) & 0x1ff) as u16;

    // XXX recheck this, a comment in mednafen says that the results for VRAM load are inconsistent
    if width == 0 {
        width = 1024;
    }

    if height == 0 {
        height = 512;
    }

    let store = VRamStore::new(left, top, width, height);

    rasterizer.state = State::VRamStore(store);
}

fn cmd_vram_load(rasterizer: &mut Rasterizer, params: &[u32]) {
    let _ = rasterizer;
    let _ = params;
    warn!("Implement VRAM load");
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
        handler: cmd_handle_poly_tri::<Opaque, NoTexture, NoShading>,
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
        handler: cmd_handle_poly_tri::<Opaque, NoTexture, Shaded>,
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
        handler: cmd_handle_poly_quad::<Opaque, NoTexture, Shaded>,
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
        handler: cmd_vram_store,
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
        handler: cmd_vram_load,
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
