mod fixed_point;

#[cfg(test)]
mod tests;

use std::sync::mpsc;

use super::{Command, CommandBuffer, Frame, Special};
use crate::psx::gpu::commands::Shaded;
use crate::psx::gpu::commands::{NoShading, Position, Transparent};
use crate::psx::gpu::commands::{NoTexture, Opaque, ShadingMode, TextureBlending, TextureRaw};
use crate::psx::gpu::commands::{TextureMode, TransparencyMode};

use crate::psx::gpu::{DrawMode, MaskSettings, TextureWindow, TransparencyFunction};
use fixed_point::{FpCoord, FpVar};
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
    vram: Box<[Pixel; 1024 * 512]>,
    state: State,
    /// Frame currently being drawn
    cur_frame: Frame,
    /// Channel used to receive commands
    command_channel: mpsc::Receiver<CommandBuffer>,
    /// Channel used to send completed frames back
    frame_channel: mpsc::Sender<Frame>,
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
    /// Mask bit settings
    mask_settings: MaskSettings,
    /// Texture mapping state
    tex_mapper: TextureMapper,
}

impl Rasterizer {
    pub fn new(
        command_channel: mpsc::Receiver<CommandBuffer>,
        frame_channel: mpsc::Sender<Frame>,
    ) -> Rasterizer {
        Rasterizer {
            vram: box_array![Pixel::black(); 1024 * 512],
            state: State::WaitingForCommand,
            cur_frame: Frame::new(1024, 512),
            command_channel,
            frame_channel,
            clip_x_min: 0,
            clip_y_min: 0,
            clip_x_max: 1,
            clip_y_max: 0,
            draw_offset_x: 0,
            draw_offset_y: 0,
            mask_settings: MaskSettings::new(),
            tex_mapper: TextureMapper::new(),
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
                                let p0 = Pixel::from_mbgr1555(*v as u16);
                                let p1 = Pixel::from_mbgr1555((*v >> 16) as u16);

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
            // _ => warn!("Unimplemented GP1 {:x}", val),
            _ => (),
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
        self.clip_x_min = 0;
        self.clip_y_min = 0;
        self.clip_x_max = 1;
        self.clip_y_max = 0;
        self.draw_offset_x = 0;
        self.draw_offset_y = 0;
        self.draw_offset_x = 0;
        self.draw_offset_y = 0;
        self.tex_mapper.reset();
        self.mask_settings.set(0);
    }

    fn draw_solid_pixel<Transparency>(&mut self, x: i32, y: i32, mut color: Pixel)
    where
        Transparency: TransparencyMode,
    {
        debug_assert!(x >= 0 && x < 1024);
        debug_assert!(y >= 0 && y < 512);

        let vram_off = y * 1024 + x;

        let vram_pixel = &mut self.vram[vram_off as usize];

        // If the draw command is semi-transparent and the mask bit is set, this is a transparent
        // pixel
        let is_transparent = Transparency::is_transparent() && color.mask();

        // XXX implement masking

        if is_transparent {
            let bg_pixel = *vram_pixel;
            let mode = self.tex_mapper.draw_mode.transparency_mode();

            color.apply_transparency(bg_pixel, mode);
        }

        *vram_pixel = color;
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
        // In order to draw it simply we need to break it into two sub-triangles by splitting the
        // full triangle with an horizontal line at B.
        //
        // To make matters more complicated the sub-triangle draw order (and whether they're draw
        // top-to-bottom or bottom-to-top) depends on the coordinates of the vertices and the order
        // in which they're received by the GPU (see how core_vertex is determined below).
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
        let core_vertex = vertices
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
            .unwrap();

        let x_min = core_vertex.position.x;

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

        let deltas = RasterVarDeltas::new::<Texture, Shading>(xproduct, &vertices);
        // Initialize the variables with the core vertex values, then move to 0, 0. This way we'll
        // then be able to interpolate the value of the variables for any absolute coordinates
        let mut vars = RasterVars::new::<Texture>(core_vertex);
        vars.translate_by::<Texture, Shading>(&deltas, -core_vertex.x(), -core_vertex.y());

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
        let ac_dxdy = FpCoord::new_dxdy(c_x - a_x, c_y - a_y);

        // Slope of AB
        let ab_dxdy = if a_y != b_y {
            FpCoord::new_dxdy(b_x - a_x, b_y - a_y)
        } else {
            // AB is horizontal, we won't have to use this variable
            FpCoord::new(0)
        };

        // Slope of BC
        let bc_dxdy = if b_y != c_y {
            FpCoord::new_dxdy(c_x - b_x, c_y - b_y)
        } else {
            // BC is horizontal, we won't have to use this variable
            FpCoord::new(0)
        };

        let a_fpx = FpCoord::new_saturated(a_x);
        let b_fpx = FpCoord::new_saturated(b_x);
        let c_fpx = FpCoord::new_saturated(c_x);
        // Coordinate of the point on AC that has the same y as B
        let h_fpx = a_fpx + ac_dxdy * (b_y - a_y);

        // The draw order depends on the core_vertex.
        if core_vertex.index == a.index {
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

                self.rasterize::<Transparency, Texture, Shading>(
                    rc,
                    &vars,
                    &deltas,
                    RasterDir::Down,
                );
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

                self.rasterize::<Transparency, Texture, Shading>(
                    rc,
                    &vars,
                    &deltas,
                    RasterDir::Down,
                );
            }
        } else {
            // Core vertex is B or C

            if b_y != c_y {
                if core_vertex.index == b.index {
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

                    self.rasterize::<Transparency, Texture, Shading>(
                        rc,
                        &vars,
                        &deltas,
                        RasterDir::Down,
                    );
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

                    self.rasterize::<Transparency, Texture, Shading>(
                        rc,
                        &vars,
                        &deltas,
                        RasterDir::Up,
                    );
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

                self.rasterize::<Transparency, Texture, Shading>(rc, &vars, &deltas, RasterDir::Up);
            }
        }
    }

    fn rasterize<Transparency, Texture, Shading>(
        &mut self,
        rc: RasterCoords,
        vars: &RasterVars,
        deltas: &RasterVarDeltas,
        dir: RasterDir,
    ) where
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
                        vars.clone(),
                        deltas,
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
                        vars.clone(),
                        deltas,
                    );
                }

                y += 1;
                left_x += rc.left_dxdy;
                right_x += rc.right_dxdy;
            }
        }
    }

    fn rasterize_line<Transparency, Texture, Shading>(
        &mut self,
        y: i32,
        left_x: i32,
        right_x: i32,
        mut vars: RasterVars,
        deltas: &RasterVarDeltas,
    ) where
        Transparency: TransparencyMode,
        Texture: TextureMode,
        Shading: ShadingMode,
    {
        let start_x = max(left_x, self.clip_x_min);
        let end_x = min(right_x, self.clip_x_max);

        if start_x >= end_x {
            // Line is either 0-length or clipped
            return;
        }

        // We "move" the variables to the start of the line
        vars.translate_by::<Texture, Shading>(deltas, start_x, y);

        for x in start_x..end_x {
            if Texture::is_textured() {
                let texel = self.get_texel(vars.u(), vars.v());
                // If the pixel is equal to 0 (including mask bit) then we don't draw it
                if !texel.is_nul() {
                    if Texture::is_raw_texture() {
                        self.draw_solid_pixel::<Transparency>(x, y, texel);
                    } else {
                        // Texture blending: the final color is a combination of the texel and
                        // the computed gouraud color
                        let blend = texel.blend(vars.color());
                        self.draw_solid_pixel::<Transparency>(x, y, blend);
                    }
                }
            } else {
                // No texture
                let mut color = vars.color();

                // We have to set the mask bit since it's used to test if the pixel should be
                // transparent, and non-textured transparent draw calls are always fully
                // transparent.
                color.set_mask();

                self.draw_solid_pixel::<Transparency>(x, y, color);
            }
            vars.translate_right::<Texture, Shading>(deltas);
        }
    }

    fn get_texel(&self, u: u8, v: u8) -> Pixel {
        self.tex_mapper.get_texel(u, v, &self.vram)
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
    left_x: FpCoord,
    /// X coordinate of the right side of the first line to be drawn
    right_x: FpCoord,
    /// Value added or subtracted to left_x every time we move down or up one line (respectively)
    left_dxdy: FpCoord,
    /// Value added or subtracted to right_x every time we move down or up one line (respectively)
    right_dxdy: FpCoord,
}

/// Structure containing the various delta values for Gouraud shading and texture mapping
struct RasterVarDeltas {
    /// Value added or subtracted to the red component every time we move along the X axis
    drdx: FpVar,
    /// Value added or subtracted to the red component every time we move along the Y axis
    drdy: FpVar,
    /// Value added or subtracted to the green component every time we move along the X axis
    dgdx: FpVar,
    /// Value added or subtracted to the green component every time we move along the Y axis
    dgdy: FpVar,
    /// Value added or subtracted to the blue component every time we move along the X axis
    dbdx: FpVar,
    /// Value added or subtracted to the blue component every time we move along the Y axis
    dbdy: FpVar,

    /// Value added or subtracted to the texture U coordinate every time we move along the X axis
    dudx: FpVar,
    /// Value added or subtracted to the texture U coordinate every time we move along the Y axis
    dudy: FpVar,
    /// Value added or subtracted to the texture V coordinate every time we move along the X axis
    dvdx: FpVar,
    /// Value added or subtracted to the texture V coordinate every time we move along the Y axis
    dvdy: FpVar,
}

impl RasterVarDeltas {
    fn new<Texture, Shading>(xproduct: i32, vertices: &[Vertex; 3]) -> RasterVarDeltas
    where
        Texture: TextureMode,
        Shading: ShadingMode,
    {
        debug_assert!(xproduct != 0);

        let mut d = RasterVarDeltas {
            drdx: FpVar::new(0),
            drdy: FpVar::new(0),
            dgdx: FpVar::new(0),
            dgdy: FpVar::new(0),
            dbdx: FpVar::new(0),
            dbdy: FpVar::new(0),
            dudx: FpVar::new(0),
            dudy: FpVar::new(0),
            dvdx: FpVar::new(0),
            dvdy: FpVar::new(0),
        };

        if Shading::is_shaded() {
            // Compute the gradient deltas for every component along both axes
            d.drdx = Self::compute_delta(xproduct, vertices, |v| v.red(), |v| v.y());
            d.drdy = Self::compute_delta(xproduct, vertices, |v| v.x(), |v| v.red());
            d.dgdx = Self::compute_delta(xproduct, vertices, |v| v.green(), |v| v.y());
            d.dgdy = Self::compute_delta(xproduct, vertices, |v| v.x(), |v| v.green());
            d.dbdx = Self::compute_delta(xproduct, vertices, |v| v.blue(), |v| v.y());
            d.dbdy = Self::compute_delta(xproduct, vertices, |v| v.x(), |v| v.blue());
        }

        if Texture::is_textured() {
            d.dudx = Self::compute_delta(xproduct, vertices, |v| i32::from(v.u), |v| v.y());
            d.dudy = Self::compute_delta(xproduct, vertices, |v| v.x(), |v| i32::from(v.u));
            d.dvdx = Self::compute_delta(xproduct, vertices, |v| i32::from(v.v), |v| v.y());
            d.dvdy = Self::compute_delta(xproduct, vertices, |v| v.x(), |v| i32::from(v.v));
        }

        d
    }

    fn compute_delta<X, Y>(xproduct: i32, vertices: &[Vertex; 3], get_x: X, get_y: Y) -> FpVar
    where
        X: Fn(&Vertex) -> i32,
        Y: Fn(&Vertex) -> i32,
    {
        let xp = cross_product_with(&vertices[0], &vertices[1], &vertices[2], get_x, get_y);

        FpVar::new(xp) / xproduct
    }
}

/// Variables used during rasterization
#[derive(Debug, Clone)]
struct RasterVars {
    /// Red shading component
    red: FpVar,
    /// Green shading component
    green: FpVar,
    /// Blue shading component
    blue: FpVar,
    /// Texture U coordinate
    u: FpVar,
    /// Texture V coordinate
    v: FpVar,
}

impl RasterVars {
    fn new<Texture>(vertex: &Vertex) -> RasterVars
    where
        Texture: TextureMode,
    {
        let (u, v) = if Texture::is_textured() {
            (
                FpVar::new_center(i32::from(vertex.u)),
                FpVar::new_center(i32::from(vertex.v)),
            )
        } else {
            (FpVar::new(0), FpVar::new(0))
        };

        // We set the red/green/blue even if shading is disabled since they'll be used for solid
        // shading as well
        RasterVars {
            red: FpVar::new_center(vertex.red()),
            green: FpVar::new_center(vertex.green()),
            blue: FpVar::new_center(vertex.blue()),
            u,
            v,
        }
    }

    fn translate_by<Texture, Shading>(&mut self, deltas: &RasterVarDeltas, x_off: i32, y_off: i32)
    where
        Texture: TextureMode,
        Shading: ShadingMode,
    {
        if Texture::is_textured() {
            self.u += deltas.dudx * x_off;
            self.u += deltas.dudy * y_off;
            self.v += deltas.dvdx * x_off;
            self.v += deltas.dvdy * y_off;
        }

        if Shading::is_shaded() {
            self.red += deltas.drdx * x_off;
            self.red += deltas.drdy * y_off;
            self.green += deltas.dgdx * x_off;
            self.green += deltas.dgdy * y_off;
            self.blue += deltas.dbdx * x_off;
            self.blue += deltas.dbdy * y_off;
        }
    }

    /// Move var one pixel to the right
    fn translate_right<Texture, Shading>(&mut self, deltas: &RasterVarDeltas)
    where
        Texture: TextureMode,
        Shading: ShadingMode,
    {
        if Texture::is_textured() {
            self.u += deltas.dudx;
            self.v += deltas.dvdx;
        }

        if Shading::is_shaded() {
            self.red += deltas.drdx;
            self.green += deltas.dgdx;
            self.blue += deltas.dbdx;
        }
    }

    fn color(&self) -> Pixel {
        let r = self.red.truncate() as u8;
        let b = self.blue.truncate() as u8;
        let g = self.green.truncate() as u8;

        Pixel::from_rgb(r, g, b)
    }

    fn u(&self) -> u8 {
        self.u.truncate() as u8
    }

    fn v(&self) -> u8 {
        self.v.truncate() as u8
    }
}

/// Compute the cross-product of (AB) x (AC) using the provided getters for x and y
fn cross_product_with<X, Y>(a: &Vertex, b: &Vertex, c: &Vertex, get_x: X, get_y: Y) -> i32
where
    X: Fn(&Vertex) -> i32,
    Y: Fn(&Vertex) -> i32,
{
    let a_x = get_x(a);
    let b_x = get_x(b);
    let c_x = get_x(c);
    let a_y = get_y(a);
    let b_y = get_y(b);
    let c_y = get_y(c);

    (b_x - a_x) * (c_y - a_y) - (c_x - a_x) * (b_y - a_y)
}

/// Compute the cross-product of (AB) x (AC)
fn cross_product(a: Position, b: Position, c: Position) -> i32 {
    (b.x - a.x) * (c.y - a.y) - (c.x - a.x) * (b.y - a.y)
}

/// Structure keeping track of the state needed to convert the extrapolated 8bit U/V values of the
/// rasterizer into absolute coordinates in VRAM. The mapping is non-trivial because the PSX GPU
/// uses 256x256 texture pages, coordinate masking and CLUTs of various depths.
struct TextureMapper {
    /// Draw mode configuration
    draw_mode: DrawMode,
    /// Texture window settings
    tex_window: TextureWindow,
    /// AND mask applied to U coordinates
    u_mask: u8,
    /// AND mask applied to V coordinates
    v_mask: u8,
    /// Value added to U coordinates to find the raw (unpaletted) texel value in VRAM. This value
    /// is a texel offset, not a VRAM pixel offset. Texel size can be 4, 8 or 16bits per pixel,
    /// VRAM pixels on the other hand are always 16bits wide (natively)
    u_offset: u16,
    /// Value added to V coordinates to find the raw (unpaletted) texel value in VRAM
    v_offset: u16,
    /// Shift value to convert a number of texels into a number of VRAM pixels. In other words, you
    /// have `(1 << pixel_to_texel_shift)` texels per VRAM pixel.
    pixel_to_texel_shift: u8,
    /// Offset of the start of the CLUT in VRAM
    clut_off: u32,
}

impl TextureMapper {
    pub fn new() -> TextureMapper {
        TextureMapper {
            draw_mode: DrawMode::new(),
            tex_window: TextureWindow::new(),
            u_mask: 0,
            v_mask: 0,
            u_offset: 0,
            v_offset: 0,
            pixel_to_texel_shift: 0,
            clut_off: 0,
        }
    }

    pub fn reset(&mut self) {
        self.draw_mode.set(0);
        self.tex_window.set(0);
        self.update_texture_params();
    }

    pub fn set_clut(&mut self, clut: u32) {
        let clut = clut >> 16;

        let clut_x = (clut & 0x3f) << 4;
        let clut_y = (clut >> 6) & 0x1ff;

        self.clut_off = clut_y * 1024 + clut_x;
    }

    pub fn set_draw_mode(&mut self, mode: u32) {
        self.draw_mode.set(mode);
        self.update_texture_params();
    }

    pub fn update_mode_from_poly(&mut self, mode: u32) {
        self.draw_mode.update_from_poly(mode);
        self.update_texture_params();
    }

    pub fn set_tex_window(&mut self, tw: u32) {
        self.tex_window.set(tw);
        self.update_texture_params();
    }

    fn update_texture_params(&mut self) {
        self.pixel_to_texel_shift = self.draw_mode.pixel_to_texel_shift();
        self.u_mask = self.tex_window.u_mask();
        self.v_mask = self.tex_window.v_mask();

        self.u_offset = u16::from(self.tex_window.u_offset());
        self.v_offset = u16::from(self.tex_window.v_offset());

        // Add texture page offset.
        let tp_x = self.draw_mode.texture_page_x();
        let tp_y = self.draw_mode.texture_page_y();

        // `texture_page_x` is in number of VRAM pixels, convert it to a number of texels
        self.u_offset += tp_x << self.pixel_to_texel_shift;
        self.v_offset += tp_y;
    }

    pub fn get_texel(&self, u: u8, v: u8, vram: &[Pixel; 1024 * 512]) -> Pixel {
        let pts = u16::from(self.pixel_to_texel_shift);
        let fb_u = u16::from(u & self.u_mask) + self.u_offset;
        let fb_v = u16::from(v & self.v_mask) + self.v_offset;

        let fb_x = (fb_u >> pts) & 0x3ff;
        let fb_y = fb_v;

        let fb_off = usize::from(fb_y) * 1024 + usize::from(fb_x);
        let raw = vram[fb_off as usize];

        if pts == 0 {
            // True color mode, we return the texture value as-is
            return raw;
        }

        // We're using a 4 or 8bpp paletted texture
        let raw = raw.to_mbgr1555();

        // XXX should we cache some of this in `update_texture_params`?

        // 4 for 4bpp, 8 for 8bpp
        let bits_per_texel = 16 >> pts;
        // 3 for 4bpp, 1 for 8bpp
        let u_mask = pts + (pts >> 1);
        // 2 for 4bpp, 3 for 8bpp
        let u_mul = 4 - pts;
        // 0, 4, 8 or 12 for 4bpp, 0 or 8 for 8bpp
        let u_shift = (fb_u & u_mask) << u_mul;
        // 0xf for 4bpp, 0xff for 8bpp
        let key_mask = (1 << bits_per_texel) - 1;

        let clut_key = (raw >> u_shift) & key_mask;

        let fb_pos = self.clut_off + u32::from(clut_key);

        vram[fb_pos as usize]
    }
}

/// Generic representation of a Pixel, used to represent both 24bit 888RGB colors and 1555xRGB VRAM
/// pixels. Internal representation is a single `u32` containing the values as 8888xRGB, meaning
/// that it can normally be passed straight to the frontend without conversion
#[derive(Copy, Clone, PartialEq, Eq)]
struct Pixel(u32);

impl Pixel {
    fn black() -> Pixel {
        Pixel(0)
    }

    /// For texels this method returns true if the pixel is all zeroes (which means that the pixel
    /// shouldn't be drawn)
    fn is_nul(self) -> bool {
        self.0 == 0
    }

    fn from_mbgr1555(mbgr: u16) -> Pixel {
        let r = (mbgr & 0x1f) as u32;
        let g = ((mbgr >> 5) & 0x1f) as u32;
        let b = ((mbgr >> 10) & 0x1f) as u32;
        let m = ((mbgr >> 15) & 1) as u32;

        // We want to extend to RGB888 so we copy the 3 MSBs to the LSBs (this way black remains
        // black and white remains white)
        let r = (r << 3) | (r >> 2);
        let g = (g << 3) | (g >> 2);
        let b = (b << 3) | (b >> 2);

        Pixel(b | (g << 8) | (r << 16) | (m << 24))
    }

    fn from_rgb(r: u8, g: u8, b: u8) -> Pixel {
        let r = r as u32;
        let g = g as u32;
        let b = b as u32;

        Pixel(b | (g << 8) | (r << 16))
    }

    fn from_command(cmd: u32) -> Pixel {
        let r = cmd & 0xff;
        let g = (cmd >> 8) & 0xff;
        let b = (cmd >> 16) & 0xff;

        Pixel(b | (g << 8) | (r << 16))
    }

    fn to_rgb888(self) -> u32 {
        self.0 & 0xff_ff_ff
    }

    fn to_mbgr1555(self) -> u16 {
        let m = self.mask() as u16;
        let r = (self.red() >> 3) as u16;
        let g = (self.green() >> 3) as u16;
        let b = (self.blue() >> 3) as u16;

        (m << 15) | (b << 10) | (g << 5) | r
    }

    fn red(self) -> u8 {
        (self.0 >> 16) as u8
    }

    fn green(self) -> u8 {
        (self.0 >> 8) as u8
    }

    fn blue(self) -> u8 {
        (self.0 & 0xff) as u8
    }

    fn mask(self) -> bool {
        (self.0 >> 24) != 0
    }

    fn set_mask(&mut self) {
        self.0 |= 0x0100_0000;
    }

    /// Perform texture blending
    fn blend(self, gouraud: Pixel) -> Pixel {
        let t_r = self.red() as u32;
        let t_g = self.green() as u32;
        let t_b = self.blue() as u32;

        let g_r = gouraud.red() as u32;
        let g_g = gouraud.green() as u32;
        let g_b = gouraud.blue() as u32;

        // In order to normalize the value we should be shifting by 8, but texture blending
        // actually doubles the value, hence the - 1.
        let mut r = (t_r * g_r) >> (8 - 1);
        let mut g = (t_g * g_g) >> (8 - 1);
        let mut b = (t_b * g_b) >> (8 - 1);

        // XXX Mednafen combines the saturation with the dithering lookup which is quite clever.
        // That would mean lowering the dithering here however.
        if r > 0xff {
            r = 0xff;
        }
        if g > 0xff {
            g = 0xff;
        }
        if b > 0xff {
            b = 0xff;
        }

        let mask = self.0 & 0xff00_0000;

        Pixel(mask | b | (g << 8) | (r << 16))
    }

    /// Blend `self` (the foreground pixel) and `bg_pixel` using the provided transparency mode
    fn apply_transparency(&mut self, bg_pixel: Pixel, mode: TransparencyFunction) {
        // XXX this is a very naive, and probably very slow implementation. We could probably do
        // that processing using the whole pixel value at once using some clever carry handling.
        let f_r = (self.0 >> 16) & 0xff;
        let f_g = (self.0 >> 8) & 0xff;
        let f_b = self.0 & 0xff;

        let b_r = (bg_pixel.0 >> 16) & 0xff;
        let b_g = (bg_pixel.0 >> 8) & 0xff;
        let b_b = bg_pixel.0 & 0xff;

        let o_r;
        let o_g;
        let o_b;

        match mode {
            TransparencyFunction::Average => {
                o_r = (f_r + b_r) >> 1;
                o_g = (f_g + b_g) >> 1;
                o_b = (f_b + b_b) >> 1;
            }
            TransparencyFunction::Add => {
                let s_r = f_r + b_r;
                let s_g = f_g + b_g;
                let s_b = f_b + b_b;

                o_r = if s_r > 0xff { 0xff } else { s_r };
                o_g = if s_g > 0xff { 0xff } else { s_g };
                o_b = if s_b > 0xff { 0xff } else { s_b };
            }
            TransparencyFunction::Sub => {
                let s_r = (0x100 | b_r) - f_r;
                let s_g = (0x100 | b_g) - f_g;
                let s_b = (0x100 | b_b) - f_b;

                o_r = if s_r <= 0xff { 0 } else { s_r };
                o_g = if s_g <= 0xff { 0 } else { s_g };
                o_b = if s_b <= 0xff { 0 } else { s_b };
            }
            TransparencyFunction::QuarterAdd => {
                let f_r = f_r >> 2;
                let f_g = f_g >> 2;
                let f_b = f_b >> 2;

                let s_r = f_r + b_r;
                let s_g = f_g + b_g;
                let s_b = f_b + b_b;

                o_r = if s_r > 0xff { 0xff } else { s_r };
                o_g = if s_g > 0xff { 0xff } else { s_g };
                o_b = if s_b > 0xff { 0xff } else { s_b };
            }
        };

        self.0 = (o_r << 16) | (o_g << 8) | o_b;
    }
}

impl fmt::Display for Pixel {
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

impl fmt::Debug for Pixel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

/// Description of a vertex with position, texture and shading (depending on the command)
#[derive(Debug, Clone)]
struct Vertex {
    position: Position,
    color: Pixel,
    /// Texture u coordinate, relative to the current texture page
    u: u8,
    /// Texture v coordinate, relative to the current texture page
    v: u8,
    /// The order in which the vertices are received is sometimes important, so we keep track of
    /// the index in the original command here.
    index: u8,
}

impl Vertex {
    fn new(index: u8) -> Vertex {
        Vertex {
            position: Position::new(0, 0),
            color: Pixel::from_command(0),
            u: 0,
            v: 0,
            index,
        }
    }

    fn set_position(&mut self, p: u32) {
        self.position = Position::from_command(p);
    }

    fn set_texture_uv(&mut self, uv: u32) {
        self.u = uv as u8;
        self.v = (uv >> 8) as u8;
    }

    fn x(&self) -> i32 {
        self.position.x
    }

    fn y(&self) -> i32 {
        self.position.y
    }

    fn red(&self) -> i32 {
        self.color.red() as i32
    }

    fn green(&self) -> i32 {
        self.color.green() as i32
    }

    fn blue(&self) -> i32 {
        self.color.blue() as i32
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
    let mut cur_color = Pixel::black();

    // Load the vertex data from the command
    for (v, vertex) in vertices.iter_mut().enumerate() {
        if v == 0 || Shading::is_shaded() {
            cur_color = Pixel::from_command(params[index]);
            index += 1;
        }

        vertex.color = cur_color;

        vertex.set_position(params[index]);
        index += 1;

        // Add the draw offset
        vertex.position.x += rasterizer.draw_offset_x;
        vertex.position.y += rasterizer.draw_offset_y;

        if Texture::is_textured() {
            if v == 0 {
                rasterizer.tex_mapper.set_clut(params[index]);
            } else if v == 1 {
                // Update texture page
                rasterizer.tex_mapper.update_mode_from_poly(params[index]);
            }
            vertex.set_texture_uv(params[index]);
            index += 1;
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
    let mut cur_color = Pixel::black();

    // Load the vertex data from the command
    for (v, vertex) in vertices.iter_mut().enumerate() {
        if v == 0 || Shading::is_shaded() {
            cur_color = Pixel::from_command(params[index]);
            index += 1;
        }
        vertex.color = cur_color;

        vertex.set_position(params[index]);
        index += 1;

        // Add the draw offset
        vertex.position.x += rasterizer.draw_offset_x;
        vertex.position.y += rasterizer.draw_offset_y;

        if Texture::is_textured() {
            if v == 0 {
                rasterizer.tex_mapper.set_clut(params[index]);
            } else if v == 1 {
                // Update texture page
                rasterizer.tex_mapper.update_mode_from_poly(params[index]);
            }
            vertex.set_texture_uv(params[index]);
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

/// Fill a rectangle with a solid color
fn cmd_fill_rect(rasterizer: &mut Rasterizer, params: &[u32]) {
    let color = Pixel::from_command(params[0]);
    let dst = params[1];
    let dim = params[2];

    let start_x = dst & 0x3f0;
    let start_y = (dst >> 16) & 0x3ff;

    let width = ((dim & 0x3ff) + 0xf) & !0xf;
    let height = (dim >> 16) & 0x1ff;

    for y in 0..height {
        let y_pos = (start_y + y) & 511;
        for x in 0..width {
            // Fill rect is supposed to ignore clip space and mask completely.
            //
            // XXX Probably worth adding a test just in case.
            let x_pos = (start_x + x) & 1023;

            let vram_index = y_pos * 1024 + x_pos;
            rasterizer.vram[vram_index as usize] = color;
        }
    }
}

fn cmd_tex_window(rasterizer: &mut Rasterizer, params: &[u32]) {
    rasterizer.tex_mapper.set_tex_window(params[0]);
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

    rasterizer.tex_mapper.set_draw_mode(mode);
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
        handler: cmd_fill_rect,
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
        handler: cmd_handle_poly_tri::<Transparent, NoTexture, NoShading>,
        len: 4,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_handle_poly_tri::<Opaque, TextureBlending, NoShading>,
        len: 7,
    },
    CommandHandler {
        handler: cmd_handle_poly_tri::<Opaque, TextureRaw, NoShading>,
        len: 7,
    },
    CommandHandler {
        handler: cmd_handle_poly_tri::<Transparent, TextureBlending, NoShading>,
        len: 7,
    },
    CommandHandler {
        handler: cmd_handle_poly_tri::<Transparent, TextureRaw, NoShading>,
        len: 7,
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
        handler: cmd_handle_poly_quad::<Transparent, NoTexture, NoShading>,
        len: 5,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_handle_poly_quad::<Opaque, TextureBlending, NoShading>,
        len: 9,
    },
    CommandHandler {
        handler: cmd_handle_poly_quad::<Opaque, TextureRaw, NoShading>,
        len: 9,
    },
    CommandHandler {
        handler: cmd_handle_poly_quad::<Transparent, TextureBlending, NoShading>,
        len: 9,
    },
    CommandHandler {
        handler: cmd_handle_poly_quad::<Transparent, TextureRaw, NoShading>,
        len: 9,
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
        handler: cmd_handle_poly_tri::<Transparent, NoTexture, Shaded>,
        len: 6,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_handle_poly_tri::<Opaque, TextureBlending, Shaded>,
        len: 9,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
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
        handler: cmd_handle_poly_quad::<Transparent, NoTexture, Shaded>,
        len: 8,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_handle_poly_quad::<Opaque, TextureBlending, Shaded>,
        len: 12,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_handle_poly_quad::<Transparent, TextureBlending, Shaded>,
        len: 12,
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
