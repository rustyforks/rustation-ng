mod fixed_point;

#[cfg(test)]
mod tests;

use std::sync::mpsc;

use super::{Command, CommandBuffer, Frame, RasterizerOption};
use crate::psx::gpu::commands::{vram_access_dimensions, Shaded};
use crate::psx::gpu::commands::{NoShading, Position, Transparent};
use crate::psx::gpu::commands::{NoTexture, Opaque, ShadingMode, TextureBlending, TextureRaw};
use crate::psx::gpu::commands::{TextureMode, TransparencyMode};

use crate::psx::gpu::{DisplayMode, DrawMode, MaskSettings, TextureWindow, TransparencyFunction};
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
    /// If true we output the entire contents of the VRAM instead of just the visible portion
    display_full_vram: bool,
    /// Number of the first line displayed on the screen
    display_line_start: u16,
    /// Number of the first line *not* displayed on the screen
    display_line_end: u16,
    /// Number of the first column displayed on the screen
    display_column_start: u16,
    /// Number of the first column *not* displayed on the screen
    display_column_end: u16,
    /// Current value of the display mode
    display_mode: DisplayMode,
    /// First column of the display area in VRAM
    display_vram_x_start: u16,
    /// First line of the display area in VRAM
    display_vram_y_start: u16,
    /// True if the display is disabled,
    display_off: bool,
    /// True to draw opaque pixel as semi-transparent
    force_transparency: bool,
    /// Dithering tables, used for dithering, 8-to-5bit color component truncation and saturation.
    ///
    /// Here's the explanation layer by layer:
    ///
    /// * `[_; 4]`: x % 4 to select the right position in the dithering pattern based on the vram
    ///   position.
    /// * `[_; 4]`: y % 4 to select the right position in the dithering pattern based on the vram
    ///   position.
    ///
    /// * `[_; 0x200]`: input value, from 0x000 to 0x1ff. Values above 0xff are saturated to 0xff
    dither_table: [[[u8; 0x200]; 4]; 4],
    /// True if dithering is currently enabled
    dither_enabled: bool,
    /// If true we force disable dithering, regardless of the draw mode. Should probably only be
    /// used when `draw_24bpp` is also true otherwise you'll get a lot of banding on shaded areas.
    dithering_force_disable: bool,
    /// If true we don't truncate the values drawn to the framebuffer to 15bit RGB555 like the real
    /// hardware but instead keep the full 24bit color depth. If this is true
    /// `dithering_force_disable` should probably also be true since it doesn't make a lot of sense
    /// to dither from 24bits to 24 bits...
    draw_24bpp: bool,
    /// True if we're interlaced and display the bottom field
    display_bottom_field: bool,
}

impl Rasterizer {
    pub fn new(
        command_channel: mpsc::Receiver<CommandBuffer>,
        frame_channel: mpsc::Sender<Frame>,
    ) -> Rasterizer {
        let mut rasterizer = Rasterizer {
            vram: box_array![Pixel::black(); 1024 * 512],
            state: State::WaitingForCommand,
            cur_frame: Frame::new(0, 0),
            command_channel,
            frame_channel,
            clip_x_min: 0,
            clip_y_min: 0,
            clip_x_max: 0,
            clip_y_max: 0,
            draw_offset_x: 0,
            draw_offset_y: 0,
            mask_settings: MaskSettings::new(),
            tex_mapper: TextureMapper::new(),
            display_full_vram: false,
            display_line_start: 0x10,
            display_line_end: 0x100,
            display_column_start: 0x200,
            display_column_end: 0xc00,
            display_mode: DisplayMode::new(),
            display_vram_x_start: 0,
            display_vram_y_start: 0,
            display_off: true,
            force_transparency: false,
            dither_table: [[[0; 0x200]; 4]; 4],
            dither_enabled: false,
            dithering_force_disable: false,
            draw_24bpp: false,
            display_bottom_field: false,
        };

        rasterizer.rebuild_dither_table();
        rasterizer.new_frame();

        rasterizer
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
                    Command::Quit => return,
                    // XXX draw one line at a time
                    Command::EndOfLine(l) => self.draw_line(*l),
                    Command::EndOfFrame => self.send_frame(),
                    Command::FieldChanged(f) => self.display_bottom_field = *f,
                    Command::Option(opt) => self.set_option(*opt),
                }
            }
        }
    }

    /// Returns `false` if the GPU config forbids writing to this line because it's currently
    /// displayed (currently only useful for interlaced output)
    pub fn can_draw_to_line(&self, y: i32) -> bool {
        if self.tex_mapper.draw_mode.draw_to_display_area() {
            // We can draw to display, no worries
            return true;
        }

        if !self.display_mode.is_true_interlaced() {
            // XXX We only implement the test for interlaced output for now, since that's the most
            // common situation where this leads to visual glitches
            return true;
        }

        // XXX This is how mednafen does it so it's probably safe enough but in practice this is
        // probably very wrong: we should probably still be able to draw to these lines if the X is
        // outside of the display. We should also be able to draw to these lines if they're below
        // or above the display area. In practice interlaced is uncommon enough that it's probably
        // good enough.
        let y_is_bottom = ((y + self.display_vram_y_start as i32) & 1) != 0;

        y_is_bottom != self.display_bottom_field
    }

    pub fn set_option(&mut self, opt: RasterizerOption) {
        match opt {
            RasterizerOption::DisplayFullVRam(v) => self.display_full_vram = v,
            RasterizerOption::ForceTransparency(v) => self.force_transparency = v,
            RasterizerOption::Draw24Bpp(v) => {
                if v != self.draw_24bpp {
                    self.draw_24bpp = v;
                    self.rebuild_dither_table();
                }
            }
            RasterizerOption::DitherForceDisable(v) => {
                self.dithering_force_disable = v;
                self.maybe_rebuild_dither_table();
            }
        }
    }

    /// Called when we should output a line to the output buffer
    pub fn draw_line(&mut self, line: u16) {
        if self.display_full_vram {
            // We're just going to dump the full VRAM, nothing to do
            return;
        }

        if self.display_off {
            // Output only black pixels
            return;
        }

        if line < self.display_line_start || line >= self.display_line_end {
            // Video is not active
            return;
        }

        let mut frame_y = line - self.display_line_start;
        if self.display_mode.is_true_interlaced() {
            frame_y = (frame_y << 1) | (self.display_bottom_field as u16);
        }

        let vram_y = self.display_vram_y_start + frame_y;

        self.output_line(self.display_vram_x_start, vram_y, frame_y);
    }

    fn output_line(&mut self, x_start: u16, vram_y: u16, frame_y: u16) {
        let x_start = x_start as u32;
        let frame_y = frame_y as u32;
        let vram_y = vram_y as i32;

        if frame_y >= self.cur_frame.height {
            // Out-of-frame. This should only happen if the video mode changed within the current
            // frame, or for the very last line of the bottom field when we're interlaced.
            return;
        }

        let width = min(self.cur_frame.width, self.display_mode.xres() as u32);

        if self.display_mode.output_24bpp() {
            // GPU is in 24bpp mode, we need to do some bitwise magic to recreate the values
            // correctly

            // X position in the framebuffer, in Byte
            let mut fb_x = (x_start * 2) as i32;

            for x in 0..width {
                // We need two consecutive pixels
                let p_x = fb_x >> 1;
                let p1 = self.read_pixel(p_x, vram_y);
                let p2 = self.read_pixel((p_x + 1) & 0x3ff, vram_y);

                let p1 = p1.to_mbgr1555() as u32;
                let p2 = p2.to_mbgr1555() as u32;

                // Reassemble 32bit word
                let mut p = p1 | (p2 << 16);

                // Realign
                p >>= (fb_x & 1) * 8;
                p &= 0xff_ff_ff;

                // Convert from BGR to RGB
                let mut out = p & 0x00_ff_00;
                out |= p >> 16;
                out |= p << 16;

                self.cur_frame.set_pixel(x, frame_y, out);

                fb_x = (fb_x + 3) & 0x7ff;
            }
        } else {
            // GPU outputs pixels "normally", 15bpp native
            for x in 0..width {
                let p = self.read_pixel((x_start + x) as i32, vram_y);
                self.cur_frame.set_pixel(x, frame_y, p.to_rgb888());
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
            0x03 => self.display_off = (val & 1) != 0,
            // DMA direction
            0x04 => (),
            0x05 => {
                // XXX from mednafen: LSB ignored.
                self.display_vram_x_start = (val & 0x3fe) as u16;
                self.display_vram_y_start = ((val >> 10) & 0x1ff) as u16;
            }
            0x06 => {
                self.display_column_start = (val & 0xfff) as u16;
                self.display_column_end = ((val >> 12) & 0xfff) as u16;
            }
            0x07 => {
                self.display_line_start = (val & 0x3ff) as u16;
                self.display_line_end = ((val >> 10) & 0x3ff) as u16;
            }
            0x08 => self.display_mode.set(val & 0xff_ffff),
            // Get info
            0x10 => (),
            _ => warn!("Unimplemented GP1 {:x}", val),
        }
    }

    /// Creatses a new, blank frame and returns the previous one
    fn new_frame(&mut self) -> Frame {
        let interlaced = self.display_mode.is_true_interlaced();

        let (width, height) = if self.display_full_vram {
            // If we display the full VRAM we don't output one line at a time in the GPU timing
            // emulation code, we can just copy it fully here. It's not like there's any meaningful
            // concept of "accuracy" in this mode anyway.

            // It's possible for this to fail if `display_full_vram` was just activated and the
            // current frame doesn't have the proper dimensions. In this case we're going to return
            // a partially drawn leftover frame which is probably not too important.
            if self.cur_frame.width == 1024 && self.cur_frame.height == 512 {
                for i in 0..(1024 * 512) {
                    self.cur_frame.pixels[i] = self.vram[i].to_rgb888();
                }
            }

            (1024, 512)
        } else {
            // XXX For now we approximate the dimensions of the visible area of the image.
            // For better accuracy we should be emulating the output video timings more accurately
            // but it's probably not worth it for now.

            let width = self.display_mode.xres();

            let mut height = self.display_line_end - self.display_line_start;
            if interlaced {
                height *= 2;
                // Last line of the bottom field isn't drawn
                height -= 1;
            }

            (width as u32, height as u32)
        };

        if width == self.cur_frame.width && height == self.cur_frame.height {
            self.cur_frame.clone()
        } else {
            // Resolution changed, create a whole new frame
            let mut new_frame = Frame::new(width, height);

            ::std::mem::swap(&mut new_frame, &mut self.cur_frame);

            new_frame
        }
    }

    fn send_frame(&mut self) {
        let frame = self.new_frame();

        self.frame_channel.send(frame).unwrap();
    }

    /// Rebuild `dither_tables` based on the various dithering and color depth settings
    fn rebuild_dither_table(&mut self) {
        // When dithering is enabled DITHER_OFFSETS[x % 4][y % 4] is added to the 8bit value before
        // truncation to 5 bits
        const DITHER_OFFSETS: [[i16; 4]; 4] = [
            [-4, 0, -3, 1],
            [2, -2, 3, -1],
            [-3, 1, -4, 0],
            [3, -1, 2, -2],
        ];

        self.dither_enabled = self.dither_enable();

        for x in 0..4 {
            for y in 0..4 {
                for input_value in 0..0x200 {
                    let mut out = input_value as i16;

                    if self.dither_enabled {
                        out += DITHER_OFFSETS[x][y];
                    }

                    // Saturate to 8bits
                    let mut out = if out < 0 {
                        0
                    } else if out > 0xff {
                        0xff
                    } else {
                        out as u8
                    };

                    if !self.draw_24bpp {
                        // 8-to-5 bit truncation. Since we always output 24 bits per pixel we just
                        // replace the LSBs with the MSBs (this ways blacks remain blacks and
                        // whites remain white and we effectively lose 3 significant bits)
                        out &= 0xf8;
                        out |= out >> 5;
                    }

                    self.dither_table[x][y][input_value] = out;
                }
            }
        }
    }

    /// Rebuild dithering tables if we need to activate/deactivate the dithering
    fn maybe_rebuild_dither_table(&mut self) {
        if self.dither_enabled != self.dither_enable() {
            self.rebuild_dither_table();
        }
    }

    /// Returns true if dithering should be active
    fn dither_enable(&self) -> bool {
        self.tex_mapper.draw_mode.dither_enable() && !self.dithering_force_disable
    }

    fn reset(&mut self) {
        self.clip_x_min = 0;
        self.clip_y_min = 0;
        self.clip_x_max = 0;
        self.clip_y_max = 0;
        self.draw_offset_x = 0;
        self.draw_offset_y = 0;
        self.draw_offset_x = 0;
        self.draw_offset_y = 0;
        self.tex_mapper.reset();
        self.mask_settings.set(0);
        self.display_line_start = 0x10;
        self.display_line_end = 0x100;
        self.display_column_start = 0x200;
        self.display_column_end = 0xc00;
        self.display_mode.set(0);
        self.display_vram_x_start = 0;
        self.display_vram_y_start = 0;

        self.maybe_rebuild_dither_table();
    }

    fn read_pixel(&self, x: i32, y: i32) -> Pixel {
        debug_assert!(x >= 0 && x < 1024, "x out of bounds ({})", x);
        debug_assert!(y >= 0 && y < 1024, "y out of bounds ({})", y);

        let y = y & 0x1ff;
        let vram_off = y * 1024 + x;

        self.vram[vram_off as usize]
    }

    fn draw_pixel<Transparency>(&mut self, x: i32, y: i32, mut color: Pixel)
    where
        Transparency: TransparencyMode,
    {
        debug_assert!(x >= 0 && x < 1024, "x out of bounds ({})", x);
        debug_assert!(y >= 0 && y < 1024, "y out of bounds ({})", y);

        // Apparently the PlayStation GPU supports 2MB VRAM (1024x1024, used in some arcade
        // machines apparently) but the bottom half isn't installed so it wraps around.
        let y = y & 0x1ff;

        let vram_off = y * 1024 + x;

        let vram_pixel = &mut self.vram[vram_off as usize];

        // If the draw command is semi-transparent and the mask bit is set, this is a transparent
        // pixel
        let is_transparent = Transparency::is_transparent() && color.mask();

        // XXX implement masking

        if is_transparent {
            let bg_pixel = *vram_pixel;
            let mode = self.tex_mapper.draw_mode.transparency_mode();

            // XXX if we wanted to be extra-accurate we might want to truncate the color here to
            // get accurate result in 15bpp. It's unlikely to make a significant difference
            // however.
            color.apply_transparency(bg_pixel, mode);
        } else if self.force_transparency {
            let bg_pixel = *vram_pixel;

            color.apply_transparency(bg_pixel, TransparencyFunction::Average);
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

        if x_max < self.clip_x_min || x_min >= self.clip_x_max {
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
                    self.rasterize_scanline::<Transparency, Texture, Shading>(
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
                    self.rasterize_scanline::<Transparency, Texture, Shading>(
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

    /// Rasterize one line from a triangle
    fn rasterize_scanline<Transparency, Texture, Shading>(
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
        let end_x = min(right_x, self.clip_x_max + 1);

        if !self.can_draw_to_line(y) {
            return;
        }

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
                        // No need to worry about truncation here since textures are always 555
                        // anyway
                        self.draw_pixel::<Transparency>(x, y, texel);
                    } else {
                        // Texture blending: the final color is a combination of the texel and
                        // the computed gouraud color
                        let blend = self.blend_and_dither(x, y, texel, vars.color());
                        self.draw_pixel::<Transparency>(x, y, blend);
                    }
                }
            } else {
                // No texture
                let (mut r, mut g, mut b) = vars.color_components();

                if Shading::is_shaded() {
                    r = self.dither(x, y, r as u32);
                    g = self.dither(x, y, g as u32);
                    b = self.dither(x, y, b as u32);
                }

                let mut color = Pixel::from_rgb(r, g, b);

                // We have to set the mask bit since it's used to test if the pixel should be
                // transparent, and non-textured transparent draw calls are always fully
                // transparent.
                if Transparency::is_transparent() {
                    color.set_mask();
                }

                self.draw_pixel::<Transparency>(x, y, color);
            }
            vars.translate_right::<Texture, Shading>(deltas);
        }
    }

    fn get_texel(&self, u: u8, v: u8) -> Pixel {
        self.tex_mapper.get_texel(u, v, &self.vram)
    }

    fn draw_rect<Transparency, Texture>(&mut self, origin: Vertex, width: i32, height: i32)
    where
        Transparency: TransparencyMode,
        Texture: TextureMode,
    {
        let mut u_start = origin.u;
        let mut v = origin.v;

        let (u_inc, v_inc) = if Texture::is_textured() {
            // Per-No$ these bits aren't supposed to function in early PSX models. If that's true
            // they probably aren't used in many games.
            let flip_x = self.tex_mapper.draw_mode.flip_rect_x();
            let flip_y = self.tex_mapper.draw_mode.flip_rect_y();

            let u_inc = if flip_x {
                // XXX Taken from Mednafen, not sure what this does
                u_start |= 1;
                -1
            } else {
                1
            };

            let v_inc = if flip_y { -1 } else { 1 };

            (u_inc, v_inc)
        } else {
            (0, 0)
        };

        let mut x_start = origin.x();
        let x_end = min(x_start + width, self.clip_x_max - 1);

        let mut y_start = origin.y();
        let y_end = min(y_start + height, self.clip_y_max - 1);

        if x_start < self.clip_x_min {
            if Texture::is_textured() {
                let skip = (self.clip_x_min - x_start) * u_inc;

                u_start = u_start.wrapping_add(skip as u8);
            }
            x_start = self.clip_x_min;
        }

        if y_start < self.clip_y_min {
            if Texture::is_textured() {
                let skip = (self.clip_y_min - y_start) * u_inc;

                v = v.wrapping_add(skip as u8);
            }
            y_start = self.clip_y_min;
        }

        if x_end <= x_start || y_end <= y_start {
            // Rect is 0-width or completely clipped
            return;
        }

        let mut color = origin.color;

        if !Texture::is_textured() {
            // We're only going to copy this color everywhere, let's truncate it here once and for
            // all
            color = self.truncate_color(color);
        }

        if Transparency::is_transparent() {
            color.set_mask();
        }

        for y in y_start..y_end {
            if !self.can_draw_to_line(y) {
                v = v.wrapping_add(v_inc as u8);
                continue;
            }

            let mut u = u_start;
            for x in x_start..x_end {
                if Texture::is_textured() {
                    let texel = self.get_texel(u, v);
                    // If the pixel is equal to 0 (including mask bit) then we don't draw it
                    if !texel.is_nul() {
                        if Texture::is_raw_texture() {
                            self.draw_pixel::<Transparency>(x, y, texel);
                        } else {
                            // Texture blending: the final color is a combination of the texel and
                            // the solid color. Rect are never dithered.
                            let blend = self.blend(texel, origin.color);
                            self.draw_pixel::<Transparency>(x, y, blend);
                        }
                    }
                } else {
                    // No texture
                    self.draw_pixel::<Transparency>(x, y, color);
                }
                u = u.wrapping_add(u_inc as u8);
            }

            v = v.wrapping_add(v_inc as u8);
        }
    }

    fn blend(&self, texel: Pixel, color: Pixel) -> Pixel {
        // If you look at DITHER_OFFSETS when we build the table you can see that
        // DITHER_OFFSETS[0][1] is equal to 0, therefore even if dithering is enabled this won't
        // actually modify the value of the pixel beyond normal saturation and truncation.
        self.blend_and_dither(0, 1, texel, color)
    }

    /// Perform texture blending and dithering
    fn blend_and_dither(&self, x: i32, y: i32, texel: Pixel, color: Pixel) -> Pixel {
        let t_r = texel.red() as u32;
        let t_g = texel.green() as u32;
        let t_b = texel.blue() as u32;

        let c_r = color.red() as u32;
        let c_g = color.green() as u32;
        let c_b = color.blue() as u32;

        // In order to normalize the value we should be shifting by 8, but texture blending
        // actually doubles the value, hence the - 1.
        let mut r = (t_r * c_r) >> (8 - 1);
        let mut g = (t_g * c_g) >> (8 - 1);
        let mut b = (t_b * c_b) >> (8 - 1);

        // Perform dithering, saturation and 8-to-5 conversion (if enabled)
        r = self.dither(x, y, r) as u32;
        g = self.dither(x, y, g) as u32;
        b = self.dither(x, y, b) as u32;

        let mask = texel.0 & 0xff00_0000;

        Pixel(mask | b | (g << 8) | (r << 16))
    }

    fn dither(&self, x: i32, y: i32, input: u32) -> u8 {
        let x = (x & 3) as usize;
        let y = (y & 3) as usize;
        let input = input as usize;

        self.dither_table[x][y][input]
    }

    /// Apply 8-to-5bit truncation if enabled
    fn truncate_color(&self, color: Pixel) -> Pixel {
        let r = color.red();
        let g = color.green();
        let b = color.blue();
        let mask = color.0 & 0xff00_0000;

        let r = self.truncate_component(r) as u32;
        let g = self.truncate_component(g) as u32;
        let b = self.truncate_component(b) as u32;

        Pixel(mask | b | (g << 8) | (r << 16))
    }

    fn truncate_component(&self, c: u8) -> u8 {
        // If you look at DITHER_OFFSETS when we build the table you can see that
        // DITHER_OFFSETS[0][1] is equal to 0, therefore even if dithering is enabled this won't
        // actually modify the value of the pixel beyond normal saturation and truncation.
        //
        // If draw_24bpp is true this is a nop since the entry in the table will be the same value
        // as the index in the table
        self.dither_table[0][1][c as usize]
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
        let (r, g, b) = self.color_components();

        Pixel::from_rgb(r, g, b)
    }

    fn color_components(&self) -> (u8, u8, u8) {
        let r = self.red.truncate() as u8;
        let b = self.blue.truncate() as u8;
        let g = self.green.truncate() as u8;

        (r, g, b)
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
                let s_r = b_r.wrapping_sub(f_r);
                let s_g = b_g.wrapping_sub(f_g);
                let s_b = b_b.wrapping_sub(f_b);

                o_r = if s_r > 0xff { 0 } else { s_r };
                o_g = if s_g > 0xff { 0 } else { s_g };
                o_b = if s_b > 0xff { 0 } else { s_b };
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

fn cmd_handle_rect<Transparency, Texture>(
    rasterizer: &mut Rasterizer,
    params: &[u32],
    dimensions: Option<(i32, i32)>,
) where
    Transparency: TransparencyMode,
    Texture: TextureMode,
{
    let mut origin = Vertex::new(0);
    let mut index = 0;

    origin.color = Pixel::from_command(params[index]);
    index += 1;

    origin.set_position(params[index]);
    index += 1;

    // Add the draw offset
    origin.position.x += rasterizer.draw_offset_x;
    origin.position.y += rasterizer.draw_offset_y;

    if Texture::is_textured() {
        rasterizer.tex_mapper.set_clut(params[index]);
        origin.set_texture_uv(params[index]);
        index += 1;
    }

    let (w, h) = match dimensions {
        Some(d) => d,
        None => {
            // Variable dimensions
            let dim = params[index];

            let w = dim & 0x3ff;
            let h = (dim >> 16) & 0x1ff;

            (w as i32, h as i32)
        }
    };

    rasterizer.draw_rect::<Transparency, Texture>(origin, w, h);
}

fn cmd_handle_rect_variable<Transparency, Texture>(rasterizer: &mut Rasterizer, params: &[u32])
where
    Transparency: TransparencyMode,
    Texture: TextureMode,
{
    cmd_handle_rect::<Transparency, Texture>(rasterizer, params, None);
}

fn cmd_handle_rect_1x1<Transparency, Texture>(rasterizer: &mut Rasterizer, params: &[u32])
where
    Transparency: TransparencyMode,
    Texture: TextureMode,
{
    cmd_handle_rect::<Transparency, Texture>(rasterizer, params, Some((1, 1)));
}

fn cmd_handle_rect_8x8<Transparency, Texture>(rasterizer: &mut Rasterizer, params: &[u32])
where
    Transparency: TransparencyMode,
    Texture: TextureMode,
{
    cmd_handle_rect::<Transparency, Texture>(rasterizer, params, Some((8, 8)));
}

fn cmd_handle_rect_16x16<Transparency, Texture>(rasterizer: &mut Rasterizer, params: &[u32])
where
    Transparency: TransparencyMode,
    Texture: TextureMode,
{
    cmd_handle_rect::<Transparency, Texture>(rasterizer, params, Some((16, 16)));
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

fn cmd_vram_copy(rasterizer: &mut Rasterizer, params: &[u32]) {
    let src = params[1];
    let dst = params[2];
    let dim = params[3];

    let src_x = (src & 0x3ff) as i32;
    let src_y = ((src >> 16) & 0x3ff) as i32;
    let dst_x = (dst & 0x3ff) as i32;
    let dst_y = ((dst >> 16) & 0x3ff) as i32;

    let (width, height) = vram_access_dimensions(dim);

    // XXX Mednafen uses a temporary buffer of 128 pixels here, presumably to emulate artifacts
    // when the source and destination zones overlap? Needs more testing.
    for y in 0..height {
        let sy = (y + src_y) % 0x1ff;
        let dy = (y + dst_y) % 0x1ff;
        for x in 0..width {
            let sx = (x + src_x) % 0x3ff;
            let dx = (x + dst_x) % 0x3ff;

            let p = rasterizer.read_pixel(sx, sy);
            // VRAM copy respects mask bit settings
            rasterizer.draw_pixel::<Opaque>(dx, dy, p);
        }
    }
}

fn cmd_vram_store(rasterizer: &mut Rasterizer, params: &[u32]) {
    let pos = params[1];
    let dim = params[2];

    let left = (pos & 0x3ff) as u16;
    let top = ((pos >> 16) & 0x3ff) as u16;

    let (width, height) = vram_access_dimensions(dim);

    let store = VRamStore::new(left, top, width as u16, height as u16);

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

    // XXX Pretty sure there's no dithering for this commands
    let color = rasterizer.truncate_color(color);

    for y in 0..height {
        let y_pos = (start_y + y) & 511;

        if !rasterizer.can_draw_to_line(y_pos as i32) {
            continue;
        }

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
}

fn cmd_draw_mode(rasterizer: &mut Rasterizer, params: &[u32]) {
    let mode = params[0];

    rasterizer.tex_mapper.set_draw_mode(mode);
    rasterizer.maybe_rebuild_dither_table();
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
        handler: cmd_handle_poly_tri::<Transparent, TextureBlending, Shaded>,
        len: 9,
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
        handler: cmd_handle_poly_quad::<Transparent, NoTexture, Shaded>,
        len: 8,
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
        handler: cmd_handle_rect_variable::<Opaque, NoTexture>,
        len: 3,
    },
    CommandHandler {
        handler: cmd_handle_rect_variable::<Opaque, NoTexture>,
        len: 3,
    },
    CommandHandler {
        handler: cmd_handle_rect_variable::<Transparent, NoTexture>,
        len: 3,
    },
    CommandHandler {
        handler: cmd_handle_rect_variable::<Transparent, NoTexture>,
        len: 3,
    },
    CommandHandler {
        handler: cmd_handle_rect_variable::<Opaque, TextureBlending>,
        len: 4,
    },
    CommandHandler {
        handler: cmd_handle_rect_variable::<Opaque, TextureRaw>,
        len: 4,
    },
    CommandHandler {
        handler: cmd_handle_rect_variable::<Transparent, TextureBlending>,
        len: 4,
    },
    CommandHandler {
        handler: cmd_handle_rect_variable::<Transparent, TextureRaw>,
        len: 4,
    },
    CommandHandler {
        handler: cmd_handle_rect_1x1::<Opaque, NoTexture>,
        len: 2,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_handle_rect_1x1::<Transparent, NoTexture>,
        len: 2,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_handle_rect_1x1::<Opaque, TextureBlending>,
        len: 3,
    },
    CommandHandler {
        handler: cmd_handle_rect_1x1::<Opaque, TextureRaw>,
        len: 3,
    },
    CommandHandler {
        handler: cmd_handle_rect_1x1::<Transparent, TextureBlending>,
        len: 3,
    },
    CommandHandler {
        handler: cmd_handle_rect_1x1::<Transparent, TextureRaw>,
        len: 3,
    },
    // 0x70
    CommandHandler {
        handler: cmd_handle_rect_8x8::<Opaque, NoTexture>,
        len: 2,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_handle_rect_8x8::<Transparent, NoTexture>,
        len: 2,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_handle_rect_8x8::<Opaque, TextureBlending>,
        len: 3,
    },
    CommandHandler {
        handler: cmd_handle_rect_8x8::<Opaque, TextureRaw>,
        len: 3,
    },
    CommandHandler {
        handler: cmd_handle_rect_8x8::<Transparent, TextureBlending>,
        len: 3,
    },
    CommandHandler {
        handler: cmd_handle_rect_8x8::<Transparent, TextureRaw>,
        len: 3,
    },
    CommandHandler {
        handler: cmd_handle_rect_16x16::<Opaque, NoTexture>,
        len: 2,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_handle_rect_16x16::<Transparent, NoTexture>,
        len: 2,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_handle_rect_16x16::<Opaque, TextureBlending>,
        len: 3,
    },
    CommandHandler {
        handler: cmd_handle_rect_16x16::<Opaque, TextureRaw>,
        len: 3,
    },
    CommandHandler {
        handler: cmd_handle_rect_16x16::<Transparent, TextureBlending>,
        len: 3,
    },
    CommandHandler {
        handler: cmd_handle_rect_16x16::<Transparent, TextureRaw>,
        len: 3,
    },
    // 0x80
    CommandHandler {
        handler: cmd_vram_copy,
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
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
    },
    CommandHandler {
        handler: cmd_unimplemented,
        len: 1,
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
