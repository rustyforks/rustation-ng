//! Implementation of the various GP0 commands.

use super::{CycleCount, Psx, State};

/// Description of the various GP0 commands
pub struct Command {
    /// Callback function to actually perform the command. If the command is a draw command it only
    /// takes care of the timings and maintaining the façade, the drawing takes place in an other
    /// thread.
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

trait TransparencyMode {
    fn is_transparent() -> bool;
}

struct Transparent;

impl TransparencyMode for Transparent {
    fn is_transparent() -> bool {
        true
    }
}

struct Opaque;

impl TransparencyMode for Opaque {
    fn is_transparent() -> bool {
        false
    }
}

trait TextureMode {
    fn is_textured() -> bool;
    fn is_raw_texture() -> bool;
}

struct NoTexture;

impl TextureMode for NoTexture {
    fn is_textured() -> bool {
        false
    }

    fn is_raw_texture() -> bool {
        false
    }
}

trait ShadingMode {
    fn is_shaded() -> bool;
}

struct NoShading;

impl ShadingMode for NoShading {
    fn is_shaded() -> bool {
        false
    }
}

/// A vertex's coordinates
#[derive(Debug, Copy, Clone)]
struct Position {
    x: i32,
    y: i32,
}

impl Position {
    fn new(x: i32, y: i32) -> Position {
        Position { x, y }
    }

    fn from_command(c: u32) -> Position {
        let x = c;
        let y = c >> 16;

        // XXX The coordinates each take 16bits and are signed, however mednafen extends the sign
        // starting on bit 11, not 15.
        let x = extend_to_i32(x, 11);
        let y = extend_to_i32(y, 11);
        Position::new(x, y)
    }
}

/// Extend a signed value on `n` bit to an i32
fn extend_to_i32(val: u32, n: usize) -> i32 {
    let shift = 32 - n;

    ((val << shift) as i32) >> shift
}

/// Compute the cross-product of (AB) x (AC)
fn cross_product(a: Position, b: Position, c: Position) -> i32 {
    (b.x - a.x) * (c.y - a.y) - (c.x - a.x) * (b.y - a.y)
}

/// Triangles taller than this value are rejected by the PSX GPU
const TRIANGLE_MAX_HEIGHT: i32 = 511;
/// Triangles wider than this value are rejected by the PSX GPU
const TRIANGLE_MAX_WIDTH: i32 = 1023;

/// Computes triangle-drawing timings
fn triangle_draw_time<Transparency, Texture, Shading>(
    psx: &mut Psx,
    mut coords: [Position; 3],
) -> CycleCount
where
    Transparency: TransparencyMode,
    Texture: TextureMode,
    Shading: ShadingMode,
{
    let mut draw_time = 0;

    // First compute the constant draw time for this command
    draw_time += if Shading::is_shaded() {
        if Texture::is_textured() {
            // Shaded and textured
            150 * 3
        } else {
            // Shaded
            96 * 3
        }
    } else if Texture::is_textured() {
        // Textured
        60 * 3
    } else {
        0
    };

    // Order coords by x coordinates
    coords.sort_by(|a, b| a.x.cmp(&b.x));

    let min_x = coords[0].x;
    let max_x = coords[2].x;

    // This is the triangle's width on screen, in pixels (not accounting for any clipping). This is
    // *not* the geometric base width of the triangle.
    let triangle_width = max_x - min_x;

    if triangle_width == 0 || triangle_width > TRIANGLE_MAX_WIDTH {
        return draw_time;
    }

    // Order coords by y coordinates
    coords.sort_by(|a, b| a.y.cmp(&b.y));

    let min_y = coords[0].y;
    let max_y = coords[2].y;

    // This is the triangle's height on screen, in pixels (not accounting for any clipping). This
    // is *not* the geometric height of the triangle.
    let triangle_height = max_y - min_y;

    if triangle_height == 0 || triangle_height > TRIANGLE_MAX_HEIGHT {
        return draw_time;
    }

    // Compute the cross product
    let cross = cross_product(coords[0], coords[1], coords[2]);

    // If the cross-product is 0 it means that the 3 vertices lie on a line and the triangle is
    // completely flat, so we have nothing to draw.
    if cross == 0 {
        return draw_time;
    }

    // The area of the triangle is half the magnitude of the cross-product.
    let triangle_area = cross.abs() / 2;

    // True if the triangle clips with at least one side of the drawing area
    let clip = min_x < psx.gpu.clip_x_min
        || max_x > psx.gpu.clip_x_max
        || min_y < psx.gpu.clip_y_min
        || max_y > psx.gpu.clip_y_max;

    let npixels = if clip {
        // If the triangle clips the draw area we can't use its area to guess the number of pixels
        // for this polygon since parts of it won't be drawn. Figuring out the exact area of the
        // displayed part quickly is not trivial (or at least, I can't think of a trivial way).
        // There are many possible scenarios to consider.
        //
        // For the time being I'm going to use a very inaccurate first approach: I consider that
        // we can approximate the ratio of the total area of the triangle over the non-clipped area
        // by using the ratio of the total area of the triangle's bounding box over the clipped
        // bounding box.
        //
        // This is of course very inaccurate in the general case, however it does have the
        // interesting property that it gives the correct result for rectangular quads (since
        // obviously in this case the bounding box of the quad *is* the quad itself). Overall I
        // hope that the proportion of clipped triangles will be low enough to make the error in
        // the estimation negligible.
        let bb_total_width = triangle_width;
        let bb_total_height = triangle_height;

        let mut bb_clip_width = bb_total_width;
        let mut bb_clip_height = bb_total_height;

        // Clip the bounding box
        if min_x < psx.gpu.clip_x_min {
            bb_clip_width -= psx.gpu.clip_x_min - min_x;
        }
        if max_x > psx.gpu.clip_x_max {
            bb_clip_width -= max_x - psx.gpu.clip_x_max;
        }
        if min_y < psx.gpu.clip_y_min {
            bb_clip_height -= psx.gpu.clip_y_min - min_y;
        }
        if max_y > psx.gpu.clip_y_max {
            bb_clip_height -= max_y - psx.gpu.clip_y_max;
        }

        if bb_clip_width <= 0 || bb_clip_height <= 0 {
            // The bounding box is entirely outside of the clip area, the pixel won't be drawn at
            // all
            //
            // XXX Is it really rejected instantly or does the GPU still iterate over every line,
            // drawing nothing?
            return draw_time;
        }

        // Use 64bits to avoid overflows
        let bb_total_area = i64::from(bb_total_width * bb_total_height);
        let bb_clip_area = i64::from(bb_clip_width * bb_clip_height);

        let clip_area_approx = (i64::from(triangle_area) * bb_clip_area) / bb_total_area;

        clip_area_approx as i32
    } else {
        // If the triangle doesn't clip the draw area we can consider that, typically, the
        // number of pixels is roughly the area of the triangle. It's not a perfect match
        // because a rasterized triangle will have at least one "ragged" edge with bits of
        // pixels poking out or in. We can hope that the discrepancies will even out over the
        // course of a frame.
        //
        // In particular for any quad in the shape of a parallelogram the symmetry of the two
        // contained triangles should make that the sum of both areas should be very close to
        // the sum of the pixels, with one half being under-evaluated and the other
        // over-evaluated.
        triangle_area
    };

    draw_time += if Texture::is_textured() || Shading::is_shaded() {
        // For texture and shading we have a base cost of about 2 cycles per pixel (like for
        // shading) *but* we also have the additional costs of the texture fetch which depends on
        // the GPU cache state.
        //
        // XXX Can we estimate the texture cache performance without emulating every pixel draw?
        // Maybe some heuristic keeping track of which texture has been used last to see if cache
        // hits are likely?
        npixels * 2
    } else if Transparency::is_transparent() || psx.gpu.mask_settings.check_mask_bit() {
        // If we need to read the VRAM for transparency or mask checking we use about 1.5 cycles
        // per pixel
        (npixels * 3) / 2
    } else {
        // For plain solid triangles we have about 1 cycle per pixel
        npixels
    };

    debug!(
        "Draw triangle {:?}: npixels: {}, draw_time: {}",
        coords, npixels, draw_time
    );

    draw_time
}

fn cmd_handle_poly_quad<Transparency, Texture, Shading>(psx: &mut Psx)
where
    Transparency: TransparencyMode,
    Texture: TextureMode,
    Shading: ShadingMode,
{
    // Quads are effectively just an optimization to reduce the length of the draw command,
    // internally they're just drawn as two triangles.

    let mut coords = [
        Position::new(0, 0),
        Position::new(0, 0),
        Position::new(0, 0),
        Position::new(0, 0),
    ];

    // Load the triangle coordinates. Since we only care about timings here we don't need to load
    // the texture coordinates or anything.
    //
    // XXX For now I read all for vertices at once to make the code simpler but I'm fairly sure
    // that it's inaccurate, the GPU probably only reads the last vertex once the first triangle
    // has been drawn.
    for (v, coord) in coords.iter_mut().enumerate() {
        if v == 0 || Shading::is_shaded() {
            // Pop the shading color (also takes care of the command word if we're not shaded)
            psx.gpu.command_fifo.pop();
        }

        let cmd = psx.gpu.command_fifo.pop();
        *coord = Position::from_command(cmd);

        // Add the draw offset
        coord.x += psx.gpu.draw_offset_x;
        coord.y += psx.gpu.draw_offset_y;

        if Texture::is_textured() {
            // Pop texture coordinates
            psx.gpu.command_fifo.pop();
        }
    }

    let triangle_1 = [coords[0], coords[1], coords[2]];
    let triangle_2 = [coords[1], coords[2], coords[3]];

    // Compute the time needed to draw the first triangle
    let draw_time = triangle_draw_time::<Transparency, Texture, Shading>(psx, triangle_1);
    psx.gpu.draw_time(64 + 18 + draw_time);

    // Compute the time needed to draw the second triangle and save it for later
    let draw_time = triangle_draw_time::<Transparency, Texture, Shading>(psx, triangle_2);

    psx.gpu.state = State::InQuad(draw_time);
}

/// Parses the command FIFO for a VRAM store or load and returns the number of words about to be
/// read/written
fn vram_access_length_words(psx: &mut Psx) -> u32 {
    // Pop command
    psx.gpu.command_fifo.pop();
    // Position in VRAM
    psx.gpu.command_fifo.pop();
    // Dimensions
    let dim = psx.gpu.command_fifo.pop();

    // Width is in GPU pixels, i.e. 16bits per pixel
    let mut width = dim & 0x3ff;
    let mut height = (dim >> 16) & 0x1ff;

    // XXX recheck this, a comment in mednafen says that the results for VRAM load are inconsistent
    if width == 0 {
        width = 1024;
    }

    if height == 0 {
        height = 512;
    }

    // Total number of words to complete the transfer. Since every pixel is 16bit and we transfer
    // 32bits at a time we need to round up
    (width * height + 1) / 2
}

fn cmd_vram_store(psx: &mut Psx) {
    let nwords = vram_access_length_words(psx);
    psx.gpu.state = State::VRamStore(nwords);
}

fn cmd_vram_load(psx: &mut Psx) {
    let nwords = vram_access_length_words(psx);
    psx.gpu.state = State::VRamLoad(nwords);
}

fn cmd_draw_mode(psx: &mut Psx) {
    let mode = psx.gpu.command_fifo.pop();

    psx.gpu.draw_mode.set(mode);
}

fn cmd_tex_window(psx: &mut Psx) {
    psx.gpu.tex_window = psx.gpu.command_fifo.pop() & 0xf_ffff;
}

fn cmd_clip_top_left(psx: &mut Psx) {
    let clip = psx.gpu.command_fifo.pop();

    psx.gpu.clip_x_min = (clip & 0x3ff) as i32;
    psx.gpu.clip_y_min = ((clip >> 10) & 0x3ff) as i32;
}

fn cmd_clip_bot_right(psx: &mut Psx) {
    let clip = psx.gpu.command_fifo.pop();

    psx.gpu.clip_x_max = (clip & 0x3ff) as i32;
    psx.gpu.clip_y_max = ((clip >> 10) & 0x3ff) as i32;
}

fn cmd_draw_offset(psx: &mut Psx) {
    let off = psx.gpu.command_fifo.pop();

    let off_x = off & 0x7ff;
    let off_y = (off >> 11) & 0x7ff;

    // Sign-extend
    psx.gpu.draw_offset_x = extend_to_i32(off_x, 11);
    psx.gpu.draw_offset_y = extend_to_i32(off_y, 11);
}

fn cmd_mask_settings(psx: &mut Psx) {
    let mask_settings = psx.gpu.command_fifo.pop() & 0x3f_ffff;

    psx.gpu.mask_settings.set(mask_settings)
}

fn cmd_clear_cache(psx: &mut Psx) {
    // XXX for now we don't implement the GPU cache timings
    psx.gpu.command_fifo.pop();
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
pub static GP0_COMMANDS: [Command; 0x100] = [
    // 0x00
    Command {
        handler: cmd_nop,
        len: 1,
        fifo_len: 1,
        out_of_band: true,
    },
    Command {
        handler: cmd_clear_cache,
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
        handler: cmd_handle_poly_quad::<Opaque, NoTexture, NoShading>,
        len: 5,
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
        handler: cmd_handle_poly_quad::<Transparent, NoTexture, NoShading>,
        len: 5,
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
        handler: cmd_vram_store,
        len: 3,
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
        handler: cmd_vram_load,
        len: 3,
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

#[test]
fn check_poly_callbacks() {
    use crate::psx::{bios, gpu, Psx};

    let dummy_bios = bios::Bios::new_dummy();
    let mut dummy_psx = Psx::new_with_bios(dummy_bios, gpu::VideoStandard::Pal);

    for op in 0x20..=0x3f {
        let cmd = &GP0_COMMANDS[op];

        if cmd.handler as *const fn() == cmd_unimplemented as *const fn() {
            // Not yet implemented
            continue;
        }

        let is_textured = op & 4 != 0;
        let is_shaded = op & 0x10 != 0;
        let is_quad = op & 8 != 0;

        let num_vertex = if is_quad { 4 } else { 3 };
        let vertex_len = 1 + is_textured as u8 + is_shaded as u8;

        let total_len = num_vertex * vertex_len + (!is_shaded) as u8;

        assert_eq!(cmd.len, total_len);
        assert_eq!(cmd.out_of_band, false);
        assert_eq!(cmd.fifo_len, 1);

        dummy_psx.gpu.reset();

        // Fill the FIFO with a dummy command to make sure the handler consumes the right number of
        // words
        for _ in 0..total_len {
            dummy_psx.gpu.command_fifo.push((op << 24) as u32);
        }

        (cmd.handler)(&mut dummy_psx);

        assert_eq!(dummy_psx.gpu.command_fifo.len(), 0);
    }
}
