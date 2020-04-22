mod fifo;

use super::{AccessWidth, Addressable, CycleCount, Psx};
use std::ops::{Index, IndexMut};

/// Motion Decoder (sometimes called macroblock or movie decoder).
pub struct MDec {
    /// Current state of the decoder
    state: State,
    /// Last received command
    command: Command,
    /// Input FIFO
    input_fifo: fifo::CommandFifo,
    /// Output FIFO
    output_fifo: fifo::OutputFifo,
    /// Remaining words expected for this command
    command_remaining: u16,
    dma_in_enabled: bool,
    dma_out_enabled: bool,
    /// Quantization matrices: 8x8 bytes, first one is for luma, 2nd is for chroma.
    quant_matrices: [[u8; 64]; 2],
    idct_matrix: IdctMatrix,
    current_block: BlockType,
    /// Index in the current macroblock
    block_index: u8,
    /// Quantization factor for the current macroblock, used for the AC values only.
    qscale: u8,
    /// 16bit coefficients during macroblock decoding
    block_coeffs: MacroblockCoeffs,
    /// Buffer for the currently decoded luma macroblock.
    block_y: Macroblock,
    /// Buffer for the currently decoded Cb macroblock.
    block_u: Macroblock,
    /// Buffer for the currently decoded Cr macroblock.
    block_v: Macroblock,
    /// DMA block line length in 32bit words
    dma_block_line_length: u8,
    /// Block line offset for DMA read
    dma_block_line: u8,
    /// Block column offset for DMA read
    dma_block_column: u8,
}

impl MDec {
    pub fn new() -> MDec {
        MDec {
            state: State::Idle,
            command: Command(0),
            input_fifo: fifo::CommandFifo::new(),
            output_fifo: fifo::OutputFifo::new(),
            command_remaining: 0,
            dma_in_enabled: false,
            dma_out_enabled: false,
            quant_matrices: [[0; 64]; 2],
            idct_matrix: IdctMatrix::new(),
            current_block: BlockType::CrMono,
            block_index: 0,
            qscale: 0,
            block_coeffs: MacroblockCoeffs::new(),
            block_y: Macroblock::new(),
            block_u: Macroblock::new(),
            block_v: Macroblock::new(),
            dma_block_line_length: 0,
            dma_block_line: 0,
            dma_block_column: 0,
        }
    }

    pub fn run(&mut self, cycles: CycleCount) {
        // XXX implement MDEC timings
        let _ = cycles;

        while !self.input_fifo.is_empty() {
            match self.state {
                State::Idle => {
                    let next_word = self.input_fifo.pop();

                    self.command = Command(next_word);

                    match self.command.opcode() {
                        1 => {
                            // Decode macroblock
                            self.output_fifo.clear();
                            self.current_block = BlockType::CrMono;
                            self.block_index = 0;
                            self.command_remaining = self.command.block_len();
                            self.state = State::Decoding;

                            self.dma_block_line_length = match self.command.output_depth() {
                                OutputDepth::D4 | OutputDepth::D8 => 0,
                                OutputDepth::D15 => 4,
                                OutputDepth::D24 => 6,
                            };
                            self.dma_block_line = 0;
                            self.dma_block_column = self.dma_block_line_length;
                        }
                        2 => {
                            // Load quantization matrices
                            self.command_remaining =
                                if self.command.quant_color() { 32 } else { 16 };

                            self.state = State::LoadQuantTable(0);
                        }
                        3 => {
                            self.command_remaining = 32;
                            self.state = State::LoadIdctMatrix;
                        }
                        o => unimplemented!("Command {:x}", o),
                    }
                }
                State::LoadQuantTable(index) => {
                    let next_word = self.input_fifo.pop();

                    let matrix = (index >> 4) as usize;
                    let pos = ((index & 0xf) << 2) as usize;

                    for i in 0..4 {
                        let b = (next_word >> (i * 8)) as u8;

                        self.quant_matrices[matrix][pos + i] = b;
                    }

                    self.command_remaining = self.command_remaining.wrapping_sub(1);
                    self.state = if self.command_remaining == 0 {
                        State::Idle
                    } else {
                        State::LoadQuantTable(index + 1)
                    };
                }
                State::LoadIdctMatrix => {
                    let next_word = self.input_fifo.pop();

                    let index = (32 - self.command_remaining) as u8;

                    let index = index * 2;

                    let c1 = next_word as i16;
                    let c2 = (next_word >> 16) as i16;

                    // XXX The loss of precision in the bitshift looks suspicious to me but that's
                    // what mednafen does. Probably worth investigating on the real hardware.
                    self.idct_matrix.set(index, c1 >> 3);
                    self.idct_matrix.set(index + 1, c2 >> 3);

                    self.command_remaining = self.command_remaining.wrapping_sub(1);
                    if self.command_remaining == 0 {
                        self.state = State::Idle;
                    }
                }
                State::Decoding => {
                    if self.output_fifo_full() {
                        // Output buffer has too much data to decode an other macroblock, retry
                        // later
                        break;
                    }

                    let next_word = self.input_fifo.pop();

                    self.decode_rle(next_word as u16);
                    self.decode_rle((next_word >> 16) as u16);

                    self.command_remaining = self.command_remaining.wrapping_sub(1);
                    if self.command_remaining == 0 {
                        self.state = State::Idle;
                    }
                }
            }
        }
    }

    pub fn push_command(&mut self, cmd: u32) {
        if self.input_fifo.is_full() {
            unimplemented!("Input FIFO overflow");
        }

        self.input_fifo.push(cmd);
    }

    pub fn set_control(&mut self, control: u32) {
        if control >> 31 != 0 {
            // Reset
            self.input_fifo.clear();
            self.output_fifo.clear();
            self.state = State::Idle;
            self.command_remaining = 0;
            self.block_index = 0;
        }

        self.dma_out_enabled = (control & (1 << 29)) != 0;
        self.dma_in_enabled = (control & (1 << 30)) != 0;
    }

    pub fn status(&self) -> u32 {
        let mut r = 0;

        // Bits [15:0] contain the number of remaining parameter words minus 1, or 0xffff if no
        // parameter is expected.
        r |= self.command_remaining.wrapping_sub(1) as u32;

        // TODO [16:18] Current block

        r |= self.command.output_format() << 23;

        r |= (self.dma_can_read() as u32) << 27;
        r |= (self.dma_can_write() as u32) << 28;

        r |= (self.state.is_busy() as u32) << 29;
        r |= (!self.input_fifo.is_full() as u32) << 30;
        r |= (!self.output_fifo.is_empty() as u32) << 31;

        r
    }

    fn output_fifo_full(&self) -> bool {
        // This returns true if the emulated FIFO is full. Since to make things simple and always
        // decode one block at a time I decided to use a larger emulated FIFO (512 bytes instead of 128)
        self.output_fifo.len() >= 128
    }

    pub fn dma_can_write(&self) -> bool {
        // XXX From Mednafen, does it really only work if the FIFO is empty?
        self.input_fifo.is_empty()
            && self.dma_in_enabled
            && self.state.is_busy()
            && self.command_remaining > 0
    }

    pub fn dma_can_read(&self) -> bool {
        // XXX From Mednafen, does it really only work if the FIFO is full?
        self.output_fifo_full() && self.dma_out_enabled
    }

    pub fn read_word(&mut self) -> u32 {
        let b0 = self.output_fifo.pop() as u32;
        let b1 = self.output_fifo.pop() as u32;
        let b2 = self.output_fifo.pop() as u32;
        let b3 = self.output_fifo.pop() as u32;

        b0 | (b1 << 8) | (b2 << 16) | (b3 << 24)
    }

    fn decode_rle(&mut self, rle: u16) {
        if self.block_index == 0 {
            if rle == 0xfe00 {
                // This is normally the end-of-block marker but if it occurs before the start of a
                // block it's just padding and we should ignore it.
                return;
            }

            // The first value in the block is the DC value (low 10 bits) and the AC quantization
            // scaling factor (high 6 bits)
            self.qscale = (rle >> 10) as u8;
            let dc = rle & 0x3ff;

            let dc = quantize(dc, self.quantization(), None);
            self.next_block_coeff(dc);
            self.block_index = 1;
        } else {
            if rle == 0xfe00 {
                // End-of-block marker
                while self.block_index < 8 * 8 {
                    self.next_block_coeff(0);
                }
            } else {
                // Decode RLE encoded block
                let zeroes = rle >> 10;
                let ac = rle & 0x3ff;

                // Fill the zeroes
                for _ in 0..zeroes {
                    self.next_block_coeff(0);
                }

                // Compute the value of the AC coefficient
                let ac = quantize(ac, self.quantization(), Some(self.qscale));
                self.next_block_coeff(ac);
            }

            if self.block_index == 8 * 8 {
                // Block full, moving on
                self.next_block();
            }
        }
    }

    /// Return the quantization factor for the current block_index
    fn quantization(&self) -> u8 {
        let index = self.block_index as usize;

        let matrix = if self.command.is_monochrome() {
            0
        } else {
            match self.current_block {
                BlockType::CrMono | BlockType::Cb => 1,
                _ => 0,
            }
        };

        self.quant_matrices[matrix][index]
    }

    /// Set the value of the current block coeff pointed to by `block_index` and increment
    /// `block_index`.
    fn next_block_coeff(&mut self, coeff: i16) {
        self.block_coeffs.set_zigzag(self.block_index, coeff);
        self.block_index += 1;
    }

    fn next_block(&mut self) {
        if self.command.is_monochrome() {
            self.idct_matrix.idct(&self.block_coeffs, &mut self.block_y);

            unimplemented!();
        } else {
            let finished_block = self.current_block;

            let generate_pixels = {
                let (idct_target, generate_pixels, next_block) = match self.current_block {
                    BlockType::Y1 => (&mut self.block_y, true, BlockType::Y2),
                    BlockType::Y2 => (&mut self.block_y, true, BlockType::Y3),
                    BlockType::Y3 => (&mut self.block_y, true, BlockType::Y4),
                    BlockType::Y4 => (&mut self.block_y, true, BlockType::CrMono),
                    BlockType::CrMono => (&mut self.block_v, false, BlockType::Cb),
                    BlockType::Cb => (&mut self.block_u, false, BlockType::Y1),
                };

                self.idct_matrix.idct(&self.block_coeffs, idct_target);

                self.current_block = next_block;

                generate_pixels
            };

            if generate_pixels {
                // We have Y, U and V macroblocks, we can convert the
                // value and generate RGB pixels
                if self.command.output_depth() == OutputDepth::D15 {
                    self.generate_pixels_rgb15();
                } else {
                    self.generate_pixels_rgb24(finished_block);
                }
            }
        }

        // We're ready for the next block's coefficients
        self.block_index = 0;
    }

    fn generate_pixels_rgb15(&mut self) {
        for y in 0..8 {
            for x in 0..8 {
                let l = self.block_y[y * 8 + x];
                let u = self.block_u[y * 8 + x];
                let v = self.block_v[y * 8 + x];

                let (r, g, b) = yuv_to_rgb(l, u, v);

                // Convert to RGB555
                let r = i16::from(r) >> 3;
                let g = i16::from(g) >> 3;
                let b = i16::from(b) >> 3;

                let v = r | (g << 5) | (b << 5);

                self.output_fifo.push(v as u8);
                self.output_fifo.push((v >> 8) as u8);
            }
        }
    }

    fn generate_pixels_rgb24(&mut self, block_type: BlockType) {
        let t = block_type as usize;

        for y in 0..8 {
            let uv_y = (y >> 1) | (t & 2) << 1;
            let uv_x = (t & 1) << 2;

            for x in 0..8 {
                let uv_x = uv_x + (x >> 1);

                let l = self.block_y[y * 8 + x];
                let u = self.block_u[uv_y * 8 + uv_x];
                let v = self.block_v[uv_y * 8 + uv_x];

                let (r, g, b) = yuv_to_rgb(l, u, v);

                self.output_fifo.push(r);
                self.output_fifo.push(g);
                self.output_fifo.push(b);
            }
        }
    }
}

pub fn run(psx: &mut Psx) {
    // TODO: timings
    psx.mdec.run(0);
}

pub fn store<T: Addressable>(psx: &mut Psx, off: u32, val: T) {
    run(psx);

    if T::width() != AccessWidth::Word {
        unimplemented!("Unhandled MDEC store ({:?})", T::width());
    }

    let val = val.as_u32();

    match off {
        0 => psx.mdec.push_command(val),
        4 => psx.mdec.set_control(val),
        _ => unimplemented!("Unhandled MDEC store: {:08x} {:08x}", off, val),
    }
}

pub fn load<T: Addressable>(psx: &mut Psx, off: u32) -> T {
    run(psx);

    if T::width() != AccessWidth::Word {
        unimplemented!("Unhandled MDEC load ({:?})", T::width());
    }

    let v = match off {
        4 => psx.mdec.status(),
        _ => unimplemented!("Unhandled MDEC load: {:08x}", off),
    };

    T::from_u32(v)
}

pub fn dma_can_write(psx: &mut Psx) -> bool {
    run(psx);

    psx.mdec.dma_can_write()
}

pub fn dma_store(psx: &mut Psx, v: u32) {
    run(psx);

    psx.mdec.push_command(v);
}

pub fn dma_can_read(psx: &mut Psx) -> bool {
    run(psx);

    psx.mdec.dma_can_read()
}

/// Retuns the word being loaded as well as the offset in VRAM
pub fn dma_load(psx: &mut Psx) -> (u32, u32) {
    run(psx);

    let mdec = &mut psx.mdec;

    let v = mdec.read_word();

    let line = mdec.dma_block_line as u32;
    let line_length = mdec.dma_block_line_length as u32;

    let mut offset = (line & 7) * line_length;

    if line & 8 != 0 {
        offset = offset.wrapping_sub(line_length * 7);
    }

    mdec.dma_block_column -= 1;
    if mdec.dma_block_column == 0 {
        mdec.dma_block_column = mdec.dma_block_line_length;
        mdec.dma_block_line = mdec.dma_block_line.wrapping_add(1);
    }

    (v, (offset as u32) << 2)
}

#[derive(PartialEq, Eq, Debug)]
enum State {
    Idle,
    LoadQuantTable(u8),
    LoadIdctMatrix,
    Decoding,
}

impl State {
    fn is_busy(&self) -> bool {
        if let State::Idle = self {
            false
        } else {
            true
        }
    }
}

/// Command word
#[derive(Copy, Clone)]
struct Command(u32);

impl Command {
    fn opcode(self) -> u32 {
        self.0 >> 29
    }

    fn block_len(self) -> u16 {
        debug_assert!(self.opcode() == 1);
        self.0 as u16
    }

    fn output_format(self) -> u32 {
        (self.0 >> 25) & 0xf
    }

    /// Returns true if we are loading color quantization matrices, false otherwise
    fn quant_color(self) -> bool {
        debug_assert!(self.opcode() == 2);
        self.0 & 1 != 0
    }

    fn output_depth(self) -> OutputDepth {
        debug_assert!(self.opcode() == 1);

        match (self.0 >> 27) & 3 {
            0 => OutputDepth::D4,
            1 => OutputDepth::D8,
            2 => OutputDepth::D24,
            3 => OutputDepth::D15,
            _ => unreachable!(),
        }
    }

    fn is_monochrome(self) -> bool {
        match self.output_depth() {
            OutputDepth::D4 | OutputDepth::D8 => true,
            _ => false,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum OutputDepth {
    D4 = 0,
    D8 = 1,
    D15 = 3,
    D24 = 2,
}

struct IdctMatrix([i16; 64]);

impl IdctMatrix {
    fn new() -> IdctMatrix {
        IdctMatrix([0; 64])
    }

    fn set(&mut self, index: u8, coef: i16) {
        // We shuffle the table to make it a bit more cache friendly in `idct` as well as making it
        // easier to implement it using SIMD
        let index = ((index & 7) << 3) | ((index >> 3) & 7);
        self[index as usize] = coef;
    }

    /// Compute the Inverse Discrete Cosine Transform of `coeffs` and store the result in `block`
    fn idct(&self, coeffs: &MacroblockCoeffs, block: &mut Macroblock) {
        // XXX This function could greatly benefit from SIMD code when Rust supports it. The full
        // IDCT takes 1024 multiplications.
        let mut block_tmp = [0i16; 8 * 8];

        // First pass, store intermediate results in `block_tmp`
        for y in 0..8 {
            for x in 0..8 {
                let mut sum = 0i32;

                for c in 0..8 {
                    let coef = coeffs[y * 8 + c] as i32;

                    // XXX what happens in case of overflow? Should test on real hardware.
                    sum += coef * self[x * 8 + c] as i32;
                }

                let v = (sum + 0x4000) >> 15;

                block_tmp[x * 8 + y] = v as i16;
            }
        }

        // 2nd pass, saturate the values into `block`
        for y in 0..8 {
            for x in 0..8 {
                let mut sum = 0i32;

                for c in 0..8 {
                    let coef = block_tmp[y * 8 + c] as i32;

                    // XXX what happens in case of overflow? Should test on real hardware.
                    sum += coef * self[x * 8 + c] as i32;
                }

                let v = (sum + 0x4000) >> 15;

                // Sign extend 9bit value
                let v = v as u16;
                let v = v << (16 - 9);
                let v = (v as i16) >> (16 - 9);

                // Saturate
                let v = if v < -128 {
                    -128
                } else if v > 127 {
                    127
                } else {
                    v as i8
                };

                block[y * 8 + x] = v;
            }
        }
    }
}

impl Index<usize> for IdctMatrix {
    type Output = i16;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl IndexMut<usize> for IdctMatrix {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum BlockType {
    Y1 = 0,
    Y2 = 1,
    Y3 = 2,
    Y4 = 3,
    /// Luma (Y) for monochrome, Cr otherwise
    CrMono = 4,
    Cb = 5,
}

/// Convert `val` into a signed 10 bit value
fn to_10bit_signed(val: u16) -> i16 {
    ((val << 6) as i16) >> 6
}

/// Quantization function for the macroblock coefficients. For the DC coeffs qscale should be None.
fn quantize(coef: u16, quantization: u8, qscale: Option<u8>) -> i16 {
    if coef == 0 {
        0
    } else {
        let c = to_10bit_signed(coef);
        let (qscale, qshift) = match qscale {
            Some(qs) => (qs, 3),
            // DC doesn't use the qscale value and does not require right shifting after the
            // multiplication.
            _ => (1, 0),
        };

        let q = quantization as i32 * qscale as i32;

        let c = if q == 0 {
            (c << 5) as i32
        } else {
            let c = c as i32;
            let c = (c * q) >> qshift;
            let c = c << 4;

            // XXX This is from mednafen, not sure why this is needed.
            if c < 0 {
                c + 8
            } else {
                c - 8
            }
        };

        // Saturate
        if c > 0x3fff {
            0x3fff
        } else if c < -0x4000 {
            -0x4000
        } else {
            c as i16
        }
    }
}

fn sign_extend_9bits(v: i32) -> i8 {
    let v = v as u16;
    let v = v << (16 - 9);
    let v = (v as i16) >> (16 - 9);

    // Saturate
    if v < -128 {
        -128
    } else if v > 127 {
        127
    } else {
        v as i8
    }
}
fn yuv_to_rgb(y: i8, u: i8, v: i8) -> (u8, u8, u8) {
    let y = y as i32;
    let u = u as i32;
    let v = v as i32;

    let r = y + (((359 * v) + 0x80) >> 8);
    let g = y + ((((-88 * u) & !0x1F) + ((-183 * v) & !0x07) + 0x80) >> 8);
    let b = y + (((454 * u) + 0x80) >> 8);

    let r = sign_extend_9bits(r) as u8 ^ 0x80;
    let g = sign_extend_9bits(g) as u8 ^ 0x80;
    let b = sign_extend_9bits(b) as u8 ^ 0x80;

    (r, g, b)
}

struct Macroblock([i8; 8 * 8]);

impl Macroblock {
    fn new() -> Macroblock {
        Macroblock([0; 8 * 8])
    }
}

impl Index<usize> for Macroblock {
    type Output = i8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl IndexMut<usize> for Macroblock {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}
/// Coefficients during macroblock decoding
struct MacroblockCoeffs([i16; 8 * 8]);

impl MacroblockCoeffs {
    fn new() -> MacroblockCoeffs {
        MacroblockCoeffs([0; 8 * 8])
    }

    /// RLE-encoded values are encoded using a "zigzag" pattern in
    /// order to maximise the number of consecutive zeroes.
    fn set_zigzag(&mut self, pos: u8, coeff: i16) {
        // Zigzag LUT
        let zigzag: [u8; 64] = [
            0x00, 0x08, 0x01, 0x02, 0x09, 0x10, 0x18, 0x11, 0x0a, 0x03, 0x04, 0x0b, 0x12, 0x19,
            0x20, 0x28, 0x21, 0x1a, 0x13, 0x0c, 0x05, 0x06, 0x0d, 0x14, 0x1b, 0x22, 0x29, 0x30,
            0x38, 0x31, 0x2a, 0x23, 0x1c, 0x15, 0x0e, 0x07, 0x0f, 0x16, 0x1d, 0x24, 0x2b, 0x32,
            0x39, 0x3a, 0x33, 0x2c, 0x25, 0x1e, 0x17, 0x1f, 0x26, 0x2d, 0x34, 0x3b, 0x3c, 0x35,
            0x2e, 0x27, 0x2f, 0x36, 0x3d, 0x3e, 0x37, 0x3f,
        ];

        if pos >= 64 {
            // XXX Not sure how the MDEC deals with index
            // overflows. Does it wrap around somehow? Does it move to
            // the next block?
            panic!("Block index overflow!");
        }

        let index = zigzag[pos as usize];

        self.0[index as usize] = coeff
    }
}

impl Index<usize> for MacroblockCoeffs {
    type Output = i16;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl IndexMut<usize> for MacroblockCoeffs {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}

#[test]
fn test_quantize_dc() {
    // XXX These values are taken from Mednafen at the moment, it
    // would be better to validate against the real hardware.
    assert_eq!(quantize(0, 0, None), 0);
    assert_eq!(quantize(0, 255, None), 0);
    assert_eq!(quantize(1, 0, None), 32);
    assert_eq!(quantize(1, 1, None), 8);
    assert_eq!(quantize(1, 2, None), 24);
    assert_eq!(quantize(1, 255, None), 4072);
    assert_eq!(quantize(2, 0, None), 64);
    assert_eq!(quantize(5, 204, None), 16312);
    assert_eq!(quantize(5, 205, None), 16383);
    assert_eq!(quantize(5, 206, None), 16383);
    assert_eq!(quantize(5, 255, None), 16383);
    assert_eq!(quantize(512, 0, None), -16384);
    assert_eq!(quantize(512, 1, None), -8184);
    assert_eq!(quantize(512, 2, None), -16376);
    assert_eq!(quantize(589, 0, None), -13920);
    assert_eq!(quantize(589, 1, None), -6952);
    assert_eq!(quantize(589, 2, None), -13912);
    assert_eq!(quantize(1023, 255, None), -4072);
    assert_eq!(quantize(1023, 0, None), -32);
}

#[test]
fn test_quantize_ac() {
    // XXX These values are taken from Mednafen at the moment, it
    // would be better to validate against the real hardware.
    assert_eq!(quantize(0, 0, Some(0)), 0);
    assert_eq!(quantize(0, 0, Some(63)), 0);
    assert_eq!(quantize(0, 1, Some(1)), 0);
    assert_eq!(quantize(0, 255, Some(63)), 0);
    assert_eq!(quantize(1, 0, Some(0)), 32);
    assert_eq!(quantize(1, 1, Some(0)), 32);
    assert_eq!(quantize(1, 1, Some(1)), -8);
    assert_eq!(quantize(1, 1, Some(7)), -8);
    assert_eq!(quantize(1, 1, Some(8)), 8);
    assert_eq!(quantize(1, 1, Some(15)), 8);
    assert_eq!(quantize(1, 1, Some(16)), 24);
    assert_eq!(quantize(1, 39, Some(62)), 4824);
    assert_eq!(quantize(1, 255, Some(63)), 16383);
    assert_eq!(quantize(1, 255, Some(32)), 16312);
    assert_eq!(quantize(2, 0, Some(0)), 64);
    assert_eq!(quantize(511, 255, Some(63)), 16383);
    assert_eq!(quantize(512, 0, Some(0)), -16384);
    assert_eq!(quantize(1000, 0, Some(0)), -768);
    assert_eq!(quantize(1000, 2, Some(57)), -5464);
    assert_eq!(quantize(1000, 220, Some(27)), -16384);
    assert_eq!(quantize(1003, 80, Some(3)), -10072);
}

#[test]
fn test_idct() {
    let coeffs = MacroblockCoeffs([
        0, 257, 514, 771, 1028, 1285, 1542, 1799, 8, 265, 522, 779, 1036, 1293, 1550, 1807, 16,
        273, 530, 787, 1044, 1301, 1558, 1815, 24, 281, 538, 795, 1052, 1309, 1566, 1823, 32, 289,
        546, 803, 1060, 1317, 1574, 1831, 40, 297, 554, 811, 1068, 1325, 1582, 1839, 48, 305, 562,
        819, 1076, 1333, 1590, 1847, 56, 313, 570, 827, 1084, 1341, 1598, 1855,
    ]);

    // This is the "standard" IDCT table used in most PSX games
    let idct_coeffs: [i16; 64] = [
        23170, 23170, 23170, 23170, 23170, 23170, 23170, 23170, 32138, 27245, 18204, 6392, -6393,
        -18205, -27246, -32139, 30273, 12539, -12540, -30274, -30274, -12540, 12539, 30273, 27245,
        -6393, -32139, -18205, 18204, 32138, 6392, -27246, 23170, -23171, -23171, 23170, 23170,
        -23171, -23171, 23170, 18204, -32139, 6392, 27245, -27246, -6393, 32138, -18205, 12539,
        -30274, 30273, -12540, -12540, 30273, -30274, 12539, 6392, -18205, 27245, -32139, 32138,
        -27246, 18204, -6393,
    ];

    let mut matrix = IdctMatrix::new();

    for (i, b) in idct_coeffs.iter().enumerate() {
        // The "weird" bitshift used by mednafen
        matrix.set(i as u8, b >> 3);
    }

    let mut block = Macroblock::new();

    matrix.idct(&coeffs, &mut block);

    let expected = Macroblock([
        -128, -95, 71, -27, 38, -5, 22, 9, 127, 96, -75, 27, -40, 4, -23, -10, 127, -39, 30, -11,
        16, -2, 9, 4, -117, 33, -26, 9, -14, 1, -8, -3, 62, -18, 14, -5, 7, -1, 4, 2, -52, 14, -11,
        4, -6, 1, -4, -2, 21, -6, 5, -2, 3, 0, 1, 1, -14, 3, -3, 1, -1, 0, -1, 0,
    ]);

    for i in 0..64 {
        assert_eq!(expected.0[i], block.0[i]);
    }
}
