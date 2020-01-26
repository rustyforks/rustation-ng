//! Code for the rasterizer. It runs in a different threads from the rest of the emulator for
//! performance reasons and communicates through a pair of channels (one to receive draw commands,
//! one to send back the finished frames).

use std::sync::mpsc;
use std::thread;

enum LastFrame {
    /// We asked the rasterizer to provide a frame and we're waiting for it on the `frame_channel`
    Pending,
    /// The last frame we've received
    Frame(Frame),
}

/// This is the handle used from the main thread to communicate with the rasterizer
pub struct Handle {
    command_buffer: Vec<RawCommand>,
    handle: Option<thread::JoinHandle<()>>,
    command_channel: mpsc::Sender<CommandBuffer>,
    frame_channel: mpsc::Receiver<Frame>,
    last_frame: LastFrame,
}

impl Handle {
    pub fn push_command(&mut self, c: Command) {
        self.command_buffer.push(c.into_raw());
    }

    /// Send the command buffer to the pasteurizer thread
    pub fn flush_command_buffer(&mut self) {
        if self.command_buffer.is_empty() {
            return;
        }

        let mut commands = CommandBuffer::new();

        ::std::mem::swap(&mut commands, &mut self.command_buffer);

        self.command_channel.send(commands).unwrap();
    }

    /// Notify the rasterizer that a line has been fully displayed on the TV output
    pub fn end_of_line(&mut self, line: u16) {
        self.push_command(Command::end_of_line(line));

        // Flush after each line
        self.flush_command_buffer();
    }

    /// Notify the rasterizer that the current frame is done drawing and should be returned through
    /// the frame channel
    pub fn end_of_frame(&mut self) {
        self.push_command(Command::end_of_frame());
        self.flush_command_buffer();

        // Make sure we were not already waiting for a frame
        self.last_frame();

        // Instead of blocking immediately waiting for the frame let's just save the fact that we
        // asked for a frame
        self.last_frame = LastFrame::Pending;
    }

    pub fn last_frame(&mut self) -> &Frame {
        match self.last_frame {
            LastFrame::Pending => {
                let frame = self.frame_channel.recv().unwrap();

                self.last_frame = LastFrame::Frame(frame);

                // Recursively call this function because why not
                self.last_frame()
            }
            LastFrame::Frame(ref f) => f,
        }
    }
}

impl ::std::ops::Drop for Handle {
    fn drop(&mut self) {
        self.command_buffer.clear();
        self.push_command(Command::quit());

        self.flush_command_buffer();

        if let Some(t) = self.handle.take() {
            t.join().unwrap();
        }
    }
}

/// Starts the rasterizer thread and returns a handle to it
pub fn start() -> Handle {
    let (command_sender, command_receiver) = mpsc::channel();
    let (frame_sender, frame_receiver) = mpsc::channel();

    let builder = thread::Builder::new()
        .name("Rustation GPU rasterizer".to_string())
        .stack_size(32 * 1024);

    let handle = builder
        .spawn(move || {
            let mut rasterizer = Rasterizer::new(command_receiver, frame_sender);
            rasterizer.run();
        })
        .unwrap();

    let dummy_frame = Frame::new(0, 0);

    Handle {
        last_frame: LastFrame::Frame(dummy_frame),
        command_buffer: Vec::new(),
        handle: Some(handle),
        command_channel: command_sender,
        frame_channel: frame_receiver,
    }
}

struct Rasterizer {
    /// Frame currently being drawn
    cur_frame: Frame,
    /// Channel used to receive commands
    command_channel: mpsc::Receiver<CommandBuffer>,
    /// Channel used to send completed frames back
    frame_channel: mpsc::Sender<Frame>,
}

impl Rasterizer {
    fn new(
        command_channel: mpsc::Receiver<CommandBuffer>,
        frame_channel: mpsc::Sender<Frame>,
    ) -> Rasterizer {
        Rasterizer {
            cur_frame: Frame::new(1024, 512),
            command_channel,
            frame_channel,
        }
    }

    fn run(&mut self) {
        loop {
            let commands = self.command_channel.recv().unwrap();

            for &raw in commands.iter() {
                let cmd = Command::from_raw(raw);

                match cmd {
                    Command::Gp0(v) => println!("GP0 {}", v),
                    Command::Gp1(v) => println!("GP1 {}", v),
                    Command::Special(Special::Quit) => return,
                    // XXX draw one line at a time
                    Command::Special(Special::EndOfLine(_)) => (),
                    Command::Special(Special::EndOfFrame) => self.send_frame(),
                }
            }
        }
    }

    fn send_frame(&mut self) {
        let mut new_frame = Frame::new(1024, 512);

        ::std::mem::swap(&mut new_frame, &mut self.cur_frame);

        self.frame_channel.send(new_frame).unwrap();
    }
}

/// Dense encoding of a Command. GP0 command keep their normal encoding, GP1 and "special" commands
/// are encoded using unused GP0 values
#[derive(Copy, Clone)]
struct RawCommand(u32);

type CommandBuffer = Vec<RawCommand>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Command {
    /// GP0 register command
    Gp0(u32),
    /// GP1 register command
    Gp1(u32),
    /// Special command
    Special(Special),
}

impl Command {
    pub fn end_of_line(line: u16) -> Command {
        Command::Special(Special::EndOfLine(line))
    }

    pub fn end_of_frame() -> Command {
        Command::Special(Special::EndOfFrame)
    }

    fn quit() -> Command {
        Command::Special(Special::Quit)
    }

    /// Encode `self` into a `RawCommand`
    fn into_raw(self) -> RawCommand {
        let raw = match self {
            // We can pass GP0 as-is, all the special encodings below map to GP0 NOPs which
            // should never be sent to the rasterizer
            Command::Gp0(v) => v,
            // We use the unused GP0 opcodes 0xf0 to 0xff and 0xe0 for GP1
            Command::Gp1(v) => {
                let op = v >> 24;

                match op {
                    // GP1 commands with the exception of the GPU info are sent prefixed with
                    // 0xf
                    0..=0xf => 0xf000_0000 | v,
                    // This one doesn't fit in the 0xf prefix, so we use 0xe0 instead
                    0x10 => 0xe000_0000 | (v & 0xff_ffff),
                    // The other values shouldn't be possible for GP1
                    _ => unreachable!(),
                }
            }
            // We use the unused GP0 opcode 0x10 for special commands
            Command::Special(s) => {
                // We use the 0x10 prefix for special commands
                0x1000_0000 | s.into_raw()
            }
        };

        let raw = RawCommand(raw);

        // Make sure we didn't mess the encoding
        debug_assert!(Command::from_raw(raw) == self);

        raw
    }

    fn from_raw(raw: RawCommand) -> Command {
        let v = raw.0;
        let op = v >> 24;

        match op {
            0xf0..=0xff => Command::Gp1(v & 0xfff_ffff),
            0xe0 => Command::Gp1(0x1000_0000 | (v & 0xff_ffff)),
            0x10 => Command::Special(Special::from_raw(v & 0xff_ffff)),
            n => Command::Gp0(n),
        }
    }
}

/// Special commands in bits [27:24] when bits [31:28] are 0xf. Value 0x0 is reserved for GP1
/// commands
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Special {
    /// Terminate rasterization
    Quit,
    /// Finalize current line
    EndOfLine(u16),
    /// Finalize frame and return it through `frame_channel`
    EndOfFrame,
}

impl Special {
    fn into_raw(self) -> u32 {
        let mut arg = 0u16;

        let op = match self {
            Special::Quit => 0,
            Special::EndOfLine(l) => {
                arg = l;
                1
            }
            Special::EndOfFrame => 2,
        };

        (op << 16) | u32::from(arg)
    }

    fn from_raw(v: u32) -> Special {
        let op = v >> 16;
        let arg = v as u16;

        match op {
            0 => Special::Quit,
            1 => Special::EndOfLine(arg),
            2 => Special::EndOfFrame,
            _ => unreachable!(),
        }
    }
}

/// One output pixel, in XRGB 8888 format
#[derive(Copy, Clone)]
pub struct OutputPixel(u32);

/// Buffer containing one rendered frame
pub struct Frame {
    pub pixels: Vec<OutputPixel>,
    pub width: usize,
    pub height: usize,
}

impl Frame {
    fn new(width: usize, height: usize) -> Frame {
        Frame {
            pixels: vec![OutputPixel(0xff_00ff); width * height],
            width,
            height,
        }
    }
}
