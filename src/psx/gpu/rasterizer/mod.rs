//! Code for the rasterizer. It runs in a different threads from the rest of the emulator for
//! performance reasons and communicates through a pair of channels (one to receive draw commands,
//! one to send back the finished frames).

mod draw;

use draw::Rasterizer;
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
    command_buffer: CommandBuffer,
    handle: Option<thread::JoinHandle<()>>,
    command_channel: mpsc::Sender<CommandBuffer>,
    frame_channel: mpsc::Receiver<Frame>,
    last_frame: LastFrame,
}

impl Handle {
    pub fn push_command(&mut self, c: Command) {
        self.command_buffer.push(c);
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

    pub fn set_option(&mut self, opt: RasterizerOption) {
        self.push_command(Command::option(opt));
        self.flush_command_buffer();
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

    pub fn push_gp0(&mut self, gp0: u32) {
        self.push_command(Command::Gp0(gp0));
    }

    pub fn push_gp1(&mut self, gp1: u32) {
        self.push_command(Command::Gp1(gp1));
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
        .name("RSX GPU".to_string())
        .stack_size(1024 * 1024);

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

type CommandBuffer = Vec<Command>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Command {
    /// GP0 register command
    Gp0(u32),
    /// GP1 register command
    Gp1(u32),
    /// Special command
    Special(Special),
    /// Option setting
    Option(RasterizerOption),
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

    fn option(opt: RasterizerOption) -> Command {
        Command::Option(opt)
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

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum RasterizerOption {
    DisplayFullVRam(bool),
    ForceTransparency(bool),
}

/// Buffer containing one rendered frame
pub struct Frame {
    /// Frame pixels in xRGB 8888 format
    pub pixels: Vec<u32>,
    pub width: u32,
    pub height: u32,
}

impl Frame {
    fn new(width: u32, height: u32) -> Frame {
        let npixels = width * height;

        Frame {
            pixels: vec![0; npixels as usize],
            width,
            height,
        }
    }

    fn set_pixel(&mut self, x: u32, y: u32, p: u32) {
        debug_assert!(x < self.width);
        debug_assert!(y < self.height);

        let x = x as usize;
        let y = y as usize;

        self.pixels[(y * self.width as usize) + x] = p;
    }
}
