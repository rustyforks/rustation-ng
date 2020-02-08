use super::{Bgr888, Command, CommandBuffer, Frame, Rasterizer, Special, VRamPixel};
use std::sync::mpsc;

fn build_rasterizer() -> (
    Rasterizer,
    mpsc::Sender<CommandBuffer>,
    mpsc::Receiver<Frame>,
) {
    let (command_sender, command_receiver) = mpsc::channel();
    let (frame_sender, frame_receiver) = mpsc::channel();

    let rasterizer = Rasterizer::new(command_receiver, frame_sender);

    (rasterizer, command_sender, frame_receiver)
}

fn vertex_coord(x: i16, y: i16) -> Command {
    let x = x as u16;
    let y = y as u16;

    Command::Gp0((x as u32) | ((y as u32) << 16))
}

fn check_rasterizer(rasterizer: &Rasterizer, expected: &[&[VRamPixel]]) {
    for (y, line) in expected.iter().enumerate() {
        for (x, &color) in line.iter().enumerate() {
            let p = rasterizer.vram[y * 1024 + x];

            assert_eq!(color, p, "VRAM {}x{}: expected {} got {}", x, y, color, p);
        }
    }
}

#[test]
fn quad_rect_solid_opaque() {
    let (mut rasterizer, command_channel, _) = build_rasterizer();

    let commands = vec![
        // Reset
        Command::Gp1(0x00000000),
        // Set drawing area top left at 0, 0
        Command::Gp0(0xe3000000),
        // Set drawing area bottom right at 256, 256
        Command::Gp0(0xe4000000 | 256 | (256 << 10)),
        // Set drawing offset at 0, 0
        Command::Gp0(0xe5000000),
        // Draw a red quad
        Command::Gp0(0x280000ff),
        vertex_coord(2, 2),
        vertex_coord(2, 6),
        vertex_coord(4, 2),
        vertex_coord(4, 6),
        Command::Special(Special::Quit),
    ];

    command_channel.send(commands).unwrap();

    rasterizer.run();

    let r = VRamPixel::from_bgr888(Bgr888::from_command(0x0000ff));
    let x = VRamPixel::from_bgr888(Bgr888::from_command(0x000000));

    let expected: &[&[VRamPixel]] = &[
        &[x, x, x, x, x, x],
        &[x, x, x, x, x, x],
        &[x, x, r, r, x, x],
        &[x, x, r, r, x, x],
        &[x, x, r, r, x, x],
        &[x, x, r, r, x, x],
        &[x, x, x, x, x, x],
    ];

    check_rasterizer(&rasterizer, &expected);
}
