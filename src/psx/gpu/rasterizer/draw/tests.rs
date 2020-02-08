//! Rasterizer tests
//!
//! Unless otherwise noted the expected output was generated on a real PlayStation (model
//! SCPH-7502, PAL).

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

    let init_commands = vec![
        // Reset
        Command::Gp1(0x00000000),
        // Set drawing area top left at 0, 0
        Command::Gp0(0xe3000000),
        // Set drawing area bottom right at 256, 256
        Command::Gp0(0xe4000000 | 256 | (256 << 10)),
        // Set drawing offset at 0, 0
        Command::Gp0(0xe5000000),
    ];

    command_sender.send(init_commands).unwrap();

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

    let x = VRamPixel::new();
    let r = VRamPixel::from_bgr888(Bgr888::from_command(0x0000ff));

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

#[test]
fn triangle_solid_opaque_flat_up() {
    let (mut rasterizer, command_channel, _) = build_rasterizer();

    let commands = vec![
        Command::Gp0(0x200000ff),
        vertex_coord(5, 1),
        vertex_coord(2, 6),
        vertex_coord(9, 6),
        Command::Special(Special::Quit),
    ];

    command_channel.send(commands).unwrap();

    rasterizer.run();

    let x = VRamPixel::new();
    let r = VRamPixel::from_bgr888(Bgr888::from_command(0x0000ff));

    let expected: &[&[VRamPixel]] = &[
        &[x, x, x, x, x, x, x, x, x, x],
        &[x, x, x, x, x, x, x, x, x, x],
        &[x, x, x, x, x, r, x, x, x, x],
        &[x, x, x, x, r, r, r, x, x, x],
        &[x, x, x, x, r, r, r, r, x, x],
        &[x, x, x, r, r, r, r, r, r, x],
        &[x, x, x, x, x, x, x, x, x, x],
    ];

    check_rasterizer(&rasterizer, &expected);
}

#[test]
fn triangle_solid_opaque_flat_down() {
    let (mut rasterizer, command_channel, _) = build_rasterizer();

    let commands = vec![
        Command::Gp0(0x200000ff),
        vertex_coord(5, 6),
        vertex_coord(2, 1),
        vertex_coord(9, 1),
        Command::Special(Special::Quit),
    ];

    command_channel.send(commands).unwrap();

    rasterizer.run();

    let x = VRamPixel::new();
    let r = VRamPixel::from_bgr888(Bgr888::from_command(0x0000ff));

    let expected: &[&[VRamPixel]] = &[
        &[x, x, x, x, x, x, x, x, x, x],
        &[x, x, r, r, r, r, r, r, r, x],
        &[x, x, x, r, r, r, r, r, r, x],
        &[x, x, x, x, r, r, r, r, x, x],
        &[x, x, x, x, r, r, r, x, x, x],
        &[x, x, x, x, x, r, x, x, x, x],
        &[x, x, x, x, x, x, x, x, x, x],
    ];

    check_rasterizer(&rasterizer, &expected);
}

#[test]
fn triangle_solid_opaque_flat_right() {
    let (mut rasterizer, command_channel, _) = build_rasterizer();

    let commands = vec![
        Command::Gp0(0x200000ff),
        vertex_coord(6, 5),
        vertex_coord(1, 2),
        vertex_coord(1, 9),
        Command::Special(Special::Quit),
    ];

    command_channel.send(commands).unwrap();

    rasterizer.run();

    let x = VRamPixel::new();
    let r = VRamPixel::from_bgr888(Bgr888::from_command(0x0000ff));

    let expected: &[&[VRamPixel]] = &[
        &[x, x, x, x, x, x, x],
        &[x, x, x, x, x, x, x],
        &[x, x, x, x, x, x, x],
        &[x, r, r, x, x, x, x],
        &[x, r, r, r, r, x, x],
        &[x, r, r, r, r, r, x],
        &[x, r, r, r, r, x, x],
        &[x, r, r, r, x, x, x],
        &[x, r, r, x, x, x, x],
        &[x, x, x, x, x, x, x],
    ];

    check_rasterizer(&rasterizer, &expected);
}

#[test]
fn triangle_solid_opaque_flat_left() {
    let (mut rasterizer, command_channel, _) = build_rasterizer();

    let commands = vec![
        Command::Gp0(0x200000ff),
        vertex_coord(1, 5),
        vertex_coord(6, 2),
        vertex_coord(6, 9),
        Command::Special(Special::Quit),
    ];

    command_channel.send(commands).unwrap();

    rasterizer.run();

    let x = VRamPixel::new();
    let r = VRamPixel::from_bgr888(Bgr888::from_command(0x0000ff));

    let expected: &[&[VRamPixel]] = &[
        &[x, x, x, x, x, x, x],
        &[x, x, x, x, x, x, x],
        &[x, x, x, x, x, x, x],
        &[x, x, x, x, x, r, x],
        &[x, x, x, r, r, r, x],
        &[x, r, r, r, r, r, x],
        &[x, x, x, r, r, r, x],
        &[x, x, x, x, r, r, x],
        &[x, x, x, x, x, r, x],
        &[x, x, x, x, x, x, x],
    ];

    check_rasterizer(&rasterizer, &expected);
}

#[test]
fn triangle_solid_opaque_slant_top_left() {
    let (mut rasterizer, command_channel, _) = build_rasterizer();

    let commands = vec![
        Command::Gp0(0x200000ff),
        vertex_coord(2, 2),
        vertex_coord(4, 6),
        vertex_coord(9, 6),
        Command::Special(Special::Quit),
    ];

    command_channel.send(commands).unwrap();

    rasterizer.run();

    let x = VRamPixel::new();
    let r = VRamPixel::from_bgr888(Bgr888::from_command(0x0000ff));

    let expected: &[&[VRamPixel]] = &[
        &[x, x, x, x, x, x, x, x, x],
        &[x, x, x, x, x, x, x, x, x],
        &[x, x, x, x, x, x, x, x, x],
        &[x, x, x, r, x, x, x, x, x],
        &[x, x, x, r, r, r, x, x, x],
        &[x, x, x, x, r, r, r, r, x],
        &[x, x, x, x, x, x, x, x, x],
    ];

    check_rasterizer(&rasterizer, &expected);
}

#[test]
fn triangle_solid_opaque_slant_top_right() {
    let (mut rasterizer, command_channel, _) = build_rasterizer();

    let commands = vec![
        Command::Gp0(0x200000ff),
        vertex_coord(9, 2),
        vertex_coord(2, 6),
        vertex_coord(7, 6),
        Command::Special(Special::Quit),
    ];

    command_channel.send(commands).unwrap();

    rasterizer.run();

    let x = VRamPixel::new();
    let r = VRamPixel::from_bgr888(Bgr888::from_command(0x0000ff));

    let expected: &[&[VRamPixel]] = &[
        &[x, x, x, x, x, x, x, x, x],
        &[x, x, x, x, x, x, x, x, x],
        &[x, x, x, x, x, x, x, x, x],
        &[x, x, x, x, x, x, x, r, x],
        &[x, x, x, x, x, r, r, x, x],
        &[x, x, x, r, r, r, r, x, x],
        &[x, x, x, x, x, x, x, x, x],
    ];

    check_rasterizer(&rasterizer, &expected);
}

#[test]
fn triangle_solid_opaque_slant_bot_left() {
    let (mut rasterizer, command_channel, _) = build_rasterizer();

    let commands = vec![
        Command::Gp0(0x200000ff),
        vertex_coord(2, 6),
        vertex_coord(4, 2),
        vertex_coord(9, 2),
        Command::Special(Special::Quit),
    ];

    command_channel.send(commands).unwrap();

    rasterizer.run();

    let x = VRamPixel::new();
    let r = VRamPixel::from_bgr888(Bgr888::from_command(0x0000ff));

    let expected: &[&[VRamPixel]] = &[
        &[x, x, x, x, x, x, x, x, x, x],
        &[x, x, x, x, x, x, x, x, x, x],
        &[x, x, x, x, r, r, r, r, r, x],
        &[x, x, x, x, r, r, r, r, x, x],
        &[x, x, x, r, r, r, x, x, x, x],
        &[x, x, x, r, x, x, x, x, x, x],
        &[x, x, x, x, x, x, x, x, x, x],
    ];

    check_rasterizer(&rasterizer, &expected);
}

#[test]
fn triangle_solid_opaque_slant_bot_right() {
    let (mut rasterizer, command_channel, _) = build_rasterizer();

    let commands = vec![
        Command::Gp0(0x200000ff),
        vertex_coord(9, 6),
        vertex_coord(2, 2),
        vertex_coord(7, 2),
        Command::Special(Special::Quit),
    ];

    command_channel.send(commands).unwrap();

    rasterizer.run();

    let x = VRamPixel::new();
    let r = VRamPixel::from_bgr888(Bgr888::from_command(0x0000ff));

    let expected: &[&[VRamPixel]] = &[
        &[x, x, x, x, x, x, x, x, x, x],
        &[x, x, x, x, x, x, x, x, x, x],
        &[x, x, r, r, r, r, r, x, x, x],
        &[x, x, x, x, r, r, r, r, x, x],
        &[x, x, x, x, x, x, r, r, x, x],
        &[x, x, x, x, x, x, x, x, r, x],
        &[x, x, x, x, x, x, x, x, x, x],
    ];

    check_rasterizer(&rasterizer, &expected);
}

#[test]
fn triangle_solid_opaque_mid_right() {
    let (mut rasterizer, command_channel, _) = build_rasterizer();

    let commands = vec![
        Command::Gp0(0x200000ff),
        vertex_coord(1, 1),
        vertex_coord(8, 5),
        vertex_coord(3, 8),
        Command::Special(Special::Quit),
    ];

    command_channel.send(commands).unwrap();

    rasterizer.run();

    let x = VRamPixel::new();
    let r = VRamPixel::from_bgr888(Bgr888::from_command(0x0000ff));

    let expected: &[&[VRamPixel]] = &[
        &[x, x, x, x, x, x, x, x, x],
        &[x, x, x, x, x, x, x, x, x],
        &[x, x, r, x, x, x, x, x, x],
        &[x, x, r, r, r, x, x, x, x],
        &[x, x, r, r, r, r, r, x, x],
        &[x, x, x, r, r, r, r, r, x],
        &[x, x, x, r, r, r, r, x, x],
        &[x, x, x, r, r, x, x, x, x],
        &[x, x, x, x, x, x, x, x, x],
    ];

    check_rasterizer(&rasterizer, &expected);
}

#[test]
fn triangle_solid_opaque_mid_left() {
    let (mut rasterizer, command_channel, _) = build_rasterizer();

    let commands = vec![
        Command::Gp0(0x200000ff),
        vertex_coord(9, 0),
        vertex_coord(1, 4),
        vertex_coord(6, 9),
        Command::Special(Special::Quit),
    ];

    command_channel.send(commands).unwrap();

    rasterizer.run();

    let x = VRamPixel::new();
    let r = VRamPixel::from_bgr888(Bgr888::from_command(0x0000ff));

    let expected: &[&[VRamPixel]] = &[
        &[x, x, x, x, x, x, x, x, x, x],
        &[x, x, x, x, x, x, x, r, r, x],
        &[x, x, x, x, x, r, r, r, r, x],
        &[x, x, x, r, r, r, r, r, x, x],
        &[x, r, r, r, r, r, r, r, x, x],
        &[x, x, r, r, r, r, r, r, x, x],
        &[x, x, x, r, r, r, r, x, x, x],
        &[x, x, x, x, r, r, r, x, x, x],
        &[x, x, x, x, x, r, r, x, x, x],
        &[x, x, x, x, x, x, x, x, x, x],
    ];

    check_rasterizer(&rasterizer, &expected);
}

/// Draw a large triangle with a non-trivial edge slope to catch precision errors
#[test]
fn triangle_solid_opaque_big1() {
    let (mut rasterizer, command_channel, _) = build_rasterizer();

    let commands = vec![
        Command::Gp0(0x200000ff),
        vertex_coord(0, 0),
        vertex_coord(0, 500),
        vertex_coord(125, 500),
        Command::Special(Special::Quit),
    ];

    command_channel.send(commands).unwrap();

    rasterizer.run();

    let x = VRamPixel::new();
    let r = VRamPixel::from_bgr888(Bgr888::from_command(0x0000ff));

    let expected: &[&[VRamPixel]] = &[
        &[x, x, x, x, x, x],
        &[r, x, x, x, x, x],
        &[r, x, x, x, x, x],
        &[r, x, x, x, x, x],
        &[r, x, x, x, x, x],
        &[r, r, x, x, x, x],
        &[r, r, x, x, x, x],
        &[r, r, x, x, x, x],
        &[r, r, x, x, x, x],
        &[r, r, r, x, x, x],
        &[r, r, r, x, x, x],
        &[r, r, r, x, x, x],
        &[r, r, r, x, x, x],
        &[r, r, r, r, x, x],
        &[r, r, r, r, x, x],
        &[r, r, r, r, x, x],
        &[r, r, r, r, x, x],
        &[r, r, r, r, r, x],
        &[r, r, r, r, r, x],
        &[r, r, r, r, r, x],
    ];

    check_rasterizer(&rasterizer, &expected);
}

/// Same as big1 but with a small x offset change, enough to change the drawing slightly
#[test]
fn triangle_solid_opaque_big2() {
    let (mut rasterizer, command_channel, _) = build_rasterizer();

    let commands = vec![
        Command::Gp0(0x200000ff),
        vertex_coord(0, 0),
        vertex_coord(0, 500),
        vertex_coord(125, 500),
        Command::Special(Special::Quit),
    ];

    command_channel.send(commands).unwrap();

    rasterizer.run();

    let x = VRamPixel::new();
    let r = VRamPixel::from_bgr888(Bgr888::from_command(0x0000ff));

    let expected: &[&[VRamPixel]] = &[
        &[x, x, x, x, x, x],
        &[r, x, x, x, x, x],
        &[r, x, x, x, x, x],
        &[r, x, x, x, x, x],
        &[r, r, x, x, x, x],
        &[r, r, x, x, x, x],
        &[r, r, x, x, x, x],
        &[r, r, x, x, x, x],
        &[r, r, r, x, x, x],
        &[r, r, r, x, x, x],
        &[r, r, r, x, x, x],
        &[r, r, r, x, x, x],
        &[r, r, r, r, x, x],
        &[r, r, r, r, x, x],
        &[r, r, r, r, x, x],
        &[r, r, r, r, x, x],
        &[r, r, r, r, r, x],
        &[r, r, r, r, r, x],
        &[r, r, r, r, r, x],
        &[r, r, r, r, r, x],
    ];

    check_rasterizer(&rasterizer, &expected);
}

/// The PSX only allows to draw triangles up to 512 pixels in height and 1024 pixels in width
#[test]
fn triangle_solid_opaque_draw_limits() {
    let (mut rasterizer, command_channel, _) = build_rasterizer();

    let commands = vec![
        // First a red triangle with the max possible size
        Command::Gp0(0x200000ff),
        vertex_coord(-10, -1),
        vertex_coord(-10, 510),
        vertex_coord(1013, 510),
        // Then attempt to draw a slightly taller green triangle (shouldn't draw anything)
        Command::Gp0(0x2000ff00),
        vertex_coord(-10, -1),
        vertex_coord(-10, 511),
        vertex_coord(1013, 510),
        // Then attempt to draw a slightly wider blue triangle (shouldn't draw anything)
        Command::Gp0(0x20ff0000),
        vertex_coord(-10, -1),
        vertex_coord(-11, 510),
        vertex_coord(1013, 510),
        Command::Special(Special::Quit),
    ];

    command_channel.send(commands).unwrap();

    rasterizer.run();

    let x = VRamPixel::new();
    let r = VRamPixel::from_bgr888(Bgr888::from_command(0x0000ff));

    let expected: &[&[VRamPixel]] = &[
        &[x, x, x, x, x, x],
        &[x, x, x, x, x, x],
        &[x, x, x, x, x, x],
        &[x, x, x, x, x, x],
        &[r, x, x, x, x, x],
        &[r, r, r, x, x, x],
        &[r, r, r, r, r, x],
        &[r, r, r, r, r, r],
    ];

    check_rasterizer(&rasterizer, &expected);
}

/// Two triangles with slightly different coordinates which actually end up drawing the exact
/// same pixels
#[test]
fn triangle_solid_opaque_false_friends() {
    let (mut rasterizer, command_channel, _) = build_rasterizer();

    let commands = vec![
        Command::Gp0(0x2000_00ff),
        vertex_coord(1, 1),
        vertex_coord(1, 6),
        vertex_coord(6, 6),
        // This 2nd triangle has a slightly different shape but it ends up drawing exactly the same
        // pixel pattern
        Command::Gp0(0x2000_00ff),
        vertex_coord(0, 7),
        vertex_coord(1, 12),
        vertex_coord(5, 11),
        Command::Special(Special::Quit),
    ];

    command_channel.send(commands).unwrap();

    rasterizer.run();

    let x = VRamPixel::new();
    let r = VRamPixel::from_bgr888(Bgr888::from_command(0x0000ff));

    let expected: &[&[VRamPixel]] = &[
        &[x, x, x, x, x, x],
        &[x, x, x, x, x, x],
        &[x, r, x, x, x, x],
        &[x, r, r, x, x, x],
        &[x, r, r, r, x, x],
        &[x, r, r, r, r, x],
        &[x, x, x, x, x, x],
        &[x, x, x, x, x, x],
        &[x, r, x, x, x, x],
        &[x, r, r, x, x, x],
        &[x, r, r, r, x, x],
        &[x, r, r, r, r, x],
        &[x, x, x, x, x, x],
    ];

    check_rasterizer(&rasterizer, &expected);
}
