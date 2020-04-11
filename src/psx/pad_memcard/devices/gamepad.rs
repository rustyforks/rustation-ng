use super::{DeviceInterface, DsrState};

/// Digital buttons on a PlayStation controller. The value assigned to each button is the bit
/// position in the 16bit word returned in the serial protocol
#[derive(Clone, Copy, Debug)]
pub enum Button {
    Select = 0,
    L3 = 1,
    R3 = 2,
    Start = 3,
    DUp = 4,
    DRight = 5,
    DDown = 6,
    DLeft = 7,
    L2 = 8,
    R2 = 9,
    L1 = 10,
    R1 = 11,
    Triangle = 12,
    Circle = 13,
    Cross = 14,
    Square = 15,
}

#[derive(Clone, Copy, Debug)]
pub enum ButtonState {
    Pressed,
    Released,
}

pub trait GamePad: DeviceInterface {
    /// Set the state of individual buttons
    fn set_button_state(&mut self, button: Button, state: ButtonState);
    /// Set the state of the axis. Each pair is `(x, y)`.
    fn set_axis_state(&mut self, left: (i16, i16), right: (i16, i16));
}

/// SCPH-1080: Digital gamepad.
///
/// Full state is only two bytes since we only need one bit per button.
pub struct DigitalPad(u16);

impl DigitalPad {
    pub fn new() -> DigitalPad {
        DigitalPad(0xffff)
    }
}

impl GamePad for DigitalPad {
    fn set_button_state(&mut self, button: Button, state: ButtonState) {
        let s = self.0;

        let mask = 1 << (button as usize);

        self.0 = match state {
            ButtonState::Pressed => s & !mask,
            ButtonState::Released => s | mask,
        };

        // Digital pads don't support L3/R3, so those bits are always set to 1
        self.0 |= 0x3;
    }

    // Digital keypads don't have analog sticks, nothing to do.
    fn set_axis_state(&mut self, _: (i16, i16), _: (i16, i16)) {}
}

impl DeviceInterface for DigitalPad {
    fn description(&self) -> String {
        "PlayStation Digital Controller (SCPH-1080)".to_string()
    }

    fn handle_command(&mut self, seq: u8, cmd: u8) -> (u8, DsrState) {
        let (resp, send_dsr) = match seq {
            // First byte should be 0x01 if the command targets the controller
            0 => (0xff, (cmd == 0x01)),
            // Digital gamepad only supports command 0x42: read buttons.
            //
            // Response 0x41: we're a digital PSX controller
            1 => (0x41, (cmd == 0x42)),
            // From then on the command byte is ignored.
            //
            // Response 0x5a: 2nd controller ID byte
            2 => (0x5a, true),
            // First button state byte: direction cross, start and select.
            3 => (self.0 as u8, true),
            // 2nd button state byte: shoulder buttons and "shape" buttons. We don't assert DSR for
            // the last byte.
            4 => ((self.0 >> 8) as u8, false),
            _ => unreachable!(),
        };

        let dsr_state = if send_dsr {
            // The actual length of the pulse seems to vary between 90 and 100 cycles depending on
            // the controller.
            //
            // Note that the 440 cycle delay is *not* from the moment the RX not empty goes up in the
            // controller, because it seems that there's a ~`baud_divider` delay between the moment
            // the controller handles the command and the moment RX not empty is asserted.
            DsrState::Pending(360, 90)
        } else {
            DsrState::Idle
        };

        (resp, dsr_state)
    }
}

/// SCPH-1200: DualShock controller
pub struct DualShock {
    /// State of the digital buttons
    buttons: u16,
    /// State of the left stick. These are the values returned by the pad on the serial link:
    /// 0x00 all the way to one side, 0xff all the way to the other, 0x80 when the stick is
    /// centered.
    left_stick: (u8, u8),
    /// State of the right stick
    right_stick: (u8, u8),
    /// DualShock can optionally deactivate their analog function. This way they behave like
    /// digital gamepads.
    analog_mode: bool,
}

impl DualShock {
    pub fn new() -> DualShock {
        DualShock {
            buttons: 0xffff,
            left_stick: (0x80, 0x80),
            right_stick: (0x80, 0x80),
            analog_mode: true,
        }
    }
}

impl GamePad for DualShock {
    fn set_button_state(&mut self, button: Button, state: ButtonState) {
        let s = self.buttons;

        let mask = 1 << (button as usize);

        self.buttons = match state {
            ButtonState::Pressed => s & !mask,
            ButtonState::Released => s | mask,
        };
    }

    fn set_axis_state(&mut self, left: (i16, i16), right: (i16, i16)) {
        // XXX calibrate the sticks like I did in beetle

        fn i16_to_u8(v: i16) -> u8 {
            let v = (v >> 8) as u8;

            // The pad returns 0x80 at 0 so we need to offset
            v.wrapping_add(0x80)
        }

        self.left_stick = (i16_to_u8(left.0), i16_to_u8(left.1));
        self.right_stick = (i16_to_u8(right.0), i16_to_u8(right.1));
    }
}

impl DeviceInterface for DualShock {
    fn description(&self) -> String {
        "PlayStation DualShock Analog Controller (SCPH-1200)".to_string()
    }

    fn handle_command(&mut self, seq: u8, cmd: u8) -> (u8, DsrState) {
        // The following sequence assumes that the controller is in DualShock mode (with the ANALOG
        // LED switched on).
        let (resp, send_dsr) = match seq {
            // First byte should be 0x01 if the command targets the controller
            0 => (0xff, (cmd == 0x01)),
            1 => {
                let response = if self.analog_mode {
                    // Response 0x73: we're a DualShock
                    0x73
                } else {
                    // Response 0x41: we're a digital PSX controller
                    0x41
                };

                // XXX at this point there's also a 0x43 command to enter a different mode that
                // gives access to rumble config among other things.
                if cmd == 0x43 {
                    warn!("Game used unsupported 0x43 DualShock command");
                }

                (response, (cmd == 0x42))
            }
            // From then on the command byte is ignored.
            // XXX Not true if rumble is activated, but we don't support that yet.
            //
            // Response 0x5a: 2nd controller ID byte
            2 => (0x5a, true),
            // First button state byte: direction cross, start and select.
            3 => {
                let mut response = self.buttons as u8;

                if !self.analog_mode {
                    // No L3/R3 in non-analog mode (tested on real hardware)
                    response |= 0x3;
                }

                (response, true)
            }
            // 2nd button state byte: shoulder buttons and "shape" buttons. We don't assert DSR in
            // non-analog mode since that's the end of the transaction
            4 => ((self.buttons >> 8) as u8, self.analog_mode),
            // Right stick X
            5 => (self.right_stick.0, true),
            // Right stick Y
            6 => (self.right_stick.1, true),
            // Left stick X
            7 => {
                println!("Get DS left X {}", self.left_stick.0);
                (self.left_stick.0, true)
            }
            // Left stick Y
            8 => (self.left_stick.1, false),
            _ => unreachable!(),
        };

        let dsr_state = if send_dsr {
            DsrState::Pending(360, 64)
        } else {
            DsrState::Idle
        };

        (resp, dsr_state)
    }
}
