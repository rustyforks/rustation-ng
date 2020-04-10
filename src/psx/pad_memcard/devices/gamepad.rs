use super::{DeviceInterface, DsrState};

/// Digital buttons on a PlayStation controller. The value assigned to each button is the bit
/// position in the 16bit word returned in the serial protocol
#[derive(Clone, Copy, Debug)]
pub enum Button {
    Select = 0,
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
    fn set_button_state(&mut self, button: Button, state: ButtonState);
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
    }
}

impl DeviceInterface for DigitalPad {
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
            // Shouldn't be reached
            _ => (0xff, false),
        };

        let dsr_state = if send_dsr {
            // The actual length of the pulse seems to vary between 90 and 100 cycles depending on
            // the controller.
            //
            // Note that the 440 cycle delay is *not* from the moment the RX not empty goes up in the
            // controller, because it seems that there's a ~`baud_divider` delay between the moment
            // the controller handles the command and the moment RX not empty is asserted.
            DsrState::Pending(440, 90)
        } else {
            DsrState::Idle
        };

        (resp, dsr_state)
    }
}
