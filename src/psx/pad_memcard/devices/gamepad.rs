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
    fn set_axis_state(&mut self, _left: (i16, i16), _right: (i16, i16)) {}
    /// Get rumble state. The first u8 is the big motor in the left handle, the 2nd is the small
    /// motor in the right handle.
    fn get_rumble(&self) -> (u8, u8) {
        (0, 0)
    }
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
        self.0 |= 0x6;
    }
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
    /// True if the game locked the analog mode and it can't be changed by pressing the ANALOG
    /// button
    analog_mode_locked: bool,
    /// Special mode activated by using a special command sequence. Changes the controller's
    /// behaviour.
    dualshock_mode: bool,
    /// Type of access for the current command
    access_type: DsAccessType,
    /// If not None it means that the watchdog is active and counts the number of frames since the
    /// last time the controller was active
    watchdog: Option<u8>,
    /// Rumble state for the big motor (left) and small motor (right). 0x00 means off, 0xff means
    /// max. The small motor is always either 0x00 or 0xff.
    rumble: (u8, u8),
    /// Full rumble configuration as set by command 0x4d
    rumble_config: [u8; 6],
    /// If true we can control the rumble motors through command 0x42
    rumble_unlocked: bool,
    /// Value used in various ways internally by some commands.
    command_internal: u8,
}

impl DualShock {
    pub fn new() -> DualShock {
        DualShock {
            buttons: 0xffff,
            left_stick: (0x80, 0x80),
            right_stick: (0x80, 0x80),
            analog_mode: false,
            analog_mode_locked: false,
            dualshock_mode: false,
            access_type: DsAccessType::ReadInput,
            watchdog: None,
            rumble: (0, 0),
            rumble_config: [0xff; 6],
            rumble_unlocked: false,
            command_internal: 0,
        }
    }

    /// Should be called exactly once per frame
    fn run_frame(&mut self) {
        if let Some(ref mut f) = self.watchdog {
            // Watchdog is running.
            *f += 1;

            // On the real hardware the watchdog triggers if we haven't had any activity in about
            // 2.5s. Assuming NTSC framerate that would be about 150 frames.
            if *f > 150 {
                // Reset to digital mode
                info!("Dual Shock watchdog reset to digital mode");

                self.analog_mode = false;
                self.analog_mode_locked = false;
                self.dualshock_mode = false;
                self.rumble = (0, 0);
                self.rumble_config = [0xff; 6];
                self.rumble_unlocked = false;
                self.watchdog = None;
            }
        }
    }

    fn handle_read_input(&mut self, seq: u8, cmd: u8) -> (u8, bool) {
        match seq {
            // First button state byte: direction cross, start and select.
            3 => {
                let mut response = self.buttons as u8;

                if !self.analog_mode && !self.dualshock_mode {
                    // No L3/R3 in non-analog mode (tested on real hardware)
                    response |= 0x6;
                }

                if self.access_type == DsAccessType::ReadInput && self.rumble_unlocked {
                    // We receive the configuration command for the small rumble motor in the right
                    // handle here. It's got only two states, on or off, driven by the bit 1
                    let small_motor_on = (cmd & 1) != 0;

                    self.rumble.1 = if small_motor_on { 0xff } else { 0x00 };
                }

                (response, true)
            }
            // 2nd button state byte: shoulder buttons and "shape" buttons
            4 => {
                if self.access_type == DsAccessType::ReadInput && self.rumble_unlocked {
                    // We receive the configuration command for the big rumble motor in the left
                    // handle here.
                    self.rumble.0 = cmd;
                }

                // We don't assert DSR in non-analog mode since that's the end of the transaction,
                // unless Dual Shock mode is activated in which case we always return the analog
                // input
                (
                    (self.buttons >> 8) as u8,
                    self.analog_mode || self.dualshock_mode,
                )
            }
            // Right stick X
            5 => (self.right_stick.0, true),
            // Right stick Y
            6 => (self.right_stick.1, true),
            // Left stick X
            7 => (self.left_stick.0, true),
            // Left stick Y
            8 => (self.left_stick.1, false),
            _ => unreachable!(),
        }
    }

    fn handle_change_mode(&mut self, seq: u8, cmd: u8) -> (u8, bool) {
        // The sequence is almost identical to read_input in normal mode.
        let (mut resp, mut send_dsr) = self.handle_read_input(seq, cmd);

        match seq {
            3 => {
                // This is where the mode change occur. Behaviour: if in normal mode we write 0x01
                // to switch to Dual Shock mode. From Dual Shock mode we write 0x00 to return to
                // normal mode. Any other value is ignored.
                if self.dualshock_mode && cmd == 0x00 {
                    self.dualshock_mode = false;
                } else if !self.dualshock_mode && cmd == 0x01 {
                    self.dualshock_mode = true;
                    /* Watchdog is activated */
                    self.watchdog = Some(0);
                }
            }
            4 => {
                // If this command started in normal mode we don't send the analog state if
                // analog_mode is false
                if self.access_type == DsAccessType::NormalChangeMode && !self.analog_mode {
                    send_dsr = false;
                }
            }
            _ => (),
        }

        if self.access_type == DsAccessType::DsChangeMode && seq >= 3 {
            // When run from Dual Shock mode this command returns the same number of bytes but all
            // input state is set to 0
            resp = 0;
        }

        (resp, send_dsr)
    }

    fn handle_set_analog_mode(&mut self, seq: u8, cmd: u8) -> (u8, bool) {
        match seq {
            // Set analog mode
            3 => {
                // Use the command internal to save whether the mode is correct (1) or incorrect
                // (0)
                self.command_internal = 1;

                match cmd {
                    0 => self.analog_mode = false,
                    1 => self.analog_mode = true,
                    _ => {
                        warn!("Received invalid analog mode {:x}", cmd);
                        self.command_internal = 0;
                    }
                }

                (0x00, true)
            }
            4 => {
                match cmd & 3 {
                    2 => self.analog_mode_locked = false,
                    3 => self.analog_mode_locked = true,
                    _ => warn!("Received invalid analog mode lock {:x}", cmd),
                }
                (0x00, true)
            }
            // 0xff if the mode was invalid, 0x00 otherwise. Invalid lock values don't seem to be
            // reflected in the response.
            5 => (
                if self.command_internal == 0 {
                    0xff
                } else {
                    0x00
                },
                true,
            ),
            6 => (0x00, true),
            7 => (0x00, true),
            8 => (0x00, false),
            _ => unreachable!(),
        }
    }

    /// Returns the current state of the analog mode alongside other unknown values
    fn handle_get_analog_mode(&mut self, seq: u8, _cmd: u8) -> (u8, bool) {
        match seq {
            3 => (0x01, true),
            4 => (0x02, true),
            5 => (self.analog_mode as u8, true),
            6 => (0x02, true),
            7 => (0x01, true),
            8 => (0x00, false),
            _ => unreachable!(),
        }
    }

    /// I think those are simply unused commands in the Dual Shock, I don't think they do anything
    /// and I don't think `cmd` is ever used for anything.
    fn handle_dummy_command(&mut self, seq: u8, _cmd: u8) -> (u8, bool) {
        match seq {
            3..=7 => (0x00, true),
            8 => (0x00, false),
            _ => unreachable!(),
        }
    }

    fn handle_mystery_46(&mut self, seq: u8, cmd: u8) -> (u8, bool) {
        match seq {
            3 => {
                // Mystery option for the mystery command
                self.command_internal = cmd;
                (0x00, true)
            }
            4 => (0x00, true),
            n => match self.command_internal {
                0x00 => match n {
                    5 => (0x01, true),
                    6 => (0x02, true),
                    7 => (0x00, true),
                    8 => (0x0a, false),
                    _ => unreachable!(),
                },
                0x01 => match n {
                    5 => (0x01, true),
                    6 => (0x01, true),
                    7 => (0x01, true),
                    8 => (0x14, false),
                    _ => unreachable!(),
                },
                _ => match n {
                    5 => (0x00, true),
                    6 => (0x00, true),
                    7 => (0x00, true),
                    8 => (0x00, false),
                    _ => unreachable!(),
                },
            },
        }
    }

    fn handle_mystery_47(&mut self, seq: u8, _cmd: u8) -> (u8, bool) {
        match seq {
            3 => (0x00, true),
            4 => (0x00, true),
            5 => (0x02, true),
            6 => (0x00, true),
            7 => (0x01, true),
            8 => (0x00, false),
            _ => unreachable!(),
        }
    }

    fn handle_mystery_48(&mut self, seq: u8, _cmd: u8) -> (u8, bool) {
        match seq {
            3 => (0x00, true),
            4 => (0x00, true),
            5 => (0x00, true),
            6 => (0x00, true),
            7 => (0x01, true),
            8 => (0x00, false),
            _ => unreachable!(),
        }
    }

    fn handle_mystery_4c(&mut self, seq: u8, cmd: u8) -> (u8, bool) {
        match seq {
            3 => {
                // Mystery option for the mystery command
                self.command_internal = cmd;
                (0x00, true)
            }
            4 | 5 => (0x00, true),
            6 => match self.command_internal {
                0x00 => (0x04, true),
                0x01 => (0x07, true),
                _ => (0x00, true),
            },
            7 => (0x00, true),
            8 => (0x00, false),
            _ => unreachable!(),
        }
    }

    fn handle_rumble_config(&mut self, seq: u8, cmd: u8) -> (u8, bool) {
        let response = match seq {
            2 => 0x5a,
            3..=8 => {
                // Get new rumble config byte, return the old one
                let index = (seq - 3) as usize;

                let old = self.rumble_config[index];
                self.rumble_config[index] = cmd;
                old
            }
            _ => unreachable!(),
        };

        let dsr_active = if seq < 8 {
            true
        } else {
            // Last byte received, check config
            self.rumble_unlocked = match self.rumble_config {
                [0xff, 0xff, 0xff, 0xff, 0xff, 0xff] => false,
                // Standard command to activate the rumble
                [0x00, 0x01, 0xff, 0xff, 0xff, 0xff] => true,
                _ => {
                    error!("Unsupported rumble config {:x?}", self.rumble_config);
                    // XXX There are many, many possible configurations for this command. You can
                    // unlock only one engine, swap their config, make them share the config etc...
                    // Since we don't know what this configuration does, we disable rumble
                    false
                }
            };

            if !self.rumble_unlocked {
                // Make sure we don't stay stuck with motors activated
                self.rumble = (0, 0);
            }

            false
        };

        (response, dsr_active)
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

        // XXX assume that this function is called exactly once per frame
        self.run_frame();
    }

    fn get_rumble(&self) -> (u8, u8) {
        self.rumble
    }
}

impl DeviceInterface for DualShock {
    fn description(&self) -> String {
        "PlayStation DualShock Analog Controller (SCPH-1200)".to_string()
    }

    fn select(&mut self) {
        // Watchdog is reset every time the select signal goes down, even if the controller is not
        // the target. I assume that the logic is that the controller should return to the default
        // digital mode if the console is reset since the BIOS boot sequence is fairly long.
        if self.watchdog.is_some() {
            // Reset watchdog
            self.watchdog = Some(0);
        }
    }

    fn handle_command(&mut self, seq: u8, cmd: u8) -> (u8, DsrState) {
        // The following sequence assumes that the controller is in DualShock mode (with the ANALOG
        // LED switched on).
        let (resp, send_dsr) = match seq {
            // First byte should be 0x01 if the command targets the controller
            0 => (0xff, (cmd == 0x01)),
            1 => {
                let response = if self.dualshock_mode {
                    0xf3
                } else if self.analog_mode {
                    // Response 0x73: we're a DualShock
                    0x73
                } else {
                    // Response 0x41: we're a digital PSX controller
                    0x41
                };

                let mut continue_sequence = true;

                self.access_type = if self.dualshock_mode {
                    match cmd {
                        0x40 => DsAccessType::DsDummyCommand,
                        0x41 => DsAccessType::DsDummyCommand,
                        0x42 => DsAccessType::ReadInput,
                        0x43 => DsAccessType::DsChangeMode,
                        0x44 => DsAccessType::DsSetAnalogMode,
                        0x45 => DsAccessType::DsGetAnalogMode,
                        0x46 => DsAccessType::DsMystery46,
                        0x47 => DsAccessType::DsMystery47,
                        0x48 => DsAccessType::DsMystery48,
                        0x49 => DsAccessType::DsDummyCommand,
                        0x4a => DsAccessType::DsDummyCommand,
                        0x4b => DsAccessType::DsDummyCommand,
                        0x4c => DsAccessType::DsMystery4c,
                        0x4d => DsAccessType::DsRumbleConfig,
                        0x4e => DsAccessType::DsDummyCommand,
                        0x4f => DsAccessType::DsDummyCommand,
                        _ => {
                            warn!("Unhandled DualShock command {:x}", cmd);
                            continue_sequence = false;
                            DsAccessType::ReadInput
                        }
                    }
                } else {
                    // "Normal" mode
                    match cmd {
                        0x42 => DsAccessType::ReadInput,
                        0x43 => DsAccessType::NormalChangeMode,
                        _ => {
                            warn!("Unhandled normal command {:x}", cmd);
                            continue_sequence = false;
                            DsAccessType::ReadInput
                        }
                    }
                };

                (response, continue_sequence)
            }
            2 => {
                // In my tests the controller doesn't like it when this byte is not 0, but *only*
                // if we're in DualShock mode, otherwise it seems to be ignored. ReadInput (0x42)
                // also seems to ignore this byte in either modes
                if self.access_type != DsAccessType::ReadInput && self.dualshock_mode && cmd != 0 {
                    (0x5a, false)
                } else {
                    (0x5a, true)
                }
            }
            // After that the sequence changes depending on the access_type
            n => match self.access_type {
                DsAccessType::ReadInput => self.handle_read_input(n, cmd),
                DsAccessType::NormalChangeMode => self.handle_change_mode(n, cmd),
                DsAccessType::DsChangeMode => self.handle_change_mode(n, cmd),
                DsAccessType::DsSetAnalogMode => self.handle_set_analog_mode(n, cmd),
                DsAccessType::DsGetAnalogMode => self.handle_get_analog_mode(n, cmd),
                DsAccessType::DsDummyCommand => self.handle_dummy_command(n, cmd),
                DsAccessType::DsMystery46 => self.handle_mystery_46(n, cmd),
                DsAccessType::DsMystery47 => self.handle_mystery_47(n, cmd),
                DsAccessType::DsMystery48 => self.handle_mystery_48(n, cmd),
                DsAccessType::DsMystery4c => self.handle_mystery_4c(n, cmd),
                DsAccessType::DsRumbleConfig => self.handle_rumble_config(n, cmd),
            },
        };

        let dsr_state = if send_dsr {
            DsrState::Pending(360, 64)
        } else {
            DsrState::Idle
        };

        (resp, dsr_state)
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum DsAccessType {
    ReadInput,
    /// Change mode while we're in normal mode
    NormalChangeMode,
    /// Change mode while we're in Dual Shock mode
    DsChangeMode,
    /// Force analog mode and lock or unlock it
    DsSetAnalogMode,
    /// Returns the current state of the analog mode
    DsGetAnalogMode,
    /// Dummy DualShock command command
    DsDummyCommand,
    /// Unknown command 0x46
    DsMystery46,
    /// Unknown command 0x47
    DsMystery47,
    /// Unknown command 0x48
    DsMystery48,
    /// Unknown command 0x4c
    DsMystery4c,
    /// Rumble configuration command (doesn't actually start the rumble, just enables it)
    DsRumbleConfig,
}
