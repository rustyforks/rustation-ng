//! Gamepad and memory card controller emulation

use super::{irq, sync, AccessWidth, Addressable, CycleCount, Psx};
use irq::Interrupt;

use self::gamepad::GamePad;

pub mod gamepad;

const PADSYNC: sync::SyncToken = sync::SyncToken::PadMemCard;

pub struct PadMemCard {
    /// Serial clock divider. The LSB is read/write but is not used, This way the hardware divide
    /// the CPU clock by half of `baud_div` and can invert the serial clock polarity twice every
    /// `baud_div` which effectively means that the resulting frequency is CPU clock / (`baud_div`
    /// & 0xfe).
    baud_div: u16,
    /// Serial config, not implemented for now...
    mode: u8,
    /// Transmission enabled if true
    tx_en: bool,
    /// If true the targeted peripheral select signal is asserted (the actual signal is active low,
    /// so it's driving low on the controller port when `select` is true). The `target` field says
    /// which peripheral is addressed.
    select: bool,
    /// This bit says which of the two pad/memorycard port pair we're selecting with `select_n`
    /// above. Multitaps are handled at the serial protocol level, not by dedicated hardware pins.
    target: Target,
    /// Control register bits 3 and 5 are read/write but I don't know what they do. I just same
    /// them here for accurate readback.
    unknown: u8,
    /// XXX not sure what this does exactly, forces a read without any TX?
    rx_en: bool,
    /// Data Set Ready signal, active low (driven by the gamepad)
    dsr: bool,
    /// If true an interrupt is generated when a DSR pulse is received from the pad/memory card
    dsr_it: bool,
    /// Current interrupt level
    interrupt: bool,
    /// Current response byte.
    /// XXX Normally it should be a FIFO but I'm not sure how it works really. Besides the game
    /// should check for the response after each byte anyway, so it's probably unused the vast
    /// majority of times.
    response: u8,
    /// True when we the RX FIFO is not empty.
    rx_not_empty: bool,
    /// Gamepad in slot 1
    pad1: GamePad,
    /// Gamepad in slot 2
    pad2: GamePad,
    /// Bus state machine
    bus: BusState,
}

impl PadMemCard {
    pub fn new() -> PadMemCard {
        PadMemCard {
            baud_div: 0,
            mode: 0,
            tx_en: false,
            select: false,
            target: Target::PadMemCard1,
            interrupt: false,
            unknown: 0,
            rx_en: false,
            dsr: false,
            dsr_it: false,
            response: 0xff,
            rx_not_empty: false,
            pad1: GamePad::disconnected(),
            pad2: GamePad::disconnected(),
            bus: BusState::Idle,
        }
    }

    /// Return a mutable reference to the gamepad profiles being used.
    pub fn gamepads_mut(&mut self) -> [&mut GamePad; 2] {
        [&mut self.pad1, &mut self.pad2]
    }

    fn send_command(&mut self, cmd: u8) {
        if !self.tx_en {
            // It should be stored in the FIFO and sent when tx_en is
            // set (I think)
            panic!("Unhandled gamepad command while tx_en is disabled");
        }

        if self.bus.is_busy() {
            // I suppose the transfer should be queued in the TX FIFO?
            warn!("Gamepad command {:x} while bus is busy!", cmd);
        }

        let (response, dsr) = if self.select {
            match self.target {
                Target::PadMemCard1 => self.pad1.send_command(cmd),
                Target::PadMemCard2 => self.pad2.send_command(cmd),
            }
        } else {
            // No response
            (0xff, false)
        };

        // XXX Handle `mode` as well, especially the "baudrate reload
        // factor". For now I assume we're sending 8 bits, one every
        // `baud_div` CPU cycles.
        let tx_duration = 8 * self.baud_div as CycleCount;

        self.bus = BusState::Transfer(response, dsr, tx_duration);
    }

    fn get_response(&mut self) -> u8 {
        let res = self.response;

        self.rx_not_empty = false;
        self.response = 0xff;

        res
    }

    fn stat(&self) -> u32 {
        let mut stat = 0u32;

        // TX Ready bits 1 and 2 (Not sure when they go low)
        stat |= 5;
        stat |= (self.rx_not_empty as u32) << 1;
        // RX parity error should always be 0 in our case.
        stat |= 0 << 3;
        stat |= (self.dsr as u32) << 7;
        stat |= (self.interrupt as u32) << 9;
        // XXX needs to add the baudrate counter in bits [31:11];
        stat |= 0 << 11;

        stat
    }

    fn set_mode(&mut self, mode: u8) {
        self.mode = mode;
    }

    fn control(&self) -> u16 {
        let mut ctrl = 0u16;

        ctrl |= self.unknown as u16;

        ctrl |= self.tx_en as u16;
        ctrl |= (self.select as u16) << 1;
        // XXX I assume this flag self-resets? When?
        ctrl |= (self.rx_en as u16) << 2;
        // XXX Add other interrupts when they're implemented
        ctrl |= (self.dsr_it as u16) << 12;
        ctrl |= (self.target as u16) << 13;

        ctrl
    }

    /// Returns `true` if an interrupt should be triggered
    fn set_control(&mut self, ctrl: u16) -> bool {
        let mut irq = false;

        if ctrl & 0x40 != 0 {
            // Soft reset
            self.baud_div = 0;
            self.mode = 0;
            self.select = false;
            self.target = Target::PadMemCard1;
            self.unknown = 0;
            self.interrupt = false;
            self.rx_not_empty = false;
            self.bus = BusState::Idle;
            // XXX since the gamepad/memory card asserts this signal it actually probably shouldn't
            // release here but it'll make our state machine simpler for the time being.
            self.dsr = false;

        // It doesn't seem to reset the contents of the RX FIFO.
        } else {
            if ctrl & 0x10 != 0 {
                // Interrupt acknowledge

                self.interrupt = false;

                if self.dsr && self.dsr_it {
                    // The controller's "dsr_it" interrupt is not edge triggered: as long as
                    // self.dsr && self.dsr_it is true it will keep being triggered. If the
                    // software attempts to acknowledge the interrupt in this state it will
                    // re-trigger immediately which will be seen by the edge-triggered top level
                    // interrupt controller. So I guess this shouldn't happen?
                    warn!("Gamepad interrupt acknowledge while DSR is active");

                    self.interrupt = true;
                    irq = true;
                }
            }

            let prev_select = self.select;

            // No idea what bits 3 and 5 do but they're read/write.
            self.unknown = (ctrl as u8) & 0x28;

            self.tx_en = ctrl & 1 != 0;
            self.select = (ctrl >> 1) & 1 != 0;
            self.rx_en = (ctrl >> 2) & 1 != 0;
            self.dsr_it = (ctrl >> 12) & 1 != 0;
            self.target = Target::from_control(ctrl);

            if self.rx_en {
                panic!("Gamepad rx_en not implemented");
            }

            if self.dsr_it && !self.interrupt && self.dsr {
                // Interrupt should trigger here but that really shouldn't happen I think.
                panic!("dsr_it enabled while DSR signal is active");
            }

            if ctrl & 0xf00 != 0 {
                // XXX add support for those interrupts
                panic!("Unsupported gamepad interrupts: {:04x}", ctrl);
            }

            if !prev_select && self.select {
                // XXX Should probably also check self.target, not sure how it influences the
                // select line. I assume only the targeted slot is selected?
                self.pad1.select();
            }
        }

        irq
    }
}

fn run_controller(psx: &mut Psx) {
    let mut remaining_cycles = sync::resync(psx, PADSYNC);

    while remaining_cycles > 0 {
        let elapsed = match psx.pad_memcard.bus {
            BusState::Idle => remaining_cycles,
            BusState::Transfer(r, dsr, delay) => {
                if remaining_cycles < delay {
                    let delay = delay - remaining_cycles;
                    psx.pad_memcard.bus = BusState::Transfer(r, dsr, delay);

                    remaining_cycles
                } else {
                    // We reached the end of the transfer

                    if psx.pad_memcard.rx_not_empty {
                        // XXX should push in the non-emulated RX FIFO instead of overwriting
                        // `psx.pad_memcard.response`
                        unimplemented!("Gamepad RX while FIFO isn't empty");
                    }

                    psx.pad_memcard.response = r;
                    psx.pad_memcard.rx_not_empty = true;
                    // XXX For now pretend that the DSR pulse follows
                    // immediately after the last byte, probably not accurate.
                    psx.pad_memcard.dsr = dsr;

                    if dsr {
                        if psx.pad_memcard.dsr_it {
                            if !psx.pad_memcard.interrupt {
                                // Rising edge of the interrupt
                                irq::trigger(psx, Interrupt::PadMemCard);
                            }

                            psx.pad_memcard.interrupt = true;
                        }

                        // The DSR pulse is generated purely by the controller without any
                        // input from the console. Therefore the actual length of the pulse
                        // changes from controller to controller. I have two seemingly
                        // identical SCPH-1080 controllers, one pulses the DSR line for ~100CPU
                        // cycles while the other one is slightly faster at around ~90 CPU
                        // cycles.
                        let dsr_duration = 90;
                        psx.pad_memcard.bus = BusState::Dsr(dsr_duration);
                    } else {
                        // We're done with this transaction
                        psx.pad_memcard.bus = BusState::Idle;
                    }

                    delay
                }
            }
            BusState::Dsr(delay) => {
                if remaining_cycles < delay {
                    let delay = delay - remaining_cycles;
                    psx.pad_memcard.bus = BusState::Dsr(delay);

                    remaining_cycles
                } else {
                    // DSR pulse is over, bus is idle
                    psx.pad_memcard.dsr = false;
                    psx.pad_memcard.bus = BusState::Idle;

                    delay
                }
            }
        };

        remaining_cycles -= elapsed;
    }
}

fn predict_next_sync(psx: &mut Psx) {
    let next_event = match psx.pad_memcard.bus {
        // No sync needed, let's just use an arbitrary value
        BusState::Idle => 1_000_000,
        BusState::Transfer(_, _, delay) => delay,
        BusState::Dsr(delay) => delay,
    };

    sync::next_event(psx, PADSYNC, next_event);
}

pub fn run(psx: &mut Psx) {
    run_controller(psx);
    predict_next_sync(psx);
}

pub fn store<T: Addressable>(psx: &mut Psx, off: u32, val: T) {
    run_controller(psx);

    let v = val.as_u16();

    match off {
        0 => {
            if T::width() != AccessWidth::Byte {
                unimplemented!("Gamepad TX access ({:?})", T::width());
            }

            psx.pad_memcard.send_command(val.as_u8());
        }
        8 => psx.pad_memcard.set_mode(val.as_u8()),
        10 => {
            if T::width() == AccessWidth::Byte {
                // Byte access behaves like a halfword
                unimplemented!("Unhandled byte gamepad control access");
            }
            let irq = psx.pad_memcard.set_control(v);
            if irq {
                irq::trigger(psx, Interrupt::PadMemCard);
            }
        }
        14 => psx.pad_memcard.baud_div = v,
        _ => unimplemented!("Write to gamepad register {} {:04x}", off, v),
    }

    predict_next_sync(psx);
}

pub fn load<T: Addressable>(psx: &mut Psx, off: u32) -> T {
    run_controller(psx);

    let v = match off {
        0 => {
            if T::width() != AccessWidth::Byte {
                unimplemented!("Unhandled gamepad RX access ({:?})", T::width());
            }

            u32::from(psx.pad_memcard.get_response())
        }
        4 => psx.pad_memcard.stat(),
        8 => u32::from(psx.pad_memcard.mode),
        10 => u32::from(psx.pad_memcard.control()),
        14 => u32::from(psx.pad_memcard.baud_div),
        _ => unimplemented!("gamepad read {:?} 0x{:x}", T::width(), off),
    };

    predict_next_sync(psx);

    T::from_u32(v)
}

/// Identifies the target of the serial communication, either the gamepad/memory card port 0 or 1.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Target {
    PadMemCard1 = 0,
    PadMemCard2 = 1,
}

impl Target {
    fn from_control(ctrl: u16) -> Target {
        if ctrl & 0x2000 != 0 {
            Target::PadMemCard1
        } else {
            Target::PadMemCard2
        }
    }
}

/// Controller transaction state machine
#[derive(Debug)]
enum BusState {
    /// Bus is idle
    Idle,
    /// Transaction in progress, we store the response byte, the DSR response and the number of
    /// Cycles remaining until we reach the DSR pulse (if any)
    Transfer(u8, bool, CycleCount),
    /// DSR is asserted, count the number of cycles remaining.
    Dsr(CycleCount),
}

impl BusState {
    fn is_busy(&self) -> bool {
        match *self {
            BusState::Idle => false,
            _ => true,
        }
    }
}
