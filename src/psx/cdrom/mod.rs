//! CD-ROM interface
//!
//! The PlayStation uses an external controller for decoding and correcting CD sectors. This
//! controller is similar to the CXD1199AQ whose datasheet is available online. I try to use the
//! symbolic names defined in this datasheet where it makes sense.
//!
//! This controller communicates asynchronously with a microcontroller handling actual CD-ROM drive
//! (called the "sub-CPU" in the CXD1199AQ datasheet).
//!
//! Since you can't access the sub-CPU directly from the main CPU it's pretty difficult to
//! reverse-engineer what's going on exactly without using an oscilloscope. As a result this
//! implementation is based on No$'s specs, mednafen's source code and some educated guesses.

mod controller;
pub mod disc;
mod fifo;
pub mod iso9660;
mod simple_rand;

use self::controller::Controller;
use self::disc::Disc;
use self::fifo::Fifo;

use super::{irq, sync, Addressable, Psx};

const CDROMSYNC: sync::SyncToken = sync::SyncToken::CdRom;

pub struct CdRom {
    /// The CD-ROM interface has four memory-mapped registers. The first one contains an index
    /// which defines the meaning of the three others.
    index: u8,
    /// Command parameter FIFO
    host_params: Fifo,
    /// Command response FIFO
    host_response: Fifo,
    /// Pending command number, if any
    command: Option<u8>,
    /// Interrupt flag (5 bits). The low 3 bits are the sub-CPU interrupt code. XXX What about the
    /// two upper bits?
    irq_flags: u8,
    /// Interrupt mask (5 bits)
    irq_mask: u8,
    /// Data RX buffer
    rx_buffer: Box<[u8; 2352]>,
    /// Number of valid bytes in the buffer
    rx_len: u16,
    /// Read position within the buffer
    rx_index: u16,
    /// This bit is set when the program wants to read sector data. It's automatically cleared when
    /// all the sector has been read but it can also be cleared by writing to the config register
    /// directly.
    rx_active: bool,
    /// The drive controller itself, running on a separate chip.
    controller: Controller,
}

impl CdRom {
    pub fn new(disc: Option<Disc>) -> CdRom {
        CdRom {
            index: 0,
            host_params: Fifo::new(),
            host_response: Fifo::new(),
            command: None,
            irq_flags: 0,
            irq_mask: 0,
            rx_buffer: box_array![0; 2352],
            rx_len: 0,
            rx_index: 0,
            rx_active: false,
            controller: Controller::new(disc),
        }
    }

    fn set_command(&mut self, cmd: u8) {
        if self.command.is_some() {
            // XXX What should we do here?
            panic!("Nested CDC command!");
        }

        self.command = Some(cmd);
    }

    fn push_parameter(&mut self, param: u8) {
        // TODO: what if we're already running a command?
        if self.host_params.is_full() {
            warn!("CDROM param FIFO overflow!");
        }

        self.host_params.push(param);
    }

    fn host_status(&self) -> u8 {
        let mut r = 0;

        r |= self.index;

        // TODO: ADPCM busy (ADPBUSY)
        r |= 0 << 2;
        // Parameter empty (PRMEMPT)
        r |= (self.host_params.is_empty() as u8) << 3;
        // Parameter write ready (PRMWRDY)
        r |= (!self.host_params.is_full() as u8) << 4;
        // Result read ready (RSLRRDY)
        r |= (!self.host_response.is_empty() as u8) << 5;

        // Data request status (DRQSTS)
        let data_available = self.rx_index < self.rx_len;
        r |= (data_available as u8) << 6;

        // "Busy" flag (BUSYSTS).
        //
        // The CXD1199AQ datasheet says it's set high when we write to the command register and
        // it's cleared when the sub-CPU asserts the CLRBUSY signal.
        r |= (self.controller.is_busy() as u8) << 7;

        r
    }

    /// Refill the RX buffer from the controller
    fn refresh_rx_data(&mut self) {
        let data = self.controller.sector_data();

        for (i, &b) in data.iter().enumerate() {
            self.rx_buffer[i] = b;
        }

        self.rx_len = data.len() as u16;
    }

    /// Returns `true` if the IRQ signal is currently asserted
    fn irq(&self) -> bool {
        self.irq_flags & self.irq_mask != 0
    }

    fn set_host_chip_control(&mut self, ctrl: u8) {
        let prev_active = self.rx_active;

        self.rx_active = ctrl & 0x80 != 0;

        if self.rx_active {
            if !prev_active {
                // Reset the index to the beginning of the RX buffer
                self.rx_index = 0;
            }
        } else {
            // It seems that on the real hardware when one attempts to read the RX data register
            // while the rx_active bit is low it returns always the same bytes which seems to be
            // located at the *closest* multiple of 8 bytes. I think there's an 8byte buffer behind
            // this register somewhere.
            //
            // I also observe that if I wait too long and a new sector gets read while I'm in the
            // middle of the previous one I can still read the previous sector data up to the next
            // 8byte boundary (need to make more intensive checks). Not that it should matter
            // anyway, it's still garbage as far as the software is concerned.

            // Align to the next multiple of 8bytes
            let i = self.rx_index;

            let adjust = (i & 4) << 1;

            self.rx_index = (i & !7) + adjust
        }

        if ctrl & 0x7f != 0 {
            unimplemented!("CDROM: unhandled HCHPCTL {:02x}", ctrl);
        }
    }

    fn read_byte(&mut self) -> u8 {
        let b = self.rx_buffer[self.rx_index as usize];

        if self.rx_active {
            self.rx_index += 1;

            if self.rx_index == self.rx_len {
                // rx_active clears automatically at the end of the
                // transfer
                self.rx_active = false;
            }
        } else {
            unimplemented!("read byte while !rx_active");
        }

        b
    }

    fn read_word(&mut self) -> u32 {
        let b0 = self.read_byte() as u32;
        let b1 = self.read_byte() as u32;
        let b2 = self.read_byte() as u32;
        let b3 = self.read_byte() as u32;

        // Pack in a little endian word
        b0 | (b1 << 8) | (b2 << 16) | (b3 << 24)
    }
}

fn run_cd(psx: &mut Psx) {
    let cycles = sync::resync(psx, CDROMSYNC);

    controller::run(psx, cycles);
}

pub fn run(psx: &mut Psx) {
    run_cd(psx);
    predict_next_sync(psx);
}

pub fn predict_next_sync(psx: &mut Psx) {
    let next_event = controller::predict_next_sync(psx);
    sync::next_event(psx, CDROMSYNC, next_event);
}

pub fn store<T: Addressable>(psx: &mut Psx, off: u32, val: T) {
    let v = val.as_u8();

    if off == 0 {
        psx.cdrom.index = (v & 3) as u8;
        return;
    }

    run_cd(psx);

    match off {
        1 => match psx.cdrom.index {
            0 => {
                psx.cdrom.set_command(v);
                maybe_start_command(psx);
            }
            _ => unimplemented!(
                "Store to CD register {}.{}: 0x{:x}",
                off,
                psx.cdrom.index,
                val.as_u32()
            ),
        },
        2 => match psx.cdrom.index {
            0 => psx.cdrom.push_parameter(v),
            1 => irq_set_mask(psx, v),
            _ => unimplemented!(
                "Store to CD register {}.{}: 0x{:x}",
                off,
                psx.cdrom.index,
                val.as_u32()
            ),
        },
        3 => match psx.cdrom.index {
            0 => psx.cdrom.set_host_chip_control(v),
            // HCLRCTL (host clear control) register
            1 => {
                irq_ack(psx, v & 0x1f);

                if v & 0x40 != 0 {
                    psx.cdrom.host_params.clear();
                }

                if v & 0xa0 != 0 {
                    unimplemented!("HCLRCTL {:x}", v);
                }
            }
            _ => unimplemented!(
                "Store to CD register {}.{}: 0x{:x}",
                off,
                psx.cdrom.index,
                val.as_u32()
            ),
        },
        _ => unimplemented!(
            "Store to CD register {}.{}: 0x{:x}",
            off,
            psx.cdrom.index,
            val.as_u32()
        ),
    }

    predict_next_sync(psx);
}

pub fn load<T: Addressable>(psx: &mut Psx, off: u32) -> T {
    // XXX Mefnafen doesn't call `run` here, should we?
    let v = match off {
        0 => psx.cdrom.host_status(),
        1 => {
            // RESULT register. The CXD1199AQ datasheet says that the response FIFO is 8-byte long,
            // however the PSX seems to be 16bytes (at least it seems to wrap around at the 16th
            // read). May be a small upgrade to the IP in order to support commands like GetQ which
            // return more than 8 response bytes.

            if psx.cdrom.host_response.is_empty() {
                panic!("CDROM response FIFO underflow");
            }

            psx.cdrom.host_response.pop()
        }
        3 => match psx.cdrom.index {
            0 => psx.cdrom.irq_mask | 0xe0,
            1 => psx.cdrom.irq_flags | 0xe0,
            _ => unimplemented!("Read from CD register {}.{}", off, psx.cdrom.index),
        },
        _ => unimplemented!("Read from CD register {}.{}", off, psx.cdrom.index),
    };

    T::from_u32(u32::from(v))
}

/// Called by the DMA when it wants to get our CD data
pub fn dma_load(psx: &mut Psx) -> u32 {
    psx.cdrom.read_word()
}

fn irq_ack(psx: &mut Psx, ack_mask: u8) {
    psx.cdrom.irq_flags &= !ack_mask;

    // Check if a command/async/read event was waiting for the IRQ ack to process.
    //
    // XXX I'm not sure which one would have the priority here, assuming that they're all
    // pending at the same time.
    maybe_start_command(psx);
    maybe_process_async_response(psx);
    maybe_notify_read(psx);
}

fn irq_set_mask(psx: &mut Psx, mask: u8) {
    let irq_was_active = psx.cdrom.irq();

    psx.cdrom.irq_mask = mask;

    if !irq_was_active && psx.cdrom.irq() {
        irq::trigger(psx, irq::Interrupt::CdRom);
    }
}

fn set_controller_irq(psx: &mut Psx, irq: controller::IrqCode) {
    let irq_was_active = psx.cdrom.irq();

    psx.cdrom.irq_flags &= !7;
    psx.cdrom.irq_flags |= irq as u8;

    if !irq_was_active && psx.cdrom.irq() {
        irq::trigger(psx, irq::Interrupt::CdRom);
    }
}

/// See if we can start to process a new command. Returns `true` if a new command has been started.
fn maybe_start_command(psx: &mut Psx) {
    if psx.cdrom.command.is_none() {
        // No pending command, nothing to do
        return;
    };

    // We have to wait until all interrupts have been acknowledged before starting a new command.
    if psx.cdrom.irq_flags != 0 {
        // We're not ready
        return;
    }

    // We're good to go, ask the CPU to start the new command if possible
    controller::maybe_start_command(psx);
}

/// Start the async response sequence if an async response is pending and the preconditions are met
fn maybe_process_async_response(psx: &mut Psx) {
    if psx.cdrom.irq_flags == 0 {
        controller::maybe_process_async_response(psx);
    }
}

/// Start the async read notification sequence if a sector read is pending and the
/// preconditions are met
fn maybe_notify_read(psx: &mut Psx) {
    if psx.cdrom.irq_flags == 0 {
        controller::maybe_notify_read(psx);
    }
}
