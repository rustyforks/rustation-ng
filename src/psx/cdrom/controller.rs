//! Implementation of the CD drive microcontroller

use super::disc;
use super::disc::Disc;
use super::simple_rand::SimpleRand;
use super::Fifo;
use crate::psx::{CycleCount, Psx};

use cdimage::msf::Msf;
use cdimage::sector::Sector;

type AsyncResponse = fn(&mut Psx) -> CycleCount;

/// Controller state. This is an 8bit controller in charge of the disc drive.
pub struct Controller {
    /// Currently loaded disc or None if the tray is empty
    disc: Option<Disc>,
    /// Controller state machine
    sequence: ControllerSequence,
    /// Countdown until the next step in `sequence`
    sequence_timer: CycleCount,
    /// Internal command parameter FIFO
    params: Fifo,
    /// Internal command response FIFO
    response: Fifo,
    /// Status for the current command
    irq_code: IrqCode,
    /// Async command response. The tuple contains a method pointer to
    /// the asynchronous command handler and the number of CPU cycles
    /// until the asynch handler must be run.
    async_response: Option<(CycleCount, AsyncResponse)>,
    /// Variable holding the CD read state
    read_state: ReadState,
    /// True if a sector has been read but not yet notified
    read_pending: bool,
    /// Target of the next seek command
    seek_target: Msf,
    /// True if `seek_target` has been set but no seek took place
    seek_target_pending: bool,
    /// Current read position
    position: Msf,
    /// If true the drive is in double speed mode (2x, 150 sectors per second), otherwise we're in
    /// the default 1x (75 sectors per second).
    double_speed: bool,
    /// If true Send ADPCM samples to the SPU
    xa_adpcm_to_spu: bool,
    /// If true we read the whole sector except for the sync bytes (0x924 bytes), otherwise it only
    /// reads 0x800 bytes.
    read_whole_sector: bool,
    /// Not sure what this does exactly, apparently it overrides the normal sector size. Needs to
    /// run more tests to see what it does exactly.
    sector_size_override: bool,
    /// Enable CD-DA mode to play Redbook Audio tracks
    cdda_mode: bool,
    /// If true automatically pause at the end of the track
    autopause: bool,
    /// If true the controller will generate interrupts for each sector while playing CD-DA tracks.
    /// The response will contain the current location amongst other things.
    report_interrupts: bool,
    /// True if the ADPCM filter is enabled
    filter_enabled: bool,
    /// PRNG to simulate the pseudo-random CD controller timings (from the host's perspective)
    rand: SimpleRand,
    /// Last raw sector read from the disc image
    sector: Sector,
}

impl Controller {
    pub fn new(disc: Option<Disc>) -> Controller {
        Controller {
            disc,
            read_pending: false,
            read_state: ReadState::Idle,
            sequence: ControllerSequence::Idle,
            sequence_timer: 0,
            params: Fifo::new(),
            response: Fifo::new(),
            irq_code: IrqCode::Ok,
            async_response: None,
            seek_target: Msf::zero(),
            seek_target_pending: false,
            position: Msf::zero(),
            double_speed: false,
            xa_adpcm_to_spu: false,
            read_whole_sector: true,
            sector_size_override: false,
            cdda_mode: false,
            autopause: false,
            report_interrupts: false,
            filter_enabled: false,
            rand: SimpleRand::new(),
            sector: Sector::empty(),
        }
    }

    /// Assembles the first status byte returned by many commands
    fn drive_status(&self) -> u8 {
        if self.disc.is_none() {
            // No disc, pretend that the shell is open (bit 4)
            return 0x10;
        }

        // XXX on the real hardware bit 4 is always set the first time this command is called
        // even if the console is booted with the tray closed. Using the "get_stat" command
        // command clears it however.
        let mut r = 0;

        let reading = !self.read_state.is_idle();

        // Motor on
        r |= 1 << 1;
        r |= (reading as u8) << 5;

        r
    }

    /// Push the drive status in the response FIFO
    fn push_drive_status(&mut self) {
        let status = self.drive_status();
        self.response.push(status);
    }

    pub fn start_command(&mut self) {
        debug_assert!(!self.in_command());

        if self.async_command_pending() {
            // Not sure what's supposed to happen here, might be command dependant. Can't really
            // see why anybody would want to start a new command without waiting for the response
            // to the previous one though.
            panic!("New CD command while still waiting for an async response");
        }

        self.sequence = ControllerSequence::CommandPending;
        self.params.clear();
        self.response.clear();

        // Let's compute the seemingly pseudo-random command pending delay. Of course in reality
        // the delay is not truly random, it depends on the controller's internal state but since
        // we don't LLE it this is the best we can do right now.
        let delta = self.rand.next() as CycleCount % timings::COMMAND_PENDING_VARIATION;

        self.sequence_timer = timings::COMMAND_PENDING + delta;

        // Assume the command will be succesful, let the command
        // handler override that if something goes wrong.
        self.irq_code = IrqCode::Ok;
    }

    pub fn schedule_async_response(&mut self, delay: CycleCount, handler: AsyncResponse) {
        assert!(self.async_response.is_none());

        self.async_response = Some((delay, handler));
    }

    /// Return true if the sub-CPU is executing a command
    pub fn in_command(&self) -> bool {
        self.sequence != ControllerSequence::Idle
    }

    /// Return true if an async command is pending
    pub fn async_command_pending(&self) -> bool {
        self.async_response.is_some()
    }

    /// Busy flag state. This is *not* equivalent to `in_command()`, the busy flag goes down
    /// roughly 2000 cyles before the IRQ triggers.
    pub fn is_busy(&self) -> bool {
        match self.sequence {
            ControllerSequence::CommandPending
            | ControllerSequence::ParamPush
            | ControllerSequence::Execution
            | ControllerSequence::RxFlush
            | ControllerSequence::RxPush
            | ControllerSequence::BusyDelay => true,
            _ => false,
        }
    }

    pub fn get_async_response(&self) -> Option<(CycleCount, AsyncResponse)> {
        self.async_response
    }

    pub fn clear_response(&mut self) {
        self.async_response = None;
        self.response.clear();

        // Assume the next command is going to be successful, the handler is free to override this
        // value if it's not
        self.irq_code = IrqCode::AsyncOk;
    }

    pub fn set_sector_ready(&mut self) {
        self.irq_code = IrqCode::SectorReady;

        self.response.clear();
        self.response.push(self.drive_status());

        self.sequence = ControllerSequence::AsyncRxPush;
        self.sequence_timer = timings::READ_RX_PUSH;
        self.read_pending = false;
    }

    /// Execute a pending seek (if any). On the real console that would mean physically moving the
    /// read head.
    fn do_seek(&mut self) {
        // Make sure we don't end up in track1's pregap, I don't know
        // if it's ever useful? Needs special handling at least...
        if self.seek_target < Msf::from_bcd(0x00, 0x02, 0x00).unwrap() {
            panic!("Seek to track 1 pregap: {}", self.seek_target);
        }

        self.position = self.seek_target;
        self.seek_target_pending = false;
    }

    /// Return the number of CPU cycles needed to read a single sector depending on the current
    /// drive speed. The PSX drive can read 75 sectors per second at 1x or 150sectors per second at
    /// 2x.
    fn cycles_per_sector(&self) -> CycleCount {
        // 1x speed: 75 sectors per second
        let cycles_1x = crate::psx::cpu::CPU_FREQ_HZ / 75;

        cycles_1x >> (self.double_speed as CycleCount)
    }

    /// Called when a new sector must be read
    fn read_sector(&mut self) {
        if self.read_pending {
            panic!("Sector read while previous one is still pending");
        }

        let position = self.position;

        // Read the sector at `position`
        let sector = match self.disc {
            Some(ref mut d) => d.image().read_sector(position),
            None => panic!("Sector read without a disc"),
        };

        self.sector = match sector {
            Ok(s) => s,
            Err(e) => panic!("Couldn't read sector {}: {}", position, e),
        };

        // Move on to the next segment.
        // XXX what happens when we're at the last one?
        self.position = match self.position.next() {
            Some(m) => m,
            None => panic!("MSF overflow!"),
        };

        self.read_pending = true;
    }

    pub fn sector_data(&mut self) -> &[u8] {
        // Extract the data we need from the sector.
        if self.read_whole_sector {
            // Read the entire sector except for the 12bits sync pattern

            let data = match self.sector.data_2352() {
                Ok(d) => d,
                Err(e) => panic!("Failed to read whole sector {}", e),
            };

            // Skip the sync pattern
            &data[12..]
        } else {
            // Read 2048 bytes after the Mode2 XA sub-header
            let data = match self.sector.mode2_xa_payload() {
                Ok(d) => d as &[u8],
                Err(e) => panic!("Failed to read sector: {}", e),
            };

            if data.len() > 2048 {
                // This is a Mode 2 Form 2 sector, it has more
                // data and no error correction. It probably
                // shouldn't be read without
                // `read_whole_sector` being set.
                warn!("Form 2 sector partial read");
            }

            &data[0..2048]
        }
    }
}

pub fn run(psx: &mut Psx, mut cycles: CycleCount) {
    while cycles > 0 {
        let elapsed = if psx.cdrom.controller.in_command() {
            if psx.cdrom.controller.sequence_timer > cycles {
                // We haven't reached the next step yet
                psx.cdrom.controller.sequence_timer -= cycles;
                cycles
            } else {
                // We reached the end of the current step, advance until the beginning of the next
                // one.
                let to_next_step = psx.cdrom.controller.sequence_timer;
                next_controller_step(psx);
                to_next_step
            }
        } else {
            // No command pending, we can go through the entire delta at once
            cycles
        };

        // We have to step the async events alongside the command
        // sequence since commands can spawn async responses.
        if let Some((delay, handler)) = psx.cdrom.controller.async_response {
            if delay > elapsed {
                psx.cdrom.controller.async_response = Some((delay - elapsed, handler));
            } else {
                // The async event is ready to be processed
                psx.cdrom.controller.async_response = Some((0, handler));
                maybe_process_async_response(psx);
            }
        }

        // Check for sector reads
        if let ReadState::Reading(delay) = psx.cdrom.controller.read_state {
            let new_state = if delay > elapsed {
                ReadState::Reading(delay - elapsed)
            } else {
                let leftover = elapsed - delay;

                // Read the current sector
                psx.cdrom.controller.read_sector();
                // XXX when is the data made available in the RX FIFO exactly?
                psx.cdrom.refresh_rx_data();
                maybe_notify_read(psx);

                // Schedule the next sector read
                let next = psx.cdrom.controller.cycles_per_sector() - leftover;

                // If this triggers something very fishy is happening, it means that we missed
                // a sync and ran very far ahead
                debug_assert!(next > 0);

                ReadState::Reading(next)
            };

            psx.cdrom.controller.read_state = new_state;
        }

        cycles -= elapsed;
    }
}

pub fn predict_next_sync(psx: &mut Psx) -> CycleCount {
    let controller = &mut psx.cdrom.controller;

    // Start with an arbitrarily large value. This value will be used if no event is pending.
    let mut next_sync = 1_000_000;

    // Check for command completion
    if controller.in_command() && controller.sequence_timer < next_sync {
        next_sync = controller.sequence_timer;
    }

    // Check for async response
    if let Some((delay, _)) = controller.async_response {
        if delay < next_sync {
            next_sync = delay;
        }
    }

    // Check for sector read
    if let ReadState::Reading(delay) = controller.read_state {
        if delay < next_sync {
            next_sync = delay;
        }
    }

    next_sync
}

fn next_controller_step(psx: &mut Psx) {
    let (next_step, duration) = match psx.cdrom.controller.sequence {
        // We shouldn't be called if we're idle
        ControllerSequence::Idle => unreachable!(),
        ControllerSequence::CommandPending | ControllerSequence::ParamPush => {
            if psx.cdrom.host_params.is_empty() {
                // No parameters left to transmit from the host FIFO, we can run the command

                // If we got to this point `command` cannot be None, or something very weird is
                // going on
                let command = psx.cdrom.command.unwrap();
                execute_command(psx, command);
                (ControllerSequence::Execution, timings::EXECUTION)
            } else {
                // We have parameters left to send
                let param = psx.cdrom.host_params.pop();
                psx.cdrom.controller.params.push(param);

                (ControllerSequence::ParamPush, timings::PARAM_PUSH)
            }
        }
        ControllerSequence::Execution => {
            psx.cdrom.host_response.clear();
            (ControllerSequence::RxFlush, timings::RX_FLUSH)
        }
        ControllerSequence::RxFlush | ControllerSequence::RxPush => {
            // We know that there is always at least one response byte for any command so we
            // can run this unconditionally after `RxFlush`
            let b = psx.cdrom.controller.response.pop();
            psx.cdrom.host_response.push(b);

            if psx.cdrom.controller.response.is_empty() {
                (ControllerSequence::BusyDelay, timings::BUSY_DELAY)
            } else {
                // We have response bytes left to push
                (ControllerSequence::RxPush, timings::RX_PUSH)
            }
        }
        ControllerSequence::BusyDelay => (ControllerSequence::IrqDelay, timings::IRQ_DELAY),
        ControllerSequence::IrqDelay => {
            psx.cdrom.command = None;
            let irq_code = psx.cdrom.controller.irq_code;

            super::set_controller_irq(psx, irq_code);

            (ControllerSequence::Idle, 0)
        }
        ControllerSequence::AsyncRxPush => {
            let b = psx.cdrom.controller.response.pop();
            psx.cdrom.host_response.push(b);

            if psx.cdrom.controller.response.is_empty() {
                // No busy flag for async transfer, we move on directly to the IRQ delay
                (ControllerSequence::IrqDelay, timings::IRQ_DELAY)
            } else {
                (ControllerSequence::AsyncRxPush, timings::RX_PUSH)
            }
        }
    };

    psx.cdrom.controller.sequence = next_step;
    psx.cdrom.controller.sequence_timer = duration;
}

/// Attempt to start a new command
pub fn maybe_start_command(psx: &mut Psx) -> bool {
    let controller = &mut psx.cdrom.controller;

    if controller.in_command() {
        // We're already running a command
        return false;
    }

    controller.start_command();
    true
}

fn execute_command(psx: &mut Psx, command: u8) {
    let (min_param, max_param, handler): (u8, u8, fn(&mut Psx)) = match command {
        0x01 => (0, 0, commands::get_stat),
        0x02 => (3, 3, commands::set_loc),
        // ReadN
        0x06 => (0, 0, commands::read),
        0x09 => (0, 0, commands::pause),
        0x0a => (0, 0, commands::init),
        0x0e => (1, 1, commands::set_mode),
        0x15 => (0, 0, commands::seek_l),
        0x19 => (1, 1, commands::test),
        0x1a => (0, 0, commands::get_id),
        0x1e => (0, 0, commands::read_toc),
        c => unimplemented!("Unhandled CD command 0x{:02x}", c),
    };

    let nparams = psx.cdrom.controller.params.len();

    if nparams < min_param || nparams > max_param {
        panic!(
            "Wrong number of parameters for CD command 0x{:02x}: {}",
            command, nparams
        );
    }

    handler(psx);
}

pub fn maybe_process_async_response(psx: &mut Psx) {
    if let Some((0, handler)) = psx.cdrom.controller.get_async_response() {
        // The async response is ready, see if the controller is ready to process it
        if !psx.cdrom.controller.in_command() {
            // We can run the response sequence
            psx.cdrom.controller.clear_response();

            let rx_delay = handler(psx);

            psx.cdrom.controller.sequence = ControllerSequence::AsyncRxPush;
            psx.cdrom.controller.sequence_timer = rx_delay;
        }
    }
}

pub fn maybe_notify_read(psx: &mut Psx) {
    let controller = &mut psx.cdrom.controller;

    if !controller.read_pending {
        return;
    }

    if controller.in_command() {
        return;
    }

    // We can notify the read
    controller.set_sector_ready();
}

/// Description of the Controller processing sequence
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
enum ControllerSequence {
    /// Sub-CPU waits for commands and async events
    Idle,
    /// Command pending, wait for the controller to start the
    /// execution.
    CommandPending,
    /// Parameter transfer
    ParamPush,
    /// Command is executed
    Execution,
    /// Response FIFO is cleared
    RxFlush,
    /// Response transfer
    RxPush,
    /// Busy flag goes down at the end of this step
    BusyDelay,
    /// IRQ is triggered at the end of this step
    IrqDelay,
    /// Async response transfer
    AsyncRxPush,
}

/// Various IRQ codes used by the controller
#[derive(Clone, Copy, Debug)]
pub enum IrqCode {
    /// A CD sector has been read and is ready to be processed.
    SectorReady = 1,
    /// Command succesful, 2nd response.
    AsyncOk = 2,
    /// Command succesful, used for the 1st response.
    Ok = 3,
    /// Error: invalid command, disc command while do disc is present
    /// etc...
    Error = 5,
}

/// CDROM disc state
enum ReadState {
    Idle,
    /// We're expecting a sector
    Reading(CycleCount),
}

impl ReadState {
    fn is_idle(&self) -> bool {
        match *self {
            ReadState::Idle => true,
            _ => false,
        }
    }
}

mod commands {
    //! The various CD-ROM commands

    use super::disc::Region;
    use super::{timings, CycleCount, IrqCode, Msf, Psx, ReadState};

    fn pop_param(psx: &mut Psx) -> u8 {
        debug_assert!(!psx.cdrom.controller.params.is_empty());
        psx.cdrom.controller.params.pop()
    }

    fn push_response(psx: &mut Psx, b: u8) {
        debug_assert!(!psx.cdrom.controller.response.is_full());
        psx.cdrom.controller.response.push(b);
    }

    /// Read the drive's status byte
    pub fn get_stat(psx: &mut Psx) {
        let status = psx.cdrom.controller.drive_status();

        push_response(psx, status);
    }

    /// Start data read sequence. This is the implementation for both ReadN and ReadS, apparently
    /// the only difference between the two is that ReadN will retry in case of an error while
    /// ReadS will continue to the next sector (useful for streaming audio/movies). In our emulator
    /// for now we'll just pretend no error ever occurs. Are there any reasons to simulate errors?
    /// Maybe for some copy-protection schemes?
    pub fn read(psx: &mut Psx) {
        let controller = &mut psx.cdrom.controller;

        if !controller.read_state.is_idle() {
            warn!("CDROM READ while we're already reading");
        }

        if controller.seek_target_pending {
            // XXX That should take some time...
            controller.do_seek();
        }

        let read_delay = controller.cycles_per_sector();

        controller.read_state = ReadState::Reading(read_delay);

        controller.push_drive_status();
    }

    /// Stop reading sectors but remain at the same position on the disc
    pub fn pause(psx: &mut Psx) {
        let controller = &mut psx.cdrom.controller;

        let async_delay = if controller.read_state.is_idle() {
            warn!("CD pause when we're not reading");
            9000
        } else {
            // XXX Very very rough approximation, can change based on many factors. Need to
            // come up with a more accurate heuristic
            1_000_000
        };

        controller.read_state = ReadState::Idle;

        controller.schedule_async_response(async_delay, async_pause);

        controller.push_drive_status();
    }

    fn async_pause(psx: &mut Psx) -> CycleCount {
        psx.cdrom.controller.push_drive_status();

        timings::PAUSE_RX_PUSH
    }

    /// Reinitialize the CD ROM controller
    pub fn init(psx: &mut Psx) {
        let controller = &mut psx.cdrom.controller;

        // XXX I think? Needs testing
        controller.read_state = ReadState::Idle;
        controller.read_pending = false;

        controller.schedule_async_response(900_000, async_init);

        controller.push_drive_status();
    }

    fn async_init(psx: &mut Psx) -> CycleCount {
        let controller = &mut psx.cdrom.controller;

        controller.position = Msf::zero();
        controller.seek_target = Msf::zero();
        controller.read_state = ReadState::Idle;
        controller.double_speed = false;
        controller.xa_adpcm_to_spu = false;
        controller.read_whole_sector = true;
        controller.sector_size_override = false;
        controller.filter_enabled = false;
        controller.report_interrupts = false;
        controller.autopause = false;
        controller.cdda_mode = false;

        controller.push_drive_status();

        timings::INIT_RX_PUSH
    }

    /// Tell the CDROM controller where the next seek should take us (but do not physically perform
    /// the seek yet)
    pub fn set_loc(psx: &mut Psx) {
        // Parameters are in BCD.
        let m = pop_param(psx);
        let s = pop_param(psx);
        let f = pop_param(psx);

        let controller = &mut psx.cdrom.controller;

        controller.seek_target = match Msf::from_bcd(m, s, f) {
            Some(m) => m,
            // XXX: what happens if invalid BCD is used?
            None => panic!("Invalid MSF in set loc: {:02x}:{:02x}:{:02x}", m, s, f),
        };

        controller.seek_target_pending = true;

        controller.push_drive_status();
    }

    /// Configure the behaviour of the CDROM drive
    pub fn set_mode(psx: &mut Psx) {
        let mode = pop_param(psx);

        let controller = &mut psx.cdrom.controller;

        controller.double_speed = (mode >> 7) & 1 != 0;
        controller.xa_adpcm_to_spu = (mode >> 6) & 1 != 0;
        controller.read_whole_sector = (mode >> 5) & 1 != 0;
        controller.sector_size_override = (mode >> 4) & 1 != 0;
        controller.filter_enabled = (mode >> 3) & 1 != 0;
        controller.report_interrupts = (mode >> 2) & 1 != 0;
        controller.autopause = (mode >> 1) & 1 != 0;
        controller.cdda_mode = mode & 1 != 0;

        if controller.cdda_mode
            || controller.autopause
            || controller.report_interrupts
            || controller.sector_size_override
        {
            unimplemented!("CDROM: unhandled mode: {:02x}", mode);
        }

        controller.push_drive_status();
    }

    /// Execute seek. Target is given by previous "set_loc" command.
    pub fn seek_l(psx: &mut Psx) {
        let controller = &mut psx.cdrom.controller;

        controller.do_seek();

        controller.push_drive_status();

        // XXX the delay for the async response is tied to the time it takes for the reading head
        // to physically seek on the disc. We probably need a heuristic based on the current head
        // position, target position and probably a bunch of other factors. For now hardcode a dumb
        // value and hope for the best.
        controller.schedule_async_response(1_000_000, async_seek_l);
    }

    fn async_seek_l(psx: &mut Psx) -> CycleCount {
        let controller = &mut psx.cdrom.controller;

        let status = controller.drive_status();

        controller.response.push(status);

        timings::SEEK_L_RX_PUSH
    }

    /// The test command can do a whole bunch of stuff, the first parameter says what
    pub fn test(psx: &mut Psx) {
        match pop_param(psx) {
            0x20 => test_version(psx),
            n => unimplemented!("Unhandled CD test subcommand 0x{:x}", n),
        }
    }

    fn test_version(psx: &mut Psx) {
        // Values returned by my PAL SCPH-7502 console:
        push_response(psx, 0x98); // Year
        push_response(psx, 0x06); // Month
        push_response(psx, 0x10); // Day
        push_response(psx, 0xc3); // Version
    }

    /// Read the CD-ROM's identification string. This is how the BIOS checks that the disc is an
    /// official PlayStation disc (and not a copy) and handles region locking.
    pub fn get_id(psx: &mut Psx) {
        let controller = &mut psx.cdrom.controller;

        if controller.disc.is_some() {
            controller.push_drive_status();

            controller.schedule_async_response(timings::GET_ID_ASYNC, async_get_id);
        } else {
            // No disc, pretend the shell is open
            controller.response.push(0x11);
            controller.response.push(0x80);

            controller.irq_code = IrqCode::Error;
        }
    }

    fn async_get_id(psx: &mut Psx) -> CycleCount {
        let controller = &mut psx.cdrom.controller;

        // If we're here we must have a disc
        let disc = controller.disc.as_ref().unwrap();

        let response = [
            // Status + bit 3 if unlicensed/audio
            controller.drive_status(),
            // Licensed, not audio, not missing
            0x00,
            // Disc type (mode2 apparently?)
            0x20,
            // Not sure what this one does. No$ says "8bit
            // ATIP from Point=C0h, if session info exists",
            // not sure what it means. Seems to be 0 for all
            // CDs I've tested...
            0x00,
            // Region string: "SCEI" for japan, "SCEE" for
            // Europe and "SCEA" for US.
            b'S',
            b'C',
            b'E',
            match disc.region() {
                Region::Japan => b'I',
                Region::NorthAmerica => b'A',
                Region::Europe => b'E',
            },
        ];

        controller.response.push_slice(&response);

        timings::GET_ID_RX_PUSH
    }

    /// Instruct the CD drive to read the table of contents
    pub fn read_toc(psx: &mut Psx) {
        let controller = &mut psx.cdrom.controller;

        controller.push_drive_status();

        // XXX should probably stop ReadN/S

        controller.schedule_async_response(timings::READ_TOC_ASYNC, async_read_toc);
    }

    fn async_read_toc(psx: &mut Psx) -> CycleCount {
        let controller = &mut psx.cdrom.controller;

        controller.push_drive_status();

        timings::READ_TOC_RX_PUSH
    }
}

mod timings {
    //! CD controller timings, expressed in CPU clock cycles.
    //!
    //! Most of those timings are rough approximations, there can be massive variations in
    //! execution time on the real hardware, probably because the controller runs its own
    //! asynchronous main loop and has to handle all the events from the physical drive at the same
    //! time it processes the host commands.

    use super::CycleCount;

    /// Delay between the moment a command starts being processed and the parameter transfer
    /// sequence. This delay is *extremely* variable on the real hardware (more so than other CD
    /// timings) so this is a low bound (it's possible to have even shorter times on the real
    /// hardware but they are quite uncommon)
    pub const COMMAND_PENDING: CycleCount = 9_400;

    /// Most command pending times are between COMMAND_PENDING and (COMMAND_PENDING +
    /// COMMAND_PENDING_VARIATION). Shorter and longer delays are possible on the real hardware but
    /// they're uncommon.
    pub const COMMAND_PENDING_VARIATION: CycleCount = 6_000;

    /// Approximate duration of a single parameter transfer
    pub const PARAM_PUSH: CycleCount = 1_800;

    /// Delay between the end of the last parameter push and the RX FIFO clear. I assume that's
    /// when the actual command execution takes place but I haven't tested it thoroughly. It
    /// shouldn't really matter anyway.
    pub const EXECUTION: CycleCount = 2_000;

    /// Delay between the RX FIFO clear and the first response byte being pushed onto the RX FIFO.
    pub const RX_FLUSH: CycleCount = 3_500;

    /// Time taken by each additional response bytes
    pub const RX_PUSH: CycleCount = 1_500;

    /// Delay between the moment the last response byte gets pushed and the moment the busy flag
    /// goes low.
    pub const BUSY_DELAY: CycleCount = 3_300;

    /// Delay between the moment the busy flag goes low and the moment the IRQ triggers OR in the
    /// case of async events the delay between the last response push and the IRQ
    pub const IRQ_DELAY: CycleCount = 2_000;

    /// Delay between GetId command execution and the asyncronous RX_CLEAR
    pub const GET_ID_ASYNC: CycleCount = 15_000;

    /// Delay between the asynchronous RX_CLEAR and first param push for the asynchronous GetId
    /// response
    pub const GET_ID_RX_PUSH: CycleCount = 3_100;

    /// Very rough estimate of the time taken to read the table of content. On my console it takes
    /// around 1 second (and probably varies depending on the disc. I'll just use ~0.5s for the
    /// emulator, I doubt it matters much in practice.
    pub const READ_TOC_ASYNC: CycleCount = 16_000_000;

    /// Delay between the asynchronous RX_CLEAR and first param push for the asynchronous ReadToc
    /// response
    pub const READ_TOC_RX_PUSH: CycleCount = 1_700;

    /// Delay between the asynchronous RX_CLEAR and first param push for the asynchronous SeekL
    /// response
    pub const SEEK_L_RX_PUSH: CycleCount = 1_700;

    /// Delay between the asynchronous RX_CLEAR and first param push for the asynchronous Read(S/N)
    /// response
    pub const READ_RX_PUSH: CycleCount = 1_800;

    /// Delay between the asynchronous RX_CLEAR and first param push for the asynchronous Pause
    /// response
    pub const PAUSE_RX_PUSH: CycleCount = 1_700;

    /// Delay between the asynchronous RX_CLEAR and first param push for the asynchronous Init
    /// response
    pub const INIT_RX_PUSH: CycleCount = 1_700;
}
