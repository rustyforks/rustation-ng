//! The PlayStation DMA, that can be used to copy data between the RAM and various devices (GPU,
//! CD drive, MDEC, SPU etc...)

use super::{gpu, irq, sync, AccessWidth, Addressable, CycleCount, Psx};
use irq::IrqState;
use std::ops::{Index, IndexMut};

const DMASYNC: sync::SyncToken = sync::SyncToken::Dma;

pub struct Dma {
    control: Control,
    irq_config: IrqConfig,
    /// The 7 DMA channels
    channels: [Channel; 7],
    /// Counter to keep track of our refresh cycle
    period_counter: CycleCount,
}

impl Dma {
    pub fn new() -> Dma {
        Dma {
            control: Control::new(),
            irq_config: IrqConfig::new(),
            channels: [
                Channel::new(),
                Channel::new(),
                Channel::new(),
                Channel::new(),
                Channel::new(),
                Channel::new(),
                Channel::new(),
            ],
            period_counter: 0,
        }
    }
}

impl Dma {
    /// Signal that channel `port` is done running. Returns true if an interrupt should be
    /// triggered
    fn end_of_dma(&mut self, port: Port) -> IrqState {
        self[port].control.stop();

        if self.irq_config.irq_enabled(port) {
            self.irq_config.flag_irq(port)
        } else {
            IrqState::Idle
        }
    }
}

impl Index<Port> for Dma {
    type Output = Channel;

    fn index(&self, port: Port) -> &Self::Output {
        &self.channels[port as usize]
    }
}

impl IndexMut<Port> for Dma {
    fn index_mut(&mut self, port: Port) -> &mut Self::Output {
        &mut self.channels[port as usize]
    }
}

pub fn run(psx: &mut Psx) {
    // XXX We probably should only call that if the GPU channel is active but that will make our
    // timings diverge from mednafen so let's not for the time being.
    gpu::run(psx);

    let elapsed = sync::resync(psx, DMASYNC);

    psx.dma.period_counter += elapsed;
    psx.dma.period_counter %= DMA_REFRESH_PERIOD;

    run_channel(psx, Port::MDecIn, elapsed);
    run_channel(psx, Port::MDecOut, elapsed);
    run_channel(psx, Port::Gpu, elapsed);
    run_channel(psx, Port::CdRom, elapsed);
    run_channel(psx, Port::Spu, elapsed);
    run_channel(psx, Port::Pio, elapsed);
    run_channel(psx, Port::Otc, elapsed);

    refresh_cpu_halt(psx);

    // XXX This is probably heavy handed, we shouldn't have to run the DMA code when it's idle. But
    // if we change this we break timing compatibility with mednafen (because this handler calls
    // `gpu::run` above) so let's keep it that way.
    let next_sync = DMA_REFRESH_PERIOD - psx.dma.period_counter;
    sync::next_event(psx, DMASYNC, next_sync);
}

pub fn load<T: Addressable>(psx: &mut Psx, offset: u32) -> T {
    if T::width() != AccessWidth::Word {
        panic!("Unhandled DMA {:?} access", T::width());
    }

    let _ = psx;

    let channel = (offset & 0x70) >> 4;
    let reg = offset & 0xf;

    let v = match channel {
        0..=6 => {
            // Channel configuration
            let port = Port::from_index(channel);
            let channel = &psx.dma[port];

            match reg {
                0 => channel.base,
                4 => channel.block_control(),
                8 => channel.control.get(),
                _ => unimplemented!("Read from channel {:?} register {:x}", port, reg),
            }
        }
        7 => match reg {
            0 => psx.dma.control.get(),
            4 => psx.dma.irq_config.get(),
            _ => unimplemented!("DMA register read from {:x}", reg),
        },
        _ => unreachable!(),
    };

    T::from_u32(v)
}

pub fn store<T: Addressable>(psx: &mut Psx, offset: u32, val: T) {
    run(psx);

    if T::width() != AccessWidth::Word {
        panic!("Unhandled DMA {:?} access", T::width());
    }

    let channel = (offset & 0x70) >> 4;
    let reg = offset & 0xf;
    let val = val.as_u32();

    match channel {
        0..=6 => {
            // Channel configuration
            let port = Port::from_index(channel);

            match reg {
                0 => psx.dma[port].base = val & 0xff_ffff,
                4 => psx.dma[port].set_block_control(val),
                8 => set_channel_control(psx, port, val),
                _ => unimplemented!("Write to channel {:?} register {:x}: {:x}", port, reg, val),
            }
        }
        7 => match reg {
            0 => {
                psx.dma.control.set(val);
                // XXX Not sure why mednafen calls this here since the DMA control register isn't
                // used. Maybe a leftover of some previous iteration?
                refresh_cpu_halt(psx);
            }
            4 => {
                if psx.dma.irq_config.set(val).is_triggered() {
                    irq::trigger(psx, irq::Interrupt::Dma);
                }
            }
            _ => unimplemented!("DMA register write to {:x}", reg),
        },
        _ => unreachable!(),
    }
}

fn set_channel_control(psx: &mut Psx, port: Port, mut ctrl: u32) {
    if port == Port::Otc {
        // Since the OTC's port sole raison d'être is to initialize the GPU linked list in RAM it
        // doesn't support all the DMA control options
        ctrl &= 0x5100_0000;
        // OTC is always backwards
        ctrl |= 2;
    }

    let was_enabled = psx.dma[port].control.is_enabled();

    psx.dma[port].control.set(ctrl);

    let is_enabled = psx.dma[port].control.is_enabled();

    if was_enabled ^ is_enabled {
        // Channel was started or stopped
        if is_enabled {
            start(psx, port);
        } else {
            unimplemented!("Force stop DMA channel {:?}", port);
        }
    }

    refresh_cpu_halt(psx);
}

fn refresh_cpu_halt(psx: &mut Psx) {
    let halt_cpu = psx.dma.channels.iter().any(|c| {
        let control = c.control;

        // The CPU is stopped it a channel is running with the "manual" sync mode without chopping
        control.is_enabled() && !control.is_chopped() && control.sync_mode() == SyncMode::Manual
    });

    let timing_penalty = if halt_cpu {
        // The CPU doesn't run, no need to slow it down
        0
    } else {
        // XXX Taken from mednafen, apparently it's only implemented properly for the GPU.
        // Probably because for the other ports the writing timings aren't implemented
        // properly.
        let gpu_chan = &psx.dma[Port::Gpu];
        let control = gpu_chan.control;

        let block_size = gpu_chan.block_size;

        let is_cpu_stalled = control.is_enabled()
            && !control.is_chopped()
            && control.sync_mode() == SyncMode::Request
            && block_size > 0
            && can_run(psx, Port::Gpu, control.is_from_ram());

        if is_cpu_stalled {
            CycleCount::from(block_size - 1)
        } else {
            0
        }
    };

    psx.set_dma_timing_penalty(timing_penalty);
    psx.set_cpu_stalled_for_dma(halt_cpu);
}

/// Called when channel `port` starts
fn start(psx: &mut Psx, port: Port) {
    psx.dma[port].clock_counter = 0;
    psx.dma[port].remaining_words = 0;

    // Mednafen mentions that some (probably buggy) games like Viewpoint expect some small DMA
    // transfer to complete almost immediately and trigger a race condition if we lag a tiny bit.
    // Given that our implementation is not super timing-accurate we can just give the DMA a small
    // headstart here to take care of this situation
    run_channel(psx, port, 64);
}

/// Run channel `port` for `cycles` CPU cycles
fn run_channel(psx: &mut Psx, port: Port, cycles: CycleCount) {
    let sync_mode = psx.dma[port].control.sync_mode();

    psx.dma[port].clock_counter += cycles;

    while psx.dma[port].clock_counter > 0 {
        let mut do_copy = true;
        let control = psx.dma[port].control;

        if psx.dma[port].remaining_words == 0 {
            if !control.is_enabled() {
                break;
            }

            if !can_run(psx, port, control.is_from_ram()) {
                // The device is not ready
                break;
            }

            // Initialize RAM pointer
            psx.dma[port].cur_address = psx.dma[port].base;
            let cur_addr = psx.dma[port].cur_address;

            match sync_mode {
                SyncMode::Manual => {
                    let channel = &mut psx.dma[port];

                    channel.remaining_words = channel.block_size;
                }
                SyncMode::Request => {
                    let channel = &mut psx.dma[port];

                    channel.remaining_words = channel.block_size;
                    channel.block_count -= 1;

                    // XXX From mednafen, only GPU timings are implemented so far
                    if port == Port::Gpu {
                        channel.clock_counter -= 7;
                    }
                }
                SyncMode::LinkedList => {
                    let overflow = cur_addr & 0x80_0000 != 0;

                    if overflow {
                        unimplemented!();
                    }

                    let header: u32 = psx.ram.load(cur_addr & 0x1f_fffc);
                    psx.dma[port].cur_address = (cur_addr + 4) & 0xff_ffff;
                    psx.dma[port].base = header & 0xff_ffff;

                    let remw = (header >> 24) as u16;
                    psx.dma[port].remaining_words = remw;

                    // Timings from mednafen
                    psx.dma[port].clock_counter -= if remw > 0 { 15 } else { 10 };

                    // Mednafen skips the copy in this case because `remaining_words` might be 0
                    // and that wouldn't work correctly because in other situations
                    // `remaining_words` == 0 is the same as setting it to 0x1_0000 (i.e. it wraps
                    // around).
                    do_copy = false;
                }
            }
        } else if control.is_chopped() {
            unimplemented!("Implement chop");
        }

        if do_copy {
            let cur_addr = psx.dma[port].cur_address & 0x1f_fffc;

            let overflow = cur_addr & 0x80_0000 != 0;
            if overflow {
                unimplemented!("DMA address overflow");
            }

            if control.is_from_ram() {
                let v = psx.ram.load(cur_addr);
                port_store(psx, port, v);
            } else {
                let v = port_load(psx, port);
                psx.ram.store(cur_addr, v);
            }

            psx.dma[port].cur_address = 0xff_ffff
                & if control.is_backwards() {
                    psx.dma[port].cur_address.wrapping_sub(4)
                } else {
                    psx.dma[port].cur_address.wrapping_add(4)
                };

            psx.dma[port].clock_counter -= 1;
            psx.dma[port].remaining_words -= 1;
        }

        if control.is_chopped() {
            unimplemented!("Implement chop");
        }

        if psx.dma[port].remaining_words == 0 {
            if !control.is_enabled() {
                unimplemented!();
            }

            let end_of_dma = match sync_mode {
                SyncMode::Manual => {
                    // We do the transfer all at once, we're done
                    true
                }
                SyncMode::Request => {
                    let channel = &mut psx.dma[port];

                    channel.base = channel.cur_address;

                    channel.block_count == 0
                }
                SyncMode::LinkedList => {
                    // Check for end-of-list marker
                    psx.dma[port].base == 0xff_ffff
                }
            };

            if end_of_dma && psx.dma.end_of_dma(port).is_triggered() {
                irq::trigger(psx, irq::Interrupt::Dma);
            }
        }
    }

    if psx.dma[port].clock_counter > 0 {
        psx.dma[port].clock_counter = 0;
    }
}

/// Check if the device targeted by `port` can either be read from of written to
fn can_run(psx: &Psx, port: Port, write: bool) -> bool {
    if write {
        match port {
            Port::Gpu => psx.gpu.dma_can_write(),
            _ => unimplemented!("Can write {:?}?", port),
        }
    } else {
        match port {
            // XXX This is from mednafen but I'm not entirely sure why this doesn't simply return
            // `true`
            Port::Otc => psx.dma[port].control.is_started(),
            _ => unimplemented!("Can read {:?}?", port),
        }
    }
}

/// Perform a DMA port write
fn port_store(psx: &mut Psx, port: Port, v: u32) {
    match port {
        Port::Gpu => gpu::dma_store(psx, v),
        _ => unimplemented!("DMA port store {:?}", port),
    }
}

/// Perform a DMA port read
fn port_load(psx: &mut Psx, port: Port) -> u32 {
    match port {
        Port::Otc => {
            let channel = &psx.dma[port];

            if channel.remaining_words == 1 {
                // Last entry contains the end of table marker
                0xff_ffff
            } else {
                // Pointer to the previous entry
                channel.cur_address.wrapping_sub(4) & 0x1f_ffff
            }
        }
        _ => unimplemented!("DMA port store {:?}", port),
    }
}

/// The 7 DMA channels
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Port {
    /// Macroblock decoder input
    MDecIn = 0,
    /// Macroblock decoder output
    MDecOut = 1,
    /// Graphics Processing Unit
    Gpu = 2,
    /// CD-ROM drive
    CdRom = 3,
    /// Sound Processing Unit
    Spu = 4,
    /// Extension port
    Pio = 5,
    /// Used to clear the ordering table in RAM
    Otc = 6,
}

impl Port {
    pub fn from_index(index: u32) -> Port {
        match index {
            0 => Port::MDecIn,
            1 => Port::MDecOut,
            2 => Port::Gpu,
            3 => Port::CdRom,
            4 => Port::Spu,
            5 => Port::Pio,
            6 => Port::Otc,
            n => panic!("Invalid DMA channel {}", n),
        }
    }
}

/// DMA transfer synchronization mode
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum SyncMode {
    /// Transfer starts when the CPU writes to the Trigger bit and transfers everything at once
    Manual = 0,
    /// Sync blocks to DMA requests
    Request = 1,
    /// Used to transfer GPU command lists
    LinkedList = 2,
}

struct Channel {
    control: ChannelControl,
    /// Base address
    base: u32,
    /// Current address during DMA operation
    cur_address: u32,
    /// Block size (not used in LinkedList mode)
    block_size: u16,
    /// Number of blocks being transferred in Request mode
    block_count: u16,
    remaining_words: u16,
    clock_counter: CycleCount,
}

impl Channel {
    fn new() -> Channel {
        Channel {
            control: ChannelControl::new(),
            base: 0,
            cur_address: 0,
            block_size: 0,
            block_count: 0,
            remaining_words: 0,
            clock_counter: 0,
        }
    }

    fn block_control(&self) -> u32 {
        let bs = u32::from(self.block_size);
        let bc = u32::from(self.block_count);

        (bc << 16) | bs
    }

    fn set_block_control(&mut self, val: u32) {
        self.block_size = val as u16;
        self.block_count = (val >> 16) as u16;
    }
}

/// DMA channel control register
#[derive(Copy, Clone)]
struct ChannelControl(u32);

impl ChannelControl {
    fn new() -> ChannelControl {
        ChannelControl(0)
    }

    fn set(&mut self, v: u32) {
        // Many bits are RO
        self.0 = v & 0x7177_0703
    }

    fn get(self) -> u32 {
        self.0
    }

    /// Returns true if the 'enable' bit is set
    fn is_enabled(self) -> bool {
        self.0 & (1 << 24) != 0
    }

    /// Returns true if the 'start' bit is set
    fn is_started(self) -> bool {
        self.0 & (1 << 28) != 0
    }

    /// Returns true if transfer is from RAM to device, false otherwise
    fn is_from_ram(self) -> bool {
        self.0 & 1 != 0
    }

    fn sync_mode(self) -> SyncMode {
        match (self.0 >> 9) & 3 {
            0 => SyncMode::Manual,
            1 => SyncMode::Request,
            2 => SyncMode::LinkedList,
            _ => unimplemented!("Unknown DMA sync mode"),
        }
    }

    fn is_chopped(self) -> bool {
        self.0 & (1 << 8) != 0
    }

    fn is_backwards(self) -> bool {
        self.0 & (1 << 1) != 0
    }

    /// Called when the channel is stopped
    fn stop(&mut self) {
        // Clear enabled bit
        self.0 &= !(1 << 24);
        // Clear start/trigger bit
        self.0 &= !(1 << 28);
    }
}

/// DMA control register
struct Control(u32);

impl Control {
    fn new() -> Control {
        Control(0)
    }

    fn set(&mut self, conf: u32) {
        self.0 = conf;
    }

    fn get(&self) -> u32 {
        self.0
    }
}

/// DMA IRQ config register
struct IrqConfig(u32);

impl IrqConfig {
    fn new() -> IrqConfig {
        IrqConfig(0)
    }

    fn set(&mut self, conf: u32) -> IrqState {
        let write_mask = 0x00ff_803f;
        self.0 &= !write_mask;
        self.0 |= conf & write_mask;

        // Writing 1 to the flag bits acks the interrupts
        let ack = conf & 0x7f00_0000;
        self.0 &= !ack;

        self.refresh_irq()
    }

    fn get(&self) -> u32 {
        self.0
    }

    fn irq_enabled(&self, port: Port) -> bool {
        let bit = 16 + port as usize;

        self.0 & (1 << bit) != 0
    }

    /// Set the IRQ flag for `Port` active
    fn flag_irq(&mut self, port: Port) -> IrqState {
        let bit = 24 + port as usize;

        self.0 |= 1 << bit;

        self.refresh_irq()
    }

    fn master_irq_forced(&self) -> bool {
        self.0 & (1 << 15) != 0
    }

    fn channel_irq_enable(&self) -> bool {
        self.0 & (1 << 23) != 0
    }

    /// Refresh the state of the master IRQ flag and returns IrqState::Triggered if an interrupt
    /// should be asserted
    fn refresh_irq(&mut self) -> IrqState {
        let cur_master = self.0 & (1 << 31) != 0;

        let flags = self.0 & 0x7f00_0000;

        let channel_irq_active = self.channel_irq_enable() && flags != 0;

        let new_master = channel_irq_active || self.master_irq_forced();

        if new_master {
            if !cur_master {
                // IRQ has been triggered
                self.0 |= 1 << 31;
                IrqState::Triggered
            } else {
                // IRQ was already active, nothing to do
                IrqState::Idle
            }
        } else {
            // Master is disabled
            self.0 &= !(1 << 31);
            IrqState::Idle
        }
    }
}

/// How often should we update the DMA state. The smaller this value the more accurate we'll be,
/// but very small values will just ruin performance
const DMA_REFRESH_PERIOD: CycleCount = 128;
