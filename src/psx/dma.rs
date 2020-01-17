//! The PlayStation DMA, that can be used to copy data between the RAM and various devices (GPU,
//! CD drive, MDEC, SPU etc...)

use super::{gpu, sync, AccessWidth, Addressable, CycleCount, Psx};
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
                4 => channel.block_control,
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
                4 => psx.dma[port].block_control = val,
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
            4 => psx.dma.irq_config.set(val),
            _ => unimplemented!("DMA register write to {:x}", reg),
        },
        _ => unreachable!(),
    }
}

fn set_channel_control(psx: &mut Psx, port: Port, ctrl: u32) {
    if port == Port::Otc {
        unimplemented!("Handle OTC channel configuration");
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

        let block_size = gpu_chan.block_control & 0xffff;

        let is_cpu_stalled = control.is_enabled()
            && !control.is_chopped()
            && control.sync_mode() == SyncMode::Request
            && block_size > 0
            && can_run(psx, Port::Gpu, control.is_from_ram());

        if is_cpu_stalled {
            block_size - 1
        } else {
            0
        }
    };

    psx.set_dma_timing_penalty(timing_penalty as CycleCount);
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
    let control = psx.dma[port].control;
    let sync_mode = control.sync_mode();

    psx.dma[port].clock_counter += cycles;

    while psx.dma[port].clock_counter > 0 {
        let mut do_copy = true;

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
                _ => unimplemented!("Implement {:?}", sync_mode),
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
                unimplemented!();
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
                SyncMode::LinkedList => {
                    // Check for end-of-list marker
                    psx.dma[port].base == 0xff_ffff
                }
                _ => unimplemented!(),
            };

            if end_of_dma {
                unimplemented!();
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
    /// Block control. The interpretation of this field depends on the sync mode configured in the
    /// channel's control register
    block_control: u32,
    remaining_words: u16,
    clock_counter: CycleCount,
}

impl Channel {
    fn new() -> Channel {
        Channel {
            control: ChannelControl::new(),
            base: 0,
            cur_address: 0,
            block_control: 0,
            remaining_words: 0,
            clock_counter: 0,
        }
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

    fn set(&mut self, conf: u32) {
        // Not all bits are writeable
        self.0 = conf & 0x00ff_803f;
    }

    fn get(&self) -> u32 {
        self.0
    }
}

/// How often should we update the DMA state. The smaller this value the more accurate we'll be,
/// but very small values will just ruin performance
const DMA_REFRESH_PERIOD: CycleCount = 128;
