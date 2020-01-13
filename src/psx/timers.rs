//! The PlayStation has three timers. They're mostly identical except that they can each select a
//! different clock source besides the regular system clock:
//!
//! - Timer 0: GPU pixel clock
//! - Timer 1: GPU horizontal blanking
//! - Timer 2: System clock / 8

use super::{irq, sync, Addressable, CycleCount, Psx};
use std::cmp::min;
use std::ops::{Index, IndexMut};

const TIMERSYNC: sync::SyncToken = sync::SyncToken::Timers;
const TIMER_IRQ: [irq::Interrupt; 3] = [
    irq::Interrupt::Timer0,
    irq::Interrupt::Timer1,
    irq::Interrupt::Timer2,
];

pub struct Timers {
    timers: [Timer; 3],
}

impl Timers {
    pub fn new() -> Timers {
        Timers {
            timers: [Timer::new(), Timer::new(), Timer::new()],
        }
    }

    /// Retrieve the clock source for timer `which`
    fn clock_source(&self, which: usize) -> Clock {
        let source_raw = self[which].mode.clock_source();

        CLOCK_SOURCE_MATRIX[which][source_raw]
    }

    /// Retrieve the sync mode for timer `which`
    fn sync_mode(&self, which: usize) -> SyncMode {
        let timer = &self[which];

        if timer.mode.sync_enabled() {
            let sync_raw = timer.mode.sync_mode();
            let sync = SYNC_MODE_MATRIX[which][sync_raw];

            if sync == SyncMode::Unimplemented {
                unimplemented!("Timer {:x} enabled sync {:x}", which, sync_raw);
            }

            sync
        } else {
            SyncMode::FreeRun
        }
    }

    fn next_irq(&self) -> Option<CycleCount> {
        let mut delta = None;

        for which in 0..3 {
            let source = self.clock_source(which);
            let sync_mode = self.sync_mode(which);

            if let Some(d) = self[which].next_irq(source, sync_mode) {
                let new_d = match delta {
                    None => d,
                    Some(delta) => min(d, delta),
                };

                delta = Some(new_d);
            }
        }

        delta
    }
}

impl Index<usize> for Timers {
    type Output = Timer;

    fn index(&self, index: usize) -> &Self::Output {
        &self.timers[index]
    }
}

impl IndexMut<usize> for Timers {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.timers[index]
    }
}

pub struct Timer {
    mode: Mode,
    /// The counter is really 16bit but we use a wider value to avoid overflows (since we might
    /// overshoot by a few cycles before we handle the overflow in `run`).
    counter: u32,
    target: u16,
    /// If the IRQ is configured to be oneshot in `Mode` we don't re-trigger.
    irq_inhibit: bool,
}

impl Timer {
    fn new() -> Timer {
        Timer {
            mode: Mode::new(),
            counter: 0,
            target: 0,
            irq_inhibit: false,
        }
    }

    fn counter(&self) -> u16 {
        debug_assert!(self.counter <= 0xffff);
        self.counter as u16
    }

    fn set_counter(&mut self, val: u16) {
        self.counter = u32::from(val);
        self.irq_inhibit = false;
    }

    fn set_mode(&mut self, mode: u16) {
        self.mode.configure(mode);
        // Writing to the mode register resets the counter to 0 and re-enables the IRQ if
        // necessary.
        self.set_counter(0);
    }

    fn read_mode(&mut self) -> u16 {
        let mode = self.mode.0;

        // Some bits are read-clear
        self.mode.clear_overflow_reached();
        if !self.target_match() {
            self.mode.clear_target_reached();
        }

        mode
    }

    fn set_target(&mut self, val: u16) {
        self.target = val;
    }

    /// Returns true if `counter` equals `target`
    fn target_match(&self) -> bool {
        u32::from(self.target) == self.counter
    }

    /// Called when a target mach occurred. Returns `true` if the interrupt has been triggered.
    fn set_match(&mut self) -> bool {
        self.mode.set_target_reached();

        if self.mode.reset_counter_on_target() {
            if self.target == 0 {
                self.counter = 0;
            } else {
                self.counter %= u32::from(self.target);
            }
        }

        if self.mode.irq_on_target() && !self.irq_inhibit {
            if self.mode.one_shot_irq() {
                self.irq_inhibit = true;
            }
            true
        } else {
            false
        }
    }

    /// Predict the date of the next interrupt (or `None` if the interrupt is disabled or can't be
    /// predicted)
    fn next_irq(&self, source: Clock, sync_mode: SyncMode) -> Option<CycleCount> {
        if !self.mode.irq_on_target() && !self.mode.irq_on_overflow() {
            // No interrupt is configured
            return None;
        }

        if self.irq_inhibit {
            // IRQ won't trigger
            return None;
        }

        if source == Clock::GpuHSync {
            // We're going to be synchronized by the GPU code, we have nothing to do
            return None;
        }

        if source == Clock::GpuPixClk {
            // XXX This one will be imprecise because the GPU pixel clock counter is (obviously)
            // not refreshed at every tick. We could actually predict when it overflows using the
            // GPU clock ratio and some clever maths but mednafen doesn't bother with it so let's
            // ignore it for now
            return None;
        }

        if sync_mode == SyncMode::Stopped {
            // Timer is stopped
            return None;
        }

        let target = u32::from(self.target);

        // Value of the counter the on the next event
        let event_counter = if self.counter >= target {
            // We've overshot the target, the next event is on overflow. Technically if the
            // overflow IRQ is disabled we could micro-optimize this to only force a refresh
            // when the counter will have wrapped all the way back to the target but it
            // probably isn't worth it.
            0x1_0000
        } else {
            // We haven't reached the target yet
            if self.mode.irq_on_target() || self.mode.reset_counter_on_target() {
                // We need to force a refresh when we hit the target to either trigger the IRQ
                // or reset the counter
                target
            } else {
                // We don't have anything special to do when we pass the target, we can aim
                // directly for the overflow
                0x1_0000
            }
        };

        let delta = event_counter - self.counter;

        if source == Clock::CpuDiv8 {
            unimplemented!("Implement SysClockDiv8");
        }

        Some(delta as CycleCount)
    }
}

pub fn run(psx: &mut Psx) {
    let mut _elapsed = sync::resync(psx, TIMERSYNC);

    predict_next_sync(psx);
}

/// Figure out when we should force a resync next
fn predict_next_sync(psx: &mut Psx) {
    // Default value used if we don't have any upcoming event to monitor for.
    let default = 1024;

    let delta = psx.timers.next_irq().unwrap_or(default);

    sync::next_event(psx, TIMERSYNC, delta);
}

pub fn load<T: Addressable>(psx: &mut Psx, offset: u32) -> T {
    run(psx);

    let which = (offset >> 4) as usize;

    let timer = &mut psx.timers[which];

    let v = match offset & 0xf {
        0x0 => timer.counter(),
        0x4 => timer.read_mode(),
        0x8 => timer.target,
        n => unimplemented!("timer read @ {:x}", n),
    };

    T::from_u32(u32::from(v))
}

pub fn store<T: Addressable>(psx: &mut Psx, offset: u32, val: T) {
    run(psx);

    let val = val.as_u16();
    let which = (offset >> 4) as usize;

    match offset & 0xf {
        0x0 => psx.timers[which].set_counter(val),
        0x4 => psx.timers[which].set_mode(val),
        0x8 => psx.timers[which].set_target(val),
        0xc => (), // Nothing in this register
        n => unimplemented!("timer write @ {:x}", n),
    }

    if psx.timers[which].target_match() {
        notify_match(psx, which);
    }

    predict_next_sync(psx);
}

/// Called when the counter for `which` reached its target value
fn notify_match(psx: &mut Psx, which: usize) {
    if psx.timers[which].set_match() {
        irq::trigger(psx, TIMER_IRQ[which]);
    }
}

/// Timer mode register
struct Mode(u16);

impl Mode {
    fn new() -> Mode {
        Mode(0)
    }

    fn configure(&mut self, mode: u16) {
        // bits [10:12] are read-only and must be preserved
        // XXX We don't implement bit 10 (Interrupt Request) for the time being.
        self.0 &= 7 << 10;
        self.0 |= mode & 0x3ff;
    }

    fn set_target_reached(&mut self) {
        self.0 |= 1 << 11;
    }

    fn clear_target_reached(&mut self) {
        self.0 &= !(1 << 11);
    }

    fn clear_overflow_reached(&mut self) {
        self.0 &= !(1 << 12);
    }

    /// Returns true if this counter should trigger an interrupt when the target is reached
    fn irq_on_target(&self) -> bool {
        self.0 & (1 << 4) != 0
    }

    /// Returns true if this counter should trigger an interrupt when the counter overflows
    fn irq_on_overflow(&self) -> bool {
        self.0 & (1 << 5) != 0
    }

    /// Returns true if the counter should be reset to 0 when the target is reached, false if it
    /// should just wrap around when it reaches 0xffff.
    fn reset_counter_on_target(&self) -> bool {
        self.0 & (1 << 3) != 0
    }

    /// If true the IRQ is one-shot: it will only trigger once and then remain inactive until
    /// rearmed by a register write to the timer mode or counter.
    fn one_shot_irq(&self) -> bool {
        self.0 & (1 << 6) == 0
    }

    /// Get the clock source configuration. The actual source will depend on which timer is
    /// configured.
    fn clock_source(&self) -> usize {
        let source = (self.0 >> 8) & 3;

        source as usize
    }

    /// Returns true if synchronization is enabled
    fn sync_enabled(&self) -> bool {
        self.0 & 1 != 0
    }

    /// Get the sync mode configuration. The actual mode will depend on which timer is
    /// configured.
    fn sync_mode(&self) -> usize {
        let mode = (self.0 >> 1) & 3;

        mode as usize
    }
}

/// The possible source of synchronization for the timers
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum SyncMode {
    /// Counter is free-running
    FreeRun,
    /// Counter is stopped
    Stopped,
    /// XXX unimplemented mode
    Unimplemented,
}

/// Look up table to get the actual sync mode from the Mode config for each of the 3 timer
const SYNC_MODE_MATRIX: [[SyncMode; 4]; 3] = [
    [
        SyncMode::Unimplemented,
        SyncMode::Unimplemented,
        SyncMode::Unimplemented,
        SyncMode::Unimplemented,
    ],
    [
        SyncMode::Unimplemented,
        SyncMode::Unimplemented,
        SyncMode::Unimplemented,
        SyncMode::Unimplemented,
    ],
    [
        SyncMode::Stopped,
        SyncMode::FreeRun,
        SyncMode::Stopped,
        SyncMode::FreeRun,
    ],
];

/// The four possible clock sources for the timers. Not every timer can use every clock.
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Clock {
    /// The CPU clock at ~33.87MHz
    Cpu,
    /// The CPU clock divided by 8 (~4.23MHz)
    CpuDiv8,
    /// The GPU's pixelclock (depends on hardware, around 53Mhz)
    GpuPixClk,
    /// The GPU's HSync signal (deponds on hardware and video timings)
    GpuHSync,
}

/// Look up table to get the actual clock source from the Mode config for each of the 3 timer
const CLOCK_SOURCE_MATRIX: [[Clock; 4]; 3] = [
    [Clock::Cpu, Clock::GpuPixClk, Clock::Cpu, Clock::GpuPixClk],
    [Clock::Cpu, Clock::GpuHSync, Clock::Cpu, Clock::GpuHSync],
    [Clock::Cpu, Clock::Cpu, Clock::CpuDiv8, Clock::CpuDiv8],
];
