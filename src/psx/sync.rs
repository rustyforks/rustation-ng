use super::{CycleCount, Psx};

/// Tokens used to keep track of the progress of each module individually
pub enum SyncToken {
    Spu,

    NumTokens,
}

pub struct Synchronizer {
    last_sync: [CycleCount; SyncToken::NumTokens as usize],
}

impl Synchronizer {
    pub fn new() -> Synchronizer {
        Synchronizer {
            last_sync: [0; SyncToken::NumTokens as usize],
        }
    }
}

/// Resynchronize `who` with the CPU, returning the number of CPU cycles elapsed since the last
/// sync date
pub fn resync(psx: &mut Psx, who: SyncToken) -> CycleCount {
    let who = who as usize;

    let elapsed = psx.cycle_counter - psx.sync.last_sync[who];
    psx.sync.last_sync[who] = psx.cycle_counter;

    debug_assert!(elapsed >= 0);

    elapsed
}

/// If `who` couldn't consume all the cycles returned by `resync` it can return the leftover here,
/// we'll move the `last_sync` back by the same number of cycles which means that they'll be
/// returned on the next call to `resync`. Should only be called with *positive* cycle amounts,
/// otherwise it would put the module in the future.
pub fn rewind(psx: &mut Psx, who: SyncToken, rewind: CycleCount) {
    debug_assert!(rewind >= 0);

    psx.sync.last_sync[who as usize] -= rewind;
}
