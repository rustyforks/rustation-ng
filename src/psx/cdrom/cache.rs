//! Multi-threaded prefetching cache for PSX discs.
//!
//! This cache tries to read sectors ahead of the emulator to avoid any I/O lockup

use std::collections::BTreeMap;
use std::thread;
use cdimage::sector::Sector;
use cdimage::msf::Msf;
use cdimage::Image;
use std::sync::{Arc, Mutex, Condvar};

pub struct Cache {
    /// The reader state and a Condvar used to notify the reader when it should read a new sector
    /// (or used by the reader to let the cache know that a sector has been read).
    reader: Arc<(Mutex<Reader>, Condvar)>,
    /// Thread handle for the reader
    handle: Option<thread::JoinHandle<()>>,
}

impl Cache {
    fn new(image: Box<dyn Image + Send>) -> Cache {
        let reader = Arc::new((Mutex::new(Reader::new(image)), Condvar::new()));

        let thread_reader = reader.clone();

        let builder = thread::Builder::new()
            .name("RSX CD prefetch".to_string())
            .stack_size(32 * 1024);

        let handle = builder
            .spawn(move || {
                run_prefetcher(thread_reader);
            })
        .unwrap();

        Cache {
            reader: reader,
            handle: Some(handle),
        }
    }
}

type SectorCache = BTreeMap<Msf, Arc<Sector>>;

/// The shared state between the main thread and the prefetcher
struct Reader {
    /// The actual sector cache
    sectors: SectorCache,
    /// The disc image we're caching
    image: Box<dyn Image + Send>,
    /// The last sector that the emulator attempted to read. Used to figure out where to prefetch
    /// sectors
    prefetch_next: Option<Msf>,
    /// Set to true when the prefetcher should quit
    quit: bool,
}

impl Reader {
    fn new(image: Box<dyn Image + Send>) -> Reader {
        Reader {
            sectors: SectorCache::new(),
            disc,
            prefetch_next: None,
            quit: false,
        }
    }
}

fn run_prefetcher(reader: Arc<(Mutex<Reader>, Condvar)>) {
    let &(ref reader, ref cond) = &*reader;

    loop {
        let next = {
            let reader = reader.lock().unwrap();

            if reader.quit {
                return;
            }

            match reader.prefetch_next {
                Some(m) => m,
                None => {
                    // Wait for the emulator to give us something to do
                    cond.wait(reader).unwrap();
                    continue;
                }
            }
        };

        panic!("Need to prefetch {}", next);
    }
}
