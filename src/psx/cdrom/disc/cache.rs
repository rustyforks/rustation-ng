//! Multi-threaded prefetching cache for PSX discs.
//!
//! This cache tries to read sectors ahead of the emulator to avoid any I/O lockup

use cdimage::msf::Msf;
use cdimage::sector::Sector;
use cdimage::{CdResult, Image, Toc};
use std::collections::BTreeMap;
use std::sync::{Arc, Condvar, Mutex, MutexGuard};
use std::thread;

pub struct Cache {
    /// The reader state and a Condvar used to notify the reader when it should read a new sector
    /// (or used by the reader to let the cache know that a sector has been read).
    reader: Arc<(Mutex<Reader>, Condvar)>,
    /// Thread handle for the prefetcher
    handle: Option<thread::JoinHandle<()>>,
    /// Name of our format
    format: String,
    /// CD table of contents
    toc: Toc,
}

impl Cache {
    pub fn new(image: Box<dyn Image + Send>) -> Cache {
        let format = format!("cache + {}", image.image_format());
        let toc = image.toc().clone();

        let reader = Arc::new((Mutex::new(Reader::new()), Condvar::new()));

        let thread_reader = reader.clone();

        let builder = thread::Builder::new()
            .name("RSX CD prefetch".to_string())
            .stack_size(1024 * 1024);

        let handle = builder
            .spawn(move || {
                run_prefetcher(image, thread_reader);
            })
            .unwrap();

        let mut cache = Cache {
            reader,
            handle: Some(handle),
            format,
            toc,
        };

        cache.preload();

        cache
    }

    fn reader(&self) -> (MutexGuard<Reader>, &Condvar) {
        let &(ref reader, ref cond) = &*self.reader;

        (reader.lock().unwrap(), cond)
    }

    /// Pre-load the beginning of every track to avoid stalling when they're accessed
    fn preload(&mut self) {
        let toc = self.toc.clone();
        // Preload the first minute (assuming 1x) of each track
        for t in toc.tracks() {
            for i in 0..(60 * 75) {
                match t.absolute_msf(Msf::from_sector_index(i).unwrap()) {
                    Ok(msf) => {
                        let _ = self.read_sector(msf);
                    }
                    _ => break,
                }
            }
        }
    }
}

impl ::std::ops::Drop for Cache {
    fn drop(&mut self) {
        {
            let (mut reader, cond) = self.reader();

            // Tell the prefetcher to quit
            reader.quit = true;
            cond.notify_one();
        }

        if let Some(t) = self.handle.take() {
            t.join().unwrap();
        }
    }
}

/// Make the cache itself behave like an image format
impl Image for Cache {
    fn image_format(&self) -> String {
        self.format.clone()
    }

    fn read_sector(&mut self, msf: Msf) -> CdResult<Sector> {
        let (mut reader, cond) = self.reader();

        loop {
            // Even if the sector is already in cache we want to notify the prefetcher of what
            // we're doing so that it can read-ahead if necessary
            reader.prefetch_next = msf;
            reader.prefetch_remaining = PREFETCH_READAHEAD_SECTORS;
            cond.notify_one();

            if let Some(s) = reader.sectors.get(&msf) {
                // Sector is in cache
                return s.clone();
            }

            // Sector isn't cached, wait for the prefetcher
            reader = cond.wait(reader).unwrap();
        }
    }

    fn toc(&self) -> &Toc {
        &self.toc
    }
}

type SectorCache = BTreeMap<Msf, CdResult<Sector>>;

/// The shared state between the main thread and the prefetcher
struct Reader {
    /// The actual sector cache
    sectors: SectorCache,
    /// Number of sectors left to prefetch before becoming idle
    prefetch_remaining: u32,
    /// Next sector we should attempt to prefetch (if `prefetch_remaining` is > 0).
    prefetch_next: Msf,
    /// Set to true when the prefetcher should quit
    quit: bool,
}

impl Reader {
    fn new() -> Reader {
        Reader {
            sectors: SectorCache::new(),
            prefetch_remaining: 0,
            prefetch_next: Msf::zero(),
            quit: false,
        }
    }
}

fn run_prefetcher(mut image: Box<dyn Image>, reader: Arc<(Mutex<Reader>, Condvar)>) {
    let &(ref reader_mutex, ref cond) = &*reader;

    let mut reader = reader_mutex.lock().unwrap();

    while !reader.quit {
        if reader.prefetch_remaining == 0 {
            // Nothing left to do, wait for the next prefetch command
            reader = cond.wait(reader).unwrap();
            continue;
        }

        // We have something to prefetch
        let fetch_msf = reader.prefetch_next;

        // Update prefetch_remaining and prefetch_next before we drop the lock since the emulator
        // code can override those.
        match reader.prefetch_next.next() {
            Some(next) => {
                reader.prefetch_remaining -= 1;
                reader.prefetch_next = next;
            }
            None => {
                // Reached max MSF, nothing left to do
                reader.prefetch_remaining = 0;
            }
        }

        if reader.sectors.contains_key(&fetch_msf) {
            // We already have this sector
            continue;
        }

        // We drop the lock while the read is taking place to avoid stalling the emulator if it
        // tries to access an already-cached sector
        ::std::mem::drop(reader);

        let sector = image.read_sector(fetch_msf);

        // Re-lock the reader
        reader = reader_mutex.lock().unwrap();

        reader.sectors.insert(fetch_msf, sector);
        // If the emulator was waiting for a sector, wake it up
        cond.notify_one();
    }
}

/// Number of sectors to read ahead
const PREFETCH_READAHEAD_SECTORS: u32 = 75;
