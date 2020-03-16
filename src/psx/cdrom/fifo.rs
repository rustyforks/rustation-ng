/// 16byte FIFO used to store command arguments and responses
pub struct Fifo {
    /// Data buffer
    buffer: [u8; 16],
    /// Write pointer (4bits + carry)
    write_idx: u8,
    /// Read pointer (4bits + carry)
    read_idx: u8,
}

impl Fifo {
    pub fn new() -> Fifo {
        Fifo {
            buffer: [0; 16],
            write_idx: 0,
            read_idx: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        // If both pointers point at the same cell and have the same carry the FIFO is empty.
        self.write_idx == self.read_idx
    }

    pub fn is_full(&self) -> bool {
        // The FIFO is full if both indexes point to the same cell while having a different carry.
        self.write_idx == self.read_idx ^ 0x10
    }

    pub fn clear(&mut self) {
        self.write_idx = 0;
        self.read_idx = 0;
        self.buffer = [0; 16];
    }

    // Retrieve the number of elements in the FIFO. This number is in the range [0; 31] so it's
    // potentially bogus if an overflow occurred. This does seem to match the behaviour of the
    // actual hardware though. For instance command 0x19 ("Test") takes a single parameter. If you
    // send 0 or more than one parameter you get an error code back. However if you push 33
    // parameters in the FIFO only the last one is actually used by the command and it works as
    // expected.
    pub fn len(&self) -> u8 {
        (self.write_idx.wrapping_sub(self.read_idx)) & 0x1f
    }

    pub fn push(&mut self, val: u8) {
        let idx = (self.write_idx & 0xf) as usize;

        self.buffer[idx] = val;

        self.write_idx = self.write_idx.wrapping_add(1) & 0x1f;
    }

    pub fn push_slice(&mut self, s: &[u8]) {
        for &v in s {
            self.push(v)
        }
    }

    pub fn pop(&mut self) -> u8 {
        debug_assert!(!self.is_empty());

        let idx = (self.read_idx & 0xf) as usize;

        self.read_idx = self.read_idx.wrapping_add(1) & 0x1f;

        self.buffer[idx]
    }
}
