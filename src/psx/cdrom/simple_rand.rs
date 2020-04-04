/// Fast non-cryptographically secure RNG. The implementation is XorShift with a period of
/// (2**31)-1. This is more than sufficient for our use case. This RNG is of course fully
/// deterministic and will always return the same sequence for any given seed
///
/// See http://www.jstatsoft.org/v08/i14/paper for more details on the algorithm.
///
/// One of the pitfalls of this algorithm is that if the output is used as a raw 32bit random
/// number (without modulo) it'll never return 0.
pub struct SimpleRand {
    state: u32,
}

impl SimpleRand {
    /// Create a new FastRand instance using a hardcoded seed
    pub fn new() -> SimpleRand {
        SimpleRand {
            // Arbitrary seed, must be non-0
            state: 1,
        }
    }

    /// Run through one cycle of XorShift and return the internal pseudo-random state. It will
    /// *never* return 0.
    fn next(&mut self) -> u32 {
        // The XorShift paper lists a bunch of valid shift triplets, I picked one at random.
        self.state ^= self.state << 6;
        self.state ^= self.state >> 1;
        self.state ^= self.state << 11;

        self.state
    }

    /// Returns a value between min and max (inclusive)
    pub fn get(&mut self, min: u32, max: u32) -> u32 {
        debug_assert!(min < max);

        // + 1 so that the range will be inclusive of `max`
        let range = max - min + 1;

        let r = self.next() % range;

        min + r
    }
}
