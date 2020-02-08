//! Fixed point implementation for the drawing code

// Clippy doesn't like our arithmetic implementations because they don't use the operators it
// expect (i.e. division uses shifts etc...)
#![allow(clippy::suspicious_arithmetic_impl)]

use std::fmt;
use std::ops::Div;

/// The number of bits used for sub-pixel precision in a FixedPoint coordinate
const FIXED_POINT_SHIFT: u32 = 8;

/// Fixed point representation of a number.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FixedPoint(i32);

impl FixedPoint {
    pub fn new(v: i32) -> FixedPoint {
        FixedPoint(v << FIXED_POINT_SHIFT)
    }

    #[cfg(test)]
    pub fn truncate(self) -> i32 {
        self.0 >> FIXED_POINT_SHIFT
    }

    pub fn to_float(self) -> f32 {
        (self.0 as f32) / ((1 << FIXED_POINT_SHIFT) as f32)
    }
}

impl fmt::Display for FixedPoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_float())
    }
}

impl Div for FixedPoint {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        // PSX coordinates use 16 bits, with FIXED_POINT_SHIFT at 8 we should be able to shift
        // twice without overflow, so no need to cast to i64
        let n = self.0 << FIXED_POINT_SHIFT;
        let d = rhs.0;

        FixedPoint(n / d)
    }
}

#[test]
fn test_divide() {
    let ten = FixedPoint::new(10);
    let two = FixedPoint::new(2);

    assert_eq!(ten / two, FixedPoint::new(5));
    assert_eq!((two / ten).truncate(), 0);
}
