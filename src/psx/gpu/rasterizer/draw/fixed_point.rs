//! Fixed point implementation for the drawing code

// Clippy doesn't like our arithmetic implementations because they don't use the operators it
// expect (i.e. division uses shifts etc...)
#![allow(clippy::suspicious_arithmetic_impl)]

use std::fmt;
use std::ops::{Add, AddAssign, Mul, Sub, SubAssign};

/// The number of bits used for the fractional part of a FpCoord value.
///
/// I'm not entirely sure what this value is on the real console (or even if it's really how the
/// drawing algorithm is really implemented).
const FIXED_POINT_SHIFT: u32 = 32;

/// Fixed point representation of a number.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FpCoord(i64);

impl FpCoord {
    pub fn new(v: i32) -> FpCoord {
        FpCoord(i64::from(v) << FIXED_POINT_SHIFT)
    }

    /// Create a new FpCoord value that's equal to the largest possible value that's less than
    /// `v + 1`, in other words: `v + 1 - epsilon()`
    pub fn new_saturated(v: i32) -> FpCoord {
        let f = FpCoord::new(v + 1);

        f - FpCoord::epsilon()
    }

    /// Create a new dx/dy slope ratio. The result is rounded to the available precision away from
    /// 0. `dy` *must* be greater than 0.
    pub fn new_dxdy(dx: i32, dy: i32) -> FpCoord {
        debug_assert!(dy > 0);

        let mut fpdx = FpCoord::new(dx);
        let dy = i64::from(dy);

        // We want the division to round away from 0. We know that dy is positive, so the sign of
        // the result will be that of dx. So if dx is negative we need to do `(dx - dy + 1) / dy`
        // and if it's positive `(dx + dy - 1) / dy`.
        let bias = dy - 1;

        if dx > 0 {
            fpdx.0 += bias;
        } else {
            fpdx.0 -= bias;
        }

        FpCoord(fpdx.0 / dy)
    }

    /// Returns the smallest possible FpCoord value > 0
    pub fn epsilon() -> FpCoord {
        FpCoord(1)
    }

    pub fn truncate(self) -> i32 {
        (self.0 >> FIXED_POINT_SHIFT) as i32
    }

    pub fn to_float(self) -> f32 {
        (self.0 as f32) / ((1i64 << FIXED_POINT_SHIFT) as f32)
    }
}

impl fmt::Display for FpCoord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_float())
    }
}

impl Add for FpCoord {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        FpCoord(self.0 + other.0)
    }
}

impl AddAssign for FpCoord {
    fn add_assign(&mut self, other: Self) {
        *self = *self + other;
    }
}

impl Sub for FpCoord {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        FpCoord(self.0 - other.0)
    }
}

impl SubAssign for FpCoord {
    fn sub_assign(&mut self, other: Self) {
        *self = *self - other;
    }
}

impl Mul<i32> for FpCoord {
    type Output = Self;

    fn mul(self, rhs: i32) -> Self::Output {
        FpCoord(self.0 * i64::from(rhs))
    }
}

#[test]
fn test_add() {
    let ten = FpCoord::new(10);
    let two = FpCoord::new(2);
    let mtwo = FpCoord::new(-2);

    assert_eq!(ten + two, FpCoord::new(12));
    assert_eq!(two + ten, FpCoord::new(12));
    assert_eq!(ten + mtwo, FpCoord::new(8));
    assert_eq!(mtwo + ten, FpCoord::new(8));
}

#[test]
fn test_sub() {
    let ten = FpCoord::new(10);
    let two = FpCoord::new(2);
    let mtwo = FpCoord::new(-2);

    assert_eq!(ten - two, FpCoord::new(8));
    assert_eq!(two - ten, FpCoord::new(-8));
    assert_eq!(ten - mtwo, FpCoord::new(12));
    assert_eq!(mtwo - ten, FpCoord::new(-12));
}

#[test]
fn test_mul_i32() {
    let ten = FpCoord::new(10);
    let two = FpCoord::new(2);

    assert_eq!(ten * 8, FpCoord::new(80));
    assert_eq!(two * 13, FpCoord::new(26));
    assert_eq!(two * -13, FpCoord::new(-26));
    assert_eq!(two * 0, FpCoord::new(0));
}
