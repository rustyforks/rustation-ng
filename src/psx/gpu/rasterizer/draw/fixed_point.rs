//! Fixed point implementation for the drawing code

// Clippy doesn't like our arithmetic implementations because they don't use the operators it
// expect (i.e. division uses shifts etc...)
#![allow(clippy::suspicious_arithmetic_impl)]

use std::fmt;
use std::ops::{Add, AddAssign, Mul, Sub, SubAssign};

/// The number of bits used for the fractional part of a FixedPoint value.
///
/// I'm not entirely sure what this value is on the real console (or even if it's really how the
/// drawing algorithm is really implemented).
const FIXED_POINT_SHIFT: u32 = 32;

/// Fixed point representation of a number.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FixedPoint(i64);

impl FixedPoint {
    pub fn new(v: i32) -> FixedPoint {
        FixedPoint(i64::from(v) << FIXED_POINT_SHIFT)
    }

    /// Create a new FixedPoint value that's equal to the largest possible value that's less than
    /// `v + 1`, in other words: `v + 1 - epsilon()`
    pub fn new_saturated(v: i32) -> FixedPoint {
        let f = FixedPoint::new(v + 1);

        f - FixedPoint::epsilon()
    }

    /// Create a new dx/dy slope ratio. The result is rounded to the available precision away from
    /// 0. `dy` *must* be greater than 0.
    pub fn new_dxdy(dx: i32, dy: i32) -> FixedPoint {
        debug_assert!(dy > 0);

        let mut fpdx = FixedPoint::new(dx);
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

        FixedPoint(fpdx.0 / dy)
    }

    /// Returns the smallest possible FixedPoint value > 0
    pub fn epsilon() -> FixedPoint {
        FixedPoint(1)
    }

    pub fn truncate(self) -> i32 {
        (self.0 >> FIXED_POINT_SHIFT) as i32
    }

    pub fn to_float(self) -> f32 {
        (self.0 as f32) / ((1i64 << FIXED_POINT_SHIFT) as f32)
    }
}

impl fmt::Display for FixedPoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_float())
    }
}

impl Add for FixedPoint {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        FixedPoint(self.0 + other.0)
    }
}

impl AddAssign for FixedPoint {
    fn add_assign(&mut self, other: Self) {
        *self = *self + other;
    }
}

impl Sub for FixedPoint {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        FixedPoint(self.0 - other.0)
    }
}

impl SubAssign for FixedPoint {
    fn sub_assign(&mut self, other: Self) {
        *self = *self - other;
    }
}

impl Mul<i32> for FixedPoint {
    type Output = Self;

    fn mul(self, rhs: i32) -> Self::Output {
        FixedPoint(self.0 * i64::from(rhs))
    }
}

#[test]
fn test_add() {
    let ten = FixedPoint::new(10);
    let two = FixedPoint::new(2);
    let mtwo = FixedPoint::new(-2);

    assert_eq!(ten + two, FixedPoint::new(12));
    assert_eq!(two + ten, FixedPoint::new(12));
    assert_eq!(ten + mtwo, FixedPoint::new(8));
    assert_eq!(mtwo + ten, FixedPoint::new(8));
}

#[test]
fn test_sub() {
    let ten = FixedPoint::new(10);
    let two = FixedPoint::new(2);
    let mtwo = FixedPoint::new(-2);

    assert_eq!(ten - two, FixedPoint::new(8));
    assert_eq!(two - ten, FixedPoint::new(-8));
    assert_eq!(ten - mtwo, FixedPoint::new(12));
    assert_eq!(mtwo - ten, FixedPoint::new(-12));
}

#[test]
fn test_mul_i32() {
    let ten = FixedPoint::new(10);
    let two = FixedPoint::new(2);

    assert_eq!(ten * 8, FixedPoint::new(80));
    assert_eq!(two * 13, FixedPoint::new(26));
    assert_eq!(two * -13, FixedPoint::new(-26));
    assert_eq!(two * 0, FixedPoint::new(0));
}
