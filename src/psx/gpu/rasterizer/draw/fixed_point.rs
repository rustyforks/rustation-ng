//! Fixed point implementation for the drawing code

// Clippy doesn't like our arithmetic implementations because they don't use the operators it
// expect (i.e. division uses shifts etc...)
#![allow(clippy::suspicious_arithmetic_impl)]

use std::fmt;
use std::ops::{Add, AddAssign, Div, Sub};

/// The number of bits used for the fractional part of a FixedPoint value.
///
/// I'm not really sure how the PSX GPU works internally but if it does use FP the way we do here
/// it must use *at least* 9bits for the fractional part since it allows drawing up to 512 pixels
/// in height. Actually for gouraud shading it's probably 10 bits since we can go up to 1024 pixels
/// horizontally.
const FIXED_POINT_SHIFT: u32 = 16;

/// Fixed point representation of a number.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FixedPoint(i32);

impl FixedPoint {
    pub fn new(v: i32) -> FixedPoint {
        FixedPoint(v << FIXED_POINT_SHIFT)
    }

    /// Returns the smallest possible FixedPoint value > 0
    pub fn epsilon() -> FixedPoint {
        FixedPoint(1)
    }

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
        let n = (self.0 as i64) << FIXED_POINT_SHIFT;
        let d = rhs.0 as i64;

        debug_assert!(d != 0);

        let q = n / d;

        debug_assert!(q >= ::std::i32::MIN as i64);
        debug_assert!(q <= ::std::i32::MAX as i64);

        FixedPoint(q as i32)
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

#[test]
fn test_divide() {
    let ten = FixedPoint::new(10);
    let two = FixedPoint::new(2);
    let mtwo = FixedPoint::new(-2);

    assert_eq!(ten / two, FixedPoint::new(5));
    assert_eq!((two / ten).truncate(), 0);

    assert_eq!(ten / mtwo, FixedPoint::new(-5));
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
