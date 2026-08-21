//! Port of zuyu/src/common/fixed_point.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! Fixed-point arithmetic type.
//! The C++ version is templated on `<I, F>` (integer bits, fractional bits).
//! In Rust, we use const generics. The underlying storage type is determined
//! by `I + F` total bits (8, 16, 32, or 64).
//!
//! Primary use in the codebase: `FixedPoint<32, 32>` (i.e., 64-bit storage
//! with 32 integer bits and 32 fractional bits).

use core::cmp::Ordering;
use core::fmt;
use core::ops::{
    Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Div, DivAssign,
    Mul, MulAssign, Neg, Not, Shl, ShlAssign, Shr, ShrAssign, Sub, SubAssign,
};

/// A fixed-point number with `I` integer bits and `F` fractional bits.
///
/// The total bit width is `I + F`. Currently supports total widths of
/// 8, 16, 32, and 64 bits.
///
/// The internal representation stores the value scaled by `2^F`.
#[derive(Clone, Copy, Default)]
pub struct FixedPoint<const I: usize, const F: usize> {
    pub data: i64,
}

impl<const I: usize, const F: usize> FixedPoint<I, F> {
    pub const FRACTIONAL_BITS: usize = F;
    pub const INTEGER_BITS: usize = I;
    pub const TOTAL_BITS: usize = I + F;

    /// The scaling factor: 1 in fixed-point representation.
    pub const ONE: i64 = 1i64 << F;

    pub const FRACTIONAL_MASK: i64 = Self::ONE - 1;
    pub const INTEGER_MASK: i64 = !Self::FRACTIONAL_MASK;

    /// Create a fixed-point value from a raw (pre-scaled) representation.
    #[inline]
    pub const fn from_base(raw: i64) -> Self {
        Self { data: raw }
    }

    /// Create a fixed-point value from an integer.
    #[inline]
    pub const fn from_int(n: i64) -> Self {
        Self {
            data: n * Self::ONE,
        }
    }

    /// Create a fixed-point value from a floating-point number.
    #[inline]
    pub fn from_f64(n: f64) -> Self {
        Self {
            data: (n * Self::ONE as f64) as i64,
        }
    }

    /// Create a fixed-point value from a f32.
    #[inline]
    pub fn from_f32(n: f32) -> Self {
        Self {
            data: (n * Self::ONE as f32) as i64,
        }
    }

    /// Get the raw internal representation.
    #[inline]
    pub const fn to_raw(self) -> i64 {
        self.data
    }

    /// Round up and convert to int.
    #[inline]
    pub fn to_int(self) -> i32 {
        let mut data = self.data;
        data += (data & Self::FRACTIONAL_MASK) >> 1;
        ((data & Self::INTEGER_MASK) >> F) as i32
    }

    /// Round up and convert to unsigned int.
    #[inline]
    pub fn to_uint(self) -> u32 {
        let mut data = self.data;
        data += (data & Self::FRACTIONAL_MASK) >> 1;
        ((data & Self::INTEGER_MASK) >> F) as u32
    }

    /// Round up and convert to i64.
    #[inline]
    pub fn to_long(self) -> i64 {
        let mut data = self.data;
        data += (data & Self::FRACTIONAL_MASK) >> 1;
        (data & Self::INTEGER_MASK) >> F
    }

    /// Floor conversion to int (no rounding).
    #[inline]
    pub const fn to_int_floor(self) -> i32 {
        ((self.data & Self::INTEGER_MASK) >> F) as i32
    }

    /// Floor conversion to i64 (no rounding).
    #[inline]
    pub const fn to_long_floor(self) -> i64 {
        (self.data & Self::INTEGER_MASK) >> F
    }

    /// Floor conversion to unsigned int (no rounding).
    #[inline]
    pub const fn to_uint_floor(self) -> u32 {
        ((self.data & Self::INTEGER_MASK) >> F) as u32
    }

    /// Convert to f32.
    #[inline]
    pub fn to_f32(self) -> f32 {
        self.data as f32 / Self::ONE as f32
    }

    /// Convert to f64.
    #[inline]
    pub fn to_f64(self) -> f64 {
        self.data as f64 / Self::ONE as f64
    }

    /// Clear the integer part, keeping only the fractional part.
    #[inline]
    pub fn clear_int(&mut self) {
        self.data &= Self::FRACTIONAL_MASK;
    }

    /// Get the fractional part as raw bits.
    #[inline]
    pub const fn get_frac(self) -> i64 {
        self.data & Self::FRACTIONAL_MASK
    }
}

// Comparison
impl<const I: usize, const F: usize> PartialEq for FixedPoint<I, F> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl<const I: usize, const F: usize> Eq for FixedPoint<I, F> {}

impl<const I: usize, const F: usize> PartialOrd for FixedPoint<I, F> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<const I: usize, const F: usize> Ord for FixedPoint<I, F> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.data.cmp(&other.data)
    }
}

// Arithmetic operators
impl<const I: usize, const F: usize> Add for FixedPoint<I, F> {
    type Output = Self;
    #[inline]
    fn add(self, rhs: Self) -> Self {
        Self::from_base(self.data + rhs.data)
    }
}

impl<const I: usize, const F: usize> AddAssign for FixedPoint<I, F> {
    #[inline]
    fn add_assign(&mut self, rhs: Self) {
        self.data += rhs.data;
    }
}

impl<const I: usize, const F: usize> Sub for FixedPoint<I, F> {
    type Output = Self;
    #[inline]
    fn sub(self, rhs: Self) -> Self {
        Self::from_base(self.data - rhs.data)
    }
}

impl<const I: usize, const F: usize> SubAssign for FixedPoint<I, F> {
    #[inline]
    fn sub_assign(&mut self, rhs: Self) {
        self.data -= rhs.data;
    }
}

impl<const I: usize, const F: usize> Mul for FixedPoint<I, F> {
    type Output = Self;
    #[inline]
    fn mul(self, rhs: Self) -> Self {
        // Use i128 for intermediate precision
        let product = self.data as i128 * rhs.data as i128;
        Self::from_base((product >> F) as i64)
    }
}

impl<const I: usize, const F: usize> MulAssign for FixedPoint<I, F> {
    #[inline]
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}

impl<const I: usize, const F: usize> Div for FixedPoint<I, F> {
    type Output = Self;
    #[inline]
    fn div(self, rhs: Self) -> Self {
        // Use i128 for intermediate precision
        let shifted = (self.data as i128) << F;
        Self::from_base((shifted / rhs.data as i128) as i64)
    }
}

impl<const I: usize, const F: usize> DivAssign for FixedPoint<I, F> {
    #[inline]
    fn div_assign(&mut self, rhs: Self) {
        *self = *self / rhs;
    }
}

impl<const I: usize, const F: usize> Neg for FixedPoint<I, F> {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        Self::from_base(-self.data)
    }
}

impl<const I: usize, const F: usize> Not for FixedPoint<I, F> {
    type Output = Self;
    #[inline]
    fn not(self) -> Self {
        Self::from_base(!self.data)
    }
}

// Bitwise operators
impl<const I: usize, const F: usize> BitAnd for FixedPoint<I, F> {
    type Output = Self;
    #[inline]
    fn bitand(self, rhs: Self) -> Self {
        Self::from_base(self.data & rhs.data)
    }
}

impl<const I: usize, const F: usize> BitAndAssign for FixedPoint<I, F> {
    #[inline]
    fn bitand_assign(&mut self, rhs: Self) {
        self.data &= rhs.data;
    }
}

impl<const I: usize, const F: usize> BitOr for FixedPoint<I, F> {
    type Output = Self;
    #[inline]
    fn bitor(self, rhs: Self) -> Self {
        Self::from_base(self.data | rhs.data)
    }
}

impl<const I: usize, const F: usize> BitOrAssign for FixedPoint<I, F> {
    #[inline]
    fn bitor_assign(&mut self, rhs: Self) {
        self.data |= rhs.data;
    }
}

impl<const I: usize, const F: usize> BitXor for FixedPoint<I, F> {
    type Output = Self;
    #[inline]
    fn bitxor(self, rhs: Self) -> Self {
        Self::from_base(self.data ^ rhs.data)
    }
}

impl<const I: usize, const F: usize> BitXorAssign for FixedPoint<I, F> {
    #[inline]
    fn bitxor_assign(&mut self, rhs: Self) {
        self.data ^= rhs.data;
    }
}

impl<const I: usize, const F: usize> Shl<i32> for FixedPoint<I, F> {
    type Output = Self;
    #[inline]
    fn shl(self, rhs: i32) -> Self {
        Self::from_base(self.data << rhs)
    }
}

impl<const I: usize, const F: usize> ShlAssign<i32> for FixedPoint<I, F> {
    #[inline]
    fn shl_assign(&mut self, rhs: i32) {
        self.data <<= rhs;
    }
}

impl<const I: usize, const F: usize> Shr<i32> for FixedPoint<I, F> {
    type Output = Self;
    #[inline]
    fn shr(self, rhs: i32) -> Self {
        Self::from_base(self.data >> rhs)
    }
}

impl<const I: usize, const F: usize> ShrAssign<i32> for FixedPoint<I, F> {
    #[inline]
    fn shr_assign(&mut self, rhs: i32) {
        self.data >>= rhs;
    }
}

impl<const I: usize, const F: usize> fmt::Debug for FixedPoint<I, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "FixedPoint<{}, {}>({})", I, F, self.to_f64())
    }
}

impl<const I: usize, const F: usize> fmt::Display for FixedPoint<I, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_f64())
    }
}

/// Common type aliases used in the codebase.
pub type FixedPoint32 = FixedPoint<16, 16>;
pub type FixedPoint64 = FixedPoint<32, 32>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_int() {
        let fp = FixedPoint::<16, 16>::from_int(5);
        assert_eq!(fp.to_int_floor(), 5);
    }

    #[test]
    fn test_from_f64() {
        let fp = FixedPoint::<16, 16>::from_f64(2.5);
        assert!((fp.to_f64() - 2.5).abs() < 0.001);
    }

    #[test]
    fn test_addition() {
        let a = FixedPoint::<16, 16>::from_int(3);
        let b = FixedPoint::<16, 16>::from_int(4);
        let c = a + b;
        assert_eq!(c.to_int_floor(), 7);
    }

    #[test]
    fn test_subtraction() {
        let a = FixedPoint::<16, 16>::from_int(10);
        let b = FixedPoint::<16, 16>::from_int(3);
        let c = a - b;
        assert_eq!(c.to_int_floor(), 7);
    }

    #[test]
    fn test_multiplication() {
        let a = FixedPoint::<16, 16>::from_int(3);
        let b = FixedPoint::<16, 16>::from_int(4);
        let c = a * b;
        assert_eq!(c.to_int_floor(), 12);
    }

    #[test]
    fn test_division() {
        let a = FixedPoint::<16, 16>::from_int(12);
        let b = FixedPoint::<16, 16>::from_int(4);
        let c = a / b;
        assert_eq!(c.to_int_floor(), 3);
    }

    #[test]
    fn test_negation() {
        let a = FixedPoint::<16, 16>::from_int(5);
        let b = -a;
        assert_eq!(b.to_int_floor(), -5);
    }

    #[test]
    fn test_comparison() {
        let a = FixedPoint::<16, 16>::from_int(3);
        let b = FixedPoint::<16, 16>::from_int(4);
        assert!(a < b);
        assert!(b > a);
        assert_eq!(a, a);
    }

    #[test]
    fn test_from_base() {
        let fp = FixedPoint::<16, 16>::from_base(0x0003_8000); // 3.5
        assert!((fp.to_f64() - 3.5).abs() < 0.001);
    }

    #[test]
    fn test_64bit() {
        let a = FixedPoint64::from_int(1_000_000);
        let b = FixedPoint64::from_int(2);
        let c = a * b;
        assert_eq!(c.to_long_floor(), 2_000_000);
    }
}
