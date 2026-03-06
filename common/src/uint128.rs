//! Port of zuyu/src/common/uint128.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! 128-bit integer support. Rust has native u128, but the C++ codebase uses
//! `std::array<u64, 2>` as its u128 type. We provide helper functions that
//! match the C++ API, using Rust's native u128 for the actual computation.

/// The C++ codebase defines `u128 = std::array<u64, 2>`.
/// In Rust we use native u128 for computation, but provide conversion helpers
/// for the [u64; 2] representation used in the C++ code.
pub type U128 = [u64; 2];

/// Convert a [u64; 2] (little-endian pair) to u128.
#[inline]
pub const fn u128_from_pair(pair: U128) -> u128 {
    pair[0] as u128 | ((pair[1] as u128) << 64)
}

/// Convert a u128 to [u64; 2] (little-endian pair).
#[inline]
pub const fn u128_to_pair(value: u128) -> U128 {
    [value as u64, (value >> 64) as u64]
}

/// Multiply two u64 values and divide by a u64 value.
/// Equivalent to `(a * b) / d` with 128-bit intermediate precision.
#[inline]
pub fn multiply_and_divide64(a: u64, b: u64, d: u64) -> u64 {
    let product = a as u128 * b as u128;
    (product / d as u128) as u64
}

/// Multiply two u64 values producing a u128 result (as [u64; 2]).
#[inline]
pub fn multiply64_into128(a: u64, b: u64) -> U128 {
    let result = a as u128 * b as u128;
    u128_to_pair(result)
}

/// Get a fixed-point 64-bit factor: `(numerator << 64) / divisor`.
#[inline]
pub fn get_fixed_point64_factor(numerator: u64, divisor: u64) -> u64 {
    let base = (numerator as u128) << 64;
    (base / divisor as u128) as u64
}

/// Return the high 64 bits of a u64 * u64 multiplication.
#[inline]
pub fn multiply_high(a: u64, b: u64) -> u64 {
    let product = a as u128 * b as u128;
    (product >> 64) as u64
}

/// Divide a u128 (as [u64; 2]) by a u32, returning (quotient, remainder).
#[inline]
pub fn divide128_on32(dividend: U128, divisor: u32) -> (u64, u64) {
    let val = u128_from_pair(dividend);
    let d = divisor as u128;
    let quotient = (val / d) as u64;
    let remainder = (val % d) as u64;
    (quotient, remainder)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_u128_pair_roundtrip() {
        let value: u128 = 0xDEAD_BEEF_CAFE_BABE_1234_5678_9ABC_DEF0;
        let pair = u128_to_pair(value);
        assert_eq!(u128_from_pair(pair), value);
    }

    #[test]
    fn test_multiply_and_divide64() {
        assert_eq!(multiply_and_divide64(100, 200, 50), 400);
        assert_eq!(multiply_and_divide64(u64::MAX, 2, 2), u64::MAX);
    }

    #[test]
    fn test_multiply64_into128() {
        let result = multiply64_into128(0xFFFF_FFFF_FFFF_FFFF, 2);
        let val = u128_from_pair(result);
        assert_eq!(val, 0xFFFF_FFFF_FFFF_FFFF_u128 * 2);
    }

    #[test]
    fn test_multiply_high() {
        // 2^63 * 2 = 2^64, high part = 1
        assert_eq!(multiply_high(1u64 << 63, 2), 1);
        assert_eq!(multiply_high(1, 1), 0);
    }

    #[test]
    fn test_get_fixed_point64_factor() {
        // (1 << 64) / 1 = 2^64 truncated to u64 = 0 (overflow)
        // (1 << 64) / 2 = 2^63
        assert_eq!(get_fixed_point64_factor(1, 2), 1u64 << 63);
    }

    #[test]
    fn test_divide128_on32() {
        let dividend = u128_to_pair(1000u128);
        let (quot, rem) = divide128_on32(dividend, 7);
        assert_eq!(quot, 142);
        assert_eq!(rem, 6);
    }
}
