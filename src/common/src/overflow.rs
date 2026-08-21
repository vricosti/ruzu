//! Port of zuyu/src/common/overflow.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! Overflow-checked arithmetic helpers for signed integers.

/// Wrapping addition for signed integers.
/// Performs the addition using unsigned arithmetic to avoid UB,
/// then reinterprets the result as signed.
#[inline]
pub const fn wrapping_add_i32(lhs: i32, rhs: i32) -> i32 {
    (lhs as u32).wrapping_add(rhs as u32) as i32
}

/// Wrapping addition for i64.
#[inline]
pub const fn wrapping_add_i64(lhs: i64, rhs: i64) -> i64 {
    (lhs as u64).wrapping_add(rhs as u64) as i64
}

/// Check if two signed i32 values can be added without overflow.
#[inline]
pub const fn can_add_without_overflow_i32(lhs: i32, rhs: i32) -> bool {
    lhs.checked_add(rhs).is_some()
}

/// Check if two signed i64 values can be added without overflow.
#[inline]
pub const fn can_add_without_overflow_i64(lhs: i64, rhs: i64) -> bool {
    lhs.checked_add(rhs).is_some()
}

/// Generic wrapping add using Rust's built-in wrapping arithmetic.
/// This is provided for convenience since Rust already has wrapping_add on primitives.
#[inline]
pub const fn wrapping_add_i8(lhs: i8, rhs: i8) -> i8 {
    (lhs as u8).wrapping_add(rhs as u8) as i8
}

/// Wrapping addition for i16.
#[inline]
pub const fn wrapping_add_i16(lhs: i16, rhs: i16) -> i16 {
    (lhs as u16).wrapping_add(rhs as u16) as i16
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_wrapping_add() {
        assert_eq!(wrapping_add_i32(1, 2), 3);
        assert_eq!(wrapping_add_i32(i32::MAX, 1), i32::MIN);
        assert_eq!(wrapping_add_i64(i64::MAX, 1), i64::MIN);
    }

    #[test]
    fn test_can_add_without_overflow() {
        assert!(can_add_without_overflow_i32(1, 2));
        assert!(can_add_without_overflow_i32(-1, 1));
        assert!(!can_add_without_overflow_i32(i32::MAX, 1));
        assert!(!can_add_without_overflow_i32(i32::MIN, -1));
        assert!(can_add_without_overflow_i32(i32::MAX, 0));
        assert!(can_add_without_overflow_i32(i32::MIN, 0));

        // Mixed signs can never overflow
        assert!(can_add_without_overflow_i32(i32::MAX, -1));
        assert!(can_add_without_overflow_i32(i32::MIN, 1));
    }

    #[test]
    fn test_can_add_without_overflow_i64() {
        assert!(can_add_without_overflow_i64(1, 2));
        assert!(!can_add_without_overflow_i64(i64::MAX, 1));
        assert!(!can_add_without_overflow_i64(i64::MIN, -1));
    }
}
