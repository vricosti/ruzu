//! Port of zuyu/src/common/bit_util.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! Bit manipulation utilities: bit size, MSB finding, log2, power-of-two checks, etc.

/// Gets the size of a type T in bits. Use as `bit_size::<u32>()`.
#[inline]
pub const fn bit_size<T>() -> usize {
    core::mem::size_of::<T>() * 8
}

/// Returns the position (0-indexed from LSB) of the most significant set bit in a u32.
/// Value must be non-zero.
#[inline]
pub const fn most_significant_bit_32(value: u32) -> u32 {
    31 - value.leading_zeros()
}

/// Returns the position (0-indexed from LSB) of the most significant set bit in a u64.
/// Value must be non-zero.
#[inline]
pub const fn most_significant_bit_64(value: u64) -> u32 {
    63 - value.leading_zeros() as u32
}

/// Returns floor(log2(value)) for a u32. Value must be non-zero.
#[inline]
pub const fn log2_floor_32(value: u32) -> u32 {
    most_significant_bit_32(value)
}

/// Returns floor(log2(value)) for a u64. Value must be non-zero.
#[inline]
pub const fn log2_floor_64(value: u64) -> u32 {
    most_significant_bit_64(value)
}

/// Returns ceil(log2(value)) for a u32. Value must be non-zero.
#[inline]
pub const fn log2_ceil_32(value: u32) -> u32 {
    let log2_f = log2_floor_32(value);
    log2_f + ((value ^ (1u32 << log2_f)) != 0) as u32
}

/// Returns ceil(log2(value)) for a u64. Value must be non-zero.
#[inline]
pub const fn log2_ceil_64(value: u64) -> u32 {
    let log2_f = log2_floor_64(value);
    log2_f + ((value ^ (1u64 << log2_f)) != 0) as u32
}

/// Check if a value is a power of two. Equivalent to `std::has_single_bit`.
#[inline]
pub const fn is_pow2(value: u64) -> bool {
    value != 0 && (value & (value - 1)) == 0
}

/// Returns the smallest power of two >= value. Value must be > 0.
#[inline]
pub const fn next_pow2_u32(value: u32) -> u32 {
    1u32 << (32 - (value - 1).leading_zeros())
}

/// Returns the smallest power of two >= value. Value must be > 0.
#[inline]
pub const fn next_pow2_u64(value: u64) -> u64 {
    1u64 << (64 - (value - 1).leading_zeros())
}

/// Check if a specific bit (by index from LSB) is set.
#[inline]
pub const fn bit_u32(bit_index: usize, value: u32) -> bool {
    ((value >> bit_index) & 1) == 1
}

/// Check if a specific bit (by index from LSB) is set.
#[inline]
pub const fn bit_u64(bit_index: usize, value: u64) -> bool {
    ((value >> bit_index) & 1) == 1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bit_size() {
        assert_eq!(bit_size::<u8>(), 8);
        assert_eq!(bit_size::<u16>(), 16);
        assert_eq!(bit_size::<u32>(), 32);
        assert_eq!(bit_size::<u64>(), 64);
    }

    #[test]
    fn test_most_significant_bit() {
        assert_eq!(most_significant_bit_32(1), 0);
        assert_eq!(most_significant_bit_32(2), 1);
        assert_eq!(most_significant_bit_32(0xFF), 7);
        assert_eq!(most_significant_bit_64(1), 0);
        assert_eq!(most_significant_bit_64(0x8000_0000_0000_0000), 63);
    }

    #[test]
    fn test_log2_floor() {
        assert_eq!(log2_floor_32(1), 0);
        assert_eq!(log2_floor_32(4), 2);
        assert_eq!(log2_floor_32(5), 2);
        assert_eq!(log2_floor_64(8), 3);
    }

    #[test]
    fn test_log2_ceil() {
        assert_eq!(log2_ceil_32(1), 0);
        assert_eq!(log2_ceil_32(2), 1);
        assert_eq!(log2_ceil_32(3), 2);
        assert_eq!(log2_ceil_32(4), 2);
        assert_eq!(log2_ceil_32(5), 3);
    }

    #[test]
    fn test_is_pow2() {
        assert!(!is_pow2(0));
        assert!(is_pow2(1));
        assert!(is_pow2(2));
        assert!(!is_pow2(3));
        assert!(is_pow2(4));
    }

    #[test]
    fn test_next_pow2() {
        assert_eq!(next_pow2_u32(1), 1);
        assert_eq!(next_pow2_u32(2), 2);
        assert_eq!(next_pow2_u32(3), 4);
        assert_eq!(next_pow2_u32(5), 8);
        assert_eq!(next_pow2_u64(1), 1);
        assert_eq!(next_pow2_u64(3), 4);
    }

    #[test]
    fn test_bit() {
        assert!(bit_u32(0, 0b101));
        assert!(!bit_u32(1, 0b101));
        assert!(bit_u32(2, 0b101));
        assert!(bit_u64(63, 1u64 << 63));
    }
}
