//! Port of zuyu/src/common/bit_field.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! In the C++ codebase, `BitField<Position, Bits, T>` is a union-based struct that
//! overlays bitfield access on a raw integer. In Rust, we cannot do the same union
//! trick safely. Instead, we provide:
//!
//! 1. A `BitFieldStorage` trait for types usable as bitfield backing storage.
//! 2. Free functions for extracting/inserting bit ranges.
//! 3. Concrete mask functions for each integer width.

use core::mem::size_of;

/// Trait for types that can be used as bitfield storage (u8, u16, u32, u64).
pub trait BitFieldStorage:
    Copy
    + Eq
    + core::ops::BitAnd<Output = Self>
    + core::ops::BitOr<Output = Self>
    + core::ops::Not<Output = Self>
    + core::ops::Shl<usize, Output = Self>
    + core::ops::Shr<usize, Output = Self>
    + From<u8>
{
    const ZERO: Self;
    const BITS: usize;

    /// Wrapping shift left (needed for mask computation).
    fn wrapping_shl(self, rhs: u32) -> Self;
    /// Wrapping shift right.
    fn wrapping_shr(self, rhs: u32) -> Self;
    /// Convert to i64 for sign-extension.
    fn as_i64(self) -> i64;
    /// Create from i64 (for sign-extended results).
    fn from_i64(v: i64) -> Self;
}

macro_rules! impl_bitfield_storage {
    ($($t:ty),*) => {
        $(
            impl BitFieldStorage for $t {
                const ZERO: Self = 0;
                const BITS: usize = size_of::<$t>() * 8;

                #[inline]
                fn wrapping_shl(self, rhs: u32) -> Self {
                    <$t>::wrapping_shl(self, rhs)
                }
                #[inline]
                fn wrapping_shr(self, rhs: u32) -> Self {
                    <$t>::wrapping_shr(self, rhs)
                }
                #[inline]
                fn as_i64(self) -> i64 {
                    self as i64
                }
                #[inline]
                fn from_i64(v: i64) -> Self {
                    v as $t
                }
            }
        )*
    };
}

impl_bitfield_storage!(u8, u16, u32, u64);

/// Compute mask for u8.
#[inline]
pub const fn mask_u8(position: usize, bits: usize) -> u8 {
    (0xFFu8.wrapping_shr((8 - bits) as u32)).wrapping_shl(position as u32)
}

/// Compute mask for u16.
#[inline]
pub const fn mask_u16(position: usize, bits: usize) -> u16 {
    (0xFFFFu16.wrapping_shr((16 - bits) as u32)).wrapping_shl(position as u32)
}

/// Compute mask for u32.
#[inline]
pub const fn mask_u32(position: usize, bits: usize) -> u32 {
    (0xFFFF_FFFFu32.wrapping_shr((32 - bits) as u32)).wrapping_shl(position as u32)
}

/// Compute mask for u64.
#[inline]
pub const fn mask_u64(position: usize, bits: usize) -> u64 {
    (0xFFFF_FFFF_FFFF_FFFFu64.wrapping_shr((64 - bits) as u32)).wrapping_shl(position as u32)
}

/// Extract an unsigned bitfield value from storage.
/// `position` is the bit offset from LSB, `bits` is the width.
#[inline]
pub fn extract_unsigned<T: BitFieldStorage>(storage: T, position: usize, bits: usize) -> T {
    let mask = (!T::ZERO).wrapping_shr((T::BITS - bits) as u32).wrapping_shl(position as u32);
    (storage & mask).wrapping_shr(position as u32)
}

/// Extract a signed bitfield value from storage (sign-extends).
#[inline]
pub fn extract_signed<T: BitFieldStorage>(storage: T, position: usize, bits: usize) -> T {
    let shift = (T::BITS - bits) as u32;
    let shifted = storage.wrapping_shl(shift - position as u32);
    // Arithmetic right shift for sign extension
    let as_signed = shifted.as_i64() >> shift as i64;
    T::from_i64(as_signed)
}

/// Format (pack) a value into a bitfield position.
#[inline]
pub fn format_value<T: BitFieldStorage>(value: T, position: usize, bits: usize) -> T {
    let mask = (!T::ZERO).wrapping_shr((T::BITS - bits) as u32).wrapping_shl(position as u32);
    value.wrapping_shl(position as u32) & mask
}

/// Assign a value to a bitfield within existing storage.
#[inline]
pub fn assign<T: BitFieldStorage>(storage: T, value: T, position: usize, bits: usize) -> T {
    let mask = (!T::ZERO).wrapping_shr((T::BITS - bits) as u32).wrapping_shl(position as u32);
    (storage & !mask) | format_value(value, position, bits)
}

/// Macro for defining bitfield accessors on a newtype wrapper struct.
///
/// Usage:
/// ```ignore
/// define_bit_field! {
///     pub struct MyRegister(u32);
///     field_a: u32, 0, 7;    // bits 0..6 (7 bits wide), type u32
///     field_b: u32, 7, 8;    // bits 7..14 (8 bits wide), type u32
/// }
/// ```
#[macro_export]
macro_rules! define_bit_field {
    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident($storage:ty);
        $(
            $(#[$field_meta:meta])*
            $field_vis:vis $field:ident : $field_ty:ty, $start:expr, $bits:expr
        );* $(;)?
    ) => {
        $(#[$meta])*
        #[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
        #[repr(transparent)]
        $vis struct $name {
            pub raw: $storage,
        }

        impl $name {
            /// Create a new instance from raw storage.
            #[inline]
            pub const fn new(raw: $storage) -> Self {
                Self { raw }
            }

            $(
                $(#[$field_meta])*
                #[inline]
                $field_vis fn $field(&self) -> $field_ty {
                    $crate::bit_field::extract_unsigned(self.raw, $start, $bits) as $field_ty
                }

                paste::paste! {
                    #[inline]
                    $field_vis fn [<set_ $field>](&mut self, value: $field_ty) {
                        self.raw = $crate::bit_field::assign(
                            self.raw,
                            value as $storage,
                            $start,
                            $bits,
                        );
                    }
                }
            )*
        }

        impl From<$storage> for $name {
            #[inline]
            fn from(raw: $storage) -> Self {
                Self { raw }
            }
        }

        impl From<$name> for $storage {
            #[inline]
            fn from(v: $name) -> Self {
                v.raw
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_unsigned_u32() {
        // bits 0..7 of 0xFF = 0x7F (7 bits)
        let val: u32 = 0b1111_1111;
        assert_eq!(extract_unsigned(val, 0, 7), 0b111_1111);
        assert_eq!(extract_unsigned(val, 7, 1), 1);
    }

    #[test]
    fn test_extract_unsigned_u64() {
        let val: u64 = 0xDEAD_BEEF_CAFE_BABE;
        let low16 = extract_unsigned(val, 0, 16);
        assert_eq!(low16, 0xBABE);
    }

    #[test]
    fn test_format_value() {
        let formatted: u32 = format_value(0b101u32, 4, 3);
        assert_eq!(formatted, 0b101_0000);
    }

    #[test]
    fn test_assign() {
        let storage: u32 = 0xFFFF_FFFF;
        let result = assign(storage, 0u32, 8, 8);
        assert_eq!(result, 0xFFFF_00FF);
    }

    #[test]
    fn test_extract_signed() {
        // -1 in 4 bits = 0b1111
        let storage: u32 = 0b1111;
        let val = extract_signed(storage, 0, 4);
        // Sign-extended -1 as u32 is 0xFFFFFFFF
        assert_eq!(val as i32, -1);
    }

    #[test]
    fn test_mask_u32() {
        assert_eq!(mask_u32(0, 7), 0x7F);
        assert_eq!(mask_u32(7, 8), 0x7F80);
        assert_eq!(mask_u32(0, 32), 0xFFFFFFFF);
    }
}
