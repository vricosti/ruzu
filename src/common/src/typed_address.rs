//! Port of zuyu/src/common/typed_address.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! Typed address wrappers providing type safety for different address spaces.
//! The C++ version uses `TypedAddress<bool Virtual, typename Tag>`.
//! In Rust we use distinct newtypes for each address type.

use core::fmt;
use core::hash::{Hash, Hasher};
use core::ops::{Add, AddAssign, Sub, SubAssign};

/// Macro to define a typed address wrapper.
macro_rules! define_typed_address {
    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident;
    ) => {
        $(#[$meta])*
        #[derive(Clone, Copy, Default, Eq)]
        #[repr(transparent)]
        $vis struct $name(u64);

        impl $name {
            /// Null address constant.
            pub const NULL: Self = Self(0);

            /// Create a new typed address from a raw u64 value.
            #[inline]
            pub const fn new(addr: u64) -> Self {
                Self(addr)
            }

            /// Get the raw u64 value.
            #[inline]
            pub const fn get(self) -> u64 {
                self.0
            }

            /// Check if the address is null (zero).
            #[inline]
            pub const fn is_null(self) -> bool {
                self.0 == 0
            }
        }

        // Arithmetic: TypedAddress + integer
        impl Add<u64> for $name {
            type Output = Self;
            #[inline]
            fn add(self, rhs: u64) -> Self {
                Self(self.0 + rhs)
            }
        }

        impl Add<usize> for $name {
            type Output = Self;
            #[inline]
            fn add(self, rhs: usize) -> Self {
                Self(self.0 + rhs as u64)
            }
        }

        impl Add<i64> for $name {
            type Output = Self;
            #[inline]
            fn add(self, rhs: i64) -> Self {
                Self((self.0 as i64 + rhs) as u64)
            }
        }

        // TypedAddress + TypedAddress
        impl Add<$name> for $name {
            type Output = Self;
            #[inline]
            fn add(self, rhs: Self) -> Self {
                Self(self.0 + rhs.0)
            }
        }

        // TypedAddress += integer
        impl AddAssign<u64> for $name {
            #[inline]
            fn add_assign(&mut self, rhs: u64) {
                self.0 += rhs;
            }
        }

        impl AddAssign<usize> for $name {
            #[inline]
            fn add_assign(&mut self, rhs: usize) {
                self.0 += rhs as u64;
            }
        }

        impl AddAssign<i64> for $name {
            #[inline]
            fn add_assign(&mut self, rhs: i64) {
                self.0 = (self.0 as i64 + rhs) as u64;
            }
        }

        // TypedAddress - integer
        impl Sub<u64> for $name {
            type Output = Self;
            #[inline]
            fn sub(self, rhs: u64) -> Self {
                Self(self.0 - rhs)
            }
        }

        impl Sub<usize> for $name {
            type Output = Self;
            #[inline]
            fn sub(self, rhs: usize) -> Self {
                Self(self.0 - rhs as u64)
            }
        }

        // TypedAddress - TypedAddress = ptrdiff (i64)
        impl Sub<$name> for $name {
            type Output = i64;
            #[inline]
            fn sub(self, rhs: Self) -> i64 {
                self.0 as i64 - rhs.0 as i64
            }
        }

        // TypedAddress -= integer
        impl SubAssign<u64> for $name {
            #[inline]
            fn sub_assign(&mut self, rhs: u64) {
                self.0 -= rhs;
            }
        }

        impl SubAssign<usize> for $name {
            #[inline]
            fn sub_assign(&mut self, rhs: usize) {
                self.0 -= rhs as u64;
            }
        }

        // Logical/bitwise operators returning u64 (matching C++ behavior)
        impl core::ops::BitAnd<u64> for $name {
            type Output = u64;
            #[inline]
            fn bitand(self, mask: u64) -> u64 {
                self.0 & mask
            }
        }

        impl core::ops::BitOr<u64> for $name {
            type Output = u64;
            #[inline]
            fn bitor(self, mask: u64) -> u64 {
                self.0 | mask
            }
        }

        impl core::ops::Shl<i32> for $name {
            type Output = u64;
            #[inline]
            fn shl(self, shift: i32) -> u64 {
                self.0 << shift
            }
        }

        impl core::ops::Shr<i32> for $name {
            type Output = u64;
            #[inline]
            fn shr(self, shift: i32) -> u64 {
                self.0 >> shift
            }
        }

        impl core::ops::Div<u64> for $name {
            type Output = u64;
            #[inline]
            fn div(self, rhs: u64) -> u64 {
                self.0 / rhs
            }
        }

        // Comparison
        impl PartialEq for $name {
            #[inline]
            fn eq(&self, other: &Self) -> bool {
                self.0 == other.0
            }
        }

        impl PartialEq<u64> for $name {
            #[inline]
            fn eq(&self, other: &u64) -> bool {
                self.0 == *other
            }
        }

        impl PartialOrd for $name {
            #[inline]
            fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        impl Ord for $name {
            #[inline]
            fn cmp(&self, other: &Self) -> core::cmp::Ordering {
                self.0.cmp(&other.0)
            }
        }

        impl Hash for $name {
            #[inline]
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.0.hash(state);
            }
        }

        // Conversion from u64
        impl From<u64> for $name {
            #[inline]
            fn from(addr: u64) -> Self {
                Self(addr)
            }
        }

        impl From<$name> for u64 {
            #[inline]
            fn from(addr: $name) -> u64 {
                addr.0
            }
        }

        // Display/Debug
        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}({:#x})", stringify!($name), self.0)
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{:#x}", self.0)
            }
        }

        impl fmt::LowerHex for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::LowerHex::fmt(&self.0, f)
            }
        }

        impl fmt::UpperHex for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::UpperHex::fmt(&self.0, f)
            }
        }
    };
}

define_typed_address! {
    /// A physical address in the ARM physical address space.
    pub struct PhysicalAddress;
}

define_typed_address! {
    /// A virtual address in the userspace virtual address space.
    pub struct VirtualAddress;
}

define_typed_address! {
    /// A process-specific virtual address.
    pub struct ProcessAddress;
}

/// Helper function to get the raw integer value from any typed address.
#[inline]
pub fn get_integer(addr: impl Into<u64>) -> u64 {
    addr.into()
}

/// Trait for identifying typed address types, matching the C++ `IsTypedAddress` concept.
pub trait IsTypedAddress: Copy + From<u64> + Into<u64> + Eq + Ord + Hash {
    const NULL: Self;
    fn get(self) -> u64;
    fn is_null(self) -> bool;
}

impl IsTypedAddress for PhysicalAddress {
    const NULL: Self = Self::NULL;
    #[inline]
    fn get(self) -> u64 {
        self.get()
    }
    #[inline]
    fn is_null(self) -> bool {
        self.is_null()
    }
}

impl IsTypedAddress for VirtualAddress {
    const NULL: Self = Self::NULL;
    #[inline]
    fn get(self) -> u64 {
        self.get()
    }
    #[inline]
    fn is_null(self) -> bool {
        self.is_null()
    }
}

impl IsTypedAddress for ProcessAddress {
    const NULL: Self = Self::NULL;
    #[inline]
    fn get(self) -> u64 {
        self.get()
    }
    #[inline]
    fn is_null(self) -> bool {
        self.is_null()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_size() {
        assert_eq!(core::mem::size_of::<PhysicalAddress>(), 8);
        assert_eq!(core::mem::size_of::<VirtualAddress>(), 8);
        assert_eq!(core::mem::size_of::<ProcessAddress>(), 8);
    }

    #[test]
    fn test_null() {
        assert!(PhysicalAddress::NULL.is_null());
        assert_eq!(PhysicalAddress::NULL, 0u64);
    }

    #[test]
    fn test_arithmetic() {
        let a = PhysicalAddress::new(10);
        assert_eq!((a + 5u64).get(), 15);
        assert_eq!((a - 5u64).get(), 5);

        let b = PhysicalAddress::new(15);
        let diff: i64 = b - a;
        assert_eq!(diff, 5);
    }

    #[test]
    fn test_add_assign() {
        let mut a = PhysicalAddress::new(10);
        a += 5u64;
        assert_eq!(a.get(), 15);
    }

    #[test]
    fn test_sub_assign() {
        let mut a = PhysicalAddress::new(10);
        a -= 5u64;
        assert_eq!(a.get(), 5);
    }

    #[test]
    fn test_bitwise() {
        let a = PhysicalAddress::new(0xFF);
        assert_eq!(a & 0x0Fu64, 0x0F);
        assert_eq!(a | 0x100u64, 0x1FF);
        assert_eq!(a >> 4, 0x0F);
        assert_eq!(a << 4, 0xFF0);
    }

    #[test]
    fn test_comparison() {
        let a = PhysicalAddress::new(10);
        let b = PhysicalAddress::new(20);
        assert!(a < b);
        assert!(b > a);
        assert_eq!(a, a);
        assert_ne!(a, b);
    }

    #[test]
    fn test_copy_constructor() {
        let a = PhysicalAddress::new(5);
        let b = a;
        assert_eq!(b, PhysicalAddress::new(5));
    }

    #[test]
    fn test_from_u64() {
        let addr: PhysicalAddress = 42u64.into();
        assert_eq!(addr.get(), 42);
        let val: u64 = addr.into();
        assert_eq!(val, 42);
    }

    #[test]
    fn test_increment_decrement() {
        // Post-increment equivalent
        let a = PhysicalAddress::new(10);
        let b = a + 1u64;
        assert_eq!(b, PhysicalAddress::new(11));

        // Pre-decrement equivalent
        let c = a - 1u64;
        assert_eq!(c, PhysicalAddress::new(9));
    }
}
