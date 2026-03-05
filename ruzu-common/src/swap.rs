//! Port of zuyu/src/common/swap.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! Endianness swapping types and utilities.
//! On little-endian systems (which is the target), LE types are no-ops
//! and BE types perform byte swapping. Rust's `to_le_bytes`/`to_be_bytes`
//! provide the low-level swapping; we wrap them in newtype structs.

/// Swap bytes of a u16.
#[inline]
pub const fn swap16(data: u16) -> u16 {
    data.swap_bytes()
}

/// Swap bytes of a u32.
#[inline]
pub const fn swap32(data: u32) -> u32 {
    data.swap_bytes()
}

/// Swap bytes of a u64.
#[inline]
pub const fn swap64(data: u64) -> u64 {
    data.swap_bytes()
}

/// Swap bytes of a f32.
#[inline]
pub fn swapf(f: f32) -> f32 {
    f32::from_bits(f.to_bits().swap_bytes())
}

/// Swap bytes of a f64.
#[inline]
pub fn swapd(f: f64) -> f64 {
    f64::from_bits(f.to_bits().swap_bytes())
}

/// Trait for types that support endianness conversion.
pub trait Swappable: Copy + Default {
    /// Convert from native endian to big endian.
    fn to_be(self) -> Self;
    /// Convert from big endian to native endian.
    fn from_be(self) -> Self;
    /// Convert from native endian to little endian.
    fn to_le(self) -> Self;
    /// Convert from little endian to native endian.
    fn from_le(self) -> Self;
}

macro_rules! impl_swappable_int {
    ($($t:ty),*) => {
        $(
            impl Swappable for $t {
                #[inline] fn to_be(self) -> Self { <$t>::to_be(self) }
                #[inline] fn from_be(self) -> Self { <$t>::from_be(self) }
                #[inline] fn to_le(self) -> Self { <$t>::to_le(self) }
                #[inline] fn from_le(self) -> Self { <$t>::from_le(self) }
            }
        )*
    };
}

impl_swappable_int!(u8, u16, u32, u64, i8, i16, i32, i64);

impl Swappable for f32 {
    #[inline]
    fn to_be(self) -> Self {
        f32::from_bits(self.to_bits().to_be())
    }
    #[inline]
    fn from_be(self) -> Self {
        f32::from_bits(self.to_bits().from_be())
    }
    #[inline]
    fn to_le(self) -> Self {
        f32::from_bits(self.to_bits().to_le())
    }
    #[inline]
    fn from_le(self) -> Self {
        f32::from_bits(self.to_bits().from_le())
    }
}

impl Swappable for f64 {
    #[inline]
    fn to_be(self) -> Self {
        f64::from_bits(self.to_bits().to_be())
    }
    #[inline]
    fn from_be(self) -> Self {
        f64::from_bits(self.to_bits().from_be())
    }
    #[inline]
    fn to_le(self) -> Self {
        f64::from_bits(self.to_bits().to_le())
    }
    #[inline]
    fn from_le(self) -> Self {
        f64::from_bits(self.to_bits().from_le())
    }
}

/// A little-endian wrapper type. On little-endian systems this is essentially
/// a no-op wrapper. On big-endian systems, byte-swapping occurs on read/write.
///
/// Corresponds to the C++ `u16_le`, `u32_le`, `u64_le`, etc. type aliases.
#[derive(Clone, Copy, Default, Eq)]
#[repr(transparent)]
pub struct Le<T: Swappable>(T);

impl<T: Swappable> Le<T> {
    /// Create a new LE value from a native-endian value.
    #[inline]
    pub fn new(value: T) -> Self {
        Self(value.to_le())
    }

    /// Get the native-endian value.
    #[inline]
    pub fn get(self) -> T {
        T::from_le(self.0)
    }

    /// Get the raw (stored) value without conversion.
    #[inline]
    pub fn raw(self) -> T {
        self.0
    }
}

impl<T: Swappable + core::fmt::Debug> core::fmt::Debug for Le<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.get().fmt(f)
    }
}

impl<T: Swappable + PartialEq> PartialEq for Le<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        // Compare raw bytes directly - same endianness means same bytes = same value
        self.0 == other.0
    }
}

impl<T: Swappable + PartialOrd + Ord> PartialOrd for Le<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Swappable + Ord> Ord for Le<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.get().cmp(&other.get())
    }
}

impl<T: Swappable + core::hash::Hash> core::hash::Hash for Le<T> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T: Swappable> From<T> for Le<T> {
    #[inline]
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

/// A big-endian wrapper type. On big-endian systems this is essentially
/// a no-op wrapper. On little-endian systems, byte-swapping occurs on read/write.
///
/// Corresponds to the C++ `u16_be`, `u32_be`, `u64_be`, etc. type aliases.
#[derive(Clone, Copy, Default, Eq)]
#[repr(transparent)]
pub struct Be<T: Swappable>(T);

impl<T: Swappable> Be<T> {
    /// Create a new BE value from a native-endian value.
    #[inline]
    pub fn new(value: T) -> Self {
        Self(value.to_be())
    }

    /// Get the native-endian value.
    #[inline]
    pub fn get(self) -> T {
        T::from_be(self.0)
    }

    /// Get the raw (stored) value without conversion.
    #[inline]
    pub fn raw(self) -> T {
        self.0
    }
}

impl<T: Swappable + core::fmt::Debug> core::fmt::Debug for Be<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.get().fmt(f)
    }
}

impl<T: Swappable + PartialEq> PartialEq for Be<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: Swappable + PartialOrd + Ord> PartialOrd for Be<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Swappable + Ord> Ord for Be<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.get().cmp(&other.get())
    }
}

impl<T: Swappable + core::hash::Hash> core::hash::Hash for Be<T> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T: Swappable> From<T> for Be<T> {
    #[inline]
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

// Type aliases matching the C++ naming convention
pub type U16Le = Le<u16>;
pub type U32Le = Le<u32>;
pub type U64Le = Le<u64>;
pub type S16Le = Le<i16>;
pub type S32Le = Le<i32>;
pub type S64Le = Le<i64>;
pub type F32Le = Le<f32>;
pub type F64Le = Le<f64>;

pub type U16Be = Be<u16>;
pub type U32Be = Be<u32>;
pub type U64Be = Be<u64>;
pub type S16Be = Be<i16>;
pub type S32Be = Be<i32>;
pub type S64Be = Be<i64>;
pub type F32Be = Be<f32>;
pub type F64Be = Be<f64>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_swap_functions() {
        assert_eq!(swap16(0x0102), 0x0201);
        assert_eq!(swap32(0x01020304), 0x04030201);
        assert_eq!(swap64(0x0102030405060708), 0x0807060504030201);
    }

    #[test]
    fn test_le_wrapper() {
        let val = Le::new(0x1234u16);
        assert_eq!(val.get(), 0x1234u16);

        // On little-endian, raw should equal the value
        #[cfg(target_endian = "little")]
        assert_eq!(val.raw(), 0x1234u16);
    }

    #[test]
    fn test_be_wrapper() {
        let val = Be::new(0x1234u16);
        assert_eq!(val.get(), 0x1234u16);

        // On little-endian, raw should be swapped
        #[cfg(target_endian = "little")]
        assert_eq!(val.raw(), 0x3412u16);
    }

    #[test]
    fn test_le_comparison() {
        assert_eq!(Le::new(42u32), Le::new(42u32));
        assert_ne!(Le::new(42u32), Le::new(43u32));
    }

    #[test]
    fn test_be_comparison() {
        assert_eq!(Be::new(42u32), Be::new(42u32));
        assert_ne!(Be::new(42u32), Be::new(43u32));
    }

    #[test]
    fn test_from() {
        let le: Le<u32> = 42u32.into();
        assert_eq!(le.get(), 42);
        let be: Be<u32> = 42u32.into();
        assert_eq!(be.get(), 42);
    }

    #[test]
    fn test_float_swap() {
        let f: f32 = 1.0;
        let swapped = swapf(f);
        let back = swapf(swapped);
        assert_eq!(f, back);
    }
}
