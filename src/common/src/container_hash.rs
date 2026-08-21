// SPDX-FileCopyrightText: 2005-2014 Daniel James
// SPDX-FileCopyrightText: 2016 Austin Appleby
// SPDX-License-Identifier: BSL-1.0

/// Rust equivalent of upstream `Common::HashValue(...)` overloads.
pub trait ContainerHashValue {
    fn container_hash_value(&self) -> usize;
}

/// Hashes an unsigned integer value using a multi-word splitting approach
/// matching the upstream C++ `detail::HashValue<T>`.
fn hash_value_unsigned(val: u128, val_bits: u32) -> usize {
    let size_t_bits = usize::BITS as u32;
    let length = (val_bits - 1) / size_t_bits;

    let mut seed: usize = 0;

    let mut i = length * size_t_bits;
    while i > 0 {
        seed ^= ((val >> i) as usize)
            .wrapping_add(seed << 6)
            .wrapping_add(seed >> 2);
        i -= size_t_bits;
    }

    seed ^= (val as usize)
        .wrapping_add(seed << 6)
        .wrapping_add(seed >> 2);

    seed
}

macro_rules! impl_unsigned_hash_value {
    ($($ty:ty),* $(,)?) => {
        $(
            impl ContainerHashValue for $ty {
                fn container_hash_value(&self) -> usize {
                    hash_value_unsigned(*self as u128, <$ty>::BITS)
                }
            }
        )*
    };
}

impl_unsigned_hash_value!(u8, u16, u32, u64, u128, usize);

impl<T: ContainerHashValue> ContainerHashValue for [T] {
    fn container_hash_value(&self) -> usize {
        hash_range(self.iter())
    }
}

impl<T: ContainerHashValue, const N: usize> ContainerHashValue for [T; N] {
    fn container_hash_value(&self) -> usize {
        self.as_slice().container_hash_value()
    }
}

impl<T: ContainerHashValue> ContainerHashValue for Vec<T> {
    fn container_hash_value(&self) -> usize {
        self.as_slice().container_hash_value()
    }
}

impl<T: ContainerHashValue + ?Sized> ContainerHashValue for &T {
    fn container_hash_value(&self) -> usize {
        (*self).container_hash_value()
    }
}

/// Combines a hash seed with a new value.
/// On 64-bit platforms, uses a MurmurHash-inspired mixing function.
/// On 32-bit platforms, uses the boost hash_combine formula.
fn hash_combine_impl(h: usize, k: usize) -> usize {
    #[cfg(target_pointer_width = "64")]
    {
        let m: u64 = (0xc6a4a793u64 << 32) + 0x5bd1e995;
        let r = 47;

        let mut k = k as u64;
        k = k.wrapping_mul(m);
        k ^= k >> r;
        k = k.wrapping_mul(m);

        let mut h = h as u64;
        h ^= k;
        h = h.wrapping_mul(m);

        // Completely arbitrary number, to prevent 0's from hashing to 0.
        h = h.wrapping_add(0xe6546b64);

        h as usize
    }

    #[cfg(not(target_pointer_width = "64"))]
    {
        h ^= k
            .wrapping_add(0x9e3779b9)
            .wrapping_add(h << 6)
            .wrapping_add(h >> 2);
        h
    }
}

/// Combines a hash seed with a new unsigned value, matching
/// upstream `Common::HashCombine`.
pub fn hash_combine<T: ContainerHashValue>(seed: &mut usize, v: T) {
    *seed = hash_combine_impl(*seed, v.container_hash_value());
}

/// Hashes a range of values (iterator), matching
/// upstream `Common::HashRange`.
pub fn hash_range<I>(iter: I) -> usize
where
    I: IntoIterator,
    I::Item: ContainerHashValue,
{
    let mut seed: usize = 0;
    for v in iter {
        hash_combine(&mut seed, v);
    }
    seed
}

/// Hashes an array/slice of unsigned values, matching upstream
/// `Common::HashValue` for `std::array`/`std::vector`.
pub fn hash_array(v: &[u64]) -> usize {
    v.container_hash_value()
}

/// Hashes a slice of bytes by treating each byte as a u64 value.
/// This preserves parity with the upstream which hashes element-by-element.
pub fn hash_byte_slice(v: &[u8]) -> usize {
    v.container_hash_value()
}

/// Hashes a slice of u32 values.
pub fn hash_u32_slice(v: &[u32]) -> usize {
    v.container_hash_value()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn upstream_hash_value_unsigned_u32(val: u32) -> usize {
        let mut seed: usize = 0;
        seed ^= (val as usize)
            .wrapping_add(seed << 6)
            .wrapping_add(seed >> 2);
        seed
    }

    fn upstream_hash_range_u32(v: &[u32]) -> usize {
        let mut seed: usize = 0;
        for &word in v {
            seed = hash_combine_impl(seed, upstream_hash_value_unsigned_u32(word));
        }
        seed
    }

    #[test]
    fn test_hash_combine_deterministic() {
        let mut seed1: usize = 0;
        hash_combine(&mut seed1, 42u64);
        let mut seed2: usize = 0;
        hash_combine(&mut seed2, 42u64);
        assert_eq!(seed1, seed2);
    }

    #[test]
    fn test_hash_combine_different_values() {
        let mut seed1: usize = 0;
        hash_combine(&mut seed1, 1u64);
        let mut seed2: usize = 0;
        hash_combine(&mut seed2, 2u64);
        assert_ne!(seed1, seed2);
    }

    #[test]
    fn test_hash_range() {
        let h1 = hash_range([1u64, 2, 3].iter().copied());
        let h2 = hash_range([1u64, 2, 3].iter().copied());
        assert_eq!(h1, h2);

        let h3 = hash_range([3u64, 2, 1].iter().copied());
        assert_ne!(h1, h3);
    }

    #[test]
    fn test_hash_array() {
        let arr = [10u64, 20, 30];
        let h = hash_array(&arr);
        assert_ne!(h, 0);
    }

    #[test]
    fn test_hash_empty() {
        let h = hash_range(std::iter::empty::<u64>());
        assert_eq!(h, 0);
    }

    #[test]
    fn test_hash_u32_slice_matches_upstream_reference() {
        let code = [
            0x0011_0071,
            0x0740_0451,
            0x0000_0301,
            0x0000_0041,
            0x0000_2041,
            0x0000_1841,
            0x0231_0021,
            0x0000_0841,
        ];

        assert_eq!(hash_u32_slice(&code), upstream_hash_range_u32(&code));
    }

    #[test]
    fn test_array_and_vec_hash_value_match_range() {
        let array = [1u32, 2, 3, 4];
        let vector = array.to_vec();

        assert_eq!(array.container_hash_value(), hash_range(array.iter()));
        assert_eq!(vector.container_hash_value(), hash_range(array.iter()));
        assert_eq!(array.container_hash_value(), vector.container_hash_value());
    }
}
