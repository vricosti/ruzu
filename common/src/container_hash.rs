// SPDX-FileCopyrightText: 2005-2014 Daniel James
// SPDX-FileCopyrightText: 2016 Austin Appleby
// SPDX-License-Identifier: BSL-1.0

/// Hashes an unsigned integer value using a multi-word splitting approach
/// matching the upstream C++ `detail::HashValue`.
fn hash_value_unsigned(val: u64) -> usize {
    let size_t_bits = usize::BITS as u32;
    let val_bits = 64u32; // we always work with u64
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
        h ^= k.wrapping_add(0x9e3779b9).wrapping_add(h << 6).wrapping_add(h >> 2);
        h
    }
}

/// Combines a hash seed with a new unsigned value, matching
/// upstream `Common::HashCombine`.
pub fn hash_combine(seed: &mut usize, v: u64) {
    *seed = hash_combine_impl(*seed, hash_value_unsigned(v));
}

/// Hashes a range of unsigned values (iterator), matching
/// upstream `Common::HashRange`.
pub fn hash_range<I>(iter: I) -> usize
where
    I: IntoIterator<Item = u64>,
{
    let mut seed: usize = 0;
    for v in iter {
        hash_combine(&mut seed, v);
    }
    seed
}

/// Hashes an array of unsigned values, matching upstream
/// `Common::HashValue` for `std::array`.
pub fn hash_array(v: &[u64]) -> usize {
    hash_range(v.iter().copied())
}

/// Hashes a slice of bytes by treating each byte as a u64 value.
/// This preserves parity with the upstream which hashes element-by-element.
pub fn hash_byte_slice(v: &[u8]) -> usize {
    hash_range(v.iter().map(|&b| b as u64))
}

/// Hashes a slice of u32 values.
pub fn hash_u32_slice(v: &[u32]) -> usize {
    hash_range(v.iter().map(|&x| x as u64))
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
        hash_combine(&mut seed1, 42);
        let mut seed2: usize = 0;
        hash_combine(&mut seed2, 42);
        assert_eq!(seed1, seed2);
    }

    #[test]
    fn test_hash_combine_different_values() {
        let mut seed1: usize = 0;
        hash_combine(&mut seed1, 1);
        let mut seed2: usize = 0;
        hash_combine(&mut seed2, 2);
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
}
