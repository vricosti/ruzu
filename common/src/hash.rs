// SPDX-FileCopyrightText: 2015 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

use std::hash::{Hash, Hasher};

/// A hasher for pairs (tuples of two elements).
/// Uses boost-style hash_combine on the individual hashes.
pub struct PairHash;

impl PairHash {
    /// Hash a pair of hashable values, combining them with boost-style hash_combine.
    pub fn hash_pair<T1: Hash, T2: Hash>(pair: &(T1, T2)) -> u64 {
        use std::collections::hash_map::DefaultHasher;

        let mut hasher = DefaultHasher::new();
        pair.0.hash(&mut hasher);
        let mut seed = hasher.finish();

        let mut hasher2 = DefaultHasher::new();
        pair.1.hash(&mut hasher2);
        let h2 = hasher2.finish();

        // boost::hash_combine equivalent
        seed ^= h2.wrapping_add(0x9e3779b9).wrapping_add(seed << 6).wrapping_add(seed >> 2);
        seed
    }
}

/// An identity hash that simply casts the value to usize.
/// Useful when the key is already a well-distributed integer.
#[derive(Debug, Clone, Default)]
pub struct IdentityHasher {
    value: u64,
}

impl Hasher for IdentityHasher {
    fn finish(&self) -> u64 {
        self.value
    }

    fn write(&mut self, bytes: &[u8]) {
        // Read up to 8 bytes as a little-endian u64
        let mut buf = [0u8; 8];
        let len = bytes.len().min(8);
        buf[..len].copy_from_slice(&bytes[..len]);
        self.value = u64::from_le_bytes(buf);
    }

    fn write_u8(&mut self, i: u8) {
        self.value = i as u64;
    }
    fn write_u16(&mut self, i: u16) {
        self.value = i as u64;
    }
    fn write_u32(&mut self, i: u32) {
        self.value = i as u64;
    }
    fn write_u64(&mut self, i: u64) {
        self.value = i;
    }
    fn write_usize(&mut self, i: usize) {
        self.value = i as u64;
    }
    fn write_i8(&mut self, i: i8) {
        self.value = i as u64;
    }
    fn write_i16(&mut self, i: i16) {
        self.value = i as u64;
    }
    fn write_i32(&mut self, i: i32) {
        self.value = i as u64;
    }
    fn write_i64(&mut self, i: i64) {
        self.value = i as u64;
    }
    fn write_isize(&mut self, i: isize) {
        self.value = i as u64;
    }
}

/// A BuildHasher that produces IdentityHasher instances.
/// Use with HashMap/HashSet when keys are already well-distributed integers.
#[derive(Debug, Clone, Default)]
pub struct BuildIdentityHasher;

impl std::hash::BuildHasher for BuildIdentityHasher {
    type Hasher = IdentityHasher;

    fn build_hasher(&self) -> IdentityHasher {
        IdentityHasher { value: 0 }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pair_hash() {
        let h1 = PairHash::hash_pair(&(1u32, 2u32));
        let h2 = PairHash::hash_pair(&(2u32, 1u32));
        // Different order should produce different hashes
        assert_ne!(h1, h2);
        // Same input should produce same hash
        assert_eq!(h1, PairHash::hash_pair(&(1u32, 2u32)));
    }

    #[test]
    fn test_identity_hasher() {
        use std::hash::BuildHasher;

        let build = BuildIdentityHasher;
        let mut hasher = build.build_hasher();
        42u64.hash(&mut hasher);
        assert_eq!(hasher.finish(), 42);
    }
}
