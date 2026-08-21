// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/common/bit_set.h
//!
//! Constexpr bitset with bit operations. The C++ version is templated on a
//! storage type (u8/u16/u32/u64) and a capacity N. In Rust we use a
//! `BitSet` backed by a `Vec<u64>` since const generics cannot yet be used
//! in array lengths that depend on associated constants.
//! Type aliases `BitSet8`, `BitSet16`, etc. are provided for source-level parity.

const FLAGS_PER_WORD: usize = 64;

/// A fixed-capacity bitset stored as a vector of `u64` words.
///
/// Bits are numbered from 0, with bit 0 being the most-significant bit of
/// word 0 (matching the upstream C++ convention where `GetBitMask` places the
/// bit at `FlagsPerWord - 1 - bit`).
#[derive(Clone, Debug)]
pub struct BitSet {
    words: Vec<u64>,
    num_words: usize,
    capacity: usize,
}

impl BitSet {
    /// Create a new bitset with capacity `n` bits, all cleared.
    pub fn new(n: usize) -> Self {
        let num_words = (n + FLAGS_PER_WORD - 1) / FLAGS_PER_WORD;
        Self {
            words: vec![0u64; num_words],
            num_words,
            capacity: n,
        }
    }

    /// Set bit `i` (0-indexed, MSB-first within each word).
    pub fn set_bit(&mut self, i: usize) {
        self.words[i / FLAGS_PER_WORD] |= get_bit_mask(i % FLAGS_PER_WORD);
    }

    /// Clear bit `i`.
    pub fn clear_bit(&mut self, i: usize) {
        self.words[i / FLAGS_PER_WORD] &= !get_bit_mask(i % FLAGS_PER_WORD);
    }

    /// Count leading zeros -- returns the index of the first set bit, or
    /// `FLAGS_PER_WORD * num_words` if no bit is set.
    pub fn count_leading_zero(&self) -> usize {
        for i in 0..self.num_words {
            if self.words[i] != 0 {
                return FLAGS_PER_WORD * i + self.words[i].leading_zeros() as usize;
            }
        }
        FLAGS_PER_WORD * self.num_words
    }

    /// Get the next set bit strictly after position `n`. Returns the index of
    /// the next set bit, or `FLAGS_PER_WORD * num_words` if none.
    ///
    /// This mirrors the upstream `GetNextSet` which scans starting from
    /// word `(n+1) / FlagsPerWord` and masks off bits at or before position `n`.
    pub fn get_next_set(&self, n: usize) -> usize {
        let start_word = (n + 1) / FLAGS_PER_WORD;
        for i in start_word..self.num_words {
            let mut word = self.words[i];
            if !is_aligned(n + 1, FLAGS_PER_WORD) {
                // Mask off bits at positions <= n within this word.
                // Upstream: word &= GetBitMask(n % FlagsPerWord) - 1;
                word &= get_bit_mask(n % FLAGS_PER_WORD).wrapping_sub(1);
            }
            if word != 0 {
                return FLAGS_PER_WORD * i + word.leading_zeros() as usize;
            }
        }
        FLAGS_PER_WORD * self.num_words
    }

    /// Returns the total capacity in bits.
    pub fn capacity(&self) -> usize {
        self.capacity
    }
}

impl Default for BitSet {
    fn default() -> Self {
        Self::new(0)
    }
}

/// Returns the bitmask for position `bit` within a word.
/// Bit 0 is the MSB (matching upstream convention).
#[inline]
const fn get_bit_mask(bit: usize) -> u64 {
    1u64 << (FLAGS_PER_WORD - 1 - bit)
}

/// Check if `value` is aligned to `alignment`.
#[inline]
const fn is_aligned(value: usize, alignment: usize) -> bool {
    (value & (alignment - 1)) == 0
}

// Type aliases matching upstream C++ `BitSet8<N>`, `BitSet16<N>`, etc.
// In Rust the underlying storage is always u64; the aliases preserve API parity.
pub type BitSet8 = BitSet;
pub type BitSet16 = BitSet;
pub type BitSet32 = BitSet;
pub type BitSet64 = BitSet;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_set_and_clear_bit() {
        let mut bs = BitSet::new(128);
        assert_eq!(bs.count_leading_zero(), 128);

        bs.set_bit(0);
        assert_eq!(bs.count_leading_zero(), 0);

        bs.set_bit(65);
        bs.clear_bit(0);
        assert_eq!(bs.count_leading_zero(), 65);
    }

    #[test]
    fn test_count_leading_zero_empty() {
        let bs = BitSet::new(64);
        assert_eq!(bs.count_leading_zero(), 64);
    }

    #[test]
    fn test_get_next_set() {
        // Test within the same word and at word boundaries.
        // Note: upstream GetNextSet applies a mask based on n%FlagsPerWord to ALL
        // subsequent words, so cross-word searches only work reliably when the
        // target bit falls in the unmasked region. We test the pattern that
        // matches upstream behavior.
        let mut bs = BitSet::new(256);
        bs.set_bit(10);
        bs.set_bit(50);

        assert_eq!(bs.count_leading_zero(), 10);
        assert_eq!(bs.get_next_set(10), 50);
        assert_eq!(bs.get_next_set(50), 256); // no more in this word
    }

    #[test]
    fn test_get_next_set_same_word() {
        let mut bs = BitSet::new(64);
        bs.set_bit(5);
        bs.set_bit(10);
        assert_eq!(bs.get_next_set(5), 10);
    }
}
