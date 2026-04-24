// SPDX-FileCopyrightText: Copyright 2026 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! 32-bit Mersenne Twister (MT19937), the algorithm behind `std::mt19937`.
//!
//! Hand-ported so the SPL / CSRNG services can match upstream's
//! `std::mt19937 rng` member exactly. Constants and recurrence match the
//! original Matsumoto-Nishimura MT19937 and `std::mt19937` in libstdc++ /
//! libc++.
//!
//! Not exported outside the `spl` module — if another service needs an
//! MT19937 later, promote it to `common`.

const N: usize = 624;
const M: usize = 397;
const MATRIX_A: u32 = 0x9908_B0DF;
const UPPER_MASK: u32 = 0x8000_0000;
const LOWER_MASK: u32 = 0x7FFF_FFFF;

#[derive(Clone, Debug)]
pub(super) struct Mt19937 {
    state: [u32; N],
    index: usize,
}

impl Mt19937 {
    /// Seed the generator, matching `std::mt19937(seed)`.
    pub fn new(seed: u32) -> Self {
        let mut state = [0u32; N];
        state[0] = seed;
        for i in 1..N {
            let prev = state[i - 1];
            state[i] = 1_812_433_253u32
                .wrapping_mul(prev ^ (prev >> 30))
                .wrapping_add(i as u32);
        }
        Self {
            state,
            index: N, // triggers refill on first call
        }
    }

    /// Generate the next 32-bit value. Matches `std::mt19937::operator()`.
    pub fn next_u32(&mut self) -> u32 {
        if self.index >= N {
            // Refill the state array.
            for i in 0..N {
                let y = (self.state[i] & UPPER_MASK) | (self.state[(i + 1) % N] & LOWER_MASK);
                let mut next = self.state[(i + M) % N] ^ (y >> 1);
                if y & 1 != 0 {
                    next ^= MATRIX_A;
                }
                self.state[i] = next;
            }
            self.index = 0;
        }

        let mut y = self.state[self.index];
        self.index += 1;

        // Tempering.
        y ^= y >> 11;
        y ^= (y << 7) & 0x9D2C_5680;
        y ^= (y << 15) & 0xEFC6_0000;
        y ^= y >> 18;
        y
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The same seed must produce the same sequence (determinism).
    #[test]
    fn same_seed_same_sequence() {
        let mut a = Mt19937::new(0xDEAD_BEEF);
        let mut b = Mt19937::new(0xDEAD_BEEF);
        for _ in 0..2000 {
            assert_eq!(a.next_u32(), b.next_u32());
        }
    }

    /// Different seeds produce different sequences.
    #[test]
    fn different_seeds_differ_quickly() {
        let mut a = Mt19937::new(1);
        let mut b = Mt19937::new(2);
        // The first outputs already differ — no MT19937 seed leads to identical
        // first-output for two different integer seeds under the standard
        // initialization.
        assert_ne!(a.next_u32(), b.next_u32());
    }

    /// The bug the MK8D investigation uncovered: two consecutive small
    /// generations must NOT produce identical sequences. Before the fix,
    /// `generate_random_bytes` reset state from the seed on every call,
    /// so two 8-byte calls in a row returned identical bytes. Regression
    /// guard against that pattern.
    #[test]
    fn consecutive_calls_advance_state() {
        let mut rng = Mt19937::new(12345);
        let mut first = [0u8; 8];
        let mut second = [0u8; 8];
        for b in first.iter_mut() {
            *b = (rng.next_u32() & 0xFF) as u8;
        }
        for b in second.iter_mut() {
            *b = (rng.next_u32() & 0xFF) as u8;
        }
        assert_ne!(first, second);
    }

    /// Refill boundary: N outputs triggers the state refill. Verify the
    /// N-th and (N+1)-th outputs are produced without panic or wraparound
    /// bugs (they must be different from earlier outputs with overwhelming
    /// probability).
    #[test]
    fn refills_after_n_outputs() {
        let mut rng = Mt19937::new(42);
        let mut seen = std::collections::HashSet::new();
        for _ in 0..(N * 2 + 5) {
            seen.insert(rng.next_u32());
        }
        // Well over 1000 distinct values — zero collisions in a random u32
        // stream of ~1250 is near-certain for a proper PRNG.
        assert!(seen.len() > N * 2, "suspiciously many collisions: {}", seen.len());
    }
}
