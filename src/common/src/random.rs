// SPDX-FileCopyrightText: Copyright 2026 Eden Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `common/random.h` and `common/random.cpp`.

const STATE_WORDS: usize = 624;
const PERIOD: usize = 397;
const MATRIX_A: u32 = 0x9908_B0DF;
const UPPER_MASK: u32 = 0x8000_0000;
const LOWER_MASK: u32 = 0x7FFF_FFFF;

/// Rust counterpart of `std::mt19937` returned by upstream `GetMT19937`.
#[derive(Clone, Debug)]
pub struct Mt19937 {
    state: [u32; STATE_WORDS],
    index: usize,
}

impl Mt19937 {
    /// Constructs the engine with the same initialization as `std::mt19937(seed)`.
    pub fn new(seed: u32) -> Self {
        let mut state = [0; STATE_WORDS];
        state[0] = seed;
        for index in 1..STATE_WORDS {
            let previous = state[index - 1];
            state[index] = 1_812_433_253_u32
                .wrapping_mul(previous ^ (previous >> 30))
                .wrapping_add(index as u32);
        }
        Self {
            state,
            index: STATE_WORDS,
        }
    }

    /// Rust counterpart of `std::mt19937::operator()`.
    pub fn next_u32(&mut self) -> u32 {
        if self.index >= STATE_WORDS {
            self.twist();
        }

        let mut value = self.state[self.index];
        self.index += 1;
        value ^= value >> 11;
        value ^= (value << 7) & 0x9D2C_5680;
        value ^= (value << 15) & 0xEFC6_0000;
        value ^= value >> 18;
        value
    }

    fn twist(&mut self) {
        for index in 0..STATE_WORDS {
            let value = (self.state[index] & UPPER_MASK)
                | (self.state[(index + 1) % STATE_WORDS] & LOWER_MASK);
            let mut mixed = value >> 1;
            if value & 1 != 0 {
                mixed ^= MATRIX_A;
            }
            self.state[index] = self.state[(index + PERIOD) % STATE_WORDS] ^ mixed;
        }
        self.index = 0;
    }
}

/// Returns a random 32-bit value. Upstream currently ignores `seed`.
pub fn random32(_seed: u32) -> u32 {
    fastrand::u32(..)
}

/// Returns a random value produced by `std::random_device::operator()`.
///
/// Upstream's `random_device::result_type` is 32-bit, so the upper half of the
/// returned `u64` remains zero.
pub fn random64(_seed: u64) -> u64 {
    u64::from(fastrand::u32(..))
}

/// Returns an MT19937 engine seeded from the global random source.
pub fn get_mt19937() -> Mt19937 {
    Mt19937::new(random32(0))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mt19937_matches_the_standard_reference_sequence() {
        let mut random = Mt19937::new(5489);
        assert_eq!(random.next_u32(), 3_499_211_612);
        assert_eq!(random.next_u32(), 581_869_302);
        assert_eq!(random.next_u32(), 3_890_346_734);
    }

    #[test]
    fn random64_preserves_upstream_random_device_width() {
        assert_eq!(random64(0) >> 32, 0);
    }
}
