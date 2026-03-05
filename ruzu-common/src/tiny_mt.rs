//! Port of zuyu/src/common/tiny_mt.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! Implementation of TinyMT (Mersenne Twister RNG).
//! Uses the same sample parameters as Nintendo.

const NUM_STATE_WORDS: usize = 4;

const PARAM_MAT1: u32 = 0x8F7011EE;
const PARAM_MAT2: u32 = 0xFC78FF1F;
const PARAM_TMAT: u32 = 0x3793FDFF;

const PARAM_MULT: u32 = 0x6C078965;
const PARAM_PLUS: u32 = 0x0019660D;
const PARAM_XOR: u32 = 0x5D588B65;

const TOP_BITMASK: u32 = 0x7FFFFFFF;

const MINIMUM_INIT_ITERATIONS: usize = 8;
const NUM_DISCARDED_INIT_OUTPUTS: usize = 8;

/// TinyMT state.
#[derive(Debug, Clone, Copy)]
pub struct TinyMTState {
    pub data: [u32; NUM_STATE_WORDS],
}

impl Default for TinyMTState {
    fn default() -> Self {
        Self {
            data: [0; NUM_STATE_WORDS],
        }
    }
}

/// TinyMT random number generator.
#[derive(Debug, Clone)]
pub struct TinyMT {
    state: TinyMTState,
}

impl TinyMT {
    pub fn new() -> Self {
        Self {
            state: TinyMTState::default(),
        }
    }

    // --- Initialization ---

    /// Initialize with a single seed value.
    pub fn initialize(&mut self, seed: u32) {
        self.state.data[0] = seed;
        self.state.data[1] = PARAM_MAT1;
        self.state.data[2] = PARAM_MAT2;
        self.state.data[3] = PARAM_TMAT;

        for i in 1..MINIMUM_INIT_ITERATIONS {
            let mixed = xor_by_shifted30(self.state.data[(i - 1) % NUM_STATE_WORDS]);
            self.state.data[i % NUM_STATE_WORDS] ^= mixed.wrapping_mul(PARAM_MULT).wrapping_add(i as u32);
        }

        self.finalize_initialization();
    }

    /// Initialize with an array of seeds.
    pub fn initialize_with_seeds(&mut self, seeds: &[u32]) {
        self.state.data[0] = 0;
        self.state.data[1] = PARAM_MAT1;
        self.state.data[2] = PARAM_MAT2;
        self.state.data[3] = PARAM_TMAT;

        let seed_count = seeds.len();
        let num_init_iterations = std::cmp::max(seed_count + 1, MINIMUM_INIT_ITERATIONS) - 1;

        generate_initial_value_plus(&mut self.state, 0, seed_count as u32);

        for i in 0..num_init_iterations {
            let seed_val = if i < seed_count { seeds[i] } else { 0 };
            generate_initial_value_plus(&mut self.state, (i + 1) % NUM_STATE_WORDS, seed_val);
        }

        for i in 0..NUM_STATE_WORDS {
            generate_initial_value_xor(
                &mut self.state,
                (i + 1 + num_init_iterations) % NUM_STATE_WORDS,
            );
        }

        self.finalize_initialization();
    }

    // --- State management ---

    /// Get the current state.
    pub fn get_state(&self) -> TinyMTState {
        self.state
    }

    /// Set the state.
    pub fn set_state(&mut self, state: TinyMTState) {
        self.state = state;
    }

    // --- Random generation ---

    /// Generate random bytes, filling the destination buffer.
    pub fn generate_random_bytes(&mut self, dst: &mut [u8]) {
        let mut offset = 0;
        let len = dst.len();

        // Handle unaligned start
        let aligned_start = (offset + 3) & !3;
        let aligned_start = aligned_start.min(len);

        if offset < aligned_start {
            let rnd = self.generate_random_u32();
            let rnd_bytes = rnd.to_le_bytes();
            let count = aligned_start - offset;
            dst[offset..offset + count].copy_from_slice(&rnd_bytes[..count]);
            offset = aligned_start;
        }

        // Write aligned u32s
        while offset + 4 <= len {
            let rnd = self.generate_random_u32();
            dst[offset..offset + 4].copy_from_slice(&rnd.to_le_bytes());
            offset += 4;
        }

        // Handle leftover
        if offset < len {
            let rnd = self.generate_random_u32();
            let rnd_bytes = rnd.to_le_bytes();
            let count = len - offset;
            dst[offset..offset + count].copy_from_slice(&rnd_bytes[..count]);
        }
    }

    /// Generate a random u32.
    pub fn generate_random_u32(&mut self) -> u32 {
        // Advance state.
        let x0 = (self.state.data[0] & TOP_BITMASK) ^ self.state.data[1] ^ self.state.data[2];
        let y0 = self.state.data[3];
        let x1 = x0 ^ (x0 << 1);
        let y1 = y0 ^ (y0 >> 1) ^ x1;

        let state0 = self.state.data[1];
        let mut state1 = self.state.data[2];
        let mut state2 = x1 ^ (y1 << 10);
        let state3 = y1;

        if (y1 & 1) != 0 {
            state1 ^= PARAM_MAT1;
            state2 ^= PARAM_MAT2;
        }

        self.state.data[0] = state0;
        self.state.data[1] = state1;
        self.state.data[2] = state2;
        self.state.data[3] = state3;

        // Temper.
        let t1 = state0.wrapping_add(state2 >> 8);
        let mut t0 = state3 ^ t1;

        if (t1 & 1) != 0 {
            t0 ^= PARAM_TMAT;
        }

        t0
    }

    /// Generate a random u64.
    pub fn generate_random_u64(&mut self) -> u64 {
        let lo = self.generate_random_u32() as u64;
        let hi = self.generate_random_u32() as u64;
        (hi << 32) | lo
    }

    /// Generate a random f32 in [0, 1).
    pub fn generate_random_f32(&mut self) -> f32 {
        const MANTISSA_BITS: u32 = 24;
        let value = self.generate_random_u24();
        (value as f32) * (1.0f32 / (1u32 << MANTISSA_BITS) as f32)
    }

    /// Generate a random f64 in [0, 1).
    ///
    /// Matches Nintendo's implementation which uses (32-5)=27 bits from the first call
    /// and (32-6)=26 bits from the second.
    pub fn generate_random_f64(&mut self) -> f64 {
        const MANTISSA_BITS: u32 = 53;
        const SHIFT_1ST: u32 = (64 - MANTISSA_BITS) / 2; // 5
        const SHIFT_2ND: u32 = (64 - MANTISSA_BITS) - SHIFT_1ST; // 6

        let first = (self.generate_random_u32() >> SHIFT_1ST) as u64;
        let second = (self.generate_random_u32() >> SHIFT_2ND) as u64;

        (1.0f64 * first as f64 * (1u64 << (32 - SHIFT_2ND)) as f64 + second as f64)
            * (1.0f64 / (1u64 << MANTISSA_BITS) as f64)
    }

    // --- Private ---

    fn generate_random_u24(&mut self) -> u32 {
        self.generate_random_u32() >> 8
    }

    fn finalize_initialization(&mut self) {
        let state0 = self.state.data[0] & TOP_BITMASK;
        let state1 = self.state.data[1];
        let state2 = self.state.data[2];
        let state3 = self.state.data[3];

        if state0 == 0 && state1 == 0 && state2 == 0 && state3 == 0 {
            self.state.data[0] = b'T' as u32;
            self.state.data[1] = b'I' as u32;
            self.state.data[2] = b'N' as u32;
            self.state.data[3] = b'Y' as u32;
        }

        for _ in 0..NUM_DISCARDED_INIT_OUTPUTS {
            self.generate_random_u32();
        }
    }
}

impl Default for TinyMT {
    fn default() -> Self {
        Self::new()
    }
}

// Helper functions

#[inline]
fn xor_by_shifted27(value: u32) -> u32 {
    value ^ (value >> 27)
}

#[inline]
fn xor_by_shifted30(value: u32) -> u32 {
    value ^ (value >> 30)
}

fn generate_initial_value_plus(state: &mut TinyMTState, index: usize, value: u32) {
    let i0 = (index + 0) % NUM_STATE_WORDS;
    let i1 = (index + 1) % NUM_STATE_WORDS;
    let i2 = (index + 2) % NUM_STATE_WORDS;
    let i3 = (index + 3) % NUM_STATE_WORDS;

    let x = xor_by_shifted27(state.data[i0] ^ state.data[i1] ^ state.data[i3])
        .wrapping_mul(PARAM_PLUS);
    let y = x.wrapping_add(index as u32).wrapping_add(value);

    state.data[i0] = y;
    state.data[i1] = state.data[i1].wrapping_add(x);
    state.data[i2] = state.data[i2].wrapping_add(y);
}

fn generate_initial_value_xor(state: &mut TinyMTState, index: usize) {
    let i0 = (index + 0) % NUM_STATE_WORDS;
    let i1 = (index + 1) % NUM_STATE_WORDS;
    let i2 = (index + 2) % NUM_STATE_WORDS;
    let i3 = (index + 3) % NUM_STATE_WORDS;

    let x = xor_by_shifted27(
        state.data[i0]
            .wrapping_add(state.data[i1])
            .wrapping_add(state.data[i3]),
    )
    .wrapping_mul(PARAM_XOR);
    let y = x.wrapping_sub(index as u32);

    state.data[i0] = y;
    state.data[i1] ^= x;
    state.data[i2] ^= y;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_initialization() {
        let mut rng = TinyMT::new();
        rng.initialize(42);
        // Just verify it produces different values
        let v1 = rng.generate_random_u32();
        let v2 = rng.generate_random_u32();
        assert_ne!(v1, v2);
    }

    #[test]
    fn test_deterministic() {
        let mut rng1 = TinyMT::new();
        let mut rng2 = TinyMT::new();
        rng1.initialize(12345);
        rng2.initialize(12345);
        for _ in 0..100 {
            assert_eq!(rng1.generate_random_u32(), rng2.generate_random_u32());
        }
    }

    #[test]
    fn test_generate_random_bytes() {
        let mut rng = TinyMT::new();
        rng.initialize(42);
        let mut buf = [0u8; 17]; // Odd size to test alignment handling
        rng.generate_random_bytes(&mut buf);
        // At least some bytes should be non-zero
        assert!(buf.iter().any(|&b| b != 0));
    }

    #[test]
    fn test_f32_range() {
        let mut rng = TinyMT::new();
        rng.initialize(42);
        for _ in 0..1000 {
            let v = rng.generate_random_f32();
            assert!((0.0..1.0).contains(&v), "f32 out of range: {}", v);
        }
    }

    #[test]
    fn test_f64_range() {
        let mut rng = TinyMT::new();
        rng.initialize(42);
        for _ in 0..1000 {
            let v = rng.generate_random_f64();
            assert!((0.0..1.0).contains(&v), "f64 out of range: {}", v);
        }
    }
}
