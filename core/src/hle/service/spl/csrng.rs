// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/spl/csrng.h
//! Port of zuyu/src/core/hle/service/spl/csrng.cpp
//!
//! CSRNG service — cryptographic secure random number generator ("csrng").
//!
//! This is a Module::Interface variant with only GenerateRandomBytes (cmd 0).

/// IPC command table for CSRNG (IRandomInterface).
///
/// Corresponds to the function table in upstream csrng.cpp.
pub mod commands {
    pub const GENERATE_RANDOM_BYTES: u32 = 0;
}

/// CSRNG — IRandomInterface service.
///
/// Corresponds to `CSRNG` in upstream csrng.h / csrng.cpp.
/// This is a Module::Interface with only the GenerateRandomBytes handler.
pub struct Csrng {
    rng_seed: u32,
}

impl Csrng {
    pub fn new(rng_seed: Option<u32>) -> Self {
        let seed = rng_seed.unwrap_or_else(|| {
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_secs() as u32)
                .unwrap_or(0)
        });
        Self { rng_seed: seed }
    }

    /// GenerateRandomBytes (cmd 0).
    ///
    /// Corresponds to `Module::Interface::GenerateRandomBytes` in upstream.
    pub fn generate_random_bytes(&self, buf: &mut [u8]) {
        log::debug!(
            "CSRNG::generate_random_bytes called, size={}",
            buf.len()
        );
        // Use a simple PRNG. For proper emulation, this should use a
        // cryptographic RNG. Upstream uses std::mt19937 with
        // uniform_int_distribution<u16>.
        let mut state = self.rng_seed as u64;
        for byte in buf.iter_mut() {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            *byte = (state & 0xFF) as u8;
        }
    }
}
