// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/spl/csrng.h
//! Port of zuyu/src/core/hle/service/spl/csrng.cpp
//!
//! IRandomInterface — cryptographic secure random number generator ("csrng").

/// IPC command table for IRandomInterface.
pub mod commands {
    pub const GENERATE_RANDOM_BYTES: u32 = 0;
}

/// IRandomInterface — CSRNG service.
///
/// Corresponds to `IRandomInterface` in upstream csrng.h / csrng.cpp.
pub struct IRandomInterface;

impl IRandomInterface {
    pub fn new() -> Self {
        Self
    }

    /// GenerateRandomBytes (cmd 0).
    pub fn generate_random_bytes(&self, buf: &mut [u8]) {
        log::debug!("IRandomInterface::generate_random_bytes called, size={}", buf.len());
        // TODO: use proper cryptographic RNG
        for byte in buf.iter_mut() {
            *byte = 0;
        }
    }
}
