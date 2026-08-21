// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/crypto/sha_util.h and sha_util.cpp
//! SHA-256 hashing utilities.
//!
//! The upstream sha_util.h/cpp is minimal. The main type used is SHA256Hash = [u8; 0x20].
//! SHA-256 operations in the codebase primarily use mbedtls_sha256_ret directly.
//! In Rust, we use the `sha2` crate.

use sha2::{Digest, Sha256};

/// SHA-256 hash type (32 bytes).
pub type Sha256Hash = [u8; 0x20];

/// Computes the SHA-256 hash of the given data.
pub fn sha256(data: &[u8]) -> Sha256Hash {
    let mut hasher = Sha256::new();
    hasher.update(data);
    let result = hasher.finalize();
    let mut out = [0u8; 0x20];
    out.copy_from_slice(&result);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sha256_empty() {
        let hash = sha256(b"");
        // SHA-256 of empty string
        let expected = [
            0xe3, 0xb0, 0xc4, 0x42, 0x98, 0xfc, 0x1c, 0x14, 0x9a, 0xfb, 0xf4, 0xc8, 0x99, 0x6f,
            0xb9, 0x24, 0x27, 0xae, 0x41, 0xe4, 0x64, 0x9b, 0x93, 0x4c, 0xa4, 0x95, 0x99, 0x1b,
            0x78, 0x52, 0xb8, 0x55,
        ];
        assert_eq!(hash, expected);
    }
}
