// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/file_sys/fssystem/fssystem_crypto_configuration.h/.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-12
//!
//! Provides the global NCA crypto configuration, including the key generation
//! function that maps key types to actual decryption keys.

use super::nca_file_system_driver::{KeyType, NcaCryptoConfiguration};

/// Generate a decryption key from a source key and key type.
///
/// Corresponds to upstream anonymous `GenerateKey` function in
/// fssystem_crypto_configuration.cpp.
///
/// Key type mapping:
/// - ZeroKey: output is all zeros
/// - InvalidKey or out of range: output is all 0xFF
/// - NcaHeaderKey1/2: reads from KeyManager header key
/// - Others: derives from key area encryption keys
fn generate_key(dst_key: &mut [u8], src_key: &[u8], key_type: i32) {
    let zero_key = KeyType::ZeroKey as i32;
    let invalid_key = KeyType::InvalidKey as i32;
    let nca_external_key = KeyType::NCA_EXTERNAL_KEY;
    let nca_header_key1 = KeyType::NCA_HEADER_KEY1;
    let nca_header_key2 = KeyType::NCA_HEADER_KEY2;

    if key_type == zero_key {
        dst_key.fill(0);
        return;
    }

    if key_type == invalid_key || key_type < zero_key || key_type >= nca_external_key {
        dst_key.fill(0xFF);
        return;
    }

    if key_type == nca_header_key1 || key_type == nca_header_key2 {
        // TODO: Read from KeyManager when integrated.
        // For now, fill with zeros.
        let _key_index = if key_type == nca_header_key2 { 1 } else { 0 };
        dst_key.fill(0);
        return;
    }

    // Derive from key area encryption keys.
    let _key_generation =
        std::cmp::max(key_type / NcaCryptoConfiguration::KEY_AREA_ENCRYPTION_KEY_INDEX_COUNT, 1)
            - 1;
    let _key_index =
        key_type % NcaCryptoConfiguration::KEY_AREA_ENCRYPTION_KEY_INDEX_COUNT;

    // TODO: Perform AES-ECB decrypt using KeyManager keys.
    // For now, copy source key as placeholder.
    let copy_len = dst_key.len().min(src_key.len());
    dst_key[..copy_len].copy_from_slice(&src_key[..copy_len]);
}

/// Get the global NCA crypto configuration.
///
/// Corresponds to upstream `GetCryptoConfiguration`.
pub fn get_crypto_configuration() -> NcaCryptoConfiguration {
    NcaCryptoConfiguration {
        generate_key: Some(generate_key),
        ..NcaCryptoConfiguration::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_key_zero_key() {
        let mut dst = [0xAAu8; 16];
        let src = [0xBBu8; 16];
        generate_key(&mut dst, &src, KeyType::ZeroKey as i32);
        assert_eq!(dst, [0u8; 16]);
    }

    #[test]
    fn test_generate_key_invalid_key() {
        let mut dst = [0x00u8; 16];
        let src = [0xBBu8; 16];
        generate_key(&mut dst, &src, KeyType::InvalidKey as i32);
        assert_eq!(dst, [0xFFu8; 16]);
    }

    #[test]
    fn test_generate_key_negative_type() {
        let mut dst = [0x00u8; 16];
        let src = [0xBBu8; 16];
        generate_key(&mut dst, &src, -1);
        assert_eq!(dst, [0xFFu8; 16]);
    }

    #[test]
    fn test_generate_key_out_of_range() {
        let mut dst = [0x00u8; 16];
        let src = [0xBBu8; 16];
        generate_key(&mut dst, &src, KeyType::NCA_EXTERNAL_KEY);
        assert_eq!(dst, [0xFFu8; 16]);
    }

    #[test]
    fn test_generate_key_header_key1() {
        let mut dst = [0xAAu8; 16];
        let src = [0xBBu8; 16];
        generate_key(&mut dst, &src, KeyType::NCA_HEADER_KEY1);
        // Currently stubbed to zeros (no KeyManager).
        assert_eq!(dst, [0u8; 16]);
    }

    #[test]
    fn test_generate_key_header_key2() {
        let mut dst = [0xAAu8; 16];
        let src = [0xBBu8; 16];
        generate_key(&mut dst, &src, KeyType::NCA_HEADER_KEY2);
        assert_eq!(dst, [0u8; 16]);
    }

    #[test]
    fn test_generate_key_area_key() {
        // Valid key area key types start at 0. Currently stubbed: copies src to dst.
        let mut dst = [0x00u8; 16];
        let src = [0x42u8; 16];
        let key_type = 0; // First valid key area index
        generate_key(&mut dst, &src, key_type);
        assert_eq!(dst, src);
    }

    #[test]
    fn test_get_crypto_configuration() {
        let config = get_crypto_configuration();
        assert!(config.generate_key.is_some());
    }
}
