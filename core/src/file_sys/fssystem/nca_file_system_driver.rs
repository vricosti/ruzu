// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_nca_file_system_driver.h / .cpp

use super::compression_common::GetDecompressorFunction;
use super::nca_reader::{NcaFsHeaderReader, NcaReader};
use crate::file_sys::vfs::vfs_types::VirtualFile;
use common::ResultCode;
use std::sync::Arc;

/// Key generation function type.
/// Corresponds to upstream `KeyGenerationFunction`.
pub type KeyGenerationFunction =
    fn(dst_key: &mut [u8], src_key: &[u8], key_type: i32);

/// Signature verification function type.
/// Corresponds to upstream `VerifySign1Function`.
pub type VerifySign1Function =
    fn(sig: &[u8], data: &[u8], generation: u8) -> bool;

/// NCA cryptographic configuration.
/// Corresponds to upstream `NcaCryptoConfiguration`.
pub struct NcaCryptoConfiguration {
    pub header_1_sign_key_moduli: Vec<Option<Vec<u8>>>,
    pub header_1_sign_key_public_exponent: [u8; Self::RSA_2048_KEY_PUBLIC_EXPONENT_SIZE],
    pub key_area_encryption_key_source: [[u8; Self::AES_128_KEY_SIZE]; Self::KEY_AREA_ENCRYPTION_KEY_INDEX_COUNT as usize],
    pub header_encryption_key_source: [u8; Self::AES_128_KEY_SIZE],
    pub header_encrypted_encryption_keys: [[u8; Self::AES_128_KEY_SIZE]; Self::HEADER_ENCRYPTION_KEY_COUNT as usize],
    pub generate_key: Option<KeyGenerationFunction>,
    pub verify_sign1: Option<VerifySign1Function>,
    pub is_plaintext_header_available: bool,
    pub is_available_sw_key: bool,
}

impl NcaCryptoConfiguration {
    pub const RSA_2048_KEY_MODULUS_SIZE: usize = 2048 / 8;
    pub const RSA_2048_KEY_PUBLIC_EXPONENT_SIZE: usize = 3;
    pub const RSA_2048_KEY_PRIVATE_EXPONENT_SIZE: usize = Self::RSA_2048_KEY_MODULUS_SIZE;
    pub const AES_128_KEY_SIZE: usize = 128 / 8;
    pub const HEADER_1_SIGNATURE_KEY_GENERATION_MAX: usize = 1;
    pub const KEY_AREA_ENCRYPTION_KEY_INDEX_COUNT: i32 = 3;
    pub const HEADER_ENCRYPTION_KEY_COUNT: i32 = 2;
    pub const KEY_AREA_ENCRYPTION_KEY_INDEX_ZERO_KEY: u8 = 0xFF;
    pub const KEY_GENERATION_MAX: usize = 32;
}

impl Default for NcaCryptoConfiguration {
    fn default() -> Self {
        Self {
            header_1_sign_key_moduli: Vec::new(),
            header_1_sign_key_public_exponent: [0u8; Self::RSA_2048_KEY_PUBLIC_EXPONENT_SIZE],
            key_area_encryption_key_source: [[0u8; Self::AES_128_KEY_SIZE]; Self::KEY_AREA_ENCRYPTION_KEY_INDEX_COUNT as usize],
            header_encryption_key_source: [0u8; Self::AES_128_KEY_SIZE],
            header_encrypted_encryption_keys: [[0u8; Self::AES_128_KEY_SIZE]; Self::HEADER_ENCRYPTION_KEY_COUNT as usize],
            generate_key: None,
            verify_sign1: None,
            is_plaintext_header_available: false,
            is_available_sw_key: false,
        }
    }
}

/// NCA compression configuration.
/// Corresponds to upstream `NcaCompressionConfiguration`.
pub struct NcaCompressionConfiguration {
    pub get_decompressor: Option<GetDecompressorFunction>,
}

impl Default for NcaCompressionConfiguration {
    fn default() -> Self {
        Self {
            get_decompressor: None,
        }
    }
}

/// Key area encryption key count.
pub const KEY_AREA_ENCRYPTION_KEY_COUNT: i32 =
    NcaCryptoConfiguration::KEY_AREA_ENCRYPTION_KEY_INDEX_COUNT
        * NcaCryptoConfiguration::KEY_GENERATION_MAX as i32;

/// Key type.
/// Corresponds to upstream `KeyType`.
///
/// Note: NcaHeaderKey1, NcaHeaderKey2, NcaExternalKey, etc. are defined
/// as constants because their values depend on KEY_AREA_ENCRYPTION_KEY_COUNT
/// which is a runtime-computable expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum KeyType {
    ZeroKey = -2,
    InvalidKey = -1,
    // Dynamic values start at KEY_AREA_ENCRYPTION_KEY_COUNT.
    // Use the associated constants below.
}

impl KeyType {
    pub const NCA_HEADER_KEY1: i32 = KEY_AREA_ENCRYPTION_KEY_COUNT;
    pub const NCA_HEADER_KEY2: i32 = KEY_AREA_ENCRYPTION_KEY_COUNT + 1;
    pub const NCA_EXTERNAL_KEY: i32 = KEY_AREA_ENCRYPTION_KEY_COUNT + 2;
    pub const SAVE_DATA_DEVICE_UNIQUE_MAC: i32 = KEY_AREA_ENCRYPTION_KEY_COUNT + 3;
    pub const SAVE_DATA_SEED_UNIQUE_MAC: i32 = KEY_AREA_ENCRYPTION_KEY_COUNT + 4;
    pub const SAVE_DATA_TRANSFER_MAC: i32 = KEY_AREA_ENCRYPTION_KEY_COUNT + 5;
}

pub fn is_invalid_key_type_value(key_type: i32) -> bool {
    key_type < 0
}

pub fn get_key_type_value(key_index: u8, key_generation: u8) -> i32 {
    if key_index == NcaCryptoConfiguration::KEY_AREA_ENCRYPTION_KEY_INDEX_ZERO_KEY {
        return KeyType::ZeroKey as i32;
    }
    if key_index as i32 >= NcaCryptoConfiguration::KEY_AREA_ENCRYPTION_KEY_INDEX_COUNT {
        return KeyType::InvalidKey as i32;
    }
    NcaCryptoConfiguration::KEY_AREA_ENCRYPTION_KEY_INDEX_COUNT * key_generation as i32
        + key_index as i32
}

/// Storage context for NCA file system driver operations.
/// Corresponds to upstream `NcaFileSystemDriver::StorageContext`.
#[derive(Default)]
pub struct StorageContext {
    pub open_raw_storage: bool,
    pub body_substorage: Option<VirtualFile>,
    pub fs_data_storage: Option<VirtualFile>,
}

/// NCA file system driver.
/// Corresponds to upstream `NcaFileSystemDriver`.
pub struct NcaFileSystemDriver {
    original_reader: Option<Arc<NcaReader>>,
    reader: Arc<NcaReader>,
}

impl NcaFileSystemDriver {
    pub fn new(reader: Arc<NcaReader>) -> Self {
        Self {
            original_reader: None,
            reader,
        }
    }

    pub fn with_original(
        original_reader: Arc<NcaReader>,
        reader: Arc<NcaReader>,
    ) -> Self {
        Self {
            original_reader: Some(original_reader),
            reader,
        }
    }

    pub fn setup_fs_header_reader(
        out: &mut NcaFsHeaderReader,
        reader: &NcaReader,
        fs_index: i32,
    ) -> Result<(), ResultCode> {
        out.initialize(reader, fs_index)
    }

    pub fn open_storage(
        &self,
        _out_header_reader: &mut NcaFsHeaderReader,
        _fs_index: i32,
    ) -> Result<VirtualFile, ResultCode> {
        // TODO: implement proper storage opening with context.
        todo!("NcaFileSystemDriver::open_storage not yet implemented")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nca_crypto_configuration_default() {
        let config = NcaCryptoConfiguration::default();
        assert!(config.generate_key.is_none());
        assert!(config.verify_sign1.is_none());
        assert!(!config.is_plaintext_header_available);
        assert!(!config.is_available_sw_key);
    }

    #[test]
    fn test_nca_compression_configuration_default() {
        let config = NcaCompressionConfiguration::default();
        assert!(config.get_decompressor.is_none());
    }

    #[test]
    fn test_nca_crypto_configuration_constants() {
        assert_eq!(NcaCryptoConfiguration::AES_128_KEY_SIZE, 16);
        assert_eq!(NcaCryptoConfiguration::RSA_2048_KEY_MODULUS_SIZE, 256);
        assert_eq!(NcaCryptoConfiguration::KEY_AREA_ENCRYPTION_KEY_INDEX_COUNT, 3);
        assert_eq!(NcaCryptoConfiguration::HEADER_ENCRYPTION_KEY_COUNT, 2);
        assert_eq!(NcaCryptoConfiguration::KEY_GENERATION_MAX, 32);
    }

    #[test]
    fn test_key_type_constants() {
        assert_eq!(KeyType::ZeroKey as i32, -2);
        assert_eq!(KeyType::InvalidKey as i32, -1);
        assert!(KeyType::NCA_HEADER_KEY1 > 0);
        assert!(KeyType::NCA_EXTERNAL_KEY > KeyType::NCA_HEADER_KEY2);
    }

    #[test]
    fn test_is_invalid_key_type_value() {
        assert!(is_invalid_key_type_value(-1));
        assert!(is_invalid_key_type_value(-2));
        assert!(!is_invalid_key_type_value(0));
        assert!(!is_invalid_key_type_value(10));
    }

    #[test]
    fn test_get_key_type_value_zero_key() {
        let result = get_key_type_value(
            NcaCryptoConfiguration::KEY_AREA_ENCRYPTION_KEY_INDEX_ZERO_KEY,
            0,
        );
        assert_eq!(result, KeyType::ZeroKey as i32);
    }

    #[test]
    fn test_get_key_type_value_invalid() {
        // key_index >= KEY_AREA_ENCRYPTION_KEY_INDEX_COUNT should be InvalidKey.
        let result = get_key_type_value(3, 0);
        assert_eq!(result, KeyType::InvalidKey as i32);
    }

    #[test]
    fn test_get_key_type_value_normal() {
        let result = get_key_type_value(1, 2);
        // Should be KEY_AREA_ENCRYPTION_KEY_INDEX_COUNT * key_generation + key_index
        assert_eq!(result, 3 * 2 + 1);
    }

    #[test]
    fn test_storage_context_default() {
        let ctx = StorageContext::default();
        assert!(!ctx.open_raw_storage);
        assert!(ctx.body_substorage.is_none());
        assert!(ctx.fs_data_storage.is_none());
    }
}
