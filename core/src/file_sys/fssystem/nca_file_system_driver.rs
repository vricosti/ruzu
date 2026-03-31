// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_nca_file_system_driver.h / .cpp

use super::aes_ctr_counter_extended_storage::AesCtrCounterExtendedStorage;
use super::aes_ctr_storage::AesCtrStorage;
use super::compressed_storage::CompressedStorage;
use super::compression_common::GetDecompressorFunction;
use super::indirect_storage::IndirectStorage;
use super::nca_header::*;
use super::nca_reader::{NcaFsHeaderReader, NcaReader};
use super::sparse_storage::SparseStorage;
use crate::file_sys::errors::*;
use crate::file_sys::vfs::vfs::VfsFile;
use crate::file_sys::vfs::vfs_offset::OffsetVfsFile;
use crate::file_sys::vfs::vfs_types::{VirtualDir, VirtualFile};
use common::ResultCode;
use std::sync::Arc;

// ============================================================================
// Constants
// ============================================================================

/// Integrity data cache entry count.
/// Corresponds to upstream `IntegrityDataCacheCount`.
const INTEGRITY_DATA_CACHE_COUNT: i32 = 24;

/// Integrity hash cache entry count.
/// Corresponds to upstream `IntegrityHashCacheCount`.
const INTEGRITY_HASH_CACHE_COUNT: i32 = 8;

/// Integrity data cache entry count for meta.
/// Corresponds to upstream `IntegrityDataCacheCountForMeta`.
const INTEGRITY_DATA_CACHE_COUNT_FOR_META: i32 = 16;

/// Integrity hash cache entry count for meta.
/// Corresponds to upstream `IntegrityHashCacheCountForMeta`.
const INTEGRITY_HASH_CACHE_COUNT_FOR_META: i32 = 2;

// ============================================================================
// SharedNcaBodyStorage
// ============================================================================

/// A read-only storage that wraps an NCA body storage and holds a reference
/// to the NcaReader to keep it alive.
/// Corresponds to upstream anonymous `SharedNcaBodyStorage`.
struct SharedNcaBodyStorage {
    storage: VirtualFile,
    _nca_reader: Arc<NcaReader>,
}

impl SharedNcaBodyStorage {
    pub fn new(storage: VirtualFile, nca_reader: Arc<NcaReader>) -> Self {
        Self {
            storage,
            _nca_reader: nca_reader,
        }
    }
}

impl VfsFile for SharedNcaBodyStorage {
    fn get_name(&self) -> String {
        String::from("SharedNcaBodyStorage")
    }

    fn get_size(&self) -> usize {
        self.storage.get_size()
    }

    fn resize(&self, _new_size: usize) -> bool {
        false
    }

    fn get_containing_directory(&self) -> Option<VirtualDir> {
        None
    }

    fn is_writable(&self) -> bool {
        false
    }

    fn is_readable(&self) -> bool {
        true
    }

    fn read(&self, data: &mut [u8], length: usize, offset: usize) -> usize {
        self.storage.read(data, length, offset)
    }

    fn write(&self, _data: &[u8], _length: usize, _offset: usize) -> usize {
        0
    }

    fn rename(&self, _new_name: &str) -> bool {
        false
    }
}

// ============================================================================
// Helper functions
// ============================================================================

/// Get the byte offset of a filesystem section.
/// Corresponds to upstream anonymous `GetFsOffset`.
fn get_fs_offset(reader: &NcaReader, fs_index: i32) -> i64 {
    reader.get_fs_offset(fs_index) as i64
}

/// Get the end byte offset of a filesystem section.
/// Corresponds to upstream anonymous `GetFsEndOffset`.
fn get_fs_end_offset(reader: &NcaReader, fs_index: i32) -> i64 {
    reader.get_fs_end_offset(fs_index) as i64
}

// ============================================================================
// Key generation function type
// ============================================================================

/// Key generation function type.
/// Corresponds to upstream `KeyGenerationFunction`.
pub type KeyGenerationFunction = fn(dst_key: &mut [u8], src_key: &[u8], key_type: i32);

/// Signature verification function type.
/// Corresponds to upstream `VerifySign1Function`.
pub type VerifySign1Function = fn(sig: &[u8], data: &[u8], generation: u8) -> bool;

// ============================================================================
// NcaCryptoConfiguration
// ============================================================================

/// NCA cryptographic configuration.
/// Corresponds to upstream `NcaCryptoConfiguration`.
pub struct NcaCryptoConfiguration {
    pub header_1_sign_key_moduli: Vec<Option<Vec<u8>>>,
    pub header_1_sign_key_public_exponent: [u8; Self::RSA_2048_KEY_PUBLIC_EXPONENT_SIZE],
    pub key_area_encryption_key_source:
        [[u8; Self::AES_128_KEY_SIZE]; Self::KEY_AREA_ENCRYPTION_KEY_INDEX_COUNT as usize],
    pub header_encryption_key_source: [u8; Self::AES_128_KEY_SIZE],
    pub header_encrypted_encryption_keys:
        [[u8; Self::AES_128_KEY_SIZE]; Self::HEADER_ENCRYPTION_KEY_COUNT as usize],
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
            key_area_encryption_key_source: [[0u8; Self::AES_128_KEY_SIZE];
                Self::KEY_AREA_ENCRYPTION_KEY_INDEX_COUNT as usize],
            header_encryption_key_source: [0u8; Self::AES_128_KEY_SIZE],
            header_encrypted_encryption_keys: [[0u8; Self::AES_128_KEY_SIZE];
                Self::HEADER_ENCRYPTION_KEY_COUNT as usize],
            generate_key: None,
            verify_sign1: None,
            is_plaintext_header_available: false,
            is_available_sw_key: false,
        }
    }
}

// ============================================================================
// NcaCompressionConfiguration
// ============================================================================

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

// ============================================================================
// Key type
// ============================================================================

/// Key area encryption key count.
pub const KEY_AREA_ENCRYPTION_KEY_COUNT: i32 =
    NcaCryptoConfiguration::KEY_AREA_ENCRYPTION_KEY_INDEX_COUNT
        * NcaCryptoConfiguration::KEY_GENERATION_MAX as i32;

/// Key type.
/// Corresponds to upstream `KeyType`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum KeyType {
    ZeroKey = -2,
    InvalidKey = -1,
}

impl KeyType {
    pub const NCA_HEADER_KEY1: i32 = KEY_AREA_ENCRYPTION_KEY_COUNT;
    pub const NCA_HEADER_KEY2: i32 = KEY_AREA_ENCRYPTION_KEY_COUNT + 1;
    pub const NCA_EXTERNAL_KEY: i32 = KEY_AREA_ENCRYPTION_KEY_COUNT + 2;
    pub const SAVE_DATA_DEVICE_UNIQUE_MAC: i32 = KEY_AREA_ENCRYPTION_KEY_COUNT + 3;
    pub const SAVE_DATA_SEED_UNIQUE_MAC: i32 = KEY_AREA_ENCRYPTION_KEY_COUNT + 4;
    pub const SAVE_DATA_TRANSFER_MAC: i32 = KEY_AREA_ENCRYPTION_KEY_COUNT + 5;
}

/// Check if a key type value is invalid.
/// Corresponds to upstream `IsInvalidKeyTypeValue`.
pub fn is_invalid_key_type_value(key_type: i32) -> bool {
    key_type < 0
}

/// Get the key type value for a given key index and generation.
/// Corresponds to upstream `GetKeyTypeValue`.
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

// ============================================================================
// AlignmentStorageRequirement
// ============================================================================

/// Alignment storage requirement.
/// Corresponds to upstream `NcaFileSystemDriver::AlignmentStorageRequirement`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum AlignmentStorageRequirement {
    CacheBlockSize = 0,
    None = 1,
}

// ============================================================================
// StorageContext
// ============================================================================

/// Storage context for NCA file system driver operations.
/// Corresponds to upstream `NcaFileSystemDriver::StorageContext`.
#[derive(Default)]
pub struct StorageContext {
    pub open_raw_storage: bool,
    pub body_substorage: Option<VirtualFile>,
    pub current_sparse_storage: Option<Arc<SparseStorage>>,
    pub sparse_storage_meta_storage: Option<VirtualFile>,
    pub original_sparse_storage: Option<Arc<SparseStorage>>,
    pub aes_ctr_ex_storage_meta_storage: Option<VirtualFile>,
    pub aes_ctr_ex_storage_data_storage: Option<VirtualFile>,
    pub aes_ctr_ex_storage: Option<Arc<AesCtrCounterExtendedStorage>>,
    pub indirect_storage_meta_storage: Option<VirtualFile>,
    pub indirect_storage: Option<Arc<IndirectStorage>>,
    pub fs_data_storage: Option<VirtualFile>,
    pub compressed_storage_meta_storage: Option<VirtualFile>,
    pub compressed_storage: Option<Arc<CompressedStorage>>,
    pub patch_layer_info_storage: Option<VirtualFile>,
    pub sparse_layer_info_storage: Option<VirtualFile>,
    pub external_original_storage: Option<VirtualFile>,
}

// ============================================================================
// NcaFileSystemDriver
// ============================================================================

/// NCA file system driver.
/// Corresponds to upstream `NcaFileSystemDriver`.
pub struct NcaFileSystemDriver {
    original_reader: Option<Arc<NcaReader>>,
    reader: Arc<NcaReader>,
}

impl NcaFileSystemDriver {
    /// Create a new NCA file system driver with a single reader.
    /// Corresponds to upstream `NcaFileSystemDriver::NcaFileSystemDriver(shared_ptr<NcaReader>)`.
    pub fn new(reader: Arc<NcaReader>) -> Self {
        Self {
            original_reader: None,
            reader,
        }
    }

    /// Create a new NCA file system driver with original and current readers.
    /// Corresponds to upstream `NcaFileSystemDriver::NcaFileSystemDriver(shared_ptr<NcaReader>, shared_ptr<NcaReader>)`.
    pub fn with_original(original_reader: Arc<NcaReader>, reader: Arc<NcaReader>) -> Self {
        Self {
            original_reader: Some(original_reader),
            reader,
        }
    }

    /// Setup an FS header reader.
    /// Corresponds to upstream `NcaFileSystemDriver::SetupFsHeaderReader`.
    pub fn setup_fs_header_reader(
        out: &mut NcaFsHeaderReader,
        reader: &NcaReader,
        fs_index: i32,
    ) -> Result<(), ResultCode> {
        out.initialize(reader, fs_index)
    }

    /// Open a storage at the given filesystem index.
    /// Corresponds to upstream `NcaFileSystemDriver::OpenStorage`.
    pub fn open_storage(
        &self,
        out_header_reader: &mut NcaFsHeaderReader,
        fs_index: i32,
    ) -> Result<VirtualFile, ResultCode> {
        // Create a storage context.
        let mut ctx = StorageContext::default();

        // Open the storage.
        self.open_storage_with_context(out_header_reader, fs_index, &mut ctx)
    }

    /// Open a storage at the given filesystem index with a storage context.
    /// Corresponds to upstream `NcaFileSystemDriver::OpenStorageWithContext`.
    pub fn open_storage_with_context(
        &self,
        out_header_reader: &mut NcaFsHeaderReader,
        fs_index: i32,
        ctx: &mut StorageContext,
    ) -> Result<VirtualFile, ResultCode> {
        self.open_storage_impl(out_header_reader, fs_index, ctx)
    }

    /// Internal storage opening implementation.
    /// Corresponds to upstream `NcaFileSystemDriver::OpenStorageImpl`.
    fn open_storage_impl(
        &self,
        out_header_reader: &mut NcaFsHeaderReader,
        fs_index: i32,
        ctx: &mut StorageContext,
    ) -> Result<VirtualFile, ResultCode> {
        // Validate preconditions.
        assert!(0 <= fs_index && fs_index < NcaHeader::FS_COUNT_MAX);

        // Validate the fs index.
        if !self.reader.has_fs_info(fs_index) {
            return Err(RESULT_PARTITION_NOT_FOUND);
        }

        // Initialize our header reader for the fs index.
        out_header_reader.initialize(&self.reader, fs_index)?;

        // Get the body storage offset and size.
        let fs_offset = get_fs_offset(&self.reader, fs_index);
        let fs_end_offset = get_fs_end_offset(&self.reader, fs_index);
        let fs_size = fs_end_offset - fs_offset;

        // Validate the size.
        if fs_size <= 0 {
            return Err(RESULT_INVALID_NCA_FS_HEADER);
        }

        // Create a body sub-storage.
        let body_storage = self.create_body_sub_storage(fs_offset, fs_size)?;
        ctx.body_substorage = Some(body_storage.clone());

        // For raw storage mode, return the body directly.
        if ctx.open_raw_storage {
            return Ok(body_storage);
        }

        // Apply decryption layer based on the encryption type.
        // Corresponds to upstream OpenStorageImpl decryption logic.
        let encryption_type = out_header_reader.get_encryption_type();
        let decrypted_storage = if encryption_type == NcaFsEncryptionType::AesCtr as u8
            || encryption_type == NcaFsEncryptionType::AesCtrEx as u8
            || encryption_type == NcaFsEncryptionType::AesCtrSkipLayerHash as u8
            || encryption_type == NcaFsEncryptionType::AesCtrExSkipLayerHash as u8
        {
            // Get the AES-CTR decryption key.
            // Upstream checks HasExternalDecryptionKey() first (title key for rights-id NCAs).
            let key = if self.reader.has_external_decryption_key() {
                self.reader.get_external_decryption_key()
            } else {
                self.reader.get_decryption_key(DECRYPTION_KEY_AES_CTR)
            };

            // Build the base IV from the upper IV and the section offset.
            let upper_iv = out_header_reader.get_aes_ctr_upper_iv();
            let mut iv = [0u8; 16];
            AesCtrStorage::make_iv(&mut iv, upper_iv.value, fs_offset);

            log::trace!(
                "AES-CTR decryption: fs_index={}, offset=0x{:X}, size=0x{:X}",
                fs_index,
                fs_offset,
                fs_size
            );

            // Wrap the body storage in an AES-CTR decryption layer.
            let ctr_storage: VirtualFile = Arc::new(AesCtrStorage::new(body_storage, key, &iv));
            ctr_storage
        } else if encryption_type == NcaFsEncryptionType::None as u8 {
            // No encryption; use body storage as-is.
            body_storage
        } else {
            log::warn!(
                "Unsupported NCA FS encryption type: {} for fs_index={}",
                encryption_type,
                fs_index
            );
            body_storage
        };

        // Store as the fs data storage.
        ctx.fs_data_storage = Some(decrypted_storage.clone());

        // Apply hash/integrity layer to extract the data region.
        // Corresponds to upstream CreateStorageByRawStorage.
        let final_storage =
            self.create_storage_by_raw_storage(out_header_reader, decrypted_storage, ctx)?;

        Ok(final_storage)
    }

    /// Create a body sub-storage for the given offset and size.
    /// Corresponds to upstream `NcaFileSystemDriver::CreateBodySubStorage`.
    fn create_body_sub_storage(&self, offset: i64, size: i64) -> Result<VirtualFile, ResultCode> {
        let body_storage = self
            .reader
            .get_shared_body_storage()
            .ok_or(RESULT_INVALID_NCA_HEADER)?;

        let shared_body: VirtualFile =
            Arc::new(SharedNcaBodyStorage::new(body_storage, self.reader.clone()));

        let offset_storage: VirtualFile = Arc::new(OffsetVfsFile::new(
            shared_body,
            size as usize,
            offset as usize,
            String::new(),
        ));

        Ok(offset_storage)
    }

    /// Create a storage from raw storage and a header reader.
    /// Applies hash/integrity verification layers to extract the data region.
    ///
    /// Corresponds to upstream `NcaFileSystemDriver::CreateStorageByRawStorage`.
    ///
    /// NOTE: Currently implements data region extraction without integrity
    /// verification. The full implementation would create
    /// HierarchicalSha256Storage or HierarchicalIntegrityVerificationStorage.
    pub fn create_storage_by_raw_storage(
        &self,
        header_reader: &NcaFsHeaderReader,
        raw_storage: VirtualFile,
        ctx: &mut StorageContext,
    ) -> Result<VirtualFile, ResultCode> {
        let hash_type = header_reader.get_hash_type();

        if hash_type == NcaFsHashType::HierarchicalSha256Hash as u8
            || hash_type == NcaFsHashType::HierarchicalSha3256Hash as u8
        {
            // Extract data region from HierarchicalSha256 hash data.
            // The hash data contains layer regions; the last layer is the data region.
            let hash_data = header_reader.get_hash_data();
            let sha256_data = unsafe { hash_data.as_hierarchical_sha256() };

            let layer_count = sha256_data.hash_layer_count as usize;
            log::debug!(
                "HierarchicalSha256: hash_block_size=0x{:X}, layer_count={}",
                sha256_data.hash_block_size,
                layer_count
            );
            for i in 0..layer_count.min(HierarchicalSha256Data::HASH_LAYER_COUNT_MAX) {
                let r = &sha256_data.hash_layer_region[i];
                log::trace!(
                    "  region[{}]: offset=0x{:X}, size=0x{:X}",
                    i,
                    r.offset.get(),
                    r.size.get()
                );
            }
            if layer_count >= 2 && layer_count <= HierarchicalSha256Data::HASH_LAYER_COUNT_MAX {
                let data_region = &sha256_data.hash_layer_region[layer_count - 1];
                let data_offset = data_region.offset.get() as usize;
                let data_size = data_region.size.get() as usize;

                log::debug!(
                    "HierarchicalSha256: extracting data region at offset=0x{:X}, size=0x{:X} (layer_count={})",
                    data_offset, data_size, layer_count
                );

                let data_storage: VirtualFile = Arc::new(OffsetVfsFile::new(
                    raw_storage,
                    data_size,
                    data_offset,
                    String::new(),
                ));
                ctx.fs_data_storage = Some(data_storage.clone());
                return Ok(data_storage);
            }
        } else if hash_type == NcaFsHashType::HierarchicalIntegrityHash as u8
            || hash_type == NcaFsHashType::HierarchicalIntegritySha3Hash as u8
        {
            // Extract data region from IntegrityMetaInfo.
            // The last level in the level hash info is the data region.
            let hash_data = header_reader.get_hash_data();
            let meta_info = unsafe { hash_data.as_integrity_meta_info() };

            let max_layers = meta_info.level_hash_info.max_layers as usize;
            if max_layers >= 2 {
                let data_idx = max_layers - 2;
                let data_level = &meta_info.level_hash_info.info[data_idx];
                let data_offset = data_level.offset.get() as usize;
                let data_size = data_level.size.get() as usize;

                log::debug!(
                    "HierarchicalIntegrity: extracting data region at offset=0x{:X}, size=0x{:X} (max_layers={})",
                    data_offset, data_size, max_layers
                );

                let data_storage: VirtualFile = Arc::new(OffsetVfsFile::new(
                    raw_storage,
                    data_size,
                    data_offset,
                    String::new(),
                ));
                ctx.fs_data_storage = Some(data_storage.clone());
                return Ok(data_storage);
            }
        }

        // For None or unsupported hash types, return raw storage.
        ctx.fs_data_storage = Some(raw_storage.clone());
        Ok(raw_storage)
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
        assert_eq!(
            NcaCryptoConfiguration::KEY_AREA_ENCRYPTION_KEY_INDEX_COUNT,
            3
        );
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
        let result = get_key_type_value(3, 0);
        assert_eq!(result, KeyType::InvalidKey as i32);
    }

    #[test]
    fn test_get_key_type_value_normal() {
        let result = get_key_type_value(1, 2);
        assert_eq!(result, 3 * 2 + 1);
    }

    #[test]
    fn test_storage_context_default() {
        let ctx = StorageContext::default();
        assert!(!ctx.open_raw_storage);
        assert!(ctx.body_substorage.is_none());
        assert!(ctx.fs_data_storage.is_none());
        assert!(ctx.current_sparse_storage.is_none());
        assert!(ctx.compressed_storage.is_none());
        assert!(ctx.indirect_storage.is_none());
    }

    #[test]
    fn test_integrity_cache_constants() {
        assert_eq!(INTEGRITY_DATA_CACHE_COUNT, 24);
        assert_eq!(INTEGRITY_HASH_CACHE_COUNT, 8);
        assert_eq!(INTEGRITY_DATA_CACHE_COUNT_FOR_META, 16);
        assert_eq!(INTEGRITY_HASH_CACHE_COUNT_FOR_META, 2);
    }
}
