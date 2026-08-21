// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_nca_file_system_driver.h / .cpp

use super::aes_ctr_counter_extended_storage::{
    create_software_decryptor, AesCtrCounterExtendedStorage,
};
use super::aes_ctr_storage::AesCtrStorage;
use super::aes_xts_storage::AesXtsStorage;
use super::alignment_matching_storage::AlignmentMatchingStorage;
use super::bucket_tree::BucketTreeHeader;
use super::compressed_storage::CompressedStorage;
use super::compression_common::GetDecompressorFunction;
use super::fs_types::{HashSalt, INTEGRITY_MAX_LAYER_COUNT, INTEGRITY_MIN_LAYER_COUNT};
use super::hierarchical_integrity_verification_storage::{
    HierarchicalIntegrityVerificationInformation,
    HierarchicalIntegrityVerificationLevelInformation, HierarchicalIntegrityVerificationStorage,
    HierarchicalStorageInformation,
};
use super::hierarchical_sha256_storage::HierarchicalSha256Storage;
use super::hierarchical_sha3_storage::HierarchicalSha3Storage;
use super::indirect_storage::IndirectStorage;
use super::integrity_romfs_storage::IntegrityRomFsStorage;
use super::memory_resource_buffer_hold_storage::MemoryResourceBufferHoldStorage;
use super::nca_header::*;
use super::nca_reader::{NcaFsHeaderReader, NcaReader};
use super::sparse_storage::SparseStorage;
use super::switch_storage::{Region, RegionSwitchStorage};
use crate::file_sys::errors::*;
use crate::file_sys::vfs::vfs::VfsFile;
use crate::file_sys::vfs::vfs_offset::OffsetVfsFile;
use crate::file_sys::vfs::vfs_types::{VirtualDir, VirtualFile};
use crate::file_sys::vfs::vfs_vector::VectorVfsFile;
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

/// Decode a bucket-tree header copied byte-for-byte into an NCA FS header.
/// Corresponds to the two upstream `std::memcpy` calls from `NcaPatchInfo`.
fn decode_bucket_tree_header(bytes: &[u8; NcaBucketInfo::HEADER_SIZE]) -> BucketTreeHeader {
    BucketTreeHeader {
        magic: u32::from_le_bytes(bytes[0..4].try_into().unwrap()),
        version: u32::from_le_bytes(bytes[4..8].try_into().unwrap()),
        entry_count: i32::from_le_bytes(bytes[8..12].try_into().unwrap()),
        reserved: i32::from_le_bytes(bytes[12..16].try_into().unwrap()),
    }
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

        let (mut storage, fs_data_offset) = if out_header_reader.exists_sparse_layer() {
            let sparse_info = *out_header_reader.get_sparse_info();
            let (storage, fs_data_offset, sparse_storage, meta_storage, layer_info_storage) =
                if out_header_reader.exists_sparse_meta_hash_layer() {
                    self.create_sparse_storage_with_verification(
                        fs_index,
                        out_header_reader.get_aes_ctr_upper_iv(),
                        &sparse_info,
                        out_header_reader.get_sparse_meta_data_hash_data_info(),
                        out_header_reader.get_sparse_meta_hash_type(),
                    )?
                } else {
                    let (storage, offset, sparse, meta) = self.create_sparse_storage(
                        fs_index,
                        out_header_reader.get_aes_ctr_upper_iv(),
                        &sparse_info,
                    )?;
                    (storage, offset, sparse, meta, None)
                };
            ctx.current_sparse_storage = Some(sparse_storage);
            ctx.sparse_storage_meta_storage = meta_storage;
            ctx.sparse_layer_info_storage = layer_info_storage;
            (storage, fs_data_offset)
        } else {
            let fs_data_offset = get_fs_offset(&self.reader, fs_index);
            let fs_end_offset = get_fs_end_offset(&self.reader, fs_index);
            let fs_size = fs_end_offset - fs_data_offset;
            if fs_size <= 0 {
                return Err(RESULT_INVALID_NCA_HEADER);
            }

            let storage = self.create_body_sub_storage(fs_data_offset, fs_size)?;
            ctx.body_substorage = Some(storage.clone());
            (storage, fs_data_offset)
        };

        let patch_info = *out_header_reader.get_patch_info();
        let mut patch_meta_aes_ctr_ex_meta_storage = None;
        let mut patch_meta_indirect_meta_storage = None;
        if out_header_reader.exists_patch_meta_hash_layer() {
            if out_header_reader.get_patch_meta_hash_type()
                != NcaFsMetaDataHashType::HierarchicalIntegrity as u8
            {
                return Err(RESULT_ROM_NCA_INVALID_PATCH_META_DATA_HASH_TYPE);
            }
            let (aes_ctr_ex_meta, indirect_meta, layer_info) = self.create_patch_meta_storage(
                storage.clone(),
                fs_data_offset,
                out_header_reader.get_aes_ctr_upper_iv(),
                &patch_info,
                out_header_reader.get_patch_meta_data_hash_data_info(),
            )?;
            patch_meta_aes_ctr_ex_meta_storage = Some(aes_ctr_ex_meta);
            patch_meta_indirect_meta_storage = Some(indirect_meta);
            ctx.patch_layer_info_storage = Some(layer_info);
        }

        if patch_info.has_aes_ctr_ex_table() {
            let encryption_type = out_header_reader.get_encryption_type();
            if encryption_type != NcaFsEncryptionType::None as u8
                && encryption_type != NcaFsEncryptionType::AesCtrEx as u8
                && encryption_type != NcaFsEncryptionType::AesCtrExSkipLayerHash as u8
            {
                return Err(RESULT_INVALID_NCA_FS_HEADER_ENCRYPTION_TYPE);
            }

            let meta_storage = match patch_meta_aes_ctr_ex_meta_storage {
                Some(storage) => storage,
                None => self.create_aes_ctr_ex_storage_meta_storage(
                    storage.clone(),
                    fs_data_offset,
                    encryption_type,
                    out_header_reader.get_aes_ctr_upper_iv(),
                    &patch_info,
                )?,
            };
            let (aes_storage, aes_impl) = self.create_aes_ctr_ex_storage(
                storage,
                meta_storage.clone(),
                fs_data_offset,
                out_header_reader.get_aes_ctr_upper_iv(),
                &patch_info,
            )?;
            storage = aes_storage;
            ctx.aes_ctr_ex_storage_meta_storage = Some(meta_storage);
            ctx.aes_ctr_ex_storage_data_storage = Some(storage.clone());
            ctx.aes_ctr_ex_storage = Some(aes_impl);
            ctx.fs_data_storage = Some(storage.clone());
        } else {
            storage = match out_header_reader.get_encryption_type() {
                value if value == NcaFsEncryptionType::None as u8 => storage,
                value if value == NcaFsEncryptionType::AesXts as u8 => {
                    self.create_aes_xts_storage(storage, fs_data_offset)
                }
                value if value == NcaFsEncryptionType::AesCtr as u8 => self.create_aes_ctr_storage(
                    storage,
                    fs_data_offset,
                    out_header_reader.get_aes_ctr_upper_iv(),
                    AlignmentStorageRequirement::None,
                ),
                value if value == NcaFsEncryptionType::AesCtrSkipLayerHash as u8 => {
                    let aes_ctr_storage = self.create_aes_ctr_storage(
                        storage.clone(),
                        fs_data_offset,
                        out_header_reader.get_aes_ctr_upper_iv(),
                        AlignmentStorageRequirement::None,
                    );
                    self.create_region_switch_storage(out_header_reader, storage, aes_ctr_storage)?
                }
                _ => return Err(RESULT_INVALID_NCA_FS_HEADER_ENCRYPTION_TYPE),
            };
            ctx.fs_data_storage = Some(storage.clone());
        }

        if patch_info.has_indirect_table() {
            let meta_storage = match patch_meta_indirect_meta_storage {
                Some(storage) => storage,
                None => self.create_indirect_storage_meta_storage(storage.clone(), &patch_info)?,
            };
            ctx.indirect_storage_meta_storage = Some(meta_storage.clone());

            let original_storage = if let Some(original_reader) = &self.original_reader {
                if original_reader.has_fs_info(fs_index) {
                    let original_driver = Self::new(original_reader.clone());
                    let mut original_header_reader = NcaFsHeaderReader::new();
                    original_header_reader.initialize(original_reader, fs_index)?;
                    original_driver
                        .open_indirectable_storage_as_original(&original_header_reader, ctx)?
                } else {
                    Arc::new(VectorVfsFile::new(Vec::new(), String::new(), None))
                }
            } else if let Some(external) = &ctx.external_original_storage {
                external.clone()
            } else {
                Arc::new(VectorVfsFile::new(Vec::new(), String::new(), None))
            };

            let (indirect, indirect_impl) =
                self.create_indirect_storage(storage, original_storage, meta_storage, &patch_info)?;
            storage = indirect;
            ctx.indirect_storage = Some(indirect_impl);
        }

        if out_header_reader.exists_sparse_layer() || ctx.open_raw_storage {
            return Ok(storage);
        }

        self.create_storage_by_raw_storage(out_header_reader, storage, ctx)
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

        let body_size = shared_body.get_size() as i64;
        if offset < 0 || size < 0 || offset.checked_add(size).is_none_or(|end| end > body_size) {
            return Err(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_B);
        }

        let offset_storage: VirtualFile = Arc::new(OffsetVfsFile::new(
            shared_body,
            size as usize,
            offset as usize,
            String::new(),
        ));

        Ok(offset_storage)
    }

    /// Corresponds to upstream `CreateAesCtrStorage` for the encryption types
    /// used by original and patch NCAs.
    fn create_aes_ctr_storage(
        &self,
        base_storage: VirtualFile,
        offset: i64,
        upper_iv: NcaAesCtrUpperIv,
        _alignment_storage_requirement: AlignmentStorageRequirement,
    ) -> VirtualFile {
        let key = if self.reader.has_external_decryption_key() {
            self.reader.get_external_decryption_key()
        } else {
            self.reader.get_decryption_key(DECRYPTION_KEY_AES_CTR)
        };
        let mut iv = [0u8; 16];
        AesCtrStorage::make_iv(&mut iv, upper_iv.value, offset);
        let ctr: VirtualFile = Arc::new(AesCtrStorage::new(base_storage, key, &iv));
        Arc::new(AlignmentMatchingStorage::new(
            ctr,
            NcaHeader::CTR_BLOCK_SIZE,
            1,
        ))
    }

    /// Corresponds to upstream `CreateAesXtsStorage`.
    fn create_aes_xts_storage(&self, base_storage: VirtualFile, offset: i64) -> VirtualFile {
        let mut iv = [0u8; 16];
        AesXtsStorage::make_aes_xts_iv(&mut iv, offset, NcaHeader::XTS_BLOCK_SIZE);
        Arc::new(AesXtsStorage::new(
            base_storage,
            self.reader.get_decryption_key(DECRYPTION_KEY_AES_XTS1),
            self.reader.get_decryption_key(DECRYPTION_KEY_AES_XTS2),
            &iv,
            NcaHeader::XTS_BLOCK_SIZE,
        ))
    }

    /// Corresponds to upstream `CreateSparseStorageMetaStorage`.
    fn create_sparse_storage_meta_storage(
        &self,
        base_storage: VirtualFile,
        offset: i64,
        upper_iv: NcaAesCtrUpperIv,
        sparse_info: &NcaSparseInfo,
    ) -> Result<VirtualFile, ResultCode> {
        let base_size = base_storage.get_size() as i64;
        let meta_offset = sparse_info.bucket.offset.get();
        let meta_size = sparse_info.bucket.size.get();
        if meta_offset
            .checked_add(meta_size)
            .and_then(|end| end.checked_sub(offset))
            .is_none_or(|end| end > base_size)
        {
            return Err(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_B);
        }

        let encrypted: VirtualFile = Arc::new(OffsetVfsFile::new(
            base_storage,
            meta_size as usize,
            meta_offset as usize,
            String::new(),
        ));
        let decrypted = self.create_aes_ctr_storage(
            encrypted,
            offset + meta_offset,
            sparse_info.make_aes_ctr_upper_iv(upper_iv),
            AlignmentStorageRequirement::None,
        );
        let mut meta_data = vec![0u8; meta_size as usize];
        decrypted.read(&mut meta_data, meta_size as usize, 0);
        Ok(Arc::new(VectorVfsFile::new(meta_data, String::new(), None)))
    }

    /// Corresponds to upstream `CreateSparseStorageCore`.
    fn create_sparse_storage_core(
        &self,
        base_storage: VirtualFile,
        base_size: i64,
        meta_storage: VirtualFile,
        sparse_info: &NcaSparseInfo,
        external_info: bool,
    ) -> Result<Arc<SparseStorage>, ResultCode> {
        let header = decode_bucket_tree_header(&sparse_info.bucket.header);
        header.verify()?;
        assert_ne!(header.entry_count, 0);

        let node_size = SparseStorage::query_node_storage_size(header.entry_count);
        let entry_size = SparseStorage::query_entry_storage_size(header.entry_count);
        let node_storage: VirtualFile = Arc::new(OffsetVfsFile::new(
            meta_storage.clone(),
            node_size as usize,
            0,
            String::new(),
        ));
        let entry_storage: VirtualFile = Arc::new(OffsetVfsFile::new(
            meta_storage,
            entry_size as usize,
            node_size as usize,
            String::new(),
        ));

        let mut sparse_storage = SparseStorage::new();
        sparse_storage.initialize(node_storage, entry_storage, header.entry_count)?;
        if !external_info {
            sparse_storage.set_data_storage(Arc::new(OffsetVfsFile::new(
                base_storage,
                base_size as usize,
                0,
                String::new(),
            )));
        }
        Ok(Arc::new(sparse_storage))
    }

    /// Corresponds to upstream `CreateSparseStorage`.
    fn create_sparse_storage(
        &self,
        index: i32,
        upper_iv: NcaAesCtrUpperIv,
        sparse_info: &NcaSparseInfo,
    ) -> Result<(VirtualFile, i64, Arc<SparseStorage>, Option<VirtualFile>), ResultCode> {
        if sparse_info.generation == 0 {
            return Err(RESULT_INVALID_NCA_HEADER);
        }
        let header = decode_bucket_tree_header(&sparse_info.bucket.header);
        header.verify()?;

        let fs_offset = get_fs_offset(&self.reader, index);
        let fs_size = get_fs_end_offset(&self.reader, index) - fs_offset;
        let (sparse_storage, meta_storage) = if header.entry_count != 0 {
            let physical_size = sparse_info.get_physical_size();
            let body =
                self.create_body_sub_storage(sparse_info.physical_offset.get(), physical_size)?;
            let meta = self.create_sparse_storage_meta_storage(
                body.clone(),
                sparse_info.physical_offset.get(),
                upper_iv,
                sparse_info,
            )?;
            let sparse = self.create_sparse_storage_core(
                body,
                physical_size,
                meta.clone(),
                sparse_info,
                false,
            )?;
            (sparse, Some(meta))
        } else {
            let mut sparse = SparseStorage::new();
            sparse.initialize_empty(fs_size);
            (Arc::new(sparse), None)
        };
        let output: VirtualFile = sparse_storage.clone();
        Ok((output, fs_offset, sparse_storage, meta_storage))
    }

    /// Corresponds to upstream `CreateSparseStorageMetaStorageWithVerification`.
    fn create_sparse_storage_meta_storage_with_verification(
        &self,
        base_storage: VirtualFile,
        offset: i64,
        upper_iv: NcaAesCtrUpperIv,
        sparse_info: &NcaSparseInfo,
        meta_data_hash_data_info: &NcaMetaDataHashDataInfo,
    ) -> Result<(VirtualFile, VirtualFile), ResultCode> {
        let base_size = base_storage.get_size() as i64;
        let meta_offset = sparse_info.bucket.offset.get();
        let meta_size = sparse_info.bucket.size.get();
        if meta_offset
            .checked_add(meta_size)
            .and_then(|end| end.checked_sub(offset))
            .is_none_or(|end| end > base_size)
        {
            return Err(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_B);
        }

        let hash_offset = meta_data_hash_data_info.offset.get();
        let hash_size = common::alignment::align_up_signed(
            meta_data_hash_data_info.size.get(),
            NcaHeader::CTR_BLOCK_SIZE as u64,
        );
        if hash_offset
            .checked_add(hash_size)
            .is_none_or(|end| end > base_size)
        {
            return Err(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_B);
        }
        if meta_offset
            .checked_add(meta_size)
            .is_none_or(|end| end > hash_offset)
        {
            return Err(RESULT_ROM_NCA_INVALID_SPARSE_META_DATA_HASH_DATA_OFFSET);
        }
        if hash_offset % NcaHeader::CTR_BLOCK_SIZE as i64 != 0 {
            return Err(RESULT_ROM_NCA_INVALID_SPARSE_META_DATA_HASH_DATA_OFFSET);
        }
        if meta_offset % NcaHeader::CTR_BLOCK_SIZE as i64 != 0 {
            return Err(RESULT_INVALID_NCA_FS_HEADER);
        }

        let encrypted_size = hash_offset
            .checked_add(hash_size)
            .and_then(|end| end.checked_sub(meta_offset))
            .ok_or(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_B)?;
        let encrypted: VirtualFile = Arc::new(OffsetVfsFile::new(
            base_storage,
            encrypted_size as usize,
            meta_offset as usize,
            String::new(),
        ));
        let decrypted = self.create_aes_ctr_storage(
            encrypted,
            offset + meta_offset,
            sparse_info.make_aes_ctr_upper_iv(upper_iv),
            AlignmentStorageRequirement::None,
        );
        let (integrity, layer_info) = self
            .create_integrity_verification_storage_for_meta(
                decrypted,
                meta_offset,
                meta_data_hash_data_info,
            )
            .map_err(|error| {
                if error == RESULT_INVALID_NCA_META_DATA_HASH_DATA_SIZE {
                    RESULT_ROM_NCA_INVALID_SPARSE_META_DATA_HASH_DATA_SIZE
                } else if error == RESULT_INVALID_NCA_META_DATA_HASH_DATA_HASH {
                    RESULT_ROM_NCA_INVALID_SPARSE_META_DATA_HASH_DATA_HASH
                } else {
                    error
                }
            })?;
        let meta: VirtualFile = Arc::new(OffsetVfsFile::new(
            integrity,
            meta_size as usize,
            0,
            String::new(),
        ));
        Ok((meta, layer_info))
    }

    /// Corresponds to upstream `CreateSparseStorageWithVerification`.
    fn create_sparse_storage_with_verification(
        &self,
        index: i32,
        upper_iv: NcaAesCtrUpperIv,
        sparse_info: &NcaSparseInfo,
        meta_data_hash_data_info: &NcaMetaDataHashDataInfo,
        meta_data_hash_type: u8,
    ) -> Result<
        (
            VirtualFile,
            i64,
            Arc<SparseStorage>,
            Option<VirtualFile>,
            Option<VirtualFile>,
        ),
        ResultCode,
    > {
        if sparse_info.generation == 0 {
            return Err(RESULT_INVALID_NCA_HEADER);
        }
        let header = decode_bucket_tree_header(&sparse_info.bucket.header);
        header.verify()?;
        let fs_offset = get_fs_offset(&self.reader, index);
        let fs_size = get_fs_end_offset(&self.reader, index) - fs_offset;

        let (sparse_storage, meta_storage, layer_info_storage) = if header.entry_count != 0 {
            let body_size = common::alignment::align_up_signed(
                meta_data_hash_data_info.offset.get() + meta_data_hash_data_info.size.get(),
                NcaHeader::CTR_BLOCK_SIZE as u64,
            );
            let mut body =
                self.create_body_sub_storage(sparse_info.physical_offset.get(), body_size)?;

            if meta_data_hash_type != NcaFsMetaDataHashType::HierarchicalIntegrity as u8 {
                log::error!(
                    "Sparse meta hash type {} is not supported for verification; mounting sparse data without verification",
                    meta_data_hash_type
                );
                body = self.create_body_sub_storage(
                    sparse_info.physical_offset.get(),
                    sparse_info.get_physical_size(),
                )?;
                let sparse = self.create_sparse_storage_core(
                    body.clone(),
                    sparse_info.get_physical_size(),
                    body,
                    sparse_info,
                    false,
                )?;
                let output: VirtualFile = sparse.clone();
                return Ok((output, fs_offset, sparse, None, None));
            }

            let (meta, layer_info) = self.create_sparse_storage_meta_storage_with_verification(
                body.clone(),
                sparse_info.physical_offset.get(),
                upper_iv,
                sparse_info,
                meta_data_hash_data_info,
            )?;
            let sparse = self.create_sparse_storage_core(
                body,
                sparse_info.get_physical_size(),
                meta.clone(),
                sparse_info,
                false,
            )?;
            (sparse, Some(meta), Some(layer_info))
        } else {
            let mut sparse = SparseStorage::new();
            sparse.initialize_empty(fs_size);
            (Arc::new(sparse), None, None)
        };
        let output: VirtualFile = sparse_storage.clone();
        Ok((
            output,
            fs_offset,
            sparse_storage,
            meta_storage,
            layer_info_storage,
        ))
    }

    /// Corresponds to upstream `CreateAesCtrExStorageMetaStorage`.
    fn create_aes_ctr_ex_storage_meta_storage(
        &self,
        base_storage: VirtualFile,
        offset: i64,
        encryption_type: u8,
        upper_iv: NcaAesCtrUpperIv,
        patch_info: &NcaPatchInfo,
    ) -> Result<VirtualFile, ResultCode> {
        let indirect_offset = patch_info.indirect_offset.get();
        let indirect_size = patch_info.indirect_size.get();
        let meta_offset = patch_info.aes_ctr_ex_offset.get();
        let aes_ctr_ex_size = patch_info.aes_ctr_ex_size.get();
        if indirect_size <= 0 {
            return Err(RESULT_INVALID_NCA_PATCH_INFO_INDIRECT_SIZE);
        }
        if aes_ctr_ex_size <= 0 {
            return Err(RESULT_INVALID_NCA_PATCH_INFO_AES_CTR_EX_SIZE);
        }
        if indirect_offset
            .checked_add(indirect_size)
            .is_none_or(|end| end > meta_offset)
        {
            return Err(RESULT_INVALID_NCA_PATCH_INFO_AES_CTR_EX_OFFSET);
        }

        let meta_size =
            common::alignment::align_up_signed(aes_ctr_ex_size, NcaHeader::XTS_BLOCK_SIZE as u64);
        if meta_offset < 0
            || meta_size < 0
            || meta_offset
                .checked_add(meta_size)
                .is_none_or(|end| end > base_storage.get_size() as i64)
        {
            return Err(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_B);
        }

        let encrypted: VirtualFile = Arc::new(OffsetVfsFile::new(
            base_storage,
            meta_size as usize,
            meta_offset as usize,
            String::new(),
        ));
        let decrypted = if encryption_type == NcaFsEncryptionType::None as u8 {
            encrypted
        } else {
            self.create_aes_ctr_storage(
                encrypted,
                offset + meta_offset,
                upper_iv,
                AlignmentStorageRequirement::None,
            )
        };
        let mut meta_data = vec![0u8; meta_size as usize];
        if decrypted.read(&mut meta_data, meta_size as usize, 0) != meta_size as usize {
            return Err(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_B);
        }
        Ok(Arc::new(VectorVfsFile::new(meta_data, String::new(), None)))
    }

    /// Corresponds to upstream `CreateAesCtrExStorage`.
    fn create_aes_ctr_ex_storage(
        &self,
        base_storage: VirtualFile,
        meta_storage: VirtualFile,
        counter_offset: i64,
        upper_iv: NcaAesCtrUpperIv,
        patch_info: &NcaPatchInfo,
    ) -> Result<(VirtualFile, Arc<AesCtrCounterExtendedStorage>), ResultCode> {
        let header = decode_bucket_tree_header(&patch_info.aes_ctr_ex_header);
        header.verify()?;
        let data_size = patch_info.aes_ctr_ex_offset.get();
        let node_size = AesCtrCounterExtendedStorage::query_node_storage_size(header.entry_count);
        let entry_size = AesCtrCounterExtendedStorage::query_entry_storage_size(header.entry_count);
        if data_size < 0 || node_size < 0 || entry_size < 0 {
            return Err(RESULT_INVALID_NCA_PATCH_INFO_AES_CTR_EX_SIZE);
        }

        let data_storage: VirtualFile = Arc::new(OffsetVfsFile::new(
            base_storage,
            data_size as usize,
            0,
            String::new(),
        ));
        let node_storage: VirtualFile = Arc::new(OffsetVfsFile::new(
            meta_storage.clone(),
            node_size as usize,
            0,
            String::new(),
        ));
        let entry_storage: VirtualFile = Arc::new(OffsetVfsFile::new(
            meta_storage,
            entry_size as usize,
            node_size as usize,
            String::new(),
        ));

        let key = if self.reader.has_external_decryption_key() {
            self.reader.get_external_decryption_key()
        } else {
            self.reader.get_decryption_key(DECRYPTION_KEY_AES_CTR)
        };
        let mut implementation = AesCtrCounterExtendedStorage::new();
        implementation.initialize(
            key,
            upper_iv.secure_value(),
            counter_offset,
            data_storage,
            node_storage,
            entry_storage,
            header.entry_count,
            create_software_decryptor(),
        )?;
        let implementation = Arc::new(implementation);
        let implementation_file: VirtualFile = implementation.clone();
        let aligned: VirtualFile = Arc::new(AlignmentMatchingStorage::new(
            implementation_file,
            NcaHeader::CTR_BLOCK_SIZE,
            1,
        ));
        Ok((aligned, implementation))
    }

    /// Corresponds to upstream `CreateIndirectStorageMetaStorage`.
    fn create_indirect_storage_meta_storage(
        &self,
        base_storage: VirtualFile,
        patch_info: &NcaPatchInfo,
    ) -> Result<VirtualFile, ResultCode> {
        let offset = patch_info.indirect_offset.get();
        let size = patch_info.indirect_size.get();
        if offset < 0
            || size < 0
            || offset
                .checked_add(size)
                .is_none_or(|end| end > base_storage.get_size() as i64)
        {
            return Err(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_E);
        }
        let source: VirtualFile = Arc::new(OffsetVfsFile::new(
            base_storage,
            size as usize,
            offset as usize,
            String::new(),
        ));
        let mut data = vec![0u8; size as usize];
        if source.read(&mut data, size as usize, 0) != size as usize {
            return Err(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_E);
        }
        Ok(Arc::new(VectorVfsFile::new(data, String::new(), None)))
    }

    /// Corresponds to upstream `OpenIndirectableStorageAsOriginal`.
    fn open_indirectable_storage_as_original(
        &self,
        header_reader: &NcaFsHeaderReader,
        ctx: &mut StorageContext,
    ) -> Result<VirtualFile, ResultCode> {
        let fs_index = header_reader.get_fs_index();
        let (storage, fs_data_offset) = if header_reader.exists_sparse_layer() {
            let sparse_info = *header_reader.get_sparse_info();
            let (storage, offset, sparse, meta, layer_info) =
                if header_reader.exists_sparse_meta_hash_layer() {
                    self.create_sparse_storage_with_verification(
                        fs_index,
                        header_reader.get_aes_ctr_upper_iv(),
                        &sparse_info,
                        header_reader.get_sparse_meta_data_hash_data_info(),
                        header_reader.get_sparse_meta_hash_type(),
                    )?
                } else {
                    let (storage, offset, sparse, meta) = self.create_sparse_storage(
                        fs_index,
                        header_reader.get_aes_ctr_upper_iv(),
                        &sparse_info,
                    )?;
                    (storage, offset, sparse, meta, None)
                };
            ctx.original_sparse_storage = Some(sparse);
            ctx.sparse_storage_meta_storage = meta;
            ctx.sparse_layer_info_storage = layer_info;
            (storage, offset)
        } else {
            let offset = get_fs_offset(&self.reader, fs_index);
            let size = get_fs_end_offset(&self.reader, fs_index) - offset;
            if size <= 0 {
                return Err(RESULT_INVALID_NCA_HEADER);
            }
            (self.create_body_sub_storage(offset, size)?, offset)
        };

        match header_reader.get_encryption_type() {
            value if value == NcaFsEncryptionType::None as u8 => Ok(storage),
            value if value == NcaFsEncryptionType::AesXts as u8 => {
                Ok(self.create_aes_xts_storage(storage, fs_data_offset))
            }
            value if value == NcaFsEncryptionType::AesCtr as u8 => Ok(self.create_aes_ctr_storage(
                storage,
                fs_data_offset,
                header_reader.get_aes_ctr_upper_iv(),
                AlignmentStorageRequirement::CacheBlockSize,
            )),
            _ => Err(RESULT_INVALID_NCA_FS_HEADER_ENCRYPTION_TYPE),
        }
    }

    /// Corresponds to upstream `CreateIndirectStorage`.
    fn create_indirect_storage(
        &self,
        base_storage: VirtualFile,
        original_data_storage: VirtualFile,
        meta_storage: VirtualFile,
        patch_info: &NcaPatchInfo,
    ) -> Result<(VirtualFile, Arc<IndirectStorage>), ResultCode> {
        let header = decode_bucket_tree_header(&patch_info.indirect_header);
        header.verify()?;
        let node_size = IndirectStorage::query_node_storage_size(header.entry_count);
        let entry_size = IndirectStorage::query_entry_storage_size(header.entry_count);
        let metadata_size = patch_info.indirect_size.get();
        if node_size < 0
            || entry_size < 0
            || node_size
                .checked_add(entry_size)
                .is_none_or(|size| size > metadata_size)
        {
            return Err(RESULT_INVALID_NCA_INDIRECT_STORAGE_OUT_OF_RANGE);
        }

        let indirect_data_size = patch_info.indirect_offset.get();
        if indirect_data_size < 0 || indirect_data_size as usize % NcaHeader::XTS_BLOCK_SIZE != 0 {
            return Err(RESULT_INVALID_NCA_INDIRECT_STORAGE_OUT_OF_RANGE);
        }
        let indirect_data_storage: VirtualFile = Arc::new(OffsetVfsFile::new(
            base_storage,
            indirect_data_size as usize,
            0,
            String::new(),
        ));
        let node_storage: VirtualFile = Arc::new(OffsetVfsFile::new(
            meta_storage.clone(),
            node_size as usize,
            0,
            String::new(),
        ));
        let entry_storage: VirtualFile = Arc::new(OffsetVfsFile::new(
            meta_storage,
            entry_size as usize,
            node_size as usize,
            String::new(),
        ));

        let mut implementation = IndirectStorage::new();
        implementation.initialize(node_storage, entry_storage, header.entry_count)?;
        let original_size = original_data_storage.get_size();
        implementation.set_storage(
            0,
            Arc::new(OffsetVfsFile::new(
                original_data_storage,
                original_size,
                0,
                String::new(),
            )),
        );
        implementation.set_storage(
            1,
            Arc::new(OffsetVfsFile::new(
                indirect_data_storage,
                indirect_data_size as usize,
                0,
                String::new(),
            )),
        );
        let implementation = Arc::new(implementation);
        let output: VirtualFile = implementation.clone();
        Ok((output, implementation))
    }

    /// Corresponds to upstream `CreatePatchMetaStorage`.
    fn create_patch_meta_storage(
        &self,
        base_storage: VirtualFile,
        offset: i64,
        upper_iv: NcaAesCtrUpperIv,
        patch_info: &NcaPatchInfo,
        meta_data_hash_data_info: &NcaMetaDataHashDataInfo,
    ) -> Result<(VirtualFile, VirtualFile, VirtualFile), ResultCode> {
        assert_eq!(
            patch_info.aes_ctr_ex_size.get() % NcaHeader::XTS_BLOCK_SIZE as i64,
            0
        );

        let indirect_offset = patch_info.indirect_offset.get();
        let indirect_size = patch_info.indirect_size.get();
        let aes_ctr_ex_offset = patch_info.aes_ctr_ex_offset.get();
        let aes_ctr_ex_size = patch_info.aes_ctr_ex_size.get();
        if aes_ctr_ex_size < 0 || !patch_info.has_aes_ctr_ex_table() {
            return Err(RESULT_INVALID_NCA_PATCH_INFO_AES_CTR_EX_SIZE);
        }
        if indirect_size <= 0 || !patch_info.has_indirect_table() {
            return Err(RESULT_INVALID_NCA_PATCH_INFO_INDIRECT_SIZE);
        }
        if indirect_offset
            .checked_add(indirect_size)
            .is_none_or(|end| end > aes_ctr_ex_offset)
        {
            return Err(RESULT_INVALID_NCA_PATCH_INFO_AES_CTR_EX_OFFSET);
        }
        if aes_ctr_ex_offset
            .checked_add(aes_ctr_ex_size)
            .is_none_or(|end| end > meta_data_hash_data_info.offset.get())
        {
            return Err(RESULT_ROM_NCA_INVALID_PATCH_META_DATA_HASH_DATA_OFFSET);
        }

        let base_size = base_storage.get_size() as i64;
        if indirect_offset < 0
            || indirect_offset
                .checked_add(indirect_size)
                .is_none_or(|end| end > base_size)
        {
            return Err(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_E);
        }
        if aes_ctr_ex_offset < 0
            || aes_ctr_ex_offset
                .checked_add(aes_ctr_ex_size)
                .is_none_or(|end| end > base_size)
        {
            return Err(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_B);
        }

        let hash_data_offset = meta_data_hash_data_info.offset.get();
        let hash_data_size = common::alignment::align_up_signed(
            meta_data_hash_data_info.size.get(),
            NcaHeader::CTR_BLOCK_SIZE as u64,
        );
        if hash_data_offset < 0
            || hash_data_size < 0
            || hash_data_offset
                .checked_add(hash_data_size)
                .is_none_or(|end| end > base_size)
        {
            return Err(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_B);
        }

        let encrypted_size = hash_data_offset
            .checked_add(hash_data_size)
            .and_then(|end| end.checked_sub(indirect_offset))
            .filter(|size| *size >= 0)
            .ok_or(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_B)?;
        let encrypted: VirtualFile = Arc::new(OffsetVfsFile::new(
            base_storage,
            encrypted_size as usize,
            indirect_offset as usize,
            String::new(),
        ));
        let decrypted = self.create_aes_ctr_storage(
            encrypted,
            offset + indirect_offset,
            upper_iv,
            AlignmentStorageRequirement::None,
        );
        let (integrity_storage, layer_info_storage) = self
            .create_integrity_verification_storage_for_meta(
                decrypted,
                indirect_offset,
                meta_data_hash_data_info,
            )
            .map_err(|error| {
                if error == RESULT_INVALID_NCA_META_DATA_HASH_DATA_SIZE {
                    RESULT_ROM_NCA_INVALID_PATCH_META_DATA_HASH_DATA_SIZE
                } else if error == RESULT_INVALID_NCA_META_DATA_HASH_DATA_HASH {
                    RESULT_ROM_NCA_INVALID_PATCH_META_DATA_HASH_DATA_HASH
                } else {
                    error
                }
            })?;

        let indirect_meta: VirtualFile = Arc::new(OffsetVfsFile::new(
            integrity_storage.clone(),
            indirect_size as usize,
            0,
            String::new(),
        ));
        let aes_ctr_ex_meta: VirtualFile = Arc::new(OffsetVfsFile::new(
            integrity_storage,
            aes_ctr_ex_size as usize,
            (aes_ctr_ex_offset - indirect_offset) as usize,
            String::new(),
        ));
        Ok((aes_ctr_ex_meta, indirect_meta, layer_info_storage))
    }

    /// Corresponds to upstream `CreateIntegrityVerificationStorageForMeta`.
    fn create_integrity_verification_storage_for_meta(
        &self,
        base_storage: VirtualFile,
        offset: i64,
        meta_data_hash_data_info: &NcaMetaDataHashDataInfo,
    ) -> Result<(VirtualFile, VirtualFile), ResultCode> {
        if meta_data_hash_data_info.size.get() != std::mem::size_of::<NcaMetaDataHashData>() as i64
        {
            return Err(RESULT_INVALID_NCA_META_DATA_HASH_DATA_SIZE);
        }
        let metadata_offset = meta_data_hash_data_info
            .offset
            .get()
            .checked_sub(offset)
            .filter(|offset| *offset >= 0)
            .ok_or(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_D)?;
        let metadata_end = metadata_offset
            .checked_add(std::mem::size_of::<NcaMetaDataHashData>() as i64)
            .filter(|end| *end <= base_storage.get_size() as i64)
            .ok_or(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_D)?;

        let mut bytes = vec![0u8; std::mem::size_of::<NcaMetaDataHashData>()];
        let metadata_size = bytes.len();
        if base_storage.read(&mut bytes, metadata_size, metadata_offset as usize) != metadata_size {
            return Err(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_D);
        }
        let metadata =
            unsafe { std::ptr::read_unaligned(bytes.as_ptr().cast::<NcaMetaDataHashData>()) };
        let layer_info_offset = metadata
            .layer_info_offset
            .checked_sub(offset)
            .filter(|offset| *offset >= 0)
            .ok_or(RESULT_ROM_NCA_INVALID_INTEGRITY_LAYER_INFO_OFFSET)?;
        if layer_info_offset > metadata_end {
            return Err(RESULT_ROM_NCA_INVALID_INTEGRITY_LAYER_INFO_OFFSET);
        }

        let layer_info_storage: VirtualFile = Arc::new(OffsetVfsFile::new(
            base_storage.clone(),
            (metadata_end - layer_info_offset) as usize,
            layer_info_offset as usize,
            String::new(),
        ));
        let meta_storage: VirtualFile = Arc::new(OffsetVfsFile::new(
            base_storage,
            metadata_offset as usize,
            0,
            String::new(),
        ));
        let verified = self.create_integrity_verification_storage_impl(
            meta_storage,
            &metadata.integrity_meta_info,
            layer_info_offset,
            INTEGRITY_DATA_CACHE_COUNT_FOR_META,
            INTEGRITY_HASH_CACHE_COUNT_FOR_META,
            0,
        )?;
        Ok((verified, layer_info_storage))
    }

    /// Corresponds to upstream `CreateIntegrityVerificationStorageImpl`.
    fn create_integrity_verification_storage_impl(
        &self,
        base_storage: VirtualFile,
        meta_info: &IntegrityMetaInfo,
        layer_info_offset: i64,
        max_data_cache_entries: i32,
        max_hash_cache_entries: i32,
        buffer_level: i8,
    ) -> Result<VirtualFile, ResultCode> {
        assert!(layer_info_offset >= 0);
        let level_hash_info = HierarchicalIntegrityVerificationInformation {
            max_layers: meta_info.level_hash_info.max_layers,
            info: std::array::from_fn(|index| {
                let source = meta_info.level_hash_info.info[index];
                HierarchicalIntegrityVerificationLevelInformation {
                    offset: source.offset,
                    size: source.size,
                    block_order: source.block_order,
                    reserved: source.reserved,
                }
            }),
            seed: HashSalt {
                value: meta_info.level_hash_info.seed.value,
            },
        };
        let max_layers = level_hash_info.max_layers as usize;
        if !(INTEGRITY_MIN_LAYER_COUNT..=INTEGRITY_MAX_LAYER_COUNT).contains(&max_layers) {
            return Err(RESULT_INVALID_NCA_HIERARCHICAL_INTEGRITY_VERIFICATION_LAYER_COUNT);
        }

        let base_size = base_storage.get_size() as i64;
        let mut storage_info = HierarchicalStorageInformation::new();
        for index in 0..max_layers - 2 {
            let layer = level_hash_info.info[index];
            let start = layer_info_offset
                .checked_add(layer.offset.get())
                .filter(|start| *start >= 0)
                .ok_or(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_D)?;
            let end = start
                .checked_add(layer.size.get())
                .filter(|end| *end <= base_size)
                .ok_or(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_D)?;
            storage_info.storages[index + 1] = Some(Arc::new(OffsetVfsFile::new(
                base_storage.clone(),
                (end - start) as usize,
                start as usize,
                String::new(),
            )));
        }

        let last = level_hash_info.info[max_layers - 2];
        let last_offset = if layer_info_offset > 0 {
            0
        } else {
            last.offset.get()
        };
        let last_end = last_offset
            .checked_add(last.size.get())
            .filter(|end| last_offset >= 0 && *end <= base_size)
            .ok_or(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_D)?;
        if layer_info_offset > 0 && last_end > layer_info_offset {
            return Err(RESULT_ROM_NCA_INVALID_INTEGRITY_LAYER_INFO_OFFSET);
        }
        storage_info.storages[max_layers - 1] = Some(Arc::new(OffsetVfsFile::new(
            base_storage,
            (last_end - last_offset) as usize,
            last_offset as usize,
            String::new(),
        )));

        let mut integrity = IntegrityRomFsStorage::new();
        integrity.initialize(
            level_hash_info,
            meta_info.master_hash,
            storage_info,
            max_data_cache_entries,
            max_hash_cache_entries,
            buffer_level,
        )?;
        Ok(Arc::new(integrity))
    }

    /// Corresponds to upstream `CreateSha256Storage`.
    fn create_sha256_storage(
        &self,
        base_storage: VirtualFile,
        hash_data: &HierarchicalSha256Data,
    ) -> Result<VirtualFile, ResultCode> {
        if hash_data.hash_block_size <= 0 || hash_data.hash_block_size.count_ones() != 1 {
            return Err(RESULT_INVALID_HIERARCHICAL_SHA256_BLOCK_SIZE);
        }
        if hash_data.hash_layer_count != HierarchicalSha256Storage::LAYER_COUNT - 1 {
            return Err(RESULT_INVALID_HIERARCHICAL_SHA256_LAYER_COUNT);
        }

        let hash_region = hash_data.hash_layer_region[0];
        let data_region = hash_data.hash_layer_region[1];
        let hash_buffer_size = usize::try_from(hash_region.size.get())
            .map_err(|_| RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_C)?;
        let cache_buffer_size = 2usize
            .checked_mul(hash_data.hash_block_size as usize)
            .ok_or(RESULT_ALLOCATION_MEMORY_FAILED_IN_NCA_FILE_SYSTEM_DRIVER_I)?;
        let total_buffer_size = hash_buffer_size
            .checked_add(cache_buffer_size)
            .ok_or(RESULT_ALLOCATION_MEMORY_FAILED_IN_NCA_FILE_SYSTEM_DRIVER_I)?;
        let base_size = base_storage.get_size() as i64;
        let buffer_hold_storage = Arc::new(MemoryResourceBufferHoldStorage::new(
            base_storage,
            total_buffer_size,
        ));
        if !buffer_hold_storage.is_valid() {
            return Err(RESULT_ALLOCATION_MEMORY_FAILED_IN_NCA_FILE_SYSTEM_DRIVER_I);
        }
        if hash_region.offset.get() < 0
            || data_region.offset.get() < 0
            || hash_region
                .offset
                .get()
                .checked_add(hash_region.size.get())
                .is_none_or(|end| end > base_size)
            || data_region
                .offset
                .get()
                .checked_add(data_region.size.get())
                .is_none_or(|end| end > base_size)
        {
            return Err(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_C);
        }

        let master_hash_storage: VirtualFile = Arc::new(VectorVfsFile::new(
            hash_data.fs_data_master_hash.value.to_vec(),
            String::new(),
            None,
        ));
        let layers: [VirtualFile; 3] = [
            Arc::new(OffsetVfsFile::new(
                master_hash_storage,
                std::mem::size_of::<Hash>(),
                0,
                String::new(),
            )),
            Arc::new(OffsetVfsFile::new(
                buffer_hold_storage.clone(),
                hash_region.size.get() as usize,
                hash_region.offset.get() as usize,
                String::new(),
            )),
            Arc::new(OffsetVfsFile::new(
                buffer_hold_storage.clone(),
                data_region.size.get() as usize,
                data_region.offset.get() as usize,
                String::new(),
            )),
        ];
        let mut storage = HierarchicalSha256Storage::new();
        storage.initialize(
            &layers,
            HierarchicalSha256Storage::LAYER_COUNT,
            hash_data.hash_block_size as usize,
            &buffer_hold_storage.get_buffer()[..hash_buffer_size],
        )?;
        Ok(Arc::new(storage))
    }

    /// Corresponds to upstream `CreateSha3Storage`.
    fn create_sha3_storage(
        &self,
        base_storage: VirtualFile,
        hash_data: &HierarchicalSha256Data,
    ) -> Result<VirtualFile, ResultCode> {
        if hash_data.hash_block_size <= 0 || hash_data.hash_block_size.count_ones() != 1 {
            return Err(RESULT_INVALID_HIERARCHICAL_SHA256_BLOCK_SIZE);
        }
        if hash_data.hash_layer_count != HierarchicalSha3Storage::LAYER_COUNT - 1 {
            return Err(RESULT_INVALID_HIERARCHICAL_SHA256_LAYER_COUNT);
        }

        let hash_region = hash_data.hash_layer_region[0];
        let data_region = hash_data.hash_layer_region[1];
        let hash_buffer_size = usize::try_from(hash_region.size.get())
            .map_err(|_| RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_C)?;
        let cache_buffer_size = 2usize
            .checked_mul(hash_data.hash_block_size as usize)
            .ok_or(RESULT_ALLOCATION_MEMORY_FAILED_IN_NCA_FILE_SYSTEM_DRIVER_I)?;
        let total_buffer_size = hash_buffer_size
            .checked_add(cache_buffer_size)
            .ok_or(RESULT_ALLOCATION_MEMORY_FAILED_IN_NCA_FILE_SYSTEM_DRIVER_I)?;
        let base_size = base_storage.get_size() as i64;
        let buffer_hold_storage = Arc::new(MemoryResourceBufferHoldStorage::new(
            base_storage,
            total_buffer_size,
        ));
        if !buffer_hold_storage.is_valid() {
            return Err(RESULT_ALLOCATION_MEMORY_FAILED_IN_NCA_FILE_SYSTEM_DRIVER_I);
        }
        if hash_region.offset.get() < 0
            || data_region.offset.get() < 0
            || hash_region
                .offset
                .get()
                .checked_add(hash_region.size.get())
                .is_none_or(|end| end > base_size)
            || data_region
                .offset
                .get()
                .checked_add(data_region.size.get())
                .is_none_or(|end| end > base_size)
        {
            return Err(RESULT_NCA_BASE_STORAGE_OUT_OF_RANGE_C);
        }

        let master_hash_storage: VirtualFile = Arc::new(VectorVfsFile::new(
            hash_data.fs_data_master_hash.value.to_vec(),
            String::new(),
            None,
        ));
        let layers: [VirtualFile; 3] = [
            Arc::new(OffsetVfsFile::new(
                master_hash_storage,
                std::mem::size_of::<Hash>(),
                0,
                String::new(),
            )),
            Arc::new(OffsetVfsFile::new(
                buffer_hold_storage.clone(),
                hash_region.size.get() as usize,
                hash_region.offset.get() as usize,
                String::new(),
            )),
            Arc::new(OffsetVfsFile::new(
                buffer_hold_storage.clone(),
                data_region.size.get() as usize,
                data_region.offset.get() as usize,
                String::new(),
            )),
        ];
        let mut storage = HierarchicalSha3Storage::new();
        storage.initialize(
            &layers,
            HierarchicalSha3Storage::LAYER_COUNT,
            hash_data.hash_block_size as usize,
            &buffer_hold_storage.get_buffer()[..hash_buffer_size],
        )?;
        Ok(Arc::new(storage))
    }

    /// Corresponds to upstream `CreateIntegrityVerificationStorage`.
    fn create_integrity_verification_storage(
        &self,
        base_storage: VirtualFile,
        meta_info: &IntegrityMetaInfo,
    ) -> Result<VirtualFile, ResultCode> {
        self.create_integrity_verification_storage_impl(
            base_storage,
            meta_info,
            0,
            INTEGRITY_DATA_CACHE_COUNT,
            INTEGRITY_HASH_CACHE_COUNT,
            HierarchicalIntegrityVerificationStorage::get_default_data_cache_buffer_level(
                meta_info.level_hash_info.max_layers as u32,
            ),
        )
    }

    /// Corresponds to upstream `CreateRegionSwitchStorage`.
    fn create_region_switch_storage(
        &self,
        header_reader: &NcaFsHeaderReader,
        inside_storage: VirtualFile,
        outside_storage: VirtualFile,
    ) -> Result<VirtualFile, ResultCode> {
        assert_eq!(
            header_reader.get_hash_type(),
            NcaFsHashType::HierarchicalIntegrityHash as u8
        );
        let region = Region {
            offset: 0,
            size: header_reader.get_hash_target_offset()?,
        };
        Ok(Arc::new(RegionSwitchStorage::new(
            inside_storage,
            outside_storage,
            region,
        )))
    }

    /// Corresponds to upstream `CreateCompressedStorage`.
    fn create_compressed_storage(
        &self,
        base_storage: VirtualFile,
        compression_info: &NcaCompressionInfo,
    ) -> Result<(VirtualFile, Arc<CompressedStorage>, VirtualFile), ResultCode> {
        let get_decompressor = self
            .reader
            .get_decompressor()
            .expect("compression layer requires a decompressor provider");
        let header = decode_bucket_tree_header(&compression_info.bucket.header);
        header.verify()?;

        let table_offset = compression_info.bucket.offset.get();
        let table_size = compression_info.bucket.size.get();
        let node_size = CompressedStorage::query_node_storage_size(header.entry_count);
        let entry_size = CompressedStorage::query_entry_storage_size(header.entry_count);
        if node_size
            .checked_add(entry_size)
            .is_none_or(|size| size > table_size)
        {
            return Err(RESULT_INVALID_COMPRESSED_STORAGE_SIZE);
        }

        let meta_storage: VirtualFile = Arc::new(OffsetVfsFile::new(
            base_storage.clone(),
            table_size as usize,
            table_offset as usize,
            String::new(),
        ));
        let mut compressed = CompressedStorage::new();
        compressed.initialize(
            Arc::new(OffsetVfsFile::new(
                base_storage.clone(),
                table_offset as usize,
                0,
                String::new(),
            )),
            Arc::new(OffsetVfsFile::new(
                base_storage.clone(),
                node_size as usize,
                table_offset as usize,
                String::new(),
            )),
            Arc::new(OffsetVfsFile::new(
                base_storage,
                entry_size as usize,
                (table_offset + node_size) as usize,
                String::new(),
            )),
            header.entry_count,
            64 * 1024,
            640 * 1024,
            get_decompressor,
            16 * 1024,
            16 * 1024,
            32,
        )?;
        let compressed = Arc::new(compressed);
        let output: VirtualFile = compressed.clone();
        Ok((output, compressed, meta_storage))
    }

    /// Corresponds to upstream `NcaFileSystemDriver::CreateStorageByRawStorage`.
    pub fn create_storage_by_raw_storage(
        &self,
        header_reader: &NcaFsHeaderReader,
        raw_storage: VirtualFile,
        ctx: &mut StorageContext,
    ) -> Result<VirtualFile, ResultCode> {
        let mut storage = match header_reader.get_hash_type() {
            value if value == NcaFsHashType::HierarchicalSha256Hash as u8 => {
                let hash_data = unsafe { header_reader.get_hash_data().as_hierarchical_sha256() };
                self.create_sha256_storage(raw_storage, hash_data)?
            }
            value if value == NcaFsHashType::HierarchicalIntegrityHash as u8 => {
                let meta_info = unsafe { header_reader.get_hash_data().as_integrity_meta_info() };
                self.create_integrity_verification_storage(raw_storage, meta_info)?
            }
            value if value == NcaFsHashType::HierarchicalSha3256Hash as u8 => {
                let hash_data = unsafe { header_reader.get_hash_data().as_hierarchical_sha256() };
                self.create_sha3_storage(raw_storage, hash_data)?
            }
            value => {
                log::error!("Unhandled Fs HashType enum={value}");
                return Err(RESULT_INVALID_NCA_FS_HEADER_HASH_TYPE);
            }
        };

        if header_reader.exists_compression_layer() {
            let (compressed, implementation, meta_storage) =
                self.create_compressed_storage(storage, header_reader.get_compression_info())?;
            storage = compressed;
            ctx.compressed_storage = Some(implementation);
            ctx.compressed_storage_meta_storage = Some(meta_storage);
        }
        Ok(storage)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn virtual_file(data: Vec<u8>) -> VirtualFile {
        Arc::new(VectorVfsFile::new(data, String::new(), None))
    }

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

    #[test]
    fn patch_bucket_header_is_decoded_from_little_endian_bytes() {
        let mut bytes = [0u8; NcaBucketInfo::HEADER_SIZE];
        bytes[0..4].copy_from_slice(&super::super::bucket_tree::BUCKET_TREE_MAGIC.to_le_bytes());
        bytes[4..8].copy_from_slice(&1u32.to_le_bytes());
        bytes[8..12].copy_from_slice(&7i32.to_le_bytes());

        let header = decode_bucket_tree_header(&bytes);
        assert!(header.verify().is_ok());
        assert_eq!(header.entry_count, 7);
    }

    #[test]
    fn indirect_patch_storage_composes_original_then_patch_ranges() {
        use super::super::bucket_tree::{BUCKET_TREE_MAGIC, BUCKET_TREE_VERSION};
        use super::super::indirect_storage::{Entry, NODE_SIZE};

        let mut patch_info = NcaPatchInfo::default();
        patch_info
            .indirect_offset
            .set(NcaHeader::XTS_BLOCK_SIZE as i64);
        let node_size = IndirectStorage::query_node_storage_size(2) as usize;
        let entry_size = IndirectStorage::query_entry_storage_size(2) as usize;
        patch_info
            .indirect_size
            .set((node_size + entry_size) as i64);
        patch_info.indirect_header[0..4].copy_from_slice(&BUCKET_TREE_MAGIC.to_le_bytes());
        patch_info.indirect_header[4..8].copy_from_slice(&BUCKET_TREE_VERSION.to_le_bytes());
        patch_info.indirect_header[8..12].copy_from_slice(&2i32.to_le_bytes());

        let mut metadata = vec![0u8; node_size + entry_size];
        metadata[0..4].copy_from_slice(&0i32.to_le_bytes());
        metadata[4..8].copy_from_slice(&1i32.to_le_bytes());
        metadata[8..16].copy_from_slice(&8i64.to_le_bytes());
        metadata[16..24].copy_from_slice(&0i64.to_le_bytes());

        let entries = node_size;
        metadata[entries..entries + 4].copy_from_slice(&0i32.to_le_bytes());
        metadata[entries + 4..entries + 8].copy_from_slice(&2i32.to_le_bytes());
        metadata[entries + 8..entries + 16].copy_from_slice(&8i64.to_le_bytes());
        let first = Entry {
            virt_offset: 0i64.to_le_bytes(),
            phys_offset: 0i64.to_le_bytes(),
            storage_index: 0,
        };
        let second = Entry {
            virt_offset: 4i64.to_le_bytes(),
            phys_offset: 0i64.to_le_bytes(),
            storage_index: 1,
        };
        unsafe {
            std::ptr::copy_nonoverlapping(
                (&first as *const Entry).cast::<u8>(),
                metadata[entries + 16..].as_mut_ptr(),
                std::mem::size_of::<Entry>(),
            );
            std::ptr::copy_nonoverlapping(
                (&second as *const Entry).cast::<u8>(),
                metadata[entries + 16 + std::mem::size_of::<Entry>()..].as_mut_ptr(),
                std::mem::size_of::<Entry>(),
            );
        }

        let mut patch = vec![0u8; NcaHeader::XTS_BLOCK_SIZE];
        patch[..4].copy_from_slice(b"PCH!");
        let driver = NcaFileSystemDriver::new(Arc::new(NcaReader::new()));
        let (storage, _) = driver
            .create_indirect_storage(
                virtual_file(patch),
                virtual_file(b"BASE".to_vec()),
                virtual_file(metadata),
                &patch_info,
            )
            .unwrap();

        let mut output = [0u8; 8];
        let output_len = output.len();
        assert_eq!(storage.read(&mut output, output_len, 0), output_len);
        assert_eq!(&output, b"BASEPCH!");
        assert_eq!(NODE_SIZE, 16 * 1024);
    }

    #[test]
    fn metadata_integrity_storage_keeps_data_before_layer_info() {
        let mut meta_info: IntegrityMetaInfo = unsafe { std::mem::zeroed() };
        meta_info.level_hash_info.max_layers = 3;
        meta_info.level_hash_info.info[0].offset.set(0);
        meta_info.level_hash_info.info[0]
            .size
            .set(Hash::SIZE as i64);
        meta_info.level_hash_info.info[0].block_order = 5;
        meta_info.level_hash_info.info[1].offset.set(0);
        meta_info.level_hash_info.info[1].size.set(0x40);
        meta_info.level_hash_info.info[1].block_order = 5;

        let mut bytes = vec![0u8; 0x100];
        bytes[..4].copy_from_slice(b"META");
        let driver = NcaFileSystemDriver::new(Arc::new(NcaReader::new()));
        let storage = driver
            .create_integrity_verification_storage_impl(
                virtual_file(bytes),
                &meta_info,
                0x80,
                INTEGRITY_DATA_CACHE_COUNT_FOR_META,
                INTEGRITY_HASH_CACHE_COUNT_FOR_META,
                0,
            )
            .unwrap();

        let mut output = [0u8; 4];
        assert_eq!(storage.read(&mut output, 4, 0), 4);
        assert_eq!(&output, b"META");
        assert_eq!(storage.get_size(), 0x40);
    }

    fn two_layer_hash_data() -> HierarchicalSha256Data {
        let mut hash_data: HierarchicalSha256Data = unsafe { std::mem::zeroed() };
        hash_data.hash_block_size = 32;
        hash_data.hash_layer_count = 2;
        hash_data.hash_layer_region[0].offset.set(0);
        hash_data.hash_layer_region[0].size.set(32);
        hash_data.hash_layer_region[1].offset.set(32);
        hash_data.hash_layer_region[1].size.set(4);
        hash_data
    }

    #[test]
    fn sha256_and_sha3_factories_expose_the_verified_data_layer() {
        let driver = NcaFileSystemDriver::new(Arc::new(NcaReader::new()));
        let hash_data = two_layer_hash_data();
        let mut bytes = vec![0u8; 36];
        bytes[32..].copy_from_slice(b"DATA");

        for storage in [
            driver
                .create_sha256_storage(virtual_file(bytes.clone()), &hash_data)
                .unwrap(),
            driver
                .create_sha3_storage(virtual_file(bytes.clone()), &hash_data)
                .unwrap(),
        ] {
            let mut output = [0u8; 4];
            assert_eq!(storage.get_size(), 4);
            assert_eq!(storage.read(&mut output, 4, 0), 4);
            assert_eq!(&output, b"DATA");
        }
    }

    #[test]
    fn hash_factories_reject_negative_layer_offsets_before_vfs_conversion() {
        let driver = NcaFileSystemDriver::new(Arc::new(NcaReader::new()));
        let mut hash_data = two_layer_hash_data();
        hash_data.hash_layer_region[0].offset.set(-1);

        assert!(driver
            .create_sha256_storage(virtual_file(vec![0u8; 36]), &hash_data)
            .is_err());
        assert!(driver
            .create_sha3_storage(virtual_file(vec![0u8; 36]), &hash_data)
            .is_err());
    }
}
