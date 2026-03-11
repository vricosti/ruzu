// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_nca_header.h / .cpp

use super::fs_types::Int64;
use crate::file_sys::errors::*;
use common::ResultCode;

/// SHA-256 hash.
/// Corresponds to upstream `Hash`.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Hash {
    pub value: [u8; Self::SIZE],
}

impl Hash {
    pub const SIZE: usize = 256 / 8;
}

impl Default for Hash {
    fn default() -> Self {
        Self {
            value: [0u8; Self::SIZE],
        }
    }
}

const _: () = assert!(std::mem::size_of::<Hash>() == Hash::SIZE);

pub type NcaDigest = Hash;

// ============================================================================
// NcaHeader
// ============================================================================

/// Content type within an NCA.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum NcaContentType {
    Program = 0,
    Meta = 1,
    Control = 2,
    Manual = 3,
    Data = 4,
    PublicData = 5,
}

/// Distribution type within an NCA.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum NcaDistributionType {
    Download = 0,
    GameCard = 1,
}

/// Encryption type at the NCA level.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum NcaEncryptionType {
    Auto = 0,
    None = 1,
}

/// Decryption key indices.
pub const DECRYPTION_KEY_AES_XTS: usize = 0;
pub const DECRYPTION_KEY_AES_XTS1: usize = DECRYPTION_KEY_AES_XTS;
pub const DECRYPTION_KEY_AES_XTS2: usize = 1;
pub const DECRYPTION_KEY_AES_CTR: usize = 2;
pub const DECRYPTION_KEY_AES_CTR_EX: usize = 3;
pub const DECRYPTION_KEY_AES_CTR_HW: usize = 4;
pub const DECRYPTION_KEY_COUNT: usize = 5;

/// NCA file system info.
/// Corresponds to upstream `NcaHeader::FsInfo`.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NcaFsInfo {
    pub start_sector: u32,
    pub end_sector: u32,
    pub hash_sectors: u32,
    pub reserved: u32,
}

const _: () = assert!(std::mem::size_of::<NcaFsInfo>() == 0x10);

/// NCA header — 1 KiB.
/// Corresponds to upstream `NcaHeader`.
#[derive(Clone)]
#[repr(C)]
pub struct NcaHeader {
    pub header_sign_1: [u8; Self::HEADER_SIGN_SIZE],
    pub header_sign_2: [u8; Self::HEADER_SIGN_SIZE],
    pub magic: u32,
    pub distribution_type: u8,
    pub content_type: u8,
    pub key_generation: u8,
    pub key_index: u8,
    pub content_size: u64,
    pub program_id: u64,
    pub content_index: u32,
    pub sdk_addon_version: u32,
    pub key_generation_2: u8,
    pub header1_signature_key_generation: u8,
    pub reserved_222: [u8; 2],
    pub reserved_224: [u32; 3],
    pub rights_id: [u8; Self::RIGHTS_ID_SIZE],
    pub fs_info: [NcaFsInfo; Self::FS_COUNT_MAX as usize],
    pub fs_header_hash: [Hash; Self::FS_COUNT_MAX as usize],
    pub encrypted_key_area: [u8; Self::ENCRYPTED_KEY_AREA_SIZE],
}

impl NcaHeader {
    pub const MAGIC0: u32 = u32::from_le_bytes(*b"NCA0");
    pub const MAGIC1: u32 = u32::from_le_bytes(*b"NCA1");
    pub const MAGIC2: u32 = u32::from_le_bytes(*b"NCA2");
    pub const MAGIC3: u32 = u32::from_le_bytes(*b"NCA3");
    pub const MAGIC: u32 = Self::MAGIC3;

    pub const SIZE: usize = 1024;
    pub const FS_COUNT_MAX: i32 = 4;
    pub const HEADER_SIGN_COUNT: usize = 2;
    pub const HEADER_SIGN_SIZE: usize = 0x100;
    pub const ENCRYPTED_KEY_AREA_SIZE: usize = 0x100;
    pub const SECTOR_SIZE: usize = 0x200;
    pub const SECTOR_SHIFT: usize = 9;
    pub const RIGHTS_ID_SIZE: usize = 0x10;
    pub const XTS_BLOCK_SIZE: usize = 0x200;
    pub const CTR_BLOCK_SIZE: usize = 0x10;

    pub const fn sector_to_byte(sector: u32) -> u64 {
        (sector as u64) << Self::SECTOR_SHIFT
    }

    pub const fn byte_to_sector(byte: u64) -> u32 {
        (byte >> Self::SECTOR_SHIFT as u64) as u32
    }

    /// Returns the effective key generation value.
    /// Corresponds to upstream `NcaHeader::GetProperKeyGeneration`.
    pub fn get_proper_key_generation(&self) -> u8 {
        std::cmp::max(self.key_generation, self.key_generation_2)
    }
}

const _: () = assert!(std::mem::size_of::<NcaHeader>() == NcaHeader::SIZE);

// ============================================================================
// NcaBucketInfo
// ============================================================================

/// Bucket info for NCA patch/sparse/compression layers.
/// Corresponds to upstream `NcaBucketInfo`.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NcaBucketInfo {
    pub offset: Int64,
    pub size: Int64,
    pub header: [u8; Self::HEADER_SIZE],
}

impl NcaBucketInfo {
    pub const HEADER_SIZE: usize = 0x10;
}

// ============================================================================
// NcaPatchInfo
// ============================================================================

/// Patch info within an NCA FS header.
/// Corresponds to upstream `NcaPatchInfo`.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NcaPatchInfo {
    pub indirect_offset: Int64,
    pub indirect_size: Int64,
    pub indirect_header: [u8; NcaBucketInfo::HEADER_SIZE],
    pub aes_ctr_ex_offset: Int64,
    pub aes_ctr_ex_size: Int64,
    pub aes_ctr_ex_header: [u8; NcaBucketInfo::HEADER_SIZE],
}

impl NcaPatchInfo {
    pub const SIZE: usize = 0x40;
    pub const OFFSET: usize = 0x100;

    pub fn has_indirect_table(&self) -> bool {
        self.indirect_size.get() != 0
    }

    pub fn has_aes_ctr_ex_table(&self) -> bool {
        self.aes_ctr_ex_size.get() != 0
    }
}

// ============================================================================
// NcaAesCtrUpperIv
// ============================================================================

/// Upper IV for AES-CTR, used with NCA.
/// Corresponds to upstream `NcaAesCtrUpperIv`.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NcaAesCtrUpperIv {
    pub value: u64,
}

impl NcaAesCtrUpperIv {
    pub fn generation(&self) -> u32 {
        (self.value & 0xFFFF_FFFF) as u32
    }

    pub fn secure_value(&self) -> u32 {
        (self.value >> 32) as u32
    }

    pub fn set_generation(&mut self, generation: u32) {
        self.value = (self.value & 0xFFFF_FFFF_0000_0000) | (generation as u64);
    }
}

// ============================================================================
// NcaSparseInfo
// ============================================================================

/// Sparse info within an NCA FS header.
/// Corresponds to upstream `NcaSparseInfo`.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NcaSparseInfo {
    pub bucket: NcaBucketInfo,
    pub physical_offset: Int64,
    pub generation: u16,
    pub reserved: [u8; 6],
}

impl NcaSparseInfo {
    pub fn get_physical_size(&self) -> i64 {
        self.bucket.offset.get() + self.bucket.size.get()
    }

    pub fn get_generation(&self) -> u32 {
        (self.generation as u32) << 16
    }

    pub fn make_aes_ctr_upper_iv(&self, upper_iv: NcaAesCtrUpperIv) -> NcaAesCtrUpperIv {
        let mut sparse_upper_iv = upper_iv;
        sparse_upper_iv.set_generation(self.get_generation());
        sparse_upper_iv
    }
}

// ============================================================================
// NcaCompressionInfo
// ============================================================================

/// Compression info within an NCA FS header.
/// Corresponds to upstream `NcaCompressionInfo`.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NcaCompressionInfo {
    pub bucket: NcaBucketInfo,
    pub reserved: [u8; 8],
}

// ============================================================================
// NcaMetaDataHashDataInfo
// ============================================================================

/// Meta data hash data info.
/// Corresponds to upstream `NcaMetaDataHashDataInfo`.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NcaMetaDataHashDataInfo {
    pub offset: Int64,
    pub size: Int64,
    pub hash: Hash,
}

// ============================================================================
// NcaFsHeader
// ============================================================================

/// FS type within an NCA FS header.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum NcaFsType {
    RomFs = 0,
    PartitionFs = 1,
}

/// Encryption type within an NCA FS header.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum NcaFsEncryptionType {
    Auto = 0,
    None = 1,
    AesXts = 2,
    AesCtr = 3,
    AesCtrEx = 4,
    AesCtrSkipLayerHash = 5,
    AesCtrExSkipLayerHash = 6,
}

/// Hash type within an NCA FS header.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum NcaFsHashType {
    Auto = 0,
    None = 1,
    HierarchicalSha256Hash = 2,
    HierarchicalIntegrityHash = 3,
    AutoSha3 = 4,
    HierarchicalSha3256Hash = 5,
    HierarchicalIntegritySha3Hash = 6,
}

/// Meta data hash type within an NCA FS header.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum NcaFsMetaDataHashType {
    None = 0,
    HierarchicalIntegrity = 1,
}

/// Region within an NCA FS header.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NcaFsHeaderRegion {
    pub offset: Int64,
    pub size: Int64,
}

/// Hierarchical SHA-256 hash data within an NCA FS header.
/// Corresponds to upstream `NcaFsHeader::HashData::HierarchicalSha256Data`.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct HierarchicalSha256Data {
    pub fs_data_master_hash: Hash,
    pub hash_block_size: i32,
    pub hash_layer_count: i32,
    pub hash_layer_region: [NcaFsHeaderRegion; Self::HASH_LAYER_COUNT_MAX],
}

impl HierarchicalSha256Data {
    pub const HASH_LAYER_COUNT_MAX: usize = 5;
}

/// Level information for hierarchical integrity verification.
/// Corresponds to upstream `NcaFsHeader::HashData::IntegrityMetaInfo::LevelHashInfo::HierarchicalIntegrityVerificationLevelInformation`.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NcaFsHeaderLevelInfo {
    pub offset: Int64,
    pub size: Int64,
    pub block_order: i32,
    pub reserved: [u8; 4],
}

impl NcaFsHeaderLevelInfo {
    pub const INTEGRITY_MAX_LAYER_COUNT: usize = 7;
}

/// Signature salt for level hash info.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct NcaFsHeaderSignatureSalt {
    pub value: [u8; Self::SIZE],
}

impl NcaFsHeaderSignatureSalt {
    pub const SIZE: usize = 0x20;
}

impl Default for NcaFsHeaderSignatureSalt {
    fn default() -> Self {
        Self {
            value: [0u8; Self::SIZE],
        }
    }
}

/// Level hash info.
/// Corresponds to upstream `NcaFsHeader::HashData::IntegrityMetaInfo::LevelHashInfo`.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct NcaFsHeaderLevelHashInfo {
    pub max_layers: u32,
    pub info: [NcaFsHeaderLevelInfo; NcaFsHeaderLevelInfo::INTEGRITY_MAX_LAYER_COUNT - 1],
    pub seed: NcaFsHeaderSignatureSalt,
}

/// Integrity meta info within the NCA FS header hash data.
/// Corresponds to upstream `NcaFsHeader::HashData::IntegrityMetaInfo`.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct IntegrityMetaInfo {
    pub magic: u32,
    pub version: u32,
    pub master_hash_size: u32,
    pub level_hash_info: NcaFsHeaderLevelHashInfo,
    pub master_hash: Hash,
}

/// Hash data union within an NCA FS header.
/// In Rust we represent the union as the largest variant with accessors.
/// The union is `NcaPatchInfo::OFFSET - HASH_DATA_OFFSET` = 0x100 - 0x8 = 0xF8 bytes.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct NcaFsHeaderHashData {
    pub raw: [u8; NcaPatchInfo::OFFSET - NCA_FS_HEADER_HASH_DATA_OFFSET],
}

pub const NCA_FS_HEADER_HASH_DATA_OFFSET: usize = 0x8;

impl NcaFsHeaderHashData {
    /// Interpret as HierarchicalSha256Data. Caller must ensure hash_type is correct.
    pub unsafe fn as_hierarchical_sha256(&self) -> &HierarchicalSha256Data {
        &*(self.raw.as_ptr() as *const HierarchicalSha256Data)
    }

    /// Interpret as IntegrityMetaInfo. Caller must ensure hash_type is correct.
    pub unsafe fn as_integrity_meta_info(&self) -> &IntegrityMetaInfo {
        &*(self.raw.as_ptr() as *const IntegrityMetaInfo)
    }
}

/// NCA FS header — 0x200 bytes.
/// Corresponds to upstream `NcaFsHeader`.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct NcaFsHeader {
    pub version: u16,
    pub fs_type: u8,
    pub hash_type: u8,
    pub encryption_type: u8,
    pub meta_data_hash_type: u8,
    pub reserved: [u8; 2],
    pub hash_data: NcaFsHeaderHashData,
    pub patch_info: NcaPatchInfo,
    pub aes_ctr_upper_iv: NcaAesCtrUpperIv,
    pub sparse_info: NcaSparseInfo,
    pub compression_info: NcaCompressionInfo,
    pub meta_data_hash_data_info: NcaMetaDataHashDataInfo,
    pub pad: [u8; 0x30],
}

impl NcaFsHeader {
    pub const SIZE: usize = 0x200;

    pub fn is_skip_layer_hash_encryption(&self) -> bool {
        self.encryption_type == NcaFsEncryptionType::AesCtrSkipLayerHash as u8
            || self.encryption_type == NcaFsEncryptionType::AesCtrExSkipLayerHash as u8
    }

    pub fn get_hash_target_offset(&self) -> Result<i64, ResultCode> {
        match self.hash_type {
            x if x == NcaFsHashType::HierarchicalIntegrityHash as u8
                || x == NcaFsHashType::HierarchicalIntegritySha3Hash as u8 =>
            {
                let info = unsafe { self.hash_data.as_integrity_meta_info() };
                let idx = (info.level_hash_info.max_layers as usize).saturating_sub(2);
                Ok(info.level_hash_info.info[idx].offset.get())
            }
            x if x == NcaFsHashType::HierarchicalSha256Hash as u8
                || x == NcaFsHashType::HierarchicalSha3256Hash as u8 =>
            {
                let data = unsafe { self.hash_data.as_hierarchical_sha256() };
                let idx = (data.hash_layer_count as usize).saturating_sub(1);
                Ok(data.hash_layer_region[idx].offset.get())
            }
            _ => Err(RESULT_INVALID_NCA_FS_HEADER),
        }
    }
}

const _: () = assert!(std::mem::size_of::<NcaFsHeader>() == NcaFsHeader::SIZE);

/// NCA meta data hash data.
/// Corresponds to upstream `NcaMetaDataHashData`.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct NcaMetaDataHashData {
    pub layer_info_offset: i64,
    pub integrity_meta_info: IntegrityMetaInfo,
}
