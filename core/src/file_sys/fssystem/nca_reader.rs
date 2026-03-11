// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_nca_reader.cpp
// (header is embedded in fssystem_nca_file_system_driver.h)

use super::nca_header::*;
use crate::file_sys::vfs::vfs_types::VirtualFile;
use common::ResultCode;

/// NCA reader.
/// Corresponds to upstream `NcaReader`.
pub struct NcaReader {
    header: NcaHeader,
    decryption_keys: [[u8; 16]; DECRYPTION_KEY_COUNT],
    body_storage: Option<VirtualFile>,
    header_storage: Option<VirtualFile>,
    external_decryption_key: [u8; 16],
    is_software_aes_prioritized: bool,
    is_available_sw_key: bool,
    header_encryption_type: u8,
    is_header_sign1_signature_valid: bool,
    get_decompressor: Option<super::compression_common::GetDecompressorFunction>,
}

impl NcaReader {
    pub fn new() -> Self {
        Self {
            header: unsafe { std::mem::zeroed() },
            decryption_keys: [[0u8; 16]; DECRYPTION_KEY_COUNT],
            body_storage: None,
            header_storage: None,
            external_decryption_key: [0u8; 16],
            is_software_aes_prioritized: false,
            is_available_sw_key: false,
            header_encryption_type: 0,
            is_header_sign1_signature_valid: false,
            get_decompressor: None,
        }
    }

    /// Initialize the NCA reader from a base storage.
    /// Stub: reads raw header without decryption.
    /// TODO: implement header decryption and key derivation.
    pub fn initialize(
        &mut self,
        base_storage: VirtualFile,
        _crypto_cfg: &super::nca_file_system_driver::NcaCryptoConfiguration,
        _compression_cfg: &super::nca_file_system_driver::NcaCompressionConfiguration,
    ) -> Result<(), ResultCode> {
        // Read raw header.
        let mut header_bytes = vec![0u8; NcaHeader::SIZE];
        base_storage.read(&mut header_bytes, NcaHeader::SIZE, 0);

        // Copy header bytes into our header struct.
        unsafe {
            std::ptr::copy_nonoverlapping(
                header_bytes.as_ptr(),
                &mut self.header as *mut NcaHeader as *mut u8,
                NcaHeader::SIZE,
            );
        }

        self.body_storage = Some(base_storage.clone());
        self.header_storage = Some(base_storage);

        Ok(())
    }

    pub fn get_shared_body_storage(&self) -> Option<VirtualFile> {
        self.body_storage.clone()
    }

    pub fn get_magic(&self) -> u32 {
        self.header.magic
    }

    pub fn get_distribution_type(&self) -> u8 {
        self.header.distribution_type
    }

    pub fn get_content_type(&self) -> u8 {
        self.header.content_type
    }

    pub fn get_header_sign1_key_generation(&self) -> u8 {
        self.header.header1_signature_key_generation
    }

    pub fn get_key_generation(&self) -> u8 {
        self.header.get_proper_key_generation()
    }

    pub fn get_key_index(&self) -> u8 {
        self.header.key_index
    }

    pub fn get_content_size(&self) -> u64 {
        self.header.content_size
    }

    pub fn get_program_id(&self) -> u64 {
        self.header.program_id
    }

    pub fn get_content_index(&self) -> u32 {
        self.header.content_index
    }

    pub fn get_sdk_addon_version(&self) -> u32 {
        self.header.sdk_addon_version
    }

    pub fn get_rights_id(&self) -> &[u8; 16] {
        &self.header.rights_id
    }

    pub fn has_fs_info(&self, index: i32) -> bool {
        assert!(0 <= index && index < NcaHeader::FS_COUNT_MAX);
        let info = &self.header.fs_info[index as usize];
        info.start_sector != 0 || info.end_sector != 0
    }

    pub fn get_fs_count(&self) -> i32 {
        for i in 0..NcaHeader::FS_COUNT_MAX {
            if !self.has_fs_info(i) {
                return i;
            }
        }
        NcaHeader::FS_COUNT_MAX
    }

    pub fn get_fs_header_hash(&self, index: i32) -> &Hash {
        assert!(0 <= index && index < NcaHeader::FS_COUNT_MAX);
        &self.header.fs_header_hash[index as usize]
    }

    pub fn get_fs_info(&self, index: i32) -> &NcaFsInfo {
        assert!(0 <= index && index < NcaHeader::FS_COUNT_MAX);
        &self.header.fs_info[index as usize]
    }

    pub fn get_fs_offset(&self, index: i32) -> u64 {
        NcaHeader::sector_to_byte(self.get_fs_info(index).start_sector)
    }

    pub fn get_fs_end_offset(&self, index: i32) -> u64 {
        NcaHeader::sector_to_byte(self.get_fs_info(index).end_sector)
    }

    pub fn get_fs_size(&self, index: i32) -> u64 {
        self.get_fs_end_offset(index) - self.get_fs_offset(index)
    }

    pub fn get_decryption_key(&self, index: usize) -> &[u8; 16] {
        assert!(index < DECRYPTION_KEY_COUNT);
        &self.decryption_keys[index]
    }

    pub fn has_valid_internal_key(&self) -> bool {
        self.decryption_keys.iter().any(|k| k.iter().any(|&b| b != 0))
    }

    pub fn has_external_decryption_key(&self) -> bool {
        self.external_decryption_key.iter().any(|&b| b != 0)
    }

    pub fn get_external_decryption_key(&self) -> &[u8; 16] {
        &self.external_decryption_key
    }

    pub fn set_external_decryption_key(&mut self, key: &[u8]) {
        let len = std::cmp::min(key.len(), 16);
        self.external_decryption_key[..len].copy_from_slice(&key[..len]);
    }

    pub fn get_header_encryption_type(&self) -> u8 {
        self.header_encryption_type
    }

    pub fn is_software_aes_prioritized(&self) -> bool {
        self.is_software_aes_prioritized
    }

    pub fn prioritize_software_aes(&mut self) {
        self.is_software_aes_prioritized = true;
    }

    pub fn is_available_sw_key(&self) -> bool {
        self.is_available_sw_key
    }

    pub fn get_header_sign1_valid(&self) -> bool {
        self.is_header_sign1_signature_valid
    }

    pub fn get_decompressor(&self) -> Option<super::compression_common::GetDecompressorFunction> {
        self.get_decompressor
    }

    /// Read an NCA FS header at the given index.
    /// TODO: implement proper decryption.
    pub fn read_header(&self, index: i32) -> Result<NcaFsHeader, ResultCode> {
        assert!(0 <= index && index < NcaHeader::FS_COUNT_MAX);
        let offset = NcaHeader::SIZE + (index as usize) * NcaFsHeader::SIZE;
        let mut buf = vec![0u8; NcaFsHeader::SIZE];
        if let Some(ref hs) = self.header_storage {
            hs.read(&mut buf, NcaFsHeader::SIZE, offset);
        }
        let header: NcaFsHeader = unsafe { std::ptr::read(buf.as_ptr() as *const NcaFsHeader) };
        Ok(header)
    }
}

impl Default for NcaReader {
    fn default() -> Self {
        Self::new()
    }
}

/// NCA FS header reader.
/// Corresponds to upstream `NcaFsHeaderReader`.
pub struct NcaFsHeaderReader {
    data: NcaFsHeader,
    fs_index: i32,
}

impl NcaFsHeaderReader {
    pub fn new() -> Self {
        Self {
            data: unsafe { std::mem::zeroed() },
            fs_index: -1,
        }
    }

    pub fn initialize(&mut self, reader: &NcaReader, index: i32) -> Result<(), ResultCode> {
        self.data = reader.read_header(index)?;
        self.fs_index = index;
        Ok(())
    }

    pub fn is_initialized(&self) -> bool {
        self.fs_index >= 0
    }

    pub fn get_fs_index(&self) -> i32 {
        self.fs_index
    }

    pub fn get_version(&self) -> u16 {
        self.data.version
    }

    pub fn get_fs_type(&self) -> u8 {
        self.data.fs_type
    }

    pub fn get_hash_type(&self) -> u8 {
        self.data.hash_type
    }

    pub fn get_encryption_type(&self) -> u8 {
        self.data.encryption_type
    }

    pub fn get_patch_info(&self) -> &NcaPatchInfo {
        &self.data.patch_info
    }

    pub fn get_aes_ctr_upper_iv(&self) -> NcaAesCtrUpperIv {
        self.data.aes_ctr_upper_iv
    }

    pub fn is_skip_layer_hash_encryption(&self) -> bool {
        self.data.is_skip_layer_hash_encryption()
    }

    pub fn get_hash_target_offset(&self) -> Result<i64, ResultCode> {
        self.data.get_hash_target_offset()
    }

    pub fn exists_sparse_layer(&self) -> bool {
        self.data.sparse_info.get_physical_size() != 0
    }

    pub fn get_sparse_info(&self) -> &NcaSparseInfo {
        &self.data.sparse_info
    }

    pub fn exists_compression_layer(&self) -> bool {
        self.data.compression_info.bucket.offset.get() != 0
            || self.data.compression_info.bucket.size.get() != 0
    }

    pub fn get_compression_info(&self) -> &NcaCompressionInfo {
        &self.data.compression_info
    }

    pub fn get_hash_data(&self) -> &NcaFsHeaderHashData {
        &self.data.hash_data
    }
}

impl Default for NcaFsHeaderReader {
    fn default() -> Self {
        Self::new()
    }
}
