// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_nca_reader.cpp
// (header is embedded in fssystem_nca_file_system_driver.h)

use super::nca_header::*;
use crate::file_sys::errors::*;
use crate::file_sys::vfs::vfs::VfsFile;
use crate::file_sys::vfs::vfs_offset::OffsetVfsFile;
use crate::file_sys::vfs::vfs_types::VirtualFile;
use common::ResultCode;
use std::sync::Arc;

/// Minimum SDK addon version required.
/// Corresponds to upstream `SdkAddonVersionMin`.
const SDK_ADDON_VERSION_MIN: u32 = 0x000B0000;

/// AES-128 key size in bytes.
const AES_128_KEY_SIZE: usize = 0x10;

/// Zero key for comparison.
const ZERO_KEY: [u8; AES_128_KEY_SIZE] = [0u8; AES_128_KEY_SIZE];

/// Check NCA magic value.
/// Corresponds to upstream anonymous `CheckNcaMagic`.
fn check_nca_magic(magic: u32) -> Result<(), ResultCode> {
    // Verify the magic is not a deprecated one.
    if magic == NcaHeader::MAGIC0 {
        return Err(RESULT_UNSUPPORTED_SDK_VERSION);
    }
    if magic == NcaHeader::MAGIC1 {
        return Err(RESULT_UNSUPPORTED_SDK_VERSION);
    }
    if magic == NcaHeader::MAGIC2 {
        return Err(RESULT_UNSUPPORTED_SDK_VERSION);
    }

    // Verify the magic is the current one.
    if magic != NcaHeader::MAGIC3 {
        return Err(RESULT_INVALID_NCA_SIGNATURE);
    }

    Ok(())
}

/// NCA reader.
/// Corresponds to upstream `NcaReader`.
pub struct NcaReader {
    header: NcaHeader,
    decryption_keys: [[u8; AES_128_KEY_SIZE]; DECRYPTION_KEY_COUNT],
    body_storage: Option<VirtualFile>,
    header_storage: Option<VirtualFile>,
    external_decryption_key: [u8; AES_128_KEY_SIZE],
    is_software_aes_prioritized: bool,
    is_available_sw_key: bool,
    header_encryption_type: NcaEncryptionType,
    is_header_sign1_signature_valid: bool,
    get_decompressor: Option<super::compression_common::GetDecompressorFunction>,
}

impl NcaReader {
    pub fn new() -> Self {
        Self {
            header: unsafe { std::mem::zeroed() },
            decryption_keys: [[0u8; AES_128_KEY_SIZE]; DECRYPTION_KEY_COUNT],
            body_storage: None,
            header_storage: None,
            external_decryption_key: [0u8; AES_128_KEY_SIZE],
            is_software_aes_prioritized: false,
            is_available_sw_key: false,
            header_encryption_type: NcaEncryptionType::Auto,
            is_header_sign1_signature_valid: false,
            get_decompressor: None,
        }
    }

    /// Initialize the NCA reader from a base storage.
    /// Corresponds to upstream `NcaReader::Initialize`.
    ///
    /// This performs header decryption (via AES-XTS), magic validation,
    /// SDK version validation, key index validation, and key derivation.
    pub fn initialize(
        &mut self,
        base_storage: VirtualFile,
        crypto_cfg: &super::nca_file_system_driver::NcaCryptoConfiguration,
        compression_cfg: &super::nca_file_system_driver::NcaCompressionConfiguration,
    ) -> Result<(), ResultCode> {
        // Validate preconditions.
        assert!(self.body_storage.is_none());

        // We need to be able to generate keys.
        let generate_key = crypto_cfg.generate_key.ok_or(RESULT_INVALID_ARGUMENT)?;

        // Generate keys for header.
        use super::nca_file_system_driver::{KeyType, NcaCryptoConfiguration};

        let header_key_type_values: [i32; NcaCryptoConfiguration::HEADER_ENCRYPTION_KEY_COUNT
            as usize] = [KeyType::NCA_HEADER_KEY1, KeyType::NCA_HEADER_KEY2];

        let mut header_decryption_keys = [[0u8; NcaCryptoConfiguration::AES_128_KEY_SIZE];
            NcaCryptoConfiguration::HEADER_ENCRYPTION_KEY_COUNT as usize];
        for i in 0..NcaCryptoConfiguration::HEADER_ENCRYPTION_KEY_COUNT as usize {
            generate_key(
                &mut header_decryption_keys[i],
                &crypto_cfg.header_encrypted_encryption_keys[i],
                header_key_type_values[i],
            );
        }

        // Create the header storage (AES-XTS decrypted view).
        // In upstream, this creates an AesXtsStorage. We create it similarly.
        let header_iv = [0u8; super::aes_xts_storage::IV_SIZE];
        let work_header_storage: VirtualFile =
            Arc::new(super::aes_xts_storage::AesXtsStorage::new(
                base_storage.clone(),
                &header_decryption_keys[0],
                &header_decryption_keys[1],
                &header_iv,
                NcaHeader::XTS_BLOCK_SIZE,
            ));

        // Read the header.
        let mut header_bytes = vec![0u8; NcaHeader::SIZE];
        work_header_storage.read(&mut header_bytes, NcaHeader::SIZE, 0);

        // Copy header bytes into our header struct.
        unsafe {
            std::ptr::copy_nonoverlapping(
                header_bytes.as_ptr(),
                &mut self.header as *mut NcaHeader as *mut u8,
                NcaHeader::SIZE,
            );
        }

        // Track which header storage we're using.
        let mut final_header_storage = work_header_storage;

        // Validate the magic.
        if let Err(magic_result) = check_nca_magic(self.header.magic) {
            // Try to use a plaintext header.
            let mut plain_header_bytes = vec![0u8; NcaHeader::SIZE];
            base_storage.read(&mut plain_header_bytes, NcaHeader::SIZE, 0);

            unsafe {
                std::ptr::copy_nonoverlapping(
                    plain_header_bytes.as_ptr(),
                    &mut self.header as *mut NcaHeader as *mut u8,
                    NcaHeader::SIZE,
                );
            }

            // If plaintext header also fails, return the original error.
            check_nca_magic(self.header.magic).map_err(|_| magic_result)?;

            // Configure to use the plaintext header.
            log::debug!("NcaReader::initialize: using plaintext header (XTS decryption failed)");
            let base_storage_size = base_storage.get_size();
            final_header_storage = Arc::new(OffsetVfsFile::new(
                base_storage.clone(),
                base_storage_size,
                0,
                String::new(),
            ));

            // Set encryption type as plaintext.
            self.header_encryption_type = NcaEncryptionType::None;
        }

        // Verify the header sign1.
        if let Some(verify_sign1) = crypto_cfg.verify_sign1 {
            let sig = &self.header.header_sign_1;
            // The message is everything from magic onwards (skip the two signatures).
            let msg_offset = NcaHeader::HEADER_SIGN_SIZE * NcaHeader::HEADER_SIGN_COUNT;
            let header_raw = unsafe {
                std::slice::from_raw_parts(
                    &self.header as *const NcaHeader as *const u8,
                    NcaHeader::SIZE,
                )
            };
            let msg = &header_raw[msg_offset..];

            self.is_header_sign1_signature_valid =
                verify_sign1(sig, msg, self.header.header1_signature_key_generation);

            if !self.is_header_sign1_signature_valid {
                log::warn!("Invalid NCA header sign1");
            }
        }

        // Validate the sdk version.
        if self.header.sdk_addon_version < SDK_ADDON_VERSION_MIN {
            return Err(RESULT_UNSUPPORTED_SDK_VERSION);
        }

        // Validate the key index.
        if self.header.key_index
            >= NcaCryptoConfiguration::KEY_AREA_ENCRYPTION_KEY_INDEX_COUNT as u8
            && self.header.key_index
                != NcaCryptoConfiguration::KEY_AREA_ENCRYPTION_KEY_INDEX_ZERO_KEY
        {
            return Err(RESULT_INVALID_NCA_KEY_INDEX);
        }

        // Check if we have a rights id.
        let zero_rights_id = [0u8; NcaHeader::RIGHTS_ID_SIZE];
        if self.header.rights_id == zero_rights_id {
            // No rights id => generate decryption keys from key area.
            let key_generation = self.header.get_proper_key_generation();

            // AES-CTR key
            let ctr_offset = DECRYPTION_KEY_AES_CTR * AES_128_KEY_SIZE;
            let mut ctr_src = [0u8; AES_128_KEY_SIZE];
            ctr_src.copy_from_slice(
                &self.header.encrypted_key_area[ctr_offset..ctr_offset + AES_128_KEY_SIZE],
            );
            generate_key(
                &mut self.decryption_keys[DECRYPTION_KEY_AES_CTR],
                &ctr_src,
                super::nca_file_system_driver::get_key_type_value(
                    self.header.key_index,
                    key_generation,
                ),
            );

            // AES-XTS key 1
            let xts1_offset = DECRYPTION_KEY_AES_XTS1 * AES_128_KEY_SIZE;
            let mut xts1_src = [0u8; AES_128_KEY_SIZE];
            xts1_src.copy_from_slice(
                &self.header.encrypted_key_area[xts1_offset..xts1_offset + AES_128_KEY_SIZE],
            );
            generate_key(
                &mut self.decryption_keys[DECRYPTION_KEY_AES_XTS1],
                &xts1_src,
                super::nca_file_system_driver::get_key_type_value(
                    self.header.key_index,
                    key_generation,
                ),
            );

            // AES-XTS key 2
            let xts2_offset = DECRYPTION_KEY_AES_XTS2 * AES_128_KEY_SIZE;
            let mut xts2_src = [0u8; AES_128_KEY_SIZE];
            xts2_src.copy_from_slice(
                &self.header.encrypted_key_area[xts2_offset..xts2_offset + AES_128_KEY_SIZE],
            );
            generate_key(
                &mut self.decryption_keys[DECRYPTION_KEY_AES_XTS2],
                &xts2_src,
                super::nca_file_system_driver::get_key_type_value(
                    self.header.key_index,
                    key_generation,
                ),
            );

            // AES-CTR-EX key
            let ctr_ex_offset = DECRYPTION_KEY_AES_CTR_EX * AES_128_KEY_SIZE;
            let mut ctr_ex_src = [0u8; AES_128_KEY_SIZE];
            ctr_ex_src.copy_from_slice(
                &self.header.encrypted_key_area[ctr_ex_offset..ctr_ex_offset + AES_128_KEY_SIZE],
            );
            generate_key(
                &mut self.decryption_keys[DECRYPTION_KEY_AES_CTR_EX],
                &ctr_ex_src,
                super::nca_file_system_driver::get_key_type_value(
                    self.header.key_index,
                    key_generation,
                ),
            );

            // Copy the hardware speed emulation key (not decrypted, just raw copy).
            let hw_offset = DECRYPTION_KEY_AES_CTR_HW * AES_128_KEY_SIZE;
            self.decryption_keys[DECRYPTION_KEY_AES_CTR_HW].copy_from_slice(
                &self.header.encrypted_key_area[hw_offset..hw_offset + AES_128_KEY_SIZE],
            );
        }

        // Log key derivation results.
        log::trace!(
            "NcaReader::initialize: rights_id={:02X?}, key_index={}, key_gen={}",
            &self.header.rights_id,
            self.header.key_index,
            self.header.get_proper_key_generation(),
        );

        // Clear the external decryption key.
        self.external_decryption_key = [0u8; AES_128_KEY_SIZE];

        // Set software key availability.
        self.is_available_sw_key = crypto_cfg.is_available_sw_key;

        // Set our decompressor function getter.
        self.get_decompressor = compression_cfg.get_decompressor;

        // Set our storages.
        self.header_storage = Some(final_header_storage);
        self.body_storage = Some(base_storage);

        Ok(())
    }

    /// Get the shared body storage.
    /// Corresponds to upstream `NcaReader::GetSharedBodyStorage`.
    pub fn get_shared_body_storage(&self) -> Option<VirtualFile> {
        self.body_storage.clone()
    }

    /// Get the NCA magic number.
    /// Corresponds to upstream `NcaReader::GetMagic`.
    pub fn get_magic(&self) -> u32 {
        self.header.magic
    }

    /// Get the distribution type.
    /// Corresponds to upstream `NcaReader::GetDistributionType`.
    pub fn get_distribution_type(&self) -> u8 {
        self.header.distribution_type
    }

    /// Get the content type.
    /// Corresponds to upstream `NcaReader::GetContentType`.
    pub fn get_content_type(&self) -> u8 {
        self.header.content_type
    }

    /// Get the header sign1 key generation.
    /// Corresponds to upstream `NcaReader::GetHeaderSign1KeyGeneration`.
    pub fn get_header_sign1_key_generation(&self) -> u8 {
        self.header.header1_signature_key_generation
    }

    /// Get the effective key generation.
    /// Corresponds to upstream `NcaReader::GetKeyGeneration`.
    pub fn get_key_generation(&self) -> u8 {
        self.header.get_proper_key_generation()
    }

    /// Get the key index.
    /// Corresponds to upstream `NcaReader::GetKeyIndex`.
    pub fn get_key_index(&self) -> u8 {
        self.header.key_index
    }

    /// Get the content size.
    /// Corresponds to upstream `NcaReader::GetContentSize`.
    pub fn get_content_size(&self) -> u64 {
        self.header.content_size
    }

    /// Get the program ID.
    /// Corresponds to upstream `NcaReader::GetProgramId`.
    pub fn get_program_id(&self) -> u64 {
        self.header.program_id
    }

    /// Get the content index.
    /// Corresponds to upstream `NcaReader::GetContentIndex`.
    pub fn get_content_index(&self) -> u32 {
        self.header.content_index
    }

    /// Get the SDK addon version.
    /// Corresponds to upstream `NcaReader::GetSdkAddonVersion`.
    pub fn get_sdk_addon_version(&self) -> u32 {
        self.header.sdk_addon_version
    }

    /// Get the rights ID.
    /// Corresponds to upstream `NcaReader::GetRightsId`.
    pub fn get_rights_id(&self) -> &[u8; NcaHeader::RIGHTS_ID_SIZE] {
        &self.header.rights_id
    }

    /// Copy the rights ID into a destination buffer.
    /// Corresponds to upstream `NcaReader::GetRightsId(u8* dst, size_t dst_size)`.
    pub fn get_rights_id_into(&self, dst: &mut [u8]) {
        assert!(dst.len() >= NcaHeader::RIGHTS_ID_SIZE);
        dst[..NcaHeader::RIGHTS_ID_SIZE].copy_from_slice(&self.header.rights_id);
    }

    /// Check if a filesystem info entry exists at the given index.
    /// Corresponds to upstream `NcaReader::HasFsInfo`.
    pub fn has_fs_info(&self, index: i32) -> bool {
        assert!(0 <= index && index < NcaHeader::FS_COUNT_MAX);
        let info = &self.header.fs_info[index as usize];
        info.start_sector != 0 || info.end_sector != 0
    }

    /// Get the count of filesystem entries.
    /// Corresponds to upstream `NcaReader::GetFsCount`.
    pub fn get_fs_count(&self) -> i32 {
        for i in 0..NcaHeader::FS_COUNT_MAX {
            if !self.has_fs_info(i) {
                return i;
            }
        }
        NcaHeader::FS_COUNT_MAX
    }

    /// Get the filesystem header hash at the given index.
    /// Corresponds to upstream `NcaReader::GetFsHeaderHash`.
    pub fn get_fs_header_hash(&self, index: i32) -> &Hash {
        assert!(0 <= index && index < NcaHeader::FS_COUNT_MAX);
        &self.header.fs_header_hash[index as usize]
    }

    /// Copy the filesystem header hash into a destination.
    /// Corresponds to upstream `NcaReader::GetFsHeaderHash(Hash* dst, s32 index)`.
    pub fn get_fs_header_hash_into(&self, dst: &mut Hash, index: i32) {
        assert!(0 <= index && index < NcaHeader::FS_COUNT_MAX);
        *dst = self.header.fs_header_hash[index as usize];
    }

    /// Get the filesystem info at the given index.
    /// Corresponds to upstream `NcaReader::GetFsInfo`.
    pub fn get_fs_info(&self, index: i32) -> &NcaFsInfo {
        assert!(0 <= index && index < NcaHeader::FS_COUNT_MAX);
        &self.header.fs_info[index as usize]
    }

    /// Copy the filesystem info into a destination.
    /// Corresponds to upstream `NcaReader::GetFsInfo(NcaHeader::FsInfo* dst, s32 index)`.
    pub fn get_fs_info_into(&self, dst: &mut NcaFsInfo, index: i32) {
        assert!(0 <= index && index < NcaHeader::FS_COUNT_MAX);
        *dst = self.header.fs_info[index as usize];
    }

    /// Get the byte offset where a filesystem starts.
    /// Corresponds to upstream `NcaReader::GetFsOffset`.
    pub fn get_fs_offset(&self, index: i32) -> u64 {
        NcaHeader::sector_to_byte(self.get_fs_info(index).start_sector)
    }

    /// Get the byte offset where a filesystem ends.
    /// Corresponds to upstream `NcaReader::GetFsEndOffset`.
    pub fn get_fs_end_offset(&self, index: i32) -> u64 {
        NcaHeader::sector_to_byte(self.get_fs_info(index).end_sector)
    }

    /// Get the size of a filesystem in bytes.
    /// Corresponds to upstream `NcaReader::GetFsSize`.
    pub fn get_fs_size(&self, index: i32) -> u64 {
        self.get_fs_end_offset(index) - self.get_fs_offset(index)
    }

    /// Copy the encrypted key area into a destination buffer.
    /// Corresponds to upstream `NcaReader::GetEncryptedKey`.
    pub fn get_encrypted_key(&self, dst: &mut [u8]) {
        assert!(dst.len() >= NcaHeader::ENCRYPTED_KEY_AREA_SIZE);
        dst[..NcaHeader::ENCRYPTED_KEY_AREA_SIZE].copy_from_slice(&self.header.encrypted_key_area);
    }

    /// Get a reference to a decryption key by index.
    /// Corresponds to upstream `NcaReader::GetDecryptionKey`.
    pub fn get_decryption_key(&self, index: usize) -> &[u8; AES_128_KEY_SIZE] {
        assert!(index < DECRYPTION_KEY_COUNT);
        &self.decryption_keys[index]
    }

    /// Check if any internal key is non-zero.
    /// Corresponds to upstream `NcaReader::HasValidInternalKey`.
    pub fn has_valid_internal_key(&self) -> bool {
        for i in 0..DECRYPTION_KEY_COUNT {
            if self.header.encrypted_key_area[i * AES_128_KEY_SIZE..(i + 1) * AES_128_KEY_SIZE]
                != ZERO_KEY
            {
                return true;
            }
        }
        false
    }

    /// Check if the hardware AES-CTR decryption key is non-zero.
    /// Corresponds to upstream `NcaReader::HasInternalDecryptionKeyForAesHw`.
    pub fn has_internal_decryption_key_for_aes_hw(&self) -> bool {
        self.decryption_keys[DECRYPTION_KEY_AES_CTR_HW] != ZERO_KEY
    }

    /// Check if software AES is prioritized.
    /// Corresponds to upstream `NcaReader::IsSoftwareAesPrioritized`.
    pub fn is_software_aes_prioritized(&self) -> bool {
        self.is_software_aes_prioritized
    }

    /// Prioritize software AES.
    /// Corresponds to upstream `NcaReader::PrioritizeSoftwareAes`.
    pub fn prioritize_software_aes(&mut self) {
        self.is_software_aes_prioritized = true;
    }

    /// Check if software key is available.
    /// Corresponds to upstream `NcaReader::IsAvailableSwKey`.
    pub fn is_available_sw_key(&self) -> bool {
        self.is_available_sw_key
    }

    /// Check if an external decryption key is set (non-zero).
    /// Corresponds to upstream `NcaReader::HasExternalDecryptionKey`.
    pub fn has_external_decryption_key(&self) -> bool {
        self.external_decryption_key != ZERO_KEY
    }

    /// Get a reference to the external decryption key.
    /// Corresponds to upstream `NcaReader::GetExternalDecryptionKey`.
    pub fn get_external_decryption_key(&self) -> &[u8; AES_128_KEY_SIZE] {
        &self.external_decryption_key
    }

    /// Set the external decryption key.
    /// Corresponds to upstream `NcaReader::SetExternalDecryptionKey`.
    pub fn set_external_decryption_key(&mut self, key: &[u8]) {
        assert!(key.len() == AES_128_KEY_SIZE);
        self.external_decryption_key.copy_from_slice(key);
    }

    /// Copy the raw header data into a destination buffer.
    /// Corresponds to upstream `NcaReader::GetRawData`.
    pub fn get_raw_data(&self, dst: &mut [u8]) {
        assert!(dst.len() >= NcaHeader::SIZE);
        unsafe {
            std::ptr::copy_nonoverlapping(
                &self.header as *const NcaHeader as *const u8,
                dst.as_mut_ptr(),
                NcaHeader::SIZE,
            );
        }
    }

    /// Get the header encryption type.
    /// Corresponds to upstream `NcaReader::GetEncryptionType`.
    pub fn get_encryption_type(&self) -> NcaEncryptionType {
        self.header_encryption_type
    }

    /// Read an NCA FS header at the given index.
    /// Corresponds to upstream `NcaReader::ReadHeader`.
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

    /// Get the decompressor function.
    /// Corresponds to upstream `NcaReader::GetDecompressor`.
    pub fn get_decompressor(&self) -> Option<super::compression_common::GetDecompressorFunction> {
        self.get_decompressor
    }

    /// Check if the header sign1 signature is valid.
    /// Corresponds to upstream `NcaReader::GetHeaderSign1Valid`.
    pub fn get_header_sign1_valid(&self) -> bool {
        self.is_header_sign1_signature_valid
    }

    /// Copy the header sign2 into a destination buffer.
    /// Corresponds to upstream `NcaReader::GetHeaderSign2`.
    pub fn get_header_sign2(&self, dst: &mut [u8]) {
        assert!(dst.len() == NcaHeader::HEADER_SIGN_SIZE);
        dst.copy_from_slice(&self.header.header_sign_2);
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

    /// Initialize the reader from an NcaReader at the given filesystem index.
    /// Corresponds to upstream `NcaFsHeaderReader::Initialize`.
    pub fn initialize(&mut self, reader: &NcaReader, index: i32) -> Result<(), ResultCode> {
        // Reset ourselves to uninitialized.
        self.fs_index = -1;

        // Read the header.
        self.data = reader.read_header(index)?;

        // Set our index.
        self.fs_index = index;
        Ok(())
    }

    /// Check if this reader is initialized.
    /// Corresponds to upstream `NcaFsHeaderReader::IsInitialized`.
    pub fn is_initialized(&self) -> bool {
        self.fs_index >= 0
    }

    /// Copy the raw FS header data into a destination buffer.
    /// Corresponds to upstream `NcaFsHeaderReader::GetRawData`.
    pub fn get_raw_data(&self, dst: &mut [u8]) {
        assert!(self.is_initialized());
        assert!(dst.len() >= NcaFsHeader::SIZE);
        unsafe {
            std::ptr::copy_nonoverlapping(
                &self.data as *const NcaFsHeader as *const u8,
                dst.as_mut_ptr(),
                NcaFsHeader::SIZE,
            );
        }
    }

    /// Get the filesystem index.
    /// Corresponds to upstream `NcaFsHeaderReader::GetFsIndex`.
    pub fn get_fs_index(&self) -> i32 {
        assert!(self.is_initialized());
        self.fs_index
    }

    /// Get the header version.
    /// Corresponds to upstream `NcaFsHeaderReader::GetVersion`.
    pub fn get_version(&self) -> u16 {
        assert!(self.is_initialized());
        self.data.version
    }

    /// Get the filesystem type.
    /// Corresponds to upstream `NcaFsHeaderReader::GetFsType`.
    pub fn get_fs_type(&self) -> u8 {
        assert!(self.is_initialized());
        self.data.fs_type
    }

    /// Get the hash type.
    /// Corresponds to upstream `NcaFsHeaderReader::GetHashType`.
    pub fn get_hash_type(&self) -> u8 {
        assert!(self.is_initialized());
        self.data.hash_type
    }

    /// Get the encryption type.
    /// Corresponds to upstream `NcaFsHeaderReader::GetEncryptionType`.
    pub fn get_encryption_type(&self) -> u8 {
        assert!(self.is_initialized());
        self.data.encryption_type
    }

    /// Get a reference to the hash data.
    /// Corresponds to upstream `NcaFsHeaderReader::GetHashData`.
    pub fn get_hash_data(&self) -> &NcaFsHeaderHashData {
        assert!(self.is_initialized());
        &self.data.hash_data
    }

    /// Get a reference to the patch info.
    /// Corresponds to upstream `NcaFsHeaderReader::GetPatchInfo`.
    pub fn get_patch_info(&self) -> &NcaPatchInfo {
        assert!(self.is_initialized());
        &self.data.patch_info
    }

    /// Get the AES-CTR upper IV.
    /// Corresponds to upstream `NcaFsHeaderReader::GetAesCtrUpperIv`.
    pub fn get_aes_ctr_upper_iv(&self) -> NcaAesCtrUpperIv {
        assert!(self.is_initialized());
        self.data.aes_ctr_upper_iv
    }

    /// Check if layer hash encryption should be skipped.
    /// Corresponds to upstream `NcaFsHeaderReader::IsSkipLayerHashEncryption`.
    pub fn is_skip_layer_hash_encryption(&self) -> bool {
        assert!(self.is_initialized());
        self.data.is_skip_layer_hash_encryption()
    }

    /// Get the hash target offset.
    /// Corresponds to upstream `NcaFsHeaderReader::GetHashTargetOffset`.
    pub fn get_hash_target_offset(&self) -> Result<i64, ResultCode> {
        assert!(self.is_initialized());
        self.data.get_hash_target_offset()
    }

    /// Check if a sparse layer exists.
    /// Corresponds to upstream `NcaFsHeaderReader::ExistsSparseLayer`.
    pub fn exists_sparse_layer(&self) -> bool {
        assert!(self.is_initialized());
        self.data.sparse_info.generation != 0
    }

    /// Get a reference to the sparse info.
    /// Corresponds to upstream `NcaFsHeaderReader::GetSparseInfo`.
    pub fn get_sparse_info(&self) -> &NcaSparseInfo {
        assert!(self.is_initialized());
        &self.data.sparse_info
    }

    /// Check if a compression layer exists.
    /// Corresponds to upstream `NcaFsHeaderReader::ExistsCompressionLayer`.
    pub fn exists_compression_layer(&self) -> bool {
        assert!(self.is_initialized());
        self.data.compression_info.bucket.offset.get() != 0
            && self.data.compression_info.bucket.size.get() != 0
    }

    /// Get a reference to the compression info.
    /// Corresponds to upstream `NcaFsHeaderReader::GetCompressionInfo`.
    pub fn get_compression_info(&self) -> &NcaCompressionInfo {
        assert!(self.is_initialized());
        &self.data.compression_info
    }

    /// Check if a patch meta hash layer exists.
    /// Corresponds to upstream `NcaFsHeaderReader::ExistsPatchMetaHashLayer`.
    pub fn exists_patch_meta_hash_layer(&self) -> bool {
        assert!(self.is_initialized());
        self.data.meta_data_hash_data_info.size.get() != 0
            && self.get_patch_info().has_indirect_table()
    }

    /// Get a reference to the patch meta data hash data info.
    /// Corresponds to upstream `NcaFsHeaderReader::GetPatchMetaDataHashDataInfo`.
    pub fn get_patch_meta_data_hash_data_info(&self) -> &NcaMetaDataHashDataInfo {
        assert!(self.is_initialized());
        &self.data.meta_data_hash_data_info
    }

    /// Get the patch meta hash type.
    /// Corresponds to upstream `NcaFsHeaderReader::GetPatchMetaHashType`.
    pub fn get_patch_meta_hash_type(&self) -> u8 {
        assert!(self.is_initialized());
        self.data.meta_data_hash_type
    }

    /// Check if a sparse meta hash layer exists.
    /// Corresponds to upstream `NcaFsHeaderReader::ExistsSparseMetaHashLayer`.
    pub fn exists_sparse_meta_hash_layer(&self) -> bool {
        assert!(self.is_initialized());
        self.data.meta_data_hash_data_info.size.get() != 0 && self.exists_sparse_layer()
    }

    /// Get a reference to the sparse meta data hash data info.
    /// Corresponds to upstream `NcaFsHeaderReader::GetSparseMetaDataHashDataInfo`.
    pub fn get_sparse_meta_data_hash_data_info(&self) -> &NcaMetaDataHashDataInfo {
        assert!(self.is_initialized());
        &self.data.meta_data_hash_data_info
    }

    /// Get the sparse meta hash type.
    /// Corresponds to upstream `NcaFsHeaderReader::GetSparseMetaHashType`.
    pub fn get_sparse_meta_hash_type(&self) -> u8 {
        assert!(self.is_initialized());
        self.data.meta_data_hash_type
    }
}

impl Default for NcaFsHeaderReader {
    fn default() -> Self {
        Self::new()
    }
}

/// SetupFsHeaderReader helper.
/// Corresponds to upstream `NcaFileSystemDriver::SetupFsHeaderReader`.
pub fn setup_fs_header_reader(
    out: &mut NcaFsHeaderReader,
    reader: &NcaReader,
    fs_index: i32,
) -> Result<(), ResultCode> {
    out.initialize(reader, fs_index)
}
