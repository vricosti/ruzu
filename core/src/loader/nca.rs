// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/loader/nca.h and nca.cpp
//!
//! NCA (Nintendo Content Archive) loader.

use crate::file_sys::vfs::vfs::VfsFile;
use crate::file_sys::vfs::vfs_types::VirtualFile;

use super::loader::{
    AppLoader, FileType, FileTypeIdentifier, KProcess, LoadResult, Modules, ResultStatus, System,
    NACP,
};
use super::nso::read_object;

// ============================================================================
// Constants for VerifyIntegrity
// ============================================================================

/// Maps to upstream `NcaFileNameWithHashLength` in nca.cpp.
const NCA_FILE_NAME_WITH_HASH_LENGTH: usize = 36;

/// Maps to upstream `NcaFileNameHashLength` in nca.cpp.
const NCA_FILE_NAME_HASH_LENGTH: usize = 32;

/// Maps to upstream `NcaSha256HashLength` in nca.cpp.
const NCA_SHA256_HASH_LENGTH: usize = 32;

/// Maps to upstream `NcaSha256HalfHashLength` in nca.cpp.
const NCA_SHA256_HALF_HASH_LENGTH: usize = NCA_SHA256_HASH_LENGTH / 2;

// ============================================================================
// AppLoaderNca
// ============================================================================

/// Loads an NCA file.
///
/// Maps to upstream `Loader::AppLoader_NCA`.
pub struct AppLoaderNca {
    file: VirtualFile,
    is_loaded: bool,
    // TODO: Replace with actual FileSys::NCA when ported.
    // nca: Option<FileSys::NCA>,
    // directory_loader: Option<AppLoaderDeconstructedRomDirectory>,
}

impl FileTypeIdentifier for AppLoaderNca {
    /// Identifies whether or not the given file is an NCA file.
    ///
    /// Maps to upstream `AppLoader_NCA::IdentifyType`.
    fn identify_type(nca_file: &VirtualFile) -> FileType {
        // TODO: Full implementation requires FileSys::NCA to parse the file
        // and check NCA status and content type.
        // For now, do a basic magic check on the NCA header.
        // NCA files have a complex encrypted header; proper identification
        // requires crypto key support (FileSys::NCA constructor).
        //
        // Upstream logic:
        //   const FileSys::NCA nca(nca_file);
        //   if (nca.GetStatus() == ResultStatus::Success &&
        //       nca.GetType() == FileSys::NCAContentType::Program)
        //       return FileType::NCA;
        //   return FileType::Error;

        // Placeholder: check file size is reasonable for an NCA
        if nca_file.get_size() < 0x400 {
            return FileType::Error;
        }

        // NCA identification requires crypto infrastructure not yet ported.
        // Return Error for now; the GetLoader fallback will use filename-based detection.
        FileType::Error
    }
}

impl AppLoaderNca {
    /// Create a new NCA loader.
    ///
    /// Maps to upstream `AppLoader_NCA::AppLoader_NCA`.
    pub fn new(file: VirtualFile) -> Self {
        // TODO: Create FileSys::NCA from file when that type is ported.
        Self {
            file,
            is_loaded: false,
        }
    }
}

impl AppLoader for AppLoaderNca {
    fn get_file_type(&self) -> FileType {
        Self::identify_type(&self.file)
    }

    /// Maps to upstream `AppLoader_NCA::Load`.
    fn load(&mut self, _process: &mut KProcess, _system: &mut System) -> LoadResult {
        if self.is_loaded {
            return (ResultStatus::ErrorAlreadyLoaded, None);
        }

        // TODO: Full implementation requires:
        // - FileSys::NCA to parse NCA content
        // - Check NCA status and type
        // - Extract ExeFS
        // - Fall back to update NCA if base has no ExeFS
        // - Create AppLoader_DeconstructedRomDirectory from ExeFS
        // - Load via directory_loader
        // - Register with FileSystemController

        log::warn!("NCA loader: Load not fully implemented (requires FileSys::NCA)");
        (ResultStatus::ErrorNotImplemented, None)
    }

    /// Maps to upstream `AppLoader_NCA::VerifyIntegrity`.
    fn verify_integrity(
        &self,
        progress_callback: &dyn Fn(usize, usize) -> bool,
    ) -> ResultStatus {
        let name = self.file.get_name();

        // We won't try to verify meta NCAs.
        if name.ends_with(".cnmt.nca") {
            return ResultStatus::Success;
        }

        // Check if we can verify this file. NCAs should be named after their hashes.
        if !name.ends_with(".nca") || name.len() != NCA_FILE_NAME_WITH_HASH_LENGTH {
            log::warn!("Unable to validate NCA with name {}", name);
            return ResultStatus::ErrorIntegrityVerificationNotImplemented;
        }

        // Get the expected truncated hash of the NCA.
        let hash_hex = &name[..NCA_FILE_NAME_HASH_LENGTH];
        let input_hash = match hex_string_to_bytes(hash_hex) {
            Some(h) => h,
            None => {
                log::warn!("Unable to parse hash from NCA filename {}", name);
                return ResultStatus::ErrorIntegrityVerificationNotImplemented;
            }
        };

        // Compute SHA-256 of the file.
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();

        let total_size = self.file.get_size();
        let mut processed_size = 0usize;
        let buffer_size = 4 * 1024 * 1024; // 4 MiB

        while processed_size < total_size {
            let intended_read_size = std::cmp::min(buffer_size, total_size - processed_size);
            let data = self.file.read_bytes(intended_read_size, processed_size);
            let read_size = data.len();

            hasher.update(&data);
            processed_size += read_size;

            if !progress_callback(processed_size, total_size) {
                return ResultStatus::ErrorIntegrityVerificationFailed;
            }
        }

        let output_hash = hasher.finalize();

        // Compare first half of the hash.
        if input_hash.len() < NCA_SHA256_HALF_HASH_LENGTH
            || output_hash[..NCA_SHA256_HALF_HASH_LENGTH]
                != input_hash[..NCA_SHA256_HALF_HASH_LENGTH]
        {
            log::error!("NCA hash mismatch detected for file {}", name);
            return ResultStatus::ErrorIntegrityVerificationFailed;
        }

        ResultStatus::Success
    }

    fn read_rom_fs(
        &self,
        _out_file: &mut Option<VirtualFile>,
    ) -> ResultStatus {
        // TODO: Requires FileSys::NCA to be ported.
        ResultStatus::ErrorNotInitialized
    }

    fn read_program_id(&self, _out_program_id: &mut u64) -> ResultStatus {
        // TODO: Requires FileSys::NCA to be ported.
        ResultStatus::ErrorNotInitialized
    }

    fn read_banner(&self, _buffer: &mut Vec<u8>) -> ResultStatus {
        // TODO: Requires FileSys::NCA to be ported.
        ResultStatus::ErrorNotInitialized
    }

    fn read_logo(&self, _buffer: &mut Vec<u8>) -> ResultStatus {
        // TODO: Requires FileSys::NCA to be ported.
        ResultStatus::ErrorNotInitialized
    }

    fn read_nso_modules(&self, _modules: &mut Modules) -> ResultStatus {
        // TODO: Requires directory_loader to be initialized.
        ResultStatus::ErrorNotInitialized
    }
}

/// Convert a hex string to bytes.
///
/// Maps to upstream `Common::HexStringToVector`.
fn hex_string_to_bytes(hex: &str) -> Option<Vec<u8>> {
    if hex.len() % 2 != 0 {
        return None;
    }
    let mut bytes = Vec::with_capacity(hex.len() / 2);
    for i in (0..hex.len()).step_by(2) {
        let byte = u8::from_str_radix(&hex[i..i + 2], 16).ok()?;
        bytes.push(byte);
    }
    Some(bytes)
}
