// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/loader/nax.h and nax.cpp
//!
//! NAX (Nintendo Aes Xts) loader.

use crate::file_sys::vfs::vfs_types::VirtualFile;

use super::loader::{
    AppLoader, FileType, FileTypeIdentifier, KProcess, LoadResult, Modules, ResultStatus, System,
};

// ============================================================================
// AppLoaderNax
// ============================================================================

/// Loads a NAX file.
///
/// Maps to upstream `Loader::AppLoader_NAX`.
pub struct AppLoaderNax {
    file: VirtualFile,
    is_loaded: bool,
    // TODO: Replace with actual types when ported:
    // nax: Option<FileSys::NAX>,
    // nca_loader: Option<AppLoaderNca>,
}

impl FileTypeIdentifier for AppLoaderNax {
    /// Identifies whether or not the given file is a NAX file.
    ///
    /// Maps to upstream `AppLoader_NAX::IdentifyType`.
    fn identify_type(nax_file: &VirtualFile) -> FileType {
        // TODO: Full implementation requires FileSys::NAX to parse the file
        // and attempt to convert to NCA.
        // Upstream logic:
        //   const FileSys::NAX nax(nax_file);
        //   return IdentifyTypeImpl(nax);
        //
        // Where IdentifyTypeImpl checks:
        //   if (nax.GetStatus() != ResultStatus::Success) return FileType::Error;
        //   const auto nca = nax.AsNCA();
        //   if (nca == nullptr || nca->GetStatus() != ResultStatus::Success)
        //       return FileType::Error;
        //   return FileType::NAX;

        // NAX files have a specific header. Without the crypto subsystem,
        // we can check for the NAX0 magic.
        if nax_file.get_size() < 0x4 {
            return FileType::Error;
        }
        let mut magic = [0u8; 4];
        if nax_file.read(&mut magic, 4, 0) != 4 {
            return FileType::Error;
        }
        // NAX files start with "NAX0" magic
        if &magic == b"NAX0" {
            return FileType::NAX;
        }

        FileType::Error
    }
}

impl AppLoaderNax {
    /// Create a new NAX loader.
    ///
    /// Maps to upstream `AppLoader_NAX::AppLoader_NAX`.
    pub fn new(file: VirtualFile) -> Self {
        // TODO: Upstream creates FileSys::NAX from file, then creates
        // AppLoader_NCA from nax->GetDecrypted().
        Self {
            file,
            is_loaded: false,
        }
    }
}

impl AppLoader for AppLoaderNax {
    /// Maps to upstream `AppLoader_NAX::GetFileType`.
    ///
    /// Unlike most loaders which just call IdentifyType on self.file,
    /// upstream calls IdentifyTypeImpl(*nax) using the parsed NAX object.
    fn get_file_type(&self) -> FileType {
        // TODO: Should use IdentifyTypeImpl with the parsed NAX object.
        Self::identify_type(&self.file)
    }

    /// Maps to upstream `AppLoader_NAX::Load`.
    fn load(&mut self, _process: &mut KProcess, _system: &mut System) -> LoadResult {
        if self.is_loaded {
            return (ResultStatus::ErrorAlreadyLoaded, None);
        }

        // TODO: Full implementation requires:
        // - FileSys::NAX status check
        // - Conversion to NCA (nax->AsNCA())
        // - Key file existence check
        // - Delegate to nca_loader->Load()

        log::warn!("NAX loader: Load not fully implemented (requires FileSys::NAX)");
        (ResultStatus::ErrorNotImplemented, None)
    }

    fn read_rom_fs(&self, _out_file: &mut Option<VirtualFile>) -> ResultStatus {
        // TODO: Delegates to nca_loader.
        ResultStatus::ErrorNotImplemented
    }

    fn read_program_id(&self, _out_program_id: &mut u64) -> ResultStatus {
        // TODO: Delegates to nca_loader.
        ResultStatus::ErrorNotImplemented
    }

    fn read_banner(&self, _buffer: &mut Vec<u8>) -> ResultStatus {
        // TODO: Delegates to nca_loader.
        ResultStatus::ErrorNotImplemented
    }

    fn read_logo(&self, _buffer: &mut Vec<u8>) -> ResultStatus {
        // TODO: Delegates to nca_loader.
        ResultStatus::ErrorNotImplemented
    }

    fn read_nso_modules(&self, _modules: &mut Modules) -> ResultStatus {
        // TODO: Delegates to nca_loader.
        ResultStatus::ErrorNotImplemented
    }
}
