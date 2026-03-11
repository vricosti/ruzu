// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/loader/xci.h and xci.cpp
//!
//! XCI (NX Card Image) loader.

use crate::file_sys::vfs::vfs_types::VirtualFile;

use super::loader::{
    AppLoader, FileType, FileTypeIdentifier, KProcess, LoadResult, Modules, ResultStatus, System,
    NACP,
};

// ============================================================================
// AppLoaderXci
// ============================================================================

/// Loads an XCI file.
///
/// Maps to upstream `Loader::AppLoader_XCI`.
pub struct AppLoaderXci {
    file: VirtualFile,
    is_loaded: bool,
    program_id: u64,
    program_index: usize,
    // TODO: Replace with actual types when ported:
    // xci: Option<FileSys::XCI>,
    // nca_loader: Option<AppLoaderNca>,
    // icon_file: Option<VirtualFile>,
    // nacp_file: Option<NACP>,
}

impl FileTypeIdentifier for AppLoaderXci {
    /// Identifies whether or not the given file is an XCI file.
    ///
    /// Maps to upstream `AppLoader_XCI::IdentifyType`.
    fn identify_type(xci_file: &VirtualFile) -> FileType {
        // TODO: Full implementation requires FileSys::XCI to parse the file.
        // Upstream logic:
        //   const FileSys::XCI xci(xci_file);
        //   if (xci.GetStatus() == ResultStatus::Success &&
        //       xci.GetNCAByType(NCAContentType::Program) != nullptr &&
        //       AppLoader_NCA::IdentifyType(xci.GetNCAFileByType(NCAContentType::Program)) == NCA)
        //     return FileType::XCI;
        //   return FileType::Error;

        // Basic heuristic: XCI files start with "HEAD" magic at offset 0x100
        if xci_file.get_size() < 0x104 {
            return FileType::Error;
        }
        let mut magic = [0u8; 4];
        if xci_file.read(&mut magic, 4, 0x100) != 4 {
            return FileType::Error;
        }
        if &magic == b"HEAD" {
            return FileType::XCI;
        }

        FileType::Error
    }
}

impl AppLoaderXci {
    /// Create a new XCI loader.
    ///
    /// Maps to upstream `AppLoader_XCI::AppLoader_XCI`.
    ///
    /// TODO: Full construction requires FileSystemController and ContentProvider
    /// which are not yet ported.
    pub fn new(file: VirtualFile, program_id: u64, program_index: usize) -> Self {
        // TODO: Create FileSys::XCI and nca_loader, parse control NCA.
        Self {
            file,
            is_loaded: false,
            program_id,
            program_index,
        }
    }
}

impl AppLoader for AppLoaderXci {
    fn get_file_type(&self) -> FileType {
        Self::identify_type(&self.file)
    }

    /// Maps to upstream `AppLoader_XCI::Load`.
    fn load(&mut self, _process: &mut KProcess, _system: &mut System) -> LoadResult {
        if self.is_loaded {
            return (ResultStatus::ErrorAlreadyLoaded, None);
        }

        // TODO: Full implementation requires:
        // - FileSys::XCI to parse XCI content
        // - Check XCI status and program NCA status
        // - Check for production key file
        // - Delegate to nca_loader
        // - Handle packed updates via FileSystemController

        log::warn!("XCI loader: Load not fully implemented (requires FileSys::XCI)");
        (ResultStatus::ErrorNotImplemented, None)
    }

    /// Maps to upstream `AppLoader_XCI::VerifyIntegrity`.
    fn verify_integrity(
        &self,
        _progress_callback: &dyn Fn(usize, usize) -> bool,
    ) -> ResultStatus {
        // TODO: Requires FileSys::XCI to get secure partition NSP and
        // enumerate NCAs for verification.
        ResultStatus::ErrorIntegrityVerificationNotImplemented
    }

    fn read_rom_fs(&self, _out_file: &mut Option<VirtualFile>) -> ResultStatus {
        // TODO: Delegates to nca_loader.
        ResultStatus::ErrorNotImplemented
    }

    fn read_update_raw(&self, _out_file: &mut Option<VirtualFile>) -> ResultStatus {
        // TODO: Requires FileSys::XCI to be ported.
        ResultStatus::ErrorNoPackedUpdate
    }

    fn read_program_id(&self, _out_program_id: &mut u64) -> ResultStatus {
        // TODO: Delegates to nca_loader.
        ResultStatus::ErrorNotImplemented
    }

    fn read_program_ids(&self, _out_program_ids: &mut Vec<u64>) -> ResultStatus {
        // TODO: Requires FileSys::XCI to be ported.
        ResultStatus::ErrorNotImplemented
    }

    fn read_icon(&self, _buffer: &mut Vec<u8>) -> ResultStatus {
        // TODO: Requires icon_file from XCI construction.
        ResultStatus::ErrorNoControl
    }

    fn read_title(&self, _title: &mut String) -> ResultStatus {
        // TODO: Requires nacp_file from XCI construction.
        ResultStatus::ErrorNoControl
    }

    fn read_control_data(&self, _control: &mut NACP) -> ResultStatus {
        // TODO: Requires nacp_file from XCI construction.
        ResultStatus::ErrorNoControl
    }

    fn read_manual_rom_fs(&self, _out_file: &mut Option<VirtualFile>) -> ResultStatus {
        // TODO: Requires FileSys::XCI to be ported.
        ResultStatus::ErrorXCIMissingPartition
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
