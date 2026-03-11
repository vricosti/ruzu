// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/loader/nsp.h and nsp.cpp
//!
//! NSP (Nintendo Submission Package) loader.

use crate::file_sys::vfs::vfs_types::VirtualFile;

use super::loader::{
    AppLoader, FileType, FileTypeIdentifier, KProcess, LoadResult, Modules, ResultStatus, System,
    NACP,
};

// ============================================================================
// AppLoaderNsp
// ============================================================================

/// Loads an NSP file.
///
/// Maps to upstream `Loader::AppLoader_NSP`.
pub struct AppLoaderNsp {
    file: VirtualFile,
    is_loaded: bool,
    program_id: u64,
    program_index: usize,
    // TODO: Replace with actual types when ported:
    // nsp: Option<FileSys::NSP>,
    // secondary_loader: Option<Box<dyn AppLoader>>,
    // icon_file: Option<VirtualFile>,
    // nacp_file: Option<NACP>,
}

impl FileTypeIdentifier for AppLoaderNsp {
    /// Identifies whether or not the given file is an NSP file.
    ///
    /// Maps to upstream `AppLoader_NSP::IdentifyType`.
    fn identify_type(nsp_file: &VirtualFile) -> FileType {
        // TODO: Full implementation requires FileSys::NSP to parse the file.
        // Upstream logic:
        //   const FileSys::NSP nsp(nsp_file);
        //   if (nsp.GetStatus() == ResultStatus::Success) {
        //     // Extracted Type case
        //     if (nsp.IsExtractedType() && nsp.GetExeFS() != nullptr &&
        //         FileSys::IsDirectoryExeFS(nsp.GetExeFS()))
        //       return FileType::NSP;
        //     // Non-Extracted Type case
        //     const auto program_id = nsp.GetProgramTitleID();
        //     if (!nsp.IsExtractedType() &&
        //         nsp.GetNCA(program_id, ContentRecordType::Program) != nullptr &&
        //         AppLoader_NCA::IdentifyType(...) == FileType::NCA)
        //       return FileType::NSP;
        //   }
        //   return FileType::Error;

        // Basic heuristic: PFS0 magic at start of file
        if nsp_file.get_size() < 4 {
            return FileType::Error;
        }
        let mut magic = [0u8; 4];
        if nsp_file.read(&mut magic, 4, 0) != 4 {
            return FileType::Error;
        }
        // PFS0 magic: "PFS0"
        if &magic == b"PFS0" {
            return FileType::NSP;
        }
        // HFS0 magic: "HFS0"
        if &magic == b"HFS0" {
            return FileType::NSP;
        }

        FileType::Error
    }
}

impl AppLoaderNsp {
    /// Create a new NSP loader.
    ///
    /// Maps to upstream `AppLoader_NSP::AppLoader_NSP`.
    ///
    /// TODO: Full construction requires FileSystemController and ContentProvider
    /// which are not yet ported.
    pub fn new(file: VirtualFile, program_id: u64, program_index: usize) -> Self {
        // TODO: Create FileSys::NSP and secondary_loader.
        Self {
            file,
            is_loaded: false,
            program_id,
            program_index,
        }
    }
}

impl AppLoader for AppLoaderNsp {
    fn get_file_type(&self) -> FileType {
        Self::identify_type(&self.file)
    }

    /// Maps to upstream `AppLoader_NSP::Load`.
    fn load(&mut self, _process: &mut KProcess, _system: &mut System) -> LoadResult {
        if self.is_loaded {
            return (ResultStatus::ErrorAlreadyLoaded, None);
        }

        // TODO: Full implementation requires:
        // - FileSys::NSP to parse NSP content
        // - Check NSP status and program status
        // - Delegate to secondary_loader (NCA or DeconstructedRomDirectory)
        // - Register with FileSystemController
        // - Handle packed updates

        log::warn!("NSP loader: Load not fully implemented (requires FileSys::NSP)");
        (ResultStatus::ErrorNotImplemented, None)
    }

    /// Maps to upstream `AppLoader_NSP::VerifyIntegrity`.
    fn verify_integrity(
        &self,
        _progress_callback: &dyn Fn(usize, usize) -> bool,
    ) -> ResultStatus {
        // TODO: Requires FileSys::NSP to enumerate NCAs and verify each.
        // Extracted-type NSPs can't be verified.
        ResultStatus::ErrorIntegrityVerificationNotImplemented
    }

    fn read_rom_fs(&self, _out_file: &mut Option<VirtualFile>) -> ResultStatus {
        // TODO: Delegates to secondary_loader.
        ResultStatus::ErrorNotImplemented
    }

    fn read_update_raw(&self, _out_file: &mut Option<VirtualFile>) -> ResultStatus {
        // TODO: Requires FileSys::NSP to be ported.
        ResultStatus::ErrorNoPackedUpdate
    }

    fn read_program_id(&self, out_program_id: &mut u64) -> ResultStatus {
        // TODO: Requires FileSys::NSP for proper title ID.
        if self.program_id == 0 {
            return ResultStatus::ErrorNotInitialized;
        }
        *out_program_id = self.program_id;
        ResultStatus::Success
    }

    fn read_program_ids(&self, _out_program_ids: &mut Vec<u64>) -> ResultStatus {
        // TODO: Requires FileSys::NSP to be ported.
        ResultStatus::ErrorNotImplemented
    }

    fn read_icon(&self, _buffer: &mut Vec<u8>) -> ResultStatus {
        // TODO: Requires icon_file from NSP construction.
        ResultStatus::ErrorNoControl
    }

    fn read_title(&self, _title: &mut String) -> ResultStatus {
        // TODO: Requires nacp_file from NSP construction.
        ResultStatus::ErrorNoControl
    }

    fn read_control_data(&self, _control: &mut NACP) -> ResultStatus {
        // TODO: Requires nacp_file from NSP construction.
        ResultStatus::ErrorNoControl
    }

    fn read_manual_rom_fs(&self, _out_file: &mut Option<VirtualFile>) -> ResultStatus {
        // TODO: Requires FileSys::NSP to be ported.
        ResultStatus::ErrorNoRomFS
    }

    fn read_banner(&self, _buffer: &mut Vec<u8>) -> ResultStatus {
        // TODO: Delegates to secondary_loader.
        ResultStatus::ErrorNotImplemented
    }

    fn read_logo(&self, _buffer: &mut Vec<u8>) -> ResultStatus {
        // TODO: Delegates to secondary_loader.
        ResultStatus::ErrorNotImplemented
    }

    fn read_nso_modules(&self, _modules: &mut Modules) -> ResultStatus {
        // TODO: Delegates to secondary_loader.
        ResultStatus::ErrorNotImplemented
    }
}
