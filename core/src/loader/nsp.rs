// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/loader/nsp.h and nsp.cpp
//!
//! NSP (Nintendo Submission Package) loader.

use crate::file_sys::nca_metadata::{ContentRecordType, TitleType};
use crate::file_sys::partition_filesystem::ResultStatus as FsResultStatus;
use crate::file_sys::submission_package::NSP;
use crate::file_sys::vfs::vfs_types::VirtualFile;

use super::deconstructed_rom_directory::AppLoaderDeconstructedRomDirectory;
use super::loader::{
    AppLoader, FileType, FileTypeIdentifier, KProcess, LoadResult, Modules, ResultStatus, System,
    NACP,
};
use super::nca::AppLoaderNca;

/// Map file_sys ResultStatus to loader ResultStatus.
fn map_fs_status(fs: FsResultStatus) -> ResultStatus {
    match fs {
        FsResultStatus::Success => ResultStatus::Success,
        FsResultStatus::ErrorBadNCAHeader => ResultStatus::ErrorBadNCAHeader,
        FsResultStatus::ErrorMissingKeyAreaKey => ResultStatus::ErrorMissingKeyAreaKey,
        FsResultStatus::ErrorMissingTitlekey => ResultStatus::ErrorMissingTitlekey,
        FsResultStatus::ErrorMissingTitlekek => ResultStatus::ErrorMissingTitlekek,
        FsResultStatus::ErrorNullFile => ResultStatus::ErrorNullFile,
        FsResultStatus::ErrorNSPMissingProgramNCA => ResultStatus::ErrorNSPMissingProgramNCA,
        _ => ResultStatus::ErrorNotImplemented,
    }
}

// ============================================================================
// AppLoaderNsp
// ============================================================================

/// Loads an NSP file.
///
/// Maps to upstream `Loader::AppLoader_NSP`.
pub struct AppLoaderNsp {
    file: VirtualFile,
    is_loaded: bool,
    nsp: NSP,
    secondary_loader: Option<Box<dyn AppLoader>>,
    // TODO: Replace with actual types when control NCA parsing is ported:
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
    /// Upstream also takes FileSystemController and ContentProvider for control
    /// NCA parsing (NACP/icon). Those are not yet wired up here.
    pub fn new(file: VirtualFile, program_id: u64, program_index: usize) -> Self {
        let nsp = NSP::new(file.clone(), program_id, program_index);

        let secondary_loader: Option<Box<dyn AppLoader>> =
            if nsp.get_status() != FsResultStatus::Success {
                None
            } else if nsp.is_extracted_type() {
                // Extracted type: use DeconstructedRomDirectory from ExeFS.
                nsp.get_exefs().map(|exefs| {
                    let is_hbl = file.get_name() == "hbl.nsp";
                    Box::new(AppLoaderDeconstructedRomDirectory::new_from_directory(
                        exefs, false, is_hbl,
                    )) as Box<dyn AppLoader>
                })
            } else {
                // Non-extracted type: use NCA loader for the program NCA file.
                let title_id = nsp.get_program_title_id();
                nsp.get_nca_file(title_id, ContentRecordType::Program, TitleType::Application)
                    .map(|nca_file| Box::new(AppLoaderNca::new(nca_file)) as Box<dyn AppLoader>)
            };

        // TODO: Parse control NCA for NACP/icon when PatchManager is ported.
        // Upstream:
        //   const auto control_nca = nsp->GetNCA(title_id, ContentRecordType::Control);
        //   const FileSys::PatchManager pm{title_id, fsc, content_provider};
        //   std::tie(nacp_file, icon_file) = pm.ParseControlNCA(*control_nca);

        Self {
            file,
            is_loaded: false,
            nsp,
            secondary_loader,
        }
    }
}

impl AppLoader for AppLoaderNsp {
    fn get_file_type(&self) -> FileType {
        Self::identify_type(&self.file)
    }

    /// Maps to upstream `AppLoader_NSP::Load`.
    fn load(&mut self, process: &mut KProcess, system: &mut System) -> LoadResult {
        if self.is_loaded {
            return (ResultStatus::ErrorAlreadyLoaded, None);
        }

        let title_id = self.nsp.get_program_title_id();

        if !self.nsp.is_extracted_type() && title_id == 0 {
            return (ResultStatus::ErrorNSPMissingProgramNCA, None);
        }

        let nsp_status = self.nsp.get_status();
        if nsp_status != FsResultStatus::Success {
            return (map_fs_status(nsp_status), None);
        }

        let nsp_program_status = self.nsp.get_program_status();
        if nsp_program_status != FsResultStatus::Success {
            return (map_fs_status(nsp_program_status), None);
        }

        if !self.nsp.is_extracted_type()
            && self
                .nsp
                .get_nca(
                    title_id,
                    ContentRecordType::Program,
                    TitleType::Application,
                )
                .is_none()
        {
            // TODO: Check Core::Crypto::KeyManager::KeyFileExists(false)
            // and return ErrorMissingProductionKeyFile if keys are missing.
            return (ResultStatus::ErrorNSPMissingProgramNCA, None);
        }

        let secondary = match self.secondary_loader.as_mut() {
            Some(loader) => loader,
            None => return (ResultStatus::ErrorNSPMissingProgramNCA, None),
        };

        let result = secondary.load(process, system);
        if result.0 != ResultStatus::Success {
            return result;
        }

        // TODO: Register with FileSystemController for extracted type:
        // if self.nsp.is_extracted_type() {
        //     system.get_filesystem_controller().register_process(...);
        // }

        // TODO: Handle packed updates:
        // if let Some(update_raw) = ... ReadUpdateRaw ... {
        //     system.get_filesystem_controller().set_packed_update(...);
        // }

        self.is_loaded = true;
        result
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

    /// Maps to upstream `AppLoader_NSP::ReadRomFS`.
    fn read_rom_fs(&self, out_file: &mut Option<VirtualFile>) -> ResultStatus {
        match &self.secondary_loader {
            Some(loader) => loader.read_rom_fs(out_file),
            None => ResultStatus::ErrorNotImplemented,
        }
    }

    /// Maps to upstream `AppLoader_NSP::ReadUpdateRaw`.
    fn read_update_raw(&self, _out_file: &mut Option<VirtualFile>) -> ResultStatus {
        if self.nsp.is_extracted_type() {
            return ResultStatus::ErrorNoPackedUpdate;
        }

        // TODO: Full implementation requires get_update_title_id() helper
        // and NCA validation for the update NCA.
        // Upstream:
        //   const auto read = nsp->GetNCAFile(GetUpdateTitleID(nsp->GetProgramTitleID()),
        //                                     ContentRecordType::Program);
        //   const auto nca_test = make_shared<NCA>(read);
        //   if (nca_test->GetStatus() != ErrorMissingBKTRBaseRomFS) return nca_test->GetStatus();
        //   out_file = read;
        ResultStatus::ErrorNoPackedUpdate
    }

    /// Maps to upstream `AppLoader_NSP::ReadProgramId`.
    fn read_program_id(&self, out_program_id: &mut u64) -> ResultStatus {
        *out_program_id = self.nsp.get_program_title_id();
        if *out_program_id == 0 {
            return ResultStatus::ErrorNotInitialized;
        }
        ResultStatus::Success
    }

    /// Maps to upstream `AppLoader_NSP::ReadProgramIds`.
    fn read_program_ids(&self, out_program_ids: &mut Vec<u64>) -> ResultStatus {
        *out_program_ids = self.nsp.get_program_title_ids();
        ResultStatus::Success
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

    /// Maps to upstream `AppLoader_NSP::ReadManualRomFS`.
    fn read_manual_rom_fs(&self, out_file: &mut Option<VirtualFile>) -> ResultStatus {
        let title_id = self.nsp.get_program_title_id();
        let nca = self.nsp.get_nca(
            title_id,
            ContentRecordType::HtmlDocument,
            TitleType::Application,
        );
        if self.nsp.get_status() != FsResultStatus::Success || nca.is_none() {
            return ResultStatus::ErrorNoRomFS;
        }
        let nca = nca.unwrap();
        match nca.get_romfs() {
            Some(romfs) => {
                *out_file = Some(romfs);
                ResultStatus::Success
            }
            None => ResultStatus::ErrorNoRomFS,
        }
    }

    /// Maps to upstream `AppLoader_NSP::ReadBanner`.
    fn read_banner(&self, buffer: &mut Vec<u8>) -> ResultStatus {
        match &self.secondary_loader {
            Some(loader) => loader.read_banner(buffer),
            None => ResultStatus::ErrorNotImplemented,
        }
    }

    /// Maps to upstream `AppLoader_NSP::ReadLogo`.
    fn read_logo(&self, buffer: &mut Vec<u8>) -> ResultStatus {
        match &self.secondary_loader {
            Some(loader) => loader.read_logo(buffer),
            None => ResultStatus::ErrorNotImplemented,
        }
    }

    /// Maps to upstream `AppLoader_NSP::ReadNSOModules`.
    fn read_nso_modules(&self, modules: &mut Modules) -> ResultStatus {
        match &self.secondary_loader {
            Some(loader) => loader.read_nso_modules(modules),
            None => ResultStatus::ErrorNotImplemented,
        }
    }
}
