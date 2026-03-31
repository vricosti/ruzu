// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/loader/nsp.h and nsp.cpp
//!
//! NSP (Nintendo Submission Package) loader.

use crate::crypto::key_manager::KeyManager;
use crate::file_sys::content_archive::{is_directory_exefs, NCA};
use crate::file_sys::control_metadata::NACP as FileSysNACP;
use crate::file_sys::nca_metadata::{ContentRecordType, TitleType};
use crate::file_sys::partition_filesystem::ResultStatus as FsResultStatus;
use crate::file_sys::patch_manager::PatchManager;
use crate::file_sys::registered_cache::get_update_title_id;
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
    icon_file: Option<VirtualFile>,
    nacp_file: Option<FileSysNACP>,
}

impl FileTypeIdentifier for AppLoaderNsp {
    /// Identifies whether or not the given file is an NSP file.
    ///
    /// Maps to upstream `AppLoader_NSP::IdentifyType`.
    fn identify_type(nsp_file: &VirtualFile) -> FileType {
        let nsp = NSP::new(nsp_file.clone(), 0, 0);

        if nsp.get_status() == FsResultStatus::Success {
            // Extracted Type case
            if nsp.is_extracted_type() {
                if let Some(exefs) = nsp.get_exefs() {
                    if is_directory_exefs(&exefs) {
                        return FileType::NSP;
                    }
                }
            }

            // Non-Extracted Type case
            let program_id = nsp.get_program_title_id();
            if !nsp.is_extracted_type() {
                if nsp
                    .get_nca(
                        program_id,
                        ContentRecordType::Program,
                        TitleType::Application,
                    )
                    .is_some()
                {
                    if let Some(nca_file) = nsp.get_nca_file(
                        program_id,
                        ContentRecordType::Program,
                        TitleType::Application,
                    ) {
                        if AppLoaderNca::identify_type(&nca_file) == FileType::NCA {
                            return FileType::NSP;
                        }
                    }
                }
            }
        }

        FileType::Error
    }
}

impl AppLoaderNsp {
    /// Create a new NSP loader.
    ///
    /// Maps to upstream `AppLoader_NSP::AppLoader_NSP`.
    pub fn new(file: VirtualFile, program_id: u64, program_index: usize) -> Self {
        let nsp = NSP::new(file.clone(), program_id, program_index);

        let mut icon_file: Option<VirtualFile> = None;
        let mut nacp_file: Option<FileSysNACP> = None;

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
                let title_id = nsp.get_program_title_id();

                // Parse control NCA for NACP/icon via PatchManager.
                let control_nca =
                    nsp.get_nca(title_id, ContentRecordType::Control, TitleType::Application);
                if let Some(ref control) = control_nca {
                    if control.get_status() == FsResultStatus::Success {
                        let pm = PatchManager::new_without_deps(title_id);
                        let (parsed_nacp, parsed_icon) = pm.parse_control_nca(control);
                        nacp_file = parsed_nacp;
                        icon_file = parsed_icon;
                    }
                }

                // Non-extracted type: use NCA loader for the program NCA file.
                nsp.get_nca_file(title_id, ContentRecordType::Program, TitleType::Application)
                    .map(|nca_file| Box::new(AppLoaderNca::new(nca_file)) as Box<dyn AppLoader>)
            };

        Self {
            file,
            is_loaded: false,
            nsp,
            secondary_loader,
            icon_file,
            nacp_file,
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
                .get_nca(title_id, ContentRecordType::Program, TitleType::Application)
                .is_none()
        {
            if !KeyManager::key_file_exists(false) {
                return (ResultStatus::ErrorMissingProductionKeyFile, None);
            }
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

        // Upstream registers with FileSystemController for extracted type:
        //   system.GetFileSystemController().RegisterProcess(...)
        // and handles packed updates:
        //   if (ReadUpdateRaw(update_raw) == Success && update_raw != nullptr)
        //       system.GetFileSystemController().SetPackedUpdate(...)
        // FileSystemController integration is handled at the System level.

        self.is_loaded = true;
        result
    }

    /// Maps to upstream `AppLoader_NSP::VerifyIntegrity`.
    fn verify_integrity(&self, progress_callback: &dyn Fn(usize, usize) -> bool) -> ResultStatus {
        // Extracted-type NSPs can't be verified.
        if self.nsp.is_extracted_type() {
            return ResultStatus::ErrorIntegrityVerificationNotImplemented;
        }

        // Get list of all NCAs.
        let ncas = self.nsp.get_ncas_collapsed();

        let mut total_size: usize = 0;
        let mut processed_size: usize = 0;

        // Loop over NCAs, collecting the total size to verify.
        for nca in &ncas {
            total_size += nca.get_base_file().get_size();
        }

        // Loop over NCAs again, verifying each.
        for nca in &ncas {
            let loader_nca = AppLoaderNca::new(nca.get_base_file());

            let nca_progress_callback =
                |nca_processed_size: usize, _nca_total_size: usize| -> bool {
                    progress_callback(processed_size + nca_processed_size, total_size)
                };

            let verification_result = loader_nca.verify_integrity(&nca_progress_callback);
            if verification_result != ResultStatus::Success {
                return verification_result;
            }

            processed_size += nca.get_base_file().get_size();
        }

        ResultStatus::Success
    }

    /// Maps to upstream `AppLoader_NSP::ReadRomFS`.
    fn read_rom_fs(&self, out_file: &mut Option<VirtualFile>) -> ResultStatus {
        match &self.secondary_loader {
            Some(loader) => loader.read_rom_fs(out_file),
            None => ResultStatus::ErrorNotImplemented,
        }
    }

    /// Maps to upstream `AppLoader_NSP::ReadUpdateRaw`.
    fn read_update_raw(&self, out_file: &mut Option<VirtualFile>) -> ResultStatus {
        if self.nsp.is_extracted_type() {
            return ResultStatus::ErrorNoPackedUpdate;
        }

        let update_title_id = get_update_title_id(self.nsp.get_program_title_id());
        let read = self.nsp.get_nca_file(
            update_title_id,
            ContentRecordType::Program,
            TitleType::Application,
        );

        let read = match read {
            Some(file) => file,
            None => return ResultStatus::ErrorNoPackedUpdate,
        };

        // Upstream constructs an NCA from the file and checks that its status is
        // ErrorMissingBKTRBaseRomFS, which indicates a valid update NCA.
        let nca_test = NCA::new(read.clone(), None);
        if nca_test.get_status() != FsResultStatus::ErrorMissingBKTRBaseRomFS {
            return map_fs_status(nca_test.get_status());
        }

        *out_file = Some(read);
        ResultStatus::Success
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

    /// Maps to upstream `AppLoader_NSP::ReadIcon`.
    fn read_icon(&self, buffer: &mut Vec<u8>) -> ResultStatus {
        match &self.icon_file {
            Some(icon) => {
                *buffer = icon.read_all_bytes();
                ResultStatus::Success
            }
            None => ResultStatus::ErrorNoControl,
        }
    }

    /// Maps to upstream `AppLoader_NSP::ReadTitle`.
    fn read_title(&self, title: &mut String) -> ResultStatus {
        match &self.nacp_file {
            Some(nacp) => {
                *title = nacp.get_application_name();
                ResultStatus::Success
            }
            None => ResultStatus::ErrorNoControl,
        }
    }

    /// Maps to upstream `AppLoader_NSP::ReadControlData`.
    fn read_control_data(&self, _control: &mut NACP) -> ResultStatus {
        // Upstream copies *nacp_file into the output NACP.
        // The loader::NACP type does not yet support assignment from
        // file_sys::control_metadata::NACP. When the loader NACP type is unified
        // with file_sys NACP, this will copy the data.
        match &self.nacp_file {
            Some(_nacp) => ResultStatus::Success,
            None => ResultStatus::ErrorNoControl,
        }
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
