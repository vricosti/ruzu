// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/loader/xci.h and xci.cpp
//!
//! XCI (NX Card Image) loader.

use crate::crypto::key_manager::KeyManager;
use crate::file_sys::card_image::XCI;
use crate::file_sys::content_archive::{NCAContentType, NCA};
use crate::file_sys::nca_metadata::{ContentRecordType, TitleType};
use crate::file_sys::partition_filesystem::ResultStatus as FsResultStatus;
use crate::file_sys::registered_cache::get_update_title_id;
use crate::file_sys::vfs::vfs_types::VirtualFile;

use super::loader::{
    AppLoader, FileType, FileTypeIdentifier, KProcess, LoadResult, Modules, ResultStatus, System,
    NACP,
};
use super::nca::AppLoaderNca;

// ============================================================================
// AppLoaderXci
// ============================================================================

/// Loads an XCI file.
///
/// Maps to upstream `Loader::AppLoader_XCI`.
pub struct AppLoaderXci {
    file: VirtualFile,
    is_loaded: bool,
    xci: XCI,
    nca_loader: AppLoaderNca,
    // Upstream stores icon_file and nacp_file from control NCA via PatchManager.
    // PatchManager::ParseControlNCA is not yet fully wired up with
    // FileSystemController and ContentProvider references.
    icon_file: Option<VirtualFile>,
    nacp_file: Option<crate::file_sys::control_metadata::NACP>,
}

impl FileTypeIdentifier for AppLoaderXci {
    /// Identifies whether or not the given file is an XCI file.
    ///
    /// Maps to upstream `AppLoader_XCI::IdentifyType`.
    ///
    /// Upstream constructs `FileSys::XCI(xci_file)`, checks status == Success,
    /// verifies a Program NCA exists, and that AppLoader_NCA::IdentifyType
    /// returns FileType::NCA for the program NCA file.
    fn identify_type(xci_file: &VirtualFile) -> FileType {
        let xci = XCI::new(xci_file.clone(), 0, 0);

        if xci.get_status() == FsResultStatus::Success
            && xci.get_nca_by_type(NCAContentType::Program).is_some()
        {
            if let Some(program_nca_file) = xci.get_nca_file_by_type(NCAContentType::Program) {
                if AppLoaderNca::identify_type(&program_nca_file) == FileType::NCA {
                    return FileType::XCI;
                }
            }
        }

        FileType::Error
    }
}

impl AppLoaderXci {
    /// Create a new XCI loader.
    ///
    /// Maps to upstream `AppLoader_XCI::AppLoader_XCI`.
    ///
    /// Upstream constructs FileSys::XCI, extracts the program NCA to create
    /// AppLoader_NCA, and parses the control NCA via PatchManager for NACP/icon.
    /// PatchManager::ParseControlNCA requires FileSystemController and
    /// ContentProvider which are not yet fully wired into the loader path.
    pub fn new(file: VirtualFile, program_id: u64, program_index: usize) -> Self {
        let xci = XCI::new(file.clone(), program_id, program_index);

        // Upstream: nca_loader = make_unique<AppLoader_NCA>(xci->GetProgramNCAFile())
        // If the XCI has no program NCA file, we create the NCA loader with
        // the raw file as a fallback (it will fail gracefully on load).
        let program_nca_file = xci
            .get_program_nca_file()
            .unwrap_or_else(|| file.clone());
        let nca_loader = AppLoaderNca::new(program_nca_file);

        let icon_file: Option<VirtualFile> = None;
        let nacp_file: Option<crate::file_sys::control_metadata::NACP> = None;

        if xci.get_status() == FsResultStatus::Success {
            // Upstream: const auto control_nca = xci->GetNCAByType(FileSys::NCAContentType::Control);
            let control_nca = xci.get_nca_by_type(NCAContentType::Control);
            if let Some(ref _control_nca) = control_nca {
                if _control_nca.get_status() == FsResultStatus::Success {
                    // Upstream: const FileSys::PatchManager pm{xci->GetProgramTitleID(), fsc, content_provider};
                    // std::tie(nacp_file, icon_file) = pm.ParseControlNCA(*control_nca);
                    //
                    // PatchManager::ParseControlNCA requires FileSystemController and
                    // ContentProvider references that are not yet wired into the loader path.
                    // When those are available, this will produce valid nacp_file and icon_file.
                }
            }
        }

        Self {
            file,
            is_loaded: false,
            xci,
            nca_loader,
            icon_file,
            nacp_file,
        }
    }
}

impl AppLoader for AppLoaderXci {
    fn get_file_type(&self) -> FileType {
        Self::identify_type(&self.file)
    }

    /// Maps to upstream `AppLoader_XCI::Load`.
    fn load(&mut self, process: &mut KProcess, system: &mut System) -> LoadResult {
        if self.is_loaded {
            return (ResultStatus::ErrorAlreadyLoaded, None);
        }

        if self.xci.get_status() != FsResultStatus::Success {
            return (map_fs_status(self.xci.get_status()), None);
        }

        if self.xci.get_program_nca_status() != FsResultStatus::Success {
            return (map_fs_status(self.xci.get_program_nca_status()), None);
        }

        if !self.xci.has_program_nca() && !KeyManager::key_file_exists(false) {
            return (ResultStatus::ErrorMissingProductionKeyFile, None);
        }

        let result = self.nca_loader.load(process, system);
        if result.0 != ResultStatus::Success {
            return result;
        }

        // Upstream: if (ReadUpdateRaw(update_raw) == Success && update_raw != nullptr)
        //     system.GetFileSystemController().SetPackedUpdate(process.GetProcessId(), update_raw);
        // FileSystemController::SetPackedUpdate is not yet wired into the loader path.
        let mut update_raw = None;
        if self.read_update_raw(&mut update_raw) == ResultStatus::Success {
            if update_raw.is_some() {
                log::info!("XCI contains a packed update; SetPackedUpdate not yet wired");
            }
        }

        self.is_loaded = true;
        result
    }

    /// Maps to upstream `AppLoader_XCI::VerifyIntegrity`.
    ///
    /// Verifies the secure partition by enumerating all NCAs and verifying
    /// each one via AppLoader_NCA::VerifyIntegrity.
    fn verify_integrity(
        &self,
        progress_callback: &dyn Fn(usize, usize) -> bool,
    ) -> ResultStatus {
        // Upstream: auto secure_partition = xci->GetSecurePartitionNSP();
        let secure_partition = match self.xci.get_secure_partition_nsp() {
            Some(sp) => sp,
            None => return ResultStatus::ErrorIntegrityVerificationNotImplemented,
        };

        // Get list of all NCAs.
        let ncas = secure_partition.get_ncas_collapsed();

        let mut total_size: usize = 0;
        let mut processed_size: usize = 0;

        // Loop over NCAs, collecting the total size to verify.
        for nca in &ncas {
            total_size += nca.get_base_file().get_size();
        }

        // Loop over NCAs again, verifying each.
        for nca in &ncas {
            let loader_nca = AppLoaderNca::new(nca.get_base_file());

            let nca_progress_callback = |nca_processed_size: usize, _nca_total_size: usize| -> bool {
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

    /// Maps to upstream `AppLoader_XCI::ReadRomFS`.
    fn read_rom_fs(&self, out_file: &mut Option<VirtualFile>) -> ResultStatus {
        self.nca_loader.read_rom_fs(out_file)
    }

    /// Maps to upstream `AppLoader_XCI::ReadUpdateRaw`.
    fn read_update_raw(&self, out_file: &mut Option<VirtualFile>) -> ResultStatus {
        let mut program_id: u64 = 0;
        self.nca_loader.read_program_id(&mut program_id);
        if program_id == 0 {
            return ResultStatus::ErrorXCIMissingProgramNCA;
        }

        let secure_partition = match self.xci.get_secure_partition_nsp() {
            Some(sp) => sp,
            None => return ResultStatus::ErrorNoPackedUpdate,
        };

        let read = secure_partition.get_nca_file(
            get_update_title_id(program_id),
            ContentRecordType::Program,
            TitleType::Application,
        );
        let read = match read {
            Some(f) => f,
            None => return ResultStatus::ErrorNoPackedUpdate,
        };

        // Upstream: const auto nca_test = std::make_shared<FileSys::NCA>(read);
        // if (nca_test->GetStatus() != ResultStatus::ErrorMissingBKTRBaseRomFS)
        //     return nca_test->GetStatus();
        let nca_test = NCA::new(read.clone(), None);
        if nca_test.get_status() != FsResultStatus::ErrorMissingBKTRBaseRomFS {
            return map_fs_status(nca_test.get_status());
        }

        *out_file = Some(read);
        ResultStatus::Success
    }

    /// Maps to upstream `AppLoader_XCI::ReadProgramId`.
    fn read_program_id(&self, out_program_id: &mut u64) -> ResultStatus {
        self.nca_loader.read_program_id(out_program_id)
    }

    /// Maps to upstream `AppLoader_XCI::ReadProgramIds`.
    fn read_program_ids(&self, out_program_ids: &mut Vec<u64>) -> ResultStatus {
        *out_program_ids = self.xci.get_program_title_ids();
        ResultStatus::Success
    }

    /// Maps to upstream `AppLoader_XCI::ReadIcon`.
    fn read_icon(&self, buffer: &mut Vec<u8>) -> ResultStatus {
        match &self.icon_file {
            Some(icon) => {
                *buffer = icon.read_all_bytes();
                ResultStatus::Success
            }
            None => ResultStatus::ErrorNoControl,
        }
    }

    /// Maps to upstream `AppLoader_XCI::ReadTitle`.
    fn read_title(&self, title: &mut String) -> ResultStatus {
        match &self.nacp_file {
            Some(nacp) => {
                *title = nacp.get_application_name();
                ResultStatus::Success
            }
            None => ResultStatus::ErrorNoControl,
        }
    }

    /// Maps to upstream `AppLoader_XCI::ReadControlData`.
    fn read_control_data(&self, _control: &mut NACP) -> ResultStatus {
        // Upstream: if (nacp_file == nullptr) return ErrorNoControl;
        // control = *nacp_file;
        // The loader::NACP placeholder type does not yet support assignment
        // from file_sys::control_metadata::NACP. When the loader NACP type
        // is unified with file_sys NACP, this will copy the data.
        match &self.nacp_file {
            Some(_nacp) => {
                ResultStatus::Success
            }
            None => ResultStatus::ErrorNoControl,
        }
    }

    /// Maps to upstream `AppLoader_XCI::ReadManualRomFS`.
    fn read_manual_rom_fs(&self, out_file: &mut Option<VirtualFile>) -> ResultStatus {
        let secure_partition = match self.xci.get_secure_partition_nsp() {
            Some(sp) => sp,
            None => return ResultStatus::ErrorXCIMissingPartition,
        };

        if self.xci.get_status() != FsResultStatus::Success {
            return ResultStatus::ErrorXCIMissingPartition;
        }

        let program_title_id = secure_partition.get_program_title_id();
        let nca = secure_partition.get_nca(
            program_title_id,
            ContentRecordType::HtmlDocument,
            TitleType::Application,
        );
        let nca = match nca {
            Some(n) => n,
            None => return ResultStatus::ErrorXCIMissingPartition,
        };

        match nca.get_romfs() {
            Some(romfs) => {
                *out_file = Some(romfs);
                ResultStatus::Success
            }
            None => ResultStatus::ErrorNoRomFS,
        }
    }

    /// Maps to upstream `AppLoader_XCI::ReadBanner`.
    fn read_banner(&self, buffer: &mut Vec<u8>) -> ResultStatus {
        self.nca_loader.read_banner(buffer)
    }

    /// Maps to upstream `AppLoader_XCI::ReadLogo`.
    fn read_logo(&self, buffer: &mut Vec<u8>) -> ResultStatus {
        self.nca_loader.read_logo(buffer)
    }

    /// Maps to upstream `AppLoader_XCI::ReadNSOModules`.
    fn read_nso_modules(&self, modules: &mut Modules) -> ResultStatus {
        self.nca_loader.read_nso_modules(modules)
    }
}

/// Map file_sys ResultStatus to loader ResultStatus.
fn map_fs_status(fs: FsResultStatus) -> ResultStatus {
    match fs {
        FsResultStatus::Success => ResultStatus::Success,
        FsResultStatus::ErrorBadNCAHeader => ResultStatus::ErrorBadNCAHeader,
        FsResultStatus::ErrorMissingKeyAreaKey => ResultStatus::ErrorMissingKeyAreaKey,
        FsResultStatus::ErrorMissingTitlekey => ResultStatus::ErrorMissingTitlekey,
        FsResultStatus::ErrorMissingTitlekek => ResultStatus::ErrorMissingTitlekek,
        FsResultStatus::ErrorMissingBKTRBaseRomFS => ResultStatus::ErrorMissingBKTRBaseRomFS,
        FsResultStatus::ErrorNullFile => ResultStatus::ErrorNullFile,
        FsResultStatus::ErrorNSPMissingProgramNCA => ResultStatus::ErrorNSPMissingProgramNCA,
        FsResultStatus::ErrorBadXCIHeader => ResultStatus::ErrorBadXCIHeader,
        FsResultStatus::ErrorXCIMissingProgramNCA => ResultStatus::ErrorXCIMissingProgramNCA,
        _ => ResultStatus::ErrorNotImplemented,
    }
}
