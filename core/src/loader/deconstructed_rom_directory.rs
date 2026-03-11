// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/loader/deconstructed_rom_directory.h and
//! deconstructed_rom_directory.cpp
//!
//! Loads a "deconstructed ROM directory", which are the typical format for
//! Switch game dumps. The path should be a "main" NSO in a directory that
//! contains the other standard ExeFS NSOs (rtld, sdk, etc.).

use crate::file_sys::vfs::vfs_types::{VirtualDir, VirtualFile};

use super::loader::{
    AppLoader, FileType, FileTypeIdentifier, KProcess, LoadResult, Modules, ResultStatus, System,
};

// ============================================================================
// Static module list
// ============================================================================

/// The standard set of ExeFS modules, in load order.
///
/// Maps to upstream `static_modules` array in deconstructed_rom_directory.cpp.
const STATIC_MODULES: &[&str] = &[
    "rtld", "main", "subsdk0", "subsdk1", "subsdk2", "subsdk3", "subsdk4", "subsdk5", "subsdk6",
    "subsdk7", "subsdk8", "subsdk9", "sdk",
];

// ============================================================================
// AppLoaderDeconstructedRomDirectory
// ============================================================================

/// Loads a deconstructed ROM directory (ExeFS dump).
///
/// Maps to upstream `Loader::AppLoader_DeconstructedRomDirectory`.
pub struct AppLoaderDeconstructedRomDirectory {
    file: Option<VirtualFile>,
    dir: Option<VirtualDir>,
    is_loaded: bool,
    // TODO: metadata: FileSys::ProgramMetadata — requires ProgramMetadata to be ported.
    romfs: Option<VirtualFile>,
    icon_data: Vec<u8>,
    name: String,
    title_id: u64,
    override_update: bool,
    is_hbl: bool,
    modules: Modules,
}

impl FileTypeIdentifier for AppLoaderDeconstructedRomDirectory {
    /// Identifies whether or not the given file is a deconstructed ROM directory.
    ///
    /// Maps to upstream `AppLoader_DeconstructedRomDirectory::IdentifyType`.
    fn identify_type(dir_file: &VirtualFile) -> FileType {
        // Upstream checks: FileSys::IsDirectoryExeFS(dir_file->GetContainingDirectory())
        if let Some(containing_dir) = dir_file.get_containing_directory() {
            if is_directory_exefs(&containing_dir) {
                return FileType::DeconstructedRomDirectory;
            }
        }

        FileType::Error
    }
}

/// Check if a directory is an ExeFS directory.
///
/// Maps to upstream `FileSys::IsDirectoryExeFS`.
/// TODO: Use the actual implementation from file_sys::content_archive when it's
/// wired into the module tree.
fn is_directory_exefs(dir: &VirtualDir) -> bool {
    // An ExeFS directory contains "main" and "main.npdm" files.
    let has_main = dir.get_file("main").is_some();
    let has_npdm = dir.get_file("main.npdm").is_some();
    has_main && has_npdm
}

impl AppLoaderDeconstructedRomDirectory {
    /// Create a new deconstructed ROM directory loader from a main NSO file.
    ///
    /// Maps to upstream `AppLoader_DeconstructedRomDirectory(VirtualFile, bool)`.
    pub fn new_from_file(file: VirtualFile) -> Self {
        let title_id = 0u64;
        let mut icon_data = Vec::new();
        let name = String::new();

        // Try to read metadata from the containing directory
        if let Some(file_dir) = file.get_containing_directory() {
            // Title ID from main.npdm
            // TODO: Parse ProgramMetadata from npdm file when ported.
            let _npdm = file_dir.get_file("main.npdm");

            // Icon: look for icon_<language>.dat
            // TODO: Use FileSys::LANGUAGE_NAMES when ported.
            let language_names = [
                "AmericanEnglish",
                "BritishEnglish",
                "Japanese",
                "French",
                "German",
                "LatinAmericanSpanish",
                "Spanish",
                "Italian",
                "Dutch",
                "CanadianFrench",
                "Portuguese",
                "Russian",
                "Korean",
                "TraditionalChinese",
                "SimplifiedChinese",
                "BrazilianPortuguese",
            ];

            for lang in &language_names {
                let icon_filename = format!("icon_{}.dat", lang);
                if let Some(icon_file) = file_dir.get_file(&icon_filename) {
                    icon_data = icon_file.read_all_bytes();
                    break;
                }
            }

            if icon_data.is_empty() {
                // Any png, jpeg, or bmp file
                for f in file_dir.get_files() {
                    let ext = f.get_extension();
                    if ext == "png" || ext == "jpg" || ext == "bmp" || ext == "jpeg" {
                        icon_data = f.read_all_bytes();
                        break;
                    }
                }
            }

            // Metadata: look for control.nacp or any .nacp file
            // TODO: Parse FileSys::NACP when ported.
            let _nacp_file = file_dir.get_file("control.nacp").or_else(|| {
                file_dir
                    .get_files()
                    .into_iter()
                    .find(|f| f.get_extension() == "nacp")
            });
        }

        Self {
            file: Some(file),
            dir: None,
            is_loaded: false,
            romfs: None,
            icon_data,
            name,
            title_id,
            override_update: false,
            is_hbl: false,
            modules: Modules::new(),
        }
    }

    /// Create a new deconstructed ROM directory loader from an ExeFS directory.
    ///
    /// Maps to upstream `AppLoader_DeconstructedRomDirectory(VirtualDir, bool, bool)`.
    pub fn new_from_directory(
        directory: VirtualDir,
        override_update: bool,
        is_hbl: bool,
    ) -> Self {
        let file = directory.get_file("main");
        Self {
            file,
            dir: Some(directory),
            is_loaded: false,
            romfs: None,
            icon_data: Vec::new(),
            name: String::new(),
            title_id: 0,
            override_update,
            is_hbl,
            modules: Modules::new(),
        }
    }
}

impl AppLoader for AppLoaderDeconstructedRomDirectory {
    fn get_file_type(&self) -> FileType {
        match &self.file {
            Some(f) => Self::identify_type(f),
            None => FileType::Error,
        }
    }

    /// Maps to upstream `AppLoader_DeconstructedRomDirectory::Load`.
    fn load(&mut self, _process: &mut KProcess, _system: &mut System) -> LoadResult {
        if self.is_loaded {
            return (ResultStatus::ErrorAlreadyLoaded, None);
        }

        // Resolve dir from file if needed
        if self.dir.is_none() {
            match &self.file {
                Some(f) => {
                    self.dir = f.get_containing_directory();
                }
                None => return (ResultStatus::ErrorNullFile, None),
            }
        }

        let dir = match &self.dir {
            Some(d) => d.clone(),
            None => return (ResultStatus::ErrorNullFile, None),
        };

        // Read meta to determine title ID
        let npdm = dir.get_file("main.npdm");
        if npdm.is_none() {
            return (ResultStatus::ErrorMissingNPDM, None);
        }

        // TODO: Full implementation requires:
        // - ProgramMetadata::Load from npdm
        // - PatchManager::PatchExeFS if override_update
        // - Reload metadata after patching
        // - NCE setup based on address space type
        // - Two-pass NSO loading (layout computation then actual load)
        //   using AppLoaderNso::load_module for each static module
        // - Process setup via LoadFromMetadata
        // - Cheat/patch application

        log::warn!(
            "DeconstructedRomDirectory loader: Load not fully implemented \
             (requires ProgramMetadata, PatchManager, NSO loading pipeline)"
        );
        (ResultStatus::ErrorNotImplemented, None)
    }

    fn read_rom_fs(&self, out_file: &mut Option<VirtualFile>) -> ResultStatus {
        match &self.romfs {
            Some(f) => {
                *out_file = Some(f.clone());
                ResultStatus::Success
            }
            None => ResultStatus::ErrorNoRomFS,
        }
    }

    fn read_icon(&self, buffer: &mut Vec<u8>) -> ResultStatus {
        if self.icon_data.is_empty() {
            return ResultStatus::ErrorNoIcon;
        }
        *buffer = self.icon_data.clone();
        ResultStatus::Success
    }

    fn read_program_id(&self, out_program_id: &mut u64) -> ResultStatus {
        *out_program_id = self.title_id;
        ResultStatus::Success
    }

    fn read_title(&self, title: &mut String) -> ResultStatus {
        if self.name.is_empty() {
            return ResultStatus::ErrorNoControl;
        }
        *title = self.name.clone();
        ResultStatus::Success
    }

    fn is_rom_fs_updatable(&self) -> bool {
        false
    }

    fn read_nso_modules(&self, out_modules: &mut Modules) -> ResultStatus {
        if !self.is_loaded {
            return ResultStatus::ErrorNotInitialized;
        }
        *out_modules = self.modules.clone();
        ResultStatus::Success
    }
}
