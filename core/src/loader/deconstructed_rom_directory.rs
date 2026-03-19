// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/loader/deconstructed_rom_directory.h and
//! deconstructed_rom_directory.cpp
//!
//! Loads a "deconstructed ROM directory", which are the typical format for
//! Switch game dumps. The path should be a "main" NSO in a directory that
//! contains the other standard ExeFS NSOs (rtld, sdk, etc.).

use crate::file_sys::program_metadata::ProgramMetadata;
use crate::file_sys::vfs::vfs_types::{VirtualDir, VirtualFile};
use crate::hle::result::RESULT_SUCCESS;

use super::loader::{
    AppLoader, FileType, FileTypeIdentifier, KProcess, LoadParameters, LoadResult, Modules,
    ResultStatus, System,
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
    fn load(&mut self, process: &mut KProcess, system: &mut System) -> LoadResult {
        use crate::file_sys::partition_filesystem::ResultStatus as PfsResultStatus;

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
        let npdm_file = match dir.get_file("main.npdm") {
            Some(f) => f,
            None => return (ResultStatus::ErrorMissingNPDM, None),
        };

        let mut metadata = ProgramMetadata::new();
        let result = metadata.load(npdm_file);
        if result != PfsResultStatus::Success {
            // Map PFS result status to loader result status for NPDM errors
            let loader_status = match result {
                PfsResultStatus::ErrorBadNCAHeader => ResultStatus::ErrorBadNPDMHeader,
                _ => ResultStatus::ErrorBadNPDMHeader,
            };
            return (loader_status, None);
        }

        // TODO: PatchManager::PatchExeFS when override_update is set
        // TODO: Reload metadata after patching

        metadata.print();
        self.title_id = metadata.get_title_id();

        // Upstream does not block on 32-bit ISA here; it passes metadata
        // to KProcess::LoadFromMetadata which handles address space setup.
        let is_64bit = metadata.is_64_bit_program();
        if !is_64bit {
            log::info!("Loading a 32-bit (AArch32) program");
        }

        // ====================================================================
        // Pass 1: Layout computation (load_into_process = false)
        // ====================================================================
        // Use the NSO module loader to figure out the code layout.
        // Maps to upstream first loop with `LoadModule(..., false, ...)`.
        let mut code_size: u64 = 0;

        for &module_name in STATIC_MODULES {
            let module_file = match dir.get_file(module_name) {
                Some(f) => f,
                None => continue,
            };

            let should_pass_args = module_name == "rtld";
            match super::nso::AppLoaderNso::load_module(
                process,
                system,
                module_file.as_ref(),
                code_size,
                should_pass_args,
                false, // load_into_process = false
            ) {
                Some(tentative_next_load_addr) => {
                    code_size = tentative_next_load_addr;
                }
                None => {
                    return (ResultStatus::ErrorLoadingNSO, None);
                }
            }
        }

        // ====================================================================
        // Process setup
        // ====================================================================
        // Determine the code base address from address space type, matching upstream
        // LoadFromMetadata's address space switch.
        let code_base: u64 = match metadata.get_address_space_type() {
            crate::file_sys::program_metadata::ProgramAddressSpaceType::Is32Bit |
            crate::file_sys::program_metadata::ProgramAddressSpaceType::Is32BitNoMap => 0x0020_0000,
            crate::file_sys::program_metadata::ProgramAddressSpaceType::Is36Bit => 0x0800_0000,
            crate::file_sys::program_metadata::ProgramAddressSpaceType::Is39Bit => 0x8000_0000,
        };

        // Pre-allocate code memory BEFORE LoadFromMetadata, because
        // LoadFromMetadata -> initialize_for_user -> initialize -> create_thread_local_region
        // needs the page table's block manager to be initialized.
        // allocate_code_memory configures the address space and initializes
        // ProcessMemoryData to cover the full code + TLS region.
        process.allocate_code_memory(code_base, code_size as usize);

        // Configure TLS page allocation base before LoadFromMetadata, which calls
        // Initialize -> create_thread_local_region for the PLR.
        process.initialize_thread_local_region_allocation(code_base + code_size);

        // Upstream: process.LoadFromMetadata(metadata, code_size, aslr_space_start, is_hbl)
        let process_setup_result = process.load_from_metadata(
            &metadata,
            code_size,
            0, // aslr_space_start — upstream passes this from KProcess::Create
            false, // is_hbl
        );
        if process_setup_result != RESULT_SUCCESS.get_inner_value() {
            return (ResultStatus::ErrorBadNPDMHeader, None);
        }

        // ====================================================================
        // Pass 2: Actual loading (load_into_process = true)
        // ====================================================================
        // Maps to upstream second loop with `LoadModule(..., true, ...)`.
        self.modules.clear();
        let mut next_load_addr: u64 = code_base;

        for &module_name in STATIC_MODULES {
            let module_file = match dir.get_file(module_name) {
                Some(f) => f,
                None => continue,
            };

            let load_addr = next_load_addr;
            let should_pass_args = module_name == "rtld";
            match super::nso::AppLoaderNso::load_module(
                process,
                system,
                module_file.as_ref(),
                load_addr,
                should_pass_args,
                true, // load_into_process = true
            ) {
                Some(tentative_next_load_addr) => {
                    next_load_addr = tentative_next_load_addr;
                    self.modules.insert(load_addr, module_name.to_string());
                    log::debug!("loaded module {} @ {:#X}", module_name, load_addr);
                }
                None => {
                    return (ResultStatus::ErrorLoadingNSO, None);
                }
            }
        }

        self.is_loaded = true;

        (
            ResultStatus::Success,
            Some(LoadParameters {
                main_thread_priority: metadata.get_main_thread_priority() as i32,
                main_thread_stack_size: metadata.get_main_thread_stack_size() as u64,
            }),
        )
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
