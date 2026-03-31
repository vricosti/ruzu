// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/loader/loader.h and loader.cpp
//!
//! Main loader interface: AppLoader trait, FileType enum, ResultStatus,
//! IdentifyFile, GuessFromFilename, GetLoader.

use std::collections::BTreeMap;
use std::fmt;
use std::sync::{Arc, Mutex};

use crate::file_sys::registered_cache::ContentProvider;
use crate::file_sys::vfs::vfs_types::{VirtualDir, VirtualFile};
use crate::hle::service::filesystem::filesystem::FileSystemController;

// KProcess is re-exported from the kernel module below.

/// Minimal bridge for Core::System, carrying only the subsystem references that
/// loaders need.  Upstream `Core::System` is a monolith; here we expose only
/// the content provider (for update-NCA lookups) and the filesystem controller
/// (for process registration).
pub struct System {
    pub content_provider: Option<Arc<Mutex<dyn ContentProvider>>>,
    pub filesystem_controller: Option<Arc<Mutex<FileSystemController>>>,
}

pub use crate::hle::kernel::k_process::KProcess;

/// Opaque placeholder for FileSys::NACP (not yet fully ported).
pub struct NACP;

/// File types supported by the loader.
///
/// Maps to upstream `Loader::FileType`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FileType {
    Error,
    Unknown,
    NSO,
    NRO,
    NCA,
    NSP,
    XCI,
    NAX,
    KIP,
    DeconstructedRomDirectory,
}

/// Identifies the type of a bootable file based on the magic value in its header.
///
/// Maps to upstream `Loader::IdentifyFile`.
pub fn identify_file(file: &VirtualFile) -> FileType {
    use super::deconstructed_rom_directory::AppLoaderDeconstructedRomDirectory;
    use super::kip::AppLoaderKip;
    use super::nax::AppLoaderNax;
    use super::nca::AppLoaderNca;
    use super::nro::AppLoaderNro;
    use super::nso::AppLoaderNso;
    use super::nsp::AppLoaderNsp;
    use super::xci::AppLoaderXci;

    if let Some(ft) = identify_file_loader::<AppLoaderNsp>(file) {
        return ft;
    }
    if let Some(ft) = identify_file_loader::<AppLoaderXci>(file) {
        return ft;
    }
    if let Some(ft) = identify_file_loader::<AppLoaderNro>(file) {
        return ft;
    }
    if let Some(ft) = identify_file_loader::<AppLoaderNca>(file) {
        return ft;
    }
    if let Some(ft) = identify_file_loader::<AppLoaderNax>(file) {
        return ft;
    }
    if let Some(ft) = identify_file_loader::<AppLoaderKip>(file) {
        return ft;
    }
    if let Some(ft) = identify_file_loader::<AppLoaderNso>(file) {
        return ft;
    }
    if let Some(ft) = identify_file_loader::<AppLoaderDeconstructedRomDirectory>(file) {
        return ft;
    }

    FileType::Unknown
}

/// Helper: calls T::identify_type and returns Some(file_type) if not Error.
///
/// Maps to upstream anonymous `IdentifyFileLoader<T>` template.
fn identify_file_loader<T: FileTypeIdentifier>(file: &VirtualFile) -> Option<FileType> {
    let file_type = T::identify_type(file);
    if file_type != FileType::Error {
        Some(file_type)
    } else {
        None
    }
}

/// Trait for types that can identify a file type from a VirtualFile.
///
/// Maps to the static `IdentifyType` method on each C++ AppLoader subclass.
pub trait FileTypeIdentifier {
    fn identify_type(file: &VirtualFile) -> FileType;
}

/// Guess the type of a bootable file from its name.
///
/// Maps to upstream `Loader::GuessFromFilename`.
pub fn guess_from_filename(name: &str) -> FileType {
    if name == "main" {
        return FileType::DeconstructedRomDirectory;
    }
    if name == "00" {
        return FileType::NCA;
    }

    let extension = name.rsplit('.').next().unwrap_or("").to_lowercase();

    match extension.as_str() {
        "nro" => FileType::NRO,
        "nso" => FileType::NSO,
        "nca" => FileType::NCA,
        "xci" => FileType::XCI,
        "nsp" => FileType::NSP,
        "kip" => FileType::KIP,
        _ => FileType::Unknown,
    }
}

/// Convert a FileType into a display string.
///
/// Maps to upstream `Loader::GetFileTypeString`.
pub fn get_file_type_string(file_type: FileType) -> &'static str {
    match file_type {
        FileType::NRO => "NRO",
        FileType::NSO => "NSO",
        FileType::NCA => "NCA",
        FileType::XCI => "XCI",
        FileType::NAX => "NAX",
        FileType::NSP => "NSP",
        FileType::KIP => "KIP",
        FileType::DeconstructedRomDirectory => "Directory",
        FileType::Error | FileType::Unknown => "unknown",
    }
}

/// Return type for functions in the Loader module.
///
/// Maps to upstream `Loader::ResultStatus`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u16)]
pub enum ResultStatus {
    Success = 0,
    ErrorAlreadyLoaded,
    ErrorNotImplemented,
    ErrorNotInitialized,
    ErrorBadNPDMHeader,
    ErrorBadACIDHeader,
    ErrorBadACIHeader,
    ErrorBadFileAccessControl,
    ErrorBadFileAccessHeader,
    ErrorBadKernelCapabilityDescriptors,
    ErrorBadPFSHeader,
    ErrorIncorrectPFSFileSize,
    ErrorBadNCAHeader,
    ErrorMissingProductionKeyFile,
    ErrorMissingHeaderKey,
    ErrorIncorrectHeaderKey,
    ErrorNCA2,
    ErrorNCA0,
    ErrorMissingTitlekey,
    ErrorMissingTitlekek,
    ErrorInvalidRightsID,
    ErrorMissingKeyAreaKey,
    ErrorIncorrectKeyAreaKey,
    ErrorIncorrectTitlekeyOrTitlekek,
    ErrorXCIMissingProgramNCA,
    ErrorNCANotProgram,
    ErrorNoExeFS,
    ErrorBadXCIHeader,
    ErrorXCIMissingPartition,
    ErrorNullFile,
    ErrorMissingNPDM,
    Error32BitISA,
    ErrorUnableToParseKernelMetadata,
    ErrorNoRomFS,
    ErrorIncorrectELFFileSize,
    ErrorLoadingNRO,
    ErrorLoadingNSO,
    ErrorNoIcon,
    ErrorNoControl,
    ErrorBadNAXHeader,
    ErrorIncorrectNAXFileSize,
    ErrorNAXKeyHMACFailed,
    ErrorNAXValidationHMACFailed,
    ErrorNAXKeyDerivationFailed,
    ErrorNAXInconvertibleToNCA,
    ErrorBadNAXFilePath,
    ErrorMissingSDSeed,
    ErrorMissingSDKEKSource,
    ErrorMissingAESKEKGenerationSource,
    ErrorMissingAESKeyGenerationSource,
    ErrorMissingSDSaveKeySource,
    ErrorMissingSDNCAKeySource,
    ErrorNSPMissingProgramNCA,
    ErrorBadBKTRHeader,
    ErrorBKTRSubsectionNotAfterRelocation,
    ErrorBKTRSubsectionNotAtEnd,
    ErrorBadRelocationBlock,
    ErrorBadSubsectionBlock,
    ErrorBadRelocationBuckets,
    ErrorBadSubsectionBuckets,
    ErrorMissingBKTRBaseRomFS,
    ErrorNoPackedUpdate,
    ErrorBadKIPHeader,
    ErrorBLZDecompressionFailed,
    ErrorBadINIHeader,
    ErrorINITooManyKIPs,
    ErrorIntegrityVerificationNotImplemented,
    ErrorIntegrityVerificationFailed,
}

/// Result status messages, indexed by discriminant value.
///
/// Maps to upstream `RESULT_MESSAGES` array.
const RESULT_MESSAGES: &[&str] = &[
    "The operation completed successfully.",
    "The loader requested to load is already loaded.",
    "The operation is not implemented.",
    "The loader is not initialized properly.",
    "The NPDM file has a bad header.",
    "The NPDM has a bad ACID header.",
    "The NPDM has a bad ACI header,",
    "The NPDM file has a bad file access control.",
    "The NPDM has a bad file access header.",
    "The NPDM has bad kernel capability descriptors.",
    "The PFS/HFS partition has a bad header.",
    "The PFS/HFS partition has incorrect size as determined by the header.",
    "The NCA file has a bad header.",
    "The general keyfile could not be found.",
    "The NCA Header key could not be found.",
    "The NCA Header key is incorrect or the header is invalid.",
    "Support for NCA2-type NCAs is not implemented.",
    "Support for NCA0-type NCAs is not implemented.",
    "The titlekey for this Rights ID could not be found.",
    "The titlekek for this crypto revision could not be found.",
    "The Rights ID in the header is invalid.",
    "The key area key for this application type and crypto revision could not be found.",
    "The key area key is incorrect or the section header is invalid.",
    "The titlekey and/or titlekek is incorrect or the section header is invalid.",
    "The XCI file is missing a Program-type NCA.",
    "The NCA file is not an application.",
    "The Program-type NCA contains no executable. An update may be required.",
    "The XCI file has a bad header.",
    "The XCI file is missing a partition.",
    "The file could not be found or does not exist.",
    "The game is missing a program metadata file (main.npdm).",
    "The game uses the currently-unimplemented 32-bit architecture.",
    "Unable to completely parse the kernel metadata when loading the emulated process",
    "The RomFS could not be found.",
    "The ELF file has incorrect size as determined by the header.",
    "There was a general error loading the NRO into emulated memory.",
    "There was a general error loading the NSO into emulated memory.",
    "There is no icon available.",
    "There is no control data available.",
    "The NAX file has a bad header.",
    "The NAX file has incorrect size as determined by the header.",
    "The HMAC to generated the NAX decryption keys failed.",
    "The HMAC to validate the NAX decryption keys failed.",
    "The NAX key derivation failed.",
    "The NAX file cannot be interpreted as an NCA file.",
    "The NAX file has an incorrect path.",
    "The SD seed could not be found or derived.",
    "The SD KEK Source could not be found.",
    "The AES KEK Generation Source could not be found.",
    "The AES Key Generation Source could not be found.",
    "The SD Save Key Source could not be found.",
    "The SD NCA Key Source could not be found.",
    "The NSP file is missing a Program-type NCA.",
    "The BKTR-type NCA has a bad BKTR header.",
    "The BKTR Subsection entry is not located immediately after the Relocation entry.",
    "The BKTR Subsection entry is not at the end of the media block.",
    "The BKTR-type NCA has a bad Relocation block.",
    "The BKTR-type NCA has a bad Subsection block.",
    "The BKTR-type NCA has a bad Relocation bucket.",
    "The BKTR-type NCA has a bad Subsection bucket.",
    "Game updates cannot be loaded directly. Load the base game instead.",
    "The NSP or XCI does not contain an update in addition to the base game.",
    "The KIP file has a bad header.",
    "The KIP BLZ decompression of the section failed unexpectedly.",
    "The INI file has a bad header.",
    "The INI file contains more than the maximum allowable number of KIP files.",
    "Integrity verification could not be performed for this file.",
    "Integrity verification failed.",
];

/// Get a human-readable string for a ResultStatus.
///
/// Maps to upstream `Loader::GetResultStatusString`.
pub fn get_result_status_string(status: ResultStatus) -> &'static str {
    let index = status as usize;
    if index < RESULT_MESSAGES.len() {
        RESULT_MESSAGES[index]
    } else {
        "Unknown error."
    }
}

impl fmt::Display for ResultStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", get_result_status_string(*self))
    }
}

/// Load parameters returned on successful load.
///
/// Maps to upstream `AppLoader::LoadParameters`.
#[derive(Debug, Clone, Copy)]
pub struct LoadParameters {
    pub main_thread_priority: i32,
    pub main_thread_stack_size: u64,
}

/// Result of an AppLoader::load() call.
///
/// Maps to upstream `AppLoader::LoadResult = std::pair<ResultStatus, std::optional<LoadParameters>>`.
pub type LoadResult = (ResultStatus, Option<LoadParameters>);

/// Module map: address -> module name.
///
/// Maps to upstream `AppLoader::Modules = std::map<VAddr, std::string>`.
pub type Modules = BTreeMap<u64, String>;

/// Interface for loading an application.
///
/// Maps to upstream `Loader::AppLoader`.
///
/// The C++ class is non-copyable/non-moveable and has virtual methods.
/// In Rust, we use a trait. Each loader holds its own `file` and `is_loaded` state.
pub trait AppLoader: Send + Sync {
    /// Returns the type of this file.
    fn get_file_type(&self) -> FileType;

    /// Load the application and return the created Process instance.
    fn load(&mut self, process: &mut KProcess, system: &mut System) -> LoadResult;

    /// Try to verify the integrity of the file.
    fn verify_integrity(&self, _progress_callback: &dyn Fn(usize, usize) -> bool) -> ResultStatus {
        ResultStatus::ErrorIntegrityVerificationNotImplemented
    }

    /// Get the code (typically .code section) of the application.
    fn read_code(&self, _buffer: &mut Vec<u8>) -> ResultStatus {
        ResultStatus::ErrorNotImplemented
    }

    /// Get the icon (typically icon section) of the application.
    fn read_icon(&self, _buffer: &mut Vec<u8>) -> ResultStatus {
        ResultStatus::ErrorNotImplemented
    }

    /// Get the banner (typically banner section) of the application.
    fn read_banner(&self, _buffer: &mut Vec<u8>) -> ResultStatus {
        ResultStatus::ErrorNotImplemented
    }

    /// Get the logo (typically logo section) of the application.
    fn read_logo(&self, _buffer: &mut Vec<u8>) -> ResultStatus {
        ResultStatus::ErrorNotImplemented
    }

    /// Get the program id of the application.
    fn read_program_id(&self, _out_program_id: &mut u64) -> ResultStatus {
        ResultStatus::ErrorNotImplemented
    }

    /// Get the program ids of the application.
    fn read_program_ids(&self, _out_program_ids: &mut Vec<u64>) -> ResultStatus {
        ResultStatus::ErrorNotImplemented
    }

    /// Get the RomFS of the application.
    fn read_rom_fs(&self, _out_file: &mut Option<VirtualFile>) -> ResultStatus {
        ResultStatus::ErrorNotImplemented
    }

    /// Get the raw update of the application, should it come packed with one.
    fn read_update_raw(&self, _out_file: &mut Option<VirtualFile>) -> ResultStatus {
        ResultStatus::ErrorNotImplemented
    }

    /// Get whether or not updates can be applied to the RomFS.
    fn is_rom_fs_updatable(&self) -> bool {
        true
    }

    /// Get the title of the application.
    fn read_title(&self, _title: &mut String) -> ResultStatus {
        ResultStatus::ErrorNotImplemented
    }

    /// Get the control data (CNMT) of the application.
    fn read_control_data(&self, _control: &mut NACP) -> ResultStatus {
        ResultStatus::ErrorNotImplemented
    }

    /// Get the RomFS of the manual of the application.
    fn read_manual_rom_fs(&self, _out_file: &mut Option<VirtualFile>) -> ResultStatus {
        ResultStatus::ErrorNotImplemented
    }

    /// Read NSO modules.
    fn read_nso_modules(&self, _modules: &mut Modules) -> ResultStatus {
        ResultStatus::ErrorNotImplemented
    }
}

/// Get a loader for a file with a specific type.
///
/// Maps to upstream static `GetFileLoader`.
fn get_file_loader(
    _system: &mut System,
    file: VirtualFile,
    file_type: FileType,
    _program_id: u64,
    _program_index: usize,
) -> Option<Box<dyn AppLoader>> {
    match file_type {
        FileType::NSO => Some(Box::new(super::nso::AppLoaderNso::new(file))),
        FileType::NRO => Some(Box::new(super::nro::AppLoaderNro::new(file))),
        FileType::NCA => Some(Box::new(super::nca::AppLoaderNca::new(file))),
        FileType::XCI => {
            // XCI loader requires FileSystemController and ContentProvider
            // for full NCA extraction. Created with stub until those are ported.
            Some(Box::new(super::xci::AppLoaderXci::new(
                file,
                _program_id,
                _program_index,
            )))
        }
        FileType::NAX => Some(Box::new(super::nax::AppLoaderNax::new(file))),
        FileType::NSP => {
            // NSP loader requires FileSystemController and ContentProvider
            // for control NCA parsing. Created with stub until those are ported.
            Some(Box::new(super::nsp::AppLoaderNsp::new(
                file,
                _program_id,
                _program_index,
            )))
        }
        FileType::KIP => Some(Box::new(super::kip::AppLoaderKip::new(file))),
        FileType::DeconstructedRomDirectory => Some(Box::new(
            super::deconstructed_rom_directory::AppLoaderDeconstructedRomDirectory::new_from_file(
                file,
            ),
        )),
        _ => None,
    }
}

/// Identifies a bootable file and returns a suitable loader.
///
/// Maps to upstream `Loader::GetLoader`.
pub fn get_loader(
    system: &mut System,
    file: VirtualFile,
    program_id: u64,
    program_index: usize,
) -> Option<Box<dyn AppLoader>> {
    let mut file_type = identify_file(&file);
    let filename_type = guess_from_filename(&file.get_name());

    // Special case: 00 is either a NCA or NAX.
    if file_type != filename_type && !(file.get_name() == "00" && file_type == FileType::NAX) {
        log::warn!(
            "File {} has a different type ({}) than its extension.",
            file.get_name(),
            get_file_type_string(file_type)
        );
        if file_type == FileType::Unknown {
            file_type = filename_type;
        }
    }

    log::debug!(
        "Loading file {} as {}...",
        file.get_name(),
        get_file_type_string(file_type)
    );

    get_file_loader(system, file, file_type, program_id, program_index)
}

/// Helper to create a 4-byte magic number from individual bytes.
///
/// Maps to upstream `Common::MakeMagic`.
pub const fn make_magic(a: u8, b: u8, c: u8, d: u8) -> u32 {
    (a as u32) | ((b as u32) << 8) | ((c as u32) << 16) | ((d as u32) << 24)
}
