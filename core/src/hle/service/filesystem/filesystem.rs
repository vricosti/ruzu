//! Port of zuyu/src/core/hle/service/filesystem/filesystem.h and filesystem.cpp
//!
//! FileSystemController and VfsDirectoryServiceWrapper.

/// Port of Service::FileSystem::ContentStorageId
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ContentStorageId {
    System = 0,
    User = 1,
    SdCard = 2,
}

/// Port of Service::FileSystem::ImageDirectoryId
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ImageDirectoryId {
    Nand = 0,
    SdCard = 1,
}

/// Port of Service::FileSystem::ProcessId
pub type ProcessId = u64;

/// Port of Service::FileSystem::ProgramId
pub type ProgramId = u64;

/// Port of Service::FileSystem::FileSystemController
///
/// Manages filesystem factories and process registrations.
pub struct FileSystemController {
    // registration_lock: Mutex,
    // registrations: BTreeMap<ProcessId, Registration>,
    // sdmc_factory: Option<SDMCFactory>,
    // bis_factory: Option<BISFactory>,
    // gamecard: Option<XCI>,
    // gamecard_registered: Option<RegisteredCache>,
    // gamecard_placeholder: Option<PlaceholderCache>,
}

impl FileSystemController {
    pub fn new() -> Self {
        Self {}
    }

    pub fn create_factories(&mut self) {
        // Upstream creates SDMCFactory and BISFactory using the VfsFilesystem.
        // TODO: Wire up VfsFilesystem factories when FileSys crate is ported.
        log::warn!("FileSystemController::create_factories: VfsFilesystem not yet ported, skipping");
    }

    pub fn reset(&mut self) {
        // TODO: Clear registrations
    }
}

/// Port of Service::FileSystem::VfsDirectoryServiceWrapper
///
/// Wraps a VfsDirectory with Result-returning methods for use with Switch services.
pub struct VfsDirectoryServiceWrapper {
    // backing: VirtualDir,
}

impl VfsDirectoryServiceWrapper {
    pub fn new() -> Self {
        Self {}
    }
}

/// Named services registered by the filesystem module:
/// - "fsp-ldr" -> FSP_LDR
/// - "fsp:pr"  -> FSP_PR
/// - "fsp-srv" -> FSP_SRV
pub fn loop_process() {
    // Upstream registers FSP_LDR, FSP_PR, FSP_SRV with the ServerManager.
    // TODO: Wire up to ServerManager when service framework is ported.
    log::warn!("FileSystem::loop_process: ServerManager not yet ported, services not registered");
}
