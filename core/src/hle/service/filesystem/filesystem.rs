//! Port of zuyu/src/core/hle/service/filesystem/filesystem.h and filesystem.cpp
//!
//! FileSystemController and VfsDirectoryServiceWrapper.

use std::sync::{Arc, Mutex};

use crate::hle::service::hle_ipc::{SessionRequestHandlerFactory, SessionRequestHandlerPtr};
use crate::hle::service::sm::sm::ServiceManager;

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

/// Launches FileSystem services.
///
/// Matches upstream `void FileSystem::LoopProcess(Core::System& system)`:
/// Registers "fsp-ldr", "fsp:pr", "fsp-srv".
pub fn loop_process(service_manager: &Arc<Mutex<ServiceManager>>) {
    register_services(service_manager);
}

pub fn register_services(service_manager: &Arc<Mutex<ServiceManager>>) {
    let mut server_manager =
        crate::hle::service::server_manager::ServerManager::new(service_manager.clone());

    server_manager.register_named_service(
        "fsp-ldr",
        Box::new(|| -> SessionRequestHandlerPtr {
            Arc::new(super::fsp::fsp_ldr::FspLdr::new())
        }),
        64,
    );
    server_manager.register_named_service(
        "fsp:pr",
        Box::new(|| -> SessionRequestHandlerPtr {
            Arc::new(super::fsp::fsp_pr::FspPr::new())
        }),
        64,
    );
    server_manager.register_named_service(
        "fsp-srv",
        Box::new(|| -> SessionRequestHandlerPtr {
            Arc::new(super::fsp::fsp_srv::FspSrv::new())
        }),
        64,
    );

    crate::hle::service::server_manager::ServerManager::run_server(server_manager);
}
