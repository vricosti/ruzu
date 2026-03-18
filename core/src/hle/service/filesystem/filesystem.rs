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

/// Named services registered by the filesystem module:
/// - "fsp-ldr" -> FSP_LDR
/// - "fsp:pr"  -> FSP_PR
/// - "fsp-srv" -> FSP_SRV
pub fn loop_process(service_manager: &Arc<Mutex<ServiceManager>>) {
    register_services(service_manager);
}

pub fn register_services(service_manager: &Arc<Mutex<ServiceManager>>) {
    register_named_service(service_manager, "fsp-ldr", || -> SessionRequestHandlerPtr {
        Arc::new(super::fsp::fsp_ldr::FspLdr::new())
    });
    register_named_service(service_manager, "fsp:pr", || -> SessionRequestHandlerPtr {
        Arc::new(super::fsp::fsp_pr::FspPr::new())
    });
    register_named_service(service_manager, "fsp-srv", || -> SessionRequestHandlerPtr {
        Arc::new(super::fsp::fsp_srv::FspSrv::new())
    });
}

fn register_named_service<F>(service_manager: &Arc<Mutex<ServiceManager>>, name: &str, factory: F)
where
    F: Fn() -> SessionRequestHandlerPtr + Send + Sync + 'static,
{
    let boxed: SessionRequestHandlerFactory = Box::new(factory);
    let result = service_manager
        .lock()
        .unwrap()
        .register_service(name.to_string(), 64, boxed);
    if result.is_error() {
        log::warn!(
            "Failed to register service '{}': {:#x}",
            name,
            result.get_inner_value()
        );
    }
}
