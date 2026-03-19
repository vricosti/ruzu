//! Port of zuyu/src/core/hle/service/filesystem/filesystem.h and filesystem.cpp
//!
//! FileSystemController and VfsDirectoryServiceWrapper.

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::result::RESULT_SUCCESS;
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

/// Port of upstream `FileSystemController::Registration` (filesystem.h:129-133).
struct Registration {
    program_id: ProgramId,
    // romfs_factory: Option<Arc<RomFSFactory>>,    // TODO: wire once RomFSFactory is ported
    // save_data_factory: Option<Arc<SaveDataFactory>>, // TODO: wire once SaveDataFactory is ported
}

/// Port of Service::FileSystem::FileSystemController
///
/// Manages filesystem factories and process registrations.
/// Upstream: filesystem.h:64-146, filesystem.cpp.
pub struct FileSystemController {
    registrations: Mutex<BTreeMap<ProcessId, Registration>>,
    // sdmc_factory: Option<SDMCFactory>,
    // bis_factory: Option<BISFactory>,
    // gamecard: Option<XCI>,
    // gamecard_registered: Option<RegisteredCache>,
    // gamecard_placeholder: Option<PlaceholderCache>,
}

impl FileSystemController {
    pub fn new() -> Self {
        Self {
            registrations: Mutex::new(BTreeMap::new()),
        }
    }

    /// Port of upstream `FileSystemController::RegisterProcess` (filesystem.cpp:298-311).
    ///
    /// Called by the NCA/NRO loader after loading a process. Stores the
    /// process_id → program_id mapping so that FSP_SRV::SetCurrentProcess
    /// can resolve the program_id from the kernel process ID.
    pub fn register_process(
        &self,
        process_id: ProcessId,
        program_id: ProgramId,
        // romfs_factory: Arc<RomFSFactory>,  // TODO: pass once ported
    ) -> u32 {
        let mut registrations = self.registrations.lock().unwrap();
        registrations.insert(process_id, Registration {
            program_id,
            // romfs_factory: Some(romfs_factory),
            // save_data_factory: Some(self.create_save_data_factory(program_id)),
        });
        log::debug!(
            "FileSystemController::RegisterProcess: process_id={:#x}, program_id={:#018x}",
            process_id, program_id,
        );
        RESULT_SUCCESS.get_inner_value()
    }

    /// Port of upstream `FileSystemController::OpenProcess` (filesystem.cpp:313-328).
    ///
    /// Looks up the registration for `process_id` and returns the associated
    /// program_id. Upstream also returns SaveDataController and RomFsController;
    /// those are stubbed until the factory types are ported.
    pub fn open_process(&self, process_id: ProcessId) -> Option<ProgramId> {
        let registrations = self.registrations.lock().unwrap();
        registrations.get(&process_id).map(|r| r.program_id)
    }

    pub fn create_factories(&mut self) {
        // Upstream creates SDMCFactory and BISFactory using the VfsFilesystem.
        // TODO: Wire up VfsFilesystem factories when FileSys crate is ported.
        log::warn!("FileSystemController::create_factories: VfsFilesystem not yet ported, skipping");
    }

    pub fn reset(&mut self) {
        self.registrations.lock().unwrap().clear();
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
pub fn loop_process(
    service_manager: &Arc<Mutex<ServiceManager>>,
    fsc: Arc<Mutex<FileSystemController>>,
) {
    register_services(service_manager, fsc);
}

pub fn register_services(
    service_manager: &Arc<Mutex<ServiceManager>>,
    fsc: Arc<Mutex<FileSystemController>>,
) {
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
    let fsc_for_closure = fsc.clone();
    server_manager.register_named_service(
        "fsp-srv",
        Box::new(move || -> SessionRequestHandlerPtr {
            Arc::new(super::fsp::fsp_srv::FspSrv::new_with_fsc(fsc_for_closure.clone()))
        }),
        64,
    );

    crate::hle::service::server_manager::ServerManager::run_server(server_manager);
}
