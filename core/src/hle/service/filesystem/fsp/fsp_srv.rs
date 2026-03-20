//! Port of zuyu/src/core/hle/service/filesystem/fsp/fsp_srv.h and fsp_srv.cpp
//!
//! FSP_SRV service ("fsp-srv").

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::file_sys::errors::RESULT_TARGET_NOT_FOUND;
use crate::file_sys::vfs::vfs_types::VirtualFile;
use crate::file_sys::vfs::vfs_vector::VectorVfsFile;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

use super::fs_i_filesystem::IFileSystem;
use super::fs_i_storage::IStorage;

/// Port of Service::FileSystem::AccessLogVersion
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum AccessLogVersion {
    V7_0_0 = 2,
}

impl AccessLogVersion {
    pub const LATEST: Self = Self::V7_0_0;
}

/// Port of Service::FileSystem::AccessLogMode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum AccessLogMode {
    None = 0,
    Log = 1,
    SdCard = 2,
}

/// IPC command table for FSP_SRV ("fsp-srv"):
///
/// | Cmd  | Name                                                          |
/// |------|---------------------------------------------------------------|
/// | 0    | OpenFileSystem                                                |
/// | 1    | SetCurrentProcess                                             |
/// | 2    | OpenDataFileSystemByCurrentProcess                             |
/// | 7    | OpenFileSystemWithPatch                                       |
/// | 8    | OpenFileSystemWithId                                          |
/// | 9    | OpenDataFileSystemByApplicationId                              |
/// | 11   | OpenBisFileSystem                                             |
/// | 12   | OpenBisStorage                                                |
/// | 13   | InvalidateBisCache                                            |
/// | 17   | OpenHostFileSystem                                            |
/// | 18   | OpenSdCardFileSystem                                          |
/// | 19   | FormatSdCardFileSystem                                        |
/// | 21   | DeleteSaveDataFileSystem                                      |
/// | 22   | CreateSaveDataFileSystem                                      |
/// | 23   | CreateSaveDataFileSystemBySystemSaveDataId                     |
/// | 24   | RegisterSaveDataFileSystemAtomicDeletion                       |
/// | 25   | DeleteSaveDataFileSystemBySaveDataSpaceId                      |
/// | 26   | FormatSdCardDryRun                                            |
/// | 27   | IsExFatSupported                                              |
/// | 28   | DeleteSaveDataFileSystemBySaveDataAttribute                    |
/// | 30   | OpenGameCardStorage                                           |
/// | 31   | OpenGameCardFileSystem                                        |
/// | 32   | ExtendSaveDataFileSystem                                      |
/// | 33   | DeleteCacheStorage                                            |
/// | 34   | GetCacheStorageSize                                           |
/// | 35   | CreateSaveDataFileSystemByHashSalt                             |
/// | 36   | OpenHostFileSystemWithOption                                   |
/// | 51   | OpenSaveDataFileSystem                                        |
/// | 52   | OpenSaveDataFileSystemBySystemSaveDataId                       |
/// | 53   | OpenReadOnlySaveDataFileSystem                                 |
/// | 57   | ReadSaveDataFileSystemExtraDataBySaveDataSpaceId                |
/// | 58   | ReadSaveDataFileSystemExtraData                                 |
/// | 59   | WriteSaveDataFileSystemExtraData                                |
/// | 60   | OpenSaveDataInfoReader                                          |
/// | 61   | OpenSaveDataInfoReaderBySaveDataSpaceId                         |
/// | 62   | OpenSaveDataInfoReaderOnlyCacheStorage                          |
/// | 64   | OpenSaveDataInternalStorageFileSystem                           |
/// | 65   | UpdateSaveDataMacForDebug                                       |
/// | 66   | WriteSaveDataFileSystemExtraData2                                |
/// | 67   | FindSaveDataWithFilter                                           |
/// | 68   | OpenSaveDataInfoReaderBySaveDataFilter                           |
/// | 69   | ReadSaveDataFileSystemExtraDataBySaveDataAttribute               |
/// | 70   | WriteSaveDataFileSystemExtraDataWithMaskBySaveDataAttribute      |
/// | 71   | ReadSaveDataFileSystemExtraDataWithMaskBySaveDataAttribute       |
/// | 80   | OpenSaveDataMetaFile                                             |
/// | 81   | OpenSaveDataTransferManager                                      |
/// | 82   | OpenSaveDataTransferManagerVersion2                               |
/// | 83   | OpenSaveDataTransferProhibiter                                    |
/// | 84   | ListApplicationAccessibleSaveDataOwnerId                          |
/// | 85   | OpenSaveDataTransferManagerForSaveDataRepair                       |
/// | 86   | OpenSaveDataMover                                                  |
/// | 87   | OpenSaveDataTransferManagerForRepair                               |
/// | 100  | OpenImageDirectoryFileSystem                                       |
/// | 101  | OpenBaseFileSystem                                                  |
/// | 102  | FormatBaseFileSystem                                                |
/// | 110  | OpenContentStorageFileSystem                                        |
/// | 120  | OpenCloudBackupWorkStorageFileSystem                                |
/// | 130  | OpenCustomStorageFileSystem                                          |
/// | 200  | OpenDataStorageByCurrentProcess                                      |
/// | 201  | OpenDataStorageByProgramId                                            |
/// | 202  | OpenDataStorageByDataId                                                |
/// | 203  | OpenPatchDataStorageByCurrentProcess                                   |
/// | 204  | OpenDataFileSystemByProgramIndex                                       |
/// | 205  | OpenDataStorageWithProgramIndex                                        |
/// | 206  | OpenDataStorageByPath                                                  |
/// | 400  | OpenDeviceOperator                                                     |
/// | 500  | OpenSdCardDetectionEventNotifier                                       |
/// | 501  | OpenGameCardDetectionEventNotifier                                     |
/// | 510  | OpenSystemDataUpdateEventNotifier                                      |
/// | 511  | NotifySystemDataUpdateEvent                                            |
/// | 520  | SimulateGameCardDetectionEvent                                         |
/// | 600-617 | (various utility commands)                                          |
/// | 620  | SetSdCardEncryptionSeed                                                |
/// | 630-631 | SD card accessibility                                               |
/// | 640  | IsSignedSystemPartitionOnSdCardValid                                   |
/// | 700-720 | Access failure resolver                                             |
/// | 800  | GetAndClearFileSystemProxyErrorInfo                                    |
/// | 810  | RegisterProgramIndexMapInfo                                            |
/// | 1000-1019 | Debug/development commands                                       |
/// | 1003 | DisableAutoSaveDataCreation                                            |
/// | 1004 | SetGlobalAccessLogMode                                                 |
/// | 1005 | GetGlobalAccessLogMode                                                 |
/// | 1006 | OutputAccessLogToSdCard                                                |
/// | 1011 | GetProgramIndexForAccessLog                                            |
/// | 1016 | FlushAccessLogOnSdCard                                                 |
/// | 1100 | OverrideSaveDataTransferTokenSignVerificationKey                        |
/// | 1110 | CorruptSaveDataFileSystemBySaveDataSpaceId2                             |
/// | 1200 | OpenMultiCommitManager                                                 |
/// | 1300 | OpenBisWiper                                                           |
pub struct FspSrv {
    /// Upstream: `FileSystemController& fsc`.
    fsc: Option<Arc<std::sync::Mutex<super::super::filesystem::FileSystemController>>>,
    current_process_id: std::sync::Mutex<u64>,
    access_log_program_index: std::sync::Mutex<u32>,
    access_log_mode: std::sync::Mutex<AccessLogMode>,
    program_id: std::sync::Mutex<u64>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl FspSrv {
    pub fn new() -> Self {
        Self {
            fsc: None,
            current_process_id: std::sync::Mutex::new(0),
            access_log_program_index: std::sync::Mutex::new(0),
            access_log_mode: std::sync::Mutex::new(AccessLogMode::None),
            program_id: std::sync::Mutex::new(0),
            handlers: build_handler_map(&[
                (1, Some(Self::set_current_process_handler), "SetCurrentProcess"),
                (18, Some(Self::open_sd_card_file_system_handler), "OpenSdCardFileSystem"),
                (200, Some(Self::open_data_storage_by_current_process_handler), "OpenDataStorageByCurrentProcess"),
                (203, Some(Self::open_patch_data_storage_by_current_process_handler), "OpenPatchDataStorageByCurrentProcess"),
                (1004, Some(Self::set_global_access_log_mode_handler), "SetGlobalAccessLogMode"),
                (1005, Some(Self::get_global_access_log_mode_handler), "GetGlobalAccessLogMode"),
                (1011, Some(Self::get_program_index_for_access_log_handler), "GetProgramIndexForAccessLog"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Create an FspSrv with a reference to the FileSystemController.
    /// Matches upstream `FSP_SRV(Core::System& system_)` constructor.
    pub fn new_with_fsc(
        fsc: Arc<std::sync::Mutex<super::super::filesystem::FileSystemController>>,
    ) -> Self {
        let mut srv = Self::new();
        srv.fsc = Some(fsc);
        srv
    }

    /// Push an error response for a command that has Out<SharedPointer<T>> in domain mode.
    /// In domain mode, the response always includes a domain object ID slot (0 for null),
    /// matching upstream CMIF serialization where response layout is compile-time fixed.
    fn push_error_with_null_interface(ctx: &mut HLERequestContext, error: u32) {
        let is_domain = ctx
            .get_manager()
            .map_or(false, |m| m.lock().unwrap().is_domain());
        if is_domain {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
            rb.push_result(ResultCode::new(error));
            // Add a null domain object — WriteToOutgoingCommandBuffer will write 0
            // to the domain object ID slot, matching upstream behavior.
            ctx.add_null_domain_object();
        } else {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(ResultCode::new(error));
        }
    }

    fn push_interface_response(ctx: &mut HLERequestContext, object: Arc<dyn SessionRequestHandler>) {
        let is_domain = ctx
            .get_manager()
            .map_or(false, |manager| manager.lock().unwrap().is_domain());
        let move_handle = if is_domain {
            0
        } else {
            ctx.create_session_for_service(object.clone()).unwrap_or(0)
        };
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        if is_domain {
            ctx.add_domain_object(object);
        } else {
            rb.push_move_objects(move_handle);
        }
    }

    /// Port of upstream `FSP_SRV::SetCurrentProcess` (fsp_srv.cpp:186-193).
    ///
    /// Upstream calls `fsc.OpenProcess(&program_id, ..., current_process_id)`
    /// which looks up the process_id → program_id mapping registered by the
    /// NCA loader via `FileSystemController::RegisterProcess`.
    fn set_current_process_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const FspSrv) };
        let pid = ctx.get_pid();
        *service.current_process_id.lock().unwrap() = pid;

        // Upstream: fsc.OpenProcess(&program_id, &save_data_controller, &romfs_controller, pid)
        let program_id = service
            .fsc
            .as_ref()
            .and_then(|fsc| fsc.lock().unwrap().open_process(pid));

        match program_id {
            Some(program_id) => {
                *service.program_id.lock().unwrap() = program_id;
                log::info!(
                    "FspSrv::SetCurrentProcess: pid={:#x}, program_id={:#018x}",
                    pid, program_id,
                );
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(RESULT_SUCCESS);
            }
            None => {
                // No registration found — matches upstream returning ResultTargetNotFound.
                log::warn!(
                    "FspSrv::SetCurrentProcess: pid={:#x} not registered with FileSystemController",
                    pid,
                );
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(ResultCode::new(RESULT_TARGET_NOT_FOUND.raw()));
            }
        }
    }

    fn open_sd_card_file_system_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let _service = unsafe { &*(this as *const dyn ServiceFramework as *const FspSrv) };
        log::info!("FspSrv::OpenSdCardFileSystem called");
        Self::push_interface_response(ctx, Arc::new(IFileSystem::new()));
    }

    fn open_data_storage_by_current_process_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const FspSrv) };
        let program_id = *service.program_id.lock().unwrap();
        let pid = *service.current_process_id.lock().unwrap();

        log::info!(
            "FspSrv::OpenDataStorageByCurrentProcess called, current_process_id={:#x}, program_id={:#x}",
            pid,
            program_id
        );

        // Rust-side temporary adaptation: the upstream owner is RomFsController::OpenRomFSCurrentProcess,
        // but the service bootstrap here still lacks a System-backed RomFsController path.
        // Return a real IStorage session with an empty readable backend instead of a bare success.
        let backend: VirtualFile = Arc::new(VectorVfsFile::new(
            Vec::new(),
            format!("{program_id:016X}.romfs"),
            None,
        ));
        Self::push_interface_response(ctx, Arc::new(IStorage::new(backend)));
    }

    fn open_patch_data_storage_by_current_process_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const FspSrv) };
        let program_id = *service.program_id.lock().unwrap();

        log::warn!(
            "FspSrv::OpenPatchDataStorageByCurrentProcess called, program_id={:#x}; returning ResultTargetNotFound like upstream",
            program_id
        );

        // Upstream: this command has Out<SharedPointer<IStorage>> in its signature.
        // In domain mode, the response ALWAYS includes a domain object ID slot
        // (containing 0 for null) even on error, because the response layout is
        // computed from the method signature at compile time.
        // Without this, the game reads past the result expecting a domain object ID,
        // gets garbage, and eventually crashes.
        Self::push_error_with_null_interface(ctx, RESULT_TARGET_NOT_FOUND.raw());
    }

    fn set_global_access_log_mode_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const FspSrv) };
        let mode_raw = ctx.command_buffer()[ctx.get_data_payload_offset() as usize + 2];
        let mode = match mode_raw {
            1 => AccessLogMode::Log,
            2 => AccessLogMode::SdCard,
            _ => AccessLogMode::None,
        };
        *service.access_log_mode.lock().unwrap() = mode;
        log::info!("FspSrv::SetGlobalAccessLogMode called, mode={:?}", mode);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_global_access_log_mode_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const FspSrv) };
        let mode = *service.access_log_mode.lock().unwrap() as u32;
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(mode);
    }

    fn get_program_index_for_access_log_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const FspSrv) };
        let version = AccessLogVersion::LATEST as u32;
        let program_index = *service.access_log_program_index.lock().unwrap();

        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(version);
        rb.push_u32(program_index);
    }
}

impl SessionRequestHandler for FspSrv {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }

    fn service_name(&self) -> &str {
        "fsp-srv"
    }
}

impl ServiceFramework for FspSrv {
    fn get_service_name(&self) -> &str {
        "fsp-srv"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }

    fn invoke_request(&self, ctx: &mut HLERequestContext)
    where
        Self: Sized,
    {
        let cmd = ctx.get_command();
        if let Some(fi) = self.handlers().get(&cmd) {
            if let Some(callback) = fi.handler_callback {
                log::trace!("Service::{}: {}", self.get_service_name(), fi.name);
                callback(self, ctx);
                return;
            }
        }

        log::warn!(
            "FspSrv: unimplemented command '{}' returned stub success",
            cmd
        );
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}
