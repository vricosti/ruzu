// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nim/nim.cpp
//!
//! NIM, NIM_ECA, NIM_SHP, NTC, and related sub-services.

/// IPC command IDs for IShopServiceAsync.
///
/// Corresponds to the function table in `IShopServiceAsync` constructor (upstream nim.cpp).
pub mod shop_async_commands {
    pub const CANCEL: u32 = 0;
    pub const GET_SIZE: u32 = 1;
    pub const READ: u32 = 2;
    pub const GET_ERROR_CODE: u32 = 3;
    pub const REQUEST: u32 = 4;
    pub const PREPARE: u32 = 5;
}

/// IPC command IDs for IShopServiceAccessor.
///
/// Corresponds to the function table in `IShopServiceAccessor` constructor (upstream nim.cpp).
pub mod shop_accessor_commands {
    pub const CREATE_ASYNC_INTERFACE: u32 = 0;
}

/// IPC command IDs for IShopServiceAccessServer.
///
/// Corresponds to the function table in `IShopServiceAccessServer` constructor (upstream nim.cpp).
pub mod shop_access_server_commands {
    pub const CREATE_ACCESSOR_INTERFACE: u32 = 0;
}

/// IPC command IDs for NIM_ECA ("nim:eca").
///
/// Corresponds to the function table in `NIM_ECA` constructor (upstream nim.cpp).
pub mod nim_eca_commands {
    pub const CREATE_SERVER_INTERFACE: u32 = 0;
    pub const REFRESH_DEBUG_AVAILABILITY: u32 = 1;
    pub const CLEAR_DEBUG_RESPONSE: u32 = 2;
    pub const REGISTER_DEBUG_RESPONSE: u32 = 3;
    pub const IS_LARGE_RESOURCE_AVAILABLE: u32 = 4;
}

/// IPC command IDs for NTC ("ntc").
///
/// Corresponds to the function table in `NTC` constructor (upstream nim.cpp).
pub mod ntc_commands {
    pub const OPEN_ENSURE_NETWORK_CLOCK_AVAILABILITY_SERVICE: u32 = 0;
    pub const SUSPEND_AUTONOMIC_TIME_CORRECTION: u32 = 100;
    pub const RESUME_AUTONOMIC_TIME_CORRECTION: u32 = 101;
}

/// IPC command IDs for IEnsureNetworkClockAvailabilityService.
///
/// Corresponds to the function table in `IEnsureNetworkClockAvailabilityService` constructor.
pub mod clock_availability_commands {
    pub const START_TASK: u32 = 0;
    pub const GET_FINISH_NOTIFICATION_EVENT: u32 = 1;
    pub const GET_RESULT: u32 = 2;
    pub const CANCEL: u32 = 3;
    pub const IS_PROCESSING: u32 = 4;
    pub const GET_SERVER_TIME: u32 = 5;
}

/// IPC command IDs for NIM_SHP ("nim:shp") -- selected entries.
///
/// Corresponds to the function table in `NIM_SHP` constructor (upstream nim.cpp).
pub mod nim_shp_commands {
    pub const REQUEST_DEVICE_AUTHENTICATION_TOKEN: u32 = 0;
    pub const REQUEST_CACHED_DEVICE_AUTHENTICATION_TOKEN: u32 = 1;
    pub const REQUEST_EDGE_TOKEN: u32 = 2;
    pub const REQUEST_CACHED_EDGE_TOKEN: u32 = 3;
    pub const REQUEST_REGISTER_DEVICE_ACCOUNT: u32 = 100;
    pub const REQUEST_UNREGISTER_DEVICE_ACCOUNT: u32 = 101;
    pub const REQUEST_DEVICE_ACCOUNT_STATUS: u32 = 102;
    pub const GET_DEVICE_ACCOUNT_INFO: u32 = 103;
    pub const REQUEST_DEVICE_REGISTRATION_INFO: u32 = 104;
    pub const REQUEST_TRANSFER_DEVICE_ACCOUNT: u32 = 105;
    pub const REQUEST_SYNC_REGISTRATION: u32 = 106;
    pub const IS_OWN_DEVICE_ID: u32 = 107;
    pub const REQUEST_REGISTER_NOTIFICATION_TOKEN: u32 = 200;
    pub const REQUEST_UNLINK_DEVICE: u32 = 300;
    pub const REQUEST_UNLINK_DEVICE_INTEGRATED: u32 = 301;
    pub const REQUEST_LINK_DEVICE: u32 = 302;
    pub const HAS_DEVICE_LINK: u32 = 303;
    pub const REQUEST_UNLINK_DEVICE_ALL: u32 = 304;
    pub const REQUEST_CREATE_VIRTUAL_ACCOUNT: u32 = 305;
    pub const REQUEST_DEVICE_LINK_STATUS: u32 = 306;
    pub const GET_ACCOUNT_BY_VIRTUAL_ACCOUNT: u32 = 400;
    pub const GET_VIRTUAL_ACCOUNT: u32 = 401;
    pub const REQUEST_SYNC_TICKET_LEGACY: u32 = 500;
    pub const REQUEST_DOWNLOAD_TICKET: u32 = 501;
    pub const REQUEST_DOWNLOAD_TICKET_FOR_PREPURCHASED_CONTENTS: u32 = 502;
    pub const REQUEST_SYNC_TICKET: u32 = 503;
}

/// NIM service ("nim"). All commands are nullptr stubs in upstream.
///
/// Corresponds to `NIM` in upstream nim.cpp.
pub struct NIM;

impl NIM {
    pub fn new() -> Self {
        Self
    }
}

/// IShopServiceAsync -- all commands nullptr in upstream.
///
/// Corresponds to `IShopServiceAsync` in upstream nim.cpp.
pub struct IShopServiceAsync;

impl IShopServiceAsync {
    pub fn new() -> Self {
        Self
    }
}

/// IShopServiceAccessor.
///
/// Corresponds to `IShopServiceAccessor` in upstream nim.cpp.
pub struct IShopServiceAccessor;

impl IShopServiceAccessor {
    pub fn new() -> Self {
        Self
    }

    /// CreateAsyncInterface (cmd 0).
    ///
    /// Corresponds to `IShopServiceAccessor::CreateAsyncInterface` in upstream nim.cpp.
    pub fn create_async_interface(&self) -> IShopServiceAsync {
        log::warn!("(STUBBED) IShopServiceAccessor::create_async_interface called");
        IShopServiceAsync::new()
    }
}

/// IShopServiceAccessServer.
///
/// Corresponds to `IShopServiceAccessServer` in upstream nim.cpp.
pub struct IShopServiceAccessServer;

impl IShopServiceAccessServer {
    pub fn new() -> Self {
        Self
    }

    /// CreateAccessorInterface (cmd 0).
    ///
    /// Corresponds to `IShopServiceAccessServer::CreateAccessorInterface` in upstream nim.cpp.
    pub fn create_accessor_interface(&self) -> IShopServiceAccessor {
        log::warn!("(STUBBED) IShopServiceAccessServer::create_accessor_interface called");
        IShopServiceAccessor::new()
    }
}

/// NIM_ECA service ("nim:eca").
///
/// Corresponds to `NIM_ECA` in upstream nim.cpp.
pub struct NimEca;

impl NimEca {
    pub fn new() -> Self {
        Self
    }

    /// CreateServerInterface (cmd 0).
    ///
    /// Corresponds to `NIM_ECA::CreateServerInterface` in upstream nim.cpp.
    pub fn create_server_interface(&self) -> IShopServiceAccessServer {
        log::warn!("(STUBBED) NimEca::create_server_interface called");
        IShopServiceAccessServer::new()
    }

    /// IsLargeResourceAvailable (cmd 4).
    ///
    /// Corresponds to `NIM_ECA::IsLargeResourceAvailable` in upstream nim.cpp.
    pub fn is_large_resource_available(&self, unknown: u64) -> bool {
        log::info!(
            "(STUBBED) NimEca::is_large_resource_available called, unknown={}",
            unknown
        );
        false
    }
}

/// NIM_SHP service ("nim:shp"). All commands are nullptr stubs in upstream.
///
/// Corresponds to `NIM_SHP` in upstream nim.cpp.
pub struct NimShp;

impl NimShp {
    pub fn new() -> Self {
        Self
    }
}

/// IEnsureNetworkClockAvailabilityService.
///
/// Corresponds to `IEnsureNetworkClockAvailabilityService` in upstream nim.cpp.
pub struct IEnsureNetworkClockAvailabilityService {
    service_context: crate::hle::service::kernel_helpers::ServiceContext,
    finished_event_handle: u32,
}

impl IEnsureNetworkClockAvailabilityService {
    pub fn new() -> Self {
        let mut service_context = crate::hle::service::kernel_helpers::ServiceContext::new(
            "IEnsureNetworkClockAvailabilityService".to_string(),
        );
        let finished_event_handle = service_context
            .create_event("IEnsureNetworkClockAvailabilityService:FinishEvent".to_string());
        Self {
            service_context,
            finished_event_handle,
        }
    }

    /// StartTask (cmd 0).
    ///
    /// Corresponds to upstream: signals finished_event immediately (no network needed).
    pub fn start_task(&self) {
        log::debug!("IEnsureNetworkClockAvailabilityService::start_task called");
        // No need to connect to the internet, just finish the task straight away.
        if let Some(event) = self.service_context.get_event(self.finished_event_handle) {
            event.signal();
        }
    }

    /// GetFinishNotificationEvent (cmd 1).
    ///
    /// Returns the finished_event's readable event handle.
    pub fn get_finish_notification_event(&self) -> u32 {
        log::debug!("IEnsureNetworkClockAvailabilityService::get_finish_notification_event called");
        self.finished_event_handle
    }

    /// GetResult (cmd 2).
    pub fn get_result(&self) {
        log::debug!("IEnsureNetworkClockAvailabilityService::get_result called");
    }

    /// Cancel (cmd 3).
    ///
    /// Upstream clears the finished_event.
    pub fn cancel(&self) {
        log::debug!("IEnsureNetworkClockAvailabilityService::cancel called");
        if let Some(event) = self.service_context.get_event(self.finished_event_handle) {
            event.clear();
        }
    }

    /// IsProcessing (cmd 4).
    ///
    /// Returns 0 because we instantly process the request.
    pub fn is_processing(&self) -> u32 {
        log::debug!("IEnsureNetworkClockAvailabilityService::is_processing called");
        0
    }

    /// GetServerTime (cmd 5).
    ///
    /// Returns the current system time as seconds since the Unix epoch.
    pub fn get_server_time(&self) -> i64 {
        log::debug!("IEnsureNetworkClockAvailabilityService::get_server_time called");
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs() as i64)
            .unwrap_or(0)
    }
}

/// NTC service ("ntc").
///
/// Corresponds to `NTC` in upstream nim.cpp.
pub struct NTC;

impl NTC {
    pub fn new() -> Self {
        Self
    }

    /// OpenEnsureNetworkClockAvailabilityService (cmd 0).
    ///
    /// Corresponds to `NTC::OpenEnsureNetworkClockAvailabilityService` in upstream nim.cpp.
    pub fn open_ensure_network_clock_availability_service(
        &self,
    ) -> IEnsureNetworkClockAvailabilityService {
        log::debug!("NTC::open_ensure_network_clock_availability_service called");
        IEnsureNetworkClockAvailabilityService::new()
    }

    /// SuspendAutonomicTimeCorrection (cmd 100).
    ///
    /// Corresponds to `NTC::SuspendAutonomicTimeCorrection` in upstream nim.cpp.
    pub fn suspend_autonomic_time_correction(&self) {
        log::warn!("(STUBBED) NTC::suspend_autonomic_time_correction called");
    }

    /// ResumeAutonomicTimeCorrection (cmd 101).
    ///
    /// Corresponds to `NTC::ResumeAutonomicTimeCorrection` in upstream nim.cpp.
    pub fn resume_autonomic_time_correction(&self) {
        log::warn!("(STUBBED) NTC::resume_autonomic_time_correction called");
    }
}

/// Registers "nim:shp" service.
///
/// Corresponds to `LoopProcess` in upstream nim.cpp.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::server_manager::ServerManager;

    let mut server_manager = ServerManager::new(system);

    let stub = |sm: &mut ServerManager, name: &str| {
        let svc_name = name.to_string();
        sm.register_named_service(
            name,
            Box::new(move || -> SessionRequestHandlerPtr {
                std::sync::Arc::new(crate::hle::service::services::GenericStubService::new(
                    &svc_name,
                ))
            }),
            64,
        );
    };
    stub(&mut server_manager, "nim:shp");
    ServerManager::run_server(server_manager);
}
