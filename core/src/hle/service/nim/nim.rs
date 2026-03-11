// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nim/nim.cpp
//!
//! NIM, NIM_ECA, NIM_SHP, and NTC services.

/// IPC command IDs for NIM_ECA
pub mod nim_eca_commands {
    pub const CREATE_SERVER_INTERFACE: u32 = 0;
    pub const REFRESH_DEBUG_AVAILABILITY: u32 = 1;
    pub const CLEAR_DEBUG_RESPONSE: u32 = 2;
    pub const REGISTER_DEBUG_RESPONSE: u32 = 3;
    pub const IS_LARGE_RESOURCE_AVAILABLE: u32 = 4;
}

/// IPC command IDs for NTC
pub mod ntc_commands {
    pub const OPEN_ENSURE_NETWORK_CLOCK_AVAILABILITY_SERVICE: u32 = 0;
    pub const SUSPEND_AUTONOMIC_TIME_CORRECTION: u32 = 100;
    pub const RESUME_AUTONOMIC_TIME_CORRECTION: u32 = 101;
}

/// IPC command IDs for IEnsureNetworkClockAvailabilityService
pub mod clock_availability_commands {
    pub const START_TASK: u32 = 0;
    pub const GET_FINISH_NOTIFICATION_EVENT: u32 = 1;
    pub const GET_RESULT: u32 = 2;
    pub const CANCEL: u32 = 3;
    pub const IS_PROCESSING: u32 = 4;
    pub const GET_SERVER_TIME: u32 = 5;
}

/// NIM service. All commands are unimplemented stubs.
pub struct NIM;
impl NIM {
    pub fn new() -> Self { Self }
}

/// IShopServiceAsync. All stubs.
pub struct IShopServiceAsync;
impl IShopServiceAsync {
    pub fn new() -> Self { Self }
}

/// IShopServiceAccessor.
pub struct IShopServiceAccessor;
impl IShopServiceAccessor {
    pub fn new() -> Self { Self }

    pub fn create_async_interface(&self) -> IShopServiceAsync {
        log::warn!("(STUBBED) IShopServiceAccessor::create_async_interface called");
        IShopServiceAsync::new()
    }
}

/// IShopServiceAccessServer.
pub struct IShopServiceAccessServer;
impl IShopServiceAccessServer {
    pub fn new() -> Self { Self }

    pub fn create_accessor_interface(&self) -> IShopServiceAccessor {
        log::warn!("(STUBBED) IShopServiceAccessServer::create_accessor_interface called");
        IShopServiceAccessor::new()
    }
}

/// NIM_ECA service ("nim:eca").
pub struct NimEca;
impl NimEca {
    pub fn new() -> Self { Self }

    pub fn create_server_interface(&self) -> IShopServiceAccessServer {
        log::warn!("(STUBBED) NimEca::create_server_interface called");
        IShopServiceAccessServer::new()
    }

    pub fn is_large_resource_available(&self, _unknown: u64) -> bool {
        log::info!("(STUBBED) NimEca::is_large_resource_available called");
        false
    }
}

/// NIM_SHP service ("nim:shp"). All stubs.
pub struct NimShp;
impl NimShp {
    pub fn new() -> Self { Self }
}

/// IEnsureNetworkClockAvailabilityService.
pub struct IEnsureNetworkClockAvailabilityService {
    // TODO: service_context, finished_event
}

impl IEnsureNetworkClockAvailabilityService {
    pub fn new() -> Self {
        Self {}
    }

    pub fn start_task(&self) {
        log::debug!("IEnsureNetworkClockAvailabilityService::start_task called");
        // Signal finished immediately
    }

    pub fn get_finish_notification_event(&self) {
        log::debug!("IEnsureNetworkClockAvailabilityService::get_finish_notification_event called");
        // TODO: return event handle
    }

    pub fn get_result(&self) {
        log::debug!("IEnsureNetworkClockAvailabilityService::get_result called");
    }

    pub fn cancel(&self) {
        log::debug!("IEnsureNetworkClockAvailabilityService::cancel called");
    }

    pub fn is_processing(&self) -> u32 {
        log::debug!("IEnsureNetworkClockAvailabilityService::is_processing called");
        0 // Instantly processed
    }

    pub fn get_server_time(&self) -> i64 {
        log::debug!("IEnsureNetworkClockAvailabilityService::get_server_time called");
        // Return current time as seconds since epoch
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs() as i64)
            .unwrap_or(0)
    }
}

/// NTC service ("ntc").
pub struct NTC;
impl NTC {
    pub fn new() -> Self { Self }

    pub fn open_ensure_network_clock_availability_service(
        &self,
    ) -> IEnsureNetworkClockAvailabilityService {
        log::debug!("NTC::open_ensure_network_clock_availability_service called");
        IEnsureNetworkClockAvailabilityService::new()
    }

    pub fn suspend_autonomic_time_correction(&self) {
        log::warn!("(STUBBED) NTC::suspend_autonomic_time_correction called");
    }

    pub fn resume_autonomic_time_correction(&self) {
        log::warn!("(STUBBED) NTC::resume_autonomic_time_correction called");
    }
}

/// Registers "nim", "nim:eca", "nim:shp", "ntc" services.
///
/// Corresponds to `LoopProcess` in upstream `nim.cpp`.
pub fn loop_process() {
    // TODO: register services with ServerManager
}
