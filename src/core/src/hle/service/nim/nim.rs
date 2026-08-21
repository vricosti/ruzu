// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nim/nim.cpp
//!
//! NIM, NIM_ECA, NIM_SHP, NTC, and related sub-services.

use std::collections::BTreeMap;
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use std::sync::{Arc, Mutex};
use std::thread::JoinHandle;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::kernel_helpers::ServiceContext;
use crate::hle::service::os::event::Event;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

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
    pub const CREATE_SERVER_INTERFACE_2: u32 = 5;
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

struct ShopWorker {
    stop_requested: Arc<AtomicBool>,
    handle: JoinHandle<()>,
}

/// IShopServiceAsync.
///
/// Corresponds to `IShopServiceAsync` in upstream nim.cpp.
pub struct IShopServiceAsync {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    service_context: ServiceContext,
    completion_event_handle: u32,
    worker: Mutex<Option<ShopWorker>>,
    error_code: Arc<AtomicU32>,
    download_data: Arc<Mutex<Vec<u8>>>,
}

impl IShopServiceAsync {
    pub fn new() -> Self {
        let mut service_context = ServiceContext::new("IShopServiceAsync".to_string());
        let completion_event_handle =
            service_context.create_event("IShopServiceAsync:Completion".to_string());
        Self {
            handlers: build_handler_map(&[
                (
                    shop_async_commands::CANCEL,
                    Some(Self::cancel_handler),
                    "Cancel",
                ),
                (
                    shop_async_commands::GET_SIZE,
                    Some(Self::get_size_handler),
                    "GetSize",
                ),
                (shop_async_commands::READ, Some(Self::read_handler), "Read"),
                (
                    shop_async_commands::GET_ERROR_CODE,
                    Some(Self::get_error_code_handler),
                    "GetErrorCode",
                ),
                (
                    shop_async_commands::REQUEST,
                    Some(Self::request_handler),
                    "Request",
                ),
                (
                    shop_async_commands::PREPARE,
                    Some(Self::prepare_handler),
                    "Prepare",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
            service_context,
            completion_event_handle,
            worker: Mutex::new(None),
            error_code: Arc::new(AtomicU32::new(0)),
            download_data: Arc::new(Mutex::new(Vec::new())),
        }
    }

    fn completion_event(&self) -> Option<Arc<Event>> {
        self.service_context.get_event(self.completion_event_handle)
    }

    fn cancel_impl(&self) {
        let worker = self.worker.lock().unwrap().take();
        if let Some(worker) = worker {
            worker.stop_requested.store(true, Ordering::Release);
            let _ = worker.handle.join();
        }
    }

    fn cancel(&self) {
        log::debug!("IShopServiceAsync::Cancel called");
        self.cancel_impl();
    }

    fn get_size(&self) -> u64 {
        log::debug!("IShopServiceAsync::GetSize called");
        self.download_data.lock().unwrap().len() as u64
    }

    fn read(&self, offset: u64, output_size: usize) -> Vec<u8> {
        let data = self.download_data.lock().unwrap();
        let Ok(offset) = usize::try_from(offset) else {
            return Vec::new();
        };
        if offset >= data.len() {
            return Vec::new();
        }
        let actual_read = output_size.min(data.len() - offset);
        data[offset..offset + actual_read].to_vec()
    }

    fn get_error_code(&self) -> u32 {
        log::debug!("IShopServiceAsync::GetErrorCode called");
        self.error_code.load(Ordering::Acquire)
    }

    fn request(&self) {
        log::debug!("(STUBBED) IShopServiceAsync::Request called");
        self.cancel_impl();

        self.error_code.store(0, Ordering::Release);
        if let Some(event) = self.completion_event() {
            event.clear();
        }
        self.download_data.lock().unwrap().clear();

        let stop_requested = Arc::new(AtomicBool::new(false));
        let thread_stop_requested = Arc::clone(&stop_requested);
        let error_code = Arc::clone(&self.error_code);
        let download_data = Arc::clone(&self.download_data);
        let completion_event = self.completion_event();
        let handle = std::thread::spawn(move || {
            if thread_stop_requested.load(Ordering::Acquire) {
                error_code.store(1, Ordering::Release);
            } else {
                // Dummy JSON response, else the caller rejects the request.
                *download_data.lock().unwrap() = b"{}".to_vec();
                error_code.store(0, Ordering::Release);
            }
            if let Some(event) = completion_event {
                event.signal();
            }
        });
        *self.worker.lock().unwrap() = Some(ShopWorker {
            stop_requested,
            handle,
        });
    }

    fn prepare(&self, path: &[u8], _post: &[u8]) {
        log::debug!("IShopServiceAsync::Prepare called");
        if !path.is_empty() {
            log::info!(
                "IShopServiceAsync: preparing request for URL: {}",
                String::from_utf8_lossy(path)
            );
        }
    }

    fn cancel_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        this.cancel();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_size_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let size = this.get_size();
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(size);
    }

    fn read_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rp = RequestParser::new(ctx);
        let offset = rp.pop_u64();
        let data = this.read(offset, ctx.get_write_buffer_size(0));
        let actual_read = ctx.write_buffer(&data, 0) as u64;
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(actual_read);
    }

    fn get_error_code_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let error_code = this.get_error_code();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(error_code);
    }

    fn request_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        this.request();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn prepare_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let path = ctx.read_buffer(0);
        let post = ctx.read_buffer(1);
        this.prepare(&path, &post);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl Drop for IShopServiceAsync {
    fn drop(&mut self) {
        self.cancel_impl();
        self.service_context
            .close_event(self.completion_event_handle);
    }
}

impl SessionRequestHandler for IShopServiceAsync {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "IShopServiceAsync"
    }
}

impl ServiceFramework for IShopServiceAsync {
    fn get_service_name(&self) -> &str {
        "IShopServiceAsync"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// IShopServiceAccessor.
///
/// Corresponds to `IShopServiceAccessor` in upstream nim.cpp.
pub struct IShopServiceAccessor {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IShopServiceAccessor {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[(
                shop_accessor_commands::CREATE_ASYNC_INTERFACE,
                Some(Self::create_async_interface_handler),
                "CreateAsyncInterface",
            )]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// CreateAsyncInterface (cmd 0).
    ///
    /// Corresponds to `IShopServiceAccessor::CreateAsyncInterface` in upstream nim.cpp.
    pub fn create_async_interface(&self) -> IShopServiceAsync {
        log::debug!("IShopServiceAccessor::CreateAsyncInterface called");
        IShopServiceAsync::new()
    }

    fn create_async_interface_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let async_interface = Arc::new(this.create_async_interface());
        let event_handle = async_interface
            .completion_event()
            .and_then(|event| event.copy_handle(ctx))
            .unwrap_or(0);

        let mut rb = ResponseBuilder::new(ctx, 2, 1, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(event_handle);
        rb.push_ipc_interface(async_interface);
    }
}

impl SessionRequestHandler for IShopServiceAccessor {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "IShopServiceAccessor"
    }
}

impl ServiceFramework for IShopServiceAccessor {
    fn get_service_name(&self) -> &str {
        "IShopServiceAccessor"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// IShopServiceAccessServer.
///
/// Corresponds to `IShopServiceAccessServer` in upstream nim.cpp.
pub struct IShopServiceAccessServer {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IShopServiceAccessServer {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[(
                shop_access_server_commands::CREATE_ACCESSOR_INTERFACE,
                Some(Self::create_accessor_interface_handler),
                "CreateAccessorInterface",
            )]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// CreateAccessorInterface (cmd 0).
    ///
    /// Corresponds to `IShopServiceAccessServer::CreateAccessorInterface` in upstream nim.cpp.
    pub fn create_accessor_interface(&self) -> IShopServiceAccessor {
        log::warn!("(STUBBED) IShopServiceAccessServer::create_accessor_interface called");
        IShopServiceAccessor::new()
    }

    fn create_accessor_interface_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let interface = Arc::new(this.create_accessor_interface());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(interface);
    }
}

impl SessionRequestHandler for IShopServiceAccessServer {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "IShopServiceAccessServer"
    }
}

impl ServiceFramework for IShopServiceAccessServer {
    fn get_service_name(&self) -> &str {
        "IShopServiceAccessServer"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// NIM_ECA service ("nim:eca").
///
/// Corresponds to `NIM_ECA` in upstream nim.cpp.
pub struct NimEca {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl NimEca {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (
                    nim_eca_commands::CREATE_SERVER_INTERFACE,
                    Some(Self::create_server_interface_handler),
                    "CreateServerInterface",
                ),
                (
                    nim_eca_commands::REFRESH_DEBUG_AVAILABILITY,
                    None,
                    "RefreshDebugAvailability",
                ),
                (
                    nim_eca_commands::CLEAR_DEBUG_RESPONSE,
                    None,
                    "ClearDebugResponse",
                ),
                (
                    nim_eca_commands::REGISTER_DEBUG_RESPONSE,
                    None,
                    "RegisterDebugResponse",
                ),
                (
                    nim_eca_commands::IS_LARGE_RESOURCE_AVAILABLE,
                    Some(Self::is_large_resource_available_handler),
                    "IsLargeResourceAvailable",
                ),
                (
                    nim_eca_commands::CREATE_SERVER_INTERFACE_2,
                    Some(Self::create_server_interface_handler),
                    "CreateServerInterface2",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
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

    fn create_server_interface_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let interface = Arc::new(this.create_server_interface());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(interface);
    }

    fn is_large_resource_available_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rp = RequestParser::new(ctx);
        let unknown = rp.pop_u64();
        let available = this.is_large_resource_available(unknown);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(available);
    }
}

impl SessionRequestHandler for NimEca {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "nim:eca"
    }
}

impl ServiceFramework for NimEca {
    fn get_service_name(&self) -> &str {
        "nim:eca"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
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

/// Registers "nim", "nim:eca", "nim:shp", and "ntc" services.
///
/// Corresponds to `LoopProcess` in upstream nim.cpp.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::server_manager::ServerManager;

    let server_manager = ServerManager::new_shared(system);

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
    {
        let mut server_manager = server_manager.lock().unwrap();
        stub(&mut server_manager, "nim");
        server_manager.register_named_service(
            "nim:eca",
            Box::new(|| -> SessionRequestHandlerPtr { Arc::new(NimEca::new()) }),
            64,
        );
        stub(&mut server_manager, "nim:shp");
        stub(&mut server_manager, "ntc");
    }
    ServerManager::run_server_shared(server_manager);
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use super::*;

    #[test]
    fn shop_async_table_matches_upstream() {
        let service = IShopServiceAsync::new();
        assert_eq!(
            service.handlers().keys().copied().collect::<Vec<_>>(),
            [0, 1, 2, 3, 4, 5]
        );
        assert_eq!(service.service_name(), "IShopServiceAsync");
    }

    #[test]
    fn request_completes_with_dummy_json_and_signals_event() {
        let service = IShopServiceAsync::new();
        let completion_event = service.completion_event().unwrap();

        service.request();

        assert!(completion_event.wait_timeout(Duration::from_secs(1)));
        service.cancel_impl();
        assert_eq!(service.get_error_code(), 0);
        assert_eq!(service.get_size(), 2);
        assert_eq!(service.read(0, 8), b"{}");
        assert_eq!(service.read(1, 8), b"}");
        assert!(service.read(2, 8).is_empty());
    }

    #[test]
    fn request_resets_previous_data_and_completion_event() {
        let service = IShopServiceAsync::new();
        let completion_event = service.completion_event().unwrap();

        service.request();
        assert!(completion_event.wait_timeout(Duration::from_secs(1)));
        service.cancel_impl();
        service
            .download_data
            .lock()
            .unwrap()
            .extend_from_slice(b"old");

        service.request();
        assert!(completion_event.wait_timeout(Duration::from_secs(1)));
        service.cancel_impl();
        assert_eq!(&*service.download_data.lock().unwrap(), b"{}");
    }

    #[test]
    fn accessor_chain_tables_expose_async_interface() {
        assert_eq!(
            IShopServiceAccessor::new()
                .handlers()
                .keys()
                .copied()
                .collect::<Vec<_>>(),
            [0]
        );
        assert_eq!(
            IShopServiceAccessServer::new()
                .handlers()
                .keys()
                .copied()
                .collect::<Vec<_>>(),
            [0]
        );
        assert_eq!(
            NimEca::new().handlers().keys().copied().collect::<Vec<_>>(),
            [0, 1, 2, 3, 4, 5]
        );
    }
}
