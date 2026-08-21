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
    pub const REQUEST_DOWNLOAD_TICKET_FOR_PREPURCHASED_CONTENTS_2: u32 = 504;
    pub const REQUEST_DOWNLOAD_TICKET_FOR_PREPURCHASED_CONTENTS_FOR_ACCOUNT: u32 = 505;
}

/// NIM service ("nim"). All commands are nullptr entries in upstream.
///
/// Corresponds to `NIM` in upstream nim.cpp.
pub struct NIM {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl NIM {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (0, None, "CreateSystemUpdateTask"),
                (1, None, "DestroySystemUpdateTask"),
                (2, None, "ListSystemUpdateTask"),
                (3, None, "RequestSystemUpdateTaskRun"),
                (4, None, "GetSystemUpdateTaskInfo"),
                (5, None, "CommitSystemUpdateTask"),
                (6, None, "CreateNetworkInstallTask"),
                (7, None, "DestroyNetworkInstallTask"),
                (8, None, "ListNetworkInstallTask"),
                (9, None, "RequestNetworkInstallTaskRun"),
                (10, None, "GetNetworkInstallTaskInfo"),
                (11, None, "CommitNetworkInstallTask"),
                (12, None, "RequestLatestSystemUpdateMeta"),
                (14, None, "ListApplicationNetworkInstallTask"),
                (15, None, "ListNetworkInstallTaskContentMeta"),
                (16, None, "RequestLatestVersion"),
                (17, None, "SetNetworkInstallTaskAttribute"),
                (18, None, "AddNetworkInstallTaskContentMeta"),
                (19, None, "GetDownloadedSystemDataPath"),
                (20, None, "CalculateNetworkInstallTaskRequiredSize"),
                (21, None, "IsExFatDriverIncluded"),
                (22, None, "GetBackgroundDownloadStressTaskInfo"),
                (23, None, "RequestDeviceAuthenticationToken"),
                (24, None, "RequestGameCardRegistrationStatus"),
                (25, None, "RequestRegisterGameCard"),
                (26, None, "RequestRegisterNotificationToken"),
                (27, None, "RequestDownloadTaskList"),
                (28, None, "RequestApplicationControl"),
                (29, None, "RequestLatestApplicationControl"),
                (30, None, "RequestVersionList"),
                (31, None, "CreateApplyDeltaTask"),
                (32, None, "DestroyApplyDeltaTask"),
                (33, None, "ListApplicationApplyDeltaTask"),
                (34, None, "RequestApplyDeltaTaskRun"),
                (35, None, "GetApplyDeltaTaskInfo"),
                (36, None, "ListApplyDeltaTask"),
                (37, None, "CommitApplyDeltaTask"),
                (38, None, "CalculateApplyDeltaTaskRequiredSize"),
                (39, None, "PrepareShutdown"),
                (40, None, "ListApplyDeltaTask"),
                (41, None, "ClearNotEnoughSpaceStateOfApplyDeltaTask"),
                (42, None, "CreateApplyDeltaTaskFromDownloadTask"),
                (43, None, "GetBackgroundApplyDeltaStressTaskInfo"),
                (44, None, "GetApplyDeltaTaskRequiredStorage"),
                (45, None, "CalculateNetworkInstallTaskContentsSize"),
                (46, None, "PrepareShutdownForSystemUpdate"),
                (47, None, "FindMaxRequiredApplicationVersionOfTask"),
                (48, None, "CommitNetworkInstallTaskPartially"),
                (49, None, "ListNetworkInstallTaskCommittedContentMeta"),
                (50, None, "ListNetworkInstallTaskNotCommittedContentMeta"),
                (51, None, "FindMaxRequiredSystemVersionOfTask"),
                (52, None, "GetNetworkInstallTaskErrorContext"),
                (53, None, "CreateLocalCommunicationReceiveApplicationTask"),
                (54, None, "DestroyLocalCommunicationReceiveApplicationTask"),
                (55, None, "ListLocalCommunicationReceiveApplicationTask"),
                (
                    56,
                    None,
                    "RequestLocalCommunicationReceiveApplicationTaskRun",
                ),
                (57, None, "GetLocalCommunicationReceiveApplicationTaskInfo"),
                (58, None, "CommitLocalCommunicationReceiveApplicationTask"),
                (
                    59,
                    None,
                    "ListLocalCommunicationReceiveApplicationTaskContentMeta",
                ),
                (60, None, "CreateLocalCommunicationSendApplicationTask"),
                (61, None, "RequestLocalCommunicationSendApplicationTaskRun"),
                (
                    62,
                    None,
                    "GetLocalCommunicationReceiveApplicationTaskErrorContext",
                ),
                (63, None, "GetLocalCommunicationSendApplicationTaskInfo"),
                (64, None, "DestroyLocalCommunicationSendApplicationTask"),
                (
                    65,
                    None,
                    "GetLocalCommunicationSendApplicationTaskErrorContext",
                ),
                (
                    66,
                    None,
                    "CalculateLocalCommunicationReceiveApplicationTaskRequiredSize",
                ),
                (
                    67,
                    None,
                    "ListApplicationLocalCommunicationReceiveApplicationTask",
                ),
                (
                    68,
                    None,
                    "ListApplicationLocalCommunicationSendApplicationTask",
                ),
                (69, None, "CreateLocalCommunicationReceiveSystemUpdateTask"),
                (70, None, "DestroyLocalCommunicationReceiveSystemUpdateTask"),
                (71, None, "ListLocalCommunicationReceiveSystemUpdateTask"),
                (
                    72,
                    None,
                    "RequestLocalCommunicationReceiveSystemUpdateTaskRun",
                ),
                (73, None, "GetLocalCommunicationReceiveSystemUpdateTaskInfo"),
                (74, None, "CommitLocalCommunicationReceiveSystemUpdateTask"),
                (
                    75,
                    None,
                    "GetLocalCommunicationReceiveSystemUpdateTaskErrorContext",
                ),
                (76, None, "CreateLocalCommunicationSendSystemUpdateTask"),
                (77, None, "RequestLocalCommunicationSendSystemUpdateTaskRun"),
                (78, None, "GetLocalCommunicationSendSystemUpdateTaskInfo"),
                (79, None, "DestroyLocalCommunicationSendSystemUpdateTask"),
                (
                    80,
                    None,
                    "GetLocalCommunicationSendSystemUpdateTaskErrorContext",
                ),
                (81, None, "ListLocalCommunicationSendSystemUpdateTask"),
                (82, None, "GetReceivedSystemDataPath"),
                (83, None, "CalculateApplyDeltaTaskOccupiedSize"),
                (84, None, "ReloadErrorSimulation"),
                (85, None, "ListNetworkInstallTaskContentMetaFromInstallMeta"),
                (86, None, "ListNetworkInstallTaskOccupiedSize"),
                (87, None, "RequestQueryAvailableELicenses"),
                (88, None, "RequestAssignELicenses"),
                (89, None, "RequestExtendELicenses"),
                (90, None, "RequestSyncELicenses"),
                (91, None, "Unknown91"),
                (92, None, "Unknown92"),
                (93, None, "RequestReportActiveELicenses"),
                (94, None, "RequestReportActiveELicensesPassively"),
                (95, None, "RequestRegisterDynamicRightsNotificationToken"),
                (96, None, "RequestAssignAllDeviceLinkedELicenses"),
                (97, None, "RequestRevokeAllELicenses"),
                (98, None, "RequestPrefetchForDynamicRights"),
                (99, None, "CreateNetworkInstallTask"),
                (100, None, "ListNetworkInstallTaskRightsIds"),
                (101, None, "RequestDownloadETickets"),
                (102, None, "RequestQueryDownloadableContents"),
                (103, None, "DeleteNetworkInstallTaskContentMeta"),
                (104, None, "RequestIssueEdgeTokenForDebug"),
                (105, None, "RequestQueryAvailableELicenses2"),
                (106, None, "RequestAssignELicenses2"),
                (107, None, "GetNetworkInstallTaskStateCounter"),
                (108, None, "InvalidateDynamicRightsNaIdTokenCacheForDebug"),
                (109, None, "ListNetworkInstallTaskPartialInstallContentMeta"),
                (110, None, "ListNetworkInstallTaskRightsIdsFromIndex"),
                (111, None, "AddNetworkInstallTaskContentMetaForUser"),
                (112, None, "RequestAssignELicensesAndDownloadETickets"),
                (113, None, "RequestQueryAvailableCommonELicenses"),
                (114, None, "SetNetworkInstallTaskExtendedAttribute"),
                (115, None, "GetNetworkInstallTaskExtendedAttribute"),
                (116, None, "GetAllocatorInfo"),
                (117, None, "RequestQueryDownloadableContentsByApplicationId"),
                (118, None, "MarkNoDownloadRightsErrorResolved"),
                (119, None, "GetApplyDeltaTaskAllAppliedContentMeta"),
                (120, None, "PrioritizeNetworkInstallTask"),
                (121, None, "RequestQueryAvailableCommonELicenses2"),
                (122, None, "RequestAssignCommonELicenses"),
                (123, None, "RequestAssignCommonELicenses2"),
                (124, None, "IsNetworkInstallTaskFrontOfQueue"),
                (125, None, "PrioritizeApplyDeltaTask"),
                (126, None, "RerouteDownloadingPatch"),
                (127, None, "UnmarkNoDownloadRightsErrorResolved"),
                (128, None, "RequestContentsSize"),
                (129, None, "RequestContentsAuthorizationToken"),
                (130, None, "RequestCdnVendorDiscovery"),
                (131, None, "RefreshDebugAvailability"),
                (132, None, "ClearResponseSimulationEntry"),
                (133, None, "RegisterResponseSimulationEntry"),
                (134, None, "GetProcessedCdnVendors"),
                (135, None, "RefreshRuntimeBehaviorsForDebug"),
                (136, None, "RequestOnlineSubscriptionFreeTrialAvailability"),
                (137, None, "GetNetworkInstallTaskContentMetaCount"),
                (138, None, "RequestRevokeELicenses"),
                (139, None, "EnableNetworkConnectionToUseApplicationCore"),
                (140, None, "DisableNetworkConnectionToUseApplicationCore"),
                (141, None, "IsNetworkConnectionEnabledToUseApplicationCore"),
                (142, None, "RequestCheckSafeSystemVersion"),
                (143, None, "RequestApplicationIcon"),
                (144, None, "RequestDownloadIdbeIconFile"),
                (147, None, "Unknown147"),
                (148, None, "Unknown148"),
                (150, None, "Unknown150"),
                (151, None, "Unknown151"),
                (152, None, "Unknown152"),
                (153, None, "Unknown153"),
                (154, None, "Unknown154"),
                (155, None, "Unknown155"),
                (156, None, "Unknown156"),
                (157, None, "Unknown157"),
                (158, None, "Unknown158"),
                (159, None, "Unknown159"),
                (160, None, "Unknown160"),
                (161, None, "Unknown161"),
                (162, None, "Unknown162"),
                (163, None, "Unknown163"),
                (164, None, "Unknown164"),
                (165, None, "Unknown165"),
                (166, None, "Unknown166"),
                (167, None, "Unknown167"),
                (168, None, "Unknown168"),
                (169, None, "Unknown169"),
                (170, None, "Unknown170"),
                (171, None, "Unknown171"),
                (172, None, "Unknown172"),
                (173, None, "Unknown173"),
                (174, None, "Unknown174"),
                (175, None, "Unknown175"),
                (176, None, "Unknown176"),
                (177, None, "Unknown177"),
                (2000, None, "Unknown2000"),
                (2001, None, "Unknown2001"),
                (2002, None, "Unknown2002"),
                (2003, None, "Unknown2003"),
                (2004, None, "Unknown2004"),
                (2007, None, "Unknown2007"),
                (2011, None, "Unknown2011"),
                (2012, None, "Unknown2012"),
                (2013, None, "Unknown2013"),
                (2014, None, "Unknown2014"),
                (2015, None, "Unknown2015"),
                (2016, None, "Unknown2016"),
                (2017, None, "Unknown2017"),
                (2018, None, "Unknown2018"),
                (2019, None, "Unknown2019"),
                (2020, None, "Unknown2020"),
                (2021, None, "Unknown2021"),
                (2022, None, "Unknown2022"),
                (2023, None, "Unknown2023"),
                (2024, None, "Unknown2024"),
                (2025, None, "Unknown2025"),
                (2026, None, "Unknown2026"),
                (2027, None, "Unknown2027"),
                (2028, None, "Unknown2028"),
                (2029, None, "Unknown2029"),
                (2030, None, "Unknown2030"),
                (2031, None, "Unknown2031"),
                (2032, None, "Unknown2032"),
                (2033, None, "Unknown2033"),
                (2034, None, "Unknown2034"),
                (2035, None, "Unknown2035"),
                (2036, None, "Unknown2036"),
                (2037, None, "Unknown2037"),
                (2038, None, "Unknown2038"),
                (2039, None, "Unknown2039"),
                (2040, None, "Unknown2040"),
                (2041, None, "Unknown2041"),
                (2042, None, "Unknown2042"),
                (2043, None, "Unknown2043"),
                (2044, None, "Unknown2044"),
                (2045, None, "Unknown2045"),
                (2046, None, "Unknown2046"),
                (2047, None, "Unknown2047"),
                (2048, None, "Unknown2048"),
                (2049, None, "Unknown2049"),
                (2050, None, "Unknown2050"),
                (2051, None, "Unknown2051"),
                (3000, None, "RequestLatestApplicationIcon"),
                (3001, None, "RequestDownloadIdbeLatestIconFile"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for NIM {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "nim"
    }
}

impl ServiceFramework for NIM {
    fn get_service_name(&self) -> &str {
        "nim"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
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
                    Some(Self::create_server_interface_2_handler),
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

    /// CreateServerInterface2 (cmd 5).
    pub fn create_server_interface_2(&self) -> IShopServiceAccessServer {
        log::warn!("(STUBBED) NimEca::create_server_interface_2 called");
        IShopServiceAccessServer::new()
    }

    fn create_server_interface_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(Arc::new(this.create_server_interface()));
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

    fn create_server_interface_2_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(Arc::new(this.create_server_interface_2()));
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

/// `nim:ecas` special-client registration service.
pub struct NimEcas {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl NimEcas {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (0, None, "RegisterSpecialClient"),
                (1, None, "UnregisterSpecialClient"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for NimEcas {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "nim:ecas"
    }
}

impl ServiceFramework for NimEcas {
    fn get_service_name(&self) -> &str {
        "nim:ecas"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// NIM_SHP service ("nim:shp"). All commands are nullptr entries in upstream.
///
/// Corresponds to `NIM_SHP` in upstream nim.cpp.
pub struct NimShp {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl NimShp {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (
                    nim_shp_commands::REQUEST_DEVICE_AUTHENTICATION_TOKEN,
                    None,
                    "RequestDeviceAuthenticationToken",
                ),
                (
                    nim_shp_commands::REQUEST_CACHED_DEVICE_AUTHENTICATION_TOKEN,
                    None,
                    "RequestCachedDeviceAuthenticationToken",
                ),
                (
                    nim_shp_commands::REQUEST_EDGE_TOKEN,
                    None,
                    "RequestEdgeToken",
                ),
                (
                    nim_shp_commands::REQUEST_CACHED_EDGE_TOKEN,
                    None,
                    "RequestCachedEdgeToken",
                ),
                (
                    nim_shp_commands::REQUEST_REGISTER_DEVICE_ACCOUNT,
                    None,
                    "RequestRegisterDeviceAccount",
                ),
                (
                    nim_shp_commands::REQUEST_UNREGISTER_DEVICE_ACCOUNT,
                    None,
                    "RequestUnregisterDeviceAccount",
                ),
                (
                    nim_shp_commands::REQUEST_DEVICE_ACCOUNT_STATUS,
                    None,
                    "RequestDeviceAccountStatus",
                ),
                (
                    nim_shp_commands::GET_DEVICE_ACCOUNT_INFO,
                    None,
                    "GetDeviceAccountInfo",
                ),
                (
                    nim_shp_commands::REQUEST_DEVICE_REGISTRATION_INFO,
                    None,
                    "RequestDeviceRegistrationInfo",
                ),
                (
                    nim_shp_commands::REQUEST_TRANSFER_DEVICE_ACCOUNT,
                    None,
                    "RequestTransferDeviceAccount",
                ),
                (
                    nim_shp_commands::REQUEST_SYNC_REGISTRATION,
                    None,
                    "RequestSyncRegistration",
                ),
                (nim_shp_commands::IS_OWN_DEVICE_ID, None, "IsOwnDeviceId"),
                (
                    nim_shp_commands::REQUEST_REGISTER_NOTIFICATION_TOKEN,
                    None,
                    "RequestRegisterNotificationToken",
                ),
                (
                    nim_shp_commands::REQUEST_UNLINK_DEVICE,
                    None,
                    "RequestUnlinkDevice",
                ),
                (
                    nim_shp_commands::REQUEST_UNLINK_DEVICE_INTEGRATED,
                    None,
                    "RequestUnlinkDeviceIntegrated",
                ),
                (
                    nim_shp_commands::REQUEST_LINK_DEVICE,
                    None,
                    "RequestLinkDevice",
                ),
                (nim_shp_commands::HAS_DEVICE_LINK, None, "HasDeviceLink"),
                (
                    nim_shp_commands::REQUEST_UNLINK_DEVICE_ALL,
                    None,
                    "RequestUnlinkDeviceAll",
                ),
                (
                    nim_shp_commands::REQUEST_CREATE_VIRTUAL_ACCOUNT,
                    None,
                    "RequestCreateVirtualAccount",
                ),
                (
                    nim_shp_commands::REQUEST_DEVICE_LINK_STATUS,
                    None,
                    "RequestDeviceLinkStatus",
                ),
                (
                    nim_shp_commands::GET_ACCOUNT_BY_VIRTUAL_ACCOUNT,
                    None,
                    "GetAccountByVirtualAccount",
                ),
                (
                    nim_shp_commands::GET_VIRTUAL_ACCOUNT,
                    None,
                    "GetVirtualAccount",
                ),
                (
                    nim_shp_commands::REQUEST_SYNC_TICKET_LEGACY,
                    None,
                    "RequestSyncTicketLegacy",
                ),
                (
                    nim_shp_commands::REQUEST_DOWNLOAD_TICKET,
                    None,
                    "RequestDownloadTicket",
                ),
                (
                    nim_shp_commands::REQUEST_DOWNLOAD_TICKET_FOR_PREPURCHASED_CONTENTS,
                    None,
                    "RequestDownloadTicketForPrepurchasedContents",
                ),
                (
                    nim_shp_commands::REQUEST_SYNC_TICKET,
                    None,
                    "RequestSyncTicket",
                ),
                (
                    nim_shp_commands::REQUEST_DOWNLOAD_TICKET_FOR_PREPURCHASED_CONTENTS_2,
                    None,
                    "RequestDownloadTicketForPrepurchasedContents2",
                ),
                (
                    nim_shp_commands::REQUEST_DOWNLOAD_TICKET_FOR_PREPURCHASED_CONTENTS_FOR_ACCOUNT,
                    None,
                    "RequestDownloadTicketForPrepurchasedContentsForAccount",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for NimShp {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "nim:shp"
    }
}

impl ServiceFramework for NimShp {
    fn get_service_name(&self) -> &str {
        "nim:shp"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// IEnsureNetworkClockAvailabilityService.
///
/// Corresponds to `IEnsureNetworkClockAvailabilityService` in upstream nim.cpp.
pub struct IEnsureNetworkClockAvailabilityService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
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
            handlers: build_handler_map(&[
                (
                    clock_availability_commands::START_TASK,
                    Some(Self::start_task_handler),
                    "StartTask",
                ),
                (
                    clock_availability_commands::GET_FINISH_NOTIFICATION_EVENT,
                    Some(Self::get_finish_notification_event_handler),
                    "GetFinishNotificationEvent",
                ),
                (
                    clock_availability_commands::GET_RESULT,
                    Some(Self::get_result_handler),
                    "GetResult",
                ),
                (
                    clock_availability_commands::CANCEL,
                    Some(Self::cancel_handler),
                    "Cancel",
                ),
                (
                    clock_availability_commands::IS_PROCESSING,
                    Some(Self::is_processing_handler),
                    "IsProcessing",
                ),
                (
                    clock_availability_commands::GET_SERVER_TIME,
                    Some(Self::get_server_time_handler),
                    "GetServerTime",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
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
        match std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH) {
            Ok(duration) => duration.as_secs() as i64,
            Err(error) => -(error.duration().as_secs() as i64),
        }
    }

    fn start_task_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        this.start_task();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_finish_notification_event_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let handle = this
            .service_context
            .get_event(this.get_finish_notification_event())
            .and_then(|event| event.copy_handle(ctx))
            .unwrap_or(0);
        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(handle);
    }

    fn get_result_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        this.get_result();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn cancel_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        this.cancel();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn is_processing_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let processing = this.is_processing();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(processing);
    }

    fn get_server_time_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let server_time = this.get_server_time();
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(server_time as u64);
    }
}

impl Drop for IEnsureNetworkClockAvailabilityService {
    fn drop(&mut self) {
        self.service_context.close_event(self.finished_event_handle);
    }
}

impl SessionRequestHandler for IEnsureNetworkClockAvailabilityService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "IEnsureNetworkClockAvailabilityService"
    }
}

impl ServiceFramework for IEnsureNetworkClockAvailabilityService {
    fn get_service_name(&self) -> &str {
        "IEnsureNetworkClockAvailabilityService"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// NTC service ("ntc").
///
/// Corresponds to `NTC` in upstream nim.cpp.
pub struct NTC {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl NTC {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (
                    ntc_commands::OPEN_ENSURE_NETWORK_CLOCK_AVAILABILITY_SERVICE,
                    Some(Self::open_ensure_network_clock_availability_service_handler),
                    "OpenEnsureNetworkClockAvailabilityService",
                ),
                (
                    ntc_commands::SUSPEND_AUTONOMIC_TIME_CORRECTION,
                    Some(Self::suspend_autonomic_time_correction_handler),
                    "SuspendAutonomicTimeCorrection",
                ),
                (
                    ntc_commands::RESUME_AUTONOMIC_TIME_CORRECTION,
                    Some(Self::resume_autonomic_time_correction_handler),
                    "ResumeAutonomicTimeCorrection",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
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

    fn open_ensure_network_clock_availability_service_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(Arc::new(
            this.open_ensure_network_clock_availability_service(),
        ));
    }

    fn suspend_autonomic_time_correction_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        this.suspend_autonomic_time_correction();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn resume_autonomic_time_correction_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        this.resume_autonomic_time_correction();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for NTC {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ntc"
    }
}

impl ServiceFramework for NTC {
    fn get_service_name(&self) -> &str {
        "ntc"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// Registers "nim", "nim:eca", "nim:shp", and "ntc" services.
///
/// Corresponds to `LoopProcess` in upstream nim.cpp.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::server_manager::ServerManager;

    let server_manager = ServerManager::new_shared(system);

    {
        let mut server_manager = server_manager.lock().unwrap();
        server_manager.register_named_service(
            "nim",
            Box::new(|| -> SessionRequestHandlerPtr { Arc::new(NIM::new()) }),
            64,
        );
        server_manager.register_named_service(
            "nim:eca",
            Box::new(|| -> SessionRequestHandlerPtr { Arc::new(NimEca::new()) }),
            64,
        );
        server_manager.register_named_service(
            "nim:ecas",
            Box::new(|| -> SessionRequestHandlerPtr { Arc::new(NimEcas::new()) }),
            64,
        );
        server_manager.register_named_service(
            "nim:shp",
            Box::new(|| -> SessionRequestHandlerPtr { Arc::new(NimShp::new()) }),
            64,
        );
        server_manager.register_named_service(
            "ntc",
            Box::new(|| -> SessionRequestHandlerPtr { Arc::new(NTC::new()) }),
            64,
        );
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

    #[test]
    fn ecas_table_matches_upstream() {
        assert_eq!(
            NimEcas::new()
                .handlers()
                .keys()
                .copied()
                .collect::<Vec<_>>(),
            [0, 1]
        );
    }

    #[test]
    fn nim_service_tables_match_a41_upstream() {
        assert_eq!(NIM::new().handlers().len(), 223);
        assert_eq!(IShopServiceAsync::new().handlers().len(), 6);
        assert_eq!(IShopServiceAccessor::new().handlers().len(), 1);
        assert_eq!(IShopServiceAccessServer::new().handlers().len(), 1);
        assert_eq!(NimEca::new().handlers().len(), 6);
        assert_eq!(NimShp::new().handlers().len(), 28);
        assert_eq!(
            IEnsureNetworkClockAvailabilityService::new()
                .handlers()
                .len(),
            6
        );
        assert_eq!(NTC::new().handlers().len(), 3);
    }
}
