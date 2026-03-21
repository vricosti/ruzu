// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/application_accessor.h
//! Port of zuyu/src/core/hle/service/am/service/application_accessor.cpp

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IApplicationAccessor:
/// - 0: GetAppletStateChangedEvent
/// - 1: IsCompleted (unimplemented)
/// - 10: Start
/// - 20: RequestExit
/// - 25: Terminate
/// - 30: GetResult
/// - 101: RequestForApplicationToGetForeground
/// - 110: TerminateAllLibraryApplets (unimplemented)
/// - 111: AreAnyLibraryAppletsLeft (unimplemented)
/// - 112: GetCurrentLibraryApplet
/// - 120: GetApplicationId (unimplemented)
/// - 121: PushLaunchParameter
/// - 122: GetApplicationControlProperty
/// - 123: GetApplicationLaunchProperty (unimplemented)
/// - 124: GetApplicationLaunchRequestInfo (unimplemented)
/// - 130: SetUsers
/// - 131: CheckRightsEnvironmentAvailable
/// - 132: GetNsRightsEnvironmentHandle
/// - 140: GetDesirableUids (unimplemented)
/// - 150: ReportApplicationExitTimeout
/// - 160: SetApplicationAttribute (unimplemented)
/// - 170: HasSaveDataAccessPermission (unimplemented)
/// - 180: PushToFriendInvitationStorageChannel (unimplemented)
/// - 190: PushToNotificationStorageChannel (unimplemented)
/// - 200: RequestApplicationSoftReset (unimplemented)
/// - 201: RestartApplicationTimer (unimplemented)
pub struct IApplicationAccessor {
    // TODO: WindowSystem reference, Applet reference
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IApplicationAccessor {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, None, "GetAppletStateChangedEvent"),
            (1, None, "IsCompleted"),
            (10, Some(Self::start_handler), "Start"),
            (20, Some(Self::request_exit_handler), "RequestExit"),
            (25, Some(Self::terminate_handler), "Terminate"),
            (30, None, "GetResult"),
            (101, None, "RequestForApplicationToGetForeground"),
            (110, None, "TerminateAllLibraryApplets"),
            (111, None, "AreAnyLibraryAppletsLeft"),
            (112, None, "GetCurrentLibraryApplet"),
            (120, None, "GetApplicationId"),
            (121, None, "PushLaunchParameter"),
            (122, None, "GetApplicationControlProperty"),
            (123, None, "GetApplicationLaunchProperty"),
            (124, None, "GetApplicationLaunchRequestInfo"),
            (130, None, "SetUsers"),
            (131, Some(Self::check_rights_environment_available_handler), "CheckRightsEnvironmentAvailable"),
            (132, Some(Self::get_ns_rights_environment_handle_handler), "GetNsRightsEnvironmentHandle"),
            (140, None, "GetDesirableUids"),
            (150, Some(Self::report_application_exit_timeout_handler), "ReportApplicationExitTimeout"),
            (160, None, "SetApplicationAttribute"),
            (170, None, "HasSaveDataAccessPermission"),
            (180, None, "PushToFriendInvitationStorageChannel"),
            (190, None, "PushToNotificationStorageChannel"),
            (200, None, "RequestApplicationSoftReset"),
            (201, None, "RestartApplicationTimer"),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Port of IApplicationAccessor::Start
    pub fn start(&self) {
        log::info!("IApplicationAccessor::Start called");
        // TODO: m_applet->process->Run()
    }

    /// Port of IApplicationAccessor::RequestExit
    pub fn request_exit(&self) {
        log::info!("IApplicationAccessor::RequestExit called");
        // TODO: check exit_locked, lifecycle_manager.RequestExit()
    }

    /// Port of IApplicationAccessor::Terminate
    pub fn terminate(&self) {
        log::info!("IApplicationAccessor::Terminate called");
        // TODO: m_applet->process->Terminate()
    }

    /// Port of IApplicationAccessor::CheckRightsEnvironmentAvailable
    pub fn check_rights_environment_available(&self) -> bool {
        log::warn!("(STUBBED) CheckRightsEnvironmentAvailable called");
        true
    }

    /// Port of IApplicationAccessor::GetNsRightsEnvironmentHandle
    pub fn get_ns_rights_environment_handle(&self) -> u64 {
        log::warn!("(STUBBED) GetNsRightsEnvironmentHandle called");
        0xdeadbeef
    }

    /// Port of IApplicationAccessor::ReportApplicationExitTimeout
    pub fn report_application_exit_timeout(&self) {
        log::error!("ReportApplicationExitTimeout called");
    }

    fn start_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IApplicationAccessor) };
        service.start();

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn request_exit_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IApplicationAccessor) };
        service.request_exit();

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn terminate_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IApplicationAccessor) };
        service.terminate();

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn check_rights_environment_available_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IApplicationAccessor) };
        let available = service.check_rights_environment_available();

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(available);
    }

    fn get_ns_rights_environment_handle_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IApplicationAccessor) };
        let handle = service.get_ns_rights_environment_handle();

        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(handle);
    }

    fn report_application_exit_timeout_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IApplicationAccessor) };
        service.report_application_exit_timeout();

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for IApplicationAccessor {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }

    fn service_name(&self) -> &str {
        "am::IApplicationAccessor"
    }
}

impl ServiceFramework for IApplicationAccessor {
    fn get_service_name(&self) -> &str {
        "am::IApplicationAccessor"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
