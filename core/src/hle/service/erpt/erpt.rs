// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/erpt/erpt.cpp
//!
//! Error report services: ErrorReportContext ("erpt:c") and ErrorReportSession ("erpt:r").

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for ErrorReportContext ("erpt:c").
///
/// Corresponds to the function table in upstream erpt.cpp `ErrorReportContext` constructor.
pub mod context_commands {
    pub const SUBMIT_CONTEXT: u32 = 0;
    pub const CREATE_REPORT_V0: u32 = 1;
    pub const SET_INITIAL_LAUNCH_SETTINGS_COMPLETION_TIME: u32 = 2;
    pub const CLEAR_INITIAL_LAUNCH_SETTINGS_COMPLETION_TIME: u32 = 3;
    pub const UPDATE_POWER_ON_TIME: u32 = 4;
    pub const UPDATE_AWAKE_TIME: u32 = 5;
    pub const SUBMIT_MULTIPLE_CATEGORY_CONTEXT: u32 = 6;
    pub const UPDATE_APPLICATION_LAUNCH_TIME: u32 = 7;
    pub const CLEAR_APPLICATION_LAUNCH_TIME: u32 = 8;
    pub const SUBMIT_ATTACHMENT: u32 = 9;
    pub const CREATE_REPORT_WITH_ATTACHMENTS: u32 = 10;
    pub const CREATE_REPORT_V1: u32 = 11;
    pub const CREATE_REPORT: u32 = 12;
    pub const REGISTER_RUNNING_APPLET: u32 = 20;
    pub const UNREGISTER_RUNNING_APPLET: u32 = 21;
    pub const UPDATE_APPLET_SUSPENDED_DURATION: u32 = 22;
    pub const INVALIDATE_FORCED_SHUTDOWN_DETECTION: u32 = 30;
}

/// IPC command IDs for ErrorReportSession ("erpt:r").
///
/// Corresponds to the function table in upstream erpt.cpp `ErrorReportSession` constructor.
pub mod session_commands {
    pub const OPEN_REPORT: u32 = 0;
    pub const OPEN_MANAGER: u32 = 1;
    pub const OPEN_ATTACHMENT: u32 = 2;
}

/// ErrorReportContext service ("erpt:c").
///
/// Corresponds to `ErrorReportContext` in upstream erpt.cpp.
pub struct ErrorReportContext {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ErrorReportContext {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (
                    context_commands::SUBMIT_CONTEXT,
                    Some(ErrorReportContext::submit_context_handler),
                    "SubmitContext",
                ),
                (
                    context_commands::CREATE_REPORT_V0,
                    Some(ErrorReportContext::create_report_v0_handler),
                    "CreateReportV0",
                ),
                (
                    context_commands::SET_INITIAL_LAUNCH_SETTINGS_COMPLETION_TIME,
                    None,
                    "SetInitialLaunchSettingsCompletionTime",
                ),
                (
                    context_commands::CLEAR_INITIAL_LAUNCH_SETTINGS_COMPLETION_TIME,
                    None,
                    "ClearInitialLaunchSettingsCompletionTime",
                ),
                (
                    context_commands::UPDATE_POWER_ON_TIME,
                    None,
                    "UpdatePowerOnTime",
                ),
                (context_commands::UPDATE_AWAKE_TIME, None, "UpdateAwakeTime"),
                (
                    context_commands::SUBMIT_MULTIPLE_CATEGORY_CONTEXT,
                    None,
                    "SubmitMultipleCategoryContext",
                ),
                (
                    context_commands::UPDATE_APPLICATION_LAUNCH_TIME,
                    None,
                    "UpdateApplicationLaunchTime",
                ),
                (
                    context_commands::CLEAR_APPLICATION_LAUNCH_TIME,
                    None,
                    "ClearApplicationLaunchTime",
                ),
                (
                    context_commands::SUBMIT_ATTACHMENT,
                    None,
                    "SubmitAttachment",
                ),
                (
                    context_commands::CREATE_REPORT_WITH_ATTACHMENTS,
                    None,
                    "CreateReportWithAttachments",
                ),
                (
                    context_commands::CREATE_REPORT_V1,
                    Some(ErrorReportContext::create_report_v1_handler),
                    "CreateReportV1",
                ),
                (
                    context_commands::CREATE_REPORT,
                    Some(ErrorReportContext::create_report_handler),
                    "CreateReport",
                ),
                (
                    context_commands::REGISTER_RUNNING_APPLET,
                    None,
                    "RegisterRunningApplet",
                ),
                (
                    context_commands::UNREGISTER_RUNNING_APPLET,
                    None,
                    "UnregisterRunningApplet",
                ),
                (
                    context_commands::UPDATE_APPLET_SUSPENDED_DURATION,
                    None,
                    "UpdateAppletSuspendedDuration",
                ),
                (
                    context_commands::INVALIDATE_FORCED_SHUTDOWN_DETECTION,
                    None,
                    "InvalidateForcedShutdownDetection",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// SubmitContext (cmd 0).
    ///
    /// Corresponds to `ErrorReportContext::SubmitContext` in upstream erpt.cpp.
    pub fn submit_context(&self, context_entry: &[u8], field_list: &[u8]) {
        log::warn!(
            "(STUBBED) ErrorReportContext::submit_context called, context_entry_size={}, field_list_size={}",
            context_entry.len(),
            field_list.len()
        );
    }

    fn submit_context_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("ErrorReportContext::SubmitContext (STUBBED) called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(crate::hle::result::RESULT_SUCCESS);
    }

    /// CreateReportV0 (cmd 1).
    ///
    /// Corresponds to `ErrorReportContext::CreateReportV0` in upstream erpt.cpp.
    pub fn create_report_v0(
        &self,
        report_type: u32,
        _context_entry: &[u8],
        _report_list: &[u8],
        _report_meta_data: &[u8],
    ) {
        log::warn!(
            "(STUBBED) ErrorReportContext::create_report_v0 called, report_type={:#x}",
            report_type
        );
    }

    fn create_report_v0_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("ErrorReportContext::CreateReportV0 (STUBBED) called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(crate::hle::result::RESULT_SUCCESS);
    }

    /// CreateReportV1 (cmd 11).
    ///
    /// Corresponds to `ErrorReportContext::CreateReportV1` in upstream erpt.cpp.
    pub fn create_report_v1(
        &self,
        report_type: u32,
        unknown: u32,
        _context_entry: &[u8],
        _report_list: &[u8],
        _report_meta_data: &[u8],
    ) {
        log::warn!(
            "(STUBBED) ErrorReportContext::create_report_v1 called, report_type={:#x}, unknown={:#x}",
            report_type,
            unknown
        );
    }

    fn create_report_v1_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("ErrorReportContext::CreateReportV1 (STUBBED) called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(crate::hle::result::RESULT_SUCCESS);
    }

    /// CreateReport (cmd 12).
    ///
    /// Corresponds to `ErrorReportContext::CreateReport` in upstream erpt.cpp.
    pub fn create_report(
        &self,
        report_type: u32,
        unknown: u32,
        create_report_option_flag: u32,
        _context_entry: &[u8],
        _report_list: &[u8],
        _report_meta_data: &[u8],
    ) {
        log::warn!(
            "(STUBBED) ErrorReportContext::create_report called, report_type={:#x}, unknown={:#x}, create_report_option_flag={:#x}",
            report_type,
            unknown,
            create_report_option_flag
        );
    }

    fn create_report_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("ErrorReportContext::CreateReport (STUBBED) called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(crate::hle::result::RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for ErrorReportContext {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "erpt:c"
    }
}

impl ServiceFramework for ErrorReportContext {
    fn get_service_name(&self) -> &str {
        "erpt:c"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// ErrorReportSession service ("erpt:r").
///
/// Corresponds to `ErrorReportSession` in upstream erpt.cpp.
/// All commands are nullptr (unimplemented) in upstream.
pub struct ErrorReportSession {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ErrorReportSession {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (session_commands::OPEN_REPORT, None, "OpenReport"),
                (session_commands::OPEN_MANAGER, None, "OpenManager"),
                (session_commands::OPEN_ATTACHMENT, None, "OpenAttachment"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for ErrorReportSession {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "erpt:r"
    }
}

impl ServiceFramework for ErrorReportSession {
    fn get_service_name(&self) -> &str {
        "erpt:r"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// Registers "erpt:c" and "erpt:r" services.
///
/// Corresponds to `LoopProcess` in upstream erpt.cpp:
/// ```cpp
/// server_manager->RegisterNamedService("erpt:c", std::make_shared<ErrorReportContext>(system));
/// server_manager->RegisterNamedService("erpt:r", std::make_shared<ErrorReportSession>(system));
/// ```
pub fn loop_process(system: crate::core::SystemRef) {
    let server_manager = crate::hle::service::server_manager::ServerManager::new_shared(system);
    {
        let mut server_manager = server_manager.lock().unwrap();
        server_manager.register_named_service(
            "erpt:c",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(ErrorReportContext::new())
            }),
            64,
        );
        server_manager.register_named_service(
            "erpt:r",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(ErrorReportSession::new())
            }),
            64,
        );
    }
    crate::hle::service::server_manager::ServerManager::run_server_shared(server_manager);
}
