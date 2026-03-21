// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/erpt/erpt.cpp
//!
//! Error report services: ErrorReportContext ("erpt:c") and ErrorReportSession ("erpt:r").

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
pub struct ErrorReportContext;

impl ErrorReportContext {
    pub fn new() -> Self {
        Self
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
}

/// ErrorReportSession service ("erpt:r").
///
/// Corresponds to `ErrorReportSession` in upstream erpt.cpp.
/// All commands are nullptr (unimplemented) in upstream.
pub struct ErrorReportSession;

impl ErrorReportSession {
    pub fn new() -> Self {
        Self
    }
}

/// Registers "erpt:c" and "erpt:r" services.
///
/// Corresponds to `LoopProcess` in upstream erpt.cpp:
/// ```cpp
/// server_manager->RegisterNamedService("erpt:c", std::make_shared<ErrorReportContext>(system));
/// server_manager->RegisterNamedService("erpt:r", std::make_shared<ErrorReportSession>(system));
/// ```
///
/// Neither ErrorReportContext nor ErrorReportSession implement SessionRequestHandler yet,
/// so we use stub services.
pub fn loop_process() {
    let mut server_manager = crate::hle::service::server_manager::ServerManager::new(
        crate::core::SystemRef::null(),
    );
    crate::hle::service::services::register_stub_services(
        &mut server_manager,
        &["erpt:c", "erpt:r"],
    );
    crate::hle::service::server_manager::ServerManager::run_server(server_manager);
}
