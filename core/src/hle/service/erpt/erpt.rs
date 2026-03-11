// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/erpt/erpt.cpp
//!
//! ErrorReportContext ("erpt:c") and ErrorReportSession ("erpt:r") services.

/// IPC command IDs for ErrorReportContext
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

/// IPC command IDs for ErrorReportSession
pub mod session_commands {
    pub const OPEN_REPORT: u32 = 0;
    pub const OPEN_MANAGER: u32 = 1;
    pub const OPEN_ATTACHMENT: u32 = 2;
}

/// ErrorReportContext service ("erpt:c").
pub struct ErrorReportContext;

impl ErrorReportContext {
    pub fn new() -> Self {
        Self
    }

    /// Stubbed: SubmitContext (cmd 0)
    pub fn submit_context(&self, _context_entry: &[u8], _field_list: &[u8]) {
        log::warn!("(STUBBED) ErrorReportContext::submit_context called");
    }

    /// Stubbed: CreateReportV0 (cmd 1)
    pub fn create_report_v0(&self, report_type: u32) {
        log::warn!(
            "(STUBBED) ErrorReportContext::create_report_v0 called, report_type={:#x}",
            report_type
        );
    }

    /// Stubbed: CreateReportV1 (cmd 11)
    pub fn create_report_v1(&self, report_type: u32, unknown: u32) {
        log::warn!(
            "(STUBBED) ErrorReportContext::create_report_v1 called, report_type={:#x}, unknown={:#x}",
            report_type,
            unknown
        );
    }

    /// Stubbed: CreateReport (cmd 12)
    pub fn create_report(&self, report_type: u32, unknown: u32, create_report_option_flag: u32) {
        log::warn!(
            "(STUBBED) ErrorReportContext::create_report called, report_type={:#x}, unknown={:#x}, flag={:#x}",
            report_type,
            unknown,
            create_report_option_flag
        );
    }
}

/// ErrorReportSession service ("erpt:r"). All commands are unimplemented stubs.
pub struct ErrorReportSession;

impl ErrorReportSession {
    pub fn new() -> Self {
        Self
    }
}

/// Registers "erpt:c" and "erpt:r" services.
///
/// Corresponds to `LoopProcess` in upstream `erpt.cpp`.
pub fn loop_process() {
    // TODO: register services with ServerManager
}
