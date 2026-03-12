// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/prepo/prepo.cpp
//!
//! PlayReport service -- "prepo:a", "prepo:a2", "prepo:m", "prepo:s", "prepo:u".

/// Play report type, matching upstream Reporter::PlayReportType.
///
/// Corresponds to `Core::Reporter::PlayReportType` used in upstream prepo.cpp.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum PlayReportType {
    Old = 0,
    Old2 = 1,
    New = 2,
    System = 3,
}

/// IPC command IDs for PlayReport.
///
/// Corresponds to the function table in `PlayReport` constructor (upstream prepo.cpp).
pub mod commands {
    pub const SAVE_REPORT_OLD: u32 = 10100;
    pub const SAVE_REPORT_WITH_USER_OLD: u32 = 10101;
    pub const SAVE_REPORT_OLD2: u32 = 10102;
    pub const SAVE_REPORT_WITH_USER_OLD2: u32 = 10103;
    pub const SAVE_REPORT: u32 = 10104;
    pub const SAVE_REPORT_WITH_USER: u32 = 10105;
    pub const REQUEST_IMMEDIATE_TRANSMISSION: u32 = 10200;
    pub const GET_TRANSMISSION_STATUS: u32 = 10300;
    pub const GET_SYSTEM_SESSION_ID: u32 = 10400;
    pub const SAVE_SYSTEM_REPORT: u32 = 20100;
    pub const SAVE_SYSTEM_REPORT_WITH_USER: u32 = 20101;
    pub const SET_OPERATION_MODE: u32 = 20200;
    pub const CLEAR_STORAGE: u32 = 30100;
    pub const CLEAR_STATISTICS: u32 = 30200;
    pub const GET_STORAGE_USAGE: u32 = 30300;
    pub const GET_STATISTICS: u32 = 30400;
    pub const GET_THROUGHPUT_HISTORY: u32 = 30401;
    pub const GET_LAST_UPLOAD_ERROR: u32 = 30500;
    pub const GET_APPLICATION_UPLOAD_SUMMARY: u32 = 30600;
    pub const IS_USER_AGREEMENT_CHECK_ENABLED: u32 = 40100;
    pub const SET_USER_AGREEMENT_CHECK_ENABLED: u32 = 40101;
    pub const READ_ALL_APPLICATION_REPORT_FILES: u32 = 50100;
    pub const READ_ALL_REPORT_FILES: u32 = 90100;
    pub const UNKNOWN_90101: u32 = 90101;
    pub const UNKNOWN_90102: u32 = 90102;
    pub const GET_STATISTICS_90200: u32 = 90200;
    pub const GET_THROUGHPUT_HISTORY_90201: u32 = 90201;
    pub const GET_LAST_UPLOAD_ERROR_90300: u32 = 90300;
}

/// PlayReport service ("prepo:a", "prepo:a2", "prepo:m", "prepo:s", "prepo:u").
///
/// Corresponds to `PlayReport` in upstream prepo.cpp.
pub struct PlayReport {
    name: String,
}

impl PlayReport {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }

    /// SaveReport -- saves a play report of the given type.
    ///
    /// Corresponds to `PlayReport::SaveReport<Type>` in upstream prepo.cpp.
    pub fn save_report(
        &self,
        report_type: PlayReportType,
        process_id: u64,
        data1: &[u8],
        data2: &[u8],
    ) {
        log::debug!(
            "PlayReport({})::save_report called, type={:02X}, process_id={:016X}, data1_size={:016X}, data2_size={:016X}",
            self.name,
            report_type as u8,
            process_id,
            data1.len(),
            data2.len()
        );
        // TODO: forward to reporter via system.GetReporter().SavePlayReport(...)
    }

    /// SaveReportWithUser -- saves a play report with a user ID.
    ///
    /// Corresponds to `PlayReport::SaveReportWithUser<Type>` in upstream prepo.cpp.
    pub fn save_report_with_user(
        &self,
        report_type: PlayReportType,
        user_id: u128,
        process_id: u64,
        data1: &[u8],
        data2: &[u8],
    ) {
        log::debug!(
            "PlayReport({})::save_report_with_user called, type={:02X}, user_id={:032X}, process_id={:016X}, data1_size={:016X}, data2_size={:016X}",
            self.name,
            report_type as u8,
            user_id,
            process_id,
            data1.len(),
            data2.len()
        );
        // TODO: forward to reporter via system.GetReporter().SavePlayReport(...)
    }

    /// RequestImmediateTransmission (cmd 10200).
    ///
    /// Corresponds to `PlayReport::RequestImmediateTransmission` in upstream prepo.cpp.
    pub fn request_immediate_transmission(&self) {
        log::warn!("(STUBBED) PlayReport::request_immediate_transmission called");
    }

    /// GetTransmissionStatus (cmd 10300).
    ///
    /// Corresponds to `PlayReport::GetTransmissionStatus` in upstream prepo.cpp.
    pub fn get_transmission_status(&self) -> i32 {
        log::warn!("(STUBBED) PlayReport::get_transmission_status called");
        0
    }

    /// GetSystemSessionId (cmd 10400).
    ///
    /// Corresponds to `PlayReport::GetSystemSessionId` in upstream prepo.cpp.
    pub fn get_system_session_id(&self) -> u64 {
        log::warn!("(STUBBED) PlayReport::get_system_session_id called");
        0
    }

    /// SaveSystemReport (cmd 20100).
    ///
    /// Corresponds to `PlayReport::SaveSystemReport` in upstream prepo.cpp.
    pub fn save_system_report(&self, title_id: u64, data1: &[u8], data2: &[u8]) {
        log::debug!(
            "PlayReport({})::save_system_report called, title_id={:016X}, data1_size={:016X}, data2_size={:016X}",
            self.name,
            title_id,
            data1.len(),
            data2.len()
        );
        // TODO: forward to reporter
    }

    /// SaveSystemReportWithUser (cmd 20101).
    ///
    /// Corresponds to `PlayReport::SaveSystemReportWithUser` in upstream prepo.cpp.
    pub fn save_system_report_with_user(
        &self,
        user_id: u128,
        title_id: u64,
        data1: &[u8],
        data2: &[u8],
    ) {
        log::debug!(
            "PlayReport({})::save_system_report_with_user called, user_id={:032X}, title_id={:016X}, data1_size={:016X}, data2_size={:016X}",
            self.name,
            user_id,
            title_id,
            data1.len(),
            data2.len()
        );
        // TODO: forward to reporter
    }
}

/// Registers "prepo:a", "prepo:a2", "prepo:m", "prepo:s", "prepo:u" services.
///
/// Corresponds to `LoopProcess` in upstream prepo.cpp.
pub fn loop_process() {
    log::debug!("PlayReport::LoopProcess called");
    // TODO: register services with ServerManager
}
