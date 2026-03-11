// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/prepo/prepo.cpp
//!
//! PlayReport service.

/// IPC command IDs for PlayReport
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
}

/// PlayReport service ("prepo:a", "prepo:a2", "prepo:m", "prepo:s", "prepo:u").
pub struct PlayReport {
    name: String,
}

impl PlayReport {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }

    pub fn save_report(&self, _process_id: u64, _data1: &[u8], _data2: &[u8]) {
        log::debug!("PlayReport({})::save_report called", self.name);
        // TODO: forward to reporter
    }

    pub fn save_report_with_user(
        &self,
        _user_id: u128,
        _process_id: u64,
        _data1: &[u8],
        _data2: &[u8],
    ) {
        log::debug!("PlayReport({})::save_report_with_user called", self.name);
        // TODO: forward to reporter
    }

    pub fn request_immediate_transmission(&self) {
        log::warn!("(STUBBED) PlayReport::request_immediate_transmission called");
    }

    pub fn get_transmission_status(&self) -> i32 {
        log::warn!("(STUBBED) PlayReport::get_transmission_status called");
        0
    }

    pub fn get_system_session_id(&self) -> u64 {
        log::warn!("(STUBBED) PlayReport::get_system_session_id called");
        0
    }

    pub fn save_system_report(&self, _title_id: u64, _data1: &[u8], _data2: &[u8]) {
        log::debug!("PlayReport({})::save_system_report called", self.name);
        // TODO: forward to reporter
    }

    pub fn save_system_report_with_user(
        &self,
        _user_id: u128,
        _title_id: u64,
        _data1: &[u8],
        _data2: &[u8],
    ) {
        log::debug!("PlayReport({})::save_system_report_with_user called", self.name);
        // TODO: forward to reporter
    }
}

/// Registers "prepo:a", "prepo:a2", "prepo:m", "prepo:s", "prepo:u" services.
///
/// Corresponds to `LoopProcess` in upstream `prepo.cpp`.
pub fn loop_process() {
    // TODO: register services with ServerManager
}
