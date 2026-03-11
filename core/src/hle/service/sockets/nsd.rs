// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/sockets/nsd.h
//! Port of zuyu/src/core/hle/service/sockets/nsd.cpp
//!
//! NSD service — Nintendo Socket Daemon ("nsd:u", "nsd:a").

/// IPC command table for NSD.
pub mod commands {
    pub const RESOLVE: u32 = 10;
    pub const RESOLVE_EX: u32 = 11;
    pub const GET_ENVIRONMENT_IDENTIFIER: u32 = 12;
    pub const GET_ENVIRONMENT_IDENTIFIER_FROM_DATA: u32 = 13;
    pub const GET_APPLICATION_SERVER_ENVIRONMENT_TYPE: u32 = 14;
    pub const SET_ENVIRONMENT_IDENTIFIER: u32 = 20;
    pub const DELETE_SETTINGS: u32 = 21;
    pub const READ_SAVE_DATA_FROM_FS_FOR_TEST: u32 = 30;
    pub const WRITE_SAVE_DATA_TO_FS_FOR_TEST: u32 = 31;
    pub const DELETE_SAVE_DATA_OF_FS_FOR_TEST: u32 = 32;
}

/// NSD service.
///
/// Corresponds to `NSD` in upstream nsd.h / nsd.cpp.
pub struct Nsd;

impl Nsd {
    pub fn new() -> Self {
        Self
    }

    /// Resolve (cmd 10).
    pub fn resolve(&self, _request: &str) -> Option<String> {
        log::warn!("NSD::resolve (STUBBED) called");
        None
    }

    /// GetEnvironmentIdentifier (cmd 12).
    pub fn get_environment_identifier(&self) -> &str {
        log::debug!("NSD::get_environment_identifier called");
        "lp1" // Production environment
    }
}
