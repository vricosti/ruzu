// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/system_update_control.h
//! Port of zuyu/src/core/hle/service/ns/system_update_control.cpp
//!
//! ISystemUpdateControl — system update control sub-interface.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for ISystemUpdateControl.
///
/// Corresponds to the function table in upstream system_update_control.cpp.
pub mod commands {
    pub const HAS_DOWNLOADED: u32 = 0;
    pub const REQUEST_CHECK_LATEST_UPDATE: u32 = 1;
    pub const REQUEST_DOWNLOAD_LATEST_UPDATE: u32 = 2;
    pub const GET_DOWNLOAD_PROGRESS: u32 = 3;
    pub const APPLY_DOWNLOADED_UPDATE: u32 = 4;
    pub const REQUEST_PREPARE_CARD_UPDATE: u32 = 5;
    pub const GET_PREPARE_CARD_UPDATE_PROGRESS: u32 = 6;
    pub const HAS_PREPARED_CARD_UPDATE: u32 = 7;
    pub const APPLY_CARD_UPDATE: u32 = 8;
    pub const GET_DOWNLOADED_EULA_DATA_SIZE: u32 = 9;
    pub const GET_DOWNLOADED_EULA_DATA: u32 = 10;
    pub const SETUP_CARD_UPDATE: u32 = 11;
    pub const GET_PREPARED_CARD_UPDATE_EULA_DATA_SIZE: u32 = 12;
    pub const GET_PREPARED_CARD_UPDATE_EULA_DATA: u32 = 13;
    pub const SETUP_CARD_UPDATE_VIA_SYSTEM_UPDATER: u32 = 14;
    pub const HAS_RECEIVED: u32 = 15;
    pub const REQUEST_RECEIVE_SYSTEM_UPDATE: u32 = 16;
    pub const GET_RECEIVE_PROGRESS: u32 = 17;
    pub const APPLY_RECEIVED_UPDATE: u32 = 18;
    pub const GET_RECEIVED_EULA_DATA_SIZE: u32 = 19;
    pub const GET_RECEIVED_EULA_DATA: u32 = 20;
    pub const SETUP_TO_RECEIVE_SYSTEM_UPDATE: u32 = 21;
    pub const REQUEST_CHECK_LATEST_UPDATE_INCLUDES_REBOOTLESS_UPDATE: u32 = 22;
}

/// ISystemUpdateControl.
///
/// Corresponds to `ISystemUpdateControl` in upstream.
/// All commands are unimplemented (nullptr) in upstream.
pub struct ISystemUpdateControl {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ISystemUpdateControl {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (commands::HAS_DOWNLOADED, None, "HasDownloaded"),
            (
                commands::REQUEST_CHECK_LATEST_UPDATE,
                None,
                "RequestCheckLatestUpdate",
            ),
            (
                commands::REQUEST_DOWNLOAD_LATEST_UPDATE,
                None,
                "RequestDownloadLatestUpdate",
            ),
            (commands::GET_DOWNLOAD_PROGRESS, None, "GetDownloadProgress"),
            (
                commands::APPLY_DOWNLOADED_UPDATE,
                None,
                "ApplyDownloadedUpdate",
            ),
            (
                commands::REQUEST_PREPARE_CARD_UPDATE,
                None,
                "RequestPrepareCardUpdate",
            ),
            (
                commands::GET_PREPARE_CARD_UPDATE_PROGRESS,
                None,
                "GetPrepareCardUpdateProgress",
            ),
            (
                commands::HAS_PREPARED_CARD_UPDATE,
                None,
                "HasPreparedCardUpdate",
            ),
            (commands::APPLY_CARD_UPDATE, None, "ApplyCardUpdate"),
            (
                commands::GET_DOWNLOADED_EULA_DATA_SIZE,
                None,
                "GetDownloadedEulaDataSize",
            ),
            (
                commands::GET_DOWNLOADED_EULA_DATA,
                None,
                "GetDownloadedEulaData",
            ),
            (commands::SETUP_CARD_UPDATE, None, "SetupCardUpdate"),
            (
                commands::GET_PREPARED_CARD_UPDATE_EULA_DATA_SIZE,
                None,
                "GetPreparedCardUpdateEulaDataSize",
            ),
            (
                commands::GET_PREPARED_CARD_UPDATE_EULA_DATA,
                None,
                "GetPreparedCardUpdateEulaData",
            ),
            (
                commands::SETUP_CARD_UPDATE_VIA_SYSTEM_UPDATER,
                None,
                "SetupCardUpdateViaSystemUpdater",
            ),
            (commands::HAS_RECEIVED, None, "HasReceived"),
            (
                commands::REQUEST_RECEIVE_SYSTEM_UPDATE,
                None,
                "RequestReceiveSystemUpdate",
            ),
            (commands::GET_RECEIVE_PROGRESS, None, "GetReceiveProgress"),
            (commands::APPLY_RECEIVED_UPDATE, None, "ApplyReceivedUpdate"),
            (
                commands::GET_RECEIVED_EULA_DATA_SIZE,
                None,
                "GetReceivedEulaDataSize",
            ),
            (
                commands::GET_RECEIVED_EULA_DATA,
                None,
                "GetReceivedEulaData",
            ),
            (
                commands::SETUP_TO_RECEIVE_SYSTEM_UPDATE,
                None,
                "SetupToReceiveSystemUpdate",
            ),
            (
                commands::REQUEST_CHECK_LATEST_UPDATE_INCLUDES_REBOOTLESS_UPDATE,
                None,
                "RequestCheckLatestUpdateIncludesRebootlessUpdate",
            ),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for ISystemUpdateControl {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ns::ISystemUpdateControl"
    }
}

impl ServiceFramework for ISystemUpdateControl {
    fn get_service_name(&self) -> &str {
        "ns::ISystemUpdateControl"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
