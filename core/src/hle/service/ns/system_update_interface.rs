// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/system_update_interface.h
//! Port of zuyu/src/core/hle/service/ns/system_update_interface.cpp
//!
//! ISystemUpdateInterface — "ns:su" service.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use super::ns_types::BackgroundNetworkUpdateState;

/// IPC command table for ISystemUpdateInterface.
///
/// Corresponds to the function table in upstream system_update_interface.cpp.
pub mod commands {
    pub const GET_BACKGROUND_NETWORK_UPDATE_STATE: u32 = 0;
    pub const OPEN_SYSTEM_UPDATE_CONTROL: u32 = 1;
    pub const NOTIFY_EX_FAT_DRIVER_REQUIRED: u32 = 2;
    pub const CLEAR_EX_FAT_DRIVER_STATUS_FOR_DEBUG: u32 = 3;
    pub const REQUEST_BACKGROUND_NETWORK_UPDATE: u32 = 4;
    pub const NOTIFY_BACKGROUND_NETWORK_UPDATE: u32 = 5;
    pub const NOTIFY_EX_FAT_DRIVER_DOWNLOADED_FOR_DEBUG: u32 = 6;
    pub const GET_SYSTEM_UPDATE_NOTIFICATION_EVENT_FOR_CONTENT_DELIVERY: u32 = 9;
    pub const NOTIFY_SYSTEM_UPDATE_FOR_CONTENT_DELIVERY: u32 = 10;
    pub const PREPARE_SHUTDOWN: u32 = 11;
    pub const UNKNOWN_12: u32 = 12;
    pub const UNKNOWN_13: u32 = 13;
    pub const UNKNOWN_14: u32 = 14;
    pub const UNKNOWN_15: u32 = 15;
    pub const DESTROY_SYSTEM_UPDATE_TASK: u32 = 16;
    pub const REQUEST_SEND_SYSTEM_UPDATE: u32 = 17;
    pub const GET_SEND_SYSTEM_UPDATE_PROGRESS: u32 = 18;
}

/// ISystemUpdateInterface.
///
/// Corresponds to `ISystemUpdateInterface` in upstream.
pub struct ISystemUpdateInterface {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ISystemUpdateInterface {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (commands::GET_BACKGROUND_NETWORK_UPDATE_STATE, None, "GetBackgroundNetworkUpdateState"),
            (commands::OPEN_SYSTEM_UPDATE_CONTROL, None, "OpenSystemUpdateControl"),
            (commands::NOTIFY_EX_FAT_DRIVER_REQUIRED, None, "NotifyExFatDriverRequired"),
            (commands::CLEAR_EX_FAT_DRIVER_STATUS_FOR_DEBUG, None, "ClearExFatDriverStatusForDebug"),
            (commands::REQUEST_BACKGROUND_NETWORK_UPDATE, None, "RequestBackgroundNetworkUpdate"),
            (commands::NOTIFY_BACKGROUND_NETWORK_UPDATE, None, "NotifyBackgroundNetworkUpdate"),
            (commands::NOTIFY_EX_FAT_DRIVER_DOWNLOADED_FOR_DEBUG, None, "NotifyExFatDriverDownloadedForDebug"),
            (commands::GET_SYSTEM_UPDATE_NOTIFICATION_EVENT_FOR_CONTENT_DELIVERY, None, "GetSystemUpdateNotificationEventForContentDelivery"),
            (commands::NOTIFY_SYSTEM_UPDATE_FOR_CONTENT_DELIVERY, None, "NotifySystemUpdateForContentDelivery"),
            (commands::PREPARE_SHUTDOWN, None, "PrepareShutdown"),
            (commands::UNKNOWN_12, None, "Unknown12"),
            (commands::UNKNOWN_13, None, "Unknown13"),
            (commands::UNKNOWN_14, None, "Unknown14"),
            (commands::UNKNOWN_15, None, "Unknown15"),
            (commands::DESTROY_SYSTEM_UPDATE_TASK, None, "DestroySystemUpdateTask"),
            (commands::REQUEST_SEND_SYSTEM_UPDATE, None, "RequestSendSystemUpdate"),
            (commands::GET_SEND_SYSTEM_UPDATE_PROGRESS, None, "GetSendSystemUpdateProgress"),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// GetBackgroundNetworkUpdateState (cmd 0).
    ///
    /// Corresponds to upstream `ISystemUpdateInterface::GetBackgroundNetworkUpdateState`.
    pub fn get_background_network_update_state(
        &self,
    ) -> Result<BackgroundNetworkUpdateState, ResultCode> {
        log::warn!("(STUBBED) GetBackgroundNetworkUpdateState called");
        Ok(BackgroundNetworkUpdateState::None)
    }

    /// OpenSystemUpdateControl (cmd 1).
    ///
    /// Corresponds to upstream `ISystemUpdateInterface::OpenSystemUpdateControl`.
    pub fn open_system_update_control(&self) -> Result<(), ResultCode> {
        log::warn!("(STUBBED) OpenSystemUpdateControl called");
        // TODO: Create and return ISystemUpdateControl
        Ok(())
    }
}

impl SessionRequestHandler for ISystemUpdateInterface {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ns::ISystemUpdateInterface"
    }
}

impl ServiceFramework for ISystemUpdateInterface {
    fn get_service_name(&self) -> &str {
        "ns::ISystemUpdateInterface"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
