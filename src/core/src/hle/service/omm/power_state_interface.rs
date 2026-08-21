// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/omm/power_state_interface.h
//! Port of zuyu/src/core/hle/service/omm/power_state_interface.cpp

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

pub mod commands {
    pub const GET_STATE: u32 = 0;
    pub const ENTER_SLEEP: u32 = 1;
    pub const GET_LAST_WAKE_REASON: u32 = 2;
    pub const SHUTDOWN: u32 = 3;
    pub const GET_NOTIFICATION_MESSAGE_EVENT_HANDLE: u32 = 4;
    pub const RECEIVE_NOTIFICATION_MESSAGE: u32 = 5;
    pub const ANALYZE_LOG_FOR_LAST_SLEEP_WAKE_SEQUENCE: u32 = 6;
    pub const RESET_EVENT_LOG: u32 = 7;
    pub const ANALYZE_PERFORMANCE_LOG_FOR_LAST_SLEEP_WAKE_SEQUENCE: u32 = 8;
    pub const CHANGE_HOME_BUTTON_LONG_PRESSING_TIME: u32 = 9;
    pub const PUT_ERROR_STATE: u32 = 10;
    pub const INVALIDATE_CURRENT_HOME_BUTTON_PRESSING: u32 = 11;
}

/// IPowerStateInterface service ("spsm").
///
/// All commands are `nullptr` in upstream.
pub struct IPowerStateInterface {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IPowerStateInterface {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (commands::GET_STATE, None, "GetState"),
                (commands::ENTER_SLEEP, None, "EnterSleep"),
                (commands::GET_LAST_WAKE_REASON, None, "GetLastWakeReason"),
                (commands::SHUTDOWN, None, "Shutdown"),
                (
                    commands::GET_NOTIFICATION_MESSAGE_EVENT_HANDLE,
                    None,
                    "GetNotificationMessageEventHandle",
                ),
                (
                    commands::RECEIVE_NOTIFICATION_MESSAGE,
                    None,
                    "ReceiveNotificationMessage",
                ),
                (
                    commands::ANALYZE_LOG_FOR_LAST_SLEEP_WAKE_SEQUENCE,
                    None,
                    "AnalyzeLogForLastSleepWakeSequence",
                ),
                (commands::RESET_EVENT_LOG, None, "ResetEventLog"),
                (
                    commands::ANALYZE_PERFORMANCE_LOG_FOR_LAST_SLEEP_WAKE_SEQUENCE,
                    None,
                    "AnalyzePerformanceLogForLastSleepWakeSequence",
                ),
                (
                    commands::CHANGE_HOME_BUTTON_LONG_PRESSING_TIME,
                    None,
                    "ChangeHomeButtonLongPressingTime",
                ),
                (commands::PUT_ERROR_STATE, None, "PutErrorState"),
                (
                    commands::INVALIDATE_CURRENT_HOME_BUTTON_PRESSING,
                    None,
                    "InvalidateCurrentHomeButtonPressing",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IPowerStateInterface {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "spsm"
    }
}

impl ServiceFramework for IPowerStateInterface {
    fn get_service_name(&self) -> &str {
        "spsm"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn power_state_interface_table_matches_upstream_command_count() {
        assert_eq!(IPowerStateInterface::new().handlers.len(), 12);
    }
}
