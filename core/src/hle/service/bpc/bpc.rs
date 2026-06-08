// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/bpc/bpc.cpp
//!
//! BPC service ("bpc") and BPC_R service ("bpc:r").
//! All commands are stubs (nullptr in upstream).

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for BPC service
pub mod bpc_commands {
    pub const SHUTDOWN_SYSTEM: u32 = 0;
    pub const REBOOT_SYSTEM: u32 = 1;
    pub const GET_WAKEUP_REASON: u32 = 2;
    pub const GET_SHUTDOWN_REASON: u32 = 3;
    pub const GET_AC_OK: u32 = 4;
    pub const GET_BOARD_POWER_CONTROL_EVENT: u32 = 5;
    pub const GET_SLEEP_BUTTON_STATE: u32 = 6;
    pub const GET_POWER_EVENT: u32 = 7;
    pub const CREATE_WAKEUP_TIMER: u32 = 8;
    pub const CANCEL_WAKEUP_TIMER: u32 = 9;
    pub const ENABLE_WAKEUP_TIMER_ON_DEVICE: u32 = 10;
    pub const CREATE_WAKEUP_TIMER_EX: u32 = 11;
    pub const GET_LAST_ENABLED_WAKEUP_TIMER_TYPE: u32 = 12;
    pub const CLEAN_ALL_WAKEUP_TIMERS: u32 = 13;
    pub const GET_POWER_BUTTON: u32 = 14;
    pub const SET_ENABLE_WAKEUP_TIMER: u32 = 15;
}

/// IPC command IDs for BPC_R service
pub mod bpc_r_commands {
    pub const GET_RTC_TIME: u32 = 0;
    pub const SET_RTC_TIME: u32 = 1;
    pub const GET_RTC_RESET_DETECTED: u32 = 2;
    pub const CLEAR_RTC_RESET_DETECTED: u32 = 3;
    pub const SET_UP_RTC_RESET_ON_SHUTDOWN: u32 = 4;
}

/// BPC service. All commands are unimplemented stubs in upstream.
pub struct BPC {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl BPC {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (bpc_commands::SHUTDOWN_SYSTEM, None, "ShutdownSystem"),
                (bpc_commands::REBOOT_SYSTEM, None, "RebootSystem"),
                (bpc_commands::GET_WAKEUP_REASON, None, "GetWakeupReason"),
                (bpc_commands::GET_SHUTDOWN_REASON, None, "GetShutdownReason"),
                (bpc_commands::GET_AC_OK, None, "GetAcOk"),
                (
                    bpc_commands::GET_BOARD_POWER_CONTROL_EVENT,
                    None,
                    "GetBoardPowerControlEvent",
                ),
                (
                    bpc_commands::GET_SLEEP_BUTTON_STATE,
                    None,
                    "GetSleepButtonState",
                ),
                (bpc_commands::GET_POWER_EVENT, None, "GetPowerEvent"),
                (bpc_commands::CREATE_WAKEUP_TIMER, None, "CreateWakeupTimer"),
                (bpc_commands::CANCEL_WAKEUP_TIMER, None, "CancelWakeupTimer"),
                (
                    bpc_commands::ENABLE_WAKEUP_TIMER_ON_DEVICE,
                    None,
                    "EnableWakeupTimerOnDevice",
                ),
                (
                    bpc_commands::CREATE_WAKEUP_TIMER_EX,
                    None,
                    "CreateWakeupTimerEx",
                ),
                (
                    bpc_commands::GET_LAST_ENABLED_WAKEUP_TIMER_TYPE,
                    None,
                    "GetLastEnabledWakeupTimerType",
                ),
                (
                    bpc_commands::CLEAN_ALL_WAKEUP_TIMERS,
                    None,
                    "CleanAllWakeupTimers",
                ),
                (bpc_commands::GET_POWER_BUTTON, None, "GetPowerButton"),
                (
                    bpc_commands::SET_ENABLE_WAKEUP_TIMER,
                    None,
                    "SetEnableWakeupTimer",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for BPC {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "bpc"
    }
}

impl ServiceFramework for BPC {
    fn get_service_name(&self) -> &str {
        "bpc"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// BPC_R service. All commands are unimplemented stubs in upstream.
pub struct BpcR {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl BpcR {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (bpc_r_commands::GET_RTC_TIME, None, "GetRtcTime"),
                (bpc_r_commands::SET_RTC_TIME, None, "SetRtcTime"),
                (
                    bpc_r_commands::GET_RTC_RESET_DETECTED,
                    None,
                    "GetRtcResetDetected",
                ),
                (
                    bpc_r_commands::CLEAR_RTC_RESET_DETECTED,
                    None,
                    "ClearRtcResetDetected",
                ),
                (
                    bpc_r_commands::SET_UP_RTC_RESET_ON_SHUTDOWN,
                    None,
                    "SetUpRtcResetOnShutdown",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for BpcR {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "bpc:r"
    }
}

impl ServiceFramework for BpcR {
    fn get_service_name(&self) -> &str {
        "bpc:r"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// Registers "bpc" and "bpc:r" services.
///
/// Corresponds to `LoopProcess` in upstream `bpc.cpp`:
/// ```cpp
/// server_manager->RegisterNamedService("bpc", std::make_shared<BPC>(system));
/// server_manager->RegisterNamedService("bpc:r", std::make_shared<BPC_R>(system));
/// ```
pub fn loop_process(system: crate::core::SystemRef) {
    let server_manager = crate::hle::service::server_manager::ServerManager::new_shared(system);
    {
        let mut server_manager = server_manager.lock().unwrap();
        server_manager.register_named_service(
            "bpc",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(BPC::new()) }),
            64,
        );
        server_manager.register_named_service(
            "bpc:r",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(BpcR::new()) }),
            64,
        );
    }
    crate::hle::service::server_manager::ServerManager::run_server_shared(server_manager);
}
