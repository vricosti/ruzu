// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/bpc/bpc.cpp
//!
//! BPC service ("bpc") and BPC_R service ("bpc:r").
//! All commands are stubs (nullptr in upstream).

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
pub struct BPC;

impl BPC {
    pub fn new() -> Self {
        Self
    }
}

/// BPC_R service. All commands are unimplemented stubs in upstream.
pub struct BpcR;

impl BpcR {
    pub fn new() -> Self {
        Self
    }
}

/// Registers "bpc" and "bpc:r" services.
///
/// Corresponds to `LoopProcess` in upstream `bpc.cpp`:
/// ```cpp
/// server_manager->RegisterNamedService("bpc", std::make_shared<BPC>(system));
/// server_manager->RegisterNamedService("bpc:r", std::make_shared<BPC_R>(system));
/// ```
///
/// Neither BPC nor BPC_R implement SessionRequestHandler yet (all commands are
/// nullptr in upstream), so we use stub services.
pub fn loop_process(system: crate::core::SystemRef) {
    let mut server_manager = crate::hle::service::server_manager::ServerManager::new(system);
    crate::hle::service::services::register_stub_services(&mut server_manager, &["bpc", "bpc:r"]);
    crate::hle::service::server_manager::ServerManager::run_server(server_manager);
}
