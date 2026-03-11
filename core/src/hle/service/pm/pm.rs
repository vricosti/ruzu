// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/pm/pm.cpp
//!
//! Process Manager services.

use crate::hle::result::{ErrorModule, ResultCode};

/// Upstream: `ResultProcessNotFound{ErrorModule::PM, 1}`
pub const RESULT_PROCESS_NOT_FOUND: ResultCode =
    ResultCode::from_module_description(ErrorModule::PM, 1);

/// SystemBootMode enum. Upstream: `SystemBootMode` in `pm.h`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SystemBootMode {
    Normal = 0,
    Maintenance = 1,
}

/// IPC command IDs for BootMode ("pm:bm")
pub mod boot_mode_commands {
    pub const GET_BOOT_MODE: u32 = 0;
    pub const SET_MAINTENANCE_BOOT: u32 = 1;
}

/// IPC command IDs for DebugMonitor ("pm:dmnt")
pub mod debug_monitor_commands {
    pub const GET_JIT_DEBUG_PROCESS_ID_LIST: u32 = 0;
    pub const START_PROCESS: u32 = 1;
    pub const GET_PROCESS_ID: u32 = 2;
    pub const HOOK_TO_CREATE_PROCESS: u32 = 3;
    pub const GET_APPLICATION_PROCESS_ID: u32 = 4;
    pub const ATMOSPHERE_GET_PROCESS_INFO: u32 = 65000;
}

/// IPC command IDs for Info ("pm:info")
pub mod info_commands {
    pub const GET_PROGRAM_ID: u32 = 0;
    pub const ATMOSPHERE_GET_PROCESS_ID: u32 = 65000;
}

/// IPC command IDs for Shell ("pm:shell")
pub mod shell_commands {
    pub const LAUNCH_PROGRAM: u32 = 0;
    pub const TERMINATE_PROCESS: u32 = 1;
    pub const TERMINATE_PROGRAM: u32 = 2;
    pub const GET_APPLICATION_PROCESS_ID_FOR_SHELL: u32 = 6;
}

/// BootMode service ("pm:bm").
pub struct BootMode {
    boot_mode: SystemBootMode,
}

impl BootMode {
    pub fn new() -> Self {
        Self {
            boot_mode: SystemBootMode::Normal,
        }
    }

    pub fn get_boot_mode(&self) -> SystemBootMode {
        log::debug!("BootMode::get_boot_mode called");
        self.boot_mode
    }

    pub fn set_maintenance_boot(&mut self) {
        log::debug!("BootMode::set_maintenance_boot called");
        self.boot_mode = SystemBootMode::Maintenance;
    }
}

/// DebugMonitor service ("pm:dmnt").
pub struct DebugMonitor;
impl DebugMonitor {
    pub fn new() -> Self { Self }

    pub fn get_process_id(&self, _program_id: u64) -> Result<u64, ResultCode> {
        log::debug!("DebugMonitor::get_process_id called");
        // TODO: search process list
        Err(RESULT_PROCESS_NOT_FOUND)
    }

    pub fn get_application_process_id(&self) -> u64 {
        log::debug!("DebugMonitor::get_application_process_id called");
        // TODO: search process list
        0
    }
}

/// Info service ("pm:info").
pub struct Info;
impl Info {
    pub fn new() -> Self { Self }

    pub fn get_program_id(&self, _process_id: u64) -> Result<u64, ResultCode> {
        log::debug!("Info::get_program_id called");
        // TODO: search process list
        Err(RESULT_PROCESS_NOT_FOUND)
    }
}

/// Shell service ("pm:shell").
pub struct Shell;
impl Shell {
    pub fn new() -> Self { Self }

    pub fn get_application_process_id_for_shell(&self) -> u64 {
        log::debug!("Shell::get_application_process_id_for_shell called");
        // TODO: search process list
        0
    }
}

/// Registers "pm:bm", "pm:dmnt", "pm:info", "pm:shell" services.
///
/// Corresponds to `LoopProcess` in upstream `pm.cpp`.
pub fn loop_process() {
    // TODO: register services with ServerManager
}
