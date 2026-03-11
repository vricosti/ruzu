// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ldr/ldr.cpp
//!
//! Loader services. All commands are unimplemented stubs.

/// IPC command IDs for DebugMonitor ("ldr:dmnt")
pub mod debug_monitor_commands {
    pub const SET_PROGRAM_ARGUMENT: u32 = 0;
    pub const FLUSH_ARGUMENTS: u32 = 1;
    pub const GET_PROCESS_MODULE_INFO: u32 = 2;
}

/// IPC command IDs for ProcessManager ("ldr:pm")
pub mod process_manager_commands {
    pub const CREATE_PROCESS: u32 = 0;
    pub const GET_PROGRAM_INFO: u32 = 1;
    pub const PIN_PROGRAM: u32 = 2;
    pub const UNPIN_PROGRAM: u32 = 3;
    pub const SET_ENABLED_PROGRAM_VERIFICATION: u32 = 4;
}

/// IPC command IDs for Shell ("ldr:shel")
pub mod shell_commands {
    pub const SET_PROGRAM_ARGUMENT: u32 = 0;
    pub const FLUSH_ARGUMENTS: u32 = 1;
}

pub struct DebugMonitor;
impl DebugMonitor {
    pub fn new() -> Self { Self }
}

pub struct ProcessManager;
impl ProcessManager {
    pub fn new() -> Self { Self }
}

pub struct Shell;
impl Shell {
    pub fn new() -> Self { Self }
}

/// Registers "ldr:dmnt", "ldr:pm", "ldr:shel" services.
///
/// Corresponds to `LoopProcess` in upstream `ldr.cpp`.
pub fn loop_process() {
    // TODO: register services with ServerManager
}
