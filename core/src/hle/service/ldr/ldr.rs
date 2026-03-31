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
    pub fn new() -> Self {
        Self
    }
}

pub struct ProcessManager;
impl ProcessManager {
    pub fn new() -> Self {
        Self
    }
}

pub struct Shell;
impl Shell {
    pub fn new() -> Self {
        Self
    }
}

/// Registers "ldr:pm", "ldr:shel", "ldr:dmnt" services.
///
/// Corresponds to `LoopProcess` in upstream `ldr.cpp`.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::server_manager::ServerManager;

    let mut server_manager = ServerManager::new(system);

    let stub = |sm: &mut ServerManager, name: &str| {
        let svc_name = name.to_string();
        sm.register_named_service(
            name,
            Box::new(move || -> SessionRequestHandlerPtr {
                std::sync::Arc::new(crate::hle::service::services::GenericStubService::new(
                    &svc_name,
                ))
            }),
            64,
        );
    };
    stub(&mut server_manager, "ldr:pm");
    stub(&mut server_manager, "ldr:shel");
    stub(&mut server_manager, "ldr:dmnt");

    ServerManager::run_server(server_manager);
}
