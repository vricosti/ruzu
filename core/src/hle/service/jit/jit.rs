// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/jit/jit.h
//! Port of zuyu/src/core/hle/service/jit/jit.cpp
//!
//! JIT service — "jit:u" service registration and IJitEnvironment.

/// LoopProcess — registers "jit:u" service.
///
/// Corresponds to `Service::JIT::LoopProcess` in upstream jit.cpp.
pub fn loop_process() {
    use crate::hle::service::server_manager::ServerManager;
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;

    let mut server_manager = ServerManager::new(crate::core::SystemRef::null());

    let stub = |sm: &mut ServerManager, name: &str| {
        let svc_name = name.to_string();
        sm.register_named_service(
            name,
            Box::new(move || -> SessionRequestHandlerPtr {
                std::sync::Arc::new(
                    crate::hle::service::services::GenericStubService::new(&svc_name),
                )
            }),
            64,
        );
    };
    stub(&mut server_manager, "jit:u");

    ServerManager::run_server(server_manager);
}

/// IPC command table for IJitEnvironment.
pub mod jit_environment_commands {
    pub const GENERATE_CODE: u32 = 0;
    pub const CONTROL: u32 = 1;
    pub const LOAD_PLUGIN: u32 = 1000;
    pub const GET_CODE_ADDRESS: u32 = 1001;
}

/// IJitEnvironment — JIT execution environment.
///
/// Corresponds to `IJitEnvironment` in upstream jit.cpp.
pub struct IJitEnvironment {
    // TODO: JitContext, code memory, etc.
}

impl IJitEnvironment {
    pub fn new() -> Self {
        Self {}
    }
}
