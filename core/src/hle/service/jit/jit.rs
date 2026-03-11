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
    log::debug!("JIT::LoopProcess called");
    // TODO: Register jit:u with ServerManager
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
