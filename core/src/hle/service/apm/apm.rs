// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/apm/apm.h
//! Port of zuyu/src/core/hle/service/apm/apm.cpp
//!
//! APM Module and LoopProcess.

use std::sync::Arc;

/// APM Module.
///
/// Corresponds to `Module` class in upstream `apm.h`.
pub struct Module;

impl Module {
    pub fn new() -> Self {
        Self
    }
}

/// Registers "apm", "apm:am", "apm:sys" services.
///
/// Corresponds to `LoopProcess` in upstream `apm.cpp`.
pub fn loop_process() {
    // TODO: register services with ServerManager
    // server_manager->RegisterNamedService("apm", APM(system, module, controller, "apm"))
    // server_manager->RegisterNamedService("apm:am", APM(system, module, controller, "apm:am"))
    // server_manager->RegisterNamedService("apm:sys", APM_Sys(system, controller))
}
