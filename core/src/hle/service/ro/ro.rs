// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ro/ro.h
//! Port of zuyu/src/core/hle/service/ro/ro.cpp
//!
//! RO service — read-only module loading ("ro:1" and "ro:a").

/// Maximum number of NRO modules.
pub const MAX_NRO_COUNT: usize = 64;
/// Maximum number of NRR registrations.
pub const MAX_NRR_COUNT: usize = 64;

/// IPC command table for IRoInterface.
pub mod commands {
    pub const LOAD_MODULE: u32 = 0;
    pub const UNLOAD_MODULE: u32 = 1;
    pub const REGISTER_MODULE_INFO: u32 = 2;
    pub const UNREGISTER_MODULE_INFO: u32 = 3;
    pub const INITIALIZE: u32 = 4;
    pub const REGISTER_MODULE_INFO2: u32 = 10;
}

/// LoopProcess — registers "ro:1" and "ro:a" services.
///
/// Corresponds to `Service::RO::LoopProcess` in upstream ro.cpp.
pub fn loop_process() {
    log::debug!("RO::LoopProcess called");
    // TODO: Register ro:1 and ro:a with ServerManager
}

/// IRoInterface — read-only module loading interface.
///
/// Corresponds to `IRoInterface` in upstream ro.cpp.
pub struct IRoInterface {
    // TODO: NRO/NRR tracking state
}

impl IRoInterface {
    pub fn new() -> Self {
        Self {}
    }
}
