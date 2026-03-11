// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/pcie/pcie.cpp
//!
//! PCIe service and ISession. All commands are unimplemented stubs.

/// IPC command IDs for ISession
pub mod session_commands {
    pub const QUERY_FUNCTIONS: u32 = 0;
    pub const ACQUIRE_FUNCTION: u32 = 1;
    pub const RELEASE_FUNCTION: u32 = 2;
    pub const GET_FUNCTION_STATE: u32 = 3;
    pub const GET_BAR_PROFILE: u32 = 4;
    pub const READ_CONFIG: u32 = 5;
    pub const WRITE_CONFIG: u32 = 6;
    pub const READ_BAR_REGION: u32 = 7;
    pub const WRITE_BAR_REGION: u32 = 8;
    pub const FIND_CAPABILITY: u32 = 9;
    pub const FIND_EXTENDED_CAPABILITY: u32 = 10;
    pub const MAP_DMA: u32 = 11;
    pub const UNMAP_DMA: u32 = 12;
    pub const RESET_FUNCTION: u32 = 22;
}

/// IPC command IDs for PCIe
pub mod pcie_commands {
    pub const REGISTER_CLASS_DRIVER: u32 = 0;
    pub const QUERY_FUNCTIONS_UNREGISTERED: u32 = 1;
}

pub struct ISession;
impl ISession {
    pub fn new() -> Self { Self }
}

pub struct PCIe;
impl PCIe {
    pub fn new() -> Self { Self }
}

/// Registers "pcie" service.
///
/// Corresponds to `LoopProcess` in upstream `pcie.cpp`.
pub fn loop_process() {
    // TODO: register "pcie" -> PCIe with ServerManager
}
