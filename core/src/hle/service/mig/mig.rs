// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mig/mig.cpp
//!
//! MIG_USR service ("mig:user"). All commands are unimplemented stubs.

/// IPC command IDs for MIG_USR
pub mod commands {
    pub const TRY_GET_LAST_MIGRATION_INFO: u32 = 10;
    pub const CREATE_SERVER: u32 = 100;
    pub const RESUME_SERVER: u32 = 101;
    pub const CREATE_CLIENT: u32 = 200;
    pub const RESUME_CLIENT: u32 = 201;
    pub const UNKNOWN_1001: u32 = 1001;
    pub const UNKNOWN_1010: u32 = 1010;
    pub const UNKNOWN_1100: u32 = 1100;
    pub const UNKNOWN_1101: u32 = 1101;
    pub const UNKNOWN_1200: u32 = 1200;
    pub const UNKNOWN_1201: u32 = 1201;
}

/// MIG_USR service ("mig:user"). All stubs.
pub struct MigUsr;

impl MigUsr {
    pub fn new() -> Self {
        Self
    }
}

/// Registers "mig:user" service.
///
/// Corresponds to `LoopProcess` in upstream `mig.cpp`.
pub fn loop_process() {
    // TODO: register "mig:user" -> MigUsr with ServerManager
}
