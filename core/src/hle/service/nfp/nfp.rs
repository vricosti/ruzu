// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nfp/nfp.h
//! Port of zuyu/src/core/hle/service/nfp/nfp.cpp
//!
//! NFP service — "nfp:user" and "nfp:sys" service registration.

/// IPC command table for IUserManager / ISystemManager.
pub mod commands {
    pub const CREATE_USER_INTERFACE: u32 = 0;
}

/// LoopProcess — registers "nfp:user", "nfp:sys", "nfp:dbg" services.
///
/// Corresponds to `Service::NFP::LoopProcess` in upstream nfp.cpp.
pub fn loop_process() {
    log::debug!("NFP::LoopProcess called");
    // TODO: Register nfp:user, nfp:sys, nfp:dbg with ServerManager
}
