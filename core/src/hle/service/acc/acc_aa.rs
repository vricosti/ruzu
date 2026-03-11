// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/acc/acc_aa.h
//! Port of zuyu/src/core/hle/service/acc/acc_aa.cpp
//!
//! ACC_AA service ("acc:aa").

/// IPC command IDs for ACC_AA
pub mod commands {
    pub const ENSURE_CACHE_ASYNC: u32 = 0;
    pub const LOAD_CACHE: u32 = 1;
    pub const GET_DEVICE_ACCOUNT_ID: u32 = 2;
    pub const REGISTER_NOTIFICATION_TOKEN_ASYNC: u32 = 50;  // 1.0.0 - 6.2.0
    pub const UNREGISTER_NOTIFICATION_TOKEN_ASYNC: u32 = 51; // 1.0.0 - 6.2.0
}

/// ACC_AA service.
///
/// Corresponds to `ACC_AA` in upstream `acc_aa.h`.
/// All handlers are nullptr (unimplemented) in upstream.
pub struct AccAA {
    pub interface: super::acc::Interface,
}

impl AccAA {
    pub fn new() -> Self {
        Self {
            interface: super::acc::Interface::new("acc:aa"),
        }
    }
}
