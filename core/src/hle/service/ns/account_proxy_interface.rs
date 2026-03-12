// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/account_proxy_interface.h
//! Port of zuyu/src/core/hle/service/ns/account_proxy_interface.cpp
//!
//! IAccountProxyInterface — account proxy operations for NS.

/// IPC command table for IAccountProxyInterface.
///
/// Corresponds to the function table in upstream account_proxy_interface.cpp.
pub mod commands {
    pub const CREATE_USER_ACCOUNT: u32 = 0;
}

/// IAccountProxyInterface.
///
/// Corresponds to `IAccountProxyInterface` in upstream.
pub struct IAccountProxyInterface;

impl IAccountProxyInterface {
    pub fn new() -> Self {
        Self
    }
}
