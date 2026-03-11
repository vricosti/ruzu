// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/account_proxy_interface.cpp/.h
//!
//! IAccountProxyInterface - stub service with command table.

/// IPC command table for IAccountProxyInterface ("IAccountProxyInterface").
/// All commands are stubbed (nullptr in upstream).
pub const IACCOUNT_PROXY_INTERFACE_COMMANDS: &[(u32, &str)] = &[
    (0, "CreateUserAccount"),
];
