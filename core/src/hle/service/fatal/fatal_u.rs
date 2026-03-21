// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/fatal/fatal_u.h
//! Port of zuyu/src/core/hle/service/fatal/fatal_u.cpp
//!
//! Fatal_U -- "fatal:u" service interface.
//! Provides user-facing fatal error reporting (ThrowFatal, ThrowFatalWithPolicy,
//! ThrowFatalWithCpuContext).

use std::sync::Arc;

/// IPC command table for fatal:u.
///
/// Corresponds to the function table in `Fatal_U` constructor (upstream fatal_u.cpp).
pub mod commands {
    pub const THROW_FATAL: u32 = 0;
    pub const THROW_FATAL_WITH_POLICY: u32 = 1;
    pub const THROW_FATAL_WITH_CPU_CONTEXT: u32 = 2;
}

/// Fatal_U service.
///
/// Corresponds to `Fatal_U` in upstream fatal_u.h / fatal_u.cpp.
/// Commands delegate to Module::Interface methods in fatal.rs.
pub struct FatalU {
    pub interface: super::fatal::Interface,
}

impl FatalU {
    pub fn new(module: Arc<super::fatal::Module>) -> Self {
        log::debug!("fatal:u created");
        Self {
            interface: super::fatal::Interface::new(crate::core::SystemRef::null(), module, "fatal:u"),
        }
    }
}
