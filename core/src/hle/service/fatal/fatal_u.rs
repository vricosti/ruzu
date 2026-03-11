// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/fatal/fatal_u.h
//! Port of zuyu/src/core/hle/service/fatal/fatal_u.cpp
//!
//! IService for "fatal:u" — user-facing fatal error interface.

/// IPC command table for fatal:u IService.
pub mod commands {
    pub const THROW_FATAL: u32 = 0;
    pub const THROW_FATAL_WITH_POLICY: u32 = 1;
    pub const THROW_FATAL_WITH_CPU_CONTEXT: u32 = 2;
}

/// IService for "fatal:u".
///
/// Corresponds to `IService` in upstream fatal_u.h / fatal_u.cpp.
pub struct IService;

impl IService {
    pub fn new() -> Self {
        log::debug!("fatal:u IService created");
        Self
    }

    /// ThrowFatalWithPolicy (cmd 1).
    ///
    /// Upstream logs the fatal error but does not crash the emulator.
    pub fn throw_fatal_with_policy(&self, result_code: u32, policy: u32) {
        log::error!(
            "fatal:u ThrowFatalWithPolicy called, result_code=0x{:08X}, policy={}",
            result_code,
            policy
        );
        // TODO: propagate to fatal error reporter
    }

    /// ThrowFatalWithCpuContext (cmd 2).
    pub fn throw_fatal_with_cpu_context(&self, result_code: u32, policy: u32) {
        log::error!(
            "fatal:u ThrowFatalWithCpuContext called, result_code=0x{:08X}, policy={}",
            result_code,
            policy
        );
        // TODO: propagate to fatal error reporter
    }
}
