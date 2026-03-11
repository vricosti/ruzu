// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/fatal/fatal_p.h
//! Port of zuyu/src/core/hle/service/fatal/fatal_p.cpp
//!
//! IService for "fatal:p" — private fatal error interface.

/// IPC command table for fatal:p IService (all stubs, upstream has no implemented handlers).
pub mod commands {
    pub const GET_FATAL_EVENT: u32 = 0;
    pub const GET_FATAL_CONTEXT: u32 = 1;
}

/// IService for "fatal:p".
///
/// Corresponds to `IService` in upstream fatal_p.h / fatal_p.cpp.
pub struct IService;

impl IService {
    pub fn new() -> Self {
        log::debug!("fatal:p IService created");
        Self
    }
}
