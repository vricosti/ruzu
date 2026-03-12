// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/fatal/fatal_p.h
//! Port of zuyu/src/core/hle/service/fatal/fatal_p.cpp
//!
//! Fatal_P -- "fatal:p" service interface.
//! This service provides private fatal error query interface.

use std::sync::Arc;

/// IPC command table for fatal:p.
///
/// Corresponds to the function table in `Fatal_P` constructor (upstream fatal_p.cpp).
pub mod commands {
    pub const GET_FATAL_EVENT: u32 = 0;
    pub const GET_FATAL_CONTEXT: u32 = 1;
}

/// Fatal_P service.
///
/// Corresponds to `Fatal_P` in upstream fatal_p.h / fatal_p.cpp.
/// Both commands are nullptr (unimplemented) in upstream.
pub struct FatalP {
    pub module: Arc<super::fatal::Module>,
}

impl FatalP {
    pub fn new(module: Arc<super::fatal::Module>) -> Self {
        log::debug!("fatal:p created");
        Self { module }
    }
}
