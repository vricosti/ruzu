// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/btm/btm_system.h
//! Port of zuyu/src/core/hle/service/btm/btm_system.cpp
//!
//! IBtmSystem — "btm:sys".

/// IPC command table for IBtmSystem.
pub mod commands {
    pub const GET_CORE: u32 = 0;
}

/// IBtmSystem.
pub struct IBtmSystem;

impl IBtmSystem {
    pub fn new() -> Self {
        Self
    }
}
