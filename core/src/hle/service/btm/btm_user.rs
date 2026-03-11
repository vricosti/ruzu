// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/btm/btm_user.h
//! Port of zuyu/src/core/hle/service/btm/btm_user.cpp
//!
//! IBtmUser — "btm:u".

/// IPC command table for IBtmUser.
pub mod commands {
    pub const GET_CORE: u32 = 0;
}

/// IBtmUser.
pub struct IBtmUser;

impl IBtmUser {
    pub fn new() -> Self {
        Self
    }
}
