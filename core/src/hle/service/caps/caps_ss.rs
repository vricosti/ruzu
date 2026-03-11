// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/caps/caps_ss.h
//! Port of zuyu/src/core/hle/service/caps/caps_ss.cpp
//!
//! IScreenShotService — "caps:ss".

/// IPC command table for IScreenShotService.
pub mod commands {
    pub const SAVE_SCREEN_SHOT: u32 = 201;
    pub const SAVE_EDITED_SCREEN_SHOT: u32 = 202;
    pub const SAVE_SCREEN_SHOT_EX0: u32 = 203;
    pub const SAVE_EDITED_SCREEN_SHOT_EX0: u32 = 204;
    pub const SAVE_EDITED_SCREEN_SHOT_EX1: u32 = 206;
    pub const SAVE_SCREEN_SHOT_OF_MOVIE_EX1: u32 = 208;
    pub const UNKNOWN_1000: u32 = 1000;
}

/// IScreenShotService.
///
/// Corresponds to `IScreenShotService` in upstream caps_ss.h / caps_ss.cpp.
pub struct IScreenShotService {
    // TODO: AlbumManager reference
}

impl IScreenShotService {
    pub fn new() -> Self {
        Self {}
    }
}
