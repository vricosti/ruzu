// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/caps/caps_su.h
//! Port of zuyu/src/core/hle/service/caps/caps_su.cpp
//!
//! IScreenShotApplicationService — "caps:su".

/// IPC command table for IScreenShotApplicationService.
pub mod commands {
    pub const SET_SHIM_LIBRARY_VERSION: u32 = 32;
    pub const SAVE_SCREEN_SHOT: u32 = 201;
    pub const SAVE_SCREEN_SHOT_EX0: u32 = 203;
    pub const SAVE_SCREEN_SHOT_EX1: u32 = 205;
    pub const SAVE_SCREEN_SHOT_EX2: u32 = 210;
}

/// Screenshot dimensions (upstream constants).
pub const SCREENSHOT_WIDTH: usize = 1280;
pub const SCREENSHOT_HEIGHT: usize = 720;
pub const BYTES_PER_PIXEL: usize = 4;

/// IScreenShotApplicationService.
///
/// Corresponds to `IScreenShotApplicationService` in upstream caps_su.h / caps_su.cpp.
pub struct IScreenShotApplicationService {
    // TODO: AlbumManager reference, image_data buffer
}

impl IScreenShotApplicationService {
    pub fn new() -> Self {
        Self {}
    }
}
