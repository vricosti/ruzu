// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/caps/caps_sc.h
//! Port of zuyu/src/core/hle/service/caps/caps_sc.cpp
//!
//! IScreenShotControlService — "caps:sc".

/// IPC command table for IScreenShotControlService.
pub mod commands {
    pub const CAPTURE_RAW_IMAGE: u32 = 1;
    pub const CAPTURE_RAW_IMAGE_WITH_TIMEOUT: u32 = 2;
    pub const ATTACH_SHARED_BUFFER: u32 = 3;
    pub const CAPTURE_RAW_IMAGE_TO_ATTACHED_SHARED_BUFFER: u32 = 5;
    pub const UNKNOWN210: u32 = 210;
    pub const REQUEST_TAKING_SCREEN_SHOT: u32 = 1001;
    pub const REQUEST_TAKING_SCREEN_SHOT_WITH_TIMEOUT: u32 = 1002;
    pub const REQUEST_TAKING_SCREEN_SHOT_EX: u32 = 1003;
    pub const REQUEST_TAKING_SCREEN_SHOT_EX1: u32 = 1004;
    pub const CANCEL_TAKING_SCREEN_SHOT: u32 = 1009;
    pub const SET_TAKING_SCREEN_SHOT_CANCEL_STATE: u32 = 1010;
    pub const NOTIFY_TAKING_SCREEN_SHOT_REFUSED: u32 = 1011;
    pub const NOTIFY_TAKING_SCREEN_SHOT_FAILED: u32 = 1012;
    pub const SETUP_OVERLAY_MOVIE_THUMBNAIL: u32 = 1101;
    pub const OPEN_RAW_SCREEN_SHOT_READ_STREAM: u32 = 1201;
    pub const CLOSE_RAW_SCREEN_SHOT_READ_STREAM: u32 = 1202;
    pub const READ_RAW_SCREEN_SHOT_READ_STREAM: u32 = 1203;
}

/// IScreenShotControlService.
///
/// Corresponds to `IScreenShotControlService` in upstream caps_sc.h / caps_sc.cpp.
/// All commands are stubs (upstream has no implemented handlers).
pub struct IScreenShotControlService;

impl IScreenShotControlService {
    pub fn new() -> Self {
        Self
    }
}
