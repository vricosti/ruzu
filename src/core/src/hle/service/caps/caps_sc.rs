// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/caps/caps_sc.h
//! Port of zuyu/src/core/hle/service/caps/caps_sc.cpp
//!
//! IScreenShotControlService — "caps:sc".

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

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
    pub const UNKNOWN_1106: u32 = 1106;
    pub const UNKNOWN_1107: u32 = 1107;
    pub const OPEN_RAW_SCREEN_SHOT_READ_STREAM: u32 = 1201;
    pub const CLOSE_RAW_SCREEN_SHOT_READ_STREAM: u32 = 1202;
    pub const READ_RAW_SCREEN_SHOT_READ_STREAM: u32 = 1203;
    pub const UNKNOWN_1204: u32 = 1204;
}

/// IScreenShotControlService.
///
/// Corresponds to `IScreenShotControlService` in upstream caps_sc.h / caps_sc.cpp.
/// All commands are stubs (upstream has no implemented handlers).
pub struct IScreenShotControlService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IScreenShotControlService {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (1, None, "CaptureRawImage"),
            (2, None, "CaptureRawImageWithTimeout"),
            (3, None, "AttachSharedBuffer"),
            (5, None, "CaptureRawImageToAttachedSharedBuffer"),
            (210, None, "Unknown210"),
            (1001, None, "RequestTakingScreenShot"),
            (1002, None, "RequestTakingScreenShotWithTimeout"),
            (1003, None, "RequestTakingScreenShotEx"),
            (1004, None, "RequestTakingScreenShotEx1"),
            (1009, None, "CancelTakingScreenShot"),
            (1010, None, "SetTakingScreenShotCancelState"),
            (1011, None, "NotifyTakingScreenShotRefused"),
            (1012, None, "NotifyTakingScreenShotFailed"),
            (1101, None, "SetupOverlayMovieThumbnail"),
            (1106, None, "Unknown1106"),
            (1107, None, "Unknown1107"),
            (1201, None, "OpenRawScreenShotReadStream"),
            (1202, None, "CloseRawScreenShotReadStream"),
            (1203, None, "ReadRawScreenShotReadStream"),
            (1204, None, "Unknown1204"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IScreenShotControlService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "caps:sc"
    }
}

impl ServiceFramework for IScreenShotControlService {
    fn get_service_name(&self) -> &str {
        "caps:sc"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
