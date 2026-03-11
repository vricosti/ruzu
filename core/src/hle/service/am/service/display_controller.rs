// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/display_controller.h
//! Port of zuyu/src/core/hle/service/am/service/display_controller.cpp

/// IPC command table for IDisplayController:
/// - 0: GetLastForegroundCaptureImage (unimplemented)
/// - 1: UpdateLastForegroundCaptureImage (unimplemented)
/// - 2: GetLastApplicationCaptureImage (unimplemented)
/// - 3: GetCallerAppletCaptureImage (unimplemented)
/// - 4: UpdateCallerAppletCaptureImage (unimplemented)
/// - 5: GetLastForegroundCaptureImageEx (unimplemented)
/// - 6: GetLastApplicationCaptureImageEx (unimplemented)
/// - 7: GetCallerAppletCaptureImageEx
/// - 8: TakeScreenShotOfOwnLayer
/// - 9: CopyBetweenCaptureBuffers (unimplemented)
/// - 10: AcquireLastApplicationCaptureBuffer (unimplemented)
/// - 11: ReleaseLastApplicationCaptureBuffer (unimplemented)
/// - 12: AcquireLastForegroundCaptureBuffer (unimplemented)
/// - 13: ReleaseLastForegroundCaptureBuffer (unimplemented)
/// - 14: AcquireCallerAppletCaptureBuffer (unimplemented)
/// - 15: ReleaseCallerAppletCaptureBuffer (unimplemented)
/// - 16: AcquireLastApplicationCaptureBufferEx (unimplemented)
/// - 17: AcquireLastForegroundCaptureBufferEx (unimplemented)
/// - 18: AcquireCallerAppletCaptureBufferEx (unimplemented)
/// - 20: ClearCaptureBuffer
/// - 21: ClearAppletTransitionBuffer (unimplemented)
/// - 22: AcquireLastApplicationCaptureSharedBuffer
/// - 23: ReleaseLastApplicationCaptureSharedBuffer
/// - 24: AcquireLastForegroundCaptureSharedBuffer
/// - 25: ReleaseLastForegroundCaptureSharedBuffer
/// - 26: AcquireCallerAppletCaptureSharedBuffer
/// - 27: ReleaseCallerAppletCaptureSharedBuffer
/// - 28: TakeScreenShotOfOwnLayerEx (unimplemented)
pub struct IDisplayController {
    // TODO: Applet reference
}

impl IDisplayController {
    pub fn new() -> Self {
        Self {}
    }

    /// Port of IDisplayController::GetCallerAppletCaptureImageEx
    pub fn get_caller_applet_capture_image_ex(&self) -> bool {
        log::warn!("(STUBBED) GetCallerAppletCaptureImageEx called");
        true // out_was_written
    }

    /// Port of IDisplayController::TakeScreenShotOfOwnLayer
    pub fn take_screen_shot_of_own_layer(&self, _unknown0: bool, _fbshare_layer_index: i32) {
        log::warn!("(STUBBED) TakeScreenShotOfOwnLayer called");
    }

    /// Port of IDisplayController::ClearCaptureBuffer
    pub fn clear_capture_buffer(&self, _unknown0: bool, _fbshare_layer_index: i32, _color: u32) {
        log::warn!("(STUBBED) ClearCaptureBuffer called");
    }

    /// Port of IDisplayController::ReleaseLastForegroundCaptureSharedBuffer
    pub fn release_last_foreground_capture_shared_buffer(&self) {
        log::warn!("(STUBBED) ReleaseLastForegroundCaptureSharedBuffer called");
    }

    /// Port of IDisplayController::ReleaseCallerAppletCaptureSharedBuffer
    pub fn release_caller_applet_capture_shared_buffer(&self) {
        log::warn!("(STUBBED) ReleaseCallerAppletCaptureSharedBuffer called");
    }

    /// Port of IDisplayController::ReleaseLastApplicationCaptureSharedBuffer
    pub fn release_last_application_capture_shared_buffer(&self) {
        log::warn!("(STUBBED) ReleaseLastApplicationCaptureSharedBuffer called");
    }
}
