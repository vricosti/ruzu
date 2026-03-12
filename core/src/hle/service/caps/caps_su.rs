// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/caps/caps_su.h
//! Port of zuyu/src/core/hle/service/caps/caps_su.cpp
//!
//! IScreenShotApplicationService — "caps:su".

use crate::hle::result::ResultCode;
use super::caps_types::{
    AlbumReportOption, ApplicationAlbumEntry, ApplicationData, ScreenShotAttribute,
    ShimLibraryVersion,
};

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
    /// Internal buffer for CaptureAndSaveScreenshot.
    image_data: Vec<u8>,
    // TODO: AlbumManager reference
}

impl IScreenShotApplicationService {
    pub fn new() -> Self {
        Self {
            image_data: vec![0u8; SCREENSHOT_WIDTH * SCREENSHOT_HEIGHT * BYTES_PER_PIXEL],
        }
    }

    /// SetShimLibraryVersion (cmd 32).
    ///
    /// Corresponds to upstream `SetShimLibraryVersion`.
    pub fn set_shim_library_version(
        &self,
        library_version: u64,
        aruid: u64,
    ) -> Result<(), ResultCode> {
        log::warn!(
            "(STUBBED) SetShimLibraryVersion called. library_version={}, applet_resource_user_id={}",
            library_version,
            aruid,
        );
        Ok(())
    }

    /// SaveScreenShotEx0 (cmd 203).
    ///
    /// Corresponds to upstream `SaveScreenShotEx0`.
    pub fn save_screen_shot_ex0(
        &self,
        _attribute: &ScreenShotAttribute,
        report_option: AlbumReportOption,
        aruid: u64,
        image_data_buffer: &[u8],
    ) -> Result<ApplicationAlbumEntry, ResultCode> {
        log::info!(
            "SaveScreenShotEx0 called, report_option={:?}, image_data_buffer_size={}, applet_resource_user_id={}",
            report_option,
            image_data_buffer.len(),
            aruid,
        );

        // TODO: manager.flip_vertically_on_write(false);
        // TODO: manager.save_screen_shot(...)
        let entry = ApplicationAlbumEntry::default();
        Ok(entry)
    }

    /// SaveScreenShotEx1 (cmd 205).
    ///
    /// Corresponds to upstream `SaveScreenShotEx1`.
    pub fn save_screen_shot_ex1(
        &self,
        _attribute: &ScreenShotAttribute,
        report_option: AlbumReportOption,
        aruid: u64,
        _app_data_buffer: &ApplicationData,
        image_data_buffer: &[u8],
    ) -> Result<ApplicationAlbumEntry, ResultCode> {
        log::info!(
            "SaveScreenShotEx1 called, report_option={:?}, image_data_buffer_size={}, applet_resource_user_id={}",
            report_option,
            image_data_buffer.len(),
            aruid,
        );

        // TODO: manager.flip_vertically_on_write(false);
        // TODO: manager.save_screen_shot(...)
        let entry = ApplicationAlbumEntry::default();
        Ok(entry)
    }

    /// CaptureAndSaveScreenshot — captures the current framebuffer and saves it.
    ///
    /// Corresponds to upstream `CaptureAndSaveScreenshot`.
    pub fn capture_and_save_screenshot(&mut self, _report_option: AlbumReportOption) {
        // In upstream, this:
        //   1. Creates a DefaultFrameLayout(1280, 720)
        //   2. Creates a default ScreenShotAttribute with AlbumImageOrientation::None
        //   3. Calls renderer.RequestScreenshot with a callback that:
        //      a. Converts BGRA to RGBA by swapping image_data[i] and image_data[i+2]
        //      b. Calls manager.SaveScreenShot
        //
        // We stub the renderer interaction since it requires GPU integration.
        log::warn!("CaptureAndSaveScreenshot (STUBBED) called");

        // Convert BGRA to RGBA (matching upstream callback logic).
        let len = self.image_data.len();
        let mut i = 0;
        while i < len {
            self.image_data.swap(i, i + 2);
            i += BYTES_PER_PIXEL;
        }

        // TODO: Call manager.save_screen_shot with the converted data.
    }
}
