// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/camera.h` and `input_common/drivers/camera.cpp`.
//!
//! Camera input driver that receives camera data and forwards it to input devices.

use common::input::{CameraFormat, CameraStatus, DriverResult};

use crate::input_engine::{InputEngine, PadIdentifier};

/// Port of `Camera` class from camera.h / camera.cpp
pub struct Camera {
    engine: InputEngine,
    status: CameraStatus,
}

impl Camera {
    /// Port of Camera::Camera
    pub fn new(input_engine: String) -> Self {
        Self {
            engine: InputEngine::new(input_engine),
            status: CameraStatus::default(),
        }
    }

    /// Returns a reference to the underlying input engine.
    pub fn engine(&self) -> &InputEngine {
        &self.engine
    }

    /// Returns a mutable reference to the underlying input engine.
    pub fn engine_mut(&mut self) -> &mut InputEngine {
        &mut self.engine
    }

    /// Port of Camera::SetCameraData
    pub fn set_camera_data(&mut self, width: usize, height: usize, data: &[u32]) {
        let desired_width = self.get_image_width();
        let desired_height = self.get_image_height();
        self.status.data.resize(desired_width * desired_height, 0);

        // Resize image to desired format
        for y in 0..desired_height {
            for x in 0..desired_width {
                let pixel_index = y * desired_width + x;
                let old_x = if desired_width > 0 {
                    width * x / desired_width
                } else {
                    0
                };
                let old_y = if desired_height > 0 {
                    height * y / desired_height
                } else {
                    0
                };
                let data_pixel_index = old_y * width + old_x;
                if data_pixel_index < data.len() {
                    self.status.data[pixel_index] = (data[data_pixel_index] & 0xFF) as u8;
                }
            }
        }

        // SetCamera(identifier, status) would forward to engine
        // Requires InputEngine::set_camera wiring
    }

    /// Port of Camera::getImageWidth
    pub fn get_image_width(&self) -> usize {
        match self.status.format {
            CameraFormat::Size320x240 => 320,
            CameraFormat::Size160x120 => 160,
            CameraFormat::Size80x60 => 80,
            CameraFormat::Size40x30 => 40,
            CameraFormat::Size20x15 => 20,
            CameraFormat::None => 0,
        }
    }

    /// Port of Camera::getImageHeight
    pub fn get_image_height(&self) -> usize {
        match self.status.format {
            CameraFormat::Size320x240 => 240,
            CameraFormat::Size160x120 => 120,
            CameraFormat::Size80x60 => 60,
            CameraFormat::Size40x30 => 30,
            CameraFormat::Size20x15 => 15,
            CameraFormat::None => 0,
        }
    }

    /// Port of Camera::SetCameraFormat (override)
    pub fn set_camera_format(
        &mut self,
        _identifier: &PadIdentifier,
        camera_format: CameraFormat,
    ) -> DriverResult {
        self.status.format = camera_format;
        DriverResult::Success
    }
}
