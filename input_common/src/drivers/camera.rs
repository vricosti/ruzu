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
    pub fn set_camera_data(&mut self, _width: usize, _height: usize, _data: &[u32]) {
        todo!()
    }

    /// Port of Camera::getImageWidth
    pub fn get_image_width(&self) -> usize {
        todo!()
    }

    /// Port of Camera::getImageHeight
    pub fn get_image_height(&self) -> usize {
        todo!()
    }

    /// Port of Camera::SetCameraFormat (override)
    pub fn set_camera_format(
        &mut self,
        _identifier: &PadIdentifier,
        _camera_format: CameraFormat,
    ) -> DriverResult {
        todo!()
    }
}
