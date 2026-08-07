// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/camera.h` and `input_common/drivers/camera.cpp`.
//!
//! Camera input driver that receives camera data and forwards it to input devices.

use std::sync::{Arc, Weak};

use common::input::{CameraFormat, CameraStatus, DriverResult};
use common::uuid::UUID;
use parking_lot::Mutex;

use crate::input_engine::{InputEngine, InputEngineOutput, PadIdentifier};

fn camera_identifier() -> PadIdentifier {
    PadIdentifier {
        guid: UUID::new(),
        port: 0,
        pad: 0,
    }
}

/// Port of `Camera` class from camera.h / camera.cpp
#[derive(Clone)]
pub struct Camera {
    engine: Arc<Mutex<InputEngine>>,
    status: Arc<Mutex<CameraStatus>>,
}

struct CameraOutput {
    status: Weak<Mutex<CameraStatus>>,
}

impl InputEngineOutput for CameraOutput {
    fn set_camera_format(
        &self,
        _identifier: &PadIdentifier,
        camera_format: CameraFormat,
    ) -> DriverResult {
        let Some(status) = self.status.upgrade() else {
            return DriverResult::NotSupported;
        };
        status.lock().format = camera_format;
        DriverResult::Success
    }
}

impl Camera {
    /// Port of Camera::Camera
    pub fn new(input_engine: String) -> Self {
        let engine = Arc::new(Mutex::new(InputEngine::new(input_engine)));
        let status = Arc::new(Mutex::new(CameraStatus::default()));
        {
            let mut engine = engine.lock();
            engine.pre_set_controller(&camera_identifier());
            engine.set_output_handler(Arc::new(CameraOutput {
                status: Arc::downgrade(&status),
            }));
        }
        Self { engine, status }
    }

    /// Returns the shared underlying input engine.
    pub fn engine(&self) -> Arc<Mutex<InputEngine>> {
        Arc::clone(&self.engine)
    }

    /// Port of Camera::SetCameraData
    pub fn set_camera_data(&mut self, width: usize, height: usize, data: &[u32]) {
        let desired_width = self.get_image_width();
        let desired_height = self.get_image_height();
        let status = {
            let mut status = self.status.lock();
            status.data.resize(desired_width * desired_height, 0);

            // Resize image to desired format.
            for y in 0..desired_height {
                for x in 0..desired_width {
                    let pixel_index = y * desired_width + x;
                    let old_x = width * x / desired_width;
                    let old_y = height * y / desired_height;
                    let data_pixel_index = old_y * width + old_x;
                    status.data[pixel_index] = (data[data_pixel_index] & 0xFF) as u8;
                }
            }
            status.clone()
        };
        let callbacks = self.engine.lock().set_camera(&camera_identifier(), &status);
        callbacks.dispatch();
    }

    /// Port of Camera::getImageWidth
    pub fn get_image_width(&self) -> usize {
        match self.status.lock().format {
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
        match self.status.lock().format {
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
        self.status.lock().format = camera_format;
        DriverResult::Success
    }
}
