// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/irsensor/moment_processor.h and moment_processor.cpp

use super::irs_types::*;
use super::processor_base::ProcessorBase;

const COLUMNS: usize = 8;
const ROWS: usize = 6;
const IMAGE_WIDTH: usize = 40;
const IMAGE_HEIGHT: usize = 30;
const THRESHOLD: usize = 30;
const REAL_WIDTH: usize = 320;
const REAL_HEIGHT: usize = 240;

/// This is nn::irsensor::MomentStatistic
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct MomentStatistic {
    pub average_intensity: f32,
    pub centroid: IrsCentroid,
}
const _: () = assert!(std::mem::size_of::<MomentStatistic>() == 0xC);

/// This is nn::irsensor::MomentProcessorState
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct MomentProcessorState {
    pub sampling_number: i64,
    pub timestamp: u64,
    pub ambient_noise_level: CameraAmbientNoiseLevel,
    pub _padding: [u8; 4],
    pub statistic: [MomentStatistic; COLUMNS * ROWS],
}

impl Default for MomentProcessorState {
    fn default() -> Self {
        Self {
            sampling_number: 0,
            timestamp: 0,
            ambient_noise_level: CameraAmbientNoiseLevel::Low,
            _padding: [0; 4],
            statistic: [MomentStatistic::default(); COLUMNS * ROWS],
        }
    }
}

/// This is nn::irsensor::MomentProcessorConfig (unpacked)
#[derive(Debug, Clone, Copy)]
pub struct MomentProcessorConfig {
    pub camera_config: CameraConfig,
    pub window_of_interest: IrsRect,
    pub preprocess: MomentProcessorPreprocess,
    pub preprocess_intensity_threshold: u32,
}

impl Default for MomentProcessorConfig {
    fn default() -> Self {
        Self {
            camera_config: CameraConfig {
                exposure_time: 0,
                light_target: CameraLightTarget::AllLeds,
                gain: 0,
                is_negative_used: false,
                _padding: [0; 7],
            },
            window_of_interest: IrsRect::default(),
            preprocess: MomentProcessorPreprocess::Unknown0,
            preprocess_intensity_threshold: 0,
        }
    }
}

/// Moment processor — computes intensity and centroid statistics over an 8x6
/// grid of the camera image. Matches upstream moment_processor.cpp.
pub struct MomentProcessor {
    pub base: ProcessorBase,
    current_config: MomentProcessorConfig,
    next_state: MomentProcessorState,
}

impl MomentProcessor {
    pub fn new(device: &mut DeviceFormat) -> Self {
        device.mode = IrSensorMode::MomentProcessor;
        device.camera_status = IrCameraStatus::Unconnected;
        device.camera_internal_status = IrCameraInternalStatus::Stopped;
        Self {
            base: ProcessorBase::new(),
            current_config: MomentProcessorConfig::default(),
            next_state: MomentProcessorState::default(),
        }
    }

    pub fn start_processor(&mut self) {
        // Upstream sets camera status to Available/Ready
    }

    pub fn suspend_processor(&mut self) {}
    pub fn stop_processor(&mut self) {}

    /// Port of MomentProcessor::GetPixel.
    fn get_pixel(data: &[u8], x: usize, y: usize) -> u8 {
        let index = (y * IMAGE_WIDTH) + x;
        if index >= data.len() {
            return 0;
        }
        data[index]
    }

    /// Port of MomentProcessor::GetStatistic — computes average intensity and
    /// centroid for a rectangular block within the image.
    fn get_statistic(
        data: &[u8],
        start_x: usize,
        start_y: usize,
        width: usize,
        height: usize,
    ) -> MomentStatistic {
        let mut statistic = MomentStatistic::default();
        let mut active_points: usize = 0;

        for y in 0..width {
            for x in 0..height {
                let x_pos = x + start_x;
                let y_pos = y + start_y;
                let pixel = Self::get_pixel(
                    data,
                    x_pos * IMAGE_WIDTH / REAL_WIDTH,
                    y_pos * IMAGE_HEIGHT / REAL_HEIGHT,
                );

                if (pixel as usize) < THRESHOLD {
                    continue;
                }

                statistic.average_intensity += pixel as f32;
                statistic.centroid.x += x_pos as f32;
                statistic.centroid.y += y_pos as f32;
                active_points += 1;
            }
        }

        if active_points == 0 {
            return MomentStatistic::default();
        }

        statistic.centroid.x /= active_points as f32;
        statistic.centroid.y /= active_points as f32;
        statistic.average_intensity /= (width * height) as f32;

        statistic
    }

    /// Process camera data into moment statistics (called from controller
    /// update callback in full system).
    pub fn on_controller_update(&mut self, camera_data: &[u8], sample: i64, timestamp: u64) {
        self.next_state = MomentProcessorState::default();

        let window_width = self.current_config.window_of_interest.width as usize;
        let window_height = self.current_config.window_of_interest.height as usize;
        let window_start_x = self.current_config.window_of_interest.x as usize;
        let window_start_y = self.current_config.window_of_interest.y as usize;

        if window_width == 0 || window_height == 0 {
            return;
        }

        let block_width = window_width / COLUMNS;
        let block_height = window_height / ROWS;

        if block_width == 0 || block_height == 0 {
            return;
        }

        for row in 0..ROWS {
            for column in 0..COLUMNS {
                let x_pos = (column * block_width) + window_start_x;
                let y_pos = (row * block_height) + window_start_y;
                self.next_state.statistic[column + (row * COLUMNS)] =
                    Self::get_statistic(camera_data, x_pos, y_pos, block_width, block_height);
            }
        }

        self.next_state.sampling_number = sample;
        self.next_state.timestamp = timestamp;
        self.next_state.ambient_noise_level = CameraAmbientNoiseLevel::Low;
    }

    /// Port of MomentProcessor::SetConfig.
    pub fn set_config(&mut self, config: PackedMomentProcessorConfig) {
        self.current_config.camera_config.exposure_time = config.camera_config.exposure_time;
        self.current_config.camera_config.gain = config.camera_config.gain as u32;
        self.current_config.camera_config.is_negative_used = config.camera_config.is_negative_used;
        self.current_config.camera_config.light_target = unsafe {
            std::mem::transmute::<u32, CameraLightTarget>(config.camera_config.light_target as u32)
        };
        self.current_config.window_of_interest = config.window_of_interest;
        self.current_config.preprocess = unsafe {
            std::mem::transmute::<u32, MomentProcessorPreprocess>(config.preprocess as u32)
        };
        self.current_config.preprocess_intensity_threshold =
            config.preprocess_intensity_threshold as u32;
    }
}
