// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/irsensor/image_transfer_processor.h and image_transfer_processor.cpp

use super::irs_types::*;
use super::processor_base::ProcessorBase;

/// This is nn::irsensor::ImageTransferProcessorExConfig (unpacked)
#[derive(Debug, Clone, Copy)]
pub struct ImageTransferProcessorExConfig {
    pub camera_config: CameraConfig,
    pub origin_format: ImageTransferProcessorFormat,
    pub trimming_format: ImageTransferProcessorFormat,
    pub trimming_start_x: u16,
    pub trimming_start_y: u16,
    pub is_external_light_filter_enabled: bool,
}

impl Default for ImageTransferProcessorExConfig {
    fn default() -> Self {
        Self {
            camera_config: CameraConfig {
                exposure_time: 0,
                light_target: CameraLightTarget::AllLeds,
                gain: 0,
                is_negative_used: false,
                _padding: [0; 7],
            },
            origin_format: ImageTransferProcessorFormat::Size320x240,
            trimming_format: ImageTransferProcessorFormat::Size320x240,
            trimming_start_x: 0,
            trimming_start_y: 0,
            is_external_light_filter_enabled: false,
        }
    }
}

/// Image transfer processor — captures camera images and writes them to
/// transfer memory, optionally trimming to a sub-rectangle.
pub struct ImageTransferProcessor {
    pub base: ProcessorBase,
    current_config: ImageTransferProcessorExConfig,
    processor_state: ImageTransferProcessorState,
    transfer_memory: u64,
}

impl ImageTransferProcessor {
    pub fn new(device: &mut DeviceFormat) -> Self {
        device.mode = IrSensorMode::ImageTransferProcessor;
        device.camera_status = IrCameraStatus::Unconnected;
        device.camera_internal_status = IrCameraInternalStatus::Stopped;
        Self {
            base: ProcessorBase::new(),
            current_config: ImageTransferProcessorExConfig::default(),
            processor_state: ImageTransferProcessorState::default(),
            transfer_memory: 0,
        }
    }

    pub fn start_processor(&mut self) {
        self.base.is_active = true;
        self.processor_state.sampling_number = 0;
        self.processor_state.ambient_noise_level = CameraAmbientNoiseLevel::Low;
    }

    pub fn suspend_processor(&mut self) {}
    pub fn stop_processor(&mut self) {}

    /// Port of ImageTransferProcessor::SetConfig (basic).
    pub fn set_config(&mut self, config: PackedImageTransferProcessorConfig) {
        self.current_config.camera_config.exposure_time = config.camera_config.exposure_time;
        self.current_config.camera_config.gain = config.camera_config.gain as u32;
        self.current_config.camera_config.is_negative_used =
            config.camera_config.is_negative_used;
        self.current_config.camera_config.light_target = unsafe {
            std::mem::transmute::<u32, CameraLightTarget>(
                config.camera_config.light_target as u32,
            )
        };
        self.current_config.origin_format = unsafe {
            std::mem::transmute::<u32, ImageTransferProcessorFormat>(config.format as u32)
        };
        self.current_config.trimming_format = self.current_config.origin_format;
        self.current_config.trimming_start_x = 0;
        self.current_config.trimming_start_y = 0;
    }

    /// Port of ImageTransferProcessor::SetConfig (extended).
    pub fn set_config_ex(&mut self, config: PackedImageTransferProcessorExConfig) {
        self.current_config.camera_config.exposure_time = config.camera_config.exposure_time;
        self.current_config.camera_config.gain = config.camera_config.gain as u32;
        self.current_config.camera_config.is_negative_used =
            config.camera_config.is_negative_used;
        self.current_config.camera_config.light_target = unsafe {
            std::mem::transmute::<u32, CameraLightTarget>(
                config.camera_config.light_target as u32,
            )
        };
        self.current_config.origin_format = unsafe {
            std::mem::transmute::<u32, ImageTransferProcessorFormat>(
                config.origin_format as u32,
            )
        };
        self.current_config.trimming_format = unsafe {
            std::mem::transmute::<u32, ImageTransferProcessorFormat>(
                config.trimming_format as u32,
            )
        };
        self.current_config.trimming_start_x = config.trimming_start_x;
        self.current_config.trimming_start_y = config.trimming_start_y;
    }

    /// Port of ImageTransferProcessor::SetTransferMemoryAddress.
    pub fn set_transfer_memory_address(&mut self, t_mem: u64) {
        self.transfer_memory = t_mem;
    }

    /// Port of ImageTransferProcessor::GetState.
    pub fn get_state(&self) -> ImageTransferProcessorState {
        self.processor_state
    }
}
