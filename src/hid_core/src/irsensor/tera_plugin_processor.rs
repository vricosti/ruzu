// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/irsensor/tera_plugin_processor.h and tera_plugin_processor.cpp

use super::irs_types::*;
use super::processor_base::ProcessorBase;

/// This is nn::irsensor::TeraPluginProcessorConfig
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct TeraPluginProcessorConfig {
    pub mode: u8,
    pub unknown_1: u8,
    pub unknown_2: u8,
    pub unknown_3: u8,
}
const _: () = assert!(std::mem::size_of::<TeraPluginProcessorConfig>() == 0x4);

/// This is nn::irsensor::TeraPluginProcessorState
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct TeraPluginProcessorState {
    pub sampling_number: i64,
    pub timestamp: u64,
    pub ambient_noise_level: CameraAmbientNoiseLevel,
    pub data: [u8; 0x12c],
}

/// Tera plugin processor — handles Tera (game-specific) IR sensor processing.
pub struct TeraPluginProcessor {
    pub base: ProcessorBase,
    current_config: TeraPluginProcessorConfig,
}

impl TeraPluginProcessor {
    pub fn new(device: &mut DeviceFormat) -> Self {
        device.mode = IrSensorMode::TeraPluginProcessor;
        device.camera_status = IrCameraStatus::Unconnected;
        device.camera_internal_status = IrCameraInternalStatus::Stopped;
        Self {
            base: ProcessorBase::new(),
            current_config: TeraPluginProcessorConfig::default(),
        }
    }

    pub fn start_processor(&mut self) {}
    pub fn suspend_processor(&mut self) {}
    pub fn stop_processor(&mut self) {}

    /// Port of TeraPluginProcessor::SetConfig.
    pub fn set_config(&mut self, config: PackedTeraPluginProcessorConfig) {
        self.current_config.mode = config.mode;
        self.current_config.unknown_1 = config.unknown_1;
        self.current_config.unknown_2 = config.unknown_2;
        self.current_config.unknown_3 = config.unknown_3;
    }
}
