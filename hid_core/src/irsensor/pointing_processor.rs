// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/irsensor/pointing_processor.h and pointing_processor.cpp

use super::irs_types::*;
use super::processor_base::ProcessorBase;

/// This is nn::irsensor::PointingProcessorConfig
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct PointingProcessorConfig {
    pub window_of_interest: IrsRect,
}
const _: () = assert!(std::mem::size_of::<PointingProcessorConfig>() == 0x8);

/// This is nn::irsensor::PointingProcessorMarkerData
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct PointingProcessorMarkerData {
    pub pointing_status: u8,
    pub _padding: [u8; 3],
    pub unknown: u32,
    pub unknown_float1: f32,
    pub position_x: f32,
    pub position_y: f32,
    pub unknown_float2: f32,
    pub window_of_interest: IrsRect,
}
const _: () = assert!(std::mem::size_of::<PointingProcessorMarkerData>() == 0x20);

/// This is nn::irsensor::PointingProcessorMarkerState
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct PointingProcessorMarkerState {
    pub sampling_number: i64,
    pub timestamp: u64,
    pub data: [PointingProcessorMarkerData; 3],
}
const _: () = assert!(std::mem::size_of::<PointingProcessorMarkerState>() == 0x70);

/// Pointing processor — handles IR pointing marker detection.
pub struct PointingProcessor {
    pub base: ProcessorBase,
    current_config: PointingProcessorConfig,
}

impl PointingProcessor {
    pub fn new(device: &mut DeviceFormat) -> Self {
        device.mode = IrSensorMode::PointingProcessorMarker;
        device.camera_status = IrCameraStatus::Unconnected;
        device.camera_internal_status = IrCameraInternalStatus::Stopped;
        Self {
            base: ProcessorBase::new(),
            current_config: PointingProcessorConfig::default(),
        }
    }

    pub fn start_processor(&mut self) {}
    pub fn suspend_processor(&mut self) {}
    pub fn stop_processor(&mut self) {}

    /// Port of PointingProcessor::SetConfig.
    pub fn set_config(&mut self, config: PackedPointingProcessorConfig) {
        self.current_config.window_of_interest = config.window_of_interest;
    }
}
