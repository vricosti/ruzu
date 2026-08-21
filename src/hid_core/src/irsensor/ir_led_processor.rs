// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/irsensor/ir_led_processor.h and ir_led_processor.cpp

use super::irs_types::*;
use super::processor_base::ProcessorBase;

/// This is nn::irsensor::IrLedProcessorConfig
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct IrLedProcessorConfig {
    pub light_target: CameraLightTarget,
}
const _: () = assert!(std::mem::size_of::<IrLedProcessorConfig>() == 0x4);

/// This is nn::irsensor::IrLedProcessorState
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct IrLedProcessorState {
    pub sampling_number: i64,
    pub timestamp: u64,
    pub data: [u8; 0x8],
}
const _: () = assert!(std::mem::size_of::<IrLedProcessorState>() == 0x18);

/// IR LED processor — minimal processor that sets LED light targeting.
pub struct IrLedProcessor {
    pub base: ProcessorBase,
    current_config: IrLedProcessorConfig,
}

impl IrLedProcessor {
    pub fn new(device: &mut DeviceFormat) -> Self {
        device.mode = IrSensorMode::IrLedProcessor;
        device.camera_status = IrCameraStatus::Unconnected;
        device.camera_internal_status = IrCameraInternalStatus::Stopped;
        Self {
            base: ProcessorBase::new(),
            current_config: IrLedProcessorConfig::default(),
        }
    }

    pub fn start_processor(&mut self) {}
    pub fn suspend_processor(&mut self) {}
    pub fn stop_processor(&mut self) {}

    /// Port of IrLedProcessor::SetConfig.
    pub fn set_config(&mut self, config: PackedIrLedProcessorConfig) {
        // Safety: light_target is a u8 in packed config, cast to CameraLightTarget enum
        self.current_config.light_target =
            unsafe { std::mem::transmute::<u32, CameraLightTarget>(config.light_target as u32) };
    }
}
