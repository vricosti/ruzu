// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/irsensor/irs_types.h

use crate::hid_types::NpadStyleIndex;

/// This is nn::irsensor::CameraAmbientNoiseLevel
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum CameraAmbientNoiseLevel {
    #[default]
    Low = 0,
    Medium = 1,
    High = 2,
    Unknown3 = 3,
}

/// This is nn::irsensor::CameraLightTarget
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum CameraLightTarget {
    #[default]
    AllLeds = 0,
    BrightLeds = 1,
    DimLeds = 2,
    None = 3,
}

/// This is nn::irsensor::PackedCameraLightTarget
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u8)]
pub enum PackedCameraLightTarget {
    #[default]
    AllLeds = 0,
    BrightLeds = 1,
    DimLeds = 2,
    None = 3,
}

/// This is nn::irsensor::ImageTransferProcessorFormat
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum ImageTransferProcessorFormat {
    #[default]
    Size320x240 = 0,
    Size160x120 = 1,
    Size80x60 = 2,
    Size40x30 = 3,
    Size20x15 = 4,
}

/// This is nn::irsensor::PackedImageTransferProcessorFormat
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u8)]
pub enum PackedImageTransferProcessorFormat {
    #[default]
    Size320x240 = 0,
    Size160x120 = 1,
    Size80x60 = 2,
    Size40x30 = 3,
    Size20x15 = 4,
}

/// This is nn::irsensor::IrCameraStatus
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum IrCameraStatus {
    Available = 0,
    Unsupported = 1,
    #[default]
    Unconnected = 2,
}

/// This is nn::irsensor::IrCameraInternalStatus
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum IrCameraInternalStatus {
    Stopped = 0,
    FirmwareUpdateNeeded = 1,
    Unknown2 = 2,
    Unknown3 = 3,
    Unknown4 = 4,
    FirmwareVersionRequested = 5,
    FirmwareVersionIsInvalid = 6,
    #[default]
    Ready = 7,
    Setting = 8,
}

/// This is nn::irsensor::detail::StatusManager::IrSensorMode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u64)]
pub enum IrSensorMode {
    #[default]
    None = 0,
    MomentProcessor = 1,
    ClusteringProcessor = 2,
    ImageTransferProcessor = 3,
    PointingProcessorMarker = 4,
    TeraPluginProcessor = 5,
    IrLedProcessor = 6,
}

/// This is nn::irsensor::ImageProcessorStatus
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum ImageProcessorStatus {
    #[default]
    Stopped = 0,
    Running = 1,
}

/// This is nn::irsensor::HandAnalysisMode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum HandAnalysisMode {
    #[default]
    None = 0,
    Silhouette = 1,
    Image = 2,
    SilhouetteAndImage = 3,
    SilhouetteOnly = 4,
}

/// This is nn::irsensor::IrSensorFunctionLevel
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u8)]
pub enum IrSensorFunctionLevel {
    #[default]
    Unknown0 = 0,
    Unknown1 = 1,
    Unknown2 = 2,
    Unknown3 = 3,
    Unknown4 = 4,
}

/// This is nn::irsensor::MomentProcessorPreprocess
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum MomentProcessorPreprocess {
    #[default]
    Unknown0 = 0,
    Unknown1 = 1,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct IrsRect {
    pub x: i16,
    pub y: i16,
    pub width: i16,
    pub height: i16,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct IrsCentroid {
    pub x: f32,
    pub y: f32,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct CameraConfig {
    pub exposure_time: u64,
    pub light_target: CameraLightTarget,
    pub gain: u32,
    pub is_negative_used: bool,
    pub _padding: [u8; 7],
}
const _: () = assert!(std::mem::size_of::<CameraConfig>() == 0x18);

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct PackedCameraConfig {
    pub exposure_time: u64,
    pub light_target: PackedCameraLightTarget,
    pub gain: u8,
    pub is_negative_used: bool,
    pub _padding: [u8; 5],
}
const _: () = assert!(std::mem::size_of::<PackedCameraConfig>() == 0x10);

/// This is nn::irsensor::IrCameraHandle
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct IrCameraHandle {
    pub npad_id: u8,
    pub npad_type: NpadStyleIndex,
    pub _padding: [u8; 2],
}
const _: () = assert!(std::mem::size_of::<IrCameraHandle>() == 4);

/// This is nn::irsensor::PackedMcuVersion
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct PackedMcuVersion {
    pub major: u16,
    pub minor: u16,
}
const _: () = assert!(std::mem::size_of::<PackedMcuVersion>() == 4);

/// This is nn::irsensor::PackedClusteringProcessorConfig
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct PackedClusteringProcessorConfig {
    pub camera_config: PackedCameraConfig,
    pub window_of_interest: IrsRect,
    pub required_mcu_version: PackedMcuVersion,
    pub pixel_count_min: u32,
    pub pixel_count_max: u32,
    pub object_intensity_min: u8,
    pub is_external_light_filter_enabled: bool,
    pub _padding: [u8; 2],
}
const _: () = assert!(std::mem::size_of::<PackedClusteringProcessorConfig>() == 0x28);

/// This is nn::irsensor::PackedImageTransferProcessorConfig
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct PackedImageTransferProcessorConfig {
    pub camera_config: PackedCameraConfig,
    pub required_mcu_version: PackedMcuVersion,
    pub format: PackedImageTransferProcessorFormat,
    pub _padding: [u8; 3],
}
const _: () = assert!(std::mem::size_of::<PackedImageTransferProcessorConfig>() == 0x18);

/// This is nn::irsensor::PackedImageTransferProcessorExConfig
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct PackedImageTransferProcessorExConfig {
    pub camera_config: PackedCameraConfig,
    pub required_mcu_version: PackedMcuVersion,
    pub origin_format: PackedImageTransferProcessorFormat,
    pub trimming_format: PackedImageTransferProcessorFormat,
    pub trimming_start_x: u16,
    pub trimming_start_y: u16,
    pub is_external_light_filter_enabled: bool,
    pub _padding: [u8; 5],
}
const _: () = assert!(std::mem::size_of::<PackedImageTransferProcessorExConfig>() == 0x20);

/// This is nn::irsensor::PackedMomentProcessorConfig
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct PackedMomentProcessorConfig {
    pub camera_config: PackedCameraConfig,
    pub window_of_interest: IrsRect,
    pub required_mcu_version: PackedMcuVersion,
    pub preprocess: u8, // PackedMomentProcessorPreprocess
    pub preprocess_intensity_threshold: u8,
    pub _padding: [u8; 2],
}
const _: () = assert!(std::mem::size_of::<PackedMomentProcessorConfig>() == 0x20);

/// This is nn::irsensor::PackedTeraPluginProcessorConfig
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct PackedTeraPluginProcessorConfig {
    pub required_mcu_version: PackedMcuVersion,
    pub mode: u8,
    pub unknown_1: u8,
    pub unknown_2: u8,
    pub unknown_3: u8,
}
const _: () = assert!(std::mem::size_of::<PackedTeraPluginProcessorConfig>() == 0x8);

/// This is nn::irsensor::PackedPointingProcessorConfig
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct PackedPointingProcessorConfig {
    pub window_of_interest: IrsRect,
    pub required_mcu_version: PackedMcuVersion,
}
const _: () = assert!(std::mem::size_of::<PackedPointingProcessorConfig>() == 0xC);

/// This is nn::irsensor::PackedIrLedProcessorConfig
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct PackedIrLedProcessorConfig {
    pub required_mcu_version: PackedMcuVersion,
    pub light_target: u8,
    pub _padding: [u8; 3],
}
const _: () = assert!(std::mem::size_of::<PackedIrLedProcessorConfig>() == 0x8);

/// This is nn::irsensor::detail::ProcessorState
#[derive(Debug, Clone)]
pub struct ProcessorState {
    pub processor_raw_data: [u8; 0xE20],
}

impl Default for ProcessorState {
    fn default() -> Self {
        Self {
            processor_raw_data: [0u8; 0xE20],
        }
    }
}

/// This is nn::irsensor::detail::DeviceFormat
#[derive(Debug, Clone)]
pub struct DeviceFormat {
    pub camera_status: IrCameraStatus,
    pub camera_internal_status: IrCameraInternalStatus,
    pub mode: IrSensorMode,
    pub state: ProcessorState,
}

impl Default for DeviceFormat {
    fn default() -> Self {
        Self {
            camera_status: IrCameraStatus::Unconnected,
            camera_internal_status: IrCameraInternalStatus::Ready,
            mode: IrSensorMode::None,
            state: ProcessorState::default(),
        }
    }
}

/// This is nn::irsensor::ImageTransferProcessorState
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct ImageTransferProcessorState {
    pub sampling_number: u64,
    pub ambient_noise_level: CameraAmbientNoiseLevel,
    pub _padding: [u8; 4],
}
const _: () = assert!(std::mem::size_of::<ImageTransferProcessorState>() == 0x10);
