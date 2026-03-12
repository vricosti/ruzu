// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/shared_memory_format.h
//!
//! Defines the full shared memory layout for HID services.

use crate::hid_types::*;
use crate::resources::npad::npad_types::*;
use crate::resources::ring_lifo::Lifo;
use crate::resources::debug_pad::debug_pad_types::DebugPadState;
use crate::resources::keyboard::keyboard_types::KeyboardState;
use crate::resources::system_buttons::system_button_types::*;
use crate::resources::touch_screen::touch_types::*;

pub const HID_ENTRY_COUNT: usize = 17;

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct CommonHeader {
    pub timestamp: i64,
    pub total_entry_count: i64,
    pub last_entry_index: i64,
    pub entry_count: i64,
}
const _: () = assert!(std::mem::size_of::<CommonHeader>() == 0x20);

/// This is nn::hid::detail::NpadSixAxisSensorLifo
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NpadSixAxisSensorLifo {
    pub lifo: Lifo<SixAxisSensorState, HID_ENTRY_COUNT>,
}

/// This is nn::hid::detail::NpadInternalState
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct NpadInternalState {
    pub style_tag: NpadStyleTag,
    pub assignment_mode: NpadJoyAssignmentMode,
    pub fullkey_color: NpadFullKeyColorState,
    pub joycon_color: NpadJoyColorState,
    pub fullkey_lifo: Lifo<NPadGenericState, HID_ENTRY_COUNT>,
    pub handheld_lifo: Lifo<NPadGenericState, HID_ENTRY_COUNT>,
    pub joy_dual_lifo: Lifo<NPadGenericState, HID_ENTRY_COUNT>,
    pub joy_left_lifo: Lifo<NPadGenericState, HID_ENTRY_COUNT>,
    pub joy_right_lifo: Lifo<NPadGenericState, HID_ENTRY_COUNT>,
    pub palma_lifo: Lifo<NPadGenericState, HID_ENTRY_COUNT>,
    pub system_ext_lifo: Lifo<NPadGenericState, HID_ENTRY_COUNT>,
    pub sixaxis_fullkey_lifo: NpadSixAxisSensorLifo,
    pub sixaxis_handheld_lifo: NpadSixAxisSensorLifo,
    pub sixaxis_dual_left_lifo: NpadSixAxisSensorLifo,
    pub sixaxis_dual_right_lifo: NpadSixAxisSensorLifo,
    pub sixaxis_left_lifo: NpadSixAxisSensorLifo,
    pub sixaxis_right_lifo: NpadSixAxisSensorLifo,
    pub device_type: DeviceType,
    pub _reserved1: [u8; 0x4],
    pub system_properties: NPadSystemProperties,
    pub button_properties: NpadSystemButtonProperties,
    pub battery_level_dual: NpadBatteryLevel,
    pub battery_level_left: NpadBatteryLevel,
    pub battery_level_right: NpadBatteryLevel,
    pub applet_footer_attributes: AppletFooterUiAttributes,
    pub applet_footer_type: AppletFooterUiType,
    pub _reserved2: [u8; 0x5B],
    pub _unknown: [u8; 0x20],
    pub gc_trigger_lifo: Lifo<NpadGcTriggerState, HID_ENTRY_COUNT>,
    pub lark_type_l_and_main: NpadLarkType,
    pub lark_type_r: NpadLarkType,
    pub lucia_type: NpadLuciaType,
    pub lager_type: NpadLagerType,
    pub sixaxis_fullkey_properties: SixAxisSensorProperties,
    pub sixaxis_handheld_properties: SixAxisSensorProperties,
    pub sixaxis_dual_left_properties: SixAxisSensorProperties,
    pub sixaxis_dual_right_properties: SixAxisSensorProperties,
    pub sixaxis_left_properties: SixAxisSensorProperties,
    pub sixaxis_right_properties: SixAxisSensorProperties,
}

/// This is nn::hid::detail::NpadSharedMemoryEntry
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct NpadSharedMemoryEntry {
    pub internal_state: NpadInternalState,
    pub _padding: [u8; 0xC08],
}

/// This is nn::hid::detail::NpadSharedMemoryFormat
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct NpadSharedMemoryFormat {
    pub npad_entry: [NpadSharedMemoryEntry; MAX_SUPPORTED_NPAD_ID_TYPES],
}

/// This is nn::hid::detail::ConsoleSixAxisSensorSharedMemoryFormat
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct ConsoleSixAxisSensorSharedMemoryFormat {
    pub sampling_number: u64,
    pub is_seven_six_axis_sensor_at_rest: bool,
    pub _padding1: [u8; 3],
    pub verticalization_error: f32,
    pub gyro_bias: Vec3f,
    pub _padding2: [u8; 4],
}
const _: () = assert!(std::mem::size_of::<ConsoleSixAxisSensorSharedMemoryFormat>() == 0x20);
