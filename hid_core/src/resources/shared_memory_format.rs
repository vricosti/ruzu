// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/shared_memory_format.h
//!
//! Defines the full shared memory layout for HID services.

use crate::hid_types::{
    self, NpadBatteryLevel, NpadGcTriggerState, NpadStyleTag, SixAxisSensorProperties,
    SixAxisSensorState, Vec3f,
};
use crate::resources::npad::npad_types::*;
use crate::resources::ring_lifo::Lifo;
use crate::resources::debug_pad::debug_pad_types::DebugPadState;
use crate::resources::keyboard::keyboard_types::KeyboardState;
use crate::resources::system_buttons::system_button_types;
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

/// This is nn::hid::detail::DebugPadSharedMemoryFormat
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct DebugPadSharedMemoryFormat {
    /// This is nn::hid::detail::DebugPadLifo
    pub debug_pad_lifo: Lifo<DebugPadState, HID_ENTRY_COUNT>,
    pub _padding: [u32; 0x4E],
}

impl Default for DebugPadSharedMemoryFormat {
    fn default() -> Self {
        Self {
            debug_pad_lifo: Lifo::default(),
            _padding: [0u32; 0x4E],
        }
    }
}

/// This is nn::hid::detail::TouchScreenSharedMemoryFormat
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct TouchScreenSharedMemoryFormat {
    /// This is nn::hid::detail::TouchScreenLifo
    pub touch_screen_lifo: Lifo<TouchScreenState, HID_ENTRY_COUNT>,
    pub _padding: [u32; 0xF2],
}

impl Default for TouchScreenSharedMemoryFormat {
    fn default() -> Self {
        Self {
            touch_screen_lifo: Lifo::default(),
            _padding: [0u32; 0xF2],
        }
    }
}

/// This is nn::hid::detail::MouseSharedMemoryFormat
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct MouseSharedMemoryFormat {
    /// This is nn::hid::detail::MouseLifo
    pub mouse_lifo: Lifo<hid_types::MouseState, HID_ENTRY_COUNT>,
    pub _padding: [u32; 0x2C],
}

impl Default for MouseSharedMemoryFormat {
    fn default() -> Self {
        Self {
            mouse_lifo: Lifo::default(),
            _padding: [0u32; 0x2C],
        }
    }
}

/// This is nn::hid::detail::KeyboardSharedMemoryFormat
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct KeyboardSharedMemoryFormat {
    /// This is nn::hid::detail::KeyboardLifo
    pub keyboard_lifo: Lifo<KeyboardState, HID_ENTRY_COUNT>,
    pub _padding: [u32; 0xA],
}

impl Default for KeyboardSharedMemoryFormat {
    fn default() -> Self {
        Self {
            keyboard_lifo: Lifo::default(),
            _padding: [0u32; 0xA],
        }
    }
}

/// This is nn::hid::detail::DigitizerSharedMemoryFormat
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct DigitizerSharedMemoryFormat {
    pub header: CommonHeader,
    pub _padding: [u8; 0xFE0],
}

impl Default for DigitizerSharedMemoryFormat {
    fn default() -> Self {
        Self {
            header: CommonHeader::default(),
            _padding: [0u8; 0xFE0],
        }
    }
}

/// This is nn::hid::detail::HomeButtonSharedMemoryFormat
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct HomeButtonSharedMemoryFormat {
    pub home_lifo: Lifo<system_button_types::HomeButtonState, HID_ENTRY_COUNT>,
    pub _padding: [u8; 0x48],
}

impl Default for HomeButtonSharedMemoryFormat {
    fn default() -> Self {
        Self {
            home_lifo: Lifo::default(),
            _padding: [0u8; 0x48],
        }
    }
}

/// This is nn::hid::detail::SleepButtonSharedMemoryFormat
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct SleepButtonSharedMemoryFormat {
    pub sleep_lifo: Lifo<system_button_types::SleepButtonState, HID_ENTRY_COUNT>,
    pub _padding: [u8; 0x48],
}

impl Default for SleepButtonSharedMemoryFormat {
    fn default() -> Self {
        Self {
            sleep_lifo: Lifo::default(),
            _padding: [0u8; 0x48],
        }
    }
}

/// This is nn::hid::detail::CaptureButtonSharedMemoryFormat
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct CaptureButtonSharedMemoryFormat {
    pub capture_lifo: Lifo<system_button_types::CaptureButtonState, HID_ENTRY_COUNT>,
    pub _padding: [u8; 0x48],
}

impl Default for CaptureButtonSharedMemoryFormat {
    fn default() -> Self {
        Self {
            capture_lifo: Lifo::default(),
            _padding: [0u8; 0x48],
        }
    }
}

/// This is nn::hid::detail::InputDetectorSharedMemoryFormat
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct InputDetectorSharedMemoryFormat {
    pub header: CommonHeader,
    pub _padding: [u8; 0x7E0],
}

impl Default for InputDetectorSharedMemoryFormat {
    fn default() -> Self {
        Self {
            header: CommonHeader::default(),
            _padding: [0u8; 0x7E0],
        }
    }
}

/// This is nn::hid::detail::UniquePadSharedMemoryFormat
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct UniquePadSharedMemoryFormat {
    pub header: CommonHeader,
    pub _padding: [u8; 0x3FE0],
}

impl Default for UniquePadSharedMemoryFormat {
    fn default() -> Self {
        Self {
            header: CommonHeader::default(),
            _padding: [0u8; 0x3FE0],
        }
    }
}

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

/// This is nn::hid::detail::GestureSharedMemoryFormat
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct GestureSharedMemoryFormat {
    /// This is nn::hid::detail::GestureLifo
    pub gesture_lifo: Lifo<GestureState, HID_ENTRY_COUNT>,
    pub _padding: [u32; 0x3E],
}

impl Default for GestureSharedMemoryFormat {
    fn default() -> Self {
        Self {
            gesture_lifo: Lifo::default(),
            _padding: [0u32; 0x3E],
        }
    }
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

/// This is nn::hid::detail::SharedMemoryFormat
///
/// The full shared memory layout as used by HID services.
/// Total size is 0x40000 bytes.
#[repr(C)]
pub struct SharedMemoryFormat {
    pub debug_pad: DebugPadSharedMemoryFormat,
    pub touch_screen: TouchScreenSharedMemoryFormat,
    pub mouse: MouseSharedMemoryFormat,
    pub keyboard: KeyboardSharedMemoryFormat,
    pub digitizer: DigitizerSharedMemoryFormat,
    pub home_button: HomeButtonSharedMemoryFormat,
    pub sleep_button: SleepButtonSharedMemoryFormat,
    pub capture_button: CaptureButtonSharedMemoryFormat,
    pub input_detector: InputDetectorSharedMemoryFormat,
    pub unique_pad: UniquePadSharedMemoryFormat,
    pub npad: NpadSharedMemoryFormat,
    pub gesture: GestureSharedMemoryFormat,
    pub console: ConsoleSixAxisSensorSharedMemoryFormat,
    pub _padding1: [u8; 0x19E0],
    pub debug_mouse: MouseSharedMemoryFormat,
    pub _padding2: [u8; 0x2000],
}

impl SharedMemoryFormat {
    pub fn initialize(&mut self) {
        // Upstream Initialize() is empty — shared memory is zero-initialized.
    }
}
