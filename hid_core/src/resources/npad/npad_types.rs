// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/npad/npad_types.h and npad_types.cpp

use crate::hid_types::*;

pub const MAX_SUPPORTED_NPAD_ID_TYPES: usize = 10;
pub const STYLE_INDEX_COUNT: usize = 7;

/// This is nn::hid::NpadJoyHoldType
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u64)]
pub enum NpadJoyHoldType {
    #[default]
    Vertical = 0,
    Horizontal = 1,
}

/// This is nn::hid::NpadJoyAssignmentMode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum NpadJoyAssignmentMode {
    #[default]
    Dual = 0,
    Single = 1,
}

/// This is nn::hid::NpadJoyDeviceType
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(i64)]
pub enum NpadJoyDeviceType {
    #[default]
    Left = 0,
    Right = 1,
}

/// This is nn::hid::NpadHandheldActivationMode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u64)]
pub enum NpadHandheldActivationMode {
    #[default]
    Dual = 0,
    Single = 1,
    None = 2,
    MaxActivationMode = 3,
}

/// This is nn::hid::system::AppletFooterUiType
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u8)]
pub enum AppletFooterUiType {
    #[default]
    None = 0,
    HandheldNone = 1,
    HandheldJoyConLeftOnly = 2,
    HandheldJoyConRightOnly = 3,
    HandheldJoyConLeftJoyConRight = 4,
    JoyDual = 5,
    JoyDualLeftOnly = 6,
    JoyDualRightOnly = 7,
    JoyLeftHorizontal = 8,
    JoyLeftVertical = 9,
    JoyRightHorizontal = 10,
    JoyRightVertical = 11,
    SwitchProController = 12,
    CompatibleProController = 13,
    CompatibleJoyCon = 14,
    LarkHvc1 = 15,
    LarkHvc2 = 16,
    LarkNesLeft = 17,
    LarkNesRight = 18,
    Lucia = 19,
    Verification = 20,
    Lagon = 21,
}

/// This is nn::hid::NpadCommunicationMode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u64)]
pub enum NpadCommunicationMode {
    Mode5ms = 0,
    Mode10ms = 1,
    Mode15ms = 2,
    #[default]
    Default = 3,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum NpadRevision {
    #[default]
    Revision0 = 0,
    Revision1 = 1,
    Revision2 = 2,
    Revision3 = 3,
}

/// This is nn::hid::detail::ColorAttribute
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum ColorAttribute {
    Ok = 0,
    ReadError = 1,
    #[default]
    NoController = 2,
}

/// This is nn::hid::detail::NpadFullKeyColorState
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NpadFullKeyColorState {
    pub attribute: ColorAttribute,
    pub fullkey: NpadControllerColor,
}
const _: () = assert!(std::mem::size_of::<NpadFullKeyColorState>() == 0xC);

/// This is nn::hid::detail::NpadJoyColorState
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NpadJoyColorState {
    pub attribute: ColorAttribute,
    pub left: NpadControllerColor,
    pub right: NpadControllerColor,
}
const _: () = assert!(std::mem::size_of::<NpadJoyColorState>() == 0x14);

/// This is nn::hid::NpadAttribute
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NpadAttribute {
    pub raw: u32,
}
const _: () = assert!(std::mem::size_of::<NpadAttribute>() == 4);

/// NPadGenericState - common state for all npad styles
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NPadGenericState {
    pub sampling_number: i64,
    pub npad_buttons: NpadButtonState,
    pub l_stick: AnalogStickState,
    pub r_stick: AnalogStickState,
    pub connection_status: NpadAttribute,
    pub _reserved: [u8; 4],
}
const _: () = assert!(std::mem::size_of::<NPadGenericState>() == 0x28);

/// This is nn::hid::NpadSystemProperties
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NPadSystemProperties {
    pub raw: i64,
}
const _: () = assert!(std::mem::size_of::<NPadSystemProperties>() == 0x8);

/// This is nn::hid::NpadSystemButtonProperties
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NpadSystemButtonProperties {
    pub raw: i32,
}
const _: () = assert!(std::mem::size_of::<NpadSystemButtonProperties>() == 0x4);

/// This is nn::hid::system::DeviceType
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct DeviceType {
    pub raw: u32,
}

/// This is nn::hid::system::AppletFooterUiAttributes
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct AppletFooterUiAttributes {
    pub _padding: [u8; 0x4],
}

/// This is nn::hid::NpadLarkType
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum NpadLarkType {
    #[default]
    Invalid = 0,
    H1 = 1,
    H2 = 2,
    NL = 3,
    NR = 4,
}

/// This is nn::hid::NpadLuciaType
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum NpadLuciaType {
    #[default]
    Invalid = 0,
    J = 1,
    E = 2,
    U = 3,
}

/// This is nn::hid::NpadLagerType
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum NpadLagerType {
    #[default]
    Invalid = 0,
    J = 1,
    E = 2,
    U = 3,
}

/// Npad GC trigger state (resource version)
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NpadGcTriggerState {
    pub sampling_number: i64,
    pub l_analog: i32,
    pub r_analog: i32,
}
const _: () = assert!(std::mem::size_of::<NpadGcTriggerState>() == 0x10);
