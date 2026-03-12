// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/hid_types.h

use bitflags::bitflags;

/// This is nn::hid::DeviceIndex
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u8)]
pub enum DeviceIndex {
    Left = 0,
    Right = 1,
    #[default]
    None = 2,
    MaxDeviceIndex = 3,
}

bitflags! {
    /// This is nn::hid::NpadButton
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
    pub struct NpadButton: u64 {
        const NONE = 0;
        const A = 1 << 0;
        const B = 1 << 1;
        const X = 1 << 2;
        const Y = 1 << 3;
        const STICK_L = 1 << 4;
        const STICK_R = 1 << 5;
        const L = 1 << 6;
        const R = 1 << 7;
        const ZL = 1 << 8;
        const ZR = 1 << 9;
        const PLUS = 1 << 10;
        const MINUS = 1 << 11;
        const LEFT = 1 << 12;
        const UP = 1 << 13;
        const RIGHT = 1 << 14;
        const DOWN = 1 << 15;
        const STICK_L_LEFT = 1 << 16;
        const STICK_L_UP = 1 << 17;
        const STICK_L_RIGHT = 1 << 18;
        const STICK_L_DOWN = 1 << 19;
        const STICK_R_LEFT = 1 << 20;
        const STICK_R_UP = 1 << 21;
        const STICK_R_RIGHT = 1 << 22;
        const STICK_R_DOWN = 1 << 23;
        const LEFT_SL = 1 << 24;
        const LEFT_SR = 1 << 25;
        const RIGHT_SL = 1 << 26;
        const RIGHT_SR = 1 << 27;
        const PALMA = 1 << 28;
        const VERIFICATION = 1 << 29;
        const HANDHELD_LEFT_B = 1 << 30;
        const LAGON_C_LEFT = 1 << 31;
        const LAGON_C_UP = 1u64 << 32;
        const LAGON_C_RIGHT = 1u64 << 33;
        const LAGON_C_DOWN = 1u64 << 34;
        const ALL = 0xFFFFFFFFFFFFFFFF;
    }
}

/// This is nn::hid::KeyboardKeyIndex
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum KeyboardKeyIndex {
    A = 4,
    B = 5,
    C = 6,
    D = 7,
    E = 8,
    F = 9,
    G = 10,
    H = 11,
    I = 12,
    J = 13,
    K = 14,
    L = 15,
    M = 16,
    N = 17,
    O = 18,
    P = 19,
    Q = 20,
    R = 21,
    S = 22,
    T = 23,
    U = 24,
    V = 25,
    W = 26,
    X = 27,
    Y = 28,
    Z = 29,
    D1 = 30,
    D2 = 31,
    D3 = 32,
    D4 = 33,
    D5 = 34,
    D6 = 35,
    D7 = 36,
    D8 = 37,
    D9 = 38,
    D0 = 39,
    Return = 40,
    Escape = 41,
    Backspace = 42,
    Tab = 43,
    Space = 44,
    Minus = 45,
    Plus = 46,
    OpenBracket = 47,
    CloseBracket = 48,
    Pipe = 49,
    Tilde = 50,
    Semicolon = 51,
    Quote = 52,
    Backquote = 53,
    Comma = 54,
    Period = 55,
    Slash = 56,
    CapsLock = 57,
    F1 = 58,
    F2 = 59,
    F3 = 60,
    F4 = 61,
    F5 = 62,
    F6 = 63,
    F7 = 64,
    F8 = 65,
    F9 = 66,
    F10 = 67,
    F11 = 68,
    F12 = 69,
    PrintScreen = 70,
    ScrollLock = 71,
    Pause = 72,
    Insert = 73,
    Home = 74,
    PageUp = 75,
    Delete = 76,
    End = 77,
    PageDown = 78,
    RightArrow = 79,
    LeftArrow = 80,
    DownArrow = 81,
    UpArrow = 82,
    NumLock = 83,
    NumPadDivide = 84,
    NumPadMultiply = 85,
    NumPadSubtract = 86,
    NumPadAdd = 87,
    NumPadEnter = 88,
    NumPad1 = 89,
    NumPad2 = 90,
    NumPad3 = 91,
    NumPad4 = 92,
    NumPad5 = 93,
    NumPad6 = 94,
    NumPad7 = 95,
    NumPad8 = 96,
    NumPad9 = 97,
    NumPad0 = 98,
    NumPadDot = 99,
    Backslash = 100,
    Application = 101,
    Power = 102,
    NumPadEquals = 103,
    F13 = 104,
    F14 = 105,
    F15 = 106,
    F16 = 107,
    F17 = 108,
    F18 = 109,
    F19 = 110,
    F20 = 111,
    F21 = 112,
    F22 = 113,
    F23 = 114,
    F24 = 115,
    NumPadComma = 133,
    Ro = 135,
    KatakanaHiragana = 136,
    Yen = 137,
    Henkan = 138,
    Muhenkan = 139,
    NumPadCommaPc98 = 140,
    HangulEnglish = 144,
    Hanja = 145,
    Katakana = 146,
    Hiragana = 147,
    ZenkakuHankaku = 148,
    LeftControl = 224,
    LeftShift = 225,
    LeftAlt = 226,
    LeftGui = 227,
    RightControl = 228,
    RightShift = 229,
    RightAlt = 230,
    RightGui = 231,
}

/// This is nn::hid::NpadIdType
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum NpadIdType {
    Player1 = 0x0,
    Player2 = 0x1,
    Player3 = 0x2,
    Player4 = 0x3,
    Player5 = 0x4,
    Player6 = 0x5,
    Player7 = 0x6,
    Player8 = 0x7,
    Other = 0x10,
    #[default]
    Handheld = 0x20,
    Invalid = 0xFFFFFFFF,
}

/// This is nn::hid::NpadInterfaceType
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u8)]
pub enum NpadInterfaceType {
    #[default]
    None = 0,
    Bluetooth = 1,
    Rail = 2,
    Usb = 3,
    Embedded = 4,
}

/// This is nn::hid::NpadStyleIndex
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u8)]
pub enum NpadStyleIndex {
    #[default]
    None = 0,
    Fullkey = 3,
    Handheld = 4,
    // HandheldNES = 4, // same value as Handheld
    JoyconDual = 5,
    JoyconLeft = 6,
    JoyconRight = 7,
    GameCube = 8,
    Pokeball = 9,
    NES = 10,
    SNES = 12,
    N64 = 13,
    SegaGenesis = 14,
    SystemExt = 32,
    System = 33,
    MaxNpadType = 34,
}

bitflags! {
    /// This is nn::hid::NpadStyleSet
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
    pub struct NpadStyleSet: u32 {
        const NONE = 0;
        const FULLKEY = 1 << 0;
        const HANDHELD = 1 << 1;
        const JOY_DUAL = 1 << 2;
        const JOY_LEFT = 1 << 3;
        const JOY_RIGHT = 1 << 4;
        const GC = 1 << 5;
        const PALMA = 1 << 6;
        const LARK = 1 << 7;
        const HANDHELD_LARK = 1 << 8;
        const LUCIA = 1 << 9;
        const LAGOON = 1 << 10;
        const LAGER = 1 << 11;
        const SYSTEM_EXT = 1 << 29;
        const SYSTEM = 1 << 30;
        const ALL = 0xFFFFFFFF;
    }
}

/// This is nn::hid::VibrationDevicePosition
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum VibrationDevicePosition {
    #[default]
    None = 0,
    Left = 1,
    Right = 2,
}

/// This is nn::hid::VibrationDeviceType
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum VibrationDeviceType {
    #[default]
    Unknown = 0,
    LinearResonantActuator = 1,
    GcErm = 2,
    N64 = 3,
}

/// This is nn::hid::VibrationGcErmCommand
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u64)]
pub enum VibrationGcErmCommand {
    #[default]
    Stop = 0,
    Start = 1,
    StopHard = 2,
}

/// This is nn::hid::GyroscopeZeroDriftMode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum GyroscopeZeroDriftMode {
    Loose = 0,
    #[default]
    Standard = 1,
    Tight = 2,
}

/// This is nn::hid::TouchScreenModeForNx
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u8)]
pub enum TouchScreenModeForNx {
    #[default]
    UseSystemSetting = 0,
    Finger = 1,
    Heat2 = 2,
}

/// This is nn::hid::system::NpadBatteryLevel
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum NpadBatteryLevel {
    Empty = 0,
    Critical = 1,
    Low = 2,
    High = 3,
    #[default]
    Full = 4,
}

/// This is nn::hid::NpadStyleTag
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NpadStyleTag {
    pub raw: NpadStyleSet,
}
const _: () = assert!(std::mem::size_of::<NpadStyleTag>() == 4);

/// This is nn::hid::TouchAttribute
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct TouchAttribute {
    pub raw: u32,
}
const _: () = assert!(std::mem::size_of::<TouchAttribute>() == 0x4);

impl TouchAttribute {
    pub fn start_touch(&self) -> bool {
        (self.raw & (1 << 0)) != 0
    }
    pub fn end_touch(&self) -> bool {
        (self.raw & (1 << 1)) != 0
    }
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct TouchFinger {
    pub last_touch: u64,
    pub position_x: f32,
    pub position_y: f32,
    pub id: u32,
    pub attribute: TouchAttribute,
    pub pressed: bool,
}

/// This is nn::hid::TouchScreenConfigurationForNx
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct TouchScreenConfigurationForNx {
    pub mode: TouchScreenModeForNx,
    pub _padding: [u8; 0xF],
}
const _: () = assert!(std::mem::size_of::<TouchScreenConfigurationForNx>() == 0x10);

impl Default for TouchScreenConfigurationForNx {
    fn default() -> Self {
        Self {
            mode: TouchScreenModeForNx::UseSystemSetting,
            _padding: [0u8; 0xF],
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NpadColor {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}
const _: () = assert!(std::mem::size_of::<NpadColor>() == 4);

/// This is nn::hid::NpadControllerColor
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NpadControllerColor {
    pub body: NpadColor,
    pub button: NpadColor,
}
const _: () = assert!(std::mem::size_of::<NpadControllerColor>() == 8);

/// This is nn::hid::AnalogStickState
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct AnalogStickState {
    pub x: i32,
    pub y: i32,
}
const _: () = assert!(std::mem::size_of::<AnalogStickState>() == 8);

/// This is nn::hid::server::NpadGcTriggerState
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NpadGcTriggerState {
    pub sampling_number: i64,
    pub left: i32,
    pub right: i32,
}
const _: () = assert!(std::mem::size_of::<NpadGcTriggerState>() == 0x10);

/// This is nn::hid::system::NpadPowerInfo
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct NpadPowerInfo {
    pub is_powered: bool,
    pub is_charging: bool,
    pub _padding: [u8; 0x6],
    pub battery_level: NpadBatteryLevel,
}
const _: () = assert!(std::mem::size_of::<NpadPowerInfo>() == 0xC);

impl Default for NpadPowerInfo {
    fn default() -> Self {
        Self {
            is_powered: false,
            is_charging: false,
            _padding: [0u8; 0x6],
            battery_level: NpadBatteryLevel::Full,
        }
    }
}

/// LED pattern for controllers
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct LedPattern {
    pub raw: u64,
}

impl LedPattern {
    pub fn new(light1: u64, light2: u64, light3: u64, light4: u64) -> Self {
        let mut raw = 0u64;
        if light1 != 0 {
            raw |= 1 << 0;
        }
        if light2 != 0 {
            raw |= 1 << 1;
        }
        if light3 != 0 {
            raw |= 1 << 2;
        }
        if light4 != 0 {
            raw |= 1 << 3;
        }
        Self { raw }
    }
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct SleepButtonState {
    pub raw: u64,
}
const _: () = assert!(std::mem::size_of::<SleepButtonState>() == 0x8);

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct HomeButtonState {
    pub raw: u64,
}
const _: () = assert!(std::mem::size_of::<HomeButtonState>() == 0x8);

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct CaptureButtonState {
    pub raw: u64,
}
const _: () = assert!(std::mem::size_of::<CaptureButtonState>() == 0x8);

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NpadButtonState {
    pub raw: NpadButton,
}
const _: () = assert!(std::mem::size_of::<NpadButtonState>() == 0x8);

/// This is nn::hid::DebugPadButton
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct DebugPadButton {
    pub raw: u32,
}
const _: () = assert!(std::mem::size_of::<DebugPadButton>() == 0x4);

/// This is nn::hid::ConsoleSixAxisSensorHandle
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct ConsoleSixAxisSensorHandle {
    pub unknown_1: u8,
    pub unknown_2: u8,
    pub _padding: [u8; 2],
}
const _: () = assert!(std::mem::size_of::<ConsoleSixAxisSensorHandle>() == 4);

/// This is nn::hid::SixAxisSensorHandle
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct SixAxisSensorHandle {
    pub npad_type: NpadStyleIndex,
    pub npad_id: u8,
    pub device_index: DeviceIndex,
    pub _padding: u8,
}
const _: () = assert!(std::mem::size_of::<SixAxisSensorHandle>() == 4);

/// Parameters for six-axis sensor fusion
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct SixAxisSensorFusionParameters {
    pub parameter1: f32,
    pub parameter2: f32,
}
const _: () = assert!(std::mem::size_of::<SixAxisSensorFusionParameters>() == 8);

impl Default for SixAxisSensorFusionParameters {
    fn default() -> Self {
        Self {
            parameter1: 0.03,
            parameter2: 0.4,
        }
    }
}

/// This is nn::hid::server::SixAxisSensorProperties
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct SixAxisSensorProperties {
    pub raw: u8,
}
const _: () = assert!(std::mem::size_of::<SixAxisSensorProperties>() == 1);

impl SixAxisSensorProperties {
    pub fn is_newly_assigned(&self) -> bool {
        (self.raw & (1 << 0)) != 0
    }
    pub fn set_is_newly_assigned(&mut self, val: bool) {
        if val {
            self.raw |= 1 << 0;
        } else {
            self.raw &= !(1 << 0);
        }
    }
    pub fn is_firmware_update_available(&self) -> bool {
        (self.raw & (1 << 1)) != 0
    }
    pub fn set_is_firmware_update_available(&mut self, val: bool) {
        if val {
            self.raw |= 1 << 1;
        } else {
            self.raw &= !(1 << 1);
        }
    }
}

/// This is nn::hid::SixAxisSensorCalibrationParameter
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct SixAxisSensorCalibrationParameter {
    pub unknown_data: [u8; 0x744],
}
const _: () = assert!(std::mem::size_of::<SixAxisSensorCalibrationParameter>() == 0x744);

/// This is nn::hid::SixAxisSensorIcInformation
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct SixAxisSensorIcInformation {
    pub angular_rate: f32,
    pub unknown_gyro_data1: [f32; 6],
    pub unknown_gyro_data2: [f32; 9],
    pub unknown_gyro_data3: [f32; 9],
    pub acceleration_range: f32,
    pub unknown_accel_data1: [f32; 6],
    pub unknown_accel_data2: [f32; 9],
    pub unknown_accel_data3: [f32; 9],
}
const _: () = assert!(std::mem::size_of::<SixAxisSensorIcInformation>() == 0xC8);

/// This is nn::hid::SixAxisSensorAttribute
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct SixAxisSensorAttribute {
    pub raw: u32,
}
const _: () = assert!(std::mem::size_of::<SixAxisSensorAttribute>() == 4);

impl SixAxisSensorAttribute {
    pub fn is_connected(&self) -> bool {
        (self.raw & (1 << 0)) != 0
    }
    pub fn is_interpolated(&self) -> bool {
        (self.raw & (1 << 1)) != 0
    }
}

/// Vec3f placeholder (maps to Common::Vec3f)
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct Vec3f {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

/// This is nn::hid::SixAxisSensorState
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct SixAxisSensorState {
    pub delta_time: i64,
    pub sampling_number: i64,
    pub accel: Vec3f,
    pub gyro: Vec3f,
    pub rotation: Vec3f,
    pub orientation: [Vec3f; 3],
    pub attribute: SixAxisSensorAttribute,
    pub _reserved: [u8; 4],
}
const _: () = assert!(std::mem::size_of::<SixAxisSensorState>() == 0x60);

/// This is nn::hid::VibrationDeviceHandle
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct VibrationDeviceHandle {
    pub npad_type: NpadStyleIndex,
    pub npad_id: u8,
    pub device_index: DeviceIndex,
    pub _padding: u8,
}
const _: () = assert!(std::mem::size_of::<VibrationDeviceHandle>() == 4);

/// This is nn::hid::VibrationValue
#[derive(Debug, Clone, Copy, Default, PartialEq)]
#[repr(C)]
pub struct VibrationValue {
    pub low_amplitude: f32,
    pub low_frequency: f32,
    pub high_amplitude: f32,
    pub high_frequency: f32,
}
const _: () = assert!(std::mem::size_of::<VibrationValue>() == 0x10);

impl VibrationValue {
    /// Custom equality matching upstream behavior:
    /// Changes in frequency without amplitude don't have any effect
    pub fn is_equal(&self, b: &VibrationValue) -> bool {
        if self.low_amplitude != b.low_amplitude || self.high_amplitude != b.high_amplitude {
            return false;
        }
        if self.low_amplitude == 0.0 && self.high_amplitude == 0.0 {
            return true;
        }
        if self.low_frequency != b.low_frequency || self.high_frequency != b.high_frequency {
            return false;
        }
        true
    }
}

pub const DEFAULT_VIBRATION_VALUE: VibrationValue = VibrationValue {
    low_amplitude: 0.0,
    low_frequency: 160.0,
    high_amplitude: 0.0,
    high_frequency: 320.0,
};

/// This is nn::hid::VibrationDeviceInfo
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct VibrationDeviceInfo {
    pub device_type: VibrationDeviceType,
    pub position: VibrationDevicePosition,
}
const _: () = assert!(std::mem::size_of::<VibrationDeviceInfo>() == 0x8);

/// This is nn::hid::KeyboardModifier
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct KeyboardModifier {
    pub raw: u32,
}
const _: () = assert!(std::mem::size_of::<KeyboardModifier>() == 0x4);

/// This is nn::hid::KeyboardAttribute
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct KeyboardAttribute {
    pub raw: u32,
}
const _: () = assert!(std::mem::size_of::<KeyboardAttribute>() == 0x4);

/// This is nn::hid::KeyboardKey
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct KeyboardKey {
    pub key: [u8; 32],
}
const _: () = assert!(std::mem::size_of::<KeyboardKey>() == 0x20);

/// This is nn::hid::MouseButton
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct MouseButton {
    pub raw: u32,
}
const _: () = assert!(std::mem::size_of::<MouseButton>() == 0x4);

/// This is nn::hid::MouseAttribute
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct MouseAttribute {
    pub raw: u32,
}
const _: () = assert!(std::mem::size_of::<MouseAttribute>() == 0x4);

/// This is nn::hid::detail::MouseState
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct MouseState {
    pub sampling_number: i64,
    pub x: i32,
    pub y: i32,
    pub delta_x: i32,
    pub delta_y: i32,
    pub delta_wheel_y: i32,
    pub delta_wheel_x: i32,
    pub button: MouseButton,
    pub attribute: MouseAttribute,
}
const _: () = assert!(std::mem::size_of::<MouseState>() == 0x28);

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct UniquePadId {
    pub id: u64,
}
const _: () = assert!(std::mem::size_of::<UniquePadId>() == 0x8);

/// This is nn::hid::system::FirmwareVersion
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct FirmwareVersion {
    pub major: u8,
    pub minor: u8,
    pub micro: u8,
    pub revision: u8,
    pub device_identifier: [u8; 0xc],
}
const _: () = assert!(std::mem::size_of::<FirmwareVersion>() == 0x10);
