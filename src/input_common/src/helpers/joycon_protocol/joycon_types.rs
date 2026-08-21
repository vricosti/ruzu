// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/joycon_protocol/joycon_types.h`.
//!
//! Based on dkms-hid-nintendo implementation, CTCaer joycon toolkit and dekuNukem reverse
//! engineering https://github.com/nicman23/dkms-hid-nintendo/blob/master/src/hid-nintendo.c
//! https://github.com/CTCaer/jc_toolkit
//! https://github.com/dekuNukem/Nintendo_Switch_Reverse_Engineering

use std::fmt;

pub const MAX_ERROR_COUNT: u32 = 50;
pub const MAX_BUFFER_SIZE: u32 = 368;
pub const DEFAULT_VIBRATION_BUFFER: [u8; 8] = [0x0, 0x1, 0x40, 0x40, 0x0, 0x1, 0x40, 0x40];

pub type MacAddress = [u8; 6];
pub type SerialNumber = [u8; 15];
pub type TagUUID = [u8; 7];
pub type MifareUUID = [u8; 4];

/// Port of `ControllerType` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ControllerType {
    None = 0x00,
    Left = 0x01,
    Right = 0x02,
    Pro = 0x03,
    Dual = 0x05,
    LarkHvc1 = 0x07,
    LarkHvc2 = 0x08,
    LarkNesLeft = 0x09,
    LarkNesRight = 0x0A,
    Lucia = 0x0B,
    Lagon = 0x0C,
    Lager = 0x0D,
}

impl Default for ControllerType {
    fn default() -> Self {
        ControllerType::None
    }
}

/// Port of `PadAxes` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PadAxes {
    LeftStickX,
    LeftStickY,
    RightStickX,
    RightStickY,
    Undefined,
}

/// Port of `PadMotion` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PadMotion {
    LeftMotion,
    RightMotion,
    Undefined,
}

/// Port of `PadButton` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum PadButton {
    Down = 0x000001,
    Up = 0x000002,
    Right = 0x000004,
    Left = 0x000008,
    LeftSR = 0x000010,
    LeftSL = 0x000020,
    L = 0x000040,
    ZL = 0x000080,
    Y = 0x000100,
    X = 0x000200,
    B = 0x000400,
    A = 0x000800,
    RightSR = 0x001000,
    RightSL = 0x002000,
    R = 0x004000,
    ZR = 0x008000,
    Minus = 0x010000,
    Plus = 0x020000,
    StickR = 0x040000,
    StickL = 0x080000,
    Home = 0x100000,
    Capture = 0x200000,
}

/// Port of `PassivePadButton` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum PassivePadButton {
    DownA = 0x0001,
    RightX = 0x0002,
    LeftB = 0x0004,
    UpY = 0x0008,
    SL = 0x0010,
    SR = 0x0020,
    Minus = 0x0100,
    Plus = 0x0200,
    StickL = 0x0400,
    StickR = 0x0800,
    Home = 0x1000,
    Capture = 0x2000,
    LR = 0x4000,
    ZlZr = 0x8000,
}

/// Port of `PassivePadStick` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum PassivePadStick {
    Right = 0x00,
    RightDown = 0x01,
    Down = 0x02,
    DownLeft = 0x03,
    Left = 0x04,
    LeftUp = 0x05,
    Up = 0x06,
    UpRight = 0x07,
    Neutral = 0x08,
}

/// Port of `OutputReport` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum OutputReport {
    RumbleAndSubcmd = 0x01,
    FwUpdatePkt = 0x03,
    RumbleOnly = 0x10,
    McuData = 0x11,
    UsbCmd = 0x80,
}

/// Port of `FeatureReport` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum FeatureReport {
    LastSubcmd = 0x02,
    OtaGwUpgrade = 0x70,
    SetupMemRead = 0x71,
    MemRead = 0x72,
    EraseMemSector = 0x73,
    MemWrite = 0x74,
    Launch = 0x75,
}

/// Port of `SubCommand` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum SubCommand {
    State = 0x00,
    ManualBtPairing = 0x01,
    ReqDevInfo = 0x02,
    SetReportMode = 0x03,
    TriggersElapsed = 0x04,
    GetPageListState = 0x05,
    SetHciState = 0x06,
    ResetPairingInfo = 0x07,
    LowPowerMode = 0x08,
    SpiFlashRead = 0x10,
    SpiFlashWrite = 0x11,
    SpiSectorErase = 0x12,
    ResetMcu = 0x20,
    SetMcuConfig = 0x21,
    SetMcuState = 0x22,
    SetPlayerLights = 0x30,
    GetPlayerLights = 0x31,
    SetHomeLight = 0x38,
    EnableImu = 0x40,
    SetImuSensitivity = 0x41,
    WriteImuReg = 0x42,
    ReadImuReg = 0x43,
    EnableVibration = 0x48,
    GetRegulatedVoltage = 0x50,
    SetExternalConfig = 0x58,
    GetExternalDeviceInfo = 0x59,
    EnableExternalPolling = 0x5A,
    DisableExternalPolling = 0x5B,
    SetExternalFormatConfig = 0x5C,
}

/// Port of `UsbSubCommand` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum UsbSubCommand {
    ConnStatus = 0x01,
    Handshake = 0x02,
    Baudrate3M = 0x03,
    NoTimeout = 0x04,
    EnTimeout = 0x05,
    Reset = 0x06,
    PreHandshake = 0x91,
    SendUart = 0x92,
}

/// Port of `CalibrationMagic` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum CalibrationMagic {
    UsrMagic0 = 0xB2,
    UsrMagic1 = 0xA1,
}

/// Port of `SpiAddress` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum SpiAddress {
    Magic = 0x0000,
    MacAddress = 0x0015,
    PairingInfo = 0x2000,
    Shipment = 0x5000,
    SerialNumber = 0x6000,
    DeviceType = 0x6012,
    FormatVersion = 0x601B,
    FactImuData = 0x6020,
    FactLeftData = 0x603d,
    FactRightData = 0x6046,
    ColorData = 0x6050,
    DesignVariation = 0x605C,
    SensorData = 0x6080,
    UserLeftMagic = 0x8010,
    UserLeftData = 0x8012,
    UserRightMagic = 0x801B,
    UserRightData = 0x801D,
    UserImuMagic = 0x8026,
    UserImuData = 0x8028,
}

/// Port of `ReportMode` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ReportMode {
    ActivePollingNfcIrCameraData = 0x00,
    ActivePollingNfcIrCameraConfiguration = 0x01,
    ActivePollingNfcIrCameraDataConfiguration = 0x02,
    ActivePollingIrCameraData = 0x03,
    SubcmdReply = 0x21,
    McuUpdateState = 0x23,
    StandardFull60Hz = 0x30,
    NfcIrMode60Hz = 0x31,
    SimpleHidMode = 0x3F,
    InputUsbResponse = 0x81,
}

impl Default for ReportMode {
    fn default() -> Self {
        ReportMode::SimpleHidMode
    }
}

/// Port of `GyroSensitivity` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum GyroSensitivity {
    Dps250,
    Dps500,
    Dps1000,
    Dps2000, // Default
}

impl Default for GyroSensitivity {
    fn default() -> Self {
        GyroSensitivity::Dps2000
    }
}

/// Port of `AccelerometerSensitivity` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum AccelerometerSensitivity {
    G8, // Default
    G4,
    G2,
    G16,
}

impl Default for AccelerometerSensitivity {
    fn default() -> Self {
        AccelerometerSensitivity::G8
    }
}

/// Port of `GyroPerformance` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum GyroPerformance {
    Hz833,
    Hz208, // Default
}

impl Default for GyroPerformance {
    fn default() -> Self {
        GyroPerformance::Hz208
    }
}

/// Port of `AccelerometerPerformance` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum AccelerometerPerformance {
    Hz200,
    Hz100, // Default
}

impl Default for AccelerometerPerformance {
    fn default() -> Self {
        AccelerometerPerformance::Hz100
    }
}

/// Port of `MCUCommand` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum McuCommand {
    ConfigureMcu = 0x21,
    ConfigureIr = 0x23,
}

/// Port of `MCUSubCommand` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum McuSubCommand {
    SetMcuMode = 0x0,
    SetDeviceMode = 0x1,
    ReadDeviceMode = 0x02,
    WriteDeviceRegisters = 0x4,
}

/// Port of `MCUMode` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum McuMode {
    Suspend = 0,
    Standby = 1,
    Ringcon = 3,
    Nfc = 4,
    Ir = 5,
    MaybeFwUpdate = 6,
}

/// Port of `MCURequest` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum McuRequest {
    GetMcuStatus = 1,
    GetNfcData = 2,
    GetIrData = 3,
}

/// Port of `MCUReport` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum McuReport {
    Empty = 0x00,
    StateReport = 0x01,
    IrData = 0x03,
    BusyInitializing = 0x0b,
    IrStatus = 0x13,
    IrRegisters = 0x1b,
    NfcState = 0x2a,
    NfcReadData = 0x3a,
    EmptyAwaitingCmd = 0xff,
}

/// Port of `MCUPacketFlag` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum McuPacketFlag {
    MorePacketsRemaining = 0x00,
    LastCommandPacket = 0x08,
}

/// Port of `NFCCommand` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum NfcCommand {
    CancelAll = 0x00,
    StartPolling = 0x01,
    StopPolling = 0x02,
    StartWaitingReceive = 0x04,
    ReadNtag = 0x06,
    WriteNtag = 0x08,
    Mifare = 0x0F,
}

/// Port of `NFCTagType` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum NfcTagType {
    AllTags = 0x00,
    Ntag215 = 0x01,
}

/// Port of `NFCPages` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NfcPages {
    Block0 = 0,
    Block3 = 3,
    Block45 = 45,
    Block135 = 135,
    Block231 = 231,
}

/// Port of `NFCStatus` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum JoyconNfcStatus {
    Ready = 0x00,
    Polling = 0x01,
    LastPackage = 0x04,
    WriteDone = 0x05,
    TagLost = 0x07,
    WriteReady = 0x09,
    MifareDone = 0x10,
}

/// Port of `MifareCmd` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum MifareCmd {
    None = 0x00,
    Read = 0x30,
    AuthA = 0x60,
    AuthB = 0x61,
    Write = 0xA0,
    Transfer = 0xB0,
    Decrement = 0xC0,
    Increment = 0xC1,
    Store = 0xC2,
}

/// Port of `IrsMode` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum IrsMode {
    None = 0x02,
    Moment = 0x03,
    Dpd = 0x04,
    Clustering = 0x06,
    ImageTransfer = 0x07,
    Silhouette = 0x08,
    TeraImage = 0x09,
    SilhouetteTeraImage = 0x0A,
}

/// Port of `IrsResolution` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IrsResolution {
    Size320x240,
    Size160x120,
    Size80x60,
    Size40x30,
    Size20x15,
    None,
}

/// Port of `IrsResolutionCode` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum IrsResolutionCode {
    Size320x240 = 0x00,
    Size160x120 = 0x50,
    Size80x60 = 0x64,
    Size40x30 = 0x69,
    Size20x15 = 0x6A,
}

/// Port of `IrsFragments` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum IrsFragments {
    Size20x15 = 0x00,
    Size40x30 = 0x03,
    Size80x60 = 0x0f,
    Size160x120 = 0x3f,
    Size320x240 = 0xFF,
}

/// Port of `IrLeds` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum IrLeds {
    BrightAndDim = 0x00,
    Bright = 0x20,
    Dim = 0x10,
    None = 0x30,
}

/// Port of `IrExLedFilter` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum IrExLedFilter {
    Disabled = 0x00,
    Enabled = 0x03,
}

/// Port of `IrImageFlip` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum IrImageFlip {
    Normal = 0x00,
    Inverted = 0x02,
}

/// Port of `IrRegistersAddress` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum IrRegistersAddress {
    UpdateTime = 0x0400,
    FinalizeConfig = 0x0700,
    LedFilter = 0x0e00,
    Leds = 0x1000,
    LedIntensityMsb = 0x1100,
    LedIntensityLsb = 0x1200,
    ImageFlip = 0x2d00,
    Resolution = 0x2e00,
    DigitalGainLsb = 0x2e01,
    DigitalGainMsb = 0x2f01,
    ExposureLsb = 0x3001,
    ExposureMsb = 0x3101,
    ExposureTime = 0x3201,
    WhitePixelThreshold = 0x4301,
    DenoiseSmoothing = 0x6701,
    DenoiseEdge = 0x6801,
    DenoiseColor = 0x6901,
}

/// Port of `ExternalDeviceId` enum from joycon_types.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum ExternalDeviceId {
    RingController = 0x2000,
    Starlink = 0x2800,
}

// ---- Structs ----

/// Port of `MotionSensorCalibration` struct from joycon_types.h
#[derive(Debug, Clone, Default)]
pub struct MotionSensorCalibration {
    pub offset: i16,
    pub scale: i16,
}

/// Port of `MotionCalibration` struct from joycon_types.h
#[derive(Debug, Clone, Default)]
pub struct MotionCalibration {
    pub accelerometer: [MotionSensorCalibration; 3],
    pub gyro: [MotionSensorCalibration; 3],
}

/// Port of `MotionData` struct from joycon_types.h
#[derive(Debug, Clone, Default)]
pub struct MotionData {
    pub gyro_x: f32,
    pub gyro_y: f32,
    pub gyro_z: f32,
    pub accel_x: f32,
    pub accel_y: f32,
    pub accel_z: f32,
    pub delta_timestamp: u64,
}

/// Port of `JoyStickAxisCalibration` struct from joycon_types.h
#[derive(Debug, Clone, Default)]
pub struct JoyStickAxisCalibration {
    pub max: u16,
    pub min: u16,
    pub center: u16,
}

/// Port of `JoyStickCalibration` struct from joycon_types.h
#[derive(Debug, Clone, Default)]
pub struct JoyStickCalibration {
    pub x: JoyStickAxisCalibration,
    pub y: JoyStickAxisCalibration,
}

/// Port of `RingCalibration` struct from joycon_types.h
#[derive(Debug, Clone, Default)]
pub struct RingCalibration {
    pub default_value: i16,
    pub max_value: i16,
    pub min_value: i16,
}

/// Port of `Color` struct from joycon_types.h
#[derive(Debug, Clone, Default)]
pub struct Color {
    pub body: u32,
    pub buttons: u32,
    pub left_grip: u32,
    pub right_grip: u32,
}

/// Port of `Battery` struct from joycon_types.h
#[derive(Debug, Clone, Default)]
pub struct Battery {
    pub raw: u8,
}

impl Battery {
    pub fn charging(&self) -> bool {
        (self.raw >> 4) & 1 != 0
    }

    pub fn status(&self) -> u8 {
        (self.raw >> 5) & 0x7
    }
}

/// Port of `VibrationValue` struct from joycon_types.h
#[derive(Debug, Clone, Default)]
pub struct VibrationValue {
    pub low_amplitude: f32,
    pub low_frequency: f32,
    pub high_amplitude: f32,
    pub high_frequency: f32,
}

/// Port of `FirmwareVersion` struct from joycon_types.h
#[derive(Debug, Clone, Copy, Default)]
pub struct FirmwareVersion {
    pub major: u8,
    pub minor: u8,
}

/// Port of `DeviceInfo` struct from joycon_types.h
#[derive(Debug, Clone)]
pub struct DeviceInfo {
    pub firmware: FirmwareVersion,
    pub unknown_1: [u8; 2],
    pub mac_address: MacAddress,
    pub unknown_2: [u8; 2],
}

/// Port of `MotionStatus` struct from joycon_types.h
#[derive(Debug, Clone)]
pub struct MotionStatus {
    pub is_enabled: bool,
    pub delta_time: u64,
    pub gyro_sensitivity: GyroSensitivity,
    pub accelerometer_sensitivity: AccelerometerSensitivity,
}

/// Port of `RingStatus` struct from joycon_types.h
#[derive(Debug, Clone)]
pub struct RingStatus {
    pub is_enabled: bool,
    pub default_value: i16,
    pub max_value: i16,
    pub min_value: i16,
}

/// Port of `TagInfo` struct from joycon_types.h
#[derive(Debug, Clone)]
pub struct TagInfo {
    pub uuid_length: u8,
    pub protocol: u8,
    pub tag_type: u8,
    pub uuid: [u8; 10],
}

/// Port of `MifareReadChunk` struct from joycon_types.h
#[derive(Debug, Clone)]
pub struct MifareReadChunk {
    pub command: MifareCmd,
    pub sector_key: [u8; 6],
    pub sector: u8,
}

/// Port of `MifareWriteChunk` struct from joycon_types.h
#[derive(Debug, Clone)]
pub struct MifareWriteChunk {
    pub command: MifareCmd,
    pub sector_key: [u8; 6],
    pub sector: u8,
    pub data: [u8; 16],
}

/// Port of `MifareReadData` struct from joycon_types.h
#[derive(Debug, Clone)]
pub struct MifareReadData {
    pub sector: u8,
    pub data: [u8; 16],
}

/// Port of `InputReportPassive` struct from joycon_types.h
///
/// #pragma pack(push, 1) in upstream — repr(C, packed) here.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C, packed)]
pub struct InputReportPassive {
    pub report_mode: u8,
    pub button_input: u16,
    pub stick_state: u8,
    pub unknown_data: [u8; 10],
}
const _: () = assert!(std::mem::size_of::<InputReportPassive>() == 0xE);

/// Port of `InputReportActive` struct from joycon_types.h
///
/// #pragma pack(push, 1) in upstream — repr(C, packed) here.
/// motion_input is 6*2 = 12 i16 values (3 samples of accel+gyro).
/// Upstream uses `std::array<s16, 6 * 2>` which is 6 sensors × 2 samples but
/// the code only uses indices 0..5 (first sample). The struct is 0x29 bytes.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C, packed)]
pub struct InputReportActive {
    pub report_mode: u8,
    pub packet_id: u8,
    pub battery_status: u8,
    pub button_input: [u8; 3],
    pub left_stick_state: [u8; 3],
    pub right_stick_state: [u8; 3],
    pub vibration_code: u8,
    pub motion_input: [i16; 12],
    pub _pad: [u8; 2],
    pub ring_input: i16,
}
const _: () = assert!(std::mem::size_of::<InputReportActive>() == 0x29);

/// Port of `JoyconCallbacks` struct from joycon_types.h
pub struct JoyconCallbacks {
    pub on_battery_data: Option<Box<dyn Fn(Battery) + Send + Sync>>,
    pub on_color_data: Option<Box<dyn Fn(Color) + Send + Sync>>,
    pub on_button_data: Option<Box<dyn Fn(i32, bool) + Send + Sync>>,
    pub on_stick_data: Option<Box<dyn Fn(i32, f32) + Send + Sync>>,
    pub on_motion_data: Option<Box<dyn Fn(i32, &MotionData) + Send + Sync>>,
    pub on_ring_data: Option<Box<dyn Fn(f32) + Send + Sync>>,
    pub on_amiibo_data: Option<Box<dyn Fn(&TagInfo) + Send + Sync>>,
    pub on_camera_data: Option<Box<dyn Fn(&[u8], IrsResolution) + Send + Sync>>,
}
