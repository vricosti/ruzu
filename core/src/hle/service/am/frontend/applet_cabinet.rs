// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/frontend/applet_cabinet.h
//! Port of zuyu/src/core/hle/service/am/frontend/applet_cabinet.cpp

/// Port of CabinetAppletVersion
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CabinetAppletVersion {
    Version1 = 0x1,
}

/// Port of CabinetFlags
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CabinetFlags {
    None = 0,
    DeviceHandle = 1,
    TagInfo = 2,
    RegisterInfo = 4,
}

/// Port of CabinetResult
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CabinetResult {
    Cancel = 0,
    TagInfo = 2,
    RegisterInfo = 4,
}

/// Port of NFP::CabinetMode (nn::nfp::CabinetMode) — u8 enum.
/// Defined here to avoid a cross-crate dependency on nfp for this one value.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CabinetMode {
    #[default]
    StartNicknameAndOwnerSettings = 0,
    StartGameDataEraser = 1,
    StartRestorer = 2,
    StartFormatter = 3,
}

/// Port of StartParamForAmiiboSettings (nn::nfp::StartParamForAmiiboSettings) — 0x1A8 bytes.
///
/// Upstream uses `#pragma pack(push, 1)` so there is no alignment padding.
/// `tag_info` mirrors `NFC::TagInfo` (0x58 bytes) and `register_info` mirrors
/// `NFP::RegisterInfo` (0x100 bytes); both are represented as raw byte arrays
/// to avoid cross-crate dependencies.
#[repr(C, packed)]
#[derive(Clone, Copy)]
pub struct StartParamForAmiiboSettings {
    pub param_1: u8,
    pub applet_mode: CabinetMode,
    pub flags: CabinetFlags,
    pub amiibo_settings_1: u8,
    pub device_handle: u64,
    pub tag_info: [u8; 0x58],       // NFC::TagInfo
    pub register_info: [u8; 0x100], // NFP::RegisterInfo
    pub amiibo_settings_3: [u8; 0x20],
    pub _padding: [u8; 0x24],
}
const _: () = assert!(std::mem::size_of::<StartParamForAmiiboSettings>() == 0x1A8);

impl Default for StartParamForAmiiboSettings {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

/// Port of AmiiboSettingsStartParam (nn::nfp::AmiiboSettingsStartParam)
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct AmiiboSettingsStartParam {
    pub device_handle: u64,
    pub param_1: [u8; 0x20],
    pub param_2: u8,
    _padding: [u8; 7],
}
const _: () = assert!(std::mem::size_of::<AmiiboSettingsStartParam>() == 0x30);

impl Default for AmiiboSettingsStartParam {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

/// Cabinet frontend applet stub.
pub struct Cabinet {
    is_complete: bool,
}

impl Cabinet {
    pub fn new() -> Self {
        Self { is_complete: false }
    }
}
