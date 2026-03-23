// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/frontend/applet_mii_edit_types.h

/// Port of MiiEditAppletVersion
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MiiEditAppletVersion {
    Version3 = 0x3,
    Version4 = 0x4,
}

/// Port of MiiEditAppletMode (nn::mii::AppletMode)
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MiiEditAppletMode {
    ShowMiiEdit = 0,
    AppendMii = 1,
    AppendMiiImage = 2,
    UpdateMiiImage = 3,
    CreateMii = 4,
    EditMii = 5,
}

/// Port of MiiEditResult
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MiiEditResult {
    Success = 0,
    Cancel = 1,
}

/// Port of MiiEditAppletInputCommon
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct MiiEditAppletInputCommon {
    pub version: i32,
    pub applet_mode: u32,
}
const _: () = assert!(std::mem::size_of::<MiiEditAppletInputCommon>() == 0x8);

/// Port of MiiEditAppletInputV3 — size is 0x100 - sizeof(MiiEditAppletInputCommon) = 0xF8.
///
/// `valid_uuids` and `used_uuid` use `[u8; 16]` (not `u128`) to match the
/// 1-byte alignment of C++ `Common::UUID` (`std::array<u8, 0x10>`).
#[repr(C)]
#[derive(Clone, Copy)]
pub struct MiiEditAppletInputV3 {
    pub special_mii_key_code: u32,
    pub valid_uuids: [[u8; 16]; 8],
    pub used_uuid: [u8; 16],
    pub _padding: [u8; 0x64],
}
const _: () = assert!(
    std::mem::size_of::<MiiEditAppletInputV3>() == 0x100 - std::mem::size_of::<MiiEditAppletInputCommon>()
);

impl Default for MiiEditAppletInputV3 {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

/// Port of MiiEditAppletInputV4 — size is 0x100 - sizeof(MiiEditAppletInputCommon) = 0xF8.
///
/// `char_info` mirrors `Mii::CharInfo` (0x58 bytes) as a raw byte array to
/// avoid a cross-crate dependency on the Mii service.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct MiiEditAppletInputV4 {
    pub special_mii_key_code: u32,
    pub char_info: [u8; 0x58], // Mii::CharInfo
    pub _padding1: [u8; 0x28],
    pub used_uuid: [u8; 16],
    pub _padding2: [u8; 0x64],
}
const _: () = assert!(
    std::mem::size_of::<MiiEditAppletInputV4>() == 0x100 - std::mem::size_of::<MiiEditAppletInputCommon>()
);

impl Default for MiiEditAppletInputV4 {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

/// Local aggregate matching upstream's `MiiEditV3` anonymous struct in
/// `PushInShowMiiEditData`. Version3 uses `MiiEditAppletInputV3`.
/// `static_assert(sizeof(MiiEditV3) == 0x100)` in upstream.
#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct MiiEditV3 {
    pub common: MiiEditAppletInputCommon,
    pub input: MiiEditAppletInputV3,
}
const _: () = assert!(std::mem::size_of::<MiiEditV3>() == 0x100);

/// Port of MiiEditAppletOutput (nn::mii::AppletOutput)
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct MiiEditAppletOutput {
    pub result: u32,
    pub index: i32,
    _padding: [u8; 0x18],
}
const _: () = assert!(std::mem::size_of::<MiiEditAppletOutput>() == 0x20);

impl Default for MiiEditAppletOutput {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}
