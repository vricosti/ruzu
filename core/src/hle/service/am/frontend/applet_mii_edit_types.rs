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
