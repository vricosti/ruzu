// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/frontend/applet_controller.h
//! Port of zuyu/src/core/hle/service/am/frontend/applet_controller.cpp

pub type IdentificationColor = [u8; 4];
pub type ExplainText = [u8; 0x81];

/// Port of ControllerAppletVersion
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ControllerAppletVersion {
    Version3 = 0x3,
    Version4 = 0x4,
    Version5 = 0x5,
    Version7 = 0x7,
    Version8 = 0x8,
}

/// Port of ControllerSupportMode
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ControllerSupportMode {
    ShowControllerSupport = 0,
    ShowControllerStrapGuide = 1,
    ShowControllerFirmwareUpdate = 2,
    ShowControllerKeyRemappingForSystem = 3,
    MaxControllerSupportMode = 4,
}

/// Port of ControllerSupportCaller
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ControllerSupportCaller {
    Application = 0,
    System = 1,
    MaxControllerSupportCaller = 2,
}

/// Port of ControllerSupportResult
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ControllerSupportResult {
    Success = 0,
    Cancel = 2,
}

/// Port of ControllerSupportArgPrivate
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct ControllerSupportArgPrivate {
    pub arg_private_size: u32,
    pub arg_size: u32,
    pub is_home_menu: bool,
    pub flag_1: bool,
    pub mode: u8,   // ControllerSupportMode
    pub caller: u8, // ControllerSupportCaller
    pub style_set: u32,
    pub joy_hold_type: u32,
}
const _: () = assert!(std::mem::size_of::<ControllerSupportArgPrivate>() == 0x14);

/// Port of ControllerSupportArgHeader
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct ControllerSupportArgHeader {
    pub player_count_min: i8,
    pub player_count_max: i8,
    pub enable_take_over_connection: bool,
    pub enable_left_justify: bool,
    pub enable_permit_joy_dual: bool,
    pub enable_single_mode: bool,
    pub enable_identification_color: bool,
}
const _: () = assert!(std::mem::size_of::<ControllerSupportArgHeader>() == 0x7);

/// Port of ControllerSupportArgOld (LibraryAppletVersion 0x3, 0x4, 0x5) — 0x21C bytes.
///
/// `identification_colors`: 4 entries × 4 bytes each.
/// `explain_text`: 4 entries × 0x81 bytes each.
/// No padding between fields (all fields have alignment ≤ 1).
#[repr(C)]
#[derive(Clone, Copy)]
pub struct ControllerSupportArgOld {
    pub header: ControllerSupportArgHeader,
    pub identification_colors: [IdentificationColor; 4],
    pub enable_explain_text: bool,
    pub explain_text: [ExplainText; 4],
}
const _: () = assert!(std::mem::size_of::<ControllerSupportArgOld>() == 0x21C);

impl Default for ControllerSupportArgOld {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

/// Port of ControllerSupportArgNew (LibraryAppletVersion 0x7, 0x8) — 0x430 bytes.
///
/// `identification_colors`: 8 entries × 4 bytes each.
/// `explain_text`: 8 entries × 0x81 bytes each.
/// No padding between fields (all fields have alignment ≤ 1).
#[repr(C)]
#[derive(Clone, Copy)]
pub struct ControllerSupportArgNew {
    pub header: ControllerSupportArgHeader,
    pub identification_colors: [IdentificationColor; 8],
    pub enable_explain_text: bool,
    pub explain_text: [ExplainText; 8],
}
const _: () = assert!(std::mem::size_of::<ControllerSupportArgNew>() == 0x430);

impl Default for ControllerSupportArgNew {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

/// Port of ControllerSupportResultInfo
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct ControllerSupportResultInfo {
    pub player_count: i8,
    _padding: [u8; 3],
    pub selected_id: u32,
    pub result: u32, // ControllerSupportResult
}
const _: () = assert!(std::mem::size_of::<ControllerSupportResultInfo>() == 0xC);

/// Controller frontend applet stub.
pub struct Controller {
    complete: bool,
}

impl Controller {
    pub fn new() -> Self {
        Self { complete: false }
    }
}
