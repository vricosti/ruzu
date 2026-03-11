// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/frontend/applet_software_keyboard_types.h

/// Port of SwkbdType
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SwkbdType {
    Normal = 0,
    NumberPad = 1,
    Qwerty = 2,
    Unknown3 = 3,
    Latin = 4,
    SimplifiedChinese = 5,
    TraditionalChinese = 6,
    Korean = 7,
}

/// Port of SwkbdInitialCursorPosition
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SwkbdInitialCursorPosition {
    First = 0,
    Last = 1,
}

/// Port of SwkbdPasswordMode
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SwkbdPasswordMode {
    Show = 0,
    Hide = 1,
}

/// Port of SwkbdTextCheckResult
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SwkbdTextCheckResult {
    Success = 0,
    ShowFailureDialog = 1,
    ShowConfirmDialog = 2,
    Silent = 3,
}

/// Port of SwkbdResult
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SwkbdResult {
    Ok = 0,
    Cancel = 1,
}
