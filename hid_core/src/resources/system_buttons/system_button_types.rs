// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/system_buttons/system_button_types.h and system_button_types.cpp

use crate::hid_types;

/// This is nn::hid::system::SleepButtonState (resource version, not hid_types version)
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct SleepButtonState {
    pub sampling_number: i64,
    pub buttons: hid_types::SleepButtonState,
}
const _: () = assert!(std::mem::size_of::<SleepButtonState>() == 0x10);

/// This is nn::hid::system::HomeButtonState (resource version)
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct HomeButtonState {
    pub sampling_number: i64,
    pub buttons: hid_types::HomeButtonState,
}
const _: () = assert!(std::mem::size_of::<HomeButtonState>() == 0x10);

/// This is nn::hid::system::CaptureButtonState (resource version)
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct CaptureButtonState {
    pub sampling_number: i64,
    pub buttons: hid_types::CaptureButtonState,
}
const _: () = assert!(std::mem::size_of::<CaptureButtonState>() == 0x10);
