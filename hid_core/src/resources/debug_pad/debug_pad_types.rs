// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/debug_pad/debug_pad_types.h and debug_pad_types.cpp

use crate::hid_types::{AnalogStickState, DebugPadButton};

/// This is nn::hid::DebugPadAttribute
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct DebugPadAttribute {
    pub raw: u32,
}
const _: () = assert!(std::mem::size_of::<DebugPadAttribute>() == 0x4);

impl DebugPadAttribute {
    pub fn connected(&self) -> bool {
        (self.raw & (1 << 0)) != 0
    }
}

/// This is nn::hid::DebugPadState
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct DebugPadState {
    pub sampling_number: i64,
    pub attribute: DebugPadAttribute,
    pub pad_state: DebugPadButton,
    pub r_stick: AnalogStickState,
    pub l_stick: AnalogStickState,
}
const _: () = assert!(std::mem::size_of::<DebugPadState>() == 0x20);
