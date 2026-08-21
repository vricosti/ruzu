// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/keyboard/keyboard_types.h and keyboard_types.cpp

use crate::hid_types::{KeyboardAttribute, KeyboardKey, KeyboardModifier};

/// This is nn::hid::detail::KeyboardState
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct KeyboardState {
    pub sampling_number: i64,
    pub modifier: KeyboardModifier,
    pub attribute: KeyboardAttribute,
    pub key: KeyboardKey,
}
const _: () = assert!(std::mem::size_of::<KeyboardState>() == 0x30);
