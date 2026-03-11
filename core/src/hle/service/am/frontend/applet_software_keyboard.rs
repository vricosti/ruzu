// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/frontend/applet_software_keyboard.h
//! Port of zuyu/src/core/hle/service/am/frontend/applet_software_keyboard.cpp

/// SoftwareKeyboard frontend applet stub.
pub struct SoftwareKeyboard {
    is_complete: bool,
}

impl SoftwareKeyboard {
    pub fn new() -> Self {
        Self { is_complete: false }
    }
}
