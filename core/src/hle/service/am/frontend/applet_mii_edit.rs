// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/frontend/applet_mii_edit.h
//! Port of zuyu/src/core/hle/service/am/frontend/applet_mii_edit.cpp

/// MiiEdit frontend applet stub.
pub struct MiiEdit {
    is_complete: bool,
}

impl MiiEdit {
    pub fn new() -> Self {
        Self { is_complete: false }
    }
}
