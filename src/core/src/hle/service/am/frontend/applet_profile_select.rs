// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/frontend/applet_profile_select.h
//! Port of zuyu/src/core/hle/service/am/frontend/applet_profile_select.cpp

/// ProfileSelect frontend applet stub.
pub struct ProfileSelect {
    complete: bool,
}

impl ProfileSelect {
    pub fn new() -> Self {
        Self { complete: false }
    }
}
