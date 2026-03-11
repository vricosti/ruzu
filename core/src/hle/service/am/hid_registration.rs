// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/hid_registration.h
//! Port of zuyu/src/core/hle/service/am/hid_registration.cpp

/// Port of HidRegistration
///
/// Manages HID resource registration for an applet process.
/// Stubbed: requires HID service integration.
pub struct HidRegistration {
    // TODO: Process reference, IHidServer reference
}

impl HidRegistration {
    pub fn new() -> Self {
        Self {}
    }

    pub fn enable_applet_to_get_input(&self, _enable: bool) {
        // TODO: forward to HID resource manager
    }
}
