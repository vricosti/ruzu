// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/frontend/applets/mii_edit.h and mii_edit.cpp
//! Mii editor applet interface.

use super::applet::Applet;

/// Callback type for Mii edit completion.
///
/// Corresponds to upstream `MiiEditApplet::MiiEditCallback`.
pub type MiiEditCallback = Box<dyn FnOnce() + Send>;

/// Mii edit applet trait.
///
/// Corresponds to upstream `Core::Frontend::MiiEditApplet`.
pub trait MiiEditApplet: Applet {
    fn show_mii_edit(&self, callback: MiiEditCallback);
}

/// Default (stub) Mii edit applet implementation.
///
/// Corresponds to upstream `Core::Frontend::DefaultMiiEditApplet`.
pub struct DefaultMiiEditApplet;

impl Applet for DefaultMiiEditApplet {
    fn close(&self) {}
}

impl MiiEditApplet for DefaultMiiEditApplet {
    fn show_mii_edit(&self, callback: MiiEditCallback) {
        log::warn!("(STUBBED) called");
        callback();
    }
}
