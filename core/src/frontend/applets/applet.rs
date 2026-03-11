// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/frontend/applets/applet.h
//! Base applet trait.

/// Base trait for all frontend applets.
///
/// Corresponds to upstream `Core::Frontend::Applet`.
pub trait Applet: Send + Sync {
    /// Close the applet.
    fn close(&self);
}
