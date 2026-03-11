// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/frontend/applets.h
//! Port of zuyu/src/core/hle/service/am/frontend/applets.cpp
//!
//! Base FrontendApplet class and FrontendAppletHolder.

use crate::hle::service::am::am_types::{AppletId, LibraryAppletMode};

/// Base trait for all frontend applet implementations.
///
/// Port of FrontendApplet class.
pub trait FrontendApplet: Send + Sync {
    fn initialize(&mut self);
    fn execute_interactive(&mut self);
    fn execute(&mut self);
    fn request_exit(&mut self);
    fn get_library_applet_mode(&self) -> LibraryAppletMode;
    fn is_initialized(&self) -> bool;
}

/// Holds the set of frontend applet implementations.
///
/// Port of FrontendAppletHolder class.
pub struct FrontendAppletHolder {
    current_applet_id: AppletId,
}

impl FrontendAppletHolder {
    pub fn new() -> Self {
        Self {
            current_applet_id: AppletId::None,
        }
    }

    pub fn get_current_applet_id(&self) -> AppletId {
        self.current_applet_id
    }

    pub fn set_current_applet_id(&mut self, applet_id: AppletId) {
        self.current_applet_id = applet_id;
    }
}
