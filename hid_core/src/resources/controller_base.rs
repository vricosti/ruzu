// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/resources/controller_base.h and controller_base.cpp

use common::ResultCode;

/// Base trait for HID controllers
pub trait ControllerBase {
    fn on_init(&mut self);
    fn on_release(&mut self);
    fn on_update(&mut self);
    fn on_motion_update(&mut self) {}
}

/// Shared controller activation state
pub struct ControllerActivation {
    pub is_activated: bool,
}

impl ControllerActivation {
    pub fn new() -> Self {
        Self {
            is_activated: false,
        }
    }

    pub fn activate(&mut self) -> ResultCode {
        self.is_activated = true;
        ResultCode::SUCCESS
    }

    pub fn deactivate(&mut self) {
        self.is_activated = false;
    }

    pub fn is_controller_activated(&self) -> bool {
        self.is_activated
    }
}

impl Default for ControllerActivation {
    fn default() -> Self {
        Self::new()
    }
}
