// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/vibration/vibration_base.h and vibration_base.cpp

use common::ResultCode;

/// Base for Npad vibration devices. Manages activation reference counting and
/// mount state.
pub struct NpadVibrationBase {
    pub ref_counter: i32,
    pub is_mounted: bool,
}

impl NpadVibrationBase {
    pub fn new() -> Self {
        Self {
            ref_counter: 0,
            is_mounted: false,
        }
    }

    pub fn activate(&mut self) -> ResultCode {
        self.ref_counter += 1;
        ResultCode::SUCCESS
    }

    pub fn deactivate(&mut self) -> ResultCode {
        if self.ref_counter > 0 {
            self.ref_counter -= 1;
        }
        ResultCode::SUCCESS
    }

    pub fn is_active(&self) -> bool {
        self.ref_counter > 0
    }

    pub fn is_vibration_mounted(&self) -> bool {
        self.is_mounted
    }
}

impl Default for NpadVibrationBase {
    fn default() -> Self {
        Self::new()
    }
}
