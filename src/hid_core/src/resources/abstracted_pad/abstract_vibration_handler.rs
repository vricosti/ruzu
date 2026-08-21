// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/abstracted_pad/abstract_vibration_handler.h and abstract_vibration_handler.cpp

use common::ResultCode;

use crate::hid_result;

/// Keeps track of vibration state and updates npad vibration devices
pub struct NpadAbstractVibrationHandler {
    ref_counter: i32,
}

impl Default for NpadAbstractVibrationHandler {
    fn default() -> Self {
        Self { ref_counter: 0 }
    }
}

impl NpadAbstractVibrationHandler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn increment_ref_counter(&mut self) -> ResultCode {
        if self.ref_counter == i32::MAX - 1 {
            return hid_result::RESULT_NPAD_HANDLER_OVERFLOW;
        }
        self.ref_counter += 1;
        ResultCode::SUCCESS
    }

    pub fn decrement_ref_counter(&mut self) -> ResultCode {
        if self.ref_counter == 0 {
            return hid_result::RESULT_NPAD_HANDLER_NOT_INITIALIZED;
        }
        self.ref_counter -= 1;
        ResultCode::SUCCESS
    }

    pub fn update_vibration_state(&mut self) {
        // Upstream checks handheld_config flags, then gets abstracted pads from
        // properties_handler and mounts/unmounts vibration devices based on
        // connection state.
        // Without full pad infrastructure wired up, this is a no-op.
    }
}
