// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/abstracted_pad/abstract_sixaxis_handler.h and abstract_sixaxis_handler.cpp

use common::ResultCode;

use crate::hid_result;

/// Handles Npad six-axis sensor request from HID interfaces
pub struct NpadAbstractSixAxisHandler {
    ref_counter: i32,
}

impl Default for NpadAbstractSixAxisHandler {
    fn default() -> Self {
        Self { ref_counter: 0 }
    }
}

impl NpadAbstractSixAxisHandler {
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

    pub fn is_firmware_update_available(&self) -> u64 {
        // Upstream TODO: not yet implemented in C++ upstream
        0
    }

    pub fn update_six_axis_state(&mut self) -> ResultCode {
        // Upstream iterates over AruidIndexMax aruid data entries and calls
        // UpdateSixaxisInternalState for each
        ResultCode::SUCCESS
    }

    pub fn update_six_axis_state_for_aruid(&mut self, _aruid: u64) -> ResultCode {
        // Upstream gets aruid data and calls UpdateSixaxisInternalState
        ResultCode::SUCCESS
    }

    pub fn update_six_axis_state2(&mut self, _aruid: u64) -> ResultCode {
        // Same as update_six_axis_state_for_aruid but named differently upstream
        ResultCode::SUCCESS
    }

    // Private helper methods - upstream has TODO stubs for all of these:

    fn update_sixaxis_fullkey_lifo(&mut self, _is_sensor_enabled: bool) {
        // Upstream TODO: not yet implemented in C++ upstream
    }

    fn update_six_axis_palma_lifo(&mut self, _is_sensor_enabled: bool) {
        // Upstream TODO: not yet implemented in C++ upstream
    }

    fn update_sixaxis_handheld_lifo(&mut self, _is_sensor_enabled: bool) {
        // Upstream TODO: not yet implemented in C++ upstream
    }

    fn update_sixaxis_dual_lifo(&mut self, _is_sensor_enabled: bool) {
        // Upstream TODO: not yet implemented in C++ upstream
    }

    fn update_sixaxis_left_lifo(&mut self, _is_sensor_enabled: bool) {
        // Upstream TODO: not yet implemented in C++ upstream
    }

    fn update_sixaxis_right_lifo(&mut self, _is_sensor_enabled: bool) {
        // Upstream TODO: not yet implemented in C++ upstream
    }
}
