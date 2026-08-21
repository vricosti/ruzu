// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/abstracted_pad/abstract_battery_handler.h and abstract_battery_handler.cpp

use common::ResultCode;

use crate::hid_result;
use crate::hid_types::*;

/// Handles Npad battery request from HID interfaces
pub struct NpadAbstractBatteryHandler {
    ref_counter: i32,
    dual_battery: NpadPowerInfo,
    left_battery: NpadPowerInfo,
    right_battery: NpadPowerInfo,
    has_new_battery_data: bool,
}

impl Default for NpadAbstractBatteryHandler {
    fn default() -> Self {
        Self {
            ref_counter: 0,
            dual_battery: NpadPowerInfo::default(),
            left_battery: NpadPowerInfo::default(),
            right_battery: NpadPowerInfo::default(),
            has_new_battery_data: false,
        }
    }
}

impl NpadAbstractBatteryHandler {
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

    pub fn update_battery_state(&mut self) {
        if self.ref_counter == 0 {
            return;
        }
        self.has_new_battery_data = self.get_new_battery_state();
    }

    /// Checks abstracted pads for new battery data and updates internal state.
    /// Returns true if any battery data changed.
    fn get_new_battery_state(&mut self) -> bool {
        // Note: Upstream iterates over abstract_pad_holder->GetAbstractedPads() and checks
        // assignment_style to determine which battery (dual/left/right) to update.
        // Without the full abstracted pad infrastructure wired up, we preserve the structure
        // and return false (no change) as a safe default.
        false
    }

    pub fn update_core_battery_state(&mut self) {
        if self.ref_counter == 0 {
            return;
        }
        if !self.has_new_battery_data {
            return;
        }
        // Upstream calls UpdateBatteryState(0) here
    }

    pub fn initialize_battery_state(&mut self) {
        // Upstream calls UpdateBatteryState(aruid) here
    }

    pub fn has_battery(&self) -> bool {
        // Upstream iterates pads checking disabled_feature_set.has_fullkey_battery
        // or disabled_feature_set.has_left_right_joy_battery
        false
    }

    pub fn has_left_right_battery(&self) -> (bool, bool) {
        // Returns (has_left, has_right)
        // Upstream iterates pads checking assignment_style
        (false, false)
    }

    pub fn get_dual_battery(&self) -> &NpadPowerInfo {
        &self.dual_battery
    }

    pub fn get_left_battery(&self) -> &NpadPowerInfo {
        &self.left_battery
    }

    pub fn get_right_battery(&self) -> &NpadPowerInfo {
        &self.right_battery
    }
}
