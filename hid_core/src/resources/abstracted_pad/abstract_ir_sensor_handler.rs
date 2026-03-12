// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/abstracted_pad/abstract_ir_sensor_handler.h and abstract_ir_sensor_handler.cpp

use common::ResultCode;

use crate::hid_result;

/// IR sensor state enum
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u32)]
pub enum NpadIrSensorState {
    #[default]
    Disabled = 0,
    Unavailable = 1,
    Available = 2,
    Active = 3,
}

/// Handles Npad IR sensor request from HID interfaces
pub struct NpadAbstractIrSensorHandler {
    ref_counter: i32,
    sensor_state: NpadIrSensorState,
}

impl Default for NpadAbstractIrSensorHandler {
    fn default() -> Self {
        Self {
            ref_counter: 0,
            sensor_state: NpadIrSensorState::Disabled,
        }
    }
}

impl NpadAbstractIrSensorHandler {
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

    pub fn update_ir_sensor_state(&mut self) {
        // Upstream iterates abstract pads looking for one with has_bluetooth_address.
        // Without the pad infrastructure wired up, we keep current state.
        let _previous_state = self.sensor_state;
        // If count == 0, set to Disabled and signal if changed
        // If found pad with bluetooth, set Available (unless Active)
        // Otherwise set Unavailable
    }

    pub fn activate_ir_sensor(&mut self, is_enabled: bool) -> ResultCode {
        if self.sensor_state == NpadIrSensorState::Unavailable {
            return hid_result::RESULT_IR_SENSOR_IS_NOT_READY;
        }
        if is_enabled && self.sensor_state == NpadIrSensorState::Available {
            self.sensor_state = NpadIrSensorState::Active;
        } else {
            if is_enabled {
                return ResultCode::SUCCESS;
            }
            if self.sensor_state != NpadIrSensorState::Active {
                return ResultCode::SUCCESS;
            }
            self.sensor_state = NpadIrSensorState::Available;
        }
        // Upstream signals ir_sensor_event here
        ResultCode::SUCCESS
    }

    pub fn get_xcd_handle_for_npad_with_ir_sensor(&self) -> Result<u64, ResultCode> {
        if (self.sensor_state as u32) < (NpadIrSensorState::Available as u32) {
            return Err(hid_result::RESULT_IR_SENSOR_IS_NOT_READY);
        }
        Ok(0)
    }

    pub fn get_sensor_state(&self) -> NpadIrSensorState {
        self.sensor_state
    }
}
