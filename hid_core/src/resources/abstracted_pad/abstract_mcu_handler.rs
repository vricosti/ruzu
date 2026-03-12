// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/abstracted_pad/abstract_mcu_handler.h and abstract_mcu_handler.cpp

use common::ResultCode;

use crate::hid_result;
use crate::resources::npad::npad_types::IAbstractedPad;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u32)]
pub enum NpadMcuState {
    #[default]
    None = 0,
    Available = 1,
    Active = 2,
}

struct NpadMcuHolder {
    state: NpadMcuState,
}

impl Default for NpadMcuHolder {
    fn default() -> Self {
        Self {
            state: NpadMcuState::None,
        }
    }
}

/// Handles Npad MCU request from HID interfaces
pub struct NpadAbstractMcuHandler {
    ref_counter: i32,
    mcu_holder: [NpadMcuHolder; 2],
}

impl Default for NpadAbstractMcuHandler {
    fn default() -> Self {
        Self {
            ref_counter: 0,
            mcu_holder: Default::default(),
        }
    }
}

impl NpadAbstractMcuHandler {
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

    pub fn update_mcu_state(&mut self) {
        // Upstream iterates over properties_handler->GetAbstractedPads() and:
        // - If pad is connected and has_left_joy_rail_bus, sets mcu_holder[0] to Available
        // - If pad is connected and has left/right six_axis_sensor, sets mcu_holder[1] to Available
        // Without full pad infrastructure, reset holders
        self.mcu_holder = Default::default();
    }

    pub fn get_mcu_state(&self, mcu_index: u32) -> NpadMcuState {
        self.mcu_holder[mcu_index as usize].state
    }

    pub fn set_mcu_state(&mut self, is_enabled: bool, mcu_index: u32) -> ResultCode {
        let state = &mut self.mcu_holder[mcu_index as usize].state;

        if *state == NpadMcuState::None {
            return hid_result::RESULT_MCU_IS_NOT_READY;
        }

        if is_enabled && *state == NpadMcuState::Available {
            *state = NpadMcuState::Active;
            return ResultCode::SUCCESS;
        }

        if is_enabled {
            return ResultCode::SUCCESS;
        }
        if *state != NpadMcuState::Active {
            return ResultCode::SUCCESS;
        }

        *state = NpadMcuState::Available;
        ResultCode::SUCCESS
    }
}
