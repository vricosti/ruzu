// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/abstracted_pad/abstract_mcu_handler.h and abstract_mcu_handler.cpp

use std::sync::Arc;

use common::ResultCode;
use parking_lot::Mutex;

use crate::hid_result;
use crate::resources::abstracted_pad::abstract_pad_holder::AbstractPadRef;
use crate::resources::abstracted_pad::abstract_properties_handler::NpadAbstractPropertiesHandler;

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
    abstracted_pad: Option<AbstractPadRef>,
}

impl Default for NpadMcuHolder {
    fn default() -> Self {
        Self {
            state: NpadMcuState::None,
            abstracted_pad: None,
        }
    }
}

/// Handles Npad MCU request from HID interfaces
pub struct NpadAbstractMcuHandler {
    properties_handler: Option<Arc<Mutex<NpadAbstractPropertiesHandler>>>,
    ref_counter: i32,
    mcu_holder: [NpadMcuHolder; 2],
}

impl Default for NpadAbstractMcuHandler {
    fn default() -> Self {
        Self {
            properties_handler: None,
            ref_counter: 0,
            mcu_holder: Default::default(),
        }
    }
}

impl NpadAbstractMcuHandler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_properties_handler(&mut self, handler: Arc<Mutex<NpadAbstractPropertiesHandler>>) {
        self.properties_handler = Some(handler);
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
        let Some(properties_handler) = &self.properties_handler else {
            self.mcu_holder = Default::default();
            return;
        };
        let abstract_pads = properties_handler.lock().get_abstracted_pads();
        if abstract_pads.is_empty() {
            self.mcu_holder = Default::default();
            return;
        }

        for abstract_pad in abstract_pads {
            let pad = abstract_pad.lock();
            if !pad.internal_flags.is_connected() {
                continue;
            }
            if !pad.disabled_feature_set.has_left_joy_rail_bus() {
                if !pad.disabled_feature_set.has_left_joy_six_axis_sensor()
                    && !pad.disabled_feature_set.has_right_joy_six_axis_sensor()
                {
                    continue;
                }
                if self.mcu_holder[1].state != NpadMcuState::Active {
                    self.mcu_holder[1].state = NpadMcuState::Available;
                }
                self.mcu_holder[1].abstracted_pad = Some(Arc::clone(&abstract_pad));
                continue;
            }
            if self.mcu_holder[0].state != NpadMcuState::Active {
                self.mcu_holder[0].state = NpadMcuState::Available;
            }
            self.mcu_holder[0].abstracted_pad = Some(Arc::clone(&abstract_pad));
        }
    }

    pub fn get_abstracted_pad(&self, mcu_index: u32) -> Result<AbstractPadRef, ResultCode> {
        let holder = &self.mcu_holder[mcu_index as usize];
        if holder.state == NpadMcuState::None {
            return Err(hid_result::RESULT_MCU_IS_NOT_READY);
        }
        holder
            .abstracted_pad
            .clone()
            .ok_or(hid_result::RESULT_MCU_IS_NOT_READY)
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
