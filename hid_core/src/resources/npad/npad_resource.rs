// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/npad/npad_resource.h and npad_resource.cpp

use common::ResultCode;

use crate::hid_result;
use crate::hid_types::*;
use crate::hid_util;
use crate::resources::applet_resource::ARUID_INDEX_MAX;
use crate::resources::npad::npad_data::NPadData;
use crate::resources::npad::npad_types::*;

/// Handles Npad resource management from HID interfaces
pub struct NPadResource {
    active_data: NPadData,
    active_data_aruid: u64,
    default_hold_type: NpadJoyHoldType,
    ref_counter: i32,
    state: Vec<NpadState>,
}

struct NpadState {
    aruid: u64,
    data: NPadData,
    npad_revision: NpadRevision,
}

impl Default for NpadState {
    fn default() -> Self {
        Self {
            aruid: 0,
            data: NPadData::new(),
            npad_revision: NpadRevision::Revision0,
        }
    }
}

impl Default for NPadResource {
    fn default() -> Self {
        let mut state = Vec::with_capacity(ARUID_INDEX_MAX);
        for _ in 0..ARUID_INDEX_MAX {
            state.push(NpadState::default());
        }
        Self {
            active_data: NPadData::new(),
            active_data_aruid: 0,
            default_hold_type: NpadJoyHoldType::Vertical,
            ref_counter: 0,
            state,
        }
    }
}

impl NPadResource {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_active_data(&self) -> &NPadData {
        &self.active_data
    }

    pub fn get_active_data_mut(&mut self) -> &mut NPadData {
        &mut self.active_data
    }

    pub fn get_active_data_aruid(&self) -> u64 {
        self.active_data_aruid
    }

    pub fn set_app_resource_user_id(&mut self, aruid: u64) {
        self.active_data_aruid = aruid;
        // Upstream copies data from the matching state entry to active_data
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            // Copy data to active_data would happen here
            let _ = idx;
        }
    }

    fn get_index_from_aruid(&self, aruid: u64) -> Option<usize> {
        for i in 0..self.state.len() {
            if self.state[i].aruid == aruid {
                return Some(i);
            }
        }
        None
    }

    pub fn apply_npad_system_common_policy(&mut self, aruid: u64, is_full_policy: bool) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            self.state[idx].data.set_npad_system_common_policy(is_full_policy);
            return ResultCode::SUCCESS;
        }
        hid_result::RESULT_ARUID_NOT_REGISTERED
    }

    pub fn clear_npad_system_common_policy(&mut self, aruid: u64) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            self.state[idx].data.clear_npad_system_common_policy();
            return ResultCode::SUCCESS;
        }
        hid_result::RESULT_ARUID_NOT_REGISTERED
    }

    pub fn set_supported_npad_style_set(&mut self, aruid: u64, style_set: NpadStyleSet) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            self.state[idx].data.set_supported_npad_style_set(style_set);
            return ResultCode::SUCCESS;
        }
        hid_result::RESULT_ARUID_NOT_REGISTERED
    }

    pub fn get_supported_npad_style_set(&self, aruid: u64) -> Result<NpadStyleSet, ResultCode> {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            return Ok(self.state[idx].data.get_supported_npad_style_set());
        }
        Err(hid_result::RESULT_ARUID_NOT_REGISTERED)
    }

    pub fn is_supported_npad_style_set(&self, aruid: u64) -> Result<bool, ResultCode> {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            return Ok(self.state[idx].data.get_npad_status().is_supported_styleset_set());
        }
        Err(hid_result::RESULT_ARUID_NOT_REGISTERED)
    }

    pub fn get_npad_revision(&self, aruid: u64) -> NpadRevision {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            return self.state[idx].npad_revision;
        }
        NpadRevision::Revision0
    }

    pub fn set_npad_revision(&mut self, aruid: u64, revision: NpadRevision) {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            self.state[idx].npad_revision = revision;
        }
    }

    pub fn set_npad_joy_hold_type(&mut self, aruid: u64, hold_type: NpadJoyHoldType) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            self.state[idx].data.set_npad_joy_hold_type(hold_type);
            return ResultCode::SUCCESS;
        }
        hid_result::RESULT_ARUID_NOT_REGISTERED
    }

    pub fn get_npad_joy_hold_type(&self, aruid: u64) -> Result<NpadJoyHoldType, ResultCode> {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            return Ok(self.state[idx].data.get_npad_joy_hold_type());
        }
        Err(hid_result::RESULT_ARUID_NOT_REGISTERED)
    }

    pub fn set_npad_handheld_activation_mode(
        &mut self,
        aruid: u64,
        activation_mode: NpadHandheldActivationMode,
    ) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            self.state[idx].data.set_handheld_activation_mode(activation_mode);
            return ResultCode::SUCCESS;
        }
        hid_result::RESULT_ARUID_NOT_REGISTERED
    }

    pub fn get_npad_handheld_activation_mode(
        &self,
        aruid: u64,
    ) -> Result<NpadHandheldActivationMode, ResultCode> {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            return Ok(self.state[idx].data.get_handheld_activation_mode());
        }
        Err(hid_result::RESULT_ARUID_NOT_REGISTERED)
    }

    pub fn set_supported_npad_id_type(
        &mut self,
        aruid: u64,
        supported_npad_list: &[NpadIdType],
    ) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            return self.state[idx].data.set_supported_npad_id_type(supported_npad_list);
        }
        hid_result::RESULT_ARUID_NOT_REGISTERED
    }

    pub fn is_controller_supported(&self, aruid: u64, style_index: NpadStyleIndex) -> bool {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            return self.state[idx].data.is_npad_style_index_supported(style_index);
        }
        false
    }

    pub fn set_lr_assignment_mode(&mut self, aruid: u64, is_enabled: bool) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            self.state[idx].data.set_lr_assignment_mode(is_enabled);
            return ResultCode::SUCCESS;
        }
        hid_result::RESULT_ARUID_NOT_REGISTERED
    }

    pub fn get_lr_assignment_mode(&self, aruid: u64) -> Result<bool, ResultCode> {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            return Ok(self.state[idx].data.get_lr_assignment_mode());
        }
        Err(hid_result::RESULT_ARUID_NOT_REGISTERED)
    }

    pub fn set_assigning_single_on_sl_sr_press(&mut self, aruid: u64, is_enabled: bool) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            self.state[idx].data.set_assigning_single_on_sl_sr_press(is_enabled);
            return ResultCode::SUCCESS;
        }
        hid_result::RESULT_ARUID_NOT_REGISTERED
    }

    pub fn get_home_protection_enabled(
        &self,
        aruid: u64,
        npad_id: NpadIdType,
    ) -> Result<bool, ResultCode> {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            return Ok(self.state[idx].data.get_home_protection_enabled(npad_id));
        }
        Err(hid_result::RESULT_ARUID_NOT_REGISTERED)
    }

    pub fn set_home_protection_enabled(
        &mut self,
        aruid: u64,
        npad_id: NpadIdType,
        is_enabled: bool,
    ) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            self.state[idx].data.set_home_protection_enabled(is_enabled, npad_id);
            return ResultCode::SUCCESS;
        }
        hid_result::RESULT_ARUID_NOT_REGISTERED
    }

    pub fn set_npad_analog_stick_use_center_clamp(&mut self, aruid: u64, is_enabled: bool) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            self.state[idx].data.set_npad_analog_stick_use_center_clamp(is_enabled);
            return ResultCode::SUCCESS;
        }
        hid_result::RESULT_ARUID_NOT_REGISTERED
    }

    pub fn set_npad_system_ext_state_enabled(&mut self, aruid: u64, is_enabled: bool) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            self.state[idx].data.set_npad_system_ext_state_enabled(is_enabled);
            return ResultCode::SUCCESS;
        }
        hid_result::RESULT_ARUID_NOT_REGISTERED
    }
}
