// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/npad/npad_data.h and npad_data.cpp

use common::ResultCode;

use crate::hid_result;
use crate::hid_types::*;
use crate::hid_util;
use crate::resources::npad::npad_types::*;

/// Handles Npad data state from HID interfaces
pub struct NPadData {
    status: NpadStatus,
    supported_npad_style_set: NpadStyleSet,
    npad_hold_type: NpadJoyHoldType,
    handheld_activation_mode: NpadHandheldActivationMode,
    supported_npad_id_types: [NpadIdType; MAX_SUPPORTED_NPAD_ID_TYPES],
    npad_button_assignment: [NpadButton; STYLE_INDEX_COUNT],
    supported_npad_id_types_count: usize,
    is_unintended_home_button_input_protection: [bool; MAX_SUPPORTED_NPAD_ID_TYPES],
}

impl Default for NPadData {
    fn default() -> Self {
        let mut data = Self {
            status: NpadStatus::default(),
            supported_npad_style_set: NpadStyleSet::ALL,
            npad_hold_type: NpadJoyHoldType::Vertical,
            handheld_activation_mode: NpadHandheldActivationMode::Dual,
            supported_npad_id_types: [NpadIdType::Invalid; MAX_SUPPORTED_NPAD_ID_TYPES],
            npad_button_assignment: [NpadButton::NONE; STYLE_INDEX_COUNT],
            supported_npad_id_types_count: 0,
            is_unintended_home_button_input_protection: [false; MAX_SUPPORTED_NPAD_ID_TYPES],
        };
        data.clear_npad_system_common_policy();
        data
    }
}

impl NPadData {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_npad_status(&self) -> NpadStatus {
        self.status
    }

    pub fn set_npad_analog_stick_use_center_clamp(&mut self, is_enabled: bool) {
        self.status.set_use_center_clamp(is_enabled);
    }

    pub fn get_npad_analog_stick_use_center_clamp(&self) -> bool {
        self.status.use_center_clamp()
    }

    pub fn set_npad_system_ext_state_enabled(&mut self, is_enabled: bool) {
        self.status.set_system_ext_state(is_enabled);
    }

    pub fn get_npad_system_ext_state(&self) -> bool {
        self.status.system_ext_state()
    }

    pub fn set_supported_npad_id_type(&mut self, list: &[NpadIdType]) -> ResultCode {
        // Note: Real limit is 11. But array size is 10. N's bug?
        if list.len() > MAX_SUPPORTED_NPAD_ID_TYPES {
            return hid_result::RESULT_INVALID_ARRAY_SIZE;
        }

        self.supported_npad_id_types_count = list.len();
        for (i, &npad_id) in list.iter().enumerate() {
            self.supported_npad_id_types[i] = npad_id;
        }

        ResultCode::SUCCESS
    }

    pub fn get_supported_npad_id_type(&self, out_list: &mut [NpadIdType]) -> usize {
        let out_size = std::cmp::min(self.supported_npad_id_types_count, out_list.len());
        out_list[..out_size].copy_from_slice(&self.supported_npad_id_types[..out_size]);
        out_size
    }

    pub fn is_npad_id_type_supported(&self, npad_id: NpadIdType) -> bool {
        for i in 0..self.supported_npad_id_types_count {
            if self.supported_npad_id_types[i] == npad_id {
                return true;
            }
        }
        false
    }

    pub fn set_npad_system_common_policy(&mut self, is_full_policy: bool) {
        self.supported_npad_style_set = NpadStyleSet::FULLKEY
            | NpadStyleSet::JOY_DUAL
            | NpadStyleSet::SYSTEM_EXT
            | NpadStyleSet::SYSTEM;
        self.handheld_activation_mode = NpadHandheldActivationMode::Dual;

        self.status.set_is_supported_styleset_set(true);
        self.status.set_is_hold_type_set(true);
        self.status.set_lr_assignment_mode(false);
        self.status.set_is_policy(true);
        if is_full_policy {
            self.status.set_is_full_policy(true);
        }

        self.set_default_npad_id_types();

        for input_protection in &mut self.is_unintended_home_button_input_protection {
            *input_protection = true;
        }
    }

    pub fn clear_npad_system_common_policy(&mut self) {
        self.status.raw = 0;
        self.supported_npad_style_set = NpadStyleSet::ALL;
        self.npad_hold_type = NpadJoyHoldType::Vertical;
        self.handheld_activation_mode = NpadHandheldActivationMode::Dual;

        for button_assignment in &mut self.npad_button_assignment {
            *button_assignment = NpadButton::NONE;
        }

        self.set_default_npad_id_types();

        for input_protection in &mut self.is_unintended_home_button_input_protection {
            *input_protection = true;
        }
    }

    fn set_default_npad_id_types(&mut self) {
        self.supported_npad_id_types_count = 10;
        self.supported_npad_id_types[0] = NpadIdType::Player1;
        self.supported_npad_id_types[1] = NpadIdType::Player2;
        self.supported_npad_id_types[2] = NpadIdType::Player3;
        self.supported_npad_id_types[3] = NpadIdType::Player4;
        self.supported_npad_id_types[4] = NpadIdType::Player5;
        self.supported_npad_id_types[5] = NpadIdType::Player6;
        self.supported_npad_id_types[6] = NpadIdType::Player7;
        self.supported_npad_id_types[7] = NpadIdType::Player8;
        self.supported_npad_id_types[8] = NpadIdType::Other;
        self.supported_npad_id_types[9] = NpadIdType::Handheld;
    }

    pub fn set_npad_joy_hold_type(&mut self, hold_type: NpadJoyHoldType) {
        self.npad_hold_type = hold_type;
        self.status.set_is_hold_type_set(true);
    }

    pub fn get_npad_joy_hold_type(&self) -> NpadJoyHoldType {
        self.npad_hold_type
    }

    pub fn set_handheld_activation_mode(&mut self, activation_mode: NpadHandheldActivationMode) {
        self.handheld_activation_mode = activation_mode;
    }

    pub fn get_handheld_activation_mode(&self) -> NpadHandheldActivationMode {
        self.handheld_activation_mode
    }

    pub fn set_supported_npad_style_set(&mut self, style_set: NpadStyleSet) {
        self.supported_npad_style_set = style_set;
        self.status.set_is_supported_styleset_set(true);
        self.status.set_is_hold_type_set(true);
    }

    pub fn get_supported_npad_style_set(&self) -> NpadStyleSet {
        self.supported_npad_style_set
    }

    pub fn is_npad_style_index_supported(&self, style_index: NpadStyleIndex) -> bool {
        let style = self.supported_npad_style_set;
        match style_index {
            NpadStyleIndex::Fullkey => style.contains(NpadStyleSet::FULLKEY),
            NpadStyleIndex::Handheld => style.contains(NpadStyleSet::HANDHELD),
            NpadStyleIndex::JoyconDual => style.contains(NpadStyleSet::JOY_DUAL),
            NpadStyleIndex::JoyconLeft => style.contains(NpadStyleSet::JOY_LEFT),
            NpadStyleIndex::JoyconRight => style.contains(NpadStyleSet::JOY_RIGHT),
            NpadStyleIndex::GameCube => style.contains(NpadStyleSet::GC),
            NpadStyleIndex::Pokeball => style.contains(NpadStyleSet::PALMA),
            NpadStyleIndex::NES => style.contains(NpadStyleSet::LARK),
            NpadStyleIndex::SNES => style.contains(NpadStyleSet::LUCIA),
            NpadStyleIndex::N64 => style.contains(NpadStyleSet::LAGOON),
            NpadStyleIndex::SegaGenesis => style.contains(NpadStyleSet::LAGER),
            _ => false,
        }
    }

    pub fn set_lr_assignment_mode(&mut self, is_enabled: bool) {
        self.status.set_lr_assignment_mode(is_enabled);
    }

    pub fn get_lr_assignment_mode(&self) -> bool {
        self.status.lr_assignment_mode()
    }

    pub fn set_assigning_single_on_sl_sr_press(&mut self, is_enabled: bool) {
        self.status.set_assigning_single_on_sl_sr_press(is_enabled);
    }

    pub fn get_assigning_single_on_sl_sr_press(&self) -> bool {
        self.status.assigning_single_on_sl_sr_press()
    }

    pub fn set_home_protection_enabled(&mut self, is_enabled: bool, npad_id: NpadIdType) {
        self.is_unintended_home_button_input_protection
            [hid_util::npad_id_type_to_index(npad_id)] = is_enabled;
    }

    pub fn get_home_protection_enabled(&self, npad_id: NpadIdType) -> bool {
        self.is_unintended_home_button_input_protection
            [hid_util::npad_id_type_to_index(npad_id)]
    }

    pub fn set_capture_button_assignment(
        &mut self,
        button_assignment: NpadButton,
        style_index: usize,
    ) {
        if style_index < self.npad_button_assignment.len() {
            self.npad_button_assignment[style_index] = button_assignment;
        }
    }

    pub fn get_capture_button_assignment(&self, style_index: usize) -> NpadButton {
        if style_index < self.npad_button_assignment.len() {
            self.npad_button_assignment[style_index]
        } else {
            NpadButton::NONE
        }
    }

    pub fn get_npad_capture_button_assignment_list(
        &self,
        out_list: &mut [NpadButton],
    ) -> usize {
        for i in 0..out_list.len() {
            let style_set = hid_util::get_styleset_by_index(i);
            if (style_set & self.supported_npad_style_set).is_empty()
                || self.npad_button_assignment[i] == NpadButton::NONE
            {
                return i;
            }
            out_list[i] = self.npad_button_assignment[i];
        }
        out_list.len()
    }
}
