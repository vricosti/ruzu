// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/npad/npad_resource.h and npad_resource.cpp

use common::ResultCode;

use crate::hid_result;
use crate::hid_types::*;
use crate::resources::applet_resource::{
    DataStatusFlag, RegistrationStatus, ARUID_INDEX_MAX, SYSTEM_ARUID,
};
use crate::resources::npad::npad_data::NPadData;
use crate::resources::npad::npad_types::*;

/// Handles Npad resource management from HID interfaces
pub struct NPadResource {
    active_data: NPadData,
    active_data_aruid: u64,
    default_hold_type: NpadJoyHoldType,
    ref_counter: i32,
    state: Vec<NpadState>,
    registration_list: NpadRegistrationList,
}

struct NpadState {
    flag: DataStatusFlag,
    aruid: u64,
    data: NPadData,
    npad_revision: NpadRevision,
}

impl Default for NpadState {
    fn default() -> Self {
        Self {
            flag: DataStatusFlag::default(),
            aruid: 0,
            data: NPadData::new(),
            npad_revision: NpadRevision::Revision0,
        }
    }
}

/// Registration list for NpadResource, matching AppletResource's pattern.
struct NpadRegistrationList {
    flag: [RegistrationStatus; ARUID_INDEX_MAX],
    aruid: [u64; ARUID_INDEX_MAX],
}

impl Default for NpadRegistrationList {
    fn default() -> Self {
        Self {
            flag: [RegistrationStatus::None; ARUID_INDEX_MAX],
            aruid: [0u64; ARUID_INDEX_MAX],
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
            registration_list: NpadRegistrationList::default(),
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
        if self.active_data_aruid == aruid {
            return;
        }
        self.active_data_aruid = aruid;
        self.default_hold_type = self.active_data.get_npad_joy_hold_type();
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            let data = &mut self.state[idx].data;
            if data.get_npad_status().is_policy() || data.get_npad_status().is_full_policy() {
                data.set_npad_joy_hold_type(self.default_hold_type);
            }
            self.active_data = data.clone();
            if data.get_npad_status().is_hold_type_set() {
                self.active_data
                    .set_npad_joy_hold_type(self.default_hold_type);
            }
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

    pub fn apply_npad_system_common_policy(
        &mut self,
        aruid: u64,
        is_full_policy: bool,
    ) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            self.state[idx]
                .data
                .set_npad_system_common_policy(is_full_policy);
            self.state[idx]
                .data
                .set_npad_joy_hold_type(self.default_hold_type);
            if self.active_data_aruid == aruid {
                self.active_data
                    .set_npad_system_common_policy(is_full_policy);
                self.active_data
                    .set_npad_joy_hold_type(self.default_hold_type);
            }
            return ResultCode::SUCCESS;
        }
        hid_result::RESULT_NPAD_NOT_CONNECTED
    }

    pub fn clear_npad_system_common_policy(&mut self, aruid: u64) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            self.state[idx].data.clear_npad_system_common_policy();
            if self.active_data_aruid == aruid {
                self.active_data.clear_npad_system_common_policy();
            }
            return ResultCode::SUCCESS;
        }
        hid_result::RESULT_NPAD_NOT_CONNECTED
    }

    pub fn set_supported_npad_style_set(
        &mut self,
        aruid: u64,
        style_set: NpadStyleSet,
    ) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            self.state[idx].data.set_supported_npad_style_set(style_set);
            if self.active_data_aruid == aruid {
                self.active_data.set_supported_npad_style_set(style_set);
                let hold_type = self.state[idx].data.get_npad_joy_hold_type();
                self.active_data.set_npad_joy_hold_type(hold_type);
            }
            return ResultCode::SUCCESS;
        }
        hid_result::RESULT_NPAD_NOT_CONNECTED
    }

    pub fn get_supported_npad_style_set(&self, aruid: u64) -> Result<NpadStyleSet, ResultCode> {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            let data = &self.state[idx].data;
            if !data.get_npad_status().is_supported_styleset_set() {
                return Err(hid_result::RESULT_UNDEFINED_STYLESET);
            }
            return Ok(self.state[idx].data.get_supported_npad_style_set());
        }
        Err(hid_result::RESULT_NPAD_NOT_CONNECTED)
    }

    pub fn is_supported_npad_style_set(&self, aruid: u64) -> Result<bool, ResultCode> {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            return Ok(self.state[idx]
                .data
                .get_npad_status()
                .is_supported_styleset_set());
        }
        Err(hid_result::RESULT_NPAD_NOT_CONNECTED)
    }

    fn npad_style_mask_for_revision(&self, idx: usize) -> NpadStyleSet {
        match self.state[idx].npad_revision {
            NpadRevision::Revision1 => {
                NpadStyleSet::FULLKEY
                    | NpadStyleSet::HANDHELD
                    | NpadStyleSet::JOY_DUAL
                    | NpadStyleSet::JOY_LEFT
                    | NpadStyleSet::JOY_RIGHT
                    | NpadStyleSet::GC
                    | NpadStyleSet::PALMA
                    | NpadStyleSet::SYSTEM_EXT
                    | NpadStyleSet::SYSTEM
            }
            NpadRevision::Revision2 => {
                NpadStyleSet::FULLKEY
                    | NpadStyleSet::HANDHELD
                    | NpadStyleSet::JOY_DUAL
                    | NpadStyleSet::JOY_LEFT
                    | NpadStyleSet::JOY_RIGHT
                    | NpadStyleSet::GC
                    | NpadStyleSet::PALMA
                    | NpadStyleSet::LARK
                    | NpadStyleSet::SYSTEM_EXT
                    | NpadStyleSet::SYSTEM
            }
            NpadRevision::Revision3 => {
                NpadStyleSet::FULLKEY
                    | NpadStyleSet::HANDHELD
                    | NpadStyleSet::JOY_DUAL
                    | NpadStyleSet::JOY_LEFT
                    | NpadStyleSet::JOY_RIGHT
                    | NpadStyleSet::GC
                    | NpadStyleSet::PALMA
                    | NpadStyleSet::LARK
                    | NpadStyleSet::HANDHELD_LARK
                    | NpadStyleSet::LUCIA
                    | NpadStyleSet::LAGOON
                    | NpadStyleSet::LAGER
                    | NpadStyleSet::SYSTEM_EXT
                    | NpadStyleSet::SYSTEM
            }
            _ => {
                NpadStyleSet::FULLKEY
                    | NpadStyleSet::HANDHELD
                    | NpadStyleSet::JOY_DUAL
                    | NpadStyleSet::JOY_LEFT
                    | NpadStyleSet::JOY_RIGHT
                    | NpadStyleSet::SYSTEM_EXT
                    | NpadStyleSet::SYSTEM
            }
        }
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
            if self.active_data_aruid == aruid {
                self.active_data.set_npad_joy_hold_type(hold_type);
            }
            return ResultCode::SUCCESS;
        }
        hid_result::RESULT_NPAD_NOT_CONNECTED
    }

    pub fn get_npad_joy_hold_type(&self, aruid: u64) -> Result<NpadJoyHoldType, ResultCode> {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            let data = &self.state[idx].data;
            if data.get_npad_status().is_policy() || data.get_npad_status().is_full_policy() {
                return Ok(self.active_data.get_npad_joy_hold_type());
            }
            return Ok(data.get_npad_joy_hold_type());
        }
        Err(hid_result::RESULT_NPAD_NOT_CONNECTED)
    }

    pub fn set_npad_handheld_activation_mode(
        &mut self,
        aruid: u64,
        activation_mode: NpadHandheldActivationMode,
    ) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            self.state[idx]
                .data
                .set_handheld_activation_mode(activation_mode);
            if self.active_data_aruid == aruid {
                self.active_data
                    .set_handheld_activation_mode(activation_mode);
            }
            return ResultCode::SUCCESS;
        }
        hid_result::RESULT_NPAD_NOT_CONNECTED
    }

    pub fn get_npad_handheld_activation_mode(
        &self,
        aruid: u64,
    ) -> Result<NpadHandheldActivationMode, ResultCode> {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            return Ok(self.state[idx].data.get_handheld_activation_mode());
        }
        Err(hid_result::RESULT_NPAD_NOT_CONNECTED)
    }

    pub fn set_supported_npad_id_type(
        &mut self,
        aruid: u64,
        supported_npad_list: &[NpadIdType],
    ) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            let result = self.state[idx]
                .data
                .set_supported_npad_id_type(supported_npad_list);
            if result.is_success() && self.active_data_aruid == aruid {
                return self
                    .active_data
                    .set_supported_npad_id_type(supported_npad_list);
            }
            return result;
        }
        hid_result::RESULT_NPAD_NOT_CONNECTED
    }

    pub fn is_controller_supported(&self, aruid: u64, style_index: NpadStyleIndex) -> bool {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            return self.state[idx]
                .data
                .is_npad_style_index_supported(style_index);
        }
        false
    }

    pub fn set_lr_assignment_mode(&mut self, aruid: u64, is_enabled: bool) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            self.state[idx].data.set_lr_assignment_mode(is_enabled);
            if self.active_data_aruid == aruid {
                self.active_data.set_lr_assignment_mode(is_enabled);
            }
            return ResultCode::SUCCESS;
        }
        hid_result::RESULT_NPAD_NOT_CONNECTED
    }

    pub fn get_lr_assignment_mode(&self, aruid: u64) -> Result<bool, ResultCode> {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            return Ok(self.state[idx].data.get_lr_assignment_mode());
        }
        Err(hid_result::RESULT_NPAD_NOT_CONNECTED)
    }

    pub fn set_assigning_single_on_sl_sr_press(
        &mut self,
        aruid: u64,
        is_enabled: bool,
    ) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            self.state[idx]
                .data
                .set_assigning_single_on_sl_sr_press(is_enabled);
            if self.active_data_aruid == aruid {
                self.active_data
                    .set_assigning_single_on_sl_sr_press(is_enabled);
            }
            return ResultCode::SUCCESS;
        }
        hid_result::RESULT_NPAD_NOT_CONNECTED
    }

    /// Port of NPadResource::IsAssigningSingleOnSlSrPressEnabled.
    pub fn is_assigning_single_on_sl_sr_press_enabled(
        &self,
        aruid: u64,
    ) -> Result<bool, ResultCode> {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            return Ok(self.state[idx].data.get_assigning_single_on_sl_sr_press());
        }
        Err(hid_result::RESULT_NPAD_NOT_CONNECTED)
    }

    /// Port of NPadResource::GetMaskedSupportedNpadStyleSet.
    pub fn get_masked_supported_npad_style_set(
        &self,
        aruid: u64,
    ) -> Result<NpadStyleSet, ResultCode> {
        if aruid == SYSTEM_ARUID {
            return Ok(NpadStyleSet::FULLKEY
                | NpadStyleSet::HANDHELD
                | NpadStyleSet::JOY_DUAL
                | NpadStyleSet::JOY_LEFT
                | NpadStyleSet::JOY_RIGHT
                | NpadStyleSet::PALMA
                | NpadStyleSet::SYSTEM_EXT
                | NpadStyleSet::SYSTEM);
        }

        let Some(idx) = self.get_index_from_aruid(aruid) else {
            return Err(hid_result::RESULT_NPAD_NOT_CONNECTED);
        };
        let data = &self.state[idx].data;
        if !data.get_npad_status().is_supported_styleset_set() {
            return Err(hid_result::RESULT_UNDEFINED_STYLESET);
        }
        Ok(data.get_supported_npad_style_set() & self.npad_style_mask_for_revision(idx))
    }

    /// Port of NPadResource::GetAvailableStyleset.
    pub fn get_available_styleset(&self, aruid: u64) -> Result<NpadStyleSet, ResultCode> {
        let Some(idx) = self.get_index_from_aruid(aruid) else {
            return Err(hid_result::RESULT_NPAD_NOT_CONNECTED);
        };
        let data = &self.state[idx].data;
        if !data.get_npad_status().is_supported_styleset_set() {
            return Err(hid_result::RESULT_UNDEFINED_STYLESET);
        }
        Ok(data.get_supported_npad_style_set() & self.npad_style_mask_for_revision(idx))
    }

    pub fn get_home_protection_enabled(
        &self,
        aruid: u64,
        npad_id: NpadIdType,
    ) -> Result<bool, ResultCode> {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            return Ok(self.state[idx].data.get_home_protection_enabled(npad_id));
        }
        Err(hid_result::RESULT_NPAD_NOT_CONNECTED)
    }

    pub fn set_home_protection_enabled(
        &mut self,
        aruid: u64,
        npad_id: NpadIdType,
        is_enabled: bool,
    ) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            self.state[idx]
                .data
                .set_home_protection_enabled(is_enabled, npad_id);
            if self.active_data_aruid == aruid {
                self.active_data
                    .set_home_protection_enabled(is_enabled, npad_id);
            }
            return ResultCode::SUCCESS;
        }
        hid_result::RESULT_NPAD_NOT_CONNECTED
    }

    pub fn set_npad_analog_stick_use_center_clamp(
        &mut self,
        aruid: u64,
        is_enabled: bool,
    ) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            self.state[idx]
                .data
                .set_npad_analog_stick_use_center_clamp(is_enabled);
            if self.active_data_aruid == aruid {
                self.active_data
                    .set_npad_analog_stick_use_center_clamp(is_enabled);
            }
            return ResultCode::SUCCESS;
        }
        hid_result::RESULT_NPAD_NOT_CONNECTED
    }

    pub fn set_npad_system_ext_state_enabled(
        &mut self,
        aruid: u64,
        is_enabled: bool,
    ) -> ResultCode {
        if let Some(idx) = self.get_index_from_aruid(aruid) {
            self.state[idx]
                .data
                .set_npad_analog_stick_use_center_clamp(is_enabled);
            if self.active_data_aruid == aruid {
                self.active_data
                    .set_npad_analog_stick_use_center_clamp(is_enabled);
            }
            return ResultCode::SUCCESS;
        }
        hid_result::RESULT_NPAD_NOT_CONNECTED
    }

    /// Port of NPadResource::RegisterAppletResourceUserId.
    pub fn register_applet_resource_user_id(&mut self, aruid: u64) -> ResultCode {
        let aruid_index = self.get_index_from_aruid(aruid);
        if aruid_index.is_some() {
            return hid_result::RESULT_ARUID_ALREADY_REGISTERED;
        }

        let mut data_index = None;
        for i in 0..ARUID_INDEX_MAX {
            if !self.state[i].flag.is_initialized() {
                data_index = Some(i);
                break;
            }
        }

        let data_index = match data_index {
            Some(idx) => idx,
            None => return hid_result::RESULT_ARUID_NO_AVAILABLE_ENTRIES,
        };

        self.state[data_index].aruid = aruid;
        self.state[data_index].flag.set_is_initialized(true);

        // Find registration list slot
        let mut reg_index = None;
        for i in 0..ARUID_INDEX_MAX {
            if self.registration_list.flag[i] == RegistrationStatus::Initialized {
                if self.registration_list.aruid[i] != aruid {
                    continue;
                }
                reg_index = Some(i);
                break;
            }
            if self.registration_list.flag[i] == RegistrationStatus::None
                || self.registration_list.flag[i] == RegistrationStatus::PendingDelete
            {
                reg_index = Some(i);
                break;
            }
        }

        if let Some(ri) = reg_index {
            self.registration_list.flag[ri] = RegistrationStatus::Initialized;
            self.registration_list.aruid[ri] = aruid;
        }

        ResultCode::SUCCESS
    }

    /// Port of NPadResource::UnregisterAppletResourceUserId.
    pub fn unregister_applet_resource_user_id(&mut self, aruid: u64) {
        let aruid_index = self.get_index_from_aruid(aruid);

        self.free_applet_resource_id(aruid);

        if let Some(idx) = aruid_index {
            self.state[idx] = NpadState::default();
            self.registration_list.flag[idx] = RegistrationStatus::PendingDelete;
        }

        for i in 0..ARUID_INDEX_MAX {
            if self.registration_list.flag[i] == RegistrationStatus::Initialized {
                self.active_data_aruid = self.registration_list.aruid[i];
            }
        }
    }

    /// Port of NPadResource::FreeAppletResourceId.
    pub fn free_applet_resource_id(&mut self, aruid: u64) {
        let aruid_index = match self.get_index_from_aruid(aruid) {
            Some(idx) => idx,
            None => return,
        };

        self.state[aruid_index].flag.set_is_assigned(false);
    }

    /// Port of NPadResource::Activate(u64 aruid).
    pub fn activate_with_aruid(&mut self, aruid: u64) -> ResultCode {
        let aruid_index = match self.get_index_from_aruid(aruid) {
            Some(idx) => idx,
            None => return ResultCode::SUCCESS,
        };

        if self.state[aruid_index].flag.is_assigned() {
            return hid_result::RESULT_ARUID_ALREADY_REGISTERED;
        }

        self.state[aruid_index].flag.set_is_assigned(true);
        self.state[aruid_index]
            .data
            .clear_npad_system_common_policy();
        self.state[aruid_index].npad_revision = NpadRevision::Revision0;

        if self.active_data_aruid == aruid {
            self.default_hold_type = self.active_data.get_npad_joy_hold_type();
            self.active_data
                .set_npad_joy_hold_type(self.default_hold_type);
        }
        ResultCode::SUCCESS
    }

    /// Port of NPadResource::Activate() (no-aruid version).
    pub fn activate(&mut self) -> ResultCode {
        if self.ref_counter == i32::MAX - 1 {
            return hid_result::RESULT_APPLET_RESOURCE_OVERFLOW;
        }
        if self.ref_counter == 0 {
            self.register_applet_resource_user_id(SYSTEM_ARUID);
            self.activate_with_aruid(SYSTEM_ARUID);
        }
        self.ref_counter += 1;
        ResultCode::SUCCESS
    }
}

#[cfg(test)]
mod tests {
    use super::NPadResource;
    use crate::hid_result;
    use crate::hid_types::NpadStyleSet;
    use crate::resources::applet_resource::SYSTEM_ARUID;
    use crate::resources::npad::npad_types::{
        NpadHandheldActivationMode, NpadJoyHoldType, NpadRevision,
    };

    const ARUID: u64 = 0x1234;

    #[test]
    fn supported_style_set_errors_match_upstream() {
        let mut resource = NPadResource::new();

        assert_eq!(
            resource.get_supported_npad_style_set(ARUID).unwrap_err(),
            hid_result::RESULT_NPAD_NOT_CONNECTED
        );

        assert!(resource
            .register_applet_resource_user_id(ARUID)
            .is_success());
        assert_eq!(
            resource.get_supported_npad_style_set(ARUID).unwrap_err(),
            hid_result::RESULT_UNDEFINED_STYLESET
        );
    }

    #[test]
    fn masked_style_set_applies_revision_mask() {
        let mut resource = NPadResource::new();
        assert!(resource
            .register_applet_resource_user_id(ARUID)
            .is_success());
        assert!(resource
            .set_supported_npad_style_set(ARUID, NpadStyleSet::ALL)
            .is_success());

        assert_eq!(
            resource.get_masked_supported_npad_style_set(ARUID).unwrap(),
            NpadStyleSet::FULLKEY
                | NpadStyleSet::HANDHELD
                | NpadStyleSet::JOY_DUAL
                | NpadStyleSet::JOY_LEFT
                | NpadStyleSet::JOY_RIGHT
                | NpadStyleSet::SYSTEM_EXT
                | NpadStyleSet::SYSTEM
        );

        resource.set_npad_revision(ARUID, NpadRevision::Revision3);
        assert_eq!(
            resource.get_masked_supported_npad_style_set(ARUID).unwrap(),
            NpadStyleSet::FULLKEY
                | NpadStyleSet::HANDHELD
                | NpadStyleSet::JOY_DUAL
                | NpadStyleSet::JOY_LEFT
                | NpadStyleSet::JOY_RIGHT
                | NpadStyleSet::GC
                | NpadStyleSet::PALMA
                | NpadStyleSet::LARK
                | NpadStyleSet::HANDHELD_LARK
                | NpadStyleSet::LUCIA
                | NpadStyleSet::LAGOON
                | NpadStyleSet::LAGER
                | NpadStyleSet::SYSTEM_EXT
                | NpadStyleSet::SYSTEM
        );
    }

    #[test]
    fn unknown_revision_uses_upstream_default_mask() {
        let mut resource = NPadResource::new();
        assert!(resource
            .register_applet_resource_user_id(ARUID)
            .is_success());
        assert!(resource
            .set_supported_npad_style_set(ARUID, NpadStyleSet::ALL)
            .is_success());

        resource.set_npad_revision(ARUID, NpadRevision::from_raw(5));
        assert_eq!(
            resource.get_masked_supported_npad_style_set(ARUID).unwrap(),
            NpadStyleSet::FULLKEY
                | NpadStyleSet::HANDHELD
                | NpadStyleSet::JOY_DUAL
                | NpadStyleSet::JOY_LEFT
                | NpadStyleSet::JOY_RIGHT
                | NpadStyleSet::SYSTEM_EXT
                | NpadStyleSet::SYSTEM
        );
    }

    #[test]
    fn available_styleset_matches_masked_revision_path() {
        let mut resource = NPadResource::new();
        assert!(resource
            .register_applet_resource_user_id(ARUID)
            .is_success());

        assert_eq!(
            resource.get_available_styleset(ARUID).unwrap_err(),
            hid_result::RESULT_UNDEFINED_STYLESET
        );

        assert!(resource
            .set_supported_npad_style_set(ARUID, NpadStyleSet::ALL)
            .is_success());
        resource.set_npad_revision(ARUID, NpadRevision::Revision1);

        assert_eq!(
            resource.get_available_styleset(ARUID).unwrap(),
            resource.get_masked_supported_npad_style_set(ARUID).unwrap()
        );
    }

    #[test]
    fn active_data_tracks_upstream_mirrored_updates() {
        let mut resource = NPadResource::new();
        assert!(resource
            .register_applet_resource_user_id(ARUID)
            .is_success());
        resource.set_app_resource_user_id(ARUID);

        assert!(resource
            .set_npad_joy_hold_type(ARUID, NpadJoyHoldType::Horizontal)
            .is_success());
        assert_eq!(
            resource.get_active_data().get_npad_joy_hold_type(),
            NpadJoyHoldType::Horizontal
        );

        assert!(resource
            .set_npad_handheld_activation_mode(ARUID, NpadHandheldActivationMode::None)
            .is_success());
        assert_eq!(
            resource.get_active_data().get_handheld_activation_mode(),
            NpadHandheldActivationMode::None
        );
    }

    #[test]
    fn system_aruid_mask_matches_upstream_special_case() {
        let resource = NPadResource::new();

        assert_eq!(
            resource
                .get_masked_supported_npad_style_set(SYSTEM_ARUID)
                .unwrap(),
            NpadStyleSet::FULLKEY
                | NpadStyleSet::HANDHELD
                | NpadStyleSet::JOY_DUAL
                | NpadStyleSet::JOY_LEFT
                | NpadStyleSet::JOY_RIGHT
                | NpadStyleSet::PALMA
                | NpadStyleSet::SYSTEM_EXT
                | NpadStyleSet::SYSTEM
        );
    }
}
