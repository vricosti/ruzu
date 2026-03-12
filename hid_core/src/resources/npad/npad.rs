// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/resources/npad/npad.h and npad.cpp
//!
//! Main NPad controller resource managing all npad-related state including
//! style sets, vibration, six-axis sensors, and abstracted pad management.

use common::ResultCode;

use crate::hid_result;
use crate::hid_types::*;
use crate::resources::npad::npad_resource::NPadResource;
use crate::resources::npad::npad_types::*;
use crate::resources::npad::npad_vibration::NpadVibration;

/// Main NPad controller resource
pub struct NPad {
    npad_resource: NPadResource,
    vibration: NpadVibration,
    ref_counter: i32,
}

impl Default for NPad {
    fn default() -> Self {
        Self {
            npad_resource: NPadResource::new(),
            vibration: NpadVibration::new(),
            ref_counter: 0,
        }
    }
}

impl NPad {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn activate(&mut self) -> ResultCode {
        self.ref_counter += 1;
        ResultCode::SUCCESS
    }

    pub fn activate_for_aruid(&mut self, _aruid: u64) -> ResultCode {
        ResultCode::SUCCESS
    }

    pub fn free_applet_resource_id(&mut self, _aruid: u64) {
        // Upstream cleans up applet resource data for the given aruid
    }

    pub fn set_supported_npad_style_set(
        &mut self,
        aruid: u64,
        supported_style_set: NpadStyleSet,
    ) -> ResultCode {
        self.npad_resource.set_supported_npad_style_set(aruid, supported_style_set)
    }

    pub fn get_supported_npad_style_set(
        &self,
        aruid: u64,
    ) -> Result<NpadStyleSet, ResultCode> {
        self.npad_resource.get_supported_npad_style_set(aruid)
    }

    pub fn set_supported_npad_id_type(
        &mut self,
        aruid: u64,
        supported_npad_list: &[NpadIdType],
    ) -> ResultCode {
        self.npad_resource.set_supported_npad_id_type(aruid, supported_npad_list)
    }

    pub fn set_npad_joy_hold_type(&mut self, aruid: u64, hold_type: NpadJoyHoldType) -> ResultCode {
        self.npad_resource.set_npad_joy_hold_type(aruid, hold_type)
    }

    pub fn get_npad_joy_hold_type(&self, aruid: u64) -> Result<NpadJoyHoldType, ResultCode> {
        self.npad_resource.get_npad_joy_hold_type(aruid)
    }

    pub fn set_npad_handheld_activation_mode(
        &mut self,
        aruid: u64,
        mode: NpadHandheldActivationMode,
    ) -> ResultCode {
        self.npad_resource.set_npad_handheld_activation_mode(aruid, mode)
    }

    pub fn get_npad_handheld_activation_mode(
        &self,
        aruid: u64,
    ) -> Result<NpadHandheldActivationMode, ResultCode> {
        self.npad_resource.get_npad_handheld_activation_mode(aruid)
    }

    pub fn set_npad_communication_mode(&mut self, _communication_mode: NpadCommunicationMode) -> ResultCode {
        ResultCode::SUCCESS
    }

    pub fn get_npad_communication_mode(&self) -> NpadCommunicationMode {
        NpadCommunicationMode::Default
    }

    pub fn set_npad_joy_assignment_mode_single_by_default(
        &mut self,
        _aruid: u64,
        _npad_id: NpadIdType,
    ) -> ResultCode {
        ResultCode::SUCCESS
    }

    pub fn set_npad_joy_assignment_mode_dual(
        &mut self,
        _aruid: u64,
        _npad_id: NpadIdType,
    ) -> ResultCode {
        ResultCode::SUCCESS
    }

    pub fn apply_npad_system_common_policy(&mut self, aruid: u64) -> ResultCode {
        self.npad_resource.apply_npad_system_common_policy(aruid, false)
    }

    pub fn apply_npad_system_common_policy_full(&mut self, aruid: u64) -> ResultCode {
        self.npad_resource.apply_npad_system_common_policy(aruid, true)
    }

    pub fn clear_npad_system_common_policy(&mut self, aruid: u64) -> ResultCode {
        self.npad_resource.clear_npad_system_common_policy(aruid)
    }

    pub fn set_vibration_master_volume(&self, master_volume: f32) -> ResultCode {
        self.vibration.set_vibration_master_volume(master_volume)
    }

    pub fn get_vibration_master_volume(&self) -> Result<f32, ResultCode> {
        self.vibration.get_vibration_master_volume()
    }

    pub fn begin_permit_vibration_session(&self, aruid: u64) -> ResultCode {
        self.vibration.begin_permit_vibration_session(aruid)
    }

    pub fn end_permit_vibration_session(&self) -> ResultCode {
        self.vibration.end_permit_vibration_session()
    }

    pub fn npad_resource(&self) -> &NPadResource {
        &self.npad_resource
    }

    pub fn npad_resource_mut(&mut self) -> &mut NPadResource {
        &mut self.npad_resource
    }
}
