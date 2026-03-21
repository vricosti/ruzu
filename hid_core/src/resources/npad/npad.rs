// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/resources/npad/npad.h and npad.cpp
//!
//! Main NPad controller resource managing all npad-related state including
//! style sets, vibration, six-axis sensors, and abstracted pad management.

use common::ResultCode;

use crate::hid_result;
use crate::hid_types::*;
use crate::resources::applet_resource::AppletResourceHolder;
use crate::resources::npad::npad_resource::NPadResource;
use crate::resources::npad::npad_types::*;
use crate::resources::npad::npad_vibration::NpadVibration;

/// Main NPad controller resource
pub struct NPad {
    npad_resource: NPadResource,
    vibration: NpadVibration,
    ref_counter: i32,
    applet_resource_holder: AppletResourceHolder,
}

impl Default for NPad {
    fn default() -> Self {
        Self {
            npad_resource: NPadResource::new(),
            vibration: NpadVibration::new(),
            ref_counter: 0,
            applet_resource_holder: AppletResourceHolder::new(),
        }
    }
}

impl NPad {
    pub fn new() -> Self {
        Self::default()
    }

    /// Port of NPad::Activate().
    pub fn activate(&mut self) -> ResultCode {
        if self.ref_counter == i32::MAX - 1 {
            return hid_result::RESULT_NPAD_RESOURCE_OVERFLOW;
        }

        if self.ref_counter == 0 {
            // Upstream TODO: Activate handlers and AbstractedPad
        }

        self.ref_counter += 1;
        ResultCode::SUCCESS
    }

    /// Port of NPad::Activate(u64 aruid).
    pub fn activate_for_aruid(&mut self, _aruid: u64) -> ResultCode {
        ResultCode::SUCCESS
    }

    /// Port of NPad::ActivateNpadResource().
    pub fn activate_npad_resource(&mut self) -> ResultCode {
        self.npad_resource.activate()
    }

    /// Port of NPad::ActivateNpadResource(u64 aruid).
    pub fn activate_npad_resource_with_aruid(&mut self, aruid: u64) -> ResultCode {
        self.npad_resource.activate_with_aruid(aruid)
    }

    pub fn free_applet_resource_id(&mut self, _aruid: u64) {
        // Upstream cleans up applet resource data for the given aruid
    }

    /// Port of NPad::RegisterAppletResourceUserId.
    pub fn register_applet_resource_user_id(&mut self, aruid: u64) -> ResultCode {
        self.npad_resource.register_applet_resource_user_id(aruid)
    }

    /// Port of NPad::UnregisterAppletResourceUserId.
    pub fn unregister_applet_resource_user_id(&mut self, aruid: u64) {
        self.npad_resource.unregister_applet_resource_user_id(aruid);
    }

    /// Port of NPad::SetNpadExternals.
    pub fn set_npad_externals(&mut self, holder: AppletResourceHolder) {
        self.applet_resource_holder = holder;
    }

    /// Port of NPad::OnUpdate.
    /// Iterates all aruids and controller data, reads input from EmulatedController
    /// and writes to shared memory.
    pub fn on_update(&mut self) {
        if self.ref_counter == 0 {
            return;
        }

        // The full NPad::OnUpdate is complex — it iterates AruidIndexMax entries,
        // checks each aruid_data, then iterates 10 controllers per aruid,
        // calling RequestPadStateUpdate and writing to shared memory lifos.
        // This requires full integration with EmulatedController callbacks
        // and shared memory NpadInternalState pointers.
        //
        // The core loop is:
        //   for aruid_index in 0..AruidIndexMax:
        //     data = applet_resource->GetAruidDataByIndex(aruid_index)
        //     if !data.flag.is_assigned: continue
        //     if !is_supported_npad_style_set: continue
        //     for i in 0..10:
        //       controller = controller_data[aruid_index][i]
        //       shared_memory = data.shared_memory_format.npad.npad_entry[i].internal_state
        //       if controller is not active/connected: continue
        //       RequestPadStateUpdate(aruid, npad_id)
        //       write pad_state into correct lifo based on controller_type
    }

    /// Get the vibration handler session aruid.
    pub fn get_vibration_handler_session_aruid(&self) -> u64 {
        self.vibration.get_session_aruid()
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

    /// Port of NPad::AssigningSingleOnSlSrPress.
    pub fn assigning_single_on_sl_sr_press(&mut self, aruid: u64, is_enabled: bool) -> ResultCode {
        let is_currently_enabled = match self.npad_resource.is_assigning_single_on_sl_sr_press_enabled(aruid) {
            Ok(v) => v,
            Err(e) => return ResultCode(e.raw()),
        };
        if is_enabled != is_currently_enabled {
            let result = self.npad_resource.set_assigning_single_on_sl_sr_press(aruid, is_enabled);
            return result;
        }
        ResultCode::SUCCESS
    }

    /// Port of NPad::GetLastActiveNpad.
    /// Upstream delegates to hid_core.GetLastActiveController().
    /// Since we don't have a direct hid_core reference here, return Player1 as default.
    pub fn get_last_active_npad(&self) -> (ResultCode, NpadIdType) {
        // TODO: forward to hid_core.get_last_active_controller() when wired
        (ResultCode::SUCCESS, NpadIdType::Player1)
    }

    /// Port of NPad::GetMaskedSupportedNpadStyleSet.
    pub fn get_masked_supported_npad_style_set(&self, aruid: u64) -> (ResultCode, NpadStyleSet) {
        match self.npad_resource.get_masked_supported_npad_style_set(aruid) {
            Ok(style_set) => (ResultCode::SUCCESS, style_set),
            Err(e) => {
                if e == hid_result::RESULT_UNDEFINED_STYLESET {
                    (ResultCode::SUCCESS, NpadStyleSet::NONE)
                } else {
                    (ResultCode(e.raw()), NpadStyleSet::NONE)
                }
            }
        }
    }

    /// Port of NPad::SetNpadSystemExtStateEnabled.
    pub fn set_npad_system_ext_state_enabled(&mut self, aruid: u64, is_enabled: bool) -> ResultCode {
        let result = self.npad_resource.set_npad_system_ext_state_enabled(aruid, is_enabled);
        if result.is_success() {
            // Upstream: TODO: abstracted_pad->EnableAppletToGetInput(aruid)
        }
        result
    }

    /// Port of NPad::EnableAppletToGetInput.
    pub fn enable_applet_to_get_input(&mut self, _aruid: u64) {
        // Upstream iterates abstracted_pads and calls EnableAppletToGetInput(aruid).
        // TODO: implement when abstracted_pads are wired up
    }

    pub fn npad_resource(&self) -> &NPadResource {
        &self.npad_resource
    }

    pub fn npad_resource_mut(&mut self) -> &mut NPadResource {
        &mut self.npad_resource
    }
}
