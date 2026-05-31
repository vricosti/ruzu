// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/resources/npad/npad.h and npad.cpp
//!
//! Main NPad controller resource managing all npad-related state including
//! style sets, vibration, six-axis sensors, and abstracted pad management.

use common::ResultCode;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::frontend::emulated_controller::get_simple_npad_button_state;
use crate::hid_result;
use crate::hid_types::*;
use crate::resources::applet_resource::{AppletResourceHolder, ARUID_INDEX_MAX};
use crate::resources::npad::npad_resource::NPadResource;
use crate::resources::npad::npad_types::*;
use crate::resources::npad::npad_vibration::NpadVibration;
use crate::resources::vibration::vibration_device::NpadVibrationDevice;

static NPAD_UPDATE_TRACE_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Main NPad controller resource
pub struct NPad {
    npad_resource: NPadResource,
    vibration: NpadVibration,
    vibration_devices: [NpadVibrationDevice; 2],
    ref_counter: i32,
    applet_resource_holder: AppletResourceHolder,
}

impl Default for NPad {
    fn default() -> Self {
        Self {
            npad_resource: NPadResource::new(),
            vibration: NpadVibration::new(),
            vibration_devices: {
                let mut left = NpadVibrationDevice::new();
                left.mount_virtual(DeviceIndex::Left);
                let mut right = NpadVibrationDevice::new();
                right.mount_virtual(DeviceIndex::Right);
                [left, right]
            },
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

    pub fn is_active(&self) -> bool {
        self.ref_counter != 0
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
    ///
    /// Upstream iterates `controller_data[aruid_index][i]` (a 2D array of 10
    /// EmulatedController-backed entries per aruid), reads input from the
    /// device, and writes `NPadGenericState` entries into the shared-memory
    /// LIFOs (fullkey_lifo, handheld_lifo, joy_*_lifo, etc.) based on each
    /// controller's `NpadStyleIndex`.
    ///
    /// ruzu's port currently lacks the full controller_data array (see TODO
    /// note below) and the `EmulatedController` integration is largely
    /// stubbed (no real input devices wired up yet). To still let the guest
    /// game see "a controller is attached", this implementation writes a
    /// fixed Pro Controller (Fullkey) `NPadGenericState` with
    /// `connection_status = is_connected | is_wired` to `npad_entry[0]`'s
    /// `fullkey_lifo` for every assigned aruid.
    ///
    /// Once `EmulatedController::reload_from_settings` honors the
    /// `Settings::values.players[i]` config and `controller_data` is fully
    /// ported, this should be replaced by the full upstream loop (see
    /// `zuyu/src/hid_core/resources/npad/npad.cpp:461`).
    pub fn on_update(&mut self) {
        let trace_update = std::env::var_os("RUZU_TRACE_NPAD_UPDATE").is_some();
        let trace_index = if trace_update {
            NPAD_UPDATE_TRACE_COUNTER.fetch_add(1, Ordering::Relaxed)
        } else {
            0
        };
        if self.ref_counter == 0 {
            if trace_update && trace_index % 1000 == 0 {
                log::info!("[NPAD_UPDATE] skip ref_counter=0");
            }
            return;
        }

        let Some(applet_resource) = self.applet_resource_holder.applet_resource.clone() else {
            if trace_update && trace_index % 1000 == 0 {
                log::info!("[NPAD_UPDATE] skip no_applet_resource");
            }
            return;
        };
        let mut applet_resource = applet_resource.lock();

        for aruid_index in 0..ARUID_INDEX_MAX {
            // Mirror upstream's "skip if data is null or not assigned" gate.
            let (assigned, aruid, enable_input) = {
                let data = applet_resource.get_aruid_data_by_index(aruid_index);
                (
                    data.flag.is_assigned(),
                    data.aruid,
                    data.flag.enable_pad_input(),
                )
            };
            if !assigned {
                if trace_update && trace_index % 1000 == 0 && aruid != 0 {
                    log::info!(
                        "[NPAD_UPDATE] skip aruid=0x{:X} assigned=false enable_input={}",
                        aruid,
                        enable_input
                    );
                }
                continue;
            }

            // Mirror upstream's IsSupportedNpadStyleSet gate.
            let is_set = self
                .npad_resource
                .is_supported_npad_style_set(aruid)
                .unwrap_or(false);
            if !is_set {
                if trace_update && trace_index % 1000 == 0 {
                    log::info!(
                        "[NPAD_UPDATE] skip aruid=0x{:X} is_supported_style_set=false enable_input={}",
                        aruid,
                        enable_input
                    );
                }
                continue;
            }

            // Mirror upstream's enable_pad_input gate.
            if !enable_input {
                if trace_update && trace_index % 1000 == 0 {
                    log::info!("[NPAD_UPDATE] skip aruid=0x{:X} enable_input=false", aruid);
                }
                continue;
            }
            if trace_update && trace_index % 1000 == 0 {
                log::info!(
                    "[NPAD_UPDATE] active aruid=0x{:X} enable_input={} buttons=0x{:X}",
                    aruid,
                    enable_input,
                    get_simple_npad_button_state().raw.bits()
                );
            }

            // Get the npad shared-memory area for this aruid.
            let Some(shared) = applet_resource.get_shared_memory_format_by_index_mut(aruid_index)
            else {
                continue;
            };

            // ruzu's temporary controller model currently populates only
            // Player1. The full upstream loop over controller_data[aruid][i]
            // should replace this once per-NpadId controller state is ported.
            for entry_index in 0..1 {
                let npad = &mut shared.npad.npad_entry[entry_index].internal_state;

                // Mirror upstream `NPad::InitNewlyAddedController` for the
                // Pro Controller (Fullkey) case (npad.cpp:202-215). These fields
                // are normally written ONCE on a connect event; ruzu has no
                // EmulatedController connect-event plumbing yet, so we keep
                // re-writing them here. The values are idempotent.
                npad.style_tag.raw |= NpadStyleSet::FULLKEY;
                // device_type.fullkey = bit 0 (upstream `BitField<0,1,s32> fullkey`).
                npad.device_type.raw |= 1 << 0;
                // system_properties bits 11 (is_vertical), 13 (use_plus), 14 (use_minus).
                npad.system_properties.raw |= (1 << 11) | (1 << 13) | (1 << 14);
                npad.fullkey_color.attribute = ColorAttribute::Ok;
                npad.applet_footer_type = AppletFooterUiType::SwitchProController;
                npad.sixaxis_fullkey_properties.set_is_newly_assigned(true);

                // Build a Pro Controller / Fullkey `NPadGenericState`:
                //   connection_status.raw bits: is_connected (0) | is_wired (1) = 0x3.
                // Sampling number monotonically increases (upstream:
                //   `npad->fullkey_lifo.ReadCurrentEntry().state.sampling_number + 1`).
                let prev_sampling = npad.fullkey_lifo.read_current_entry().state.sampling_number;
                let mut pad_state = NPadGenericState::default();
                pad_state.connection_status.raw = 0x3;
                pad_state.npad_buttons = get_simple_npad_button_state();
                pad_state.sampling_number = prev_sampling + 1;
                if std::env::var_os("RUZU_TRACE_NPAD_STATE").is_some()
                    && !pad_state.npad_buttons.raw.is_empty()
                {
                    log::info!(
                        "[NPAD_STATE] aruid=0x{:X} entry={} buttons=0x{:X} sampling={}",
                        aruid,
                        entry_index,
                        pad_state.npad_buttons.raw.bits(),
                        pad_state.sampling_number
                    );
                }
                npad.fullkey_lifo.write_next_entry(pad_state);

                // Mirror the libnx-state write: upstream also updates
                // `system_ext_lifo` with the `NpadCommonState` so libnx clients
                // (which don't activate any specific style) still see a connected
                // controller via that LIFO.
                let prev_ext = npad
                    .system_ext_lifo
                    .read_current_entry()
                    .state
                    .sampling_number;
                let mut libnx_state = NPadGenericState::default();
                libnx_state.connection_status.raw = 0x3;
                libnx_state.npad_buttons = pad_state.npad_buttons;
                libnx_state.sampling_number = prev_ext + 1;
                npad.system_ext_lifo.write_next_entry(libnx_state);
            }
        }
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
        self.npad_resource
            .set_supported_npad_style_set(aruid, supported_style_set)
    }

    pub fn get_supported_npad_style_set(&self, aruid: u64) -> Result<NpadStyleSet, ResultCode> {
        self.npad_resource.get_supported_npad_style_set(aruid)
    }

    pub fn set_supported_npad_id_type(
        &mut self,
        aruid: u64,
        supported_npad_list: &[NpadIdType],
    ) -> ResultCode {
        self.npad_resource
            .set_supported_npad_id_type(aruid, supported_npad_list)
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
        self.npad_resource
            .set_npad_handheld_activation_mode(aruid, mode)
    }

    pub fn get_npad_handheld_activation_mode(
        &self,
        aruid: u64,
    ) -> Result<NpadHandheldActivationMode, ResultCode> {
        self.npad_resource.get_npad_handheld_activation_mode(aruid)
    }

    pub fn set_npad_communication_mode(
        &mut self,
        _communication_mode: NpadCommunicationMode,
    ) -> ResultCode {
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
        self.npad_resource
            .apply_npad_system_common_policy(aruid, false)
    }

    pub fn apply_npad_system_common_policy_full(&mut self, aruid: u64) -> ResultCode {
        self.npad_resource
            .apply_npad_system_common_policy(aruid, true)
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

    /// Port of `NPad::GetVibrationDevice` for standard LRA devices.
    ///
    /// ruzu does not yet model `AbstractPad`, so the default Player1 fullkey
    /// controller owns two virtual mounted LRA devices. This mirrors the
    /// controller state faked in `on_update`.
    pub fn get_vibration_device_mut(
        &mut self,
        handle: &VibrationDeviceHandle,
    ) -> Option<&mut NpadVibrationDevice> {
        if crate::hid_util::is_vibration_handle_valid(handle).is_error() {
            return None;
        }
        match handle.npad_type {
            NpadStyleIndex::Fullkey
            | NpadStyleIndex::Handheld
            | NpadStyleIndex::JoyconDual
            | NpadStyleIndex::JoyconLeft
            | NpadStyleIndex::JoyconRight => match handle.device_index {
                DeviceIndex::Left => Some(&mut self.vibration_devices[0]),
                DeviceIndex::Right => Some(&mut self.vibration_devices[1]),
                _ => None,
            },
            _ => None,
        }
    }

    /// Port of NPad::AssigningSingleOnSlSrPress.
    pub fn assigning_single_on_sl_sr_press(&mut self, aruid: u64, is_enabled: bool) -> ResultCode {
        let is_currently_enabled = match self
            .npad_resource
            .is_assigning_single_on_sl_sr_press_enabled(aruid)
        {
            Ok(v) => v,
            Err(e) => return ResultCode(e.raw()),
        };
        if is_enabled != is_currently_enabled {
            let result = self
                .npad_resource
                .set_assigning_single_on_sl_sr_press(aruid, is_enabled);
            return result;
        }
        ResultCode::SUCCESS
    }

    /// Port of NPad::GetLastActiveNpad.
    /// Upstream delegates to hid_core.GetLastActiveController().
    /// NPad needs a reference to HidCore (which has get_last_active_controller())
    /// but that wiring is not yet in place. Returns Player1 as a safe default until
    /// NPad receives an HidCore reference matching upstream's constructor signature.
    pub fn get_last_active_npad(&self) -> (ResultCode, NpadIdType) {
        (ResultCode::SUCCESS, NpadIdType::Player1)
    }

    /// Port of NPad::GetMaskedSupportedNpadStyleSet.
    pub fn get_masked_supported_npad_style_set(&self, aruid: u64) -> (ResultCode, NpadStyleSet) {
        match self
            .npad_resource
            .get_masked_supported_npad_style_set(aruid)
        {
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
    /// Upstream additionally iterates abstracted_pads and calls
    /// abstracted_pad->EnableAppletToGetInput(aruid) on success.
    /// AbstractedPad integration is not yet wired up.
    pub fn set_npad_system_ext_state_enabled(
        &mut self,
        aruid: u64,
        is_enabled: bool,
    ) -> ResultCode {
        let result = self
            .npad_resource
            .set_npad_system_ext_state_enabled(aruid, is_enabled);
        if result.is_success() {
            // Upstream: for (auto& abstract_pad : abstracted_pads) {
            //     abstract_pad->EnableAppletToGetInput(aruid);
            // }
            // Requires AbstractedPad array to be stored in NPad.
        }
        result
    }

    /// Port of NPad::EnableAppletToGetInput.
    /// Upstream iterates abstracted_pads and calls EnableAppletToGetInput(aruid).
    /// Requires AbstractedPad array to be stored in NPad, which is not yet wired up.
    pub fn enable_applet_to_get_input(&mut self, _aruid: u64) {}

    pub fn npad_resource(&self) -> &NPadResource {
        &self.npad_resource
    }

    pub fn npad_resource_mut(&mut self) -> &mut NPadResource {
        &mut self.npad_resource
    }
}
