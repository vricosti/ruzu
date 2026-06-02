// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/resources/npad/npad.h and npad.cpp
//!
//! Main NPad controller resource managing all npad-related state including
//! style sets, vibration, six-axis sensors, and abstracted pad management.

use common::ResultCode;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::frontend::emulated_controller::get_simple_npad_button_state;
use crate::hid_util;
use crate::hid_result;
use crate::hid_types::*;
use crate::resources::applet_resource::{AppletResourceHolder, ARUID_INDEX_MAX};
use crate::resources::npad::npad_resource::NPadResource;
use crate::resources::npad::npad_types::*;
use crate::resources::npad::npad_vibration::NpadVibration;
use crate::resources::shared_memory_format::NpadInternalState;
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
    pub fn activate_for_aruid(&mut self, aruid: u64) -> ResultCode {
        let Some(applet_resource) = self.applet_resource_holder.applet_resource.clone() else {
            return ResultCode::SUCCESS;
        };
        let mut applet_resource = applet_resource.lock();

        let aruid_index = applet_resource.get_index_from_aruid(aruid);
        if aruid_index >= ARUID_INDEX_MAX {
            return ResultCode::SUCCESS;
        }

        {
            let data = applet_resource.get_aruid_data_by_index(aruid_index);
            if !data.flag.is_assigned() {
                return ResultCode::SUCCESS;
            }
        }

        let Some(shared) = applet_resource.get_shared_memory_format_by_index_mut(aruid_index)
        else {
            return ResultCode::SUCCESS;
        };

        for entry in &mut shared.npad.npad_entry {
            let npad = &mut entry.internal_state;
            npad.fullkey_color.attribute = ColorAttribute::NoController;
            npad.joycon_color.attribute = ColorAttribute::NoController;

            // HW seems to initialize the first 19 entries.
            for _ in 0..19 {
                Self::write_empty_entry(npad);
            }
        }

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
        let result = self
            .npad_resource
            .set_supported_npad_style_set(aruid, supported_style_set);
        if result.is_success() {
            self.on_update();
        }
        result
    }

    pub fn get_supported_npad_style_set(&self, aruid: u64) -> Result<NpadStyleSet, ResultCode> {
        self.npad_resource.get_supported_npad_style_set(aruid)
    }

    pub fn set_supported_npad_id_type(
        &mut self,
        aruid: u64,
        supported_npad_list: &[NpadIdType],
    ) -> ResultCode {
        let result = self
            .npad_resource
            .set_supported_npad_id_type(aruid, supported_npad_list);
        if result.is_success() {
            self.on_update();
        }
        result
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
        let result = self
            .npad_resource
            .set_npad_handheld_activation_mode(aruid, mode);
        if result.is_success() {
            self.on_update();
        }
        result
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
        aruid: u64,
        npad_id: NpadIdType,
    ) -> ResultCode {
        self.set_npad_mode(
            aruid,
            npad_id,
            NpadJoyDeviceType::Left,
            NpadJoyAssignmentMode::Single,
        );
        ResultCode::SUCCESS
    }

    pub fn set_npad_joy_assignment_mode_single(
        &mut self,
        aruid: u64,
        npad_id: NpadIdType,
        npad_device_type: NpadJoyDeviceType,
    ) -> ResultCode {
        self.set_npad_mode(
            aruid,
            npad_id,
            npad_device_type,
            NpadJoyAssignmentMode::Single,
        );
        ResultCode::SUCCESS
    }

    pub fn set_npad_joy_assignment_mode_dual(
        &mut self,
        aruid: u64,
        npad_id: NpadIdType,
    ) -> ResultCode {
        self.set_npad_mode(
            aruid,
            npad_id,
            NpadJoyDeviceType::Left,
            NpadJoyAssignmentMode::Dual,
        );
        ResultCode::SUCCESS
    }

    /// Partial port of upstream `NPad::SetNpadMode`.
    ///
    /// Upstream first validates the npad id and writes
    /// `shared_memory->assignment_mode` before any JoyCon split/merge logic.
    /// ruzu does not yet have upstream's `controller_data[aruid][npad]`
    /// device model, so it cannot faithfully decide connected JoyConDual
    /// split/reassignment state here. Keep the guest-visible assignment-mode
    /// write, then return "not reassigned" just like upstream does for
    /// non-JoyConDual controllers.
    pub fn set_npad_mode(
        &mut self,
        aruid: u64,
        npad_id: NpadIdType,
        _npad_device_type: NpadJoyDeviceType,
        assignment_mode: NpadJoyAssignmentMode,
    ) -> (bool, NpadIdType) {
        if !hid_util::is_npad_id_valid(npad_id) {
            log::error!("Invalid NpadIdType npad_id:{:?}", npad_id);
            return (false, NpadIdType::default());
        }

        let Some(applet_resource) = self.applet_resource_holder.applet_resource.clone() else {
            return (false, NpadIdType::default());
        };
        let mut applet_resource = applet_resource.lock();
        let aruid_index = applet_resource.get_index_from_aruid(aruid);
        if aruid_index >= ARUID_INDEX_MAX {
            return (false, NpadIdType::default());
        }

        let Some(shared) = applet_resource.get_shared_memory_format_by_index_mut(aruid_index)
        else {
            return (false, NpadIdType::default());
        };
        let npad_index = hid_util::npad_id_type_to_index(npad_id);
        let controller = &mut shared.npad.npad_entry[npad_index].internal_state;
        if controller.assignment_mode != assignment_mode {
            controller.assignment_mode = assignment_mode;
        }

        (false, NpadIdType::default())
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

    /// Port of NPad::GetAppletDetailedUiType.
    pub fn get_applet_detailed_ui_type(&self, npad_id: NpadIdType) -> AppletDetailedUiType {
        let Some(applet_resource) = self.applet_resource_holder.applet_resource.clone() else {
            return AppletDetailedUiType::default();
        };
        let applet_resource = applet_resource.lock();
        let aruid = applet_resource.get_active_aruid();
        let Some(shared) = applet_resource.get_shared_memory_format(aruid) else {
            return AppletDetailedUiType::default();
        };

        let npad_index = hid_util::npad_id_type_to_index(npad_id);
        let shared_memory = &shared.npad.npad_entry[npad_index].internal_state;
        AppletDetailedUiType {
            ui_variant: 0,
            _padding: [0; 2],
            footer: shared_memory.applet_footer_type,
        }
    }

    /// Port of NPad::WriteEmptyEntry.
    fn write_empty_entry(npad: &mut NpadInternalState) {
        let mut dummy_pad_state = NPadGenericState::default();
        let mut dummy_gc_state = NpadGcTriggerState::default();

        dummy_pad_state.sampling_number =
            npad.fullkey_lifo.read_current_entry().sampling_number + 1;
        npad.fullkey_lifo.write_next_entry(dummy_pad_state);

        dummy_pad_state.sampling_number =
            npad.handheld_lifo.read_current_entry().sampling_number + 1;
        npad.handheld_lifo.write_next_entry(dummy_pad_state);

        dummy_pad_state.sampling_number =
            npad.joy_dual_lifo.read_current_entry().sampling_number + 1;
        npad.joy_dual_lifo.write_next_entry(dummy_pad_state);

        dummy_pad_state.sampling_number =
            npad.joy_left_lifo.read_current_entry().sampling_number + 1;
        npad.joy_left_lifo.write_next_entry(dummy_pad_state);

        dummy_pad_state.sampling_number =
            npad.joy_right_lifo.read_current_entry().sampling_number + 1;
        npad.joy_right_lifo.write_next_entry(dummy_pad_state);

        dummy_pad_state.sampling_number =
            npad.palma_lifo.read_current_entry().sampling_number + 1;
        npad.palma_lifo.write_next_entry(dummy_pad_state);

        dummy_pad_state.sampling_number =
            npad.system_ext_lifo.read_current_entry().sampling_number + 1;
        npad.system_ext_lifo.write_next_entry(dummy_pad_state);

        dummy_gc_state.sampling_number =
            npad.gc_trigger_lifo.read_current_entry().sampling_number + 1;
        npad.gc_trigger_lifo.write_next_entry(dummy_gc_state);
    }

    pub fn npad_resource(&self) -> &NPadResource {
        &self.npad_resource
    }

    pub fn npad_resource_mut(&mut self) -> &mut NPadResource {
        &mut self.npad_resource
    }
}

#[cfg(test)]
mod tests {
    use std::any::Any;
    use std::sync::Arc;

    use parking_lot::Mutex;

    use super::NPad;
    use crate::hid_types::NpadIdType;
    use crate::resources::applet_resource::{AppletResource, AppletResourceHolder};
    use crate::resources::npad::npad_types::{
        NpadJoyAssignmentMode, NpadJoyDeviceType,
    };
    use crate::resources::shared_memory_holder::KSharedMemoryBacking;

    struct TestSharedMemoryBacking;

    impl KSharedMemoryBacking for TestSharedMemoryBacking {
        fn create(&self, size: usize) -> Option<(*mut u8, Arc<dyn Any + Send + Sync>)> {
            let mut bytes = vec![0u8; size].into_boxed_slice();
            let ptr = bytes.as_mut_ptr();
            let keepalive: Arc<dyn Any + Send + Sync> = Arc::new(bytes);
            Some((ptr, keepalive))
        }
    }

    #[test]
    fn set_npad_mode_updates_shared_assignment_mode() {
        const ARUID: u64 = 0x51;

        let mut applet_resource = AppletResource::new();
        applet_resource.set_shared_memory_backing(Arc::new(TestSharedMemoryBacking));
        assert!(applet_resource
            .register_applet_resource_user_id(ARUID, true)
            .is_success());
        assert!(applet_resource.create_applet_resource(ARUID).is_success());

        let applet_resource = Arc::new(Mutex::new(applet_resource));
        let mut npad = NPad::new();
        npad.set_npad_externals(AppletResourceHolder {
            applet_resource: Some(applet_resource.clone()),
            handheld_config: None,
        });

        let (is_reassigned, new_npad_id) = npad.set_npad_mode(
            ARUID,
            NpadIdType::Player1,
            NpadJoyDeviceType::Left,
            NpadJoyAssignmentMode::Single,
        );
        assert!(!is_reassigned);
        assert_eq!(new_npad_id, NpadIdType::default());

        let resource = applet_resource.lock();
        let shared = resource.get_shared_memory_format(ARUID).unwrap();
        assert_eq!(
            shared.npad.npad_entry[0].internal_state.assignment_mode,
            NpadJoyAssignmentMode::Single
        );
    }

    #[test]
    fn activate_for_aruid_prefills_npad_lifos_like_upstream() {
        const ARUID: u64 = 0x51;

        let mut applet_resource = AppletResource::new();
        applet_resource.set_shared_memory_backing(Arc::new(TestSharedMemoryBacking));
        assert!(applet_resource
            .register_applet_resource_user_id(ARUID, true)
            .is_success());
        assert!(applet_resource.create_applet_resource(ARUID).is_success());

        let applet_resource = Arc::new(Mutex::new(applet_resource));
        let mut npad = NPad::new();
        npad.set_npad_externals(AppletResourceHolder {
            applet_resource: Some(applet_resource.clone()),
            handheld_config: None,
        });

        assert!(npad.activate_for_aruid(ARUID).is_success());

        let resource = applet_resource.lock();
        let shared = resource.get_shared_memory_format(ARUID).unwrap();
        let state = &shared.npad.npad_entry[0].internal_state;
        assert_eq!(state.fullkey_lifo.buffer_count, 16);
        assert_eq!(state.fullkey_lifo.buffer_tail, 2);
        assert_eq!(
            state.fullkey_lifo.read_current_entry().state.sampling_number,
            19
        );
        assert_eq!(
            state.system_ext_lifo.read_current_entry().state.sampling_number,
            19
        );
        assert_eq!(
            state.gc_trigger_lifo.read_current_entry().state.sampling_number,
            19
        );
    }
}
