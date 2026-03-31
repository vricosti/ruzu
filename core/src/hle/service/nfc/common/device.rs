// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nfc/common/device.h
//! Port of zuyu/src/core/hle/service/nfc/common/device.cpp
//!
//! NfcDevice: represents a single NFC device with tag detection, reading, writing.
//!
//! Note: EmulatedController (HID) integration is not yet available in Rust, so
//! NFC tag detection won't connect to real HID. The state machine and data structures
//! are implemented correctly; HID interactions are stubbed where necessary.

use std::sync::Arc;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::kernel_helpers::ServiceContext;
use crate::hle::service::nfc::nfc_result;
use crate::hle::service::nfc::nfc_types::*;
use crate::hle::service::nfp::nfp_result;
use crate::hle::service::nfp::nfp_types;
use crate::hle::service::os::event::Event;

/// NfcDevice corresponds to `NfcDevice` in upstream `device.h`.
///
/// Manages the NFC device state machine for a single controller. Upstream fields
/// that depend on EmulatedController or amiibo crypto (tag_data, encrypted_tag_data,
/// npad_device, callback_key, is_controller_set) are omitted since HID integration
/// is not yet available.
pub struct NfcDevice {
    npad_id: u64,
    device_state: DeviceState,
    is_initialized: bool,
    allowed_protocols: NfcProtocol,
    mount_target: nfp_types::MountTarget,
    is_data_modified: bool,
    is_app_area_open: bool,
    is_plain_amiibo: bool,
    is_write_protected: bool,
    activate_event: Arc<Event>,
    deactivate_event: Arc<Event>,
    tag_info: TagInfo,
}

impl NfcDevice {
    /// Creates a new NfcDevice.
    ///
    /// Upstream constructor: creates activate/deactivate events via ServiceContext,
    /// obtains EmulatedController from HIDCore, and registers an NpadUpdate callback.
    /// HID controller registration is not available yet; the device starts in
    /// Unavailable state matching upstream behavior when no controller is connected.
    pub fn new(npad_id: u64, service_context: &mut ServiceContext) -> Self {
        let activate_handle = service_context.create_event("NFC:ActivateEvent".to_string());
        let deactivate_handle = service_context.create_event("NFC:DeactivateEvent".to_string());

        let activate_event = service_context
            .get_event(activate_handle)
            .expect("just created activate event");
        let deactivate_event = service_context
            .get_event(deactivate_handle)
            .expect("just created deactivate event");

        Self {
            npad_id,
            device_state: DeviceState::Unavailable,
            is_initialized: false,
            allowed_protocols: NfcProtocol::empty(),
            mount_target: nfp_types::MountTarget::None,
            is_data_modified: false,
            is_app_area_open: false,
            is_plain_amiibo: false,
            is_write_protected: false,
            activate_event,
            deactivate_event,
            tag_info: TagInfo::default(),
        }
    }

    /// Initialize the NFC device.
    ///
    /// Upstream: sets device_state based on whether the controller has NFC capability,
    /// clears tag data, and calls AddNfcHandle. Without HID integration, we transition
    /// directly to Initialized since there is no controller to query.
    pub fn initialize(&mut self) -> ResultCode {
        // Without HID, assume NFC is available and transition to Initialized.
        // Upstream: device_state = npad_device->HasNfc() ? Initialized : Unavailable
        self.device_state = DeviceState::Initialized;
        self.tag_info = TagInfo::default();
        self.is_initialized = true;
        RESULT_SUCCESS
    }

    /// Finalize the NFC device.
    ///
    /// Upstream: unmounts if mounted, stops detection if searching, removes NFC handle,
    /// transitions to Unavailable.
    pub fn finalize(&mut self) -> ResultCode {
        if self.device_state == DeviceState::TagMounted {
            let _ = self.unmount();
        }
        if self.device_state == DeviceState::SearchingForTag
            || self.device_state == DeviceState::TagRemoved
        {
            let _ = self.stop_detection();
        }

        self.device_state = DeviceState::Unavailable;
        self.is_initialized = false;
        RESULT_SUCCESS
    }

    /// Start scanning for NFC tags.
    ///
    /// Upstream: requires Initialized or TagRemoved state, calls npad_device->StartNfcPolling(),
    /// transitions to SearchingForTag. Without HID, we skip the polling call.
    pub fn start_detection(&mut self, protocol: NfcProtocol) -> ResultCode {
        if self.device_state != DeviceState::Initialized
            && self.device_state != DeviceState::TagRemoved
        {
            log::error!(
                "Wrong device state {:?} for start_detection",
                self.device_state
            );
            return nfc_result::RESULT_WRONG_DEVICE_STATE;
        }

        // Upstream: if !npad_device->StartNfcPolling() return ResultNfcDisabled
        // Without HID, assume polling succeeds.

        self.device_state = DeviceState::SearchingForTag;
        self.allowed_protocols = protocol;
        RESULT_SUCCESS
    }

    /// Stop scanning for NFC tags.
    ///
    /// Upstream: if already Initialized returns success, if TagFound/TagMounted closes tag,
    /// if SearchingForTag/TagRemoved stops polling and returns to Initialized.
    pub fn stop_detection(&mut self) -> ResultCode {
        if self.device_state == DeviceState::Initialized {
            return RESULT_SUCCESS;
        }

        if self.device_state == DeviceState::TagFound
            || self.device_state == DeviceState::TagMounted
        {
            self.close_nfc_tag();
        }

        if self.device_state == DeviceState::SearchingForTag
            || self.device_state == DeviceState::TagRemoved
        {
            // Upstream: npad_device->StopNfcPolling()
            self.device_state = DeviceState::Initialized;
            return RESULT_SUCCESS;
        }

        log::error!(
            "Wrong device state {:?} for stop_detection",
            self.device_state
        );
        nfc_result::RESULT_WRONG_DEVICE_STATE
    }

    /// Get the current device state.
    pub fn get_current_state(&self) -> DeviceState {
        self.device_state
    }

    /// Get the device handle (npad_id cast to u64, matching upstream GetHandle).
    pub fn get_handle(&self) -> u64 {
        self.npad_id
    }

    /// Get the npad ID.
    pub fn get_npad_id(&self) -> u64 {
        self.npad_id
    }

    /// Get tag info for the currently detected tag.
    ///
    /// Upstream: requires TagFound or TagMounted state, copies real_tag_info,
    /// optionally randomizes UUID for Type2 tags.
    pub fn get_tag_info(&self) -> Result<TagInfo, ResultCode> {
        if self.device_state != DeviceState::TagFound
            && self.device_state != DeviceState::TagMounted
        {
            log::error!(
                "Wrong device state {:?} for get_tag_info",
                self.device_state
            );
            if self.device_state == DeviceState::TagRemoved {
                return Err(nfc_result::RESULT_TAG_REMOVED);
            }
            return Err(nfc_result::RESULT_WRONG_DEVICE_STATE);
        }

        Ok(self.tag_info)
    }

    /// Get the activate event reference.
    pub fn get_activate_event(&self) -> &Arc<Event> {
        &self.activate_event
    }

    /// Get the deactivate event reference.
    pub fn get_deactivate_event(&self) -> &Arc<Event> {
        &self.deactivate_event
    }

    /// Mount an amiibo tag for reading/writing.
    ///
    /// Upstream: validates ModelType::Amiibo, requires TagFound state, loads amiibo data,
    /// validates it, transitions to TagMounted. Without HID/amiibo crypto, we validate
    /// state and transition but cannot load actual amiibo data.
    pub fn mount(&mut self, model_type: u32, mount_target: u32) -> ResultCode {
        // Upstream: if model_type != ModelType::Amiibo return ResultInvalidArgument
        if model_type != nfp_types::ModelType::Amiibo as u32 {
            return nfp_result::RESULT_INVALID_ARGUMENT;
        }

        if self.device_state != DeviceState::TagFound {
            log::error!("Wrong device state {:?} for mount", self.device_state);
            return nfp_result::RESULT_WRONG_DEVICE_STATE;
        }

        // Upstream: calls LoadAmiiboData() and validates encrypted data.
        // Without HID, no amiibo data is available — return InvalidTagType to match
        // upstream behavior when LoadAmiiboData fails.
        // However, if a tag has been loaded through some other mechanism in the future,
        // we transition to TagMounted.

        // Convert mount_target u32 to MountTarget enum
        let target = match mount_target {
            0 => nfp_types::MountTarget::None,
            1 => nfp_types::MountTarget::Rom,
            2 => nfp_types::MountTarget::Ram,
            3 => nfp_types::MountTarget::All,
            _ => return nfp_result::RESULT_INVALID_ARGUMENT,
        };

        self.device_state = DeviceState::TagMounted;
        self.mount_target = target;

        RESULT_SUCCESS
    }

    /// Unmount a previously mounted amiibo tag.
    ///
    /// Upstream: requires TagMounted state, flushes if data was modified,
    /// transitions to TagFound.
    pub fn unmount(&mut self) -> ResultCode {
        if self.device_state != DeviceState::TagMounted {
            log::error!("Wrong device state {:?} for unmount", self.device_state);
            if self.device_state == DeviceState::TagRemoved {
                return nfp_result::RESULT_TAG_REMOVED;
            }
            return nfp_result::RESULT_WRONG_DEVICE_STATE;
        }

        // Upstream: if is_data_moddified { Flush(); }
        if self.is_data_modified {
            let _ = self.flush();
        }

        self.device_state = DeviceState::TagFound;
        self.mount_target = nfp_types::MountTarget::None;
        self.is_app_area_open = false;

        RESULT_SUCCESS
    }

    /// Flush modified amiibo data to the tag.
    ///
    /// Upstream: requires TagMounted state, writable mount target, updates write date
    /// and counter, calls FlushWithBreak. Without amiibo data structures, we validate
    /// state and clear the modified flag.
    pub fn flush(&mut self) -> ResultCode {
        if self.device_state != DeviceState::TagMounted {
            log::error!("Wrong device state {:?} for flush", self.device_state);
            if self.device_state == DeviceState::TagRemoved {
                return nfp_result::RESULT_TAG_REMOVED;
            }
            return nfp_result::RESULT_WRONG_DEVICE_STATE;
        }

        if self.mount_target == nfp_types::MountTarget::None
            || self.mount_target == nfp_types::MountTarget::Rom
        {
            log::error!("Amiibo is read only");
            return nfp_result::RESULT_WRONG_DEVICE_STATE;
        }

        // Upstream: updates write_date, increments write_counter, calls FlushWithBreak.
        // Without tag_data structures, we just clear the modified flag.
        self.is_data_modified = false;

        RESULT_SUCCESS
    }

    /// Get the device state (alias for get_current_state matching upstream interface).
    pub fn get_device_state(&self) -> DeviceState {
        self.device_state
    }

    /// Get common info for the mounted amiibo.
    ///
    /// Upstream: requires TagMounted with writable mount target, reads from tag_data.
    /// Without tag_data, returns a default CommonInfo.
    pub fn get_common_info(&self) -> Result<nfp_types::CommonInfo, ResultCode> {
        if self.device_state != DeviceState::TagMounted {
            log::error!(
                "Wrong device state {:?} for get_common_info",
                self.device_state
            );
            if self.device_state == DeviceState::TagRemoved {
                return Err(nfp_result::RESULT_TAG_REMOVED);
            }
            return Err(nfp_result::RESULT_WRONG_DEVICE_STATE);
        }

        if self.mount_target == nfp_types::MountTarget::None
            || self.mount_target == nfp_types::MountTarget::Rom
        {
            log::error!("Amiibo is read only");
            return Err(nfp_result::RESULT_WRONG_DEVICE_STATE);
        }

        // Upstream fills from tag_data fields. Without tag_data, return default.
        Ok(nfp_types::CommonInfo::default())
    }

    /// Get model info for the mounted amiibo.
    ///
    /// Upstream: requires TagMounted state, reads from encrypted_tag_data.user_memory.model_info.
    /// Without amiibo data structures, validates state only.
    pub fn get_model_info(&self) -> ResultCode {
        if self.device_state != DeviceState::TagMounted {
            log::error!(
                "Wrong device state {:?} for get_model_info",
                self.device_state
            );
            if self.device_state == DeviceState::TagRemoved {
                return nfp_result::RESULT_TAG_REMOVED;
            }
            return nfp_result::RESULT_WRONG_DEVICE_STATE;
        }

        // Upstream: fills model_info from encrypted_tag_data. Without data, return success.
        RESULT_SUCCESS
    }

    /// Get register info for the mounted amiibo.
    ///
    /// Upstream: requires TagMounted with writable mount target, checks amiibo_initialized.
    /// Without amiibo data structures, validates state only.
    pub fn get_register_info(&self) -> ResultCode {
        if self.device_state != DeviceState::TagMounted {
            log::error!(
                "Wrong device state {:?} for get_register_info",
                self.device_state
            );
            if self.device_state == DeviceState::TagRemoved {
                return nfp_result::RESULT_TAG_REMOVED;
            }
            return nfp_result::RESULT_WRONG_DEVICE_STATE;
        }

        if self.mount_target == nfp_types::MountTarget::None
            || self.mount_target == nfp_types::MountTarget::Rom
        {
            log::error!("Amiibo is read only");
            return nfp_result::RESULT_WRONG_DEVICE_STATE;
        }

        // Upstream: checks amiibo_initialized, builds mii/register info.
        // Without tag_data, return success.
        RESULT_SUCCESS
    }

    /// Open the application area for read/write access.
    ///
    /// Upstream: requires TagMounted with writable mount target, checks appdata_initialized
    /// and matching access_id, then sets is_app_area_open.
    pub fn open_application_area(&mut self, _access_id: u32) -> ResultCode {
        if self.device_state != DeviceState::TagMounted {
            log::error!(
                "Wrong device state {:?} for open_application_area",
                self.device_state
            );
            if self.device_state == DeviceState::TagRemoved {
                return nfp_result::RESULT_TAG_REMOVED;
            }
            return nfp_result::RESULT_WRONG_DEVICE_STATE;
        }

        if self.mount_target == nfp_types::MountTarget::None
            || self.mount_target == nfp_types::MountTarget::Rom
        {
            log::error!("Amiibo is read only");
            return nfp_result::RESULT_WRONG_DEVICE_STATE;
        }

        // Upstream: checks tag_data.settings.settings.appdata_initialized and
        // tag_data.application_area_id == access_id. Without tag_data, the application
        // area is never initialized, so return the appropriate error.
        log::warn!("Application area is not initialized");
        nfp_result::RESULT_APPLICATION_AREA_IS_NOT_INITIALIZED
    }

    /// Get application area data.
    ///
    /// Upstream: requires TagMounted with writable mount target, app area open,
    /// appdata_initialized, copies from tag_data.application_area.
    pub fn get_application_area(&self, _data: &mut [u8]) -> Result<u32, ResultCode> {
        if self.device_state != DeviceState::TagMounted {
            log::error!(
                "Wrong device state {:?} for get_application_area",
                self.device_state
            );
            if self.device_state == DeviceState::TagRemoved {
                return Err(nfp_result::RESULT_TAG_REMOVED);
            }
            return Err(nfp_result::RESULT_WRONG_DEVICE_STATE);
        }

        if self.mount_target == nfp_types::MountTarget::None
            || self.mount_target == nfp_types::MountTarget::Rom
        {
            log::error!("Amiibo is read only");
            return Err(nfp_result::RESULT_WRONG_DEVICE_STATE);
        }

        if !self.is_app_area_open {
            log::error!("Application area is not open");
            return Err(nfp_result::RESULT_WRONG_DEVICE_STATE);
        }

        // Upstream: checks appdata_initialized, copies from tag_data.application_area.
        // Without tag_data, return not initialized.
        log::error!("Application area is not initialized");
        Err(nfp_result::RESULT_APPLICATION_AREA_IS_NOT_INITIALIZED)
    }

    /// Set application area data.
    ///
    /// Upstream: requires TagMounted with writable mount target, app area open,
    /// appdata_initialized, copies data into tag_data.application_area.
    pub fn set_application_area(&mut self, _data: &[u8]) -> ResultCode {
        if self.device_state != DeviceState::TagMounted {
            log::error!(
                "Wrong device state {:?} for set_application_area",
                self.device_state
            );
            if self.device_state == DeviceState::TagRemoved {
                return nfp_result::RESULT_TAG_REMOVED;
            }
            return nfp_result::RESULT_WRONG_DEVICE_STATE;
        }

        if self.mount_target == nfp_types::MountTarget::None
            || self.mount_target == nfp_types::MountTarget::Rom
        {
            log::error!("Amiibo is read only");
            return nfp_result::RESULT_WRONG_DEVICE_STATE;
        }

        if !self.is_app_area_open {
            log::error!("Application area is not open");
            return nfp_result::RESULT_WRONG_DEVICE_STATE;
        }

        // Upstream: checks appdata_initialized, copies data, fills remaining with random,
        // increments write counter, sets is_data_moddified. Without tag_data, return error.
        log::error!("Application area is not initialized");
        nfp_result::RESULT_APPLICATION_AREA_IS_NOT_INITIALIZED
    }

    /// Create a new application area.
    ///
    /// Upstream: requires TagMounted, checks appdata_initialized == 0 (must not already exist),
    /// delegates to RecreateApplicationArea.
    pub fn create_application_area(&mut self, _access_id: u32, _data: &[u8]) -> ResultCode {
        if self.device_state != DeviceState::TagMounted {
            log::error!(
                "Wrong device state {:?} for create_application_area",
                self.device_state
            );
            if self.device_state == DeviceState::TagRemoved {
                return nfp_result::RESULT_TAG_REMOVED;
            }
            return nfp_result::RESULT_WRONG_DEVICE_STATE;
        }

        // Upstream: checks tag_data.settings.settings.appdata_initialized != 0 -> error.
        // Without tag_data, appdata is never initialized, so this would proceed to
        // RecreateApplicationArea — but that also needs tag_data. Return success to
        // match the state machine flow (the actual data write is a no-op without tag_data).

        // Upstream would call RecreateApplicationArea here which requires writable mount,
        // valid data size, etc. Validate mount target at minimum.
        if self.is_app_area_open {
            log::error!("Application area is open");
            return nfp_result::RESULT_WRONG_DEVICE_STATE;
        }

        if self.mount_target == nfp_types::MountTarget::None
            || self.mount_target == nfp_types::MountTarget::Rom
        {
            log::error!("Amiibo is read only");
            return nfp_result::RESULT_WRONG_DEVICE_STATE;
        }

        // Without tag_data to write to, the operation is effectively a no-op.
        // Upstream would write data, set application_id, flush. We return success
        // to maintain correct state machine behavior.
        RESULT_SUCCESS
    }

    /// Delete the application area.
    ///
    /// Upstream: requires TagMounted with writable mount target, checks appdata_initialized,
    /// randomizes application area data, clears flags, flushes.
    pub fn delete_application_area(&mut self) -> ResultCode {
        if self.device_state != DeviceState::TagMounted {
            log::error!(
                "Wrong device state {:?} for delete_application_area",
                self.device_state
            );
            if self.device_state == DeviceState::TagRemoved {
                return nfp_result::RESULT_TAG_REMOVED;
            }
            return nfp_result::RESULT_WRONG_DEVICE_STATE;
        }

        if self.mount_target == nfp_types::MountTarget::None
            || self.mount_target == nfp_types::MountTarget::Rom
        {
            log::error!("Amiibo is read only");
            return nfp_result::RESULT_WRONG_DEVICE_STATE;
        }

        // Upstream: checks appdata_initialized == 0 -> ResultApplicationAreaIsNotInitialized
        // Without tag_data, application area is never initialized.
        // Return the appropriate error.
        nfp_result::RESULT_APPLICATION_AREA_IS_NOT_INITIALIZED
    }

    /// Check whether an application area exists.
    ///
    /// Upstream: requires TagMounted with writable mount target, checks
    /// tag_data.settings.settings.appdata_initialized.
    pub fn exists_application_area(&self) -> Result<bool, ResultCode> {
        if self.device_state != DeviceState::TagMounted {
            log::error!(
                "Wrong device state {:?} for exists_application_area",
                self.device_state
            );
            if self.device_state == DeviceState::TagRemoved {
                return Err(nfp_result::RESULT_TAG_REMOVED);
            }
            return Err(nfp_result::RESULT_WRONG_DEVICE_STATE);
        }

        if self.mount_target == nfp_types::MountTarget::None
            || self.mount_target == nfp_types::MountTarget::Rom
        {
            log::error!("Amiibo is read only");
            return Err(nfp_result::RESULT_WRONG_DEVICE_STATE);
        }

        // Upstream: has_application_area = tag_data.settings.settings.appdata_initialized != 0
        // Without tag_data, application area is never initialized.
        Ok(false)
    }

    /// Format the amiibo tag (delete all user data).
    ///
    /// Upstream: mounts if TagFound, deletes application area and register info, flushes.
    pub fn format(&mut self) -> ResultCode {
        if self.device_state == DeviceState::TagFound {
            let result = self.mount(
                nfp_types::ModelType::Amiibo as u32,
                nfp_types::MountTarget::All as u32,
            );
            // Upstream: allows CorruptedData and CorruptedDataWithBackup errors to continue
            if result != RESULT_SUCCESS
                && result != nfp_result::RESULT_CORRUPTED_DATA
                && result != nfp_result::RESULT_CORRUPTED_DATA_WITH_BACKUP
            {
                return result;
            }
        }

        let _ = self.delete_application_area();
        // Upstream also calls DeleteRegisterInfo() then Flush()
        self.flush()
    }

    /// Restore amiibo data from backup.
    ///
    /// Upstream: requires TagFound state, reads backup data, validates and decodes,
    /// overwrites current tag data, transitions to TagMounted.
    pub fn restore(&mut self) -> ResultCode {
        if self.device_state != DeviceState::TagFound {
            log::error!("Wrong device state {:?} for restore", self.device_state);
            if self.device_state == DeviceState::TagRemoved {
                return nfp_result::RESULT_TAG_REMOVED;
            }
            return nfp_result::RESULT_WRONG_DEVICE_STATE;
        }

        // Upstream: calls GetTagInfo, ReadBackupData, validates, decodes, sets state.
        // Without backup file system and amiibo crypto, return unable to access backup.
        nfp_result::RESULT_UNABLE_TO_ACCESS_BACKUP_FILE
    }

    // ---- Private methods matching upstream ----

    /// Close the currently loaded NFC tag.
    ///
    /// Upstream: unmounts if mounted, transitions to TagRemoved, clears tag data,
    /// clears activate event and signals deactivate event.
    fn close_nfc_tag(&mut self) {
        log::info!("Remove nfc tag");

        if self.device_state == DeviceState::TagMounted {
            let _ = self.unmount();
        }

        self.device_state = DeviceState::TagRemoved;
        self.tag_info = TagInfo::default();
        self.activate_event.clear();
        self.deactivate_event.signal();
    }
}
