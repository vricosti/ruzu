// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nfc/nfc_interface.h
//! Port of zuyu/src/core/hle/service/nfc/nfc_interface.cpp
//!
//! NfcInterface: base service class for NFC, NFP, and Mifare backends.

use crate::hle::result::{ErrorModule, ResultCode, RESULT_SUCCESS};
use super::nfc_types::{BackendType, State};
use super::nfc_result;
use super::mifare_result;
use crate::hle::service::nfp::nfp_result;

/// NfcInterface: base class for NFC services.
///
/// Corresponds to `NfcInterface` in upstream `nfc_interface.h`.
/// Manages device state, backend type, and IPC handler dispatch.
pub struct NfcInterface {
    pub service_name: String,
    pub backend_type: BackendType,
    pub state: State,
    // TODO: device_manager: Option<Arc<DeviceManager>>
    // TODO: service_context: ServiceContext
    // TODO: m_set_sys: shared reference to ISystemSettingsServer
}

/// IPC command IDs for NfcInterface
pub mod commands {
    // Old interface commands
    pub const INITIALIZE_OLD: u32 = 0;
    pub const FINALIZE_OLD: u32 = 1;
    pub const GET_STATE_OLD: u32 = 2;
    pub const IS_NFC_ENABLED_OLD: u32 = 3;
    pub const SET_NFC_ENABLED_OLD: u32 = 100;

    // New interface commands
    pub const INITIALIZE: u32 = 400;
    pub const FINALIZE: u32 = 401;
    pub const GET_STATE: u32 = 402;
    pub const IS_NFC_ENABLED: u32 = 403;
    pub const LIST_DEVICES: u32 = 404;
    pub const GET_DEVICE_STATE: u32 = 405;
    pub const GET_NPAD_ID: u32 = 406;
    pub const ATTACH_AVAILABILITY_CHANGE_EVENT: u32 = 407;
    pub const START_DETECTION: u32 = 408;
    pub const STOP_DETECTION: u32 = 409;
    pub const GET_TAG_INFO: u32 = 410;
    pub const ATTACH_ACTIVATE_EVENT: u32 = 411;
    pub const ATTACH_DEACTIVATE_EVENT: u32 = 412;
    pub const SET_NFC_ENABLED: u32 = 500;
    pub const READ_MIFARE: u32 = 1000;
    pub const WRITE_MIFARE: u32 = 1001;
    pub const SEND_COMMAND_BY_PASS_THROUGH: u32 = 1300;
}

impl NfcInterface {
    pub fn new(name: &str, service_backend: BackendType) -> Self {
        Self {
            service_name: name.to_string(),
            backend_type: service_backend,
            state: State::NonInitialized,
        }
    }

    pub fn initialize(&mut self) -> ResultCode {
        log::info!("NfcInterface::initialize called");
        // TODO: create DeviceManager and call initialize
        self.state = State::Initialized;
        RESULT_SUCCESS
    }

    pub fn finalize(&mut self) -> ResultCode {
        log::info!("NfcInterface::finalize called");
        if self.state != State::NonInitialized {
            // TODO: if backend_type != None, call device_manager.finalize()
            // device_manager = None;
            self.state = State::NonInitialized;
        }
        RESULT_SUCCESS
    }

    pub fn get_state(&self) -> State {
        log::debug!("NfcInterface::get_state called");
        self.state
    }

    pub fn is_nfc_enabled(&self) -> (ResultCode, bool) {
        log::debug!("NfcInterface::is_nfc_enabled called");
        // TODO: query m_set_sys->GetNfcEnableFlag
        (RESULT_SUCCESS, true)
    }

    pub fn list_devices(&self, _max_allowed_devices: usize) -> (ResultCode, Vec<u64>) {
        log::debug!("NfcInterface::list_devices called");
        // TODO: delegate to GetManager()->ListDevices
        (RESULT_SUCCESS, Vec::new())
    }

    pub fn get_device_state(&self, _device_handle: u64) -> super::nfc_types::DeviceState {
        log::debug!("NfcInterface::get_device_state called");
        // TODO: delegate to GetManager()->GetDeviceState
        super::nfc_types::DeviceState::Initialized
    }

    pub fn get_npad_id(&self, _device_handle: u64) -> (ResultCode, u32) {
        log::debug!("NfcInterface::get_npad_id called");
        // TODO: delegate to GetManager()->GetNpadId
        (RESULT_SUCCESS, 0)
    }

    pub fn start_detection(&self, _device_handle: u64) -> ResultCode {
        log::info!("NfcInterface::start_detection called");
        // TODO: delegate to GetManager()->StartDetection
        RESULT_SUCCESS
    }

    pub fn stop_detection(&self, _device_handle: u64) -> ResultCode {
        log::info!("NfcInterface::stop_detection called");
        // TODO: delegate to GetManager()->StopDetection
        RESULT_SUCCESS
    }

    pub fn get_tag_info(&self, _device_handle: u64) -> (ResultCode, super::nfc_types::TagInfo) {
        log::info!("NfcInterface::get_tag_info called");
        // TODO: delegate to GetManager()->GetTagInfo
        (RESULT_SUCCESS, super::nfc_types::TagInfo::default())
    }

    pub fn set_nfc_enabled(&self, _is_enabled: bool) -> ResultCode {
        log::debug!("NfcInterface::set_nfc_enabled called");
        // TODO: delegate to m_set_sys->SetNfcEnableFlag
        RESULT_SUCCESS
    }

    pub fn read_mifare(
        &self,
        _device_handle: u64,
        _read_commands: &[super::mifare_types::MifareReadBlockParameter],
    ) -> (ResultCode, Vec<super::mifare_types::MifareReadBlockData>) {
        log::info!("NfcInterface::read_mifare called");
        // TODO: delegate to GetManager()->ReadMifare
        (RESULT_SUCCESS, Vec::new())
    }

    pub fn write_mifare(
        &self,
        _device_handle: u64,
        _write_commands: &[super::mifare_types::MifareWriteBlockParameter],
    ) -> ResultCode {
        log::info!("(STUBBED) NfcInterface::write_mifare called");
        // TODO: delegate to GetManager()->WriteMifare
        RESULT_SUCCESS
    }

    pub fn send_command_by_pass_through(
        &self,
        _device_handle: u64,
        _timeout: i64,
        _command_data: &[u8],
    ) -> (ResultCode, Vec<u8>) {
        log::info!("(STUBBED) NfcInterface::send_command_by_pass_through called");
        // TODO: delegate to GetManager()->SendCommandByPassThrough
        (RESULT_SUCCESS, vec![0u8])
    }

    pub fn get_backend_type(&self) -> BackendType {
        self.backend_type
    }

    /// Translates NFC result codes to the appropriate backend-specific codes.
    ///
    /// Matches upstream `NfcInterface::TranslateResultToServiceError`.
    pub fn translate_result_to_service_error(&self, result: ResultCode) -> ResultCode {
        if result.is_success() {
            return result;
        }

        if result.get_module() != ErrorModule::NFC {
            return result;
        }

        match self.backend_type {
            BackendType::Mifare => self.translate_result_to_nfp(result),
            BackendType::Nfp => self.translate_result_to_nfp(result),
            _ => {
                if result != nfc_result::RESULT_BACKUP_PATH_ALREADY_EXIST {
                    result
                } else {
                    nfc_result::RESULT_UNKNOWN_74
                }
            }
        }
    }

    /// Translates NFC results to NFP result codes.
    ///
    /// Matches upstream `NfcInterface::TranslateResultToNfp`.
    pub fn translate_result_to_nfp(&self, result: ResultCode) -> ResultCode {
        if result == nfc_result::RESULT_DEVICE_NOT_FOUND {
            return nfp_result::RESULT_DEVICE_NOT_FOUND;
        }
        if result == nfc_result::RESULT_INVALID_ARGUMENT {
            return nfp_result::RESULT_INVALID_ARGUMENT;
        }
        if result == nfc_result::RESULT_WRONG_APPLICATION_AREA_SIZE {
            return nfp_result::RESULT_WRONG_APPLICATION_AREA_SIZE;
        }
        if result == nfc_result::RESULT_WRONG_DEVICE_STATE {
            return nfp_result::RESULT_WRONG_DEVICE_STATE;
        }
        if result == nfc_result::RESULT_UNKNOWN_74 {
            return nfp_result::RESULT_UNKNOWN_74;
        }
        if result == nfc_result::RESULT_NFC_DISABLED {
            return nfp_result::RESULT_NFC_DISABLED;
        }
        if result == nfc_result::RESULT_NFC_NOT_INITIALIZED {
            return nfp_result::RESULT_NFC_DISABLED;
        }
        if result == nfc_result::RESULT_WRITE_AMIIBO_FAILED {
            return nfp_result::RESULT_WRITE_AMIIBO_FAILED;
        }
        if result == nfc_result::RESULT_TAG_REMOVED {
            return nfp_result::RESULT_TAG_REMOVED;
        }
        if result == nfc_result::RESULT_REGISTRATION_IS_NOT_INITIALIZED {
            return nfp_result::RESULT_REGISTRATION_IS_NOT_INITIALIZED;
        }
        if result == nfc_result::RESULT_APPLICATION_AREA_IS_NOT_INITIALIZED {
            return nfp_result::RESULT_APPLICATION_AREA_IS_NOT_INITIALIZED;
        }
        if result == nfc_result::RESULT_CORRUPTED_DATA_WITH_BACKUP {
            return nfp_result::RESULT_CORRUPTED_DATA_WITH_BACKUP;
        }
        if result == nfc_result::RESULT_CORRUPTED_DATA {
            return nfp_result::RESULT_CORRUPTED_DATA;
        }
        if result == nfc_result::RESULT_WRONG_APPLICATION_AREA_ID {
            return nfp_result::RESULT_WRONG_APPLICATION_AREA_ID;
        }
        if result == nfc_result::RESULT_APPLICATION_AREA_EXIST {
            return nfp_result::RESULT_APPLICATION_AREA_EXIST;
        }
        if result == nfc_result::RESULT_INVALID_TAG_TYPE {
            return nfp_result::RESULT_NOT_AN_AMIIBO;
        }
        if result == nfc_result::RESULT_UNABLE_TO_ACCESS_BACKUP_FILE {
            return nfp_result::RESULT_UNABLE_TO_ACCESS_BACKUP_FILE;
        }
        log::warn!("NFC result conversion not handled");
        result
    }

    /// Translates NFC results to Mifare result codes.
    ///
    /// Matches upstream `NfcInterface::TranslateResultToMifare`.
    pub fn translate_result_to_mifare(&self, result: ResultCode) -> ResultCode {
        if result == nfc_result::RESULT_DEVICE_NOT_FOUND {
            return mifare_result::RESULT_DEVICE_NOT_FOUND;
        }
        if result == nfc_result::RESULT_INVALID_ARGUMENT {
            return mifare_result::RESULT_INVALID_ARGUMENT;
        }
        if result == nfc_result::RESULT_WRONG_DEVICE_STATE {
            return mifare_result::RESULT_WRONG_DEVICE_STATE;
        }
        if result == nfc_result::RESULT_NFC_DISABLED {
            return mifare_result::RESULT_NFC_DISABLED;
        }
        if result == nfc_result::RESULT_TAG_REMOVED {
            return mifare_result::RESULT_TAG_REMOVED;
        }
        if result == nfc_result::RESULT_INVALID_TAG_TYPE {
            return mifare_result::RESULT_NOT_A_MIFARE;
        }
        log::warn!("NFC Mifare result conversion not handled");
        result
    }
}
