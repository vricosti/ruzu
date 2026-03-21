// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nfc/nfc_interface.h
//! Port of zuyu/src/core/hle/service/nfc/nfc_interface.cpp
//!
//! NfcInterface: base service class for NFC, NFP, and Mifare backends.

use std::collections::BTreeMap;
use std::sync::atomic::{AtomicU32, Ordering};

use crate::hle::result::{ErrorModule, ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use super::nfc_types::{BackendType, State, TagInfo};
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
    pub state: AtomicU32,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
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
        // IUser command table from upstream nfc.cpp
        let handlers = build_handler_map(&[
            (commands::INITIALIZE_OLD, Some(Self::initialize_handler), "InitializeOld"),
            (commands::FINALIZE_OLD, Some(Self::finalize_handler), "FinalizeOld"),
            (commands::GET_STATE_OLD, Some(Self::get_state_handler), "GetStateOld"),
            (commands::IS_NFC_ENABLED_OLD, Some(Self::is_nfc_enabled_handler), "IsNfcEnabledOld"),
            (commands::INITIALIZE, Some(Self::initialize_handler), "Initialize"),
            (commands::FINALIZE, Some(Self::finalize_handler), "Finalize"),
            (commands::GET_STATE, Some(Self::get_state_handler), "GetState"),
            (commands::IS_NFC_ENABLED, Some(Self::is_nfc_enabled_handler), "IsNfcEnabled"),
            (commands::LIST_DEVICES, Some(Self::list_devices_handler), "ListDevices"),
            (commands::GET_DEVICE_STATE, Some(Self::get_device_state_handler), "GetDeviceState"),
            (commands::GET_NPAD_ID, Some(Self::get_npad_id_handler), "GetNpadId"),
            (commands::ATTACH_AVAILABILITY_CHANGE_EVENT, Some(Self::attach_availability_change_event_handler), "AttachAvailabilityChangeEvent"),
            (commands::START_DETECTION, Some(Self::start_detection_handler), "StartDetection"),
            (commands::STOP_DETECTION, Some(Self::stop_detection_handler), "StopDetection"),
            (commands::GET_TAG_INFO, Some(Self::get_tag_info_handler), "GetTagInfo"),
            (commands::ATTACH_ACTIVATE_EVENT, Some(Self::attach_activate_event_handler), "AttachActivateEvent"),
            (commands::ATTACH_DEACTIVATE_EVENT, Some(Self::attach_deactivate_event_handler), "AttachDeactivateEvent"),
            (commands::SET_NFC_ENABLED, Some(Self::set_nfc_enabled_handler), "SetNfcEnabled"),
            (commands::READ_MIFARE, Some(Self::read_mifare_handler), "ReadMifare"),
            (commands::WRITE_MIFARE, Some(Self::write_mifare_handler), "WriteMifare"),
            (commands::SEND_COMMAND_BY_PASS_THROUGH, Some(Self::send_command_by_pass_through_handler), "SendCommandByPassThrough"),
            (1301, None, "KeepPassThroughSession"),
            (1302, None, "ReleasePassThroughSession"),
        ]);

        Self {
            service_name: name.to_string(),
            backend_type: service_backend,
            state: AtomicU32::new(State::NonInitialized as u32),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    pub fn initialize(&self) -> ResultCode {
        log::info!("NfcInterface::initialize called");
        // TODO: create DeviceManager and call initialize
        self.state.store(State::Initialized as u32, Ordering::Relaxed);
        RESULT_SUCCESS
    }

    pub fn finalize(&self) -> ResultCode {
        log::info!("NfcInterface::finalize called");
        if self.state.load(Ordering::Relaxed) != State::NonInitialized as u32 {
            // TODO: if backend_type != None, call device_manager.finalize()
            // device_manager = None;
            self.state.store(State::NonInitialized as u32, Ordering::Relaxed);
        }
        RESULT_SUCCESS
    }

    pub fn get_state(&self) -> State {
        log::debug!("NfcInterface::get_state called");
        let raw = self.state.load(Ordering::Relaxed);
        match raw {
            0 => State::NonInitialized,
            _ => State::Initialized,
        }
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

    /// Returns a reference to the handler map.
    pub fn get_handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
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

    // --- IPC handler functions ---

    /// Initialize (cmd 0/400).
    /// Upstream: NfcInterface::Initialize
    fn initialize_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        log::info!("NFC::Initialize called");
        let result = service.initialize();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    /// Finalize (cmd 1/401).
    /// Upstream: NfcInterface::Finalize
    fn finalize_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        log::info!("NFC::Finalize called");
        let _result = service.finalize();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// GetState (cmd 2/402).
    /// Upstream: NfcInterface::GetState
    fn get_state_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        log::debug!("NFC::GetState called");

        let state = service.get_state();

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(state as u32);
    }

    /// IsNfcEnabled (cmd 3/403).
    /// Upstream: NfcInterface::IsNfcEnabled
    fn is_nfc_enabled_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        log::debug!("NFC::IsNfcEnabled called");

        let (result, is_enabled) = service.is_nfc_enabled();

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(result);
        rb.push_bool(is_enabled);
    }

    /// ListDevices (cmd 404).
    /// Upstream: NfcInterface::ListDevices
    fn list_devices_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        log::debug!("NFC::ListDevices called");

        // max_allowed_devices = write_buffer_size / sizeof(u64)
        let max_allowed_devices = ctx.get_write_buffer_size(0) / core::mem::size_of::<u64>();
        let (result, devices) = service.list_devices(max_allowed_devices);

        if result.is_error() {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(result);
            return;
        }

        // Write device handles to output buffer
        let bytes: Vec<u8> = devices
            .iter()
            .flat_map(|d| d.to_le_bytes())
            .collect();
        ctx.write_buffer(&bytes, 0);

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_i32(devices.len() as i32);
    }

    /// GetDeviceState (cmd 405).
    /// Upstream: NfcInterface::GetDeviceState
    fn get_device_state_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        log::debug!("NFC::GetDeviceState called, device_handle={}", device_handle);

        let device_state = service.get_device_state(device_handle);

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(device_state as u32);
    }

    /// GetNpadId (cmd 406).
    /// Upstream: NfcInterface::GetNpadId
    fn get_npad_id_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        log::debug!("NFC::GetNpadId called, device_handle={}", device_handle);

        let (result, npad_id) = service.get_npad_id(device_handle);

        if result.is_error() {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(result);
            return;
        }

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(npad_id);
    }

    /// AttachAvailabilityChangeEvent (cmd 407).
    /// Upstream: NfcInterface::AttachAvailabilityChangeEvent
    fn attach_availability_change_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let _service = Self::as_self(this);
        log::info!("NFC::AttachAvailabilityChangeEvent called");

        // TODO: GetManager()->AttachAvailabilityChangeEvent()
        let event_handle = ctx.create_readable_event_handle(false).unwrap_or(0);

        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(event_handle);
    }

    /// StartDetection (cmd 408).
    /// Upstream: NfcInterface::StartDetection
    fn start_detection_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        // For NFC backend, read tag_protocol from params
        let _tag_protocol = if service.backend_type == BackendType::Nfc {
            rp.pop_u32()
        } else {
            // NFP/Mifare backend uses NfcProtocol::All
            0xFFFFFFFF
        };
        log::info!("NFC::StartDetection called, device_handle={}", device_handle);

        let result = service.start_detection(device_handle);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    /// StopDetection (cmd 409).
    /// Upstream: NfcInterface::StopDetection
    fn stop_detection_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        log::info!("NFC::StopDetection called, device_handle={}", device_handle);

        let result = service.stop_detection(device_handle);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    /// GetTagInfo (cmd 410).
    /// Upstream: NfcInterface::GetTagInfo
    fn get_tag_info_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        log::info!("NFC::GetTagInfo called, device_handle={}", device_handle);

        let (result, tag_info) = service.get_tag_info(device_handle);

        if result.is_success() {
            let bytes = unsafe {
                core::slice::from_raw_parts(
                    &tag_info as *const TagInfo as *const u8,
                    core::mem::size_of::<TagInfo>(),
                )
            };
            ctx.write_buffer(bytes, 0);
        }

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    /// AttachActivateEvent (cmd 411).
    /// Upstream: NfcInterface::AttachActivateEvent
    fn attach_activate_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let _service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        log::debug!("NFC::AttachActivateEvent called, device_handle={}", device_handle);

        // TODO: GetManager()->AttachActivateEvent(&out_event, device_handle)
        let event_handle = ctx.create_readable_event_handle(false).unwrap_or(0);

        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(event_handle);
    }

    /// AttachDeactivateEvent (cmd 412).
    /// Upstream: NfcInterface::AttachDeactivateEvent
    fn attach_deactivate_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let _service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        log::debug!("NFC::AttachDeactivateEvent called, device_handle={}", device_handle);

        // TODO: GetManager()->AttachDeactivateEvent(&out_event, device_handle)
        let event_handle = ctx.create_readable_event_handle(false).unwrap_or(0);

        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(event_handle);
    }

    /// SetNfcEnabled (cmd 500).
    /// Upstream: NfcInterface::SetNfcEnabled
    fn set_nfc_enabled_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let is_enabled = rp.pop_bool();
        log::debug!("NFC::SetNfcEnabled called, is_enabled={}", is_enabled);

        let result = service.set_nfc_enabled(is_enabled);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    /// ReadMifare (cmd 1000).
    /// Upstream: NfcInterface::ReadMifare
    fn read_mifare_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        let buffer = ctx.read_buffer(0);

        let param_size = core::mem::size_of::<super::mifare_types::MifareReadBlockParameter>();
        let number_of_commands = buffer.len() / param_size;
        let mut read_commands = Vec::with_capacity(number_of_commands);
        for i in 0..number_of_commands {
            let offset = i * param_size;
            let mut param = super::mifare_types::MifareReadBlockParameter::default();
            unsafe {
                core::ptr::copy_nonoverlapping(
                    buffer[offset..].as_ptr(),
                    &mut param as *mut super::mifare_types::MifareReadBlockParameter as *mut u8,
                    param_size,
                );
            }
            read_commands.push(param);
        }

        log::info!(
            "NFC::ReadMifare called, device_handle={}, read_commands_size={}",
            device_handle, number_of_commands
        );

        let (result, out_data) = service.read_mifare(device_handle, &read_commands);

        if result.is_success() {
            let data_size = core::mem::size_of::<super::mifare_types::MifareReadBlockData>();
            let bytes: Vec<u8> = out_data
                .iter()
                .flat_map(|d| unsafe {
                    core::slice::from_raw_parts(
                        d as *const super::mifare_types::MifareReadBlockData as *const u8,
                        data_size,
                    )
                })
                .copied()
                .collect();
            ctx.write_buffer(&bytes, 0);
        }

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    /// WriteMifare (cmd 1001).
    /// Upstream: NfcInterface::WriteMifare
    fn write_mifare_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        let buffer = ctx.read_buffer(0);

        let param_size = core::mem::size_of::<super::mifare_types::MifareWriteBlockParameter>();
        let number_of_commands = buffer.len() / param_size;
        let mut write_commands = Vec::with_capacity(number_of_commands);
        for i in 0..number_of_commands {
            let offset = i * param_size;
            let mut param = super::mifare_types::MifareWriteBlockParameter::default();
            unsafe {
                core::ptr::copy_nonoverlapping(
                    buffer[offset..].as_ptr(),
                    &mut param as *mut super::mifare_types::MifareWriteBlockParameter as *mut u8,
                    param_size,
                );
            }
            write_commands.push(param);
        }

        log::info!(
            "(STUBBED) NFC::WriteMifare called, device_handle={}, write_commands_size={}",
            device_handle, number_of_commands
        );

        let result = service.write_mifare(device_handle, &write_commands);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    /// SendCommandByPassThrough (cmd 1300).
    /// Upstream: NfcInterface::SendCommandByPassThrough
    fn send_command_by_pass_through_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        let timeout = rp.pop_i64();
        let command_data = ctx.read_buffer(0);
        log::info!(
            "(STUBBED) NFC::SendCommandByPassThrough called, device_handle={}, timeout={}, data_size={}",
            device_handle, timeout, command_data.len()
        );

        let (result, out_data) = service.send_command_by_pass_through(device_handle, timeout, &command_data);

        if result.is_error() {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(result);
            return;
        }

        ctx.write_buffer(&out_data, 0);

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(out_data.len() as u32);
    }
}

impl SessionRequestHandler for NfcInterface {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        &self.service_name
    }
}

impl ServiceFramework for NfcInterface {
    fn get_service_name(&self) -> &str {
        &self.service_name
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
