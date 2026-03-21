// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nfp/nfp_interface.h
//! Port of zuyu/src/core/hle/service/nfp/nfp_interface.cpp
//!
//! Interface -- NFP interface for amiibo operations.
//! This is the concrete NFP service that extends NfcInterface (NFC base).

use std::collections::BTreeMap;
use std::sync::atomic::{AtomicU32, Ordering};

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use super::nfp_types::{
    BreakType, CommonInfo, DeviceState, ModelType, MountTarget, TagInfo, WriteType,
};

/// IPC command table for Interface (IUser / ISystem / IDebug).
///
/// Corresponds to the function table in upstream nfp.cpp constructors.
pub mod commands {
    pub const INITIALIZE: u32 = 0;
    pub const FINALIZE: u32 = 1;
    pub const LIST_DEVICES: u32 = 2;
    pub const START_DETECTION: u32 = 3;
    pub const STOP_DETECTION: u32 = 4;
    pub const MOUNT: u32 = 5;
    pub const UNMOUNT: u32 = 6;
    pub const OPEN_APPLICATION_AREA: u32 = 7;
    pub const GET_APPLICATION_AREA: u32 = 8;
    pub const SET_APPLICATION_AREA: u32 = 9;
    pub const FLUSH: u32 = 10;
    pub const RESTORE: u32 = 11;
    pub const CREATE_APPLICATION_AREA: u32 = 12;
    pub const GET_TAG_INFO: u32 = 13;
    pub const GET_REGISTER_INFO: u32 = 14;
    pub const GET_COMMON_INFO: u32 = 15;
    pub const GET_MODEL_INFO: u32 = 16;
    pub const ATTACH_ACTIVATE_EVENT: u32 = 17;
    pub const ATTACH_DEACTIVATE_EVENT: u32 = 18;
    pub const GET_STATE: u32 = 19;
    pub const GET_DEVICE_STATE: u32 = 20;
    pub const GET_NFC_NPAD_ID: u32 = 21;
    pub const GET_APPLICATION_AREA_SIZE: u32 = 22;
    pub const ATTACH_AVAILABILITY_CHANGE_EVENT: u32 = 23;
    pub const RECREATE_APPLICATION_AREA: u32 = 24;
    // System/Debug commands
    pub const FORMAT: u32 = 100;
    pub const GET_ADMIN_INFO: u32 = 101;
    pub const GET_REGISTER_INFO_PRIVATE: u32 = 102;
    pub const SET_REGISTER_INFO_PRIVATE: u32 = 103;
    pub const DELETE_REGISTER_INFO: u32 = 104;
    pub const DELETE_APPLICATION_AREA: u32 = 105;
    pub const EXISTS_APPLICATION_AREA: u32 = 106;
    // Debug commands
    pub const GET_ALL: u32 = 200;
    pub const SET_ALL: u32 = 201;
    pub const FLUSH_DEBUG: u32 = 202;
    pub const BREAK_TAG: u32 = 203;
    pub const READ_BACKUP_DATA: u32 = 204;
    pub const WRITE_BACKUP_DATA: u32 = 205;
    pub const WRITE_NTF: u32 = 206;
    // InitializeSystem/Debug/Finalize commands
    pub const INITIALIZE_SYSTEM: u32 = 400;
    pub const FINALIZE_SYSTEM: u32 = 401;
    pub const INITIALIZE_DEBUG: u32 = 500;
    pub const FINALIZE_DEBUG: u32 = 501;
}

/// NFP State, mirroring NFC::State for the base class behavior.
///
/// Upstream NFP::Interface inherits NfcInterface which tracks this state.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum State {
    NonInitialized = 0,
    Initialized = 1,
}

/// Interface -- NFP service interface for amiibo.
///
/// Corresponds to `Interface` in upstream nfp_interface.h / nfp_interface.cpp.
/// Extends NfcInterface with NFP-specific methods (amiibo data management).
pub struct Interface {
    system: crate::core::SystemRef,
    name: String,
    state: AtomicU32,
    device_state: DeviceState,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    // In upstream this holds a DeviceManager reference
}

impl Interface {
    pub fn new(system: crate::core::SystemRef, name: &str) -> Self {
        // IUser command table from upstream nfp.cpp
        let handlers = build_handler_map(&[
            (commands::INITIALIZE, Some(Self::initialize_handler), "Initialize"),
            (commands::FINALIZE, Some(Self::finalize_handler), "Finalize"),
            (commands::LIST_DEVICES, Some(Self::list_devices_handler), "ListDevices"),
            (commands::START_DETECTION, Some(Self::start_detection_handler), "StartDetection"),
            (commands::STOP_DETECTION, Some(Self::stop_detection_handler), "StopDetection"),
            (commands::MOUNT, Some(Self::mount_handler), "Mount"),
            (commands::UNMOUNT, Some(Self::unmount_handler), "Unmount"),
            (commands::OPEN_APPLICATION_AREA, Some(Self::open_application_area_handler), "OpenApplicationArea"),
            (commands::GET_APPLICATION_AREA, Some(Self::get_application_area_handler), "GetApplicationArea"),
            (commands::SET_APPLICATION_AREA, Some(Self::set_application_area_handler), "SetApplicationArea"),
            (commands::FLUSH, Some(Self::flush_handler), "Flush"),
            (commands::RESTORE, Some(Self::restore_handler), "Restore"),
            (commands::CREATE_APPLICATION_AREA, Some(Self::create_application_area_handler), "CreateApplicationArea"),
            (commands::GET_TAG_INFO, Some(Self::get_tag_info_handler), "GetTagInfo"),
            (commands::GET_REGISTER_INFO, Some(Self::get_register_info_handler), "GetRegisterInfo"),
            (commands::GET_COMMON_INFO, Some(Self::get_common_info_handler), "GetCommonInfo"),
            (commands::GET_MODEL_INFO, Some(Self::get_model_info_handler), "GetModelInfo"),
            (commands::ATTACH_ACTIVATE_EVENT, Some(Self::attach_activate_event_handler), "AttachActivateEvent"),
            (commands::ATTACH_DEACTIVATE_EVENT, Some(Self::attach_deactivate_event_handler), "AttachDeactivateEvent"),
            (commands::GET_STATE, Some(Self::get_state_handler), "GetState"),
            (commands::GET_DEVICE_STATE, Some(Self::get_device_state_handler), "GetDeviceState"),
            (commands::GET_NFC_NPAD_ID, Some(Self::get_npad_id_handler), "GetNpadId"),
            (commands::GET_APPLICATION_AREA_SIZE, Some(Self::get_application_area_size_handler), "GetApplicationAreaSize"),
            (commands::ATTACH_AVAILABILITY_CHANGE_EVENT, Some(Self::attach_availability_change_event_handler), "AttachAvailabilityChangeEvent"),
            (commands::RECREATE_APPLICATION_AREA, Some(Self::recreate_application_area_handler), "RecreateApplicationArea"),
        ]);

        Self {
            system,
            name: name.to_string(),
            state: AtomicU32::new(State::NonInitialized as u32),
            device_state: DeviceState::Initialized,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    // --- Base class (NfcInterface) methods ---
    // In upstream these are inherited from NfcInterface. Since Rust has no
    // inheritance, they are implemented directly on Interface.

    /// Initialize (cmd 0).
    ///
    /// Corresponds to `NfcInterface::Initialize` in upstream nfc_interface.cpp.
    pub fn initialize(&self) -> ResultCode {
        log::info!("NFP::Interface({})::Initialize called", self.name);
        // TODO: create DeviceManager and call initialize
        self.state.store(State::Initialized as u32, Ordering::Relaxed);
        RESULT_SUCCESS
    }

    /// Finalize (cmd 1).
    ///
    /// Corresponds to `NfcInterface::Finalize` in upstream nfc_interface.cpp.
    pub fn finalize(&self) -> ResultCode {
        log::info!("NFP::Interface({})::Finalize called", self.name);
        if self.state.load(Ordering::Relaxed) != State::NonInitialized as u32 {
            // TODO: if backend_type != None, call device_manager.finalize()
            self.state.store(State::NonInitialized as u32, Ordering::Relaxed);
        }
        RESULT_SUCCESS
    }

    /// ListDevices (cmd 2).
    ///
    /// Corresponds to `NfcInterface::ListDevices` in upstream nfc_interface.cpp.
    pub fn list_devices(&self) -> (ResultCode, Vec<u64>) {
        log::debug!("NFP::ListDevices called");
        // TODO: delegate to GetManager()->ListDevices
        (RESULT_SUCCESS, Vec::new())
    }

    /// StartDetection (cmd 3).
    ///
    /// Corresponds to `NfcInterface::StartDetection` in upstream nfc_interface.cpp.
    /// For NFP backend, tag_protocol is always NfcProtocol::All (no rp.PopEnum).
    pub fn start_detection(&self, _device_handle: u64) -> ResultCode {
        log::info!("NFP::StartDetection called");
        // TODO: delegate to GetManager()->StartDetection(device_handle, NfcProtocol::All)
        RESULT_SUCCESS
    }

    /// StopDetection (cmd 4).
    ///
    /// Corresponds to `NfcInterface::StopDetection` in upstream nfc_interface.cpp.
    pub fn stop_detection(&self, _device_handle: u64) -> ResultCode {
        log::info!("NFP::StopDetection called");
        // TODO: delegate to GetManager()->StopDetection(device_handle)
        RESULT_SUCCESS
    }

    /// Mount (cmd 5).
    ///
    /// Corresponds to `Interface::Mount` in upstream nfp_interface.cpp.
    pub fn mount(
        &self,
        device_handle: u64,
        model_type: ModelType,
        mount_target: MountTarget,
    ) -> ResultCode {
        log::info!(
            "NFP::Mount called, device_handle={}, model_type={:?}, mount_target={:?}",
            device_handle,
            model_type,
            mount_target
        );
        // TODO: GetManager()->Mount(device_handle, model_type, mount_target)
        RESULT_SUCCESS
    }

    /// Unmount (cmd 6).
    ///
    /// Corresponds to `Interface::Unmount` in upstream nfp_interface.cpp.
    pub fn unmount(&self, device_handle: u64) -> ResultCode {
        log::info!("NFP::Unmount called, device_handle={}", device_handle);
        // TODO: GetManager()->Unmount(device_handle)
        RESULT_SUCCESS
    }

    /// OpenApplicationArea (cmd 7).
    ///
    /// Corresponds to `Interface::OpenApplicationArea` in upstream nfp_interface.cpp.
    pub fn open_application_area(&self, device_handle: u64, access_id: u32) -> ResultCode {
        log::info!(
            "NFP::OpenApplicationArea called, device_handle={}, access_id={}",
            device_handle,
            access_id
        );
        // TODO: GetManager()->OpenApplicationArea(device_handle, access_id)
        RESULT_SUCCESS
    }

    /// GetApplicationArea (cmd 8).
    ///
    /// Corresponds to `Interface::GetApplicationArea` in upstream nfp_interface.cpp.
    pub fn get_application_area(
        &self,
        device_handle: u64,
        data: &mut Vec<u8>,
    ) -> (ResultCode, u32) {
        log::info!(
            "NFP::GetApplicationArea called, device_handle={}",
            device_handle
        );
        // TODO: GetManager()->GetApplicationArea(device_handle, data)
        let data_size = data.len() as u32;
        (RESULT_SUCCESS, data_size)
    }

    /// SetApplicationArea (cmd 9).
    ///
    /// Corresponds to `Interface::SetApplicationArea` in upstream nfp_interface.cpp.
    pub fn set_application_area(&self, device_handle: u64, data: &[u8]) -> ResultCode {
        log::info!(
            "NFP::SetApplicationArea called, device_handle={}, data_size={}",
            device_handle,
            data.len()
        );
        // TODO: GetManager()->SetApplicationArea(device_handle, data)
        RESULT_SUCCESS
    }

    /// Flush (cmd 10).
    ///
    /// Corresponds to `Interface::Flush` in upstream nfp_interface.cpp.
    pub fn flush(&self, device_handle: u64) -> ResultCode {
        log::info!("NFP::Flush called, device_handle={}", device_handle);
        // TODO: GetManager()->Flush(device_handle)
        RESULT_SUCCESS
    }

    /// Restore (cmd 11).
    ///
    /// Corresponds to `Interface::Restore` in upstream nfp_interface.cpp.
    pub fn restore(&self, device_handle: u64) -> ResultCode {
        log::info!("NFP::Restore called, device_handle={}", device_handle);
        // TODO: GetManager()->Restore(device_handle)
        RESULT_SUCCESS
    }

    /// CreateApplicationArea (cmd 12).
    ///
    /// Corresponds to `Interface::CreateApplicationArea` in upstream nfp_interface.cpp.
    pub fn create_application_area(
        &self,
        device_handle: u64,
        access_id: u32,
        data: &[u8],
    ) -> ResultCode {
        log::info!(
            "NFP::CreateApplicationArea called, device_handle={}, access_id={}, data_size={}",
            device_handle,
            access_id,
            data.len()
        );
        // TODO: GetManager()->CreateApplicationArea(device_handle, access_id, data)
        RESULT_SUCCESS
    }

    /// GetTagInfo (cmd 13).
    ///
    /// Corresponds to `NfcInterface::GetTagInfo` in upstream nfc_interface.cpp.
    pub fn get_tag_info(&self, _device_handle: u64) -> (ResultCode, TagInfo) {
        log::info!("NFP::GetTagInfo called");
        // TODO: delegate to GetManager()->GetTagInfo
        (RESULT_SUCCESS, TagInfo::default())
    }

    /// GetState (cmd 19).
    ///
    /// Corresponds to `NfcInterface::GetState` in upstream nfc_interface.cpp.
    pub fn get_state(&self) -> State {
        log::debug!("NFP::GetState called");
        let raw = self.state.load(Ordering::Relaxed);
        match raw {
            0 => State::NonInitialized,
            _ => State::Initialized,
        }
    }

    /// GetDeviceState (cmd 20).
    ///
    /// Corresponds to `NfcInterface::GetDeviceState` in upstream nfc_interface.cpp.
    pub fn get_device_state(&self, _device_handle: u64) -> DeviceState {
        log::debug!("NFP::GetDeviceState called");
        // TODO: delegate to GetManager()->GetDeviceState
        self.device_state
    }

    /// GetNpadId (cmd 21).
    ///
    /// Corresponds to `NfcInterface::GetNpadId` in upstream nfc_interface.cpp.
    pub fn get_npad_id(&self, _device_handle: u64) -> (ResultCode, u32) {
        log::debug!("NFP::GetNpadId called");
        // TODO: delegate to GetManager()->GetNpadId
        (RESULT_SUCCESS, 0)
    }

    /// GetApplicationAreaSize (cmd 22).
    ///
    /// Corresponds to `Interface::GetApplicationAreaSize` in upstream nfp_interface.cpp.
    pub fn get_application_area_size(&self, _device_handle: u64) -> u32 {
        log::debug!("NFP::GetApplicationAreaSize called");
        // TODO: GetManager()->GetApplicationAreaSize()
        0xD8 // Default application area size
    }

    /// RecreateApplicationArea (cmd 24).
    ///
    /// Corresponds to `Interface::RecreateApplicationArea` in upstream nfp_interface.cpp.
    pub fn recreate_application_area(
        &self,
        device_handle: u64,
        access_id: u32,
        data: &[u8],
    ) -> ResultCode {
        log::info!(
            "NFP::RecreateApplicationArea called, device_handle={}, access_id={}, data_size={}",
            device_handle,
            access_id,
            data.len()
        );
        // TODO: GetManager()->RecreateApplicationArea(device_handle, access_id, data)
        RESULT_SUCCESS
    }

    /// Format (cmd 100).
    ///
    /// Corresponds to `Interface::Format` in upstream nfp_interface.cpp.
    pub fn format(&self, device_handle: u64) -> ResultCode {
        log::info!("NFP::Format called, device_handle={}", device_handle);
        // TODO: GetManager()->Format(device_handle)
        RESULT_SUCCESS
    }

    /// DeleteRegisterInfo (cmd 104).
    ///
    /// Corresponds to `Interface::DeleteRegisterInfo` in upstream nfp_interface.cpp.
    pub fn delete_register_info(&self, device_handle: u64) -> ResultCode {
        log::info!(
            "NFP::DeleteRegisterInfo called, device_handle={}",
            device_handle
        );
        // TODO: GetManager()->DeleteRegisterInfo(device_handle)
        RESULT_SUCCESS
    }

    /// DeleteApplicationArea (cmd 105).
    ///
    /// Corresponds to `Interface::DeleteApplicationArea` in upstream nfp_interface.cpp.
    pub fn delete_application_area(&self, device_handle: u64) -> ResultCode {
        log::info!(
            "NFP::DeleteApplicationArea called, device_handle={}",
            device_handle
        );
        // TODO: GetManager()->DeleteApplicationArea(device_handle)
        RESULT_SUCCESS
    }

    /// ExistsApplicationArea (cmd 106).
    ///
    /// Corresponds to `Interface::ExistsApplicationArea` in upstream nfp_interface.cpp.
    pub fn exists_application_area(&self, device_handle: u64) -> (ResultCode, bool) {
        log::info!(
            "NFP::ExistsApplicationArea called, device_handle={}",
            device_handle
        );
        // TODO: GetManager()->ExistsApplicationArea(device_handle, &has_area)
        (RESULT_SUCCESS, false)
    }

    /// FlushDebug (cmd 202).
    ///
    /// Corresponds to `Interface::FlushDebug` in upstream nfp_interface.cpp.
    pub fn flush_debug(&self, device_handle: u64) -> ResultCode {
        log::info!("NFP::FlushDebug called, device_handle={}", device_handle);
        // TODO: GetManager()->FlushDebug(device_handle)
        RESULT_SUCCESS
    }

    /// BreakTag (cmd 203).
    ///
    /// Corresponds to `Interface::BreakTag` in upstream nfp_interface.cpp.
    pub fn break_tag(&self, device_handle: u64, break_type: BreakType) -> ResultCode {
        log::warn!(
            "(STUBBED) NFP::BreakTag called, device_handle={}, break_type={:?}",
            device_handle,
            break_type
        );
        // TODO: GetManager()->BreakTag(device_handle, break_type)
        RESULT_SUCCESS
    }

    /// ReadBackupData (cmd 204).
    ///
    /// Corresponds to `Interface::ReadBackupData` in upstream nfp_interface.cpp.
    pub fn read_backup_data(&self, device_handle: u64) -> (ResultCode, Vec<u8>) {
        log::info!(
            "NFP::ReadBackupData called, device_handle={}",
            device_handle
        );
        // TODO: GetManager()->ReadBackupData(device_handle, &backup_data)
        (RESULT_SUCCESS, Vec::new())
    }

    /// WriteBackupData (cmd 205).
    ///
    /// Corresponds to `Interface::WriteBackupData` in upstream nfp_interface.cpp.
    pub fn write_backup_data(&self, device_handle: u64, data: &[u8]) -> ResultCode {
        log::info!(
            "NFP::WriteBackupData called, device_handle={}",
            device_handle
        );
        let _ = data;
        // TODO: GetManager()->WriteBackupData(device_handle, data)
        RESULT_SUCCESS
    }

    /// WriteNtf (cmd 206).
    ///
    /// Corresponds to `Interface::WriteNtf` in upstream nfp_interface.cpp.
    pub fn write_ntf(
        &self,
        device_handle: u64,
        write_type: WriteType,
        data: &[u8],
    ) -> ResultCode {
        log::warn!(
            "(STUBBED) NFP::WriteNtf called, device_handle={}, write_type={:?}",
            device_handle,
            write_type
        );
        let _ = data;
        // TODO: GetManager()->WriteNtf(device_handle, write_type, data)
        RESULT_SUCCESS
    }

    // --- IPC handler functions ---

    /// Initialize (cmd 0).
    /// Upstream: NfcInterface::Initialize
    fn initialize_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        log::info!("NFP::Initialize called");
        let result = service.initialize();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    /// Finalize (cmd 1).
    /// Upstream: NfcInterface::Finalize
    fn finalize_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        log::info!("NFP::Finalize called");
        let _result = service.finalize();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// ListDevices (cmd 2).
    /// Upstream: NfcInterface::ListDevices
    fn list_devices_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        log::debug!("NFP::ListDevices called");

        let (result, devices) = service.list_devices();

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

    /// StartDetection (cmd 3).
    /// Upstream: NfcInterface::StartDetection
    /// For NFP backend, tag_protocol is always NfcProtocol::All (not read from params).
    fn start_detection_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        // NFP backend does not read tag_protocol from params (uses NfcProtocol::All)
        log::info!("NFP::StartDetection called, device_handle={}", device_handle);

        let result = service.start_detection(device_handle);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    /// StopDetection (cmd 4).
    /// Upstream: NfcInterface::StopDetection
    fn stop_detection_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        log::info!("NFP::StopDetection called, device_handle={}", device_handle);

        let result = service.stop_detection(device_handle);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    /// Mount (cmd 5).
    /// Upstream: Interface::Mount
    fn mount_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        let model_type_raw = rp.pop_u32();
        let mount_target_raw = rp.pop_u32();

        let model_type = match model_type_raw {
            0 => ModelType::Amiibo,
            _ => ModelType::Amiibo,
        };
        let mount_target = match mount_target_raw {
            1 => MountTarget::Rom,
            2 => MountTarget::Ram,
            3 => MountTarget::All,
            _ => MountTarget::All,
        };

        log::info!(
            "NFP::Mount called, device_handle={}, model_type={:?}, mount_target={:?}",
            device_handle, model_type, mount_target
        );

        let result = service.mount(device_handle, model_type, mount_target);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    /// Unmount (cmd 6).
    /// Upstream: Interface::Unmount
    fn unmount_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        log::info!("NFP::Unmount called, device_handle={}", device_handle);

        let result = service.unmount(device_handle);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    /// OpenApplicationArea (cmd 7).
    /// Upstream: Interface::OpenApplicationArea
    fn open_application_area_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        let access_id = rp.pop_u32();
        log::info!(
            "NFP::OpenApplicationArea called, device_handle={}, access_id={}",
            device_handle, access_id
        );

        let result = service.open_application_area(device_handle, access_id);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    /// GetApplicationArea (cmd 8).
    /// Upstream: Interface::GetApplicationArea
    fn get_application_area_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        let data_size = ctx.get_write_buffer_size(0);
        log::info!("NFP::GetApplicationArea called, device_handle={}", device_handle);

        let mut data = vec![0u8; data_size];
        let (result, _size) = service.get_application_area(device_handle, &mut data);

        if result.is_error() {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(result);
            return;
        }

        ctx.write_buffer(&data, 0);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(result);
        rb.push_u32(data_size as u32);
    }

    /// SetApplicationArea (cmd 9).
    /// Upstream: Interface::SetApplicationArea
    fn set_application_area_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        let data = ctx.read_buffer(0);
        log::info!(
            "NFP::SetApplicationArea called, device_handle={}, data_size={}",
            device_handle, data.len()
        );

        let result = service.set_application_area(device_handle, &data);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    /// Flush (cmd 10).
    /// Upstream: Interface::Flush
    fn flush_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        log::info!("NFP::Flush called, device_handle={}", device_handle);

        let result = service.flush(device_handle);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    /// Restore (cmd 11).
    /// Upstream: Interface::Restore
    fn restore_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        log::info!("NFP::Restore called, device_handle={}", device_handle);

        let result = service.restore(device_handle);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    /// CreateApplicationArea (cmd 12).
    /// Upstream: Interface::CreateApplicationArea
    fn create_application_area_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        let access_id = rp.pop_u32();
        let data = ctx.read_buffer(0);
        log::info!(
            "NFP::CreateApplicationArea called, device_handle={}, access_id={}, data_size={}",
            device_handle, access_id, data.len()
        );

        let result = service.create_application_area(device_handle, access_id, &data);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    /// GetTagInfo (cmd 13).
    /// Upstream: NfcInterface::GetTagInfo
    fn get_tag_info_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        log::info!("NFP::GetTagInfo called, device_handle={}", device_handle);

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

    /// GetRegisterInfo (cmd 14).
    /// Upstream: Interface::GetRegisterInfo
    fn get_register_info_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let _service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        log::info!("NFP::GetRegisterInfo called, device_handle={}", device_handle);

        // TODO: GetManager()->GetRegisterInfo(device_handle, register_info)
        // For now, write zeroed RegisterInfo to buffer
        let register_info = super::nfp_types::RegisterInfo {
            mii_store_data: [0u8; 0x44],
            creation_year: 0,
            creation_month: 0,
            creation_day: 0,
            amiibo_name: [0u8; 41],
            font_region: 0,
            reserved: [0u8; 0x7A],
        };
        let bytes = unsafe {
            core::slice::from_raw_parts(
                &register_info as *const super::nfp_types::RegisterInfo as *const u8,
                core::mem::size_of::<super::nfp_types::RegisterInfo>(),
            )
        };
        ctx.write_buffer(bytes, 0);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// GetCommonInfo (cmd 15).
    /// Upstream: Interface::GetCommonInfo
    fn get_common_info_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let _service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        log::info!("NFP::GetCommonInfo called, device_handle={}", device_handle);

        // TODO: GetManager()->GetCommonInfo(device_handle, common_info)
        let common_info = CommonInfo::default();
        let bytes = unsafe {
            core::slice::from_raw_parts(
                &common_info as *const CommonInfo as *const u8,
                core::mem::size_of::<CommonInfo>(),
            )
        };
        ctx.write_buffer(bytes, 0);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// GetModelInfo (cmd 16).
    /// Upstream: Interface::GetModelInfo
    fn get_model_info_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let _service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        log::info!("NFP::GetModelInfo called, device_handle={}", device_handle);

        // TODO: GetManager()->GetModelInfo(device_handle, model_info)
        // ModelInfo is a small struct; write zeroed data for now
        let model_info = [0u8; 0x40]; // ModelInfo size
        ctx.write_buffer(&model_info, 0);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// AttachActivateEvent (cmd 17).
    /// Upstream: NfcInterface::AttachActivateEvent
    fn attach_activate_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let _service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        log::debug!("NFP::AttachActivateEvent called, device_handle={}", device_handle);

        // TODO: GetManager()->AttachActivateEvent(&out_event, device_handle)
        let event_handle = ctx.create_readable_event_handle(false).unwrap_or(0);

        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(event_handle);
    }

    /// AttachDeactivateEvent (cmd 18).
    /// Upstream: NfcInterface::AttachDeactivateEvent
    fn attach_deactivate_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let _service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        log::debug!("NFP::AttachDeactivateEvent called, device_handle={}", device_handle);

        // TODO: GetManager()->AttachDeactivateEvent(&out_event, device_handle)
        let event_handle = ctx.create_readable_event_handle(false).unwrap_or(0);

        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(event_handle);
    }

    /// GetState (cmd 19).
    /// Upstream: NfcInterface::GetState
    fn get_state_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        log::debug!("NFP::GetState called");

        let state = service.get_state();

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(state as u32);
    }

    /// GetDeviceState (cmd 20).
    /// Upstream: NfcInterface::GetDeviceState
    fn get_device_state_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        log::debug!("NFP::GetDeviceState called, device_handle={}", device_handle);

        let device_state = service.get_device_state(device_handle);

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(device_state as u32);
    }

    /// GetNpadId (cmd 21).
    /// Upstream: NfcInterface::GetNpadId
    fn get_npad_id_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        log::debug!("NFP::GetNpadId called, device_handle={}", device_handle);

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

    /// GetApplicationAreaSize (cmd 22).
    /// Upstream: Interface::GetApplicationAreaSize
    fn get_application_area_size_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        log::debug!("NFP::GetApplicationAreaSize called, device_handle={}", device_handle);

        let size = service.get_application_area_size(device_handle);

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(size);
    }

    /// AttachAvailabilityChangeEvent (cmd 23).
    /// Upstream: NfcInterface::AttachAvailabilityChangeEvent
    fn attach_availability_change_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let _service = Self::as_self(this);
        log::info!("NFP::AttachAvailabilityChangeEvent called");

        // TODO: GetManager()->AttachAvailabilityChangeEvent()
        let event_handle = ctx.create_readable_event_handle(false).unwrap_or(0);

        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(event_handle);
    }

    /// RecreateApplicationArea (cmd 24).
    /// Upstream: Interface::RecreateApplicationArea
    fn recreate_application_area_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let device_handle = rp.pop_u64();
        let access_id = rp.pop_u32();
        let data = ctx.read_buffer(0);
        log::info!(
            "NFP::RecreateApplicationArea called, device_handle={}, access_id={}, data_size={}",
            device_handle, access_id, data.len()
        );

        let result = service.recreate_application_area(device_handle, access_id, &data);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }
}

impl SessionRequestHandler for Interface {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        &self.name
    }
}

impl ServiceFramework for Interface {
    fn get_service_name(&self) -> &str {
        &self.name
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
