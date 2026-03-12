// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nfp/nfp_interface.h
//! Port of zuyu/src/core/hle/service/nfp/nfp_interface.cpp
//!
//! Interface -- NFP interface for amiibo operations.
//! This is the concrete NFP service that extends NfcInterface (NFC base).

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use super::nfp_types::{
    BreakType, DeviceState, ModelType, MountTarget, WriteType,
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

/// Interface -- NFP service interface for amiibo.
///
/// Corresponds to `Interface` in upstream nfp_interface.h / nfp_interface.cpp.
/// Extends NfcInterface with NFP-specific methods (amiibo data management).
pub struct Interface {
    name: String,
    device_state: DeviceState,
    // In upstream this holds a DeviceManager reference
}

impl Interface {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            device_state: DeviceState::Initialized,
        }
    }

    /// InitializeSystem / InitializeDebug -> delegates to Initialize.
    ///
    /// Corresponds to `Interface::InitializeSystem` / `Interface::InitializeDebug`.
    pub fn initialize(&mut self) -> ResultCode {
        log::info!("NFP::Interface({})::initialize called", self.name);
        // TODO: initialize device manager
        RESULT_SUCCESS
    }

    /// Finalize / FinalizeSystem / FinalizeDebug.
    ///
    /// Corresponds to `Interface::Finalize` in upstream.
    pub fn finalize(&mut self) -> ResultCode {
        log::info!("NFP::Interface({})::finalize called", self.name);
        // TODO: finalize device manager
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

    /// GetDeviceState (cmd 20).
    pub fn get_device_state(&self) -> DeviceState {
        self.device_state
    }
}
