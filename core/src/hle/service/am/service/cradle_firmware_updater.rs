// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/cradle_firmware_updater.h
//! Port of zuyu/src/core/hle/service/am/service/cradle_firmware_updater.cpp

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// Port of CradleDeviceInfo
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct CradleDeviceInfo {
    pub unknown0: bool,
    pub unknown1: bool,
    pub unknown2: bool,
    _pad: [u8; 5],
    pub unknown3: u64,
}
const _: () = assert!(std::mem::size_of::<CradleDeviceInfo>() == 0x10);

/// IPC command table for ICradleFirmwareUpdater:
/// - 0: StartUpdate
/// - 1: FinishUpdate
/// - 2: GetCradleDeviceInfo
/// - 3: GetCradleDeviceInfoChangeEvent
/// - 4: GetUpdateProgressInfo (unimplemented)
/// - 5: GetLastInternalResult (unimplemented)
pub struct ICradleFirmwareUpdater {
    service_context: crate::hle::service::kernel_helpers::ServiceContext,
    cradle_device_info_event_handle: u32,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ICradleFirmwareUpdater {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::start_update_handler), "StartUpdate"),
            (1, Some(Self::finish_update_handler), "FinishUpdate"),
            (
                2,
                Some(Self::get_cradle_device_info_handler),
                "GetCradleDeviceInfo",
            ),
            (
                3,
                Some(Self::get_cradle_device_info_change_event_handler),
                "GetCradleDeviceInfoChangeEvent",
            ),
            (4, None, "GetUpdateProgressInfo"),
            (5, None, "GetLastInternalResult"),
        ]);
        let mut service_context = crate::hle::service::kernel_helpers::ServiceContext::new(
            "ICradleFirmwareUpdater".to_string(),
        );
        let cradle_device_info_event_handle = service_context
            .create_event("ICradleFirmwareUpdater:CradleDeviceInfoChangeEvent".to_string());
        Self {
            service_context,
            cradle_device_info_event_handle,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Port of ICradleFirmwareUpdater::StartUpdate
    pub fn start_update(&self) {
        log::warn!("(STUBBED) StartUpdate called");
    }

    /// Port of ICradleFirmwareUpdater::FinishUpdate
    pub fn finish_update(&self) {
        log::warn!("(STUBBED) FinishUpdate called");
    }

    /// Port of ICradleFirmwareUpdater::GetCradleDeviceInfo
    pub fn get_cradle_device_info(&self) -> CradleDeviceInfo {
        log::warn!("(STUBBED) GetCradleDeviceInfo called");
        CradleDeviceInfo::default()
    }

    fn start_update_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const ICradleFirmwareUpdater) };
        service.start_update();

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn finish_update_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const ICradleFirmwareUpdater) };
        service.finish_update();

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_cradle_device_info_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const ICradleFirmwareUpdater) };
        let info = service.get_cradle_device_info();

        let mut rb = ResponseBuilder::new(ctx, 6, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&info);
    }

    /// Port of ICradleFirmwareUpdater::GetCradleDeviceInfoChangeEvent
    /// Upstream returns m_cradle_device_info_event.GetHandle() as a copy handle.
    fn get_cradle_device_info_change_event_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("(STUBBED) ICradleFirmwareUpdater::GetCradleDeviceInfoChangeEvent called");

        if let Some(handle) = ctx.create_readable_event_handle(false) {
            let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
            rb.push_result(RESULT_SUCCESS);
            rb.push_copy_objects(handle);
        }
    }
}

impl SessionRequestHandler for ICradleFirmwareUpdater {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }

    fn service_name(&self) -> &str {
        "am::ICradleFirmwareUpdater"
    }
}

impl ServiceFramework for ICradleFirmwareUpdater {
    fn get_service_name(&self) -> &str {
        "am::ICradleFirmwareUpdater"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
