// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/

pub mod core;
pub mod devices;
pub mod nvdata;
pub mod nvdrv;
pub mod nvdrv_interface;
pub mod nvmemp;

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::result::RESULT_SUCCESS;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use crate::hle::service::sm::sm::ServiceManager;

const NVDRV_SERVICE_NAMES: &[&str] = &["nvdrv:a", "nvdrv:s", "nvdrv:t"];

/// Stub NVDRV service for initial bring-up.
pub struct NvdrvStubService {
    name: String,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl NvdrvStubService {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            handlers: build_handler_map(&[
                (0, Some(Self::open_handler), "Open"),
                (1, Some(Self::ioctl_handler), "Ioctl"),
                (2, Some(Self::close_handler), "Close"),
                (3, Some(Self::initialize_handler), "Initialize"),
                (4, Some(Self::query_event_handler), "QueryEvent"),
                (6, Some(Self::get_status_handler), "GetStatus"),
                (8, Some(Self::set_aruid_handler), "SetAruid"),
                (11, Some(Self::ioctl_handler), "Ioctl2"),
                (12, Some(Self::ioctl_handler), "Ioctl3"),
                (13, Some(Self::stub_handler), "SetGraphicsFirmwareMemoryMarginEnabled"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn open_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("NVDRV::Open (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0); // fd
        rb.push_u32(0); // NvResult::Success
    }

    fn ioctl_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("NVDRV::Ioctl (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0); // NvResult::Success
    }

    fn close_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("NVDRV::Close (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0);
    }

    fn initialize_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("NVDRV::Initialize (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0);
    }

    fn query_event_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("NVDRV::QueryEvent (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0);
        rb.push_copy_objects(0);
    }

    fn get_status_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("NVDRV::GetStatus (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0);
    }

    fn set_aruid_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("NVDRV::SetAruid (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0);
    }

    fn stub_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for NvdrvStubService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> crate::hle::result::ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        ServiceFramework::get_service_name(self)
    }
}

impl ServiceFramework for NvdrvStubService {
    fn get_service_name(&self) -> &str { &self.name }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}

/// Register nvdrv services with the service manager.
pub fn register_services(service_manager: &Arc<Mutex<ServiceManager>>) {
    for name in NVDRV_SERVICE_NAMES {
        let svc_name = name.to_string();
        let result = service_manager.lock().unwrap().register_service(
            svc_name.clone(),
            64,
            Box::new(move || -> Arc<dyn SessionRequestHandler> {
                Arc::new(NvdrvStubService::new(&svc_name))
            }),
        );
        if result.is_error() {
            log::warn!("Failed to register NVDRV service '{}'", name);
        }
    }
}
