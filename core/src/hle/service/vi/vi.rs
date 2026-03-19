// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Stub VI services: vi:u, vi:s, vi:m

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use crate::hle::service::sm::sm::ServiceManager;

pub const VI_SERVICE_NAMES: &[&str] = &["vi:u", "vi:s", "vi:m"];

macro_rules! impl_service {
    ($name:ident) => {
        impl SessionRequestHandler for $name {
            fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
                ServiceFramework::handle_sync_request_impl(self, ctx)
            }
            fn service_name(&self) -> &str {
                ServiceFramework::get_service_name(self)
            }
        }
    };
}

// -- ViRootService (vi:u / vi:s / vi:m) --

pub struct ViRootService {
    name: String,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ViRootService {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            handlers: build_handler_map(&[
                (2, Some(Self::get_display_service), "GetDisplayService"),
                (3, Some(Self::get_display_service), "GetDisplayServiceWithProxyNameExchange"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn get_display_service(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("VI::GetDisplayService (STUBBED)");
        let sub: Arc<dyn SessionRequestHandler> = Arc::new(ViDisplayService::new());
        super::super::am::service::application_proxy::IApplicationProxy::push_interface_response(ctx, sub);
    }
}

impl_service!(ViRootService);
impl ServiceFramework for ViRootService {
    fn get_service_name(&self) -> &str { &self.name }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}

// -- ViDisplayService --

pub struct ViDisplayService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ViDisplayService {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (100, Some(Self::stub_sub_service), "GetRelayService"),
                (101, Some(Self::stub_sub_service), "GetSystemDisplayService"),
                (102, Some(Self::stub_sub_service), "GetManagerDisplayService"),
                (1010, Some(Self::open_display), "OpenDisplay"),
                (1020, Some(Self::stub_ok), "CloseDisplay"),
                (2020, Some(Self::open_layer), "OpenLayer"),
                (2021, Some(Self::stub_ok), "CloseLayer"),
                (2101, Some(Self::stub_ok), "SetLayerScalingMode"),
                (5202, Some(Self::get_display_vsync_event), "GetDisplayVsyncEvent"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn stub_sub_service(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("VI::stub_sub_service (STUBBED)");
        let sub: Arc<dyn SessionRequestHandler> = Arc::new(ViStubService::new("ViSubStub"));
        super::super::am::service::application_proxy::IApplicationProxy::push_interface_response(ctx, sub);
    }

    fn open_display(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("VI::OpenDisplay (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(1);
    }

    fn open_layer(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("VI::OpenLayer (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(0);
    }

    fn get_display_vsync_event(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("VI::GetDisplayVsyncEvent (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(0);
    }

    fn stub_ok(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl_service!(ViDisplayService);
impl ServiceFramework for ViDisplayService {
    fn get_service_name(&self) -> &str { "IApplicationDisplayService" }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}

// -- ViStubService --

pub struct ViStubService {
    name: String,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ViStubService {
    pub fn new(name: &str) -> Self {
        Self { name: name.to_string(), handlers: BTreeMap::new(), handlers_tipc: BTreeMap::new() }
    }
}

impl_service!(ViStubService);
impl ServiceFramework for ViStubService {
    fn get_service_name(&self) -> &str { &self.name }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}

// -- Registration --

/// Launches VI services.
///
/// Matches upstream `void VI::LoopProcess(Core::System& system, std::stop_token token)`.
pub fn loop_process(service_manager: &Arc<Mutex<ServiceManager>>) {
    let mut server_manager =
        crate::hle::service::server_manager::ServerManager::new(service_manager.clone());
    for name in VI_SERVICE_NAMES {
        let svc_name = name.to_string();
        server_manager.register_named_service(
            name,
            Box::new(move || -> Arc<dyn SessionRequestHandler> {
                Arc::new(ViRootService::new(&svc_name))
            }),
            64,
        );
    }
    crate::hle::service::server_manager::ServerManager::run_server(server_manager);
}

/// Backward-compatible alias.
pub fn register_services(service_manager: &Arc<Mutex<ServiceManager>>) {
    loop_process(service_manager);
}
