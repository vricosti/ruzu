// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/vi.cpp/.h
//!
//! VI service registration: creates a shared Container and registers
//! vi:u, vi:s, vi:m root services.

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

use super::application_root_service::IApplicationRootService;
use super::container::Container;

pub const VI_SERVICE_NAMES: &[&str] = &["vi:u", "vi:s", "vi:m"];

// -- ViStubService (generic stub for sub-services like IHOSBinderDriver, etc.) --

pub struct ViStubService {
    name: String,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ViStubService {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            handlers: BTreeMap::new(),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for ViStubService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        ServiceFramework::get_service_name(self)
    }
}

impl ServiceFramework for ViStubService {
    fn get_service_name(&self) -> &str { &self.name }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}

// -- Stub root service for vi:s and vi:m (same pattern as IApplicationRootService
//    but with different command IDs) --

struct ViSystemRootService {
    container: Arc<Container>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ViSystemRootService {
    fn new(container: Arc<Container>) -> Self {
        Self {
            container,
            handlers: build_handler_map(&[
                (1, Some(Self::get_display_service), "GetDisplayService"),
                (3, None, "GetDisplayServiceWithProxyNameExchange"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn get_display_service(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let root = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        log::debug!("ISystemRootService::GetDisplayService called");
        let display_service = super::application_display_service::IApplicationDisplayService::new(
            Arc::clone(&root.container),
        );
        let sub: Arc<dyn SessionRequestHandler> = Arc::new(display_service);
        super::super::am::service::application_proxy::IApplicationProxy::push_interface_response(ctx, sub);
    }
}

impl SessionRequestHandler for ViSystemRootService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str { ServiceFramework::get_service_name(self) }
}

impl ServiceFramework for ViSystemRootService {
    fn get_service_name(&self) -> &str { "vi:s" }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}

struct ViManagerRootService {
    container: Arc<Container>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ViManagerRootService {
    fn new(container: Arc<Container>) -> Self {
        Self {
            container,
            handlers: build_handler_map(&[
                (2, Some(Self::get_display_service), "GetDisplayService"),
                (3, None, "GetDisplayServiceWithProxyNameExchange"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn get_display_service(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let root = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        log::debug!("IManagerRootService::GetDisplayService called");
        let display_service = super::application_display_service::IApplicationDisplayService::new(
            Arc::clone(&root.container),
        );
        let sub: Arc<dyn SessionRequestHandler> = Arc::new(display_service);
        super::super::am::service::application_proxy::IApplicationProxy::push_interface_response(ctx, sub);
    }
}

impl SessionRequestHandler for ViManagerRootService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str { ServiceFramework::get_service_name(self) }
}

impl ServiceFramework for ViManagerRootService {
    fn get_service_name(&self) -> &str { "vi:m" }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}

// -- Registration --

/// Launches VI services.
///
/// Matches upstream `void VI::LoopProcess(Core::System& system, std::stop_token token)`.
/// Creates a shared Container, then registers vi:u, vi:s, vi:m.
pub fn loop_process(system: crate::core::SystemRef) {
    let container = Arc::new(Container::new(system));

    let mut server_manager =
        crate::hle::service::server_manager::ServerManager::new(system);

    // vi:u — IApplicationRootService (cmd 0 = GetDisplayService)
    let container_u = Arc::clone(&container);
    server_manager.register_named_service(
        "vi:u",
        Box::new(move || -> Arc<dyn SessionRequestHandler> {
            Arc::new(IApplicationRootService::new(Arc::clone(&container_u)))
        }),
        64,
    );

    // vi:s — ISystemRootService (cmd 1 = GetDisplayService)
    let container_s = Arc::clone(&container);
    server_manager.register_named_service(
        "vi:s",
        Box::new(move || -> Arc<dyn SessionRequestHandler> {
            Arc::new(ViSystemRootService::new(Arc::clone(&container_s)))
        }),
        64,
    );

    // vi:m — IManagerRootService (cmd 2 = GetDisplayService)
    let container_m = Arc::clone(&container);
    server_manager.register_named_service(
        "vi:m",
        Box::new(move || -> Arc<dyn SessionRequestHandler> {
            Arc::new(ViManagerRootService::new(Arc::clone(&container_m)))
        }),
        64,
    );

    crate::hle::service::server_manager::ServerManager::run_server(server_manager);
}

/// Backward-compatible alias.
pub fn register_services(system: crate::core::SystemRef) {
    loop_process(system);
}
