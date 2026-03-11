// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/sm/sm.h and sm.cpp
//! Status: Structural port
//!
//! Contains:
//! - SM: the "sm:" service interface
//! - ServiceManager: manages service registration and lookup
//! - loop_process: entry point for running SM services
//!
//! Result codes (matching upstream):
//! - ResultInvalidClient (ErrorModule::SM, 2)
//! - ResultAlreadyRegistered (ErrorModule::SM, 4)
//! - ResultInvalidServiceName (ErrorModule::SM, 6)
//! - ResultNotRegistered (ErrorModule::SM, 7)

use std::collections::{BTreeMap, HashMap};
use std::sync::{Arc, Mutex};

use crate::hle::result::{ErrorModule, ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerFactory,
    SessionRequestHandlerPtr,
};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::server_manager::ServerManager;
use crate::hle::service::service::{
    build_handler_map, FunctionInfo, ServiceFramework, SERVER_SESSION_COUNT_MAX,
};
use crate::hle::service::sm::sm_controller::Controller;

// --- SM result codes ---

pub const RESULT_INVALID_CLIENT: ResultCode =
    ResultCode::from_module_description(ErrorModule::SM, 2);
pub const RESULT_ALREADY_REGISTERED: ResultCode =
    ResultCode::from_module_description(ErrorModule::SM, 4);
pub const RESULT_INVALID_SERVICE_NAME: ResultCode =
    ResultCode::from_module_description(ErrorModule::SM, 6);
pub const RESULT_NOT_REGISTERED: ResultCode =
    ResultCode::from_module_description(ErrorModule::SM, 7);

// --- ServiceManager ---

/// Manages service registration, lookup, and the controller interface.
///
/// Corresponds to upstream `Service::SM::ServiceManager`.
pub struct ServiceManager {
    controller_interface: Controller,

    /// Map of registered services.
    registered_services: HashMap<String, SessionRequestHandlerFactory>,

    /// Map of service ports (handles). In the full implementation these are KClientPort*.
    service_ports: HashMap<String, u32>,

    // TODO: deferral_event when kernel integration is ready
}

impl ServiceManager {
    pub fn new() -> Self {
        Self {
            controller_interface: Controller::new(),
            registered_services: HashMap::new(),
            service_ports: HashMap::new(),
        }
    }

    /// Invokes a control request on the controller interface.
    pub fn invoke_control_request(&self, ctx: &mut HLERequestContext) {
        self.controller_interface.invoke_request(ctx);
    }

    /// Validates a service name.
    fn validate_service_name(name: &str) -> ResultCode {
        if name.is_empty() || name.len() > 8 {
            log::error!("Invalid service name! service={}", name);
            return RESULT_INVALID_SERVICE_NAME;
        }
        RESULT_SUCCESS
    }

    /// Registers a service.
    ///
    /// Corresponds to upstream `ServiceManager::RegisterService`.
    pub fn register_service(
        &mut self,
        name: String,
        _max_sessions: u32,
        handler: SessionRequestHandlerFactory,
    ) -> ResultCode {
        let validate_result = Self::validate_service_name(&name);
        if validate_result.is_error() {
            return validate_result;
        }

        if self.registered_services.contains_key(&name) {
            log::error!("Service is already registered! service={}", name);
            return RESULT_ALREADY_REGISTERED;
        }

        // TODO: create KPort, register with kernel
        // For now, use a dummy port handle.
        let port_handle = self.service_ports.len() as u32 + 1;
        self.service_ports.insert(name.clone(), port_handle);
        self.registered_services.insert(name, handler);

        RESULT_SUCCESS
    }

    /// Unregisters a service.
    ///
    /// Corresponds to upstream `ServiceManager::UnregisterService`.
    pub fn unregister_service(&mut self, name: &str) -> ResultCode {
        let validate_result = Self::validate_service_name(name);
        if validate_result.is_error() {
            return validate_result;
        }

        if self.registered_services.remove(name).is_none() {
            log::error!("Server is not registered! service={}", name);
            return RESULT_NOT_REGISTERED;
        }
        self.service_ports.remove(name);

        RESULT_SUCCESS
    }

    /// Gets a service port by name.
    ///
    /// Corresponds to upstream `ServiceManager::GetServicePort`.
    pub fn get_service_port(&self, name: &str) -> Result<u32, ResultCode> {
        let validate_result = Self::validate_service_name(name);
        if validate_result.is_error() {
            return Err(validate_result);
        }

        match self.service_ports.get(name) {
            Some(&port) => Ok(port),
            None => {
                log::warn!("Server is not registered! service={}", name);
                Err(RESULT_NOT_REGISTERED)
            }
        }
    }

    /// Gets a service handler by name.
    pub fn get_service(&self, name: &str) -> Option<SessionRequestHandlerPtr> {
        self.registered_services.get(name).map(|factory| factory())
    }
}

// --- SM service ---

/// Interface to "sm:" service.
///
/// Corresponds to upstream `Service::SM::SM`.
pub struct Sm {
    service_manager: Arc<Mutex<ServiceManager>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl Sm {
    pub fn new(service_manager: Arc<Mutex<ServiceManager>>) -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Sm::initialize_handler), "Initialize"),
            (1, Some(Sm::get_service_cmif_handler), "GetService"),
            (2, Some(Sm::register_service_cmif_handler), "RegisterService"),
            (3, Some(Sm::unregister_service_handler), "UnregisterService"),
            (4, None, "DetachClient"),
        ]);
        let handlers_tipc = build_handler_map(&[
            (0, Some(Sm::initialize_handler), "Initialize"),
            (1, Some(Sm::get_service_tipc_handler), "GetService"),
            (
                2,
                Some(Sm::register_service_tipc_handler),
                "RegisterService",
            ),
            (3, Some(Sm::unregister_service_handler), "UnregisterService"),
            (4, None, "DetachClient"),
        ]);

        Self {
            service_manager,
            handlers,
            handlers_tipc,
        }
    }

    // --- Handler trampolines (fn pointers that downcast from &dyn ServiceFramework) ---

    fn initialize_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        // Downcast is safe because we know the concrete type.
        let sm = unsafe { &*(this as *const dyn ServiceFramework as *const Sm) };
        sm.initialize(ctx);
    }

    fn get_service_cmif_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let sm = unsafe { &*(this as *const dyn ServiceFramework as *const Sm) };
        sm.get_service_cmif(ctx);
    }

    fn get_service_tipc_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let sm = unsafe { &*(this as *const dyn ServiceFramework as *const Sm) };
        sm.get_service_tipc(ctx);
    }

    fn register_service_cmif_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let sm = unsafe { &*(this as *const dyn ServiceFramework as *const Sm) };
        sm.register_service_cmif(ctx);
    }

    fn register_service_tipc_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let sm = unsafe { &*(this as *const dyn ServiceFramework as *const Sm) };
        sm.register_service_tipc(ctx);
    }

    fn unregister_service_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let sm = unsafe { &*(this as *const dyn ServiceFramework as *const Sm) };
        sm.unregister_service(ctx);
    }

    // --- Actual handler implementations ---

    /// SM::Initialize service function.
    fn initialize(&self, ctx: &mut HLERequestContext) {
        log::debug!("SM::Initialize called");

        if let Some(manager) = ctx.get_manager() {
            manager.lock().unwrap().set_is_initialized_for_sm();
        }

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// SM::GetService (CMIF variant).
    fn get_service_cmif(&self, ctx: &mut HLERequestContext) {
        let result = self.get_service_impl(ctx);
        if ctx.get_is_deferred() {
            return;
        }

        if result.is_success() {
            // In the full implementation, push the client session as a move handle.
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(result);
        } else {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(result);
        }
    }

    /// SM::GetService (TIPC variant).
    fn get_service_tipc(&self, ctx: &mut HLERequestContext) {
        let result = self.get_service_impl(ctx);
        if ctx.get_is_deferred() {
            return;
        }

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    /// Pop service name from request: 8 bytes of ASCII, printable characters only.
    fn pop_service_name(rp: &mut RequestParser) -> String {
        let name_buf: [u8; 8] = rp.pop_raw();
        let mut result = String::new();
        for &c in &name_buf {
            if c >= b' ' && c <= b'~' {
                result.push(c as char);
            }
        }
        result
    }

    /// Internal implementation for GetService.
    fn get_service_impl(&self, ctx: &mut HLERequestContext) -> ResultCode {
        if let Some(manager) = ctx.get_manager() {
            if !manager.lock().unwrap().get_is_initialized_for_sm() {
                return RESULT_INVALID_CLIENT;
            }
        }

        let mut rp = RequestParser::new(ctx);
        let name = Self::pop_service_name(&mut rp);

        let sm = self.service_manager.lock().unwrap();
        match sm.get_service_port(&name) {
            Err(e) if e == RESULT_INVALID_SERVICE_NAME => {
                log::error!("Invalid service name '{}'", name);
                RESULT_INVALID_SERVICE_NAME
            }
            Err(_) => {
                log::info!("Waiting for service {} to become available", name);
                ctx.set_is_deferred();
                RESULT_NOT_REGISTERED
            }
            Ok(_port) => {
                // TODO: create a session via the client port
                RESULT_SUCCESS
            }
        }
    }

    /// SM::RegisterService (CMIF variant).
    fn register_service_cmif(&self, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let name = Self::pop_service_name(&mut rp);
        let _is_light = rp.pop_u32() != 0;
        let max_session_count = rp.pop_u32();

        self.register_service_impl(ctx, name, max_session_count, _is_light);
    }

    /// SM::RegisterService (TIPC variant).
    fn register_service_tipc(&self, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let name = Self::pop_service_name(&mut rp);
        let max_session_count = rp.pop_u32();
        let _is_light = rp.pop_u32() != 0;

        self.register_service_impl(ctx, name, max_session_count, _is_light);
    }

    /// Internal implementation for RegisterService.
    fn register_service_impl(
        &self,
        ctx: &mut HLERequestContext,
        name: String,
        max_session_count: u32,
        is_light: bool,
    ) {
        log::debug!(
            "SM::RegisterService called with name={}, max_session_count={}, is_light={}",
            name,
            max_session_count,
            is_light
        );

        let result = {
            let mut sm = self.service_manager.lock().unwrap();
            // Register with a null factory (matching upstream passing nullptr).
            let factory: SessionRequestHandlerFactory =
                Box::new(|| -> SessionRequestHandlerPtr { panic!("null factory called") });
            sm.register_service(name, max_session_count, factory)
        };

        if result.is_error() {
            log::error!(
                "failed to register service with error_code={:08X}",
                result.get_inner_value()
            );
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(result);
            return;
        }

        // TODO: push the server port as a move handle
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// SM::UnregisterService.
    fn unregister_service(&self, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let name = Self::pop_service_name(&mut rp);

        log::debug!("SM::UnregisterService called with name={}", name);

        let result = self.service_manager.lock().unwrap().unregister_service(&name);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }
}

impl SessionRequestHandler for Sm {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        self.handle_sync_request_impl(ctx)
    }

    fn service_name(&self) -> &str {
        "sm:"
    }
}

impl ServiceFramework for Sm {
    fn get_service_name(&self) -> &str {
        "sm:"
    }

    fn get_max_sessions(&self) -> u32 {
        4
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// Runs SM services.
///
/// Corresponds to upstream `Service::SM::LoopProcess`.
pub fn loop_process() {
    let service_manager = Arc::new(Mutex::new(ServiceManager::new()));
    let mut server_manager = ServerManager::new();

    // TODO: manage deferral event
    // server_manager.manage_deferral();
    // service_manager.lock().unwrap().set_deferral_event(event);

    let sm_service = Arc::new(Sm::new(service_manager.clone()));
    let sm_clone = sm_service.clone();
    let factory: SessionRequestHandlerFactory = Box::new(move || sm_clone.clone());
    server_manager.manage_named_port("sm:", factory, 64);

    // TODO: ServerManager::RunServer(server_manager);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_result_codes() {
        assert!(RESULT_INVALID_CLIENT.is_error());
        assert_eq!(RESULT_INVALID_CLIENT.get_module(), ErrorModule::SM);
        assert_eq!(RESULT_INVALID_CLIENT.get_description(), 2);

        assert!(RESULT_ALREADY_REGISTERED.is_error());
        assert_eq!(RESULT_ALREADY_REGISTERED.get_description(), 4);

        assert!(RESULT_INVALID_SERVICE_NAME.is_error());
        assert_eq!(RESULT_INVALID_SERVICE_NAME.get_description(), 6);

        assert!(RESULT_NOT_REGISTERED.is_error());
        assert_eq!(RESULT_NOT_REGISTERED.get_description(), 7);
    }

    #[test]
    fn test_service_manager_register_unregister() {
        let mut sm = ServiceManager::new();

        let factory: SessionRequestHandlerFactory =
            Box::new(|| -> SessionRequestHandlerPtr { panic!("test") });
        let result = sm.register_service("test_svc".to_string(), 64, factory);
        assert!(result.is_success());

        // Double registration should fail.
        let factory2: SessionRequestHandlerFactory =
            Box::new(|| -> SessionRequestHandlerPtr { panic!("test") });
        let result2 = sm.register_service("test_svc".to_string(), 64, factory2);
        assert_eq!(result2, RESULT_ALREADY_REGISTERED);

        // Unregister.
        let result3 = sm.unregister_service("test_svc");
        assert!(result3.is_success());

        // Unregister again should fail.
        let result4 = sm.unregister_service("test_svc");
        assert_eq!(result4, RESULT_NOT_REGISTERED);
    }

    #[test]
    fn test_validate_service_name() {
        assert!(ServiceManager::validate_service_name("sm:").is_success());
        assert!(ServiceManager::validate_service_name("12345678").is_success());
        assert!(ServiceManager::validate_service_name("").is_error());
        assert!(ServiceManager::validate_service_name("123456789").is_error());
    }
}
