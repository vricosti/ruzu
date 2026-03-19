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

use crate::hle::kernel::svc::svc_results::RESULT_INVALID_STATE;
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
    controller_interface: Arc<Controller>,

    /// Map of registered services.
    registered_services: HashMap<String, SessionRequestHandlerFactory>,

    /// Map of service ports (handles). In the full implementation these are KClientPort*.
    service_ports: HashMap<String, u32>,

    /// HLE server manager backing session registration.
    /// Matches upstream ownership where session clones are registered through
    /// ServerManager::RegisterSession.
    server_manager: Arc<Mutex<ServerManager>>,

    // TODO: deferral_event when kernel integration is ready
}

impl ServiceManager {
    pub fn new() -> Self {
        Self {
            controller_interface: Arc::new(Controller::new()),
            registered_services: HashMap::new(),
            service_ports: HashMap::new(),
            server_manager: Arc::new(Mutex::new(ServerManager::new())),
        }
    }

    /// Invokes a control request on the controller interface.
    pub fn invoke_control_request(&self, ctx: &mut HLERequestContext) {
        self.controller_interface.invoke_request(ctx);
    }

    /// Returns the shared IPC controller.
    pub fn controller_interface(&self) -> Arc<Controller> {
        self.controller_interface.clone()
    }

    pub fn server_manager(&self) -> Arc<Mutex<ServerManager>> {
        self.server_manager.clone()
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
        let (result, session_handle) = self.get_service_impl(ctx);
        if ctx.get_is_deferred() {
            return;
        }

        if result.is_success() {
            // Push the client session as a move handle.
            // Matches upstream: rb{ctx, 2, 0, 1, AlwaysMoveHandles}; rb.PushMoveObjects(session)
            let mut rb = ResponseBuilder::new_with_flags(
                ctx, 2, 0, 1,
                crate::hle::service::ipc_helpers::ResponseBuilderFlags::AlwaysMoveHandles,
            );
            rb.push_result(result);
            rb.push_move_objects(session_handle.unwrap_or(0));
        } else {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(result);
        }
    }

    /// SM::GetService (TIPC variant).
    fn get_service_tipc(&self, ctx: &mut HLERequestContext) {
        let (result, session_handle) = self.get_service_impl(ctx);
        if ctx.get_is_deferred() {
            return;
        }

        let mut rb = ResponseBuilder::new_with_flags(
            ctx, 2, 0, 1,
            crate::hle::service::ipc_helpers::ResponseBuilderFlags::AlwaysMoveHandles,
        );
        rb.push_result(result);
        rb.push_move_objects(if result.is_success() { session_handle.unwrap_or(0) } else { 0 });
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
    /// Returns (result_code, optional session handle).
    ///
    /// Matches upstream `SM::GetServiceImpl(KClientSession**, HLERequestContext&)`.
    fn get_service_impl(&self, ctx: &mut HLERequestContext) -> (ResultCode, Option<u32>) {
        if let Some(manager) = ctx.get_manager() {
            if !manager.lock().unwrap().get_is_initialized_for_sm() {
                log::warn!("  GetService: client not initialized for SM");
                return (RESULT_INVALID_CLIENT, None);
            }
        }

        let mut rp = RequestParser::new(ctx);
        let name = Self::pop_service_name(&mut rp);
        log::info!("  GetService: looking up \"{}\"", name);

        let sm = self.service_manager.lock().unwrap();
        match sm.get_service_port(&name) {
            Err(e) if e == RESULT_INVALID_SERVICE_NAME => {
                log::error!("Invalid service name '{}'", name);
                (RESULT_INVALID_SERVICE_NAME, None)
            }
            Err(_) => {
                log::error!("GetService: service '{}' not registered!", name);
                ctx.set_is_deferred();
                (RESULT_NOT_REGISTERED, None)
            }
            Ok(_port) => {
                // Create a session for the service handler.
                // Matches upstream: client_port->CreateSession(&session)
                let handler = sm.get_service(&name);
                drop(sm); // Release ServiceManager lock before accessing process

                if let Some(handler) = handler {
                    if let Some(handle) = ctx.create_session_for_service(handler) {
                        log::info!("  GetService(\"{}\") -> handle={:#x}", name, handle);
                        (RESULT_SUCCESS, Some(handle))
                    } else {
                        log::error!("  GetService(\"{}\"): failed to create session", name);
                        (RESULT_INVALID_STATE, None)
                    }
                } else {
                    log::error!("  GetService(\"{}\"): service handler not found", name);
                    (RESULT_INVALID_STATE, None)
                }
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

    fn service_manager(&self) -> Option<Arc<Mutex<ServiceManager>>> {
        Some(self.service_manager.clone())
    }
}

/// Runs SM services.
///
/// Corresponds to upstream `Service::SM::LoopProcess`.
pub fn loop_process() {
    let service_manager = create_service_manager();
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

/// Creates the process-external SM service registry and registers the "sm:" named port.
///
/// This is the current Rust-side owner for the subset of upstream system service bootstrap
/// needed by kernel IPC bring-up. Additional named services can be registered on the returned
/// manager as more HLE service processes are wired up.
pub fn create_service_manager() -> Arc<Mutex<ServiceManager>> {
    let service_manager = Arc::new(Mutex::new(ServiceManager::new()));

    // Register sm: service.
    let sm_service = Arc::new(Sm::new(service_manager.clone()));
    let sm_clone = sm_service.clone();
    let factory: SessionRequestHandlerFactory = Box::new(move || sm_clone.clone());
    let result = service_manager
        .lock()
        .unwrap()
        .register_service("sm:".to_string(), 64, factory);
    assert!(result.is_success(), "failed to register sm: service bootstrap");

    // Register core HLE services that games expect.
    // Matches upstream Services::InstallInterfaces() service registration.
    register_stub_service(&service_manager, "lm", || {
        Arc::new(crate::hle::service::lm::lm::LM::new())
    });
    crate::hle::service::apm::apm::register_services(&service_manager);
    crate::hle::service::pctl::pctl::register_services(&service_manager);
    crate::hle::service::filesystem::filesystem::register_services(&service_manager);
    crate::hle::service::aoc::addon_content_manager::loop_process(&service_manager);
    crate::hle::service::am::am::register_services(&service_manager);
    crate::hle::service::vi::vi::register_services(&service_manager);
    crate::hle::service::nvdrv::register_services(&service_manager);

    // Register all system services that games expect during SDK init.
    // Matches upstream Services::InstallInterfaces() order.
    // These are stub services that accept any IPC command and return success.
    // Games may access them during nn::* initialization.
    let system_services = &[
        // Settings (set:*)
        "set", "set:cal", "set:fd", "set:sys",
        // HID (hid, hid:dbg, hid:sys, hidbus, irs, irs:sys, xcd:sys)
        "hid", "hid:dbg", "hid:sys", "hidbus", "irs", "irs:sys", "xcd:sys",
        // Account
        "acc:u0", "acc:u1",
        // Audio (audout:u, audin:u, audren:u, audctl, hwopus)
        "audout:u", "audin:u", "audren:u", "audctl", "hwopus",
        // NS
        "ns:su", "ns:am2", "ns:ec", "ns:rid", "ns:rt", "ns:web", "ns:ro",
        // PSC / Time
        "psc:c", "psc:m",
        // Glue (arp, bgtc, ectx, notif)
        "arp:r", "arp:w", "bgtc:t", "bgtc:sc", "ectx:aw",
        "notif:a", "notif:s",
        // Friends
        "friend:u", "friend:v", "friend:m", "friend:s", "friend:a",
        // NIFM (network)
        "nifm:u", "nifm:a", "nifm:s",
        // Mii
        "mii:u", "mii:e",
        // NVNflinger
        "dispdrv",
        // Others commonly needed
        "fatal:u", "lbl", "mm:u",
        "nfc:user", "nfc:sys", "nfp:user", "nfp:sys",
        "spl:", "spl:mig", "spl:fs", "spl:ssl", "spl:es", "spl:manu",
        "ssl", "nim:shp", "erpt:c", "erpt:r",
        "bsd:u", "bsd:s", "bsdcfg", "nsd:u", "nsd:a", "sfdnsres",
        "csrng", "btdrv", "btm",
        "pcv", "caps:a", "caps:c", "caps:u", "caps:ss", "caps:sc",
        "pl:u", "prepo:u", "prepo:s", "prepo:m", "prepo:a",
        "eupld:c", "eupld:r",
        "ovln:rcv", "ovln:snd",
        "pm:shell", "pm:dmnt", "pm:info",
        "lr", "ncm",
        "ldr:pm", "ldr:shel", "ldr:dmnt",
        "ro:1",
        "usb:ds", "usb:hs", "usb:pm",
        "grc:c", "grc:d",
        "olsc:u",
        "ngc:u",
    ];

    for name in system_services {
        let svc_name = name.to_string();
        register_stub_service(&service_manager, name, move || {
            Arc::new(GenericStubService::new(&svc_name))
        });
    }

    // Register time services with real Glue::Time::StaticService instances.
    // Matches upstream Services::InstallInterfaces() which creates time services
    // with appropriate StaticServiceSetupInfo permissions.
    {
        use crate::hle::service::glue::time::r#static::StaticService as GlueTimeStaticService;
        use crate::hle::service::psc::time::common::StaticServiceSetupInfo;

        // time:u — user variant (all writes false)
        let user_setup = StaticServiceSetupInfo {
            can_write_local_clock: false,
            can_write_user_clock: false,
            can_write_network_clock: false,
            can_write_timezone_device_location: false,
            can_write_steady_clock: false,
            can_write_uninitialized_clock: false,
        };
        register_stub_service(&service_manager, "time:u", move || {
            Arc::new(GlueTimeStaticService::new(user_setup, "time:u"))
        });

        // time:s — admin variant
        let admin_setup = StaticServiceSetupInfo {
            can_write_local_clock: true,
            can_write_user_clock: true,
            can_write_network_clock: false,
            can_write_timezone_device_location: true,
            can_write_steady_clock: false,
            can_write_uninitialized_clock: false,
        };
        register_stub_service(&service_manager, "time:s", move || {
            Arc::new(GlueTimeStaticService::new(admin_setup, "time:s"))
        });

        // time:a — admin variant (same permissions as time:s)
        let admin_setup_a = StaticServiceSetupInfo {
            can_write_local_clock: true,
            can_write_user_clock: true,
            can_write_network_clock: false,
            can_write_timezone_device_location: true,
            can_write_steady_clock: false,
            can_write_uninitialized_clock: false,
        };
        register_stub_service(&service_manager, "time:a", move || {
            Arc::new(GlueTimeStaticService::new(admin_setup_a, "time:a"))
        });
    }

    service_manager
}

/// Generic stub service that accepts any IPC command and returns success.
/// Used for services that aren't fully implemented yet but need to exist
/// so that the game's SDK init doesn't abort.
pub struct GenericStubService {
    name: String,
    handlers: std::collections::BTreeMap<u32, crate::hle::service::service::FunctionInfo>,
    handlers_tipc: std::collections::BTreeMap<u32, crate::hle::service::service::FunctionInfo>,
}

impl GenericStubService {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            handlers: std::collections::BTreeMap::new(),
            handlers_tipc: std::collections::BTreeMap::new(),
        }
    }
}

impl crate::hle::service::hle_ipc::SessionRequestHandler for GenericStubService {
    fn handle_sync_request(
        &self,
        ctx: &mut crate::hle::service::hle_ipc::HLERequestContext,
    ) -> crate::hle::result::ResultCode {
        // Log the unhandled command and return success.
        log::warn!(
            "GenericStubService({}): unhandled command, returning success",
            self.name
        );
        let mut rb = crate::hle::service::ipc_helpers::ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(crate::hle::result::RESULT_SUCCESS);
        crate::hle::result::RESULT_SUCCESS
    }

    fn service_name(&self) -> &str {
        &self.name
    }
}

impl crate::hle::service::service::ServiceFramework for GenericStubService {
    fn get_service_name(&self) -> &str {
        &self.name
    }

    fn handlers(
        &self,
    ) -> &std::collections::BTreeMap<u32, crate::hle::service::service::FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(
        &self,
    ) -> &std::collections::BTreeMap<u32, crate::hle::service::service::FunctionInfo> {
        &self.handlers_tipc
    }
}

/// Helper to register a service with a factory closure.
fn register_stub_service<F>(sm: &Arc<Mutex<ServiceManager>>, name: &str, factory: F)
where
    F: Fn() -> SessionRequestHandlerPtr + Send + Sync + 'static,
{
    let result = sm
        .lock()
        .unwrap()
        .register_service(name.to_string(), 64, Box::new(factory));
    if result.is_error() {
        log::warn!("Failed to register service '{}': {:#x}", name, result.get_inner_value());
    }
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
