// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/sm/sm.h and sm.cpp
//!
//! Contains:
//! - ServiceManager: manages service registration and lookup
//! - SM: the "sm:" service interface (ServiceFramework)
//! - LoopProcess: entry point matching upstream `SM::LoopProcess(Core::System&)`
//!
//! Result codes (matching upstream):
//! - ResultInvalidClient (ErrorModule::SM, 2)
//! - ResultAlreadyRegistered (ErrorModule::SM, 4)
//! - ResultInvalidServiceName (ErrorModule::SM, 6)
//! - ResultNotRegistered (ErrorModule::SM, 7)

use std::collections::{BTreeMap, HashMap};
use std::sync::{Arc, Mutex};

use crate::hle::kernel::k_port::KPort;
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

// --- SM result codes (matching upstream sm.cpp) ---

pub const RESULT_INVALID_CLIENT: ResultCode =
    ResultCode::from_module_description(ErrorModule::SM, 2);
pub const RESULT_ALREADY_REGISTERED: ResultCode =
    ResultCode::from_module_description(ErrorModule::SM, 4);
pub const RESULT_INVALID_SERVICE_NAME: ResultCode =
    ResultCode::from_module_description(ErrorModule::SM, 6);
pub const RESULT_NOT_REGISTERED: ResultCode =
    ResultCode::from_module_description(ErrorModule::SM, 7);

// --- ServiceManager ---
//
// Matches upstream `Service::SM::ServiceManager` (sm.h lines 56-103).

/// Manages service registration, lookup, and the controller interface.
///
/// Upstream constructor: `ServiceManager::ServiceManager(Kernel::KernelCore& kernel_)`.
/// Upstream stores: `kernel`, `controller_interface`, `registered_services`,
/// `service_ports`, `deferral_event`.
pub struct ServiceManager {
    controller_interface: Arc<Controller>,

    /// Map of registered services (name → handler factory).
    /// Matches upstream `std::unordered_map<std::string, SessionRequestHandlerFactory>`.
    registered_services: HashMap<String, SessionRequestHandlerFactory>,

    /// Map of service ports (name → KPort).
    /// Matches upstream `std::unordered_map<std::string, Kernel::KClientPort*>`.
    /// Upstream stores `KClientPort*` (obtained via `&port->GetClientPort()`),
    /// but the `KPort` owns both endpoints. We store the whole `KPort` wrapped
    /// in `Arc<Mutex<>>` so the caller can access both client and server sides.
    service_ports: HashMap<String, Arc<Mutex<KPort>>>,

    /// Deferral event for service registration.
    /// Upstream: `Kernel::KEvent* deferral_event{}`.
    deferral_event: Option<Arc<crate::hle::service::os::event::Event>>,
}

impl ServiceManager {
    /// Creates a new ServiceManager.
    ///
    /// Matches upstream `ServiceManager::ServiceManager(Kernel::KernelCore& kernel_)`.
    /// Upstream also creates the Controller interface here.
    pub fn new() -> Self {
        Self {
            controller_interface: Arc::new(Controller::new()),
            registered_services: HashMap::new(),
            service_ports: HashMap::new(),
            deferral_event: None,
        }
    }

    /// Invokes a control request on the controller interface.
    ///
    /// Matches upstream `ServiceManager::InvokeControlRequest(HLERequestContext&)`.
    pub fn invoke_control_request(&self, ctx: &mut HLERequestContext) {
        self.controller_interface.invoke_request(ctx);
    }

    /// Returns the shared IPC controller.
    pub fn controller_interface(&self) -> Arc<Controller> {
        self.controller_interface.clone()
    }

    /// Sets the deferral event.
    ///
    /// Matches upstream `ServiceManager::SetDeferralEvent(Kernel::KEvent*)`.
    pub fn set_deferral_event(
        &mut self,
        event: Option<Arc<crate::hle::service::os::event::Event>>,
    ) {
        self.deferral_event = event;
    }

    /// Registers a service.
    ///
    /// Matches upstream `ServiceManager::RegisterService(KServerPort**, name, max_sessions, handler)`:
    /// ```cpp
    /// auto* port = Kernel::KPort::Create(kernel);
    /// port->Initialize(ServerSessionCountMax, false, 0);
    /// Kernel::KPort::Register(kernel, port);
    /// service_ports.emplace(name, std::addressof(port->GetClientPort()));
    /// registered_services.emplace(name, handler);
    /// if (deferral_event) { deferral_event->Signal(); }
    /// *out_server_port = std::addressof(port->GetServerPort());
    /// ```
    pub fn register_service(
        &mut self,
        name: String,
        max_sessions: u32,
        handler: SessionRequestHandlerFactory,
    ) -> ResultCode {
        let validate_result = validate_service_name(&name);
        if validate_result.is_error() {
            return validate_result;
        }

        if self.registered_services.contains_key(&name) {
            log::error!("Service is already registered! service={}", name);
            return RESULT_ALREADY_REGISTERED;
        }

        // Create and initialize a KPort (matching upstream).
        let mut port = KPort::new();
        port.initialize(max_sessions as i32, false, 0);

        // Store the port and handler factory.
        // Upstream stores &port->GetClientPort() in service_ports; we store
        // the whole KPort since we own it (no slab allocator yet).
        self.service_ports
            .insert(name.clone(), Arc::new(Mutex::new(port)));
        self.registered_services.insert(name, handler);

        // Signal deferral event so waiting GetService requests can retry.
        if let Some(ref event) = self.deferral_event {
            event.signal();
            log::debug!("ServiceManager: deferral event signaled after registration");
        }

        RESULT_SUCCESS
    }

    /// Unregisters a service.
    ///
    /// Matches upstream `ServiceManager::UnregisterService(const std::string& name)`.
    pub fn unregister_service(&mut self, name: &str) -> ResultCode {
        let validate_result = validate_service_name(name);
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
    /// Matches upstream `ServiceManager::GetServicePort(KClientPort**, const std::string& name)`.
    /// Returns the `Arc<Mutex<KPort>>` which contains both client and server endpoints.
    pub fn get_service_port(&self, name: &str) -> Result<Arc<Mutex<KPort>>, ResultCode> {
        let validate_result = validate_service_name(name);
        if validate_result.is_error() {
            return Err(validate_result);
        }

        match self.service_ports.get(name) {
            Some(port) => Ok(port.clone()),
            None => {
                log::warn!("Server is not registered! service={}", name);
                Err(RESULT_NOT_REGISTERED)
            }
        }
    }

    /// Gets a service handler by name, invoking the factory.
    ///
    /// Matches upstream `ServiceManager::GetService<T>(const std::string& name, bool block)`
    /// with `block=false`.
    pub fn get_service(&self, name: &str) -> Option<SessionRequestHandlerPtr> {
        self.registered_services.get(name).map(|factory| factory())
    }

    /// Gets a service handler by name, blocking until registered.
    ///
    /// Matches upstream `ServiceManager::GetService<T>(name, true)` which polls
    /// every 100ms until the service appears.
    pub fn get_service_blocking(
        sm: &Arc<std::sync::Mutex<Self>>,
        name: &str,
    ) -> SessionRequestHandlerPtr {
        loop {
            if let Some(handler) = sm.lock().unwrap().get_service(name) {
                return handler;
            }
            std::thread::sleep(std::time::Duration::from_millis(100));
        }
    }
}

impl Drop for ServiceManager {
    /// Matches upstream `ServiceManager::~ServiceManager()` which closes ports and deferral event.
    fn drop(&mut self) {
        // Upstream: for (auto& [name, port] : service_ports) { port->Close(); }
        // Close each port (transition to closed state).
        for (_name, port) in self.service_ports.drain() {
            port.lock().unwrap().on_server_closed();
        }
        self.registered_services.clear();
        self.deferral_event = None;
    }
}

/// Validates a service name (1-8 characters).
///
/// Matches upstream `static Result ValidateServiceName(const std::string& name)`.
fn validate_service_name(name: &str) -> ResultCode {
    if name.is_empty() || name.len() > 8 {
        log::error!("Invalid service name! service={}", name);
        return RESULT_INVALID_SERVICE_NAME;
    }
    RESULT_SUCCESS
}

// --- SM service ---
//
// Matches upstream `Service::SM::SM` (sm.h lines 35-54, sm.cpp lines 253-270).

/// Interface to "sm:" service.
///
/// Corresponds to upstream `Service::SM::SM : ServiceFramework<SM>`.
/// Constructor: `SM(ServiceManager& service_manager_, Core::System& system_)`.
pub struct Sm {
    service_manager: Arc<Mutex<ServiceManager>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl Sm {
    /// Creates a new SM service.
    ///
    /// Matches upstream `SM::SM(ServiceManager& service_manager_, Core::System& system_)`.
    pub fn new(service_manager: Arc<Mutex<ServiceManager>>) -> Self {
        // Matches upstream handler registration:
        // RegisterHandlers({{0, &SM::Initialize, "Initialize"}, ...})
        // RegisterHandlersTipc({{0, &SM::Initialize, "Initialize"}, ...})
        let handlers = build_handler_map(&[
            (0, Some(Sm::initialize_handler), "Initialize"),
            (1, Some(Sm::get_service_cmif_handler), "GetService"),
            (
                2,
                Some(Sm::register_service_cmif_handler),
                "RegisterService",
            ),
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
    ///
    /// Matches upstream `SM::Initialize(HLERequestContext& ctx)`.
    fn initialize(&self, ctx: &mut HLERequestContext) {
        log::debug!("SM::Initialize called");

        if let Some(manager) = ctx.get_manager() {
            manager.lock().unwrap().set_is_initialized_for_sm();
        }

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// SM::GetService (CMIF variant).
    ///
    /// Matches upstream `SM::GetServiceCmif(HLERequestContext& ctx)`.
    fn get_service_cmif(&self, ctx: &mut HLERequestContext) {
        let (result, session_handle) = self.get_service_impl(ctx);
        if ctx.get_is_deferred() {
            return;
        }

        if result.is_success() {
            let mut rb = ResponseBuilder::new_with_flags(
                ctx,
                2,
                0,
                1,
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
    ///
    /// Matches upstream `SM::GetServiceTipc(HLERequestContext& ctx)`.
    fn get_service_tipc(&self, ctx: &mut HLERequestContext) {
        let (result, session_handle) = self.get_service_impl(ctx);
        if ctx.get_is_deferred() {
            return;
        }

        let mut rb = ResponseBuilder::new_with_flags(
            ctx,
            2,
            0,
            1,
            crate::hle::service::ipc_helpers::ResponseBuilderFlags::AlwaysMoveHandles,
        );
        rb.push_result(result);
        rb.push_move_objects(if result.is_success() {
            session_handle.unwrap_or(0)
        } else {
            0
        });
    }

    /// Pop service name from request: 8 bytes of ASCII, printable characters only.
    ///
    /// Matches upstream `static std::string PopServiceName(IPC::RequestParser& rp)`.
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
                // Upstream: LOG_INFO(Service_SM, "Waiting for service {} to become available", name);
                log::info!("Waiting for service {} to become available", name);
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
    ///
    /// Matches upstream `SM::RegisterServiceCmif(HLERequestContext& ctx)`.
    fn register_service_cmif(&self, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let name = Self::pop_service_name(&mut rp);
        let _is_light = rp.pop_u32() != 0;
        let max_session_count = rp.pop_u32();

        self.register_service_impl(ctx, name, max_session_count, _is_light);
    }

    /// SM::RegisterService (TIPC variant).
    ///
    /// Matches upstream `SM::RegisterServiceTipc(HLERequestContext& ctx)`.
    fn register_service_tipc(&self, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let name = Self::pop_service_name(&mut rp);
        let max_session_count = rp.pop_u32();
        let _is_light = rp.pop_u32() != 0;

        self.register_service_impl(ctx, name, max_session_count, _is_light);
    }

    /// Internal implementation for RegisterService.
    ///
    /// Matches upstream `SM::RegisterServiceImpl(ctx, name, max_session_count, is_light)`.
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
            // Upstream passes nullptr as handler factory for guest-registered services.
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

        // Upstream creates a KPort via ServiceManager::RegisterService, which returns a
        // KServerPort*. That server port is then pushed as a move handle:
        //   IPC::ResponseBuilder rb{ctx, 2, 0, 1, IPC::ResponseBuilder::Flags::AlwaysMoveHandles};
        //   rb.Push(ResultSuccess);
        //   rb.PushMoveObjects(server_port);
        //
        // Blocked on KPort integration: ServiceManager::register_service needs to return a
        // KServerPort handle that can be pushed via PushMoveObjects. Until KPort is wired,
        // push 0 as a placeholder handle so the IPC response structure matches upstream.
        let mut rb = ResponseBuilder::new_with_flags(
            ctx,
            2,
            0,
            1,
            crate::hle::service::ipc_helpers::ResponseBuilderFlags::AlwaysMoveHandles,
        );
        rb.push_result(RESULT_SUCCESS);
        rb.push_move_objects(0); // placeholder: should be server_port handle from KPort
    }

    /// SM::UnregisterService.
    ///
    /// Matches upstream `SM::UnregisterService(HLERequestContext& ctx)`.
    fn unregister_service(&self, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let name = Self::pop_service_name(&mut rp);

        log::debug!("SM::UnregisterService called with name={}", name);

        let result = self
            .service_manager
            .lock()
            .unwrap()
            .unregister_service(&name);
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

// --- LoopProcess ---

/// Runs SM services.
///
/// Matches upstream `void LoopProcess(Core::System& system)` (sm.cpp lines 274-286):
/// ```cpp
/// void LoopProcess(Core::System& system) {
///     auto& service_manager = system.ServiceManager();
///     auto server_manager = std::make_unique<ServerManager>(system);
///
///     Kernel::KEvent* deferral_event{};
///     server_manager->ManageDeferral(&deferral_event);
///     service_manager.SetDeferralEvent(deferral_event);
///
///     auto sm_service = std::make_shared<SM>(system.ServiceManager(), system);
///     server_manager->ManageNamedPort("sm:", [sm_service] { return sm_service; });
///
///     ServerManager::RunServer(std::move(server_manager));
/// }
/// ```
pub fn loop_process(service_manager: &Arc<Mutex<ServiceManager>>, system: crate::core::SystemRef) {
    let mut server_manager = ServerManager::new(system);

    // Manage deferral event.
    // Upstream: server_manager->ManageDeferral(&deferral_event);
    //           service_manager.SetDeferralEvent(deferral_event);
    let (_result, deferral_event) = server_manager.manage_deferral();
    service_manager
        .lock()
        .unwrap()
        .set_deferral_event(deferral_event);

    // Register the "sm:" named port.
    // Upstream: ManageNamedPort creates a KPort and registers it via
    // KObjectName::NewFromName so ConnectToNamedPort can find it.
    let sm_service = Arc::new(Sm::new(service_manager.clone()));
    let sm_clone = sm_service.clone();
    let factory: SessionRequestHandlerFactory = Box::new(move || sm_clone.clone());
    server_manager.manage_named_port("sm:", factory, 64);

    // Also register in ServiceManager so get_service("sm:") works
    // for internal HLE lookups (ConnectToNamedPort also checks ServiceManager).
    {
        let sm_clone2 = sm_service.clone();
        let factory2: SessionRequestHandlerFactory = Box::new(move || sm_clone2.clone());
        service_manager
            .lock()
            .unwrap()
            .register_service("sm:".to_string(), 64, factory2);
    }

    // Upstream: ServerManager::RunServer(std::move(server_manager));
    ServerManager::run_server(server_manager);
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
        assert!(validate_service_name("sm:").is_success());
        assert!(validate_service_name("12345678").is_success());
        assert!(validate_service_name("").is_error());
        assert!(validate_service_name("123456789").is_error());
    }

    #[test]
    fn test_loop_process() {
        let sm = Arc::new(Mutex::new(ServiceManager::new()));
        loop_process(&sm, crate::core::SystemRef::null());
        // After loop_process, "sm:" is registered on the SM (for connect_to_named_port
        // compatibility) and also as a managed named port on the ServerManager.
        assert!(sm.lock().unwrap().get_service_port("sm:").is_ok());
    }
}
