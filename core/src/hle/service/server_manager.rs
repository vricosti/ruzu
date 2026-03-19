// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/server_manager.h and server_manager.cpp
//!
//! Contains:
//! - ServerManager: manages server ports and sessions for HLE services
//! - Session: wrapper pairing a KServerSession with a SessionRequestManager
//!
//! Upstream `ServerManager` is constructed with `Core::System&` and uses it to
//! access the global `ServiceManager` for `RegisterNamedService`. In our port,
//! we pass the `ServiceManager` directly.
//!
//! Key upstream methods:
//! - `RegisterNamedService(name, handler)` — registers with the global SM, then
//!    stores the port locally for its event loop
//! - `ManageNamedPort(name, handler)` — creates a standalone port (not in SM);
//!    used for "sm:" itself
//! - `ManageDeferral(event)` — sets up deferred request handling
//! - `LoopProcess()` / `RunServer()` — main event loop (stubbed here)

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use crate::hle::kernel::k_server_session::KServerSession;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{
    SessionRequestHandlerFactory, SessionRequestHandlerPtr, SessionRequestManager,
};
use crate::hle::service::sm::sm::ServiceManager;

/// Tag for MultiWaitHolder user data, matching upstream `UserDataTag`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UserDataTag {
    Port,
    Session,
    DeferEvent,
}

/// Session wrapper pairing a KServerSession with its SessionRequestManager.
///
/// Matches upstream `Service::Session` (server_manager.cpp) which wraps:
/// - Pointer to KServerSession (the kernel object)
/// - Shared reference to SessionRequestManager (HLE dispatch state)
///
/// In upstream, Session is also a MultiWaitHolder for the kernel event loop.
struct Session {
    server_session: Arc<Mutex<KServerSession>>,
    manager: Arc<Mutex<SessionRequestManager>>,
}

/// Manages server ports and sessions for HLE services.
///
/// Corresponds to upstream `Service::ServerManager`.
///
/// Upstream constructs with `Core::System&` and accesses `system.ServiceManager()`
/// internally. Here we store the `ServiceManager` reference directly.
pub struct ServerManager {
    /// Reference to the global ServiceManager for `RegisterNamedService`.
    /// Matches upstream `system.ServiceManager()` accessed via stored `System&`.
    service_manager: Arc<Mutex<ServiceManager>>,

    /// Locally managed named ports (not registered with SM).
    /// Used by `ManageNamedPort` for ports like "sm:".
    managed_ports: HashMap<String, SessionRequestHandlerFactory>,

    /// Active sessions (server session + manager pairs).
    /// Matches upstream `m_sessions` intrusive list of Session wrappers.
    sessions: Vec<Session>,

    /// Whether the server has been stopped.
    stopped: bool,
}

impl ServerManager {
    /// Creates a new ServerManager with a reference to the global ServiceManager.
    ///
    /// Matches upstream `ServerManager::ServerManager(Core::System& system)`.
    pub fn new(service_manager: Arc<Mutex<ServiceManager>>) -> Self {
        Self {
            service_manager,
            managed_ports: HashMap::new(),
            sessions: Vec::new(),
            stopped: false,
        }
    }

    /// Registers a session with a manager.
    ///
    /// Matches upstream `ServerManager::RegisterSession(KServerSession*, shared_ptr<SessionRequestManager>)`.
    pub fn register_session(
        &mut self,
        server_session: Arc<Mutex<KServerSession>>,
        manager: Arc<Mutex<SessionRequestManager>>,
    ) -> ResultCode {
        // Set the manager on the server session so it knows how to dispatch.
        server_session.lock().unwrap().set_manager(manager.clone());

        // Store the pairing.
        self.sessions.push(Session {
            server_session,
            manager,
        });

        RESULT_SUCCESS
    }

    /// Registers a named service with a handler factory.
    ///
    /// Matches upstream `ServerManager::RegisterNamedService(name, handler_factory, max_sessions)`.
    /// Upstream calls `ServiceManager::RegisterService()` to register globally,
    /// then stores the port locally for event-loop dispatch.
    pub fn register_named_service(
        &mut self,
        service_name: &str,
        handler_factory: SessionRequestHandlerFactory,
        max_sessions: u32,
    ) -> ResultCode {
        // Register with the global ServiceManager (matching upstream).
        let result = self.service_manager.lock().unwrap().register_service(
            service_name.to_string(),
            max_sessions,
            handler_factory,
        );

        if result.is_error() {
            log::warn!(
                "ServerManager: failed to register '{}' with SM: {:#x}",
                service_name,
                result.get_inner_value()
            );
        }

        result
    }

    /// Registers a named service with a shared handler instance.
    ///
    /// Convenience wrapper that creates a factory from a shared handler.
    pub fn register_named_service_handler(
        &mut self,
        service_name: &str,
        handler: SessionRequestHandlerPtr,
        max_sessions: u32,
    ) -> ResultCode {
        let handler_clone = handler.clone();
        let factory: SessionRequestHandlerFactory = Box::new(move || handler_clone.clone());
        self.register_named_service(service_name, factory, max_sessions)
    }

    /// Manages a named port (standalone, not registered with SM).
    ///
    /// Matches upstream `ServerManager::ManageNamedPort(name, handler_factory, max_sessions)`.
    /// Unlike `RegisterNamedService`, this does NOT register with the global
    /// ServiceManager. Used for ports like "sm:" that are discovered by name
    /// rather than through SM.
    pub fn manage_named_port(
        &mut self,
        port_name: &str,
        handler_factory: SessionRequestHandlerFactory,
        _max_sessions: u32,
    ) -> ResultCode {
        self.managed_ports
            .insert(port_name.to_string(), handler_factory);
        RESULT_SUCCESS
    }

    /// Manages deferral events.
    ///
    /// Matches upstream `ServerManager::ManageDeferral(KEvent**)`.
    /// TODO: create KEvent when kernel integration is ready.
    pub fn manage_deferral(&mut self) -> ResultCode {
        RESULT_SUCCESS
    }

    /// Main loop for processing server events.
    ///
    /// Matches upstream `ServerManager::LoopProcess()`.
    /// TODO: implement event loop when kernel integration is ready.
    pub fn loop_process(&mut self) -> ResultCode {
        RESULT_SUCCESS
    }

    /// Starts additional host threads for processing.
    pub fn start_additional_host_threads(&mut self, _name: &str, _num_threads: usize) {
        // TODO: implement when threading is wired up
    }

    /// Runs a server manager to completion.
    ///
    /// Matches upstream `ServerManager::RunServer(unique_ptr<ServerManager>)`.
    /// In upstream, this calls `system.RunServer()` which enters the event loop.
    /// Stubbed here — the manager is dropped immediately.
    pub fn run_server(_server_manager: ServerManager) {
        // In full implementation, this would block on the event loop.
        // For now, registration has already happened via register_named_service
        // (which calls through to ServiceManager), so we just return.
    }

    /// Returns the service manager reference.
    pub fn service_manager(&self) -> &Arc<Mutex<ServiceManager>> {
        &self.service_manager
    }

    /// Returns whether the server has been stopped.
    pub fn is_stopped(&self) -> bool {
        self.stopped
    }

    /// Returns the number of active sessions.
    pub fn session_count(&self) -> usize {
        self.sessions.len()
    }
}

impl Drop for ServerManager {
    fn drop(&mut self) {
        self.stopped = true;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_server_manager_creation() {
        let sm = Arc::new(Mutex::new(ServiceManager::new()));
        let mgr = ServerManager::new(sm);
        assert!(!mgr.is_stopped());
        assert_eq!(mgr.session_count(), 0);
    }

    #[test]
    fn test_register_session() {
        let sm = Arc::new(Mutex::new(ServiceManager::new()));
        let mut mgr = ServerManager::new(sm);
        let server = Arc::new(Mutex::new(KServerSession::new()));
        let manager = Arc::new(Mutex::new(SessionRequestManager::new()));

        let result = mgr.register_session(server.clone(), manager.clone());
        assert_eq!(result, RESULT_SUCCESS);
        assert_eq!(mgr.session_count(), 1);

        // Server session should now have the manager set
        assert!(server.lock().unwrap().get_manager().is_some());
    }

    #[test]
    fn test_register_named_service_goes_through_sm() {
        let sm = Arc::new(Mutex::new(ServiceManager::new()));
        let mut mgr = ServerManager::new(sm.clone());

        let factory: SessionRequestHandlerFactory =
            Box::new(|| -> SessionRequestHandlerPtr { panic!("test") });
        let result = mgr.register_named_service("test_svc", factory, 64);
        assert!(result.is_success());

        // Verify it was registered in the global ServiceManager
        assert!(sm.lock().unwrap().get_service_port("test_svc").is_ok());
    }

    #[test]
    fn test_manage_named_port_does_not_register_in_sm() {
        let sm = Arc::new(Mutex::new(ServiceManager::new()));
        let mut mgr = ServerManager::new(sm.clone());

        let factory: SessionRequestHandlerFactory =
            Box::new(|| -> SessionRequestHandlerPtr { panic!("test") });
        let result = mgr.manage_named_port("sm:", factory, 64);
        assert!(result.is_success());

        // Should NOT be in the global ServiceManager
        assert!(sm.lock().unwrap().get_service_port("sm:").is_err());
    }
}
