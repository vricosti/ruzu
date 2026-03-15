// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/server_manager.h and server_manager.cpp
//!
//! Contains:
//! - ServerManager: manages server ports and sessions for HLE services
//! - Session: wrapper pairing a KServerSession with a SessionRequestManager
//!
//! Upstream uses kernel synchronization objects, multi-wait, and intrusive lists.
//! This port implements the key structural relationships while stubbing the
//! event-driven dispatch until full kernel integration.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use crate::hle::kernel::k_server_session::KServerSession;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{
    SessionRequestHandlerFactory, SessionRequestHandlerPtr, SessionRequestManager,
};

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
pub struct ServerManager {
    /// Registered named services with their factories.
    registered_services: HashMap<String, SessionRequestHandlerFactory>,

    /// Active sessions (server session + manager pairs).
    /// Matches upstream `m_sessions` intrusive list of Session wrappers.
    sessions: Vec<Session>,

    /// Whether the server has been stopped.
    stopped: bool,
}

impl ServerManager {
    /// Creates a new ServerManager.
    pub fn new() -> Self {
        Self {
            registered_services: HashMap::new(),
            sessions: Vec::new(),
            stopped: false,
        }
    }

    /// Registers a session with a manager.
    ///
    /// Matches upstream `ServerManager::RegisterSession(KServerSession*, shared_ptr<SessionRequestManager>)`.
    ///
    /// Creates a Session wrapper that pairs the server session with the manager,
    /// and also sets the manager on the KServerSession for direct dispatch.
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
    pub fn register_named_service(
        &mut self,
        service_name: &str,
        handler_factory: SessionRequestHandlerFactory,
        _max_sessions: u32,
    ) -> ResultCode {
        self.registered_services
            .insert(service_name.to_string(), handler_factory);
        RESULT_SUCCESS
    }

    /// Registers a named service with a shared handler.
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

    /// Manages a named port.
    pub fn manage_named_port(
        &mut self,
        service_name: &str,
        handler_factory: SessionRequestHandlerFactory,
        _max_sessions: u32,
    ) -> ResultCode {
        self.registered_services
            .insert(service_name.to_string(), handler_factory);
        RESULT_SUCCESS
    }

    /// Manages deferral events.
    pub fn manage_deferral(&mut self) -> ResultCode {
        RESULT_SUCCESS
    }

    /// Main loop for processing server events.
    pub fn loop_process(&mut self) -> ResultCode {
        // TODO: implement event loop when kernel integration is ready
        RESULT_SUCCESS
    }

    /// Starts additional host threads for processing.
    pub fn start_additional_host_threads(&mut self, _name: &str, _num_threads: usize) {
        // TODO: implement when threading is wired up
    }

    /// Runs a server manager to completion.
    pub fn run_server(server: Box<ServerManager>) {
        drop(server);
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
        let mgr = ServerManager::new();
        assert!(!mgr.is_stopped());
        assert_eq!(mgr.session_count(), 0);
    }

    #[test]
    fn test_register_session() {
        let mut mgr = ServerManager::new();
        let server = Arc::new(Mutex::new(KServerSession::new()));
        let manager = Arc::new(Mutex::new(SessionRequestManager::new()));

        let result = mgr.register_session(server.clone(), manager.clone());
        assert_eq!(result, RESULT_SUCCESS);
        assert_eq!(mgr.session_count(), 1);

        // Server session should now have the manager set
        assert!(server.lock().unwrap().get_manager().is_some());
    }
}
