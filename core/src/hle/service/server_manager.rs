// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/server_manager.h and server_manager.cpp
//! Status: Structural stub
//!
//! Contains:
//! - ServerManager: manages server ports and sessions for HLE services
//!
//! The upstream implementation uses kernel synchronization objects (KEvent, KServerPort,
//! KServerSession), multi-wait, and intrusive lists. This is stubbed until kernel integration
//! is wired up. The structural shape matches upstream.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

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

/// Manages server ports and sessions for HLE services.
///
/// Corresponds to upstream `Service::ServerManager`.
///
/// The full implementation orchestrates waiting on multiple kernel synchronization objects,
/// dispatching IPC requests to the appropriate session handlers, and managing deferred requests.
pub struct ServerManager {
    /// Registered named services with their factories.
    registered_services: HashMap<String, SessionRequestHandlerFactory>,

    /// Whether the server has been stopped.
    stopped: bool,
}

impl ServerManager {
    /// Creates a new ServerManager.
    pub fn new() -> Self {
        Self {
            registered_services: HashMap::new(),
            stopped: false,
        }
    }

    /// Registers a session with a manager.
    ///
    /// Corresponds to upstream `ServerManager::RegisterSession`.
    pub fn register_session(
        &mut self,
        _manager: Arc<Mutex<SessionRequestManager>>,
    ) -> ResultCode {
        // TODO: implement when kernel integration is ready
        RESULT_SUCCESS
    }

    /// Registers a named service with a handler factory.
    ///
    /// Corresponds to upstream `ServerManager::RegisterNamedService` (factory variant).
    pub fn register_named_service(
        &mut self,
        service_name: &str,
        handler_factory: SessionRequestHandlerFactory,
        _max_sessions: u32,
    ) -> ResultCode {
        // TODO: wire up to SM service manager for full registration
        self.registered_services
            .insert(service_name.to_string(), handler_factory);
        RESULT_SUCCESS
    }

    /// Registers a named service with a shared handler.
    ///
    /// Corresponds to upstream `ServerManager::RegisterNamedService` (handler variant).
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
    ///
    /// Corresponds to upstream `ServerManager::ManageNamedPort`.
    pub fn manage_named_port(
        &mut self,
        service_name: &str,
        handler_factory: SessionRequestHandlerFactory,
        _max_sessions: u32,
    ) -> ResultCode {
        // TODO: create KPort, register with kernel object names
        self.registered_services
            .insert(service_name.to_string(), handler_factory);
        RESULT_SUCCESS
    }

    /// Manages deferral events.
    ///
    /// Corresponds to upstream `ServerManager::ManageDeferral`.
    pub fn manage_deferral(&mut self) -> ResultCode {
        // TODO: create deferral KEvent
        RESULT_SUCCESS
    }

    /// Main loop for processing server events.
    ///
    /// Corresponds to upstream `ServerManager::LoopProcess`.
    pub fn loop_process(&mut self) -> ResultCode {
        // TODO: implement event loop when kernel integration is ready
        RESULT_SUCCESS
    }

    /// Starts additional host threads for processing.
    ///
    /// Corresponds to upstream `ServerManager::StartAdditionalHostThreads`.
    pub fn start_additional_host_threads(&mut self, _name: &str, _num_threads: usize) {
        // TODO: implement when threading is wired up
    }

    /// Runs a server manager to completion.
    ///
    /// Corresponds to upstream `ServerManager::RunServer`.
    pub fn run_server(server: Box<ServerManager>) {
        // TODO: implement when system integration is ready
        drop(server);
    }

    /// Returns whether the server has been stopped.
    pub fn is_stopped(&self) -> bool {
        self.stopped
    }
}

impl Drop for ServerManager {
    fn drop(&mut self) {
        self.stopped = true;
        // TODO: signal stop, wait for threads, clean up ports and sessions
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_server_manager_creation() {
        let mgr = ServerManager::new();
        assert!(!mgr.is_stopped());
    }
}
