// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Service dispatch framework.
//!
//! [`ServiceManager`] maps service names (e.g. `"sm:"`, `"fsp-srv"`) to their
//! handler implementations and dispatches incoming IPC commands.

use parking_lot::RwLock;
use std::collections::HashMap;
use std::sync::Arc;

use crate::ipc::{IpcCommand, IpcResponse};

// ── Service handler trait ────────────────────────────────────────────────────

/// Trait implemented by every HLE service.
///
/// Each service receives the CMIF command id and the fully-parsed IPC command
/// and returns a response (or a default error).
pub trait ServiceHandler: Send + Sync {
    /// Human-readable name used in log messages.
    fn service_name(&self) -> &str;

    /// Dispatch a single IPC request.
    fn handle_request(&mut self, cmd_id: u32, command: &IpcCommand) -> IpcResponse;
}

// ── Service manager ──────────────────────────────────────────────────────────

/// Central registry that owns all service handlers and routes requests by name.
pub struct ServiceManager {
    services: HashMap<String, Arc<RwLock<Box<dyn ServiceHandler>>>>,
}

impl ServiceManager {
    pub fn new() -> Self {
        Self {
            services: HashMap::new(),
        }
    }

    /// Register a service handler under the given name.
    ///
    /// If a service with the same name already exists it is silently replaced.
    pub fn register_service(&mut self, name: &str, handler: Box<dyn ServiceHandler>) {
        log::info!("Registering service: {}", name);
        self.services
            .insert(name.to_string(), Arc::new(RwLock::new(handler)));
    }

    /// Dispatch an IPC command to the named service.
    ///
    /// Returns `None` if no service is registered under `service_name`.
    pub fn handle_request(
        &self,
        service_name: &str,
        cmd_id: u32,
        command: &IpcCommand,
    ) -> Option<IpcResponse> {
        let handler = self.services.get(service_name)?;
        let mut guard = handler.write();
        Some(guard.handle_request(cmd_id, command))
    }

    /// Check whether a service is registered.
    pub fn has_service(&self, name: &str) -> bool {
        self.services.contains_key(name)
    }

    /// Return a shared reference to a service handler (for sub-interface use).
    pub fn get_service(&self, name: &str) -> Option<Arc<RwLock<Box<dyn ServiceHandler>>>> {
        self.services.get(name).cloned()
    }

    /// Number of registered services.
    pub fn len(&self) -> usize {
        self.services.len()
    }

    /// Whether the registry is empty.
    pub fn is_empty(&self) -> bool {
        self.services.is_empty()
    }
}

impl Default for ServiceManager {
    fn default() -> Self {
        Self::new()
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ipc::{CommandType, IpcCommand, IpcResponse};
    use ruzu_common::ResultCode;

    /// Trivial test service that always returns success.
    struct DummyService;

    impl ServiceHandler for DummyService {
        fn service_name(&self) -> &str {
            "dummy"
        }

        fn handle_request(&mut self, _cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
            IpcResponse::success()
        }
    }

    fn make_dummy_command(cmd_id: u32) -> IpcCommand {
        IpcCommand {
            command_type: CommandType::Request,
            data_size: 0,
            num_x_bufs: 0,
            num_a_bufs: 0,
            num_b_bufs: 0,
            has_handle_descriptor: false,
            handles_to_copy: Vec::new(),
            handles_to_move: Vec::new(),
            send_pid: false,
            cmif_magic: 0x49434653,
            command_id: cmd_id,
            raw_data: Vec::new(),
            b_buf_addrs: Vec::new(),
        }
    }

    #[test]
    fn test_register_and_dispatch() {
        let mut mgr = ServiceManager::new();
        mgr.register_service("dummy", Box::new(DummyService));
        assert!(mgr.has_service("dummy"));
        assert_eq!(mgr.len(), 1);

        let cmd = make_dummy_command(0);
        let resp = mgr.handle_request("dummy", 0, &cmd);
        assert!(resp.is_some());
        assert!(resp.unwrap().result.is_success());
    }

    #[test]
    fn test_unknown_service() {
        let mgr = ServiceManager::new();
        let cmd = make_dummy_command(0);
        assert!(mgr.handle_request("nonexistent", 0, &cmd).is_none());
    }
}
