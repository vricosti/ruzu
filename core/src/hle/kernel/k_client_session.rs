//! Port of zuyu/src/core/hle/kernel/k_client_session.h / k_client_session.cpp
//! Status: Partial (structural port)
//! Derniere synchro: 2026-03-11
//!
//! KClientSession: the client endpoint of a session, used to send IPC requests.

use std::sync::{Arc, Mutex};

use crate::hle::service::hle_ipc::SessionRequestManager;

/// The client session object.
/// Matches upstream `KClientSession` class (k_client_session.h).
pub struct KClientSession {
    /// Parent KSession ID.
    pub parent_id: Option<u64>,
    pub request_manager: Option<Arc<Mutex<SessionRequestManager>>>,
}

impl KClientSession {
    pub fn new() -> Self {
        Self {
            parent_id: None,
            request_manager: None,
        }
    }

    /// Initialize with a parent session.
    pub fn initialize(&mut self, parent_id: u64) {
        self.parent_id = Some(parent_id);
        self.request_manager = None;
    }

    /// Initialize with a parent session and its request manager.
    pub fn initialize_with_manager(
        &mut self,
        parent_id: u64,
        request_manager: Arc<Mutex<SessionRequestManager>>,
    ) {
        self.parent_id = Some(parent_id);
        self.request_manager = Some(request_manager);
    }

    pub fn get_parent_id(&self) -> Option<u64> {
        self.parent_id
    }

    pub fn request_manager(&self) -> Option<Arc<Mutex<SessionRequestManager>>> {
        self.request_manager.clone()
    }

    /// Send a synchronous IPC request.
    /// Port of upstream `KClientSession::SendSyncRequest`.
    /// Upstream creates a KSessionRequest, initializes it, and forwards to parent's OnRequest.
    /// KSessionRequest slab allocation is not yet ported; this returns success as a no-op
    /// for HLE services (which handle IPC via SessionRequestManager instead).
    pub fn send_sync_request(&mut self, _address: usize, _size: usize) -> u32 {
        // HLE services bypass kernel IPC; requests are handled by SessionRequestManager.
        0 // ResultSuccess
    }

    /// Send an asynchronous IPC request.
    /// Port of upstream `KClientSession::SendAsyncRequest`.
    pub fn send_async_request(&mut self, _event_id: u64, _address: usize, _size: usize) -> u32 {
        // HLE services bypass kernel IPC; requests are handled by SessionRequestManager.
        0 // ResultSuccess
    }

    /// Called when the server side is closed.
    /// Port of upstream `KClientSession::OnServerClosed`.
    /// Upstream is a no-op.
    pub fn on_server_closed(&mut self) {
        // Upstream: empty body.
    }

    /// Destroy the client session.
    /// Port of upstream `KClientSession::Destroy`.
    pub fn destroy(&mut self) {
        // Upstream: m_parent->OnClientClosed(); m_parent->Close();
        // Parent reference cleanup is handled by the object system.
        self.parent_id = None;
        self.request_manager = None;
    }
}

impl Default for KClientSession {
    fn default() -> Self {
        Self::new()
    }
}
