//! Port of zuyu/src/core/hle/kernel/k_client_session.h / k_client_session.cpp
//! Status: Partial (structural port)
//! Derniere synchro: 2026-03-11
//!
//! KClientSession: the client endpoint of a session, used to send IPC requests.

use std::sync::{Arc, Mutex};

use crate::hle::kernel::k_process::KProcess;
use crate::hle::kernel::k_session_request::KSessionRequest;
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
    ///
    /// Rust still routes the active SVC path through `send_sync_request_with_process(...)`
    /// so the caller can reuse the already-held owner process. This fallback now
    /// creates a real `KSessionRequest` owner, but it still re-enters the process
    /// registry and should be migrated off the hot runtime path.
    pub fn send_sync_request(&mut self, address: usize, size: usize) -> u32 {
        let Some(parent_id) = self.parent_id else {
            return 1;
        };
        let Some(kernel) = crate::hle::kernel::kernel::get_kernel_ref() else {
            return 1;
        };
        let Some(owner_process_id) = kernel.get_session_owner_process_id(parent_id) else {
            return 1;
        };
        let Some(process_arc) = kernel.get_process_by_id(owner_process_id) else {
            return 1;
        };
        let mut process = process_arc.lock().unwrap();
        self.send_sync_request_with_process(&mut process, address, size)
    }

    /// Port of upstream `KClientSession::SendSyncRequest` when the caller
    /// already holds the owning `KProcess`.
    ///
    /// Rust now creates a real `KSessionRequest` owner in `k_session_request.rs`
    /// before forwarding to the parent `KSession`. The upstream slab lifecycle
    /// is still approximated with `Arc<Mutex<_>>`.
    pub fn send_sync_request_with_process(
        &mut self,
        process: &mut KProcess,
        address: usize,
        size: usize,
    ) -> u32 {
        let Some(parent_id) = self.parent_id else {
            return 1;
        };
        let Some(parent_session) = process.get_session_by_object_id(parent_id) else {
            return 1;
        };
        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        request.lock().unwrap().initialize(None, address, size);
        let result = {
            let session = parent_session.lock().unwrap();
            session.on_request_with_process(process, request)
        };
        result
    }

    /// Send an asynchronous IPC request.
    /// Port of upstream `KClientSession::SendAsyncRequest`.
    pub fn send_async_request(&mut self, _event_id: u64, _address: usize, _size: usize) -> u32 {
        let Some(parent_id) = self.parent_id else {
            return 1;
        };
        let Some(kernel) = crate::hle::kernel::kernel::get_kernel_ref() else {
            return 1;
        };
        let Some(owner_process_id) = kernel.get_session_owner_process_id(parent_id) else {
            return 1;
        };
        let Some(process_arc) = kernel.get_process_by_id(owner_process_id) else {
            return 1;
        };
        let mut process = process_arc.lock().unwrap();
        self.send_async_request_with_process(&mut process, _event_id, _address, _size)
    }

    /// Port of upstream `KClientSession::SendAsyncRequest` when the caller
    /// already holds the owning `KProcess`.
    pub fn send_async_request_with_process(
        &mut self,
        process: &mut KProcess,
        event_id: u64,
        address: usize,
        size: usize,
    ) -> u32 {
        let Some(parent_id) = self.parent_id else {
            return 1;
        };
        let Some(parent_session) = process.get_session_by_object_id(parent_id) else {
            return 1;
        };
        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        request
            .lock()
            .unwrap()
            .initialize(Some(event_id), address, size);
        let session = parent_session.lock().unwrap();
        session.on_request_with_process(process, request)
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hle::kernel::k_process::KProcess;
    use crate::hle::kernel::k_session::KSession;
    use std::sync::{Arc, Mutex};

    #[test]
    fn send_sync_request_with_process_enqueues_real_session_request() {
        let mut process = KProcess::new();
        let session = Arc::new(Mutex::new(KSession::new()));
        {
            let mut session_guard = session.lock().unwrap();
            session_guard.initialize(None, 0);
            session_guard.client.lock().unwrap().initialize(0x1000);
            session_guard.server.lock().unwrap().initialize(0x1000);
        }
        process.register_session_object(0x1000, Arc::clone(&session));

        let client_session = session.lock().unwrap().get_client_session().clone();
        assert_eq!(
            client_session
                .lock()
                .unwrap()
                .send_sync_request_with_process(&mut process, 0x2395000, 0x80),
            0
        );

        let server_session = session.lock().unwrap().get_server_session().clone();
        assert_eq!(server_session.lock().unwrap().receive_request(), 0);
        let current_request = server_session
            .lock()
            .unwrap()
            .get_current_request()
            .expect("queued request");
        let current_request = current_request.lock().unwrap();
        assert_eq!(current_request.get_address(), 0x2395000);
        assert_eq!(current_request.get_size(), 0x80);
    }

    #[test]
    fn send_async_request_with_process_enqueues_request_with_event() {
        let mut process = KProcess::new();
        let session = Arc::new(Mutex::new(KSession::new()));
        {
            let mut session_guard = session.lock().unwrap();
            session_guard.initialize(None, 0);
            session_guard.client.lock().unwrap().initialize(0x1000);
            session_guard.server.lock().unwrap().initialize(0x1000);
        }
        process.register_session_object(0x1000, Arc::clone(&session));

        let client_session = session.lock().unwrap().get_client_session().clone();
        assert_eq!(
            client_session
                .lock()
                .unwrap()
                .send_async_request_with_process(&mut process, 0x2222, 0x2395000, 0x80),
            0
        );

        let server_session = session.lock().unwrap().get_server_session().clone();
        assert_eq!(server_session.lock().unwrap().receive_request(), 0);
        let current_request = server_session
            .lock()
            .unwrap()
            .get_current_request()
            .expect("queued request");
        let current_request = current_request.lock().unwrap();
        assert_eq!(current_request.get_event_id(), Some(0x2222));
        assert_eq!(current_request.get_address(), 0x2395000);
    }
}
