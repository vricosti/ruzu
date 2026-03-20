//! Port of zuyu/src/core/hle/kernel/k_server_session.h / k_server_session.cpp
//! Status: Partial (structural port)
//!
//! KServerSession: the server endpoint of a session, handles incoming requests.
//!
//! In upstream, the ServerManager links a KServerSession to a SessionRequestManager
//! via RegisterSession(). When a client sends a request, the server session
//! dispatches to the handler via the manager.

use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

use crate::hle::service::hle_ipc::SessionRequestManager;

/// The server session object.
/// Matches upstream `KServerSession` class (k_server_session.h).
pub struct KServerSession {
    /// Parent KSession ID.
    pub parent_id: Option<u64>,
    /// Pending request list (upstream: intrusive list of KSessionRequest).
    pub request_list: VecDeque<u64>,
    /// Currently being processed request.
    pub current_request_id: Option<u64>,
    /// The session request manager linked via ServerManager::RegisterSession.
    /// Matches upstream where ServerManager stores the Session wrapper that
    /// pairs the KServerSession with a SessionRequestManager.
    pub manager: Option<Arc<Mutex<SessionRequestManager>>>,
}

impl KServerSession {
    pub fn new() -> Self {
        Self {
            parent_id: None,
            request_list: VecDeque::new(),
            current_request_id: None,
            manager: None,
        }
    }

    /// Initialize with a parent session.
    pub fn initialize(&mut self, parent_id: u64) {
        self.parent_id = Some(parent_id);
    }

    /// Set the session request manager.
    /// Called by ServerManager::RegisterSession to link the server session
    /// to its HLE handler.
    pub fn set_manager(&mut self, manager: Arc<Mutex<SessionRequestManager>>) {
        self.manager = Some(manager);
    }

    /// Get the session request manager.
    pub fn get_manager(&self) -> Option<&Arc<Mutex<SessionRequestManager>>> {
        self.manager.as_ref()
    }

    pub fn get_parent_id(&self) -> Option<u64> {
        self.parent_id
    }

    /// Is the server session signaled (has pending requests)?
    /// Matches upstream `KServerSession::IsSignaled`.
    pub fn is_signaled(&self) -> bool {
        !self.request_list.is_empty()
    }

    /// Called when the client side is closed.
    /// Port of upstream `KServerSession::OnClientClosed`.
    /// Upstream signals the session and wakes waiting threads.
    pub fn on_client_closed(&mut self) {
        // Upstream: KScopedSchedulerLock + notify waiters of session closure.
        // Without scheduler integration, just clean up pending requests.
        self.cleanup_requests();
    }

    /// Enqueue a request.
    /// Port of upstream `KServerSession::OnRequest`.
    pub fn on_request(&mut self, request_id: u64) -> u32 {
        self.request_list.push_back(request_id);
        // Upstream: this->Signal() to wake the event loop.
        // The HLE service layer handles dispatching via SessionRequestManager.
        0 // ResultSuccess
    }

    /// Send a reply to the current request.
    /// Port of upstream `KServerSession::SendReply` (simplified for HLE).
    /// Full kernel IPC message translation requires KProcessPageTable and message buffers.
    pub fn send_reply(&mut self) -> u32 {
        if let Some(_request_id) = self.current_request_id.take() {
            // Upstream: translate reply message, end request thread wait.
            // HLE services handle reply via SessionRequestManager.
        }
        0 // ResultSuccess
    }

    /// Receive the next pending request.
    /// Port of upstream `KServerSession::ReceiveRequest` (simplified for HLE).
    pub fn receive_request(&mut self) -> u32 {
        if let Some(request_id) = self.request_list.pop_front() {
            self.current_request_id = Some(request_id);
            return 0; // ResultSuccess
        }
        // No pending requests. Upstream would block via scheduler.
        1 // Would need to return ResultNotFound or similar
    }

    /// Clean up pending requests.
    pub fn cleanup_requests(&mut self) {
        self.request_list.clear();
        self.current_request_id = None;
    }

    /// Destroy the server session.
    pub fn destroy(&mut self) {
        self.cleanup_requests();
    }
}

impl Default for KServerSession {
    fn default() -> Self {
        Self::new()
    }
}
