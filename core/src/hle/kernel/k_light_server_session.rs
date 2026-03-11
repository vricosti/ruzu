//! Port of zuyu/src/core/hle/kernel/k_light_server_session.h / k_light_server_session.cpp
//! Status: Partial (structural port)
//! Derniere synchro: 2026-03-11
//!
//! KLightServerSession: the server endpoint of a light session.

use std::collections::VecDeque;

/// The light server session object.
/// Matches upstream `KLightServerSession` class (k_light_server_session.h).
pub struct KLightServerSession {
    /// Parent KLightSession ID.
    pub parent_id: Option<u64>,
    /// List of waiting request thread IDs (upstream: KThread::WaiterList).
    pub request_list: VecDeque<u64>,
    /// Current request thread ID.
    pub current_request_thread_id: Option<u64>,
    /// Server thread ID (std::numeric_limits<u64>::max() when unset).
    pub server_thread_id: u64,
    /// Server thread reference.
    pub server_thread: Option<u64>,
}

impl KLightServerSession {
    pub fn new() -> Self {
        Self {
            parent_id: None,
            request_list: VecDeque::new(),
            current_request_thread_id: None,
            server_thread_id: u64::MAX,
            server_thread: None,
        }
    }

    /// Initialize with a parent light session.
    pub fn initialize(&mut self, parent_id: u64) {
        self.parent_id = Some(parent_id);
    }

    pub fn get_parent_id(&self) -> Option<u64> {
        self.parent_id
    }

    /// Enqueue a request from a thread.
    /// Matches upstream `KLightServerSession::OnRequest`.
    /// TODO: Port from k_light_server_session.cpp.
    pub fn on_request(&mut self, request_thread_id: u64) -> u32 {
        self.request_list.push_back(request_thread_id);
        // TODO: Signal
        0 // ResultSuccess
    }

    /// Reply and receive.
    /// TODO: Port from k_light_server_session.cpp.
    pub fn reply_and_receive(&mut self, _data: &mut [u32]) -> u32 {
        // TODO: Full implementation
        0
    }

    /// Called when the client side is closed.
    /// TODO: Port from k_light_server_session.cpp.
    pub fn on_client_closed(&mut self) {
        // TODO: Full implementation
    }

    /// Clean up pending requests.
    pub fn cleanup_requests(&mut self) {
        self.request_list.clear();
        self.current_request_thread_id = None;
    }

    /// Destroy the light server session.
    pub fn destroy(&mut self) {
        self.cleanup_requests();
    }
}

impl Default for KLightServerSession {
    fn default() -> Self {
        Self::new()
    }
}
