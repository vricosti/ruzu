//! Port of zuyu/src/core/hle/kernel/k_server_port.h / k_server_port.cpp
//! Status: Partial (structural port)
//! Derniere synchro: 2026-03-11
//!
//! KServerPort: the server endpoint of a port, accepts sessions.

use std::collections::VecDeque;

/// The server port object.
/// Matches upstream `KServerPort` class (k_server_port.h).
pub struct KServerPort {
    /// Pending session list (KServerSession IDs).
    pub session_list: VecDeque<u64>,
    /// Pending light session list (KLightServerSession IDs).
    pub light_session_list: VecDeque<u64>,
    /// Parent KPort ID.
    pub parent_id: Option<u64>,
}

impl KServerPort {
    pub fn new() -> Self {
        Self {
            session_list: VecDeque::new(),
            light_session_list: VecDeque::new(),
            parent_id: None,
        }
    }

    /// Initialize with a parent port.
    pub fn initialize(&mut self, parent_id: u64) {
        self.parent_id = Some(parent_id);
    }

    pub fn get_parent_id(&self) -> Option<u64> {
        self.parent_id
    }

    /// Enqueue a session.
    pub fn enqueue_session(&mut self, session_id: u64) {
        self.session_list.push_back(session_id);
    }

    /// Enqueue a light session.
    pub fn enqueue_light_session(&mut self, session_id: u64) {
        self.light_session_list.push_back(session_id);
    }

    /// Accept a session (dequeue).
    pub fn accept_session(&mut self) -> Option<u64> {
        self.session_list.pop_front()
    }

    /// Accept a light session (dequeue).
    pub fn accept_light_session(&mut self) -> Option<u64> {
        self.light_session_list.pop_front()
    }

    /// Is the server port signaled (has pending sessions)?
    pub fn is_signaled(&self) -> bool {
        !self.session_list.is_empty() || !self.light_session_list.is_empty()
    }

    /// Is this a light port?
    /// TODO: Delegate to parent.
    pub fn is_light(&self) -> bool {
        false // TODO: Check parent
    }

    /// Clean up sessions.
    /// TODO: Port from k_server_port.cpp.
    pub fn cleanup_sessions(&mut self) {
        self.session_list.clear();
        self.light_session_list.clear();
    }

    /// Destroy the server port.
    /// TODO: Port from k_server_port.cpp.
    pub fn destroy(&mut self) {
        self.cleanup_sessions();
    }
}

impl Default for KServerPort {
    fn default() -> Self {
        Self::new()
    }
}
