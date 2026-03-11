//! Port of zuyu/src/core/hle/kernel/k_client_port.h / k_client_port.cpp
//! Status: Partial (structural port)
//! Derniere synchro: 2026-03-11
//!
//! KClientPort: the client endpoint of a port, used to create sessions.

use std::sync::atomic::{AtomicI32, Ordering};

/// The client port object.
/// Matches upstream `KClientPort` class (k_client_port.h).
pub struct KClientPort {
    pub num_sessions: AtomicI32,
    pub peak_sessions: AtomicI32,
    pub max_sessions: i32,
    pub parent_id: Option<u64>,
}

impl KClientPort {
    pub fn new() -> Self {
        Self {
            num_sessions: AtomicI32::new(0),
            peak_sessions: AtomicI32::new(0),
            max_sessions: 0,
            parent_id: None,
        }
    }

    /// Initialize with a parent port and maximum session count.
    pub fn initialize(&mut self, parent_id: u64, max_sessions: i32) {
        self.parent_id = Some(parent_id);
        self.max_sessions = max_sessions;
    }

    pub fn get_parent_id(&self) -> Option<u64> {
        self.parent_id
    }

    pub fn get_num_sessions(&self) -> i32 {
        self.num_sessions.load(Ordering::Relaxed)
    }

    pub fn get_peak_sessions(&self) -> i32 {
        self.peak_sessions.load(Ordering::Relaxed)
    }

    pub fn get_max_sessions(&self) -> i32 {
        self.max_sessions
    }

    /// Called when a session is finalized.
    pub fn on_session_finalized(&self) {
        self.num_sessions.fetch_sub(1, Ordering::Relaxed);
    }

    /// Called when the server side is closed.
    /// TODO: Port from k_client_port.cpp.
    pub fn on_server_closed(&mut self) {
        // TODO: Full implementation
    }

    /// Is the port signaled?
    /// TODO: Port from k_client_port.cpp.
    pub fn is_signaled(&self) -> bool {
        false // TODO
    }

    /// Is this a light port?
    /// TODO: Delegate to parent.
    pub fn is_light(&self) -> bool {
        false // TODO
    }

    /// Is the server side closed?
    /// TODO: Delegate to parent.
    pub fn is_server_closed(&self) -> bool {
        false // TODO
    }

    /// Create a new session.
    /// TODO: Port from k_client_port.cpp.
    pub fn create_session(&self) -> u32 {
        // TODO: Full implementation
        0
    }

    /// Destroy the client port.
    /// TODO: Port from k_client_port.cpp.
    pub fn destroy(&mut self) {
        // TODO: Full implementation
    }
}

impl Default for KClientPort {
    fn default() -> Self {
        Self::new()
    }
}
