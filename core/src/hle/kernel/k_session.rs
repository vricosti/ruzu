//! Port of zuyu/src/core/hle/kernel/k_session.h / k_session.cpp
//! Status: Partial (structural port)
//! Derniere synchro: 2026-03-11
//!
//! KSession: a kernel session object containing a server and client endpoint.

use std::sync::atomic::{AtomicU8, Ordering};

/// Session state.
/// Matches upstream `KSession::State` (k_session.h).
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SessionState {
    Invalid = 0,
    Normal = 1,
    ClientClosed = 2,
    ServerClosed = 3,
}

/// The kernel session object.
/// Matches upstream `KSession` class (k_session.h).
pub struct KSession {
    // In upstream, m_server and m_client are embedded sub-objects.
    // We use IDs here; the actual KServerSession/KClientSession are separate structs.
    pub server_session_id: u64,
    pub client_session_id: u64,
    pub port_id: Option<u64>,  // KClientPort*
    pub name: usize,           // uintptr_t name
    pub process_id: Option<u64>, // KProcess*
    pub atomic_state: AtomicU8,
    pub initialized: bool,
}

impl KSession {
    pub fn new() -> Self {
        Self {
            server_session_id: 0,
            client_session_id: 0,
            port_id: None,
            name: 0,
            process_id: None,
            atomic_state: AtomicU8::new(SessionState::Invalid as u8),
            initialized: false,
        }
    }

    /// Initialize the session.
    /// Matches upstream `KSession::Initialize`.
    pub fn initialize(&mut self, port_id: Option<u64>, name: usize) {
        self.port_id = port_id;
        self.name = name;
        self.set_state(SessionState::Normal);
        self.initialized = true;
    }

    pub fn is_initialized(&self) -> bool {
        self.initialized
    }

    fn set_state(&self, state: SessionState) {
        self.atomic_state.store(state as u8, Ordering::Relaxed);
    }

    fn get_state(&self) -> SessionState {
        match self.atomic_state.load(Ordering::Relaxed) {
            0 => SessionState::Invalid,
            1 => SessionState::Normal,
            2 => SessionState::ClientClosed,
            3 => SessionState::ServerClosed,
            _ => SessionState::Invalid,
        }
    }

    pub fn is_server_closed(&self) -> bool {
        self.get_state() != SessionState::Normal
    }

    pub fn is_client_closed(&self) -> bool {
        self.get_state() != SessionState::Normal
    }

    /// Called when the server side is closed.
    /// TODO: Port from k_session.cpp.
    pub fn on_server_closed(&mut self) {
        self.set_state(SessionState::ServerClosed);
        // TODO: Full implementation
    }

    /// Called when the client side is closed.
    /// TODO: Port from k_session.cpp.
    pub fn on_client_closed(&mut self) {
        self.set_state(SessionState::ClientClosed);
        // TODO: Full implementation
    }

    /// Finalize the session.
    /// TODO: Port from k_session.cpp.
    pub fn finalize(&mut self) {
        // TODO: Full implementation
    }
}

impl Default for KSession {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_session_state_values() {
        assert_eq!(SessionState::Invalid as u8, 0);
        assert_eq!(SessionState::Normal as u8, 1);
        assert_eq!(SessionState::ClientClosed as u8, 2);
        assert_eq!(SessionState::ServerClosed as u8, 3);
    }

    #[test]
    fn test_session_initialize() {
        let mut session = KSession::new();
        assert!(!session.is_initialized());
        session.initialize(None, 0);
        assert!(session.is_initialized());
        assert!(!session.is_server_closed());
    }
}
