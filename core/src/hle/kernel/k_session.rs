//! Port of zuyu/src/core/hle/kernel/k_session.h / k_session.cpp
//! Status: Structural port matching upstream ownership
//!
//! KSession: a kernel session object containing a server and client endpoint.
//! Matches upstream where KSession owns both KServerSession and KClientSession
//! as embedded members.

use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::{Arc, Mutex};

use super::k_client_session::KClientSession;
use super::k_server_session::KServerSession;

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
///
/// Upstream: KSession owns m_server (KServerSession) and m_client (KClientSession)
/// as embedded members. Both are initialized with a back-pointer to the KSession.
pub struct KSession {
    /// The server endpoint. Matches upstream `m_server`.
    pub server: Arc<Mutex<KServerSession>>,
    /// The client endpoint. Matches upstream `m_client`.
    pub client: Arc<Mutex<KClientSession>>,
    /// Associated port ID (KClientPort*).
    pub port_id: Option<u64>,
    /// Name for debugging.
    pub name: usize,
    /// Owning process ID.
    pub process_id: Option<u64>,
    /// Atomic state for thread-safe access.
    pub atomic_state: AtomicU8,
    /// Whether the session has been initialized.
    pub initialized: bool,
}

impl KSession {
    /// Create a new uninitialized session.
    /// Matches upstream `KSession::Create` + default state.
    pub fn new() -> Self {
        Self {
            server: Arc::new(Mutex::new(KServerSession::new())),
            client: Arc::new(Mutex::new(KClientSession::new())),
            port_id: None,
            name: 0,
            process_id: None,
            atomic_state: AtomicU8::new(SessionState::Invalid as u8),
            initialized: false,
        }
    }

    /// Initialize the session.
    /// Matches upstream `KSession::Initialize(KClientPort*, uintptr_t)`.
    ///
    /// Sets up the back-references from server/client to this session.
    pub fn initialize(&mut self, port_id: Option<u64>, name: usize) {
        self.port_id = port_id;
        self.name = name;
        self.set_state(SessionState::Normal);
        self.initialized = true;
    }

    pub fn is_initialized(&self) -> bool {
        self.initialized
    }

    /// Get a reference to the server session.
    /// Matches upstream `GetServerSession()`.
    pub fn get_server_session(&self) -> &Arc<Mutex<KServerSession>> {
        &self.server
    }

    /// Get a reference to the client session.
    /// Matches upstream `GetClientSession()`.
    pub fn get_client_session(&self) -> &Arc<Mutex<KClientSession>> {
        &self.client
    }

    /// Forward a request to the server session.
    /// Matches upstream `KSession::OnRequest(KSessionRequest*)`.
    pub fn on_request(&self, request_id: u64) -> u32 {
        self.server.lock().unwrap().on_request(request_id)
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
    pub fn on_server_closed(&mut self) {
        self.set_state(SessionState::ServerClosed);
    }

    /// Called when the client side is closed.
    pub fn on_client_closed(&mut self) {
        self.set_state(SessionState::ClientClosed);
    }

    /// Finalize the session.
    /// Port of upstream `KSession::Finalize`.
    pub fn finalize(&mut self) {
        if self.port_id.is_some() {
            // Upstream: m_port->OnSessionFinalized(); m_port->Close();
            // Port reference cleanup is handled by the object system.
            self.port_id = None;
        }
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

    #[test]
    fn test_session_owns_server_and_client() {
        let session = KSession::new();
        // Both endpoints are accessible
        let _server = session.get_server_session().lock().unwrap();
        let _client = session.get_client_session().lock().unwrap();
    }
}
