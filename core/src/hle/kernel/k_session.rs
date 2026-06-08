//! Port of zuyu/src/core/hle/kernel/k_session.h / k_session.cpp
//! Status: Structural port matching upstream ownership
//!
//! KSession: a kernel session object containing a server and client endpoint.
//! Matches upstream where KSession owns both KServerSession and KClientSession
//! as embedded members.

use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::{Arc, Mutex};

use super::k_client_session::KClientSession;
use super::k_resource_limit::LimitableResource;
use super::k_server_session::KServerSession;
use super::k_session_request::KSessionRequest;

fn trace_host_thread_ipc_stage(stage_id: u64, object_id: u64) {
    if common::trace::is_enabled(common::trace::cat::HOST_THREAD_IPC) {
        common::trace::emit_raw(common::trace::cat::HOST_THREAD_IPC, &[stage_id, object_id]);
    }
}

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
    /// Whether the process `SessionCountMax` reservation was committed.
    session_resource_committed: bool,
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
            session_resource_committed: false,
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

    pub fn mark_session_resource_committed(&mut self) {
        self.session_resource_committed = true;
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
    pub fn on_request(&self, request: Arc<Mutex<KSessionRequest>>) -> u32 {
        self.server.lock().unwrap().on_request(request)
    }

    /// Forward a request to the server session when the owner process is
    /// already held by the caller.
    pub fn on_request_with_process(
        &self,
        process: &mut super::k_process::KProcess,
        request: Arc<Mutex<KSessionRequest>>,
    ) -> u32 {
        let _ = process;
        self.on_request_defer_scheduler_unlock(request)
    }

    /// Forward a request while deferring scheduler unlock until after the
    /// Rust `KServerSession` mutex is released.
    pub fn on_request_defer_scheduler_unlock(&self, request: Arc<Mutex<KSessionRequest>>) -> u32 {
        Self::on_server_request_defer_scheduler_unlock(&self.server, self.name as u64, request)
    }

    /// Forward a request through an already-resolved server endpoint.
    ///
    /// This lets callers clone `KSession::server` under a short `KSession`
    /// mutex borrow, then drop the parent-session mutex before `OnRequest`
    /// performs the scheduler-unlock handoff.
    pub fn on_server_request_defer_scheduler_unlock(
        server_endpoint: &Arc<Mutex<KServerSession>>,
        trace_object_id: u64,
        request: Arc<Mutex<KSessionRequest>>,
    ) -> u32 {
        let profile_phases = std::env::var_os("RUZU_PROFILE_IPC_PHASES").is_some();
        let mut phase_last = profile_phases.then(std::time::Instant::now);
        let mut record_phase = |label: &'static str, last: &mut Option<std::time::Instant>| {
            if let Some(t) = last {
                crate::hle::kernel::svc::svc_ipc::record_ipc_phase(label, t.elapsed());
                *last = Some(std::time::Instant::now());
            }
        };

        trace_host_thread_ipc_stage(23, trace_object_id);
        let mut server = server_endpoint.lock().unwrap();
        record_phase("ksession_01_server_lock", &mut phase_last);
        trace_host_thread_ipc_stage(24, trace_object_id);
        trace_host_thread_ipc_stage(25, trace_object_id);
        let (result, scheduler_lock) = server.on_request_defer_scheduler_unlock(request);
        record_phase("ksession_02_on_request", &mut phase_last);
        drop(server);
        record_phase("ksession_03_drop_server_lock", &mut phase_last);
        drop(scheduler_lock);
        record_phase("ksession_04_drop_scheduler_lock", &mut phase_last);
        trace_host_thread_ipc_stage(26, trace_object_id);
        result
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
        if self.get_state() == SessionState::Normal {
            self.set_state(SessionState::ServerClosed);
            self.client.lock().unwrap().on_server_closed();
        }
    }

    /// Called when the client side is closed.
    pub fn on_client_closed(&mut self) {
        if self.get_state() == SessionState::Normal {
            self.set_state(SessionState::ClientClosed);
            self.server.lock().unwrap().on_client_closed();
        }
    }

    /// Called when the client side is closed, with the owning process already
    /// held by the caller.
    pub fn on_client_closed_with_process(&mut self, process: &mut super::k_process::KProcess) {
        if self.get_state() == SessionState::Normal {
            self.set_state(SessionState::ClientClosed);
            self.server
                .lock()
                .unwrap()
                .on_client_closed_with_process(process);
        }
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

    /// Finalize the session when the owning process object table is available.
    ///
    /// This is the Rust counterpart to upstream `KSession::Finalize`: the
    /// stored `port_id` identifies the parent `KClientPort`, so finalization
    /// can notify it that one accepted session has gone away.
    pub fn finalize_with_process(&mut self, process: &super::k_process::KProcess) {
        if let Some(port_id) = self.port_id.take() {
            if let Some(port) = process.get_client_port_by_object_id(port_id) {
                port.lock().unwrap().client.on_session_finalized(port_id);
            }
        }
        if self.session_resource_committed {
            if let Some(ref rl) = process.resource_limit {
                rl.lock()
                    .unwrap()
                    .release(LimitableResource::SessionCountMax, 1);
            }
            self.session_resource_committed = false;
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

    #[test]
    fn test_on_server_closed_only_transitions_from_normal() {
        let mut session = KSession::new();
        session.initialize(None, 0);
        session.on_server_closed();
        assert!(session.is_server_closed());

        session.on_server_closed();
        assert!(session.is_server_closed());
    }

    #[test]
    fn test_on_client_closed_only_transitions_from_normal() {
        let mut session = KSession::new();
        session.initialize(None, 0);
        session.on_client_closed();
        assert!(session.is_client_closed());
        assert!(session.server.lock().unwrap().client_closed);

        let mut already_server_closed = KSession::new();
        already_server_closed.initialize(None, 0);
        already_server_closed.on_server_closed();
        already_server_closed.on_client_closed();
        assert_eq!(
            already_server_closed.get_state(),
            SessionState::ServerClosed
        );
        assert!(!already_server_closed.server.lock().unwrap().client_closed);
    }
}
