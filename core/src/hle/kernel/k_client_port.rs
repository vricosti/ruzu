//! Port of zuyu/src/core/hle/kernel/k_client_port.h / k_client_port.cpp
//! Status: Partial (structural port)
//! Derniere synchro: 2026-03-19
//!
//! KClientPort: the client endpoint of a port, used to create sessions.
//!
//! Upstream `KClientPort` stores a back-pointer to its parent `KPort*`.
//! Here we omit the back-pointer since `KPort` owns both endpoints inline
//! and callers can navigate through the parent.

use std::sync::atomic::{AtomicI32, Ordering};

use super::k_process::KProcess;
use super::k_resource_limit::LimitableResource;
use super::k_scoped_resource_reservation::KScopedResourceReservation;
use super::k_session::KSession;
use super::kernel::KernelCore;
use super::svc::svc_results::{RESULT_LIMIT_REACHED, RESULT_OUT_OF_SESSIONS};
use crate::hle::result::ResultCode;

/// The client port object.
///
/// Matches upstream `KClientPort` class (k_client_port.h):
/// ```cpp
/// class KClientPort final : public KSynchronizationObject {
///     std::atomic<s32> m_num_sessions{};
///     std::atomic<s32> m_peak_sessions{};
///     s32 m_max_sessions{};
///     KPort* m_parent{};
/// };
/// ```
pub struct KClientPort {
    pub num_sessions: AtomicI32,
    pub peak_sessions: AtomicI32,
    pub max_sessions: i32,
    // Note: upstream has `KPort* m_parent` for back-navigation.
    // Omitted because KPort owns KClientPort inline; callers navigate via parent.
}

impl KClientPort {
    pub fn new() -> Self {
        Self {
            num_sessions: AtomicI32::new(0),
            peak_sessions: AtomicI32::new(0),
            max_sessions: 0,
        }
    }

    /// Initialize with a maximum session count.
    ///
    /// Matches upstream `KClientPort::Initialize(KPort* parent, s32 max_sessions)`.
    /// We omit the parent pointer since KPort owns us inline.
    pub fn initialize(&mut self, max_sessions: i32) {
        self.max_sessions = max_sessions;
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
    ///
    /// Matches upstream `KClientPort::OnSessionFinalized()`.
    pub fn on_session_finalized(&self) {
        self.num_sessions.fetch_sub(1, Ordering::Relaxed);
    }

    /// Called when the server side is closed.
    /// Port of upstream `KClientPort::OnServerClosed`.
    /// Upstream is a no-op.
    pub fn on_server_closed(&mut self) {
        // Upstream: empty body.
    }

    /// Is the port signaled?
    /// Port of upstream `KClientPort::IsSignaled`.
    /// Returns true when session count is below max (sessions available).
    pub fn is_signaled(&self) -> bool {
        self.num_sessions.load(Ordering::Relaxed) < self.max_sessions
    }

    /// Create a new session via this client port.
    /// Port of upstream `KClientPort::CreateSession`.
    ///
    /// Because the Rust `KClientPort` omits upstream's `m_parent` back-pointer,
    /// the caller still performs the final `KPort::EnqueueSession(...)` step
    /// after this method returns the created object ids.
    pub fn create_session(
        &self,
        process: &mut KProcess,
        kernel: &KernelCore,
        port_name: usize,
    ) -> Result<(u64, u64), ResultCode> {
        let mut session_reservation = KScopedResourceReservation::new(
            process.resource_limit.clone(),
            LimitableResource::SessionCountMax,
            1,
        );
        if !session_reservation.succeeded() {
            return Err(RESULT_LIMIT_REACHED);
        }

        // Atomically increment the number of sessions (CAS pattern matching upstream).
        let max = self.max_sessions;
        let new_sessions = loop {
            let cur_sessions = self.num_sessions.load(Ordering::Acquire);
            if cur_sessions >= max {
                return Err(RESULT_OUT_OF_SESSIONS);
            }
            let new_sessions = cur_sessions + 1;
            if self
                .num_sessions
                .compare_exchange_weak(
                    cur_sessions,
                    new_sessions,
                    Ordering::Relaxed,
                    Ordering::Relaxed,
                )
                .is_ok()
            {
                break new_sessions;
            }
        };

        // Atomically update peak tracking.
        loop {
            let peak = self.peak_sessions.load(Ordering::Acquire);
            if peak >= new_sessions {
                break;
            }
            if self
                .peak_sessions
                .compare_exchange_weak(peak, new_sessions, Ordering::Relaxed, Ordering::Relaxed)
                .is_ok()
            {
                break;
            }
        }

        let session_object_id = kernel.create_new_object_id() as u64;
        let client_session_object_id = kernel.create_new_object_id() as u64;

        let session = std::sync::Arc::new(std::sync::Mutex::new(KSession::new()));
        {
            let mut session_guard = session.lock().unwrap();
            session_guard.initialize(None, port_name);
            session_guard
                .get_client_session()
                .lock()
                .unwrap()
                .initialize(session_object_id);
            session_guard
                .get_server_session()
                .lock()
                .unwrap()
                .initialize(session_object_id);
        }

        process.register_session_object(session_object_id, session.clone());
        let client_session = session.lock().unwrap().get_client_session().clone();
        process.register_client_session_object(client_session_object_id, client_session);

        session_reservation.commit();
        Ok((session_object_id, client_session_object_id))
    }

    /// Destroy the client port.
    /// Port of upstream `KClientPort::Destroy`.
    pub fn destroy(&mut self) {
        // Upstream: m_parent->OnClientClosed(); m_parent->Close();
        // Parent reference cleanup is handled by the port object system.
    }
}

impl Default for KClientPort {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::System;

    #[test]
    fn create_session_allocates_and_registers_session_objects() {
        let mut system = System::new_for_test();
        let process = std::sync::Arc::new(std::sync::Mutex::new(KProcess::new()));
        {
            let mut process_guard = process.lock().unwrap();
            process_guard.process_id = 1;
            process_guard.initialize_handle_table();
        }
        system.set_current_process_arc(process.clone());

        let kernel = system.kernel().unwrap();
        let mut port = KPort::new();
        port.initialize(4, false, 0x44);

        let (session_object_id, client_session_object_id) = port
            .client
            .create_session(&mut process.lock().unwrap(), kernel, port.get_name())
            .unwrap();

        let process_guard = process.lock().unwrap();
        assert!(process_guard
            .get_session_by_object_id(session_object_id)
            .is_some());
        assert!(process_guard
            .get_client_session_by_object_id(client_session_object_id)
            .is_some());
    }
}
