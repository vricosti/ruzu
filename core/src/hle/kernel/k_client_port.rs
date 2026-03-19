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
    ///
    /// Matches upstream `KClientPort::OnServerClosed()`.
    /// TODO: full implementation (notifies waiting threads).
    pub fn on_server_closed(&mut self) {
        // TODO: KScopedSchedulerLock + NotifyAvailable
    }

    /// Is the port signaled?
    ///
    /// Matches upstream `KClientPort::IsSignaled()`.
    /// TODO: delegate to parent to check server closed state.
    pub fn is_signaled(&self) -> bool {
        false // TODO: return m_parent->IsServerClosed()
    }

    /// Create a new session via this client port.
    ///
    /// Matches upstream `KClientPort::CreateSession(KClientSession** out)`.
    /// Upstream uses atomic CAS to increment session count, creates a KSession,
    /// enqueues the server session on the parent port, and returns the client session.
    /// TODO: full implementation with proper session objects.
    pub fn create_session(&self) -> u32 {
        // Increment session count (matching upstream's atomic CAS pattern).
        let cur = self.num_sessions.fetch_add(1, Ordering::Relaxed);
        // Update peak.
        let peak = self.peak_sessions.load(Ordering::Relaxed);
        if cur + 1 > peak {
            self.peak_sessions.store(cur + 1, Ordering::Relaxed);
        }
        // TODO: actually create KSession, enqueue on parent, return client session
        0
    }

    /// Destroy the client port.
    /// TODO: full implementation.
    pub fn destroy(&mut self) {
        // TODO: close parent port reference
    }
}

impl Default for KClientPort {
    fn default() -> Self {
        Self::new()
    }
}
