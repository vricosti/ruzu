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
    /// Full implementation requires KSession slab allocation and parent port enqueueing.
    /// The atomic session count management is implemented here.
    pub fn create_session(&self) -> u32 {
        // Atomically increment the number of sessions (CAS pattern matching upstream).
        let max = self.max_sessions;
        loop {
            let cur_sessions = self.num_sessions.load(Ordering::Acquire);
            if cur_sessions >= max {
                return 1; // ResultOutOfSessions
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
                // Atomically update peak tracking.
                loop {
                    let peak = self.peak_sessions.load(Ordering::Acquire);
                    if peak >= new_sessions {
                        break;
                    }
                    if self
                        .peak_sessions
                        .compare_exchange_weak(
                            peak,
                            new_sessions,
                            Ordering::Relaxed,
                            Ordering::Relaxed,
                        )
                        .is_ok()
                    {
                        break;
                    }
                }
                break;
            }
        }
        // Session object creation and parent port enqueueing require KSession slab allocation.
        0 // ResultSuccess
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
