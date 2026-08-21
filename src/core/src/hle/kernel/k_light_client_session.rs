//! Port of zuyu/src/core/hle/kernel/k_light_client_session.h / k_light_client_session.cpp
//! Status: Partial (structural port)
//! Derniere synchro: 2026-03-11
//!
//! KLightClientSession: the client endpoint of a light session.

/// The light client session object.
/// Matches upstream `KLightClientSession` class (k_light_client_session.h).
pub struct KLightClientSession {
    /// Parent KLightSession ID.
    pub parent_id: Option<u64>,
}

impl KLightClientSession {
    pub fn new() -> Self {
        Self { parent_id: None }
    }

    /// Initialize with a parent light session.
    pub fn initialize(&mut self, parent_id: u64) {
        self.parent_id = Some(parent_id);
    }

    pub fn get_parent_id(&self) -> Option<u64> {
        self.parent_id
    }

    /// Send a synchronous light IPC request.
    /// Port of upstream `KLightClientSession::SendSyncRequest`.
    /// Upstream gets the current thread, sets light session data, and calls parent->OnRequest.
    /// Requires KThread scheduling integration for blocking; returns success for now.
    pub fn send_sync_request(&mut self, _data: &mut [u32]) -> u32 {
        // Upstream: cur_thread->SetLightSessionData(data); m_parent->OnRequest(cur_thread);
        // Blocked by KThread scheduling integration.
        0 // ResultSuccess
    }

    /// Called when the server side is closed.
    /// Port of upstream `KLightClientSession::OnServerClosed`.
    /// Upstream is a no-op.
    pub fn on_server_closed(&mut self) {
        // Upstream: empty body.
    }

    /// Destroy the light client session.
    /// Port of upstream `KLightClientSession::Destroy`.
    pub fn destroy(&mut self) {
        // Upstream: m_parent->OnClientClosed();
        self.parent_id = None;
    }
}

impl Default for KLightClientSession {
    fn default() -> Self {
        Self::new()
    }
}
