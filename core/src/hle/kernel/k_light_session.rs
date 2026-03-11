//! Port of zuyu/src/core/hle/kernel/k_light_session.h / k_light_session.cpp
//! Status: Partial (structural port)
//! Derniere synchro: 2026-03-11
//!
//! KLightSession: a lightweight IPC session with embedded server and client endpoints.

/// Light session data size: 7 u32 words.
/// Matches upstream `KLightSession::DataSize`.
pub const LIGHT_SESSION_DATA_SIZE: usize = std::mem::size_of::<u32>() * 7;

/// Reply flag bit.
/// Matches upstream `KLightSession::ReplyFlag`.
pub const REPLY_FLAG: u32 = 1u32 << 31;

/// Light session state.
/// Matches upstream `KLightSession::State`.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LightSessionState {
    Invalid = 0,
    Normal = 1,
    ClientClosed = 2,
    ServerClosed = 3,
}

impl Default for LightSessionState {
    fn default() -> Self {
        Self::Invalid
    }
}

/// The kernel light session object.
/// Matches upstream `KLightSession` class (k_light_session.h).
pub struct KLightSession {
    pub server_session_id: u64,
    pub client_session_id: u64,
    pub state: LightSessionState,
    pub port_id: Option<u64>,
    pub name: usize,
    pub process_id: Option<u64>,
    pub initialized: bool,
}

impl KLightSession {
    pub fn new() -> Self {
        Self {
            server_session_id: 0,
            client_session_id: 0,
            state: LightSessionState::Invalid,
            port_id: None,
            name: 0,
            process_id: None,
            initialized: false,
        }
    }

    /// Initialize the light session.
    /// Matches upstream `KLightSession::Initialize`.
    pub fn initialize(&mut self, client_port_id: Option<u64>, name: usize) {
        self.port_id = client_port_id;
        self.name = name;
        self.state = LightSessionState::Normal;
        self.initialized = true;
    }

    pub fn is_initialized(&self) -> bool {
        self.initialized
    }

    pub fn is_server_closed(&self) -> bool {
        self.state != LightSessionState::Normal
    }

    pub fn is_client_closed(&self) -> bool {
        self.state != LightSessionState::Normal
    }

    /// Called when the server side is closed.
    /// TODO: Port from k_light_session.cpp.
    pub fn on_server_closed(&mut self) {
        self.state = LightSessionState::ServerClosed;
    }

    /// Called when the client side is closed.
    /// TODO: Port from k_light_session.cpp.
    pub fn on_client_closed(&mut self) {
        self.state = LightSessionState::ClientClosed;
    }

    /// Finalize the light session.
    /// TODO: Port from k_light_session.cpp.
    pub fn finalize(&mut self) {
        // TODO: Full implementation
    }
}

impl Default for KLightSession {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_light_session_constants() {
        assert_eq!(LIGHT_SESSION_DATA_SIZE, 28);
        assert_eq!(REPLY_FLAG, 0x8000_0000);
    }

    #[test]
    fn test_light_session_state() {
        assert_eq!(LightSessionState::Invalid as u8, 0);
        assert_eq!(LightSessionState::Normal as u8, 1);
        assert_eq!(LightSessionState::ClientClosed as u8, 2);
        assert_eq!(LightSessionState::ServerClosed as u8, 3);
    }
}
