//! Port of zuyu/src/core/hle/kernel/k_port.h / k_port.cpp
//! Status: Partial (structural port)
//! Derniere synchro: 2026-03-11
//!
//! KPort: a kernel port object containing server and client endpoints.

/// Port state.
/// Matches upstream `KPort::State` (k_port.h).
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PortState {
    Invalid = 0,
    Normal = 1,
    ClientClosed = 2,
    ServerClosed = 3,
}

impl Default for PortState {
    fn default() -> Self {
        Self::Invalid
    }
}

/// The kernel port object.
/// Matches upstream `KPort` class (k_port.h).
pub struct KPort {
    pub server_port_id: u64,
    pub client_port_id: u64,
    pub name: usize, // uintptr_t
    pub state: PortState,
    pub is_light: bool,
}

impl KPort {
    pub fn new() -> Self {
        Self {
            server_port_id: 0,
            client_port_id: 0,
            name: 0,
            state: PortState::Invalid,
            is_light: false,
        }
    }

    /// Initialize the port.
    /// Matches upstream `KPort::Initialize`.
    pub fn initialize(&mut self, _max_sessions: i32, is_light: bool, name: usize) {
        self.is_light = is_light;
        self.name = name;
        self.state = PortState::Normal;
    }

    pub fn get_name(&self) -> usize {
        self.name
    }

    pub fn is_light(&self) -> bool {
        self.is_light
    }

    pub fn is_server_closed(&self) -> bool {
        self.state == PortState::ServerClosed
    }

    /// Called when the client side is closed.
    /// TODO: Port from k_port.cpp.
    pub fn on_client_closed(&mut self) {
        self.state = PortState::ClientClosed;
        // TODO: Full implementation
    }

    /// Called when the server side is closed.
    /// TODO: Port from k_port.cpp.
    pub fn on_server_closed(&mut self) {
        self.state = PortState::ServerClosed;
        // TODO: Full implementation
    }

    /// Enqueue a session on the server port.
    /// TODO: Port from k_port.cpp.
    pub fn enqueue_session(&mut self, _session_id: u64) -> u32 {
        // TODO: Full implementation
        0
    }
}

impl Default for KPort {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_port_state_values() {
        assert_eq!(PortState::Invalid as u8, 0);
        assert_eq!(PortState::Normal as u8, 1);
        assert_eq!(PortState::ClientClosed as u8, 2);
        assert_eq!(PortState::ServerClosed as u8, 3);
    }
}
