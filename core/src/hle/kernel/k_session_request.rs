//! Port of zuyu/src/core/hle/kernel/k_session_request.h / k_session_request.cpp
//! Status: Partial (structural port)
//! Derniere synchro: 2026-03-11
//!
//! KSessionRequest: represents an IPC request in flight, with buffer mappings.

use std::sync::{Arc, Mutex, Weak};

use super::k_memory_block::KMemoryState;
use super::k_typed_address::KProcessAddress;

/// Number of statically allocated mapping slots.
const NUM_STATIC_MAPPINGS: usize = 8;

/// A single buffer mapping entry.
/// Matches upstream `KSessionRequest::SessionMappings::Mapping`.
#[derive(Clone)]
pub struct Mapping {
    pub client_address: KProcessAddress,
    pub server_address: KProcessAddress,
    pub size: usize,
    pub state: KMemoryState,
}

impl Default for Mapping {
    fn default() -> Self {
        Self {
            client_address: KProcessAddress::new(0),
            server_address: KProcessAddress::new(0),
            size: 0,
            state: KMemoryState::NONE,
        }
    }
}

impl Mapping {
    pub fn set(
        &mut self,
        client: KProcessAddress,
        server: KProcessAddress,
        size: usize,
        state: KMemoryState,
    ) {
        self.client_address = client;
        self.server_address = server;
        self.size = size;
        self.state = state;
    }
}

/// Session mappings: manages send/receive/exchange buffer descriptors.
/// Matches upstream `KSessionRequest::SessionMappings`.
pub struct SessionMappings {
    pub static_mappings: [Mapping; NUM_STATIC_MAPPINGS],
    pub dynamic_mappings: Vec<Mapping>,
    pub num_send: u8,
    pub num_recv: u8,
    pub num_exch: u8,
}

impl SessionMappings {
    pub fn new() -> Self {
        Self {
            static_mappings: Default::default(),
            dynamic_mappings: Vec::new(),
            num_send: 0,
            num_recv: 0,
            num_exch: 0,
        }
    }

    pub fn initialize(&mut self) {
        // No-op in upstream
    }

    pub fn finalize(&mut self) {
        self.dynamic_mappings.clear();
        self.num_send = 0;
        self.num_recv = 0;
        self.num_exch = 0;
    }

    pub fn get_send_count(&self) -> usize {
        self.num_send as usize
    }

    pub fn get_receive_count(&self) -> usize {
        self.num_recv as usize
    }

    pub fn get_exchange_count(&self) -> usize {
        self.num_exch as usize
    }

    /// Push a send mapping.
    pub fn push_send(
        &mut self,
        client: KProcessAddress,
        server: KProcessAddress,
        size: usize,
        state: KMemoryState,
    ) -> u32 {
        debug_assert_eq!(self.num_recv, 0);
        debug_assert_eq!(self.num_exch, 0);
        let index = self.num_send as usize;
        self.num_send += 1;
        self.push_map(client, server, size, state, index)
    }

    /// Push a receive mapping.
    pub fn push_receive(
        &mut self,
        client: KProcessAddress,
        server: KProcessAddress,
        size: usize,
        state: KMemoryState,
    ) -> u32 {
        debug_assert_eq!(self.num_exch, 0);
        let index = (self.num_send + self.num_recv) as usize;
        self.num_recv += 1;
        self.push_map(client, server, size, state, index)
    }

    /// Push an exchange mapping.
    pub fn push_exchange(
        &mut self,
        client: KProcessAddress,
        server: KProcessAddress,
        size: usize,
        state: KMemoryState,
    ) -> u32 {
        let index = (self.num_send + self.num_recv + self.num_exch) as usize;
        self.num_exch += 1;
        self.push_map(client, server, size, state, index)
    }

    fn push_map(
        &mut self,
        client: KProcessAddress,
        server: KProcessAddress,
        size: usize,
        state: KMemoryState,
        index: usize,
    ) -> u32 {
        if index < NUM_STATIC_MAPPINGS {
            self.static_mappings[index].set(client, server, size, state);
        } else {
            let dyn_index = index - NUM_STATIC_MAPPINGS;
            if dyn_index >= self.dynamic_mappings.len() {
                self.dynamic_mappings
                    .resize(dyn_index + 1, Mapping::default());
            }
            self.dynamic_mappings[dyn_index].set(client, server, size, state);
        }
        0 // ResultSuccess
    }

    fn get_mapping(&self, index: usize) -> &Mapping {
        if index < NUM_STATIC_MAPPINGS {
            &self.static_mappings[index]
        } else {
            &self.dynamic_mappings[index - NUM_STATIC_MAPPINGS]
        }
    }

    pub fn get_send_mapping(&self, i: usize) -> &Mapping {
        self.get_mapping(i)
    }

    pub fn get_receive_mapping(&self, i: usize) -> &Mapping {
        self.get_mapping(self.num_send as usize + i)
    }

    pub fn get_exchange_mapping(&self, i: usize) -> &Mapping {
        self.get_mapping((self.num_send + self.num_recv) as usize + i)
    }
}

impl Default for SessionMappings {
    fn default() -> Self {
        Self::new()
    }
}

/// A session request (IPC message in flight).
/// Matches upstream `KSessionRequest` class (k_session_request.h).
pub struct KSessionRequest {
    pub mappings: SessionMappings,
    pub thread: Option<Weak<Mutex<super::k_thread::KThread>>>,
    pub thread_id: Option<u64>,
    pub client_process_id: Option<u64>,
    pub server_process_id: Option<u64>,
    pub event_id: Option<u64>,
    pub address: usize,
    pub size: usize,
}

impl KSessionRequest {
    pub fn new() -> Self {
        Self {
            mappings: SessionMappings::new(),
            thread: None,
            thread_id: None,
            client_process_id: None,
            server_process_id: None,
            event_id: None,
            address: 0,
            size: 0,
        }
    }

    fn initialize_impl(
        &mut self,
        thread: Option<Arc<Mutex<super::k_thread::KThread>>>,
        thread_id: Option<u64>,
        client_process_id: Option<u64>,
        event_id: Option<u64>,
        address: usize,
        size: usize,
    ) {
        self.mappings.initialize();
        self.thread = thread.as_ref().map(Arc::downgrade);
        self.thread_id = thread_id;
        self.client_process_id = client_process_id;
        self.server_process_id = None;
        self.event_id = event_id;
        self.address = address;
        self.size = size;
    }

    /// Initialize the request.
    pub fn initialize(&mut self, event_id: Option<u64>, address: usize, size: usize) {
        let current_thread = super::kernel::get_current_thread_pointer();
        let thread_id = current_thread
            .as_ref()
            .map(|thread| thread.lock().unwrap().thread_id);
        let client_process_id = current_thread
            .as_ref()
            .and_then(|thread| thread.lock().unwrap().parent.as_ref()?.upgrade())
            .map(|process| process.lock().unwrap().process_id);
        self.initialize_impl(
            current_thread,
            thread_id,
            client_process_id,
            event_id,
            address,
            size,
        );
    }

    /// Initialize the request when the caller already holds the owning
    /// process and must not re-enter its mutex through the current-thread
    /// parent lookup.
    pub fn initialize_with_process(
        &mut self,
        process: &super::k_process::KProcess,
        event_id: Option<u64>,
        address: usize,
        size: usize,
    ) {
        let current_thread = super::kernel::get_current_thread_pointer();
        let thread_id = current_thread
            .as_ref()
            .map(|thread| thread.lock().unwrap().thread_id);
        self.initialize_impl(
            current_thread,
            thread_id,
            Some(process.process_id),
            event_id,
            address,
            size,
        );
    }

    pub fn get_thread(&self) -> Option<Arc<Mutex<super::k_thread::KThread>>> {
        self.thread.as_ref()?.upgrade()
    }

    pub fn get_thread_id(&self) -> Option<u64> {
        self.thread_id
    }

    pub fn get_event_id(&self) -> Option<u64> {
        self.event_id
    }

    pub fn get_address(&self) -> usize {
        self.address
    }

    pub fn get_size(&self) -> usize {
        self.size
    }

    pub fn get_server_process_id(&self) -> Option<u64> {
        self.server_process_id
    }

    pub fn get_client_process_id(&self) -> Option<u64> {
        self.client_process_id
    }

    pub fn set_server_process(&mut self, process_id: u64) {
        self.server_process_id = Some(process_id);
    }

    pub fn clear_thread(&mut self) {
        self.thread = None;
        self.thread_id = None;
    }

    pub fn clear_event(&mut self) {
        self.event_id = None;
    }

    /// Finalize the request, releasing references.
    pub fn finalize(&mut self) {
        self.mappings.finalize();
        // Upstream: Close thread, event, and server process references.
        // Reference-counted handles are cleared here.
        self.thread = None;
        self.thread_id = None;
        self.client_process_id = None;
        self.event_id = None;
        self.server_process_id = None;
    }
}

impl Default for KSessionRequest {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn clear_thread_drops_direct_thread_owner_reference() {
        let thread = Arc::new(Mutex::new(super::super::k_thread::KThread::new()));
        let mut request = KSessionRequest::new();
        request.thread = Some(Arc::downgrade(&thread));
        request.thread_id = Some(7);

        assert!(request.get_thread().is_some());
        request.clear_thread();
        assert!(request.get_thread().is_none());
        assert!(request.get_thread_id().is_none());
    }
}
