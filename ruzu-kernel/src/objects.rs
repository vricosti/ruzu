// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use ruzu_common::{Handle, VAddr};
use std::collections::VecDeque;
use std::sync::Arc;

use parking_lot::Mutex;

/// All kernel object types wrapped in an enum for the handle table.
#[derive(Debug, Clone)]
pub enum KernelObject {
    Event(KEvent),
    SharedMemory(KSharedMemory),
    Port(KPort),
    ClientPort(KClientPort),
    ServerPort(KServerPort),
    Session(KSession),
    ClientSession(KClientSession),
    ServerSession(KServerSession),
    TransferMemory(KTransferMemory),
    Thread(Handle),
    Process(Handle),
}

/// Kernel event: a signalable synchronization primitive.
#[derive(Debug, Clone)]
pub struct KEvent {
    pub signaled: bool,
    pub readable_handle: Option<Handle>,
    pub writable_handle: Option<Handle>,
}

impl KEvent {
    pub fn new() -> Self {
        Self {
            signaled: false,
            readable_handle: None,
            writable_handle: None,
        }
    }

    pub fn signal(&mut self) {
        self.signaled = true;
    }

    pub fn clear(&mut self) {
        self.signaled = false;
    }
}

impl Default for KEvent {
    fn default() -> Self {
        Self::new()
    }
}

/// Shared memory: a region of memory that can be mapped into multiple processes.
#[derive(Debug, Clone)]
pub struct KSharedMemory {
    pub size: usize,
    pub backing_offset: Option<usize>,
    pub owner_handle: Handle,
}

impl KSharedMemory {
    pub fn new(size: usize) -> Self {
        Self {
            size,
            backing_offset: None,
            owner_handle: 0,
        }
    }
}

/// Port: server-side endpoint for IPC connections.
#[derive(Debug, Clone)]
pub struct KPort {
    pub name: String,
    pub max_sessions: u32,
    pub server_handle: Option<Handle>,
    pub client_handle: Option<Handle>,
}

impl KPort {
    pub fn new(name: String, max_sessions: u32) -> Self {
        Self {
            name,
            max_sessions,
            server_handle: None,
            client_handle: None,
        }
    }
}

/// Client port: client-side handle to a port.
#[derive(Debug, Clone)]
pub struct KClientPort {
    pub port_name: String,
    pub session_count: u32,
    pub max_sessions: u32,
}

impl KClientPort {
    pub fn new(name: String, max_sessions: u32) -> Self {
        Self {
            port_name: name,
            session_count: 0,
            max_sessions,
        }
    }
}

/// Server port: server-side handle to a port.
#[derive(Debug, Clone)]
pub struct KServerPort {
    pub port_name: String,
}

impl KServerPort {
    pub fn new(name: String) -> Self {
        Self { port_name: name }
    }
}

/// IPC session between client and server.
#[derive(Debug, Clone)]
pub struct KSession {
    pub service_name: String,
    pub client_handle: Option<Handle>,
    pub server_handle: Option<Handle>,
}

impl KSession {
    pub fn new(service_name: String) -> Self {
        Self {
            service_name,
            client_handle: None,
            server_handle: None,
        }
    }
}

/// Client-side session handle.
#[derive(Debug, Clone)]
pub struct KClientSession {
    pub service_name: String,
    pub parent_session: Option<Handle>,
}

impl KClientSession {
    pub fn new(service_name: String) -> Self {
        Self {
            service_name,
            parent_session: None,
        }
    }
}

/// Server-side session handle.
#[derive(Debug, Clone)]
pub struct KServerSession {
    pub service_name: String,
    pub parent_session: Option<Handle>,
    pub request_queue: Arc<Mutex<VecDeque<IpcRequest>>>,
}

impl KServerSession {
    pub fn new(service_name: String) -> Self {
        Self {
            service_name,
            parent_session: None,
            request_queue: Arc::new(Mutex::new(VecDeque::new())),
        }
    }
}

/// An IPC request waiting to be processed.
#[derive(Debug, Clone)]
pub struct IpcRequest {
    pub thread_handle: Handle,
    pub tls_addr: VAddr,
}

/// Transfer memory: a region of memory transferred between processes.
#[derive(Debug, Clone)]
pub struct KTransferMemory {
    pub addr: VAddr,
    pub size: usize,
    pub owner_handle: Handle,
}

impl KTransferMemory {
    pub fn new(addr: VAddr, size: usize) -> Self {
        Self {
            addr,
            size,
            owner_handle: 0,
        }
    }
}
