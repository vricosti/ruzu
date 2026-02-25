// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use std::sync::Arc;

use ruzu_common::{Handle, ProcessId, ResultCode, VAddr, NRO_BASE_ADDRESS};

use crate::objects::{KEvent, KernelObject};
use crate::process::KProcess;
use crate::scheduler::Scheduler;
use crate::thread::{ThreadState, WaitReason};

/// Result returned by an IPC handler after processing a request.
pub struct IpcHandlerResult {
    /// 0x100-byte TLS response to write back.
    pub response_bytes: Vec<u8>,
    /// If `Some`, the bridge should create a new `KClientSession` for this service name.
    pub create_session_for: Option<String>,
    /// Handles to copy into the response handle descriptor.
    pub copy_handles: Vec<u32>,
    /// Handles to move into the response handle descriptor.
    pub move_handles: Vec<u32>,
    /// Data to write directly into guest memory B-type output buffers.
    /// Each entry is `(guest_addr, data_bytes)`.
    pub out_buf_writes: Vec<(u64, Vec<u8>)>,
}

/// Trait for handling IPC requests. Bridges the kernel SVC layer to the service framework
/// without creating a circular dependency between `ruzu-kernel` and `ruzu-service`.
pub trait IpcHandler: Send + Sync {
    fn handle_ipc(&self, service_name: &str, tls_data: &[u8]) -> IpcHandlerResult;
}

/// KernelCore: orchestrates all kernel subsystems.
pub struct KernelCore {
    /// The single process (Phase 1: only one process at a time).
    pub process: Option<KProcess>,
    /// Priority-aware round-robin scheduler.
    pub scheduler: Scheduler,
    /// Next process ID.
    next_pid: ProcessId,
    /// Whether the emulation should stop.
    pub should_stop: bool,
    /// Monotonic tick counter (incremented by instruction budget each slice).
    pub tick_counter: u64,
    /// Index of the currently executing thread (set by the main loop before SVC dispatch).
    pub current_thread_idx: Option<usize>,
    /// Backing store for shared memory regions.
    pub shared_memory_pool: Vec<u8>,
    /// IPC handler bridge: dispatches SendSyncRequest to the service framework.
    pub ipc_handler: Option<Arc<dyn IpcHandler>>,
}

impl KernelCore {
    pub fn new() -> Self {
        Self {
            process: None,
            scheduler: Scheduler::new(),
            next_pid: 1,
            should_stop: false,
            tick_counter: 0,
            current_thread_idx: None,
            shared_memory_pool: Vec::new(),
            ipc_handler: None,
        }
    }

    /// Create a new process and load code into it.
    pub fn create_process(&mut self, name: &str) -> anyhow::Result<()> {
        let pid = self.next_pid;
        self.next_pid += 1;

        let process = KProcess::new(pid, name.to_string());
        self.process = Some(process);

        Ok(())
    }

    /// Load an NRO's code set into the current process.
    pub fn load_nro(
        &mut self,
        text: &[u8],
        rodata: &[u8],
        data: &[u8],
        bss_size: usize,
        text_offset: u32,
        rodata_offset: u32,
        data_offset: u32,
    ) -> anyhow::Result<VAddr> {
        let process = self
            .process
            .as_mut()
            .ok_or_else(|| anyhow::anyhow!("No process created"))?;

        let base = NRO_BASE_ADDRESS;

        // Map text segment (executable)
        process.load_code(base + text_offset as u64, text)?;

        // Map rodata segment (read-only)
        let rodata_addr = base + rodata_offset as u64;
        let rodata_size = ruzu_common::align_up(rodata.len() as u64, ruzu_common::PAGE_SIZE_U64);
        process.memory.map(
            rodata_addr,
            rodata_size,
            crate::memory_manager::MemoryPermission::READ,
            crate::memory_manager::MemoryState::Code,
        )?;
        process.memory.write_bytes(rodata_addr, rodata)?;

        // Map data + BSS segment (read-write)
        let data_total_size = data.len() + bss_size;
        process.map_data(base + data_offset as u64, data, data_total_size)?;

        Ok(base)
    }

    /// Create the main thread and prepare for execution.
    pub fn create_main_thread(&mut self, entry_point: VAddr) -> anyhow::Result<Handle> {
        let process = self
            .process
            .as_mut()
            .ok_or_else(|| anyhow::anyhow!("No process created"))?;

        // Allocate stack
        let stack_top = process.allocate_stack(ruzu_common::DEFAULT_STACK_SIZE)?;

        // Create main thread
        let thread_handle = process.create_main_thread(entry_point, stack_top, 44)?;

        // Start the thread
        if let Some(thread) = process.main_thread_mut() {
            thread.start();
            thread.state = ThreadState::Running;
        }

        process.is_running = true;

        Ok(thread_handle)
    }

    /// Get a reference to the current process.
    pub fn process(&self) -> Option<&KProcess> {
        self.process.as_ref()
    }

    /// Get a mutable reference to the current process.
    pub fn process_mut(&mut self) -> Option<&mut KProcess> {
        self.process.as_mut()
    }

    /// Create a KEvent in the process handle table and return the handle.
    ///
    /// Returns `None` if there is no process or the handle table is full.
    pub fn create_event_in_process(&mut self) -> Option<Handle> {
        let process = self.process.as_mut()?;
        let event = KernelObject::Event(KEvent::new());
        process.handle_table.add(event).ok()
    }

    /// Signal a kernel event by handle and wake any threads waiting on it.
    pub fn signal_event(&mut self, handle: Handle) {
        let process = match self.process.as_mut() {
            Some(p) => p,
            None => return,
        };

        // Mark the event as signaled.
        if let Ok(KernelObject::Event(ev)) = process.handle_table.get_mut(handle) {
            ev.signaled = true;
        }

        // Wake any threads blocked in WaitSynchronization on this handle.
        for thread in process.threads.iter_mut() {
            if thread.state != ThreadState::Waiting {
                continue;
            }
            let wake_idx = if let WaitReason::Synchronization { ref handles, .. } = thread.wait_reason {
                handles.iter().position(|&h| h == handle)
            } else {
                None
            };
            if let Some(idx) = wake_idx {
                thread.wake(ResultCode::SUCCESS.raw(), idx as i32);
            }
        }
    }

    /// Signal that emulation should stop.
    pub fn stop(&mut self) {
        self.should_stop = true;
    }
}

impl Default for KernelCore {
    fn default() -> Self {
        Self::new()
    }
}
