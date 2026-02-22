// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use ruzu_common::{Handle, ProcessId, VAddr, NRO_BASE_ADDRESS};

use crate::process::KProcess;
use crate::scheduler::Scheduler;
use crate::thread::ThreadState;

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
