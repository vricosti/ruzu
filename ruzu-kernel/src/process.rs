// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use ruzu_common::{
    Handle, ProcessId, ThreadId, VAddr, HEAP_REGION_BASE, NRO_BASE_ADDRESS,
    PAGE_SIZE, PAGE_SIZE_U64, STACK_REGION_BASE, TLS_ENTRY_SIZE,
};

use crate::handle_table::HandleTable;
use crate::memory_manager::{MemoryManager, MemoryPermission, MemoryState};
use crate::objects::{KClientSession, KernelObject};
use crate::thread::KThread;

/// Address space layout for a 39-bit process.
#[derive(Debug)]
pub struct AddressSpaceLayout {
    /// Code region base (where NRO is loaded).
    pub code_base: VAddr,
    /// Code region end.
    pub code_end: VAddr,
    /// Heap region base.
    pub heap_base: VAddr,
    /// Current heap end (grows with SetHeapSize).
    pub heap_end: VAddr,
    /// Stack region base.
    pub stack_base: VAddr,
    /// TLS region base.
    pub tls_base: VAddr,
    /// Next available TLS slot address.
    pub next_tls_addr: VAddr,
    /// Map region base (for shared memory, transfer memory).
    pub map_region_base: VAddr,
    /// Map region end.
    pub map_region_end: VAddr,
    /// Total address space size.
    pub address_space_size: u64,
}

impl Default for AddressSpaceLayout {
    fn default() -> Self {
        Self {
            code_base: NRO_BASE_ADDRESS,
            code_end: NRO_BASE_ADDRESS,
            heap_base: HEAP_REGION_BASE,
            heap_end: HEAP_REGION_BASE,
            stack_base: STACK_REGION_BASE,
            tls_base: 0,
            next_tls_addr: 0,
            map_region_base: 0x1_0000_0000,
            map_region_end: 0x1_0000_0000,
            address_space_size: 1 << 39,
        }
    }
}

/// HLE kernel process.
pub struct KProcess {
    /// Process ID.
    pub pid: ProcessId,
    /// Process name.
    pub name: String,
    /// Handle table for this process.
    pub handle_table: HandleTable,
    /// Guest memory manager.
    pub memory: MemoryManager,
    /// Address space layout.
    pub layout: AddressSpaceLayout,
    /// Threads owned by this process.
    pub threads: Vec<KThread>,
    /// Next thread ID to allocate.
    next_thread_id: ThreadId,
    /// Program title ID (from NACP or config).
    pub title_id: u64,
    /// Whether the process is running.
    pub is_running: bool,
    /// Main thread handle.
    pub main_thread_handle: Handle,
}

impl KProcess {
    pub fn new(pid: ProcessId, name: String) -> Self {
        Self {
            pid,
            name,
            handle_table: HandleTable::new(),
            memory: MemoryManager::new().expect("failed to allocate guest memory"),
            layout: AddressSpaceLayout::default(),
            threads: Vec::new(),
            next_thread_id: 1,
            title_id: 0,
            is_running: false,
            main_thread_handle: 0,
        }
    }

    /// Load code segments into the process address space.
    pub fn load_code(&mut self, base_addr: VAddr, code: &[u8]) -> anyhow::Result<()> {
        let size = ruzu_common::align_up(code.len() as u64, PAGE_SIZE_U64);

        // Map the code region
        self.memory.map(
            base_addr,
            size,
            MemoryPermission::READ | MemoryPermission::EXECUTE,
            MemoryState::Code,
        )?;

        // Write the code
        self.memory.write_bytes(base_addr, code)?;

        // Update layout
        self.layout.code_end = base_addr + size;

        Ok(())
    }

    /// Map the data segment (writable).
    pub fn map_data(
        &mut self,
        base_addr: VAddr,
        data: &[u8],
        total_size: usize,
    ) -> anyhow::Result<()> {
        let size = ruzu_common::align_up(total_size as u64, PAGE_SIZE_U64);

        self.memory.map(
            base_addr,
            size,
            MemoryPermission::READ | MemoryPermission::WRITE,
            MemoryState::CodeData,
        )?;

        // Write initial data (rest is BSS, already zeroed)
        if !data.is_empty() {
            self.memory.write_bytes(base_addr, data)?;
        }

        Ok(())
    }

    /// Allocate main thread stack.
    pub fn allocate_stack(&mut self, size: usize) -> anyhow::Result<VAddr> {
        let stack_size = ruzu_common::align_up(size as u64, PAGE_SIZE_U64);
        let stack_base = self.layout.stack_base;

        self.memory.map(
            stack_base,
            stack_size,
            MemoryPermission::READ | MemoryPermission::WRITE,
            MemoryState::Stack,
        )?;

        // Stack grows downward: top = base + size
        Ok(stack_base + stack_size)
    }

    /// Allocate a TLS slot for a thread.
    pub fn allocate_tls(&mut self) -> anyhow::Result<VAddr> {
        // If we don't have a TLS page yet, or current page is full, allocate a new one
        if self.layout.tls_base == 0
            || (self.layout.next_tls_addr - self.layout.tls_base) as usize
                >= PAGE_SIZE
        {
            // Allocate a new TLS page after the code region
            let tls_page = if self.layout.tls_base == 0 {
                self.layout.code_end
            } else {
                self.layout.next_tls_addr
            };

            let tls_page = ruzu_common::align_up(tls_page, PAGE_SIZE_U64);

            self.memory.map(
                tls_page,
                PAGE_SIZE_U64,
                MemoryPermission::READ | MemoryPermission::WRITE,
                MemoryState::ThreadLocal,
            )?;

            if self.layout.tls_base == 0 {
                self.layout.tls_base = tls_page;
            }
            self.layout.next_tls_addr = tls_page;
        }

        let tls_addr = self.layout.next_tls_addr;
        self.layout.next_tls_addr += TLS_ENTRY_SIZE as u64;

        Ok(tls_addr)
    }

    /// Set the heap size (SVC SetHeapSize).
    pub fn set_heap_size(&mut self, size: u64) -> anyhow::Result<VAddr> {
        let new_end = self.layout.heap_base + size;
        let current_end = self.layout.heap_end;

        if new_end > current_end {
            // Grow heap
            let grow_size = new_end - current_end;
            self.memory.map(
                current_end,
                grow_size,
                MemoryPermission::READ | MemoryPermission::WRITE,
                MemoryState::Heap,
            )?;
        } else if new_end < current_end {
            // Shrink heap
            let shrink_size = current_end - new_end;
            self.memory.unmap(new_end, shrink_size)?;
        }

        self.layout.heap_end = new_end;
        Ok(self.layout.heap_base)
    }

    /// Create the main thread.
    pub fn create_main_thread(
        &mut self,
        entry_point: VAddr,
        stack_top: VAddr,
        priority: u32,
    ) -> anyhow::Result<Handle> {
        let thread_id = self.next_thread_id;
        self.next_thread_id += 1;

        let mut thread = KThread::new(thread_id, entry_point, stack_top, priority, 0);
        let tls_addr = self.allocate_tls()?;
        thread.tls_addr = tls_addr;
        thread.cpu_state.tpidr_el0 = tls_addr;

        let handle = self
            .handle_table
            .add(KernelObject::Thread(0))
            .map_err(|rc| anyhow::anyhow!("handle table: {}", rc))?;
        thread.handle = handle;

        self.threads.push(thread);
        self.main_thread_handle = handle;

        Ok(handle)
    }

    /// Create a new thread.
    pub fn create_thread(
        &mut self,
        entry_point: VAddr,
        arg: u64,
        stack_top: VAddr,
        priority: u32,
        core_id: i32,
    ) -> anyhow::Result<Handle> {
        let thread_id = self.next_thread_id;
        self.next_thread_id += 1;

        let mut thread = KThread::new(thread_id, entry_point, stack_top, priority, core_id);
        thread.entry_arg = arg;

        let tls_addr = self.allocate_tls()?;
        thread.tls_addr = tls_addr;
        thread.cpu_state.tpidr_el0 = tls_addr;

        let handle = self
            .handle_table
            .add(KernelObject::Thread(0))
            .map_err(|rc| anyhow::anyhow!("handle table: {}", rc))?;
        thread.handle = handle;

        self.threads.push(thread);
        Ok(handle)
    }

    /// Get a reference to the main thread.
    pub fn main_thread(&self) -> Option<&KThread> {
        self.threads.first()
    }

    /// Get a mutable reference to the main thread.
    pub fn main_thread_mut(&mut self) -> Option<&mut KThread> {
        self.threads.first_mut()
    }

    /// Get a thread by handle.
    pub fn thread_by_handle(&self, handle: Handle) -> Option<&KThread> {
        self.threads.iter().find(|t| t.handle == handle)
    }

    /// Get a mutable thread by handle.
    pub fn thread_by_handle_mut(&mut self, handle: Handle) -> Option<&mut KThread> {
        self.threads.iter_mut().find(|t| t.handle == handle)
    }

    /// Get the currently running thread (first runnable in Phase 1).
    pub fn current_thread(&self) -> Option<&KThread> {
        self.threads
            .iter()
            .find(|t| t.state == crate::thread::ThreadState::Running)
            .or_else(|| {
                self.threads
                    .iter()
                    .find(|t| t.state == crate::thread::ThreadState::Runnable)
            })
    }

    /// Get the currently running thread (mutable).
    pub fn current_thread_mut(&mut self) -> Option<&mut KThread> {
        // Find running first, then runnable
        let has_running = self
            .threads
            .iter()
            .any(|t| t.state == crate::thread::ThreadState::Running);

        if has_running {
            self.threads
                .iter_mut()
                .find(|t| t.state == crate::thread::ThreadState::Running)
        } else {
            self.threads
                .iter_mut()
                .find(|t| t.state == crate::thread::ThreadState::Runnable)
        }
    }

    /// Get the index of the current thread (running or first runnable).
    ///
    /// This enables split borrows: callers can use `process.threads[idx]` and
    /// `process.memory` simultaneously without conflicting borrows.
    pub fn current_thread_idx(&self) -> Option<usize> {
        self.threads
            .iter()
            .position(|t| t.state == crate::thread::ThreadState::Running)
            .or_else(|| {
                self.threads
                    .iter()
                    .position(|t| t.state == crate::thread::ThreadState::Runnable)
            })
    }

    /// Add a client session handle for a service.
    pub fn add_client_session(&mut self, service_name: &str) -> anyhow::Result<Handle> {
        let session = KClientSession::new(service_name.to_string());
        let handle = self
            .handle_table
            .add(KernelObject::ClientSession(session))
            .map_err(|rc| anyhow::anyhow!("handle table: {}", rc))?;
        Ok(handle)
    }
}
