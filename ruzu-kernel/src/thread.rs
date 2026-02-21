// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use ruzu_common::{Handle, ThreadId, VAddr, DEFAULT_STACK_SIZE};
use ruzu_cpu::CpuState;

/// Thread states.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ThreadState {
    /// Thread is initialized but not yet started.
    Initialized,
    /// Thread is ready to run.
    Runnable,
    /// Thread is currently executing.
    Running,
    /// Thread is waiting on a synchronization object.
    Waiting,
    /// Thread has terminated.
    Terminated,
}

/// Default thread priority (matching Switch defaults).
pub const DEFAULT_THREAD_PRIORITY: u32 = 44;

/// Minimum thread priority (higher number = lower priority on Switch).
pub const THREAD_PRIORITY_HIGHEST: u32 = 0;

/// Maximum thread priority.
pub const THREAD_PRIORITY_LOWEST: u32 = 63;

/// HLE kernel thread.
pub struct KThread {
    /// Unique thread ID.
    pub thread_id: ThreadId,
    /// Handle in the owning process's handle table.
    pub handle: Handle,
    /// Thread priority (0 = highest, 63 = lowest).
    pub priority: u32,
    /// Core affinity mask.
    pub core_mask: u64,
    /// Ideal core for this thread.
    pub ideal_core: i32,
    /// Current thread state.
    pub state: ThreadState,
    /// CPU register state.
    pub cpu_state: CpuState,
    /// TLS (Thread Local Storage) address in guest memory.
    pub tls_addr: VAddr,
    /// Stack base address.
    pub stack_base: VAddr,
    /// Stack size.
    pub stack_size: usize,
    /// Entry point address.
    pub entry_point: VAddr,
    /// Entry argument (passed in X0).
    pub entry_arg: u64,
    /// Owning process handle.
    pub owner_process: Handle,
    /// Name for debugging.
    pub name: String,
}

impl KThread {
    pub fn new(
        thread_id: ThreadId,
        entry_point: VAddr,
        stack_top: VAddr,
        priority: u32,
        core_id: i32,
    ) -> Self {
        let mut cpu_state = CpuState::new();
        cpu_state.pc = entry_point;
        cpu_state.sp = stack_top;

        Self {
            thread_id,
            handle: 0,
            priority,
            core_mask: 1u64 << core_id.max(0) as u32,
            ideal_core: core_id,
            state: ThreadState::Initialized,
            cpu_state,
            tls_addr: 0,
            stack_base: stack_top.saturating_sub(DEFAULT_STACK_SIZE as u64),
            stack_size: DEFAULT_STACK_SIZE,
            entry_point,
            entry_arg: 0,
            owner_process: 0,
            name: format!("Thread-{}", thread_id),
        }
    }

    /// Initialize the thread for execution.
    pub fn start(&mut self) {
        self.state = ThreadState::Runnable;
        self.cpu_state.x[0] = self.entry_arg;
        self.cpu_state.pc = self.entry_point;
    }

    /// Check if the thread is runnable.
    pub fn is_runnable(&self) -> bool {
        self.state == ThreadState::Runnable
    }

    /// Check if the thread is terminated.
    pub fn is_terminated(&self) -> bool {
        self.state == ThreadState::Terminated
    }
}
