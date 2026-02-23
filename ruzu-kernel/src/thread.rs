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

/// Why a thread is waiting.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WaitReason {
    /// Not waiting.
    None,
    /// WaitSynchronization on one or more handles.
    Synchronization { handles: Vec<Handle>, timeout_ns: i64 },
    /// ArbitrateLock — waiting for a mutex.
    ArbitrateLock { mutex_addr: VAddr, tag: u32 },
    /// WaitProcessWideKeyAtomic — waiting on a condvar.
    CondVar { condvar_addr: VAddr, mutex_addr: VAddr },
    /// WaitForAddress — waiting on an address arbiter.
    AddressArbiter { addr: VAddr },
    /// SleepThread with a timeout.
    Sleep { wake_tick: u64 },
    /// SetThreadActivity — thread is suspended.
    Suspended,
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
    /// Why the thread is blocked (only meaningful when state == Waiting).
    pub wait_reason: WaitReason,
    /// Result code set when the thread is woken (SUCCESS/TIMEOUT/CANCELLED).
    pub wait_result: u32,
    /// For WaitSynchronization: which handle index was signaled (-1 = none).
    pub synced_index: i32,
    /// Tick count when the wait began (for timeout checking).
    pub wait_start_tick: u64,
    /// If true, the next WaitSynchronization will immediately return CANCELLED.
    pub cancel_pending: bool,
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
            wait_reason: WaitReason::None,
            wait_result: 0,
            synced_index: -1,
            wait_start_tick: 0,
            cancel_pending: false,
        }
    }

    /// Initialize the thread for execution.
    pub fn start(&mut self) {
        self.state = ThreadState::Runnable;
        self.cpu_state.x[0] = self.entry_arg;
        self.cpu_state.pc = self.entry_point;
    }

    /// Block the thread with the given wait reason.
    pub fn begin_wait(&mut self, reason: WaitReason, tick: u64) {
        self.state = ThreadState::Waiting;
        self.wait_reason = reason;
        self.wait_result = 0;
        self.synced_index = -1;
        self.wait_start_tick = tick;
    }

    /// Wake the thread from a wait state.
    pub fn wake(&mut self, result: u32, synced_idx: i32) {
        self.state = ThreadState::Runnable;
        self.wait_reason = WaitReason::None;
        self.wait_result = result;
        self.synced_index = synced_idx;
        // Write result and synced index back to CPU registers
        self.cpu_state.x[0] = result as u64;
        self.cpu_state.x[1] = synced_idx as u64;
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

#[cfg(test)]
mod tests {
    use super::*;
    use ruzu_common::ResultCode;

    #[test]
    fn test_begin_wait_and_wake() {
        let mut thread = KThread::new(1, 0x1000, 0x8000, 44, 0);
        thread.start();
        assert_eq!(thread.state, ThreadState::Runnable);

        thread.begin_wait(
            WaitReason::Synchronization { handles: vec![1, 2], timeout_ns: 1000 },
            100,
        );
        assert_eq!(thread.state, ThreadState::Waiting);
        assert_eq!(thread.wait_start_tick, 100);

        thread.wake(ResultCode::SUCCESS.raw(), 0);
        assert_eq!(thread.state, ThreadState::Runnable);
        assert_eq!(thread.wait_result, ResultCode::SUCCESS.raw());
        assert_eq!(thread.synced_index, 0);
        assert_eq!(thread.cpu_state.x[0], ResultCode::SUCCESS.raw() as u64);
        assert_eq!(thread.cpu_state.x[1], 0);
    }

    #[test]
    fn test_wake_timeout() {
        let mut thread = KThread::new(1, 0x1000, 0x8000, 44, 0);
        thread.start();
        thread.begin_wait(WaitReason::Sleep { wake_tick: 200 }, 100);

        thread.wake(ruzu_common::error::TIMEOUT.raw(), -1);
        assert_eq!(thread.state, ThreadState::Runnable);
        assert_eq!(thread.wait_result, ruzu_common::error::TIMEOUT.raw());
        assert_eq!(thread.synced_index, -1);
    }
}
