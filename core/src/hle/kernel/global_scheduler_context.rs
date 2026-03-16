//! Port of zuyu/src/core/hle/kernel/global_scheduler_context.h/.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-16
//!
//! GlobalSchedulerContext: the global scheduler context that manages the
//! priority queue of threads and the scheduler lock.

use std::collections::HashSet;
use std::sync::{Arc, Mutex};

use crate::hardware_properties;

use super::k_priority_queue::KPriorityQueue;
use super::k_scheduler_lock::KAbstractSchedulerLock;
use super::k_thread::KThread;

/// Priority at or above which core migration is allowed.
pub const HIGHEST_CORE_MIGRATION_ALLOWED_PRIORITY: i32 = 2;

/// Preemption priorities for each core (indices 0-3).
pub const PREEMPTION_PRIORITIES: [u32; hardware_properties::NUM_CPU_CORES as usize] = [59, 59, 59, 63];

/// The global scheduler context.
///
/// Upstream holds a KSchedulerPriorityQueue, a LockType (KAbstractSchedulerLock),
/// a set of dummy threads pending wakeup, and a thread list.
///
/// Upstream stores `KernelCore& m_kernel` for accessing schedulers in
/// PreemptThreads/UpdateHighestPriorityThreadsImpl. In our cooperative model,
/// the KernelCore ref is not yet needed because:
/// - PreemptThreads is called every 10ms by the cpu_manager and is stubbed
/// - The PQ lives in KProcess temporarily (single-process model)
/// When multi-process or multi-core support is needed, add the kernel ref.
pub struct GlobalSchedulerContext {
    pub m_scheduler_update_needed: std::sync::atomic::AtomicBool,
    pub m_priority_queue: KPriorityQueue,
    pub m_scheduler_lock: KAbstractSchedulerLock,

    /// Dummy threads pending wakeup on lock release.
    /// Upstream: std::set<KThread*>
    m_woken_dummy_threads: Mutex<HashSet<u64>>,
    /// All thread pointers that are alive.
    /// Upstream: std::vector<KThread*> with std::mutex m_global_list_guard
    m_thread_list: Mutex<Vec<Arc<Mutex<KThread>>>>,
}

impl GlobalSchedulerContext {
    pub fn new() -> Self {
        Self {
            m_scheduler_update_needed: std::sync::atomic::AtomicBool::new(false),
            m_priority_queue: KPriorityQueue::new(),
            m_scheduler_lock: KAbstractSchedulerLock::new(),
            m_woken_dummy_threads: Mutex::new(HashSet::new()),
            m_thread_list: Mutex::new(Vec::new()),
        }
    }

    /// Add a thread to the global thread list.
    /// Matches upstream: `std::scoped_lock lock{m_global_list_guard}; m_thread_list.push_back(thread);`
    pub fn add_thread(&self, thread: Arc<Mutex<KThread>>) {
        self.m_thread_list.lock().unwrap().push(thread);
    }

    /// Remove a thread from the global thread list.
    /// Matches upstream: `std::scoped_lock lock{m_global_list_guard}; std::erase(m_thread_list, thread);`
    pub fn remove_thread(&self, thread_id: u64) {
        self.m_thread_list
            .lock()
            .unwrap()
            .retain(|t| t.lock().unwrap().get_thread_id() != thread_id);
    }

    /// Returns a snapshot of the thread list.
    pub fn get_thread_list(&self) -> Vec<Arc<Mutex<KThread>>> {
        self.m_thread_list.lock().unwrap().clone()
    }

    /// Returns thread IDs for compatibility with existing code.
    pub fn get_thread_id_list(&self) -> Vec<u64> {
        self.m_thread_list
            .lock()
            .unwrap()
            .iter()
            .map(|t| t.lock().unwrap().get_thread_id())
            .collect()
    }

    /// Preempt threads at the preemption priorities for each core.
    /// Matches upstream: calls KScheduler::RotateScheduledQueue per core.
    ///
    /// Note: requires a &mut KProcess to access the PQ. This is called
    /// externally (e.g., by cpu_manager every 10ms) with the process lock held.
    pub fn preempt_threads_with_process(&self, process: &mut super::k_process::KProcess) {
        for core_id in 0..hardware_properties::NUM_CPU_CORES {
            let priority = PREEMPTION_PRIORITIES[core_id as usize] as i32;
            super::k_scheduler::KScheduler::rotate_scheduled_queue(process, core_id as i32, priority);
        }
    }

    /// Returns true if the global scheduler lock is held by the current thread.
    pub fn is_locked(&self) -> bool {
        self.m_scheduler_lock.is_locked_by_current_thread()
    }

    /// Register a dummy thread for wakeup on scheduler lock release.
    pub fn register_dummy_thread_for_wakeup(&self, thread_id: u64) {
        debug_assert!(self.is_locked());
        self.m_woken_dummy_threads
            .lock()
            .unwrap()
            .insert(thread_id);
    }

    /// Unregister a dummy thread from wakeup.
    pub fn unregister_dummy_thread_for_wakeup(&self, thread_id: u64) {
        debug_assert!(self.is_locked());
        self.m_woken_dummy_threads
            .lock()
            .unwrap()
            .remove(&thread_id);
    }

    /// Wake up all waiting dummy threads.
    /// Matches upstream: for each thread, calls thread.DummyThreadEndWait()
    pub fn wakeup_waiting_dummy_threads(&self) {
        debug_assert!(self.is_locked());
        let thread_ids: Vec<u64> = {
            let set = self.m_woken_dummy_threads.lock().unwrap();
            set.iter().copied().collect()
        };

        let thread_list = self.m_thread_list.lock().unwrap();
        for thread_id in &thread_ids {
            if let Some(thread) = thread_list.iter().find(|t| {
                t.lock().unwrap().get_thread_id() == *thread_id
            }) {
                thread.lock().unwrap().dummy_thread_end_wait();
            }
        }

        self.m_woken_dummy_threads.lock().unwrap().clear();
    }

    /// Get a reference to the scheduler lock.
    pub fn scheduler_lock(&self) -> &KAbstractSchedulerLock {
        &self.m_scheduler_lock
    }
}

impl Default for GlobalSchedulerContext {
    fn default() -> Self {
        Self::new()
    }
}
