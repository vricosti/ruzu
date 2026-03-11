//! Port of zuyu/src/core/hle/kernel/global_scheduler_context.h/.cpp
//! Status: COMPLET (stub — runtime dependencies not yet available)
//! Derniere synchro: 2026-03-11
//!
//! GlobalSchedulerContext: the global scheduler context that manages the
//! priority queue of threads and the scheduler lock. Full implementation
//! requires KThread, KScheduler, KPriorityQueue, KSchedulerLock.

use crate::hardware_properties;
use std::collections::HashSet;
use std::sync::Mutex;

/// Priority at or above which core migration is allowed.
pub const HIGHEST_CORE_MIGRATION_ALLOWED_PRIORITY: i32 = 2;

/// Preemption priorities for each core (indices 0-3).
pub const PREEMPTION_PRIORITIES: [u32; hardware_properties::NUM_CPU_CORES as usize] = [59, 59, 59, 63];

/// The global scheduler context.
///
/// Upstream holds a KSchedulerPriorityQueue, a LockType (KAbstractSchedulerLock),
/// a set of dummy threads pending wakeup, and a thread list.
pub struct GlobalSchedulerContext {
    m_scheduler_update_needed: std::sync::atomic::AtomicBool,
    // m_priority_queue: KSchedulerPriorityQueue — requires KThread
    // m_scheduler_lock: LockType — requires KScheduler

    /// Dummy threads pending wakeup on lock release.
    m_woken_dummy_threads: Mutex<HashSet<u64>>, // u64 as thread-id placeholder
    /// All thread ids that are alive.
    m_thread_list: Mutex<Vec<u64>>, // u64 as thread-id placeholder
}

impl GlobalSchedulerContext {
    pub fn new() -> Self {
        Self {
            m_scheduler_update_needed: std::sync::atomic::AtomicBool::new(false),
            m_woken_dummy_threads: Mutex::new(HashSet::new()),
            m_thread_list: Mutex::new(Vec::new()),
        }
    }

    /// Add a thread to the global thread list.
    pub fn add_thread(&self, thread_id: u64) {
        self.m_thread_list.lock().unwrap().push(thread_id);
    }

    /// Remove a thread from the global thread list.
    pub fn remove_thread(&self, thread_id: u64) {
        self.m_thread_list
            .lock()
            .unwrap()
            .retain(|&id| id != thread_id);
    }

    /// Returns a snapshot of the thread list.
    pub fn get_thread_list(&self) -> Vec<u64> {
        self.m_thread_list.lock().unwrap().clone()
    }

    /// Preempt threads at the preemption priorities for each core.
    pub fn preempt_threads(&self) {
        // TODO: Requires KScheduler::IsSchedulerLockedByCurrentThread,
        //       KScheduler::RotateScheduledQueue
        for core_id in 0..hardware_properties::NUM_CPU_CORES {
            let _priority = PREEMPTION_PRIORITIES[core_id as usize];
            // KScheduler::RotateScheduledQueue(kernel, core_id, priority);
        }
    }

    /// Returns true if the global scheduler lock is held by the current thread.
    pub fn is_locked(&self) -> bool {
        // TODO: m_scheduler_lock.IsLockedByCurrentThread()
        false
    }

    /// Register a dummy thread for wakeup on scheduler lock release.
    pub fn register_dummy_thread_for_wakeup(&self, thread_id: u64) {
        self.m_woken_dummy_threads
            .lock()
            .unwrap()
            .insert(thread_id);
    }

    /// Unregister a dummy thread from wakeup.
    pub fn unregister_dummy_thread_for_wakeup(&self, thread_id: u64) {
        self.m_woken_dummy_threads
            .lock()
            .unwrap()
            .remove(&thread_id);
    }

    /// Wake up all waiting dummy threads.
    pub fn wakeup_waiting_dummy_threads(&self) {
        let mut set = self.m_woken_dummy_threads.lock().unwrap();
        // TODO: For each thread, call thread.DummyThreadEndWait()
        set.clear();
    }
}

impl Default for GlobalSchedulerContext {
    fn default() -> Self {
        Self::new()
    }
}
