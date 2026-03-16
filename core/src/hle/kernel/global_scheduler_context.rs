//! Port of zuyu/src/core/hle/kernel/global_scheduler_context.h/.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-16
//!
//! GlobalSchedulerContext: the global scheduler context that manages the
//! priority queue of threads and the scheduler lock.

use std::collections::HashSet;
use std::sync::{Arc, Mutex};

use crate::hardware_properties;

use super::k_priority_queue::{KPriorityQueue, KPriorityQueueMember, ThreadAccessor};
use super::k_scheduler_lock::KAbstractSchedulerLock;
use super::k_thread::KThread;

/// Priority at or above which core migration is allowed.
pub const HIGHEST_CORE_MIGRATION_ALLOWED_PRIORITY: i32 = 2;

/// Preemption priorities for each core (indices 0-3).
pub const PREEMPTION_PRIORITIES: [u32; hardware_properties::NUM_CPU_CORES as usize] = [59, 59, 59, 63];

/// ThreadAccessor backed by a snapshot of Arc<Mutex<KThread>> references.
/// Used for PQ operations that need to resolve thread IDs to thread data.
pub struct ThreadListAccessor {
    threads: Vec<Arc<Mutex<KThread>>>,
}

impl ThreadListAccessor {
    fn find_thread(&self, thread_id: u64) -> Option<&Arc<Mutex<KThread>>> {
        self.threads.iter().find(|t| {
            t.lock().unwrap().get_thread_id() == thread_id
        })
    }
}

impl ThreadAccessor for ThreadListAccessor {
    fn with_thread<F, R>(&self, thread_id: u64, f: F) -> Option<R>
    where
        F: FnOnce(&dyn KPriorityQueueMember) -> R,
    {
        let thread = self.find_thread(thread_id)?;
        let guard = thread.lock().unwrap();
        Some(f(&*guard))
    }

    fn with_thread_mut<F, R>(&self, thread_id: u64, f: F) -> Option<R>
    where
        F: FnOnce(&mut dyn KPriorityQueueMember) -> R,
    {
        let thread = self.find_thread(thread_id)?;
        let mut guard = thread.lock().unwrap();
        Some(f(&mut *guard))
    }
}

/// The global scheduler context.
///
/// Matches upstream `GlobalSchedulerContext` (global_scheduler_context.h).
/// Owns the KSchedulerPriorityQueue, the scheduler lock, the thread list,
/// and dummy thread wakeup tracking.
pub struct GlobalSchedulerContext {
    pub m_scheduler_update_needed: std::sync::atomic::AtomicBool,
    pub m_priority_queue: KPriorityQueue,
    pub m_scheduler_lock: KAbstractSchedulerLock,

    /// Dummy threads pending wakeup on lock release.
    m_woken_dummy_threads: Mutex<HashSet<u64>>,
    /// All thread pointers that are alive.
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

    // -- Thread list management --

    pub fn add_thread(&self, thread: Arc<Mutex<KThread>>) {
        self.m_thread_list.lock().unwrap().push(thread);
    }

    pub fn remove_thread(&self, thread_id: u64) {
        self.m_thread_list
            .lock()
            .unwrap()
            .retain(|t| t.lock().unwrap().get_thread_id() != thread_id);
    }

    pub fn get_thread_list(&self) -> Vec<Arc<Mutex<KThread>>> {
        self.m_thread_list.lock().unwrap().clone()
    }

    pub fn get_thread_by_thread_id(&self, thread_id: u64) -> Option<Arc<Mutex<KThread>>> {
        self.m_thread_list
            .lock()
            .unwrap()
            .iter()
            .find(|t| t.lock().unwrap().get_thread_id() == thread_id)
            .cloned()
    }

    // -- Priority queue operations --
    // These snapshot the thread list, then operate on the PQ with that snapshot
    // as the ThreadAccessor. The PQ is extracted via std::mem::take to avoid
    // double-borrow issues (same pattern as KProcess::push_back_to_priority_queue).

    pub fn make_accessor(&self) -> ThreadListAccessor {
        ThreadListAccessor {
            threads: self.m_thread_list.lock().unwrap().clone(),
        }
    }

    pub fn push_back_to_priority_queue(&mut self, thread_id: u64) {
        let accessor = self.make_accessor();
        let mut pq = std::mem::take(&mut self.m_priority_queue);
        pq.push_back(thread_id, &accessor);
        self.m_priority_queue = pq;
    }

    pub fn remove_from_priority_queue(&mut self, thread_id: u64) {
        let accessor = self.make_accessor();
        let mut pq = std::mem::take(&mut self.m_priority_queue);
        pq.remove(thread_id, &accessor);
        self.m_priority_queue = pq;
    }

    pub fn change_priority_in_queue(&mut self, thread_id: u64, old_priority: i32, is_running: bool) {
        let accessor = self.make_accessor();
        let mut pq = std::mem::take(&mut self.m_priority_queue);
        pq.change_priority(old_priority, is_running, thread_id, &accessor);
        self.m_priority_queue = pq;
    }

    pub fn get_scheduled_front(&self, core: i32) -> Option<u64> {
        self.m_priority_queue.get_scheduled_front(core)
    }

    pub fn move_to_scheduled_back(&mut self, thread_id: u64) -> Option<u64> {
        let accessor = self.make_accessor();
        let mut pq = std::mem::take(&mut self.m_priority_queue);
        let result = pq.move_to_scheduled_back(thread_id, &accessor);
        self.m_priority_queue = pq;
        result
    }

    pub fn get_scheduled_next(&self, core: i32, thread_id: u64, priority: i32) -> Option<u64> {
        let accessor = self.make_accessor();
        self.m_priority_queue.get_scheduled_next(core, thread_id, priority, &accessor)
    }

    // -- PreemptThreads --

    pub fn preempt_threads(&mut self) {
        let accessor = self.make_accessor();
        for core_id in 0..hardware_properties::NUM_CPU_CORES {
            let priority = PREEMPTION_PRIORITIES[core_id as usize] as i32;
            let mut pq = std::mem::take(&mut self.m_priority_queue);
            let top_thread_id = pq.get_scheduled_front_at_priority(core_id as i32, priority);
            if let Some(top_id) = top_thread_id {
                let _ = pq.move_to_scheduled_back(top_id, &accessor);
            }
            self.m_priority_queue = pq;
        }
    }

    // -- Scheduler lock and state --

    pub fn is_locked(&self) -> bool {
        self.m_scheduler_lock.is_locked_by_current_thread()
    }

    pub fn scheduler_lock(&self) -> &KAbstractSchedulerLock {
        &self.m_scheduler_lock
    }

    pub fn register_dummy_thread_for_wakeup(&self, thread_id: u64) {
        self.m_woken_dummy_threads.lock().unwrap().insert(thread_id);
    }

    pub fn unregister_dummy_thread_for_wakeup(&self, thread_id: u64) {
        self.m_woken_dummy_threads.lock().unwrap().remove(&thread_id);
    }

    pub fn wakeup_waiting_dummy_threads(&self) {
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
}

impl Default for GlobalSchedulerContext {
    fn default() -> Self {
        Self::new()
    }
}
