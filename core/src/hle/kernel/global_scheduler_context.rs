//! Port of zuyu/src/core/hle/kernel/global_scheduler_context.h/.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-27
//!
//! GlobalSchedulerContext: the global scheduler context that manages the
//! priority queue of threads and the scheduler lock.

use std::collections::HashSet;
use std::sync::{Arc, Mutex};

use crate::hardware_properties;

use super::k_priority_queue::KPriorityQueue;
use super::k_scheduler_lock::KAbstractSchedulerLock;
use super::k_thread::{KThread, ThreadState, ThreadType};

/// Priority at or above which core migration is allowed.
pub const HIGHEST_CORE_MIGRATION_ALLOWED_PRIORITY: i32 = 2;

/// Preemption priorities for each core (indices 0-3).
pub const PREEMPTION_PRIORITIES: [u32; hardware_properties::NUM_CPU_CORES as usize] =
    [59, 59, 59, 63];

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

    // -- Centralized PQ state change handler --
    // Matches upstream KScheduler::OnThreadStateChanged (k_scheduler.cpp:522).
    //
    // This is the single authority for keeping the PQ in sync with thread state.
    // All thread state transitions that cross Runnable must call this.
    //
    // Thread properties are passed directly (extracted by the caller while
    // holding the thread lock), so this function never locks any KThread.

    /// Called when a thread's state changes. Updates the PQ accordingly.
    /// Matches upstream `KScheduler::OnThreadStateChanged`.
    ///
    /// `old_state` and `new_state` are the base states (masked with ThreadState::MASK).
    pub fn on_thread_state_changed(
        &mut self,
        thread_id: u64,
        old_state: ThreadState,
        new_state: ThreadState,
        priority: i32,
        active_core: i32,
        affinity: u64,
        is_dummy: bool,
        process_schedule_count: Option<std::sync::Arc<std::sync::atomic::AtomicI64>>,
    ) {
        if old_state == new_state {
            return;
        }

        if old_state == ThreadState::RUNNABLE {
            // Was runnable, now not — remove from PQ.
            self.m_priority_queue
                .remove(thread_id, priority, active_core, affinity, is_dummy);
            self.m_priority_queue.increment_scheduled_count(thread_id);
            self.m_scheduler_update_needed
                .store(true, std::sync::atomic::Ordering::Release);

            if is_dummy {
                self.unregister_dummy_thread_for_wakeup(thread_id);
            }
        } else if new_state == ThreadState::RUNNABLE {
            // Was not runnable, now is — add to PQ.
            self.m_priority_queue.push_back(
                thread_id,
                priority,
                active_core,
                affinity,
                is_dummy,
                process_schedule_count,
            );
            self.m_priority_queue.increment_scheduled_count(thread_id);
            self.m_scheduler_update_needed
                .store(true, std::sync::atomic::Ordering::Release);

            if is_dummy {
                self.register_dummy_thread_for_wakeup(thread_id);
            }

            // Wake the target core from idle so it picks up the new thread.
            // Upstream: KScheduler::RescheduleOtherCores triggers interrupts
            // on cores whose highest priority changed.
            if active_core >= 0 {
                if let Some(kernel) = super::kernel::get_kernel_ref() {
                    // Set needs_scheduling directly on the target core's scheduler.
                    // This ensures the core will call schedule_impl_fiber even if
                    // update_highest_priority_threads_impl was already consumed by
                    // another core.
                    if let Some(sched_arc) = kernel.scheduler(active_core as usize) {
                        sched_arc
                            .lock()
                            .unwrap()
                            .state
                            .needs_scheduling
                            .store(true, std::sync::atomic::Ordering::Relaxed);
                    }
                    if let Some(core) = kernel.physical_core(active_core as usize) {
                        core.interrupt();
                    }
                }
            }
        }

    }

    /// Called when a thread's priority changes while it is Runnable.
    /// Matches upstream `KScheduler::OnThreadPriorityChanged`.
    pub fn on_thread_priority_changed(
        &mut self,
        _thread_id: u64,
        old_priority: i32,
        new_priority: i32,
        active_core: i32,
        affinity: u64,
        is_running: bool,
        is_dummy: bool,
        _member_id: u64,
    ) {
        self.m_priority_queue.change_priority(
            old_priority,
            is_running,
            _member_id,
            new_priority,
            active_core,
            affinity,
            is_dummy,
        );
        self.m_scheduler_update_needed
            .store(true, std::sync::atomic::Ordering::Release);
    }

    /// Called when a thread's affinity mask changes while it is Runnable.
    /// Matches upstream `KScheduler::OnThreadAffinityMaskChanged`.
    pub fn on_thread_affinity_changed(
        &mut self,
        thread_id: u64,
        prev_core: i32,
        prev_affinity: u64,
        new_core: i32,
        new_affinity: u64,
        priority: i32,
        is_dummy: bool,
    ) {
        self.m_priority_queue.change_affinity_mask(
            prev_core,
            prev_affinity,
            thread_id,
            new_core,
            new_affinity,
            priority,
            is_dummy,
        );
        self.m_scheduler_update_needed
            .store(true, std::sync::atomic::Ordering::Release);
    }

    // -- Legacy PQ operations (for callers that pass properties directly) --

    pub fn push_back_to_priority_queue(
        &mut self,
        thread_id: u64,
        priority: i32,
        active_core: i32,
        affinity: u64,
        is_dummy: bool,
        process_schedule_count: Option<std::sync::Arc<std::sync::atomic::AtomicI64>>,
    ) {
        self.m_priority_queue.push_back(
            thread_id,
            priority,
            active_core,
            affinity,
            is_dummy,
            process_schedule_count,
        );
        self.m_scheduler_update_needed
            .store(true, std::sync::atomic::Ordering::Release);

        // Wake the target core so it picks up the new thread.
        if !is_dummy && active_core >= 0 {
            if let Some(kernel) = super::kernel::get_kernel_ref() {
                if let Some(core) = kernel.physical_core(active_core as usize) {
                    core.interrupt();
                }
            }
        }
    }

    pub fn remove_from_priority_queue(
        &mut self,
        thread_id: u64,
        priority: i32,
        active_core: i32,
        affinity: u64,
        is_dummy: bool,
    ) {
        self.m_priority_queue
            .remove(thread_id, priority, active_core, affinity, is_dummy);
        self.m_scheduler_update_needed
            .store(true, std::sync::atomic::Ordering::Release);
    }

    pub fn get_scheduled_front(&self, core: i32) -> Option<u64> {
        self.m_priority_queue.get_scheduled_front(core)
    }

    pub fn move_to_scheduled_back(
        &mut self,
        thread_id: u64,
        priority: i32,
        active_core: i32,
        is_dummy: bool,
    ) -> Option<u64> {
        let result = self.m_priority_queue.move_to_scheduled_back(
            thread_id,
            priority,
            active_core,
            is_dummy,
        );
        self.m_scheduler_update_needed
            .store(true, std::sync::atomic::Ordering::Release);
        result
    }

    pub fn get_scheduled_next(&self, core: i32, thread_id: u64, priority: i32) -> Option<u64> {
        self.m_priority_queue
            .get_scheduled_next(core, thread_id, priority)
    }

    // -- PreemptThreads --

    pub fn preempt_threads(&mut self) {
        for core_id in 0..hardware_properties::NUM_CPU_CORES {
            let priority = PREEMPTION_PRIORITIES[core_id as usize] as i32;
            let top_thread_id = self
                .m_priority_queue
                .get_scheduled_front_at_priority(core_id as i32, priority);
            if let Some(top_id) = top_thread_id {
                // Look up cached properties for this thread
                let (t_priority, t_core, t_dummy) = self
                    .m_priority_queue
                    .get_thread_props(top_id)
                    .map(|p| (p.priority, p.active_core, p.is_dummy))
                    .unwrap_or((priority, core_id as i32, false));
                let _ = self
                    .m_priority_queue
                    .move_to_scheduled_back(top_id, t_priority, t_core, t_dummy);
            }
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
        self.m_woken_dummy_threads
            .lock()
            .unwrap()
            .remove(&thread_id);
    }

    pub fn wakeup_waiting_dummy_threads(&self) {
        let thread_ids: Vec<u64> = {
            let set = self.m_woken_dummy_threads.lock().unwrap();
            set.iter().copied().collect()
        };

        let thread_list = self.m_thread_list.lock().unwrap();
        for thread_id in &thread_ids {
            if let Some(thread) = thread_list
                .iter()
                .find(|t| t.lock().unwrap().get_thread_id() == *thread_id)
            {
                thread.lock().unwrap().dummy_thread_end_wait();
            }
        }

        self.m_woken_dummy_threads.lock().unwrap().clear();
    }

    /// Helper: extract PQ-relevant properties from a locked KThread.
    /// Used by callers that need to pass properties to on_thread_state_changed.
    pub fn extract_thread_props(thread: &KThread) -> (u64, i32, i32, u64, bool) {
        (
            thread.get_thread_id(),
            thread.priority,
            thread.core_id,
            thread.physical_affinity_mask.get_affinity_mask(),
            thread.thread_type == ThreadType::Dummy,
        )
    }
}

impl Default for GlobalSchedulerContext {
    fn default() -> Self {
        Self::new()
    }
}
