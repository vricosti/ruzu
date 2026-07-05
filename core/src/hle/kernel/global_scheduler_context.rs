//! Port of zuyu/src/core/hle/kernel/global_scheduler_context.h/.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-27
//!
//! GlobalSchedulerContext: the global scheduler context that manages the
//! priority queue of threads and the scheduler lock.

use std::collections::HashSet;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, OnceLock};

use crate::hardware_properties;

use super::k_priority_queue::KPriorityQueue;
use super::k_scheduler_lock::KAbstractSchedulerLock;
use super::k_thread::{KThread, KThreadLock, ThreadState, ThreadType};

static TRACE_GSC_STATE_COUNT: AtomicUsize = AtomicUsize::new(0);

fn trace_sched_state_filter() -> &'static Option<Vec<u64>> {
    static FILTER: OnceLock<Option<Vec<u64>>> = OnceLock::new();
    FILTER.get_or_init(|| {
        let raw = std::env::var_os("RUZU_TRACE_SCHED_STATE")?;
        let raw = raw.to_string_lossy();
        let raw = raw.trim();
        if raw.is_empty() || raw == "1" || raw.eq_ignore_ascii_case("all") {
            return Some(Vec::new());
        }
        Some(
            raw.split(',')
                .filter_map(|value| {
                    let value = value.trim();
                    if value.is_empty() {
                        return None;
                    }
                    value
                        .strip_prefix("0x")
                        .or_else(|| value.strip_prefix("0X"))
                        .and_then(|hex| u64::from_str_radix(hex, 16).ok())
                        .or_else(|| value.parse::<u64>().ok())
                })
                .collect(),
        )
    })
}

fn increment_process_scheduled_count(
    counter: Option<&std::sync::Arc<std::sync::atomic::AtomicI64>>,
) {
    if let Some(counter) = counter {
        counter.fetch_add(1, Ordering::Relaxed);
    }
}

fn should_trace_sched_state(thread_id: u64) -> bool {
    let Some(filter) = trace_sched_state_filter() else {
        return false;
    };
    filter.is_empty() || filter.contains(&thread_id)
}

fn trace_sched_state(args: std::fmt::Arguments<'_>) {
    if trace_sched_state_filter().is_none() {
        return;
    }
    let idx = TRACE_GSC_STATE_COUNT.fetch_add(1, Ordering::Relaxed);
    if idx < 1024 {
        log::info!("{}", args);
    }
}

fn encode_top(top: Option<u64>) -> u64 {
    top.unwrap_or(u64::MAX)
}

fn trace_sched_state_fast(
    stage: u64,
    thread_id: u64,
    old_state: ThreadState,
    new_state: ThreadState,
    priority: i32,
    active_core: i32,
    queue: &KPriorityQueue,
) {
    if !common::trace::is_enabled(common::trace::cat::SCHED_STATE) {
        return;
    }
    if !should_trace_sched_state(thread_id) {
        return;
    }
    common::trace::emit_raw(
        common::trace::cat::SCHED_STATE,
        &[
            stage,
            thread_id,
            old_state.bits() as u64,
            new_state.bits() as u64,
            priority as u32 as u64,
            active_core as u32 as u64,
            encode_top(queue.get_scheduled_front(0)),
            encode_top(queue.get_scheduled_front(1)),
            encode_top(queue.get_scheduled_front(2)),
            encode_top(queue.get_scheduled_front(3)),
        ],
    );
}

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
    ///
    /// Upstream stores intrusive `KThread*` entries and can identify a thread
    /// without locking it. Keep the Rust owner keyed by `thread_id` so lookup
    /// does not relock arbitrary thread mutexes while the scheduler lock is
    /// unwinding.
    m_thread_list: Mutex<Vec<(u64, Arc<KThreadLock>)>>,
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

    pub fn add_thread(&self, thread: Arc<KThreadLock>) {
        // Matches upstream intrusive ownership: the thread list must never
        // silently accept an invalid identifier. Callers that already hold the
        // thread mutex must use add_thread_with_id() to avoid re-locking.
        let thread_id = thread.lock().unwrap().get_thread_id();
        self.m_thread_list.lock().unwrap().push((thread_id, thread));
    }

    /// Add a thread when the thread_id is already known (avoids re-locking).
    pub fn add_thread_with_id(&self, thread_id: u64, thread: Arc<KThreadLock>) {
        self.m_thread_list.lock().unwrap().push((thread_id, thread));
    }

    pub fn remove_thread(&self, thread_id: u64) {
        self.m_thread_list
            .lock()
            .unwrap()
            .retain(|(id, _)| *id != thread_id);
    }

    pub fn get_thread_list(&self) -> Vec<Arc<KThreadLock>> {
        self.m_thread_list
            .lock()
            .unwrap()
            .iter()
            .map(|(_, thread)| Arc::clone(thread))
            .collect()
    }

    pub fn get_thread_by_thread_id(&self, thread_id: u64) -> Option<Arc<KThreadLock>> {
        self.m_thread_list
            .lock()
            .unwrap()
            .iter()
            .find(|(id, _)| *id == thread_id)
            .map(|(_, thread)| Arc::clone(thread))
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
        if should_trace_sched_state(thread_id) {
            trace_sched_state(format_args!(
                "GSC::on_thread_state_changed tid={} {:?}->{:?} prio={} active_core={} affinity=0x{:x} dummy={} sched_count={}",
                thread_id,
                old_state,
                new_state,
                priority,
                active_core,
                affinity,
                is_dummy,
                process_schedule_count
                    .as_ref()
                    .map(|c| c.load(std::sync::atomic::Ordering::Relaxed))
                    .unwrap_or(-1),
            ));
        }
        log::trace!(
            "GSC::on_thread_state_changed tid={} {:?}->{:?} prio={} active_core={} affinity=0x{:x} dummy={}",
            thread_id,
            old_state,
            new_state,
            priority,
            active_core,
            affinity,
            is_dummy
        );
        if old_state == new_state {
            return;
        }
        trace_sched_state_fast(
            1,
            thread_id,
            old_state,
            new_state,
            priority,
            active_core,
            &self.m_priority_queue,
        );

        if old_state == ThreadState::RUNNABLE {
            let process_schedule_count = self
                .m_priority_queue
                .get_thread_props(thread_id)
                .and_then(|props| props.process_schedule_count.clone())
                .or(process_schedule_count);
            // Was runnable, now not — remove from PQ.
            self.m_priority_queue
                .remove(thread_id, priority, active_core, affinity, is_dummy);
            increment_process_scheduled_count(process_schedule_count.as_ref());
            self.m_scheduler_update_needed
                .store(true, Ordering::Release);

            if is_dummy {
                self.unregister_dummy_thread_for_wakeup(thread_id);
            }
            if should_trace_sched_state(thread_id) {
                trace_sched_state(format_args!(
                    "GSC::pq remove tid={} prio={} core={} dummy={} top0={:?} top1={:?} top2={:?} top3={:?}",
                    thread_id,
                    priority,
                    active_core,
                    is_dummy,
                    self.m_priority_queue.get_scheduled_front(0),
                    self.m_priority_queue.get_scheduled_front(1),
                    self.m_priority_queue.get_scheduled_front(2),
                    self.m_priority_queue.get_scheduled_front(3),
                ));
            }
            trace_sched_state_fast(
                2,
                thread_id,
                old_state,
                new_state,
                priority,
                active_core,
                &self.m_priority_queue,
            );
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
                .store(true, Ordering::Release);

            if is_dummy {
                self.register_dummy_thread_for_wakeup(thread_id);
            }

            if should_trace_sched_state(thread_id) {
                trace_sched_state(format_args!(
                    "GSC::pq push tid={} prio={} core={} dummy={} top0={:?} top1={:?} top2={:?} top3={:?}",
                    thread_id,
                    priority,
                    active_core,
                    is_dummy,
                    self.m_priority_queue.get_scheduled_front(0),
                    self.m_priority_queue.get_scheduled_front(1),
                    self.m_priority_queue.get_scheduled_front(2),
                    self.m_priority_queue.get_scheduled_front(3),
                ));
            }
            trace_sched_state_fast(
                3,
                thread_id,
                old_state,
                new_state,
                priority,
                active_core,
                &self.m_priority_queue,
            );
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
        let process_schedule_count = self
            .m_priority_queue
            .get_thread_props(_member_id)
            .and_then(|props| props.process_schedule_count.clone());
        self.m_priority_queue.change_priority(
            old_priority,
            is_running,
            _member_id,
            new_priority,
            active_core,
            affinity,
            is_dummy,
        );
        increment_process_scheduled_count(process_schedule_count.as_ref());
        self.m_scheduler_update_needed
            .store(true, Ordering::Release);
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
        self.m_priority_queue.increment_scheduled_count(thread_id);
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

    /// Rust storage recovery for the upstream invariant that every Runnable
    /// thread is present in `m_priority_queue`.
    ///
    /// Upstream stores intrusive scheduler entries in `KThread` and updates
    /// them centrally from `KScheduler::OnThreadStateChanged`. The Rust port's
    /// detached queue storage can miss a wake path or keep a stale property
    /// record while the root-visible list is empty; when that happens,
    /// `UpdateHighestPriorityThreads` would mark a core idle despite a
    /// Runnable thread in the GSC thread list. Reinsert the best Runnable
    /// candidate for this core before returning an empty front.
    pub fn recover_scheduled_front_from_runnable(&mut self, core: i32) -> Option<u64> {
        let mut best: Option<(
            u64,
            i32,
            i32,
            u64,
            bool,
            Option<std::sync::Arc<std::sync::atomic::AtomicI64>>,
        )> = None;
        for thread in self.get_thread_list() {
            let thread = thread.lock().unwrap();
            if thread.get_raw_state() != ThreadState::RUNNABLE {
                continue;
            }
            let affinity = thread.physical_affinity_mask.get_affinity_mask();
            if (affinity & (1u64 << core)) == 0 {
                continue;
            }
            let active_core = thread.get_active_core();
            if active_core >= 0 && active_core != core {
                continue;
            }
            let thread_id = thread.get_thread_id();
            let priority = thread.get_priority();
            let is_dummy = thread.is_dummy_thread();
            let process_schedule_count = thread.process_schedule_count.clone();
            let replace = best
                .as_ref()
                .map(|(_, best_priority, _, _, _, _)| priority < *best_priority)
                .unwrap_or(true);
            if replace {
                best = Some((
                    thread_id,
                    priority,
                    active_core,
                    affinity,
                    is_dummy,
                    process_schedule_count,
                ));
            }
        }

        let (thread_id, priority, active_core, affinity, is_dummy, process_schedule_count) = best?;
        log::warn!(
            "[PQ_RECOVER] core={} tid={} prio={} active_core={} affinity=0x{:x}",
            core,
            thread_id,
            priority,
            active_core,
            affinity
        );
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
        self.m_priority_queue.get_scheduled_front(core)
    }

    /// Rust-port integrity guard for `KPriorityQueue::GetScheduledFront`.
    ///
    /// Upstream uses intrusive entries owned by `KThread`; if every state
    /// transition calls `KScheduler::OnThreadStateChanged`, scheduled queues
    /// contain only `ThreadState::Runnable` threads. The Rust port stores queue
    /// nodes and cached thread properties inside `KPriorityQueue`, so a missed
    /// removal can leave a stale thread id at the front and stall a core. Keep
    /// the upstream observable contract at the read boundary: the scheduler
    /// never selects a non-runnable queue head.
    pub fn get_scheduled_front_runnable(&mut self, core: i32) -> Option<u64> {
        loop {
            let thread_id = self.m_priority_queue.get_scheduled_front(core)?;
            let is_runnable = self
                .get_thread_by_thread_id(thread_id)
                .map(|thread| thread.lock().unwrap().get_raw_state() == ThreadState::RUNNABLE)
                .unwrap_or(false);
            if is_runnable {
                return Some(thread_id);
            }

            let Some(props) = self.m_priority_queue.get_thread_props(thread_id).cloned() else {
                log::warn!(
                    "[PQ_STALE] core={} tid={} missing thread props at scheduled front",
                    core,
                    thread_id
                );
                if !self
                    .m_priority_queue
                    .remove_scheduled_front_without_props(core, thread_id)
                {
                    return None;
                }
                self.m_scheduler_update_needed
                    .store(true, std::sync::atomic::Ordering::Release);
                continue;
            };
            log::warn!(
                "[PQ_STALE] core={} tid={} non-runnable scheduled front, removing prio={} active_core={} affinity=0x{:x}",
                core,
                thread_id,
                props.priority,
                props.active_core,
                props.affinity
            );
            self.m_priority_queue.remove(
                thread_id,
                props.priority,
                props.active_core,
                props.affinity,
                props.is_dummy,
            );
            self.m_scheduler_update_needed
                .store(true, std::sync::atomic::Ordering::Release);
        }
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
            if let Some(thread) = thread_list.iter().find(|(id, _)| *id == *thread_id) {
                thread.1.lock().unwrap().dummy_thread_end_wait();
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

#[cfg(test)]
mod tests {
    use std::sync::atomic::{AtomicI64, Ordering};
    use std::sync::{Arc, Mutex};

    use super::GlobalSchedulerContext;
    use crate::hle::kernel::k_thread::{KThread, KThreadLock, ThreadState};

    #[test]
    fn get_thread_by_thread_id_does_not_need_thread_mutex() {
        let gsc = GlobalSchedulerContext::new();
        let thread = Arc::new(KThreadLock::new(KThread::new()));
        thread.lock().unwrap().thread_id = 0x1234;
        gsc.add_thread(Arc::clone(&thread));

        let _held = thread.lock().unwrap();
        let looked_up = gsc.get_thread_by_thread_id(0x1234);
        assert!(looked_up.is_some());
    }

    #[test]
    fn remove_thread_only_removes_matching_thread_id() {
        let gsc = GlobalSchedulerContext::new();
        let exiting = Arc::new(KThreadLock::new(KThread::new()));
        let survivor = Arc::new(KThreadLock::new(KThread::new()));
        exiting.lock().unwrap().thread_id = 0x10;
        survivor.lock().unwrap().thread_id = 0x20;

        gsc.add_thread(Arc::clone(&exiting));
        gsc.add_thread(Arc::clone(&survivor));
        gsc.remove_thread(0x10);

        assert!(gsc.get_thread_by_thread_id(0x10).is_none());
        assert!(Arc::ptr_eq(
            &gsc.get_thread_by_thread_id(0x20).unwrap(),
            &survivor
        ));
    }

    #[test]
    fn state_change_remove_increments_process_scheduled_count_after_props_are_removed() {
        let mut gsc = GlobalSchedulerContext::new();
        let scheduled_count = Arc::new(AtomicI64::new(0));

        gsc.on_thread_state_changed(
            100,
            ThreadState::WAITING,
            ThreadState::RUNNABLE,
            44,
            2,
            0b0100,
            false,
            Some(Arc::clone(&scheduled_count)),
        );
        assert_eq!(scheduled_count.load(Ordering::Relaxed), 1);

        gsc.on_thread_state_changed(
            100,
            ThreadState::RUNNABLE,
            ThreadState::WAITING,
            44,
            2,
            0b0100,
            false,
            None,
        );

        assert_eq!(scheduled_count.load(Ordering::Relaxed), 2);
        assert!(gsc.m_priority_queue.get_thread_props(100).is_none());
    }

    #[test]
    fn runnable_priority_change_increments_process_scheduled_count() {
        let mut gsc = GlobalSchedulerContext::new();
        let scheduled_count = Arc::new(AtomicI64::new(0));

        gsc.on_thread_state_changed(
            100,
            ThreadState::WAITING,
            ThreadState::RUNNABLE,
            44,
            2,
            0b0100,
            false,
            Some(Arc::clone(&scheduled_count)),
        );
        gsc.on_thread_priority_changed(100, 44, 29, 2, 0b0100, false, false, 100);

        assert_eq!(scheduled_count.load(Ordering::Relaxed), 2);
        assert_eq!(
            gsc.m_priority_queue.get_thread_props(100).unwrap().priority,
            29
        );
    }

    #[test]
    fn recover_scheduled_front_reinserts_runnable_thread_missing_from_queue() {
        let mut gsc = GlobalSchedulerContext::new();
        let scheduled_count = Arc::new(AtomicI64::new(0));
        let thread = Arc::new(KThreadLock::new(KThread::new()));
        {
            let mut guard = thread.lock().unwrap();
            guard.thread_id = 0x52;
            guard.priority = 21;
            guard.core_id = 2;
            guard.physical_affinity_mask.set_affinity_mask(0b0100);
            guard.process_schedule_count = Some(Arc::clone(&scheduled_count));
            guard.set_state(ThreadState::RUNNABLE);
        }
        gsc.add_thread(Arc::clone(&thread));

        assert_eq!(gsc.get_scheduled_front(2), None);
        assert_eq!(gsc.recover_scheduled_front_from_runnable(2), Some(0x52));
        assert_eq!(gsc.get_scheduled_front(2), Some(0x52));
        assert_eq!(scheduled_count.load(Ordering::Relaxed), 1);
    }

    #[test]
    fn recover_scheduled_front_repairs_props_without_visible_root() {
        let mut gsc = GlobalSchedulerContext::new();
        let scheduled_count = Arc::new(AtomicI64::new(0));
        let thread = Arc::new(KThreadLock::new(KThread::new()));
        {
            let mut guard = thread.lock().unwrap();
            guard.thread_id = 0x52;
            guard.priority = 21;
            guard.core_id = 2;
            guard.physical_affinity_mask.set_affinity_mask(0b0100);
            guard.process_schedule_count = Some(Arc::clone(&scheduled_count));
            guard.set_state(ThreadState::RUNNABLE);
        }
        gsc.add_thread(Arc::clone(&thread));
        gsc.m_priority_queue.push_back(
            0x52,
            21,
            1,
            0b0100,
            false,
            Some(Arc::clone(&scheduled_count)),
        );

        assert!(gsc.m_priority_queue.get_thread_props(0x52).is_some());
        assert_eq!(gsc.get_scheduled_front(2), None);
        assert_eq!(gsc.recover_scheduled_front_from_runnable(2), Some(0x52));
        assert_eq!(gsc.get_scheduled_front(2), Some(0x52));
        assert_eq!(
            gsc.m_priority_queue
                .get_thread_props(0x52)
                .unwrap()
                .active_core,
            2
        );
        assert_eq!(scheduled_count.load(Ordering::Relaxed), 1);
    }
}
