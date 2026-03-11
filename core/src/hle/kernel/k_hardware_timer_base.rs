//! Port of zuyu/src/core/hle/kernel/k_hardware_timer_base.h
//! Status: COMPLET (stub — runtime dependencies not yet available)
//! Derniere synchro: 2026-03-11
//!
//! KHardwareTimerBase: base class for the hardware timer, managing a tree
//! of timer tasks. Upstream uses an intrusive red-black tree of KTimerTask
//! and a KSpinLock.

use std::collections::BTreeMap;
use std::sync::Mutex;

/// Placeholder for a timer task identifier.
/// Upstream this is a KTimerTask pointer stored in an intrusive tree.
pub type TimerTaskId = u64;

/// Base class for the hardware timer.
///
/// Manages a sorted tree of timer tasks, each with an absolute wakeup time.
pub struct KHardwareTimerBase {
    m_lock: Mutex<()>,
    /// Maps task_time -> task_id. Upstream uses an intrusive rbtree.
    m_task_tree: BTreeMap<i64, Vec<TimerTaskId>>,
    m_next_task: Option<(i64, TimerTaskId)>,
}

impl KHardwareTimerBase {
    pub fn new() -> Self {
        Self {
            m_lock: Mutex::new(()),
            m_task_tree: BTreeMap::new(),
            m_next_task: None,
        }
    }

    /// Cancel a previously registered task.
    /// Note: Upstream acquires the spin lock here. Since we have &mut self,
    /// external synchronization is assumed (matching KScopedDisableDispatch +
    /// KScopedSpinLock in the caller).
    pub fn cancel_task(&mut self, task_id: TimerTaskId, task_time: i64) {
        if task_time > 0 {
            self.remove_task_from_tree(task_id, task_time);
        }
    }

    /// Get a mutable reference to the lock (for subclass use).
    pub fn get_lock(&self) -> &Mutex<()> {
        &self.m_lock
    }

    /// Process all tasks whose time has elapsed.
    /// Returns the next task time, or 0 if no tasks remain.
    pub fn do_interrupt_task_impl<F>(&mut self, cur_time: i64, mut handler: F) -> i64
    where
        F: FnMut(TimerTaskId),
    {
        loop {
            // Get the next task.
            let next = match self.m_next_task {
                Some((time, id)) => (time, id),
                None => return 0,
            };

            // If the task is in the future, return its time.
            if next.0 > cur_time {
                return next.0;
            }

            // Remove and handle the task.
            let task_id = next.1;
            let task_time = next.0;
            self.remove_task_from_tree(task_id, task_time);
            handler(task_id);
        }
    }

    /// Register an absolute timer task. Returns true if this task is now
    /// the earliest (i.e., the interrupt should be re-armed).
    pub fn register_absolute_task_impl(&mut self, task_id: TimerTaskId, task_time: i64) -> bool {
        assert!(task_time > 0);

        // Insert into tree.
        self.m_task_tree
            .entry(task_time)
            .or_insert_with(Vec::new)
            .push(task_id);

        // Update next task if relevant.
        if let Some((next_time, _)) = self.m_next_task {
            if next_time <= task_time {
                return false;
            }
        }
        self.m_next_task = Some((task_time, task_id));
        true
    }

    fn remove_task_from_tree(&mut self, task_id: TimerTaskId, task_time: i64) {
        if let Some(tasks) = self.m_task_tree.get_mut(&task_time) {
            tasks.retain(|&id| id != task_id);
            if tasks.is_empty() {
                self.m_task_tree.remove(&task_time);
            }
        }

        // Update next task.
        if let Some((_, next_id)) = self.m_next_task {
            if next_id == task_id {
                self.m_next_task = self
                    .m_task_tree
                    .iter()
                    .next()
                    .map(|(&time, ids)| (time, ids[0]));
            }
        }
    }
}

impl Default for KHardwareTimerBase {
    fn default() -> Self {
        Self::new()
    }
}
