//! Port of zuyu/src/core/hle/kernel/k_readable_event.h / k_readable_event.cpp
//! Status: Partial (signal/reset behavior ported)
//! Derniere synchro: 2026-03-14
//!
//! KReadableEvent: the readable (waitable) end of a kernel event.

use std::sync::{Arc, Mutex};

use crate::hle::kernel::k_process::KProcess;
use crate::hle::kernel::k_scheduler::KScheduler;
use crate::hle::kernel::k_scheduler_lock::KScopedSchedulerLock;
use crate::hle::kernel::k_synchronization_object::SynchronizationObjectState;
use crate::hle::kernel::svc::svc_results::RESULT_INVALID_STATE;
use crate::hle::result::RESULT_SUCCESS;

/// The readable event object.
/// Matches upstream `KReadableEvent` class (k_readable_event.h).
pub struct KReadableEvent {
    pub object_id: u64,
    pub is_signaled: bool,
    pub parent_id: Option<u64>,
    pub sync_object: SynchronizationObjectState,
}

impl KReadableEvent {
    fn lock_scheduler() -> Option<KScopedSchedulerLock<'static>> {
        let kernel = crate::hle::kernel::kernel::get_kernel_ref()?;
        let scheduler_lock = {
            let gsc = kernel.global_scheduler_context()?.lock().unwrap();
            std::ptr::addr_of!(*gsc.scheduler_lock())
                as *const crate::hle::kernel::k_scheduler_lock::KAbstractSchedulerLock
        };

        if scheduler_lock.is_null() {
            return None;
        }

        Some(KScopedSchedulerLock::new(unsafe { &*scheduler_lock }))
    }

    pub fn new() -> Self {
        Self {
            object_id: 0,
            is_signaled: false,
            parent_id: None,
            sync_object: SynchronizationObjectState::new(),
        }
    }

    /// Initialize with a parent event.
    pub fn initialize(&mut self, parent_id: u64, object_id: u64) {
        self.object_id = object_id;
        self.is_signaled = false;
        self.parent_id = Some(parent_id);
        self.sync_object.clear_waiters();
    }

    pub fn get_parent_id(&self) -> Option<u64> {
        self.parent_id
    }

    /// Signal the readable event.
    /// Matches upstream `KReadableEvent::Signal`.
    ///
    /// Re-notifies on every call (not just false→true transitions) because
    /// in the cooperative scheduler, signals from host threads can fire
    /// before waiters are linked.
    pub fn signal(&mut self, process: &mut KProcess, scheduler: &Arc<Mutex<KScheduler>>) -> u32 {
        let _scheduler_guard = Self::lock_scheduler();
        self.is_signaled = true;
        self.notify_available(process, scheduler);
        RESULT_SUCCESS.get_inner_value()
    }

    /// Signal without acquiring the scheduler lock. Used by host threads
    /// (audio ADSP, vsync conductor) where the scheduler spinlock causes
    /// livelock because guest-fiber OS threads hold it continuously during
    /// their wait/reschedule loops.
    pub fn signal_from_host(
        &mut self,
        process: &mut KProcess,
        scheduler: &Arc<Mutex<KScheduler>>,
    ) -> u32 {
        self.is_signaled = true;
        self.notify_available(process, scheduler);
        RESULT_SUCCESS.get_inner_value()
    }

    /// Clear the readable event.
    /// Matches upstream `KReadableEvent::Clear`.
    pub fn clear(&mut self) -> u32 {
        let _ = self.reset();
        RESULT_SUCCESS.get_inner_value()
    }

    /// Reset the event (clear and return invalid state if not signaled).
    /// Matches upstream `KReadableEvent::Reset`.
    pub fn reset(&mut self) -> u32 {
        let _scheduler_guard = Self::lock_scheduler();
        if !self.is_signaled {
            return RESULT_INVALID_STATE.get_inner_value();
        }
        self.is_signaled = false;
        RESULT_SUCCESS.get_inner_value()
    }

    /// Is the event signaled?
    pub fn is_signaled(&self) -> bool {
        self.is_signaled
    }

    pub fn link_waiter(&mut self, process: &mut KProcess, thread_id: u64) {
        self.sync_object.link_waiter(
            process,
            crate::hle::kernel::k_synchronization_object::SynchronizationWaitNode {
                object_id: self.object_id,
                handle:
                    crate::hle::kernel::k_synchronization_object::SynchronizationWaitNodeHandle {
                        thread_id,
                        wait_index: 0,
                    },
            },
        );
    }

    pub fn unlink_waiter(&mut self, process: &mut KProcess, thread_id: u64) {
        self.sync_object
            .unlink_waiter(process, thread_id, self.object_id, 0);
    }

    fn notify_available(&mut self, process: &mut KProcess, scheduler: &Arc<Mutex<KScheduler>>) {
        if !self.is_signaled {
            return;
        }
        let waiters = self.sync_object.waiter_snapshot(process);
        let outcome = crate::hle::kernel::k_synchronization_object::process_waiter_snapshot(
            process,
            self.object_id,
            &waiters,
            RESULT_SUCCESS.get_inner_value(),
        );

        use std::sync::atomic::{AtomicU32, Ordering};
        static NOTIFY_COUNT: AtomicU32 = AtomicU32::new(0);
        let c = NOTIFY_COUNT.fetch_add(1, Ordering::Relaxed);
        if c < 5 || c % 300 == 0 {
            log::info!(
                "KReadableEvent::notify_available #{}: object_id={} waiters={} woke_any={} unlink={}",
                c, self.object_id, waiters.len(), outcome.woke_any, outcome.unlink_thread_ids.len()
            );
        }

        for thread_id in outcome.unlink_thread_ids {
            self.unlink_waiter(process, thread_id);
        }

        if outcome.woke_any {
            scheduler.lock().unwrap().request_schedule();
        }
    }

    /// Destroy the readable event.
    pub fn destroy(&mut self) {
        let _scheduler_guard = Self::lock_scheduler();

        let Some(parent_id) = self.parent_id else {
            return;
        };

        let Some(kernel) = crate::hle::kernel::kernel::get_kernel_ref() else {
            return;
        };
        let Some(owner_process_id) = kernel.get_event_owner_process_id(parent_id) else {
            return;
        };
        let Some(process_arc) = kernel.get_process_by_id(owner_process_id) else {
            return;
        };
        let mut process = process_arc.lock().unwrap();
        let Some(parent) = process.get_event_by_object_id(parent_id) else {
            return;
        };
        parent.lock().unwrap().on_readable_event_destroyed();
    }
}

impl Default for KReadableEvent {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_signal_and_clear() {
        let mut event = KReadableEvent::new();
        assert!(!event.is_signaled());
        let mut process = KProcess::new();
        let scheduler = Arc::new(Mutex::new(KScheduler::new(0)));
        assert_eq!(
            event.signal(&mut process, &scheduler),
            RESULT_SUCCESS.get_inner_value()
        );
        assert!(event.is_signaled());
        assert_eq!(event.clear(), RESULT_SUCCESS.get_inner_value());
        assert!(!event.is_signaled());
    }

    #[test]
    fn test_reset_not_signaled() {
        let mut event = KReadableEvent::new();
        assert_eq!(event.reset(), RESULT_INVALID_STATE.get_inner_value());
    }

    #[test]
    fn test_reset_signaled() {
        let mut event = KReadableEvent::new();
        let mut process = KProcess::new();
        let scheduler = Arc::new(Mutex::new(KScheduler::new(0)));
        event.signal(&mut process, &scheduler);
        assert_eq!(event.reset(), RESULT_SUCCESS.get_inner_value());
        assert!(!event.is_signaled());
    }
}
