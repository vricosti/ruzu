//! Port of zuyu/src/core/hle/kernel/k_readable_event.h / k_readable_event.cpp
//! Status: COMPLET (upstream-faithful signal path — no KProcess in the flow)
//! Derniere synchro: 2026-04-19
//!
//! KReadableEvent: the readable (waitable) end of a kernel event.
//! Mirrors `KReadableEvent::Signal` which under scheduler lock walks the
//! intrusive waiter list on the owning sync object.

use std::sync::atomic::{AtomicBool, Ordering};

use crate::hle::kernel::k_scheduler_lock::KScopedSchedulerLock;
use crate::hle::kernel::k_synchronization_object::SynchronizationObjectState;
use crate::hle::kernel::svc::svc_results::RESULT_INVALID_STATE;
use crate::hle::result::RESULT_SUCCESS;

/// The readable event object.
/// Matches upstream `KReadableEvent` class (k_readable_event.h).
pub struct KReadableEvent {
    pub object_id: u64,
    pub is_signaled: AtomicBool,
    pub parent_id: Option<u64>,
    pub sync_object: SynchronizationObjectState,
}

impl KReadableEvent {
    /// Acquire the kernel scheduler lock. Matches upstream pattern where
    /// `KReadableEvent::Signal` / `Reset` open `KScopedSchedulerLock sl(kernel)`.
    /// Resolves through the kernel singleton; no GSC mutex round-trip.
    fn lock_scheduler() -> Option<KScopedSchedulerLock<'static>> {
        let scheduler_lock = crate::hle::kernel::kernel::scheduler_lock()?;
        Some(KScopedSchedulerLock::new(scheduler_lock))
    }

    pub fn new() -> Self {
        Self {
            object_id: 0,
            is_signaled: AtomicBool::new(false),
            parent_id: None,
            sync_object: SynchronizationObjectState::new(),
        }
    }

    pub fn initialize(&mut self, parent_id: u64, object_id: u64) {
        self.object_id = object_id;
        self.is_signaled.store(false, Ordering::Relaxed);
        self.parent_id = Some(parent_id);
        debug_assert!(self.sync_object.is_empty());
    }

    pub fn get_parent_id(&self) -> Option<u64> {
        self.parent_id
    }

    /// Signal the readable event. Mirrors upstream `KReadableEvent::Signal`.
    ///
    /// Upstream acquires the scheduler lock and calls `NotifyAvailable` which
    /// walks `KSynchronizationObject::m_thread_list_head` and wakes every
    /// waiting thread. The scheduler lock is recursive, so this is safe to
    /// call from any context — guest syscall or host thread.
    pub fn signal(&mut self) -> u32 {
        let _scheduler_guard = Self::lock_scheduler();

        if !self.is_signaled.load(Ordering::Relaxed) {
            self.is_signaled.store(true, Ordering::Relaxed);
            if std::env::var_os("RUZU_TRACE_EVENTS").is_some() {
                log::info!("EVENT_SIGNAL object_id={} via=signal", self.object_id);
            }
            self.notify_available();
        }
        RESULT_SUCCESS.get_inner_value()
    }

    /// Host-thread signal entrypoint. Identical to `signal` — upstream does
    /// not distinguish guest-context and host-context signals; both hold the
    /// scheduler lock and walk the waiter list.
    pub fn signal_from_host(&mut self) -> u32 {
        self.signal()
    }

    /// Clear the readable event. Mirrors upstream `KReadableEvent::Clear`.
    pub fn clear(&mut self) -> u32 {
        let _ = self.reset();
        RESULT_SUCCESS.get_inner_value()
    }

    /// Reset the event. Mirrors upstream `KReadableEvent::Reset`.
    pub fn reset(&mut self) -> u32 {
        let _scheduler_guard = Self::lock_scheduler();
        if !self.is_signaled.load(Ordering::Relaxed) {
            return RESULT_INVALID_STATE.get_inner_value();
        }
        self.is_signaled.store(false, Ordering::Relaxed);
        RESULT_SUCCESS.get_inner_value()
    }

    /// Is the event signaled?
    pub fn is_signaled(&self) -> bool {
        if let Some(scheduler_lock) = crate::hle::kernel::kernel::scheduler_lock() {
            debug_assert!(scheduler_lock.is_locked_by_current_thread());
        }
        self.is_signaled.load(Ordering::Relaxed)
    }

    /// Mirrors upstream `KSynchronizationObject::NotifyAvailable` as invoked
    /// by Signal: walk the intrusive waiter list and notify each thread.
    ///
    /// Caller must hold the scheduler lock.
    fn notify_available(&self) {
        if !self.is_signaled.load(Ordering::Relaxed) {
            return;
        }
        unsafe {
            crate::hle::kernel::k_synchronization_object::notify_waiters_on_state(
                &self.sync_object,
                self.object_id,
                RESULT_SUCCESS.get_inner_value(),
            );
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
    fn signal_and_clear() {
        let mut event = KReadableEvent::new();
        assert!(!event.is_signaled());
        assert_eq!(event.signal(), RESULT_SUCCESS.get_inner_value());
        assert!(event.is_signaled());
        assert_eq!(event.clear(), RESULT_SUCCESS.get_inner_value());
        assert!(!event.is_signaled());
    }

    #[test]
    fn reset_not_signaled() {
        let mut event = KReadableEvent::new();
        assert_eq!(event.reset(), RESULT_INVALID_STATE.get_inner_value());
    }

    #[test]
    fn reset_signaled() {
        let mut event = KReadableEvent::new();
        event.signal();
        assert_eq!(event.reset(), RESULT_SUCCESS.get_inner_value());
        assert!(!event.is_signaled());
    }
}
