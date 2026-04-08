//! Port of zuyu/src/core/hle/kernel/k_scheduler_lock.h
//! Status: EN COURS
//! Derniere synchro: 2026-03-16
//!
//! KAbstractSchedulerLock — recursive lock that disables/enables scheduling.
//! Parameterized by a SchedulerType that provides DisableScheduling,
//! EnableScheduling, and UpdateHighestPriorityThreads static methods.
//!
//! Upstream uses a template `KAbstractSchedulerLock<SchedulerType>`.
//! Since there's only one SchedulerType (KScheduler), we use function
//! pointers set at initialization to break the circular dependency.

use std::sync::atomic::{AtomicU64, Ordering};

use super::k_spin_lock::KAlignedSpinLock;

/// Callbacks matching upstream SchedulerType template parameter static methods.
/// Set during initialization to break the KScheduler <-> GlobalSchedulerContext cycle.
pub struct SchedulerCallbacks {
    pub disable_scheduling: fn(),
    pub enable_scheduling: fn(u64),
    pub update_highest_priority_threads: fn() -> u64,
}

/// DisableScheduling callback matching upstream `KScheduler::DisableScheduling(kernel)`.
/// Increments the current thread's disable_dispatch_count via the thread-local.
fn default_disable_scheduling() {
    super::kernel::with_current_thread_fast_mut(|t| {
        debug_assert!(t.get_disable_dispatch_count() >= 0);
        t.disable_dispatch();
    });
}

/// EnableScheduling callback matching upstream `KScheduler::EnableScheduling(kernel, cores_needing_scheduling)`.
/// Decrements dispatch count. If it reaches 0, triggers rescheduling.
/// Port of upstream `KScheduler::EnableScheduling(kernel, cores_needing_scheduling)`.
///
/// When the scheduler lock's outermost scope exits (lock_count goes to 0),
/// this callback runs. If the current thread's dispatch count is about to
/// reach 0, upstream calls `scheduler->RescheduleCurrentCore()` which
/// triggers `ScheduleImpl()` → fiber switch. This is the mechanism that
/// suspends a WAITING thread after `KScopedSchedulerLockAndSleep` drops.
fn default_enable_scheduling(cores_needing_scheduling: u64) {
    let dispatch_count = super::kernel::with_current_thread_fast_mut(|t| {
        t.get_disable_dispatch_count()
    });

    match dispatch_count {
        Some(count) if count > 1 => {
            // Nested lock — just decrement.
            super::kernel::with_current_thread_fast_mut(|t| {
                t.enable_dispatch();
            });
        }
        Some(1) => {
            // Outermost unlock — must reschedule like upstream.
            // Upstream: scheduler->RescheduleOtherCores(cores);
            //           scheduler->RescheduleCurrentCore();
            // RescheduleCurrentCore: EnableDispatch + if needs_scheduling { RescheduleCurrentCoreImpl }
            super::kernel::with_current_thread_fast_mut(|t| {
                t.enable_dispatch();
            });

            // Trigger RescheduleCurrentCoreImpl via the current scheduler.
            if let Some(kernel) = super::kernel::get_kernel_ref() {
                if let Some(scheduler_arc) = kernel.current_scheduler() {
                    // Interrupt other cores that need rescheduling.
                    if cores_needing_scheduling != 0 {
                        for core_id in 0..crate::hardware_properties::NUM_CPU_CORES as usize {
                            if cores_needing_scheduling & (1u64 << core_id) != 0 {
                                if let Some(core) = kernel.physical_core(core_id) {
                                    core.interrupt();
                                }
                            }
                        }
                    }

                    let needs = {
                        let sched = scheduler_arc.lock().unwrap();
                        sched.needs_scheduling()
                    };
                    if needs {
                        // Do the fiber switch — this is the key upstream behavior.
                        // Get a raw pointer to avoid holding the Mutex across the yield.
                        let sched_ptr = {
                            let guard = scheduler_arc.lock().unwrap();
                            &*guard as *const super::k_scheduler::KScheduler
                                as *mut super::k_scheduler::KScheduler
                        };
                        unsafe {
                            (*sched_ptr).reschedule_current_core_impl();
                        }
                    }
                }
            }
        }
        _ => {
            // No current thread (host thread) — just a no-op.
        }
    }
}

/// UpdateHighestPriorityThreads callback matching upstream
/// `KScheduler::UpdateHighestPriorityThreads(kernel)`.
/// Checks IsSchedulerUpdateNeeded and calls UpdateHighestPriorityThreadsImpl.
/// Returns bitmask of cores needing rescheduling.
fn default_update_highest_priority_threads() -> u64 {
    // This callback is called from within the scheduler lock's Unlock().
    // It needs access to the GlobalSchedulerContext to check the update flag
    // and run UpdateHighestPriorityThreadsImpl.
    // The actual implementation is wired through the SchedulerCallbacks
    // set by the kernel when it has the necessary context.
    // Default: return 0 (no cores need rescheduling).
    0
}

static DEFAULT_CALLBACKS: SchedulerCallbacks = SchedulerCallbacks {
    disable_scheduling: default_disable_scheduling,
    enable_scheduling: default_enable_scheduling,
    update_highest_priority_threads: default_update_highest_priority_threads,
};

/// KAbstractSchedulerLock — recursive scheduler lock.
///
/// Mirrors upstream `Kernel::KAbstractSchedulerLock<SchedulerType>`.
/// Uses a spinlock for the underlying mutual exclusion, with a recursive
/// lock count and owner thread tracking.
///
/// Uses interior mutability: lock()/unlock() take &self to allow
/// the scoped guard (KScopedSchedulerLock) to work without &mut.
pub struct KAbstractSchedulerLock {
    m_spin_lock: KAlignedSpinLock,
    /// Mutable state protected by the spin lock.
    m_lock_count: std::cell::Cell<i32>,
    /// Owner thread id (0 = no owner).
    /// Upstream: std::atomic<KThread*> m_owner_thread
    m_owner_thread: AtomicU64,
    /// Callbacks to KScheduler static methods.
    callbacks: &'static SchedulerCallbacks,
}

// SAFETY: KAbstractSchedulerLock uses a spinlock for synchronization.
// The Cell<i32> is only accessed while the spinlock is held.
unsafe impl Send for KAbstractSchedulerLock {}
unsafe impl Sync for KAbstractSchedulerLock {}

impl KAbstractSchedulerLock {
    pub fn new() -> Self {
        Self {
            m_spin_lock: KAlignedSpinLock::new(),
            m_lock_count: std::cell::Cell::new(0),
            m_owner_thread: AtomicU64::new(0),
            callbacks: &DEFAULT_CALLBACKS,
        }
    }

    /// Set the scheduler callbacks. Called once KScheduler static methods are ready.
    pub fn set_callbacks(&mut self, callbacks: &'static SchedulerCallbacks) {
        self.callbacks = callbacks;
    }

    /// Check if the lock is held by the current thread.
    /// Upstream: compares m_owner_thread with GetCurrentThreadPointer(m_kernel).
    /// Uses the thread-local current thread's ID to compare against the stored owner.
    pub fn is_locked_by_current_thread(&self) -> bool {
        let owner = self.m_owner_thread.load(Ordering::Relaxed);
        if owner == 0 {
            return false;
        }
        // Compare against the thread-local current thread id cache.
        // Upstream compares a thread-local KThread* directly and does not
        // relock the current thread object here.
        if let Some(current_id) = super::kernel::get_current_thread_id_fast() {
            owner == current_id
        } else if let Some(current_thread) = super::kernel::get_current_emu_thread() {
            owner == current_thread.lock().unwrap().get_thread_id()
        } else {
            self.m_lock_count.get() > 0
        }
    }

    /// Lock the scheduler lock.
    /// Matches upstream `KAbstractSchedulerLock::Lock()`.
    pub fn lock(&self) {
        let current_tid = super::kernel::get_current_thread_id_fast();
        log::trace!(
            "KAbstractSchedulerLock::lock enter current_tid={:?} owner={} count={}",
            current_tid,
            self.m_owner_thread.load(Ordering::Relaxed),
            self.m_lock_count.get()
        );
        if self.is_locked_by_current_thread() {
            debug_assert!(self.m_lock_count.get() > 0);
        } else {
            log::trace!(
                "KAbstractSchedulerLock::lock current_tid={:?} before disable_scheduling",
                current_tid
            );
            (self.callbacks.disable_scheduling)();
            log::trace!(
                "KAbstractSchedulerLock::lock current_tid={:?} before spin_lock",
                current_tid
            );
            self.m_spin_lock.lock();
            log::trace!(
                "KAbstractSchedulerLock::lock current_tid={:?} acquired spin_lock",
                current_tid
            );

            debug_assert!(self.m_lock_count.get() == 0);
            debug_assert!(self.m_owner_thread.load(Ordering::Relaxed) == 0);

            // Upstream: m_owner_thread = GetCurrentThreadPointer(m_kernel)
            // Store the current thread's ID as owner marker.
            let owner_id = super::kernel::get_current_thread_id_fast()
                .or_else(|| {
                    super::kernel::get_current_emu_thread()
                        .map(|thread| thread.lock().unwrap().get_thread_id())
                })
                .unwrap_or(1);
            self.m_owner_thread.store(owner_id, Ordering::Relaxed);
        }

        self.m_lock_count.set(self.m_lock_count.get() + 1);
        log::trace!(
            "KAbstractSchedulerLock::lock exit current_tid={:?} owner={} count={}",
            current_tid,
            self.m_owner_thread.load(Ordering::Relaxed),
            self.m_lock_count.get()
        );
    }

    /// Unlock the scheduler lock.
    /// Matches upstream `KAbstractSchedulerLock::Unlock()`.
    pub fn unlock(&self) {
        debug_assert!(self.is_locked_by_current_thread());
        debug_assert!(self.m_lock_count.get() > 0);

        log::trace!(
            "KAbstractSchedulerLock::unlock enter owner={} count={}",
            self.m_owner_thread.load(Ordering::Relaxed),
            self.m_lock_count.get()
        );
        let new_count = self.m_lock_count.get() - 1;
        self.m_lock_count.set(new_count);

        if new_count == 0 {
            std::sync::atomic::fence(Ordering::SeqCst);

            let cores_needing_scheduling = (self.callbacks.update_highest_priority_threads)();
            log::trace!(
                "KAbstractSchedulerLock::unlock zero-count cores_needing_scheduling=0x{:x}",
                cores_needing_scheduling
            );

            self.m_owner_thread.store(0, Ordering::Relaxed);
            self.m_spin_lock.unlock();
            log::trace!("KAbstractSchedulerLock::unlock before enable_scheduling");
            (self.callbacks.enable_scheduling)(cores_needing_scheduling);
            log::trace!("KAbstractSchedulerLock::unlock after enable_scheduling");
        }
    }

    pub fn get_lock_count(&self) -> i32 {
        self.m_lock_count.get()
    }

    /// Get the update_highest_priority_threads callback, if wired.
    /// Returns None if callbacks use the default (noop).
    pub fn get_update_callback(&self) -> Option<fn() -> u64> {
        let f = self.callbacks.update_highest_priority_threads;
        // Check if it's the default noop (returns 0 always).
        // We can't distinguish at runtime, so always return it.
        Some(f)
    }
}

impl Default for KAbstractSchedulerLock {
    fn default() -> Self {
        Self::new()
    }
}

/// KScopedSchedulerLock — RAII wrapper for the scheduler lock.
/// Matches upstream `KScopedSchedulerLock` (typedef of KScopedLock<KSchedulerLockType>).
pub struct KScopedSchedulerLock<'a> {
    lock: &'a KAbstractSchedulerLock,
}

impl<'a> KScopedSchedulerLock<'a> {
    pub fn new(lock: &'a KAbstractSchedulerLock) -> Self {
        lock.lock();
        Self { lock }
    }
}

impl Drop for KScopedSchedulerLock<'_> {
    fn drop(&mut self) {
        self.lock.unlock();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scheduler_lock_basic() {
        let lock = KAbstractSchedulerLock::new();
        assert_eq!(lock.get_lock_count(), 0);
        lock.lock();
        assert_eq!(lock.get_lock_count(), 1);
        lock.unlock();
        assert_eq!(lock.get_lock_count(), 0);
    }

    #[test]
    fn test_scheduler_lock_recursive() {
        let lock = KAbstractSchedulerLock::new();
        lock.lock();
        assert_eq!(lock.get_lock_count(), 1);
        lock.lock();
        assert_eq!(lock.get_lock_count(), 2);
        lock.unlock();
        assert_eq!(lock.get_lock_count(), 1);
        lock.unlock();
        assert_eq!(lock.get_lock_count(), 0);
    }

    #[test]
    fn test_scoped_scheduler_lock() {
        let lock = KAbstractSchedulerLock::new();
        assert_eq!(lock.get_lock_count(), 0);
        {
            let _guard = KScopedSchedulerLock::new(&lock);
            assert_eq!(lock.get_lock_count(), 1);
        }
        assert_eq!(lock.get_lock_count(), 0);
    }
}
