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

/// Single-core cooperative model callbacks.
///
/// In the cooperative model, only one guest thread runs at a time.
/// The process lock provides the scheduling exclusion that upstream gets
/// from DisableScheduling/EnableScheduling + the scheduler lock.
///
/// DisableScheduling: upstream increments thread dispatch_count to prevent
/// rescheduling during critical sections. In our cooperative model, the
/// Rust mutex on KScheduler/KProcess serves this purpose.
///
/// EnableScheduling: upstream decrements dispatch_count and triggers
/// RescheduleCurrentCore if needed. In our model, scheduling decisions
/// happen at SVC return boundaries in the cpu_manager dispatch loop.
///
/// UpdateHighestPriorityThreads: upstream selects the highest priority
/// thread per core. In our model, select_next_thread_from_pq handles this
/// at dispatch time. Returns 0 (no cores need IPI-based rescheduling).
fn cooperative_disable_scheduling() {
    // No-op: process lock provides exclusion in cooperative model.
}
fn cooperative_enable_scheduling(_cores_needing_scheduling: u64) {
    // No-op: scheduling happens at SVC boundaries in cooperative model.
}
fn cooperative_update_highest_priority_threads() -> u64 {
    // No cores need rescheduling via IPI in cooperative model.
    0
}

static COOPERATIVE_CALLBACKS: SchedulerCallbacks = SchedulerCallbacks {
    disable_scheduling: cooperative_disable_scheduling,
    enable_scheduling: cooperative_enable_scheduling,
    update_highest_priority_threads: cooperative_update_highest_priority_threads,
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
            callbacks: &COOPERATIVE_CALLBACKS,
        }
    }

    /// Set the scheduler callbacks. Called once KScheduler static methods are ready.
    pub fn set_callbacks(&mut self, callbacks: &'static SchedulerCallbacks) {
        self.callbacks = callbacks;
    }

    /// Check if the lock is held by the current thread.
    /// Upstream: `IsLockedByCurrentThread()` compares m_owner_thread with
    /// GetCurrentThreadPointer(m_kernel).
    ///
    /// TODO: compare with actual current thread once thread-local current
    /// thread tracking is implemented.
    pub fn is_locked_by_current_thread(&self) -> bool {
        // For single-core cooperative model, if lock_count > 0 we are the owner.
        // This is correct because only one thread runs at a time.
        self.m_lock_count.get() > 0
    }

    /// Lock the scheduler lock.
    /// Matches upstream `KAbstractSchedulerLock::Lock()`.
    pub fn lock(&self) {
        if self.is_locked_by_current_thread() {
            debug_assert!(self.m_lock_count.get() > 0);
        } else {
            (self.callbacks.disable_scheduling)();
            self.m_spin_lock.lock();

            debug_assert!(self.m_lock_count.get() == 0);
            debug_assert!(self.m_owner_thread.load(Ordering::Relaxed) == 0);

            // TODO: m_owner_thread = GetCurrentThreadPointer(m_kernel)
            self.m_owner_thread.store(1, Ordering::Relaxed); // placeholder
        }

        self.m_lock_count.set(self.m_lock_count.get() + 1);
    }

    /// Unlock the scheduler lock.
    /// Matches upstream `KAbstractSchedulerLock::Unlock()`.
    pub fn unlock(&self) {
        debug_assert!(self.is_locked_by_current_thread());
        debug_assert!(self.m_lock_count.get() > 0);

        let new_count = self.m_lock_count.get() - 1;
        self.m_lock_count.set(new_count);

        if new_count == 0 {
            std::sync::atomic::fence(Ordering::SeqCst);

            let cores_needing_scheduling =
                (self.callbacks.update_highest_priority_threads)();

            self.m_owner_thread.store(0, Ordering::Relaxed);
            self.m_spin_lock.unlock();

            (self.callbacks.enable_scheduling)(cores_needing_scheduling);
        }
    }

    pub fn get_lock_count(&self) -> i32 {
        self.m_lock_count.get()
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
