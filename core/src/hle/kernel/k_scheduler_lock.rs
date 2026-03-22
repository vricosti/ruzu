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
    if let Some(thread) = super::kernel::get_current_thread_pointer() {
        let mut t = thread.lock().unwrap();
        debug_assert!(t.get_disable_dispatch_count() >= 0);
        t.disable_dispatch();
    }
}

/// EnableScheduling callback matching upstream `KScheduler::EnableScheduling(kernel, cores_needing_scheduling)`.
/// Decrements dispatch count. If it reaches 0, triggers rescheduling.
fn default_enable_scheduling(_cores_needing_scheduling: u64) {
    if let Some(thread) = super::kernel::get_current_thread_pointer() {
        let mut t = thread.lock().unwrap();
        debug_assert!(t.get_disable_dispatch_count() >= 1);

        // Upstream: if no scheduler or phantom mode, use HLE reschedule path.
        // We don't have kernel reference here, so we use the simpler path:
        // reschedule other cores if needed, then enable dispatch.
        // The full scheduler-aware path is handled by KScheduler::enable_scheduling_with_scheduler
        // when called with scheduler context.
        if t.get_disable_dispatch_count() > 1 {
            t.enable_dispatch();
        } else {
            // Dispatch count is 1, will go to 0.
            // Upstream: RescheduleCurrentCore or RescheduleCurrentHLEThread.
            // Without scheduler context, do HLE reschedule: just enable dispatch.
            // The actual rescheduling is triggered by the caller via
            // KScheduler::enable_scheduling_with_scheduler when a scheduler is available.
            t.enable_dispatch();
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
        // Compare with the current thread's unique ID (thread_id stored as u64).
        if let Some(current) = super::kernel::get_current_thread_pointer() {
            let current_id = current.lock().unwrap().get_thread_id();
            owner == current_id
        } else {
            // No current thread set — fall back to lock_count check
            // for compatibility during initialization.
            self.m_lock_count.get() > 0
        }
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

            // Upstream: m_owner_thread = GetCurrentThreadPointer(m_kernel)
            // Store the current thread's ID as owner marker.
            let owner_id = super::kernel::get_current_thread_pointer()
                .map(|t| t.lock().unwrap().get_thread_id())
                .unwrap_or(1); // Use 1 as fallback during init
            self.m_owner_thread.store(owner_id, Ordering::Relaxed);
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
