//! Port of zuyu/src/core/hle/kernel/k_scheduler_lock.h
//! Status: EN COURS
//! Derniere synchro: 2026-03-11
//!
//! KAbstractSchedulerLock — recursive lock that disables/enables scheduling.
//! Parameterized by a SchedulerType that provides DisableScheduling,
//! EnableScheduling, and UpdateHighestPriorityThreads static methods.
//!
//! Stubbed until KScheduler, KThread, and GlobalSchedulerContext are ported.

use std::sync::atomic::{AtomicUsize, Ordering};

use super::k_spin_lock::KAlignedSpinLock;

/// Trait representing the SchedulerType template parameter.
/// Mirrors the static methods that KAbstractSchedulerLock delegates to.
pub trait SchedulerType {
    fn disable_scheduling(kernel: usize);
    fn enable_scheduling(kernel: usize, cores_needing_scheduling: u64);
    fn update_highest_priority_threads(kernel: usize) -> u64;
}

/// KAbstractSchedulerLock — recursive scheduler lock.
///
/// Mirrors upstream `Kernel::KAbstractSchedulerLock<SchedulerType>`.
/// Uses a spinlock for the underlying mutual exclusion, with a recursive
/// lock count and owner thread tracking.
pub struct KAbstractSchedulerLock {
    m_kernel: usize,
    m_spin_lock: KAlignedSpinLock,
    m_lock_count: i32,
    /// Owner thread (opaque pointer-sized value, 0 = no owner).
    // TODO: Replace with AtomicPtr<KThread> when KThread is ported.
    m_owner_thread: AtomicUsize,
}

impl KAbstractSchedulerLock {
    pub fn new(kernel: usize) -> Self {
        Self {
            m_kernel: kernel,
            m_spin_lock: KAlignedSpinLock::new(),
            m_lock_count: 0,
            m_owner_thread: AtomicUsize::new(0),
        }
    }

    /// Check if the lock is held by the current thread.
    /// Mirrors upstream `IsLockedByCurrentThread()`.
    pub fn is_locked_by_current_thread(&self) -> bool {
        // TODO: Compare m_owner_thread with GetCurrentThreadPointer(m_kernel).
        // For now, return false as a stub.
        false
    }

    /// Lock the scheduler lock.
    /// Mirrors upstream `KAbstractSchedulerLock::Lock()`.
    ///
    /// TODO: Requires SchedulerType::DisableScheduling and GetCurrentThreadPointer.
    pub fn lock(&mut self) {
        if self.is_locked_by_current_thread() {
            debug_assert!(self.m_lock_count > 0);
        } else {
            // TODO: SchedulerType::DisableScheduling(m_kernel);
            self.m_spin_lock.lock();

            debug_assert!(self.m_lock_count == 0);
            debug_assert!(self.m_owner_thread.load(Ordering::Relaxed) == 0);

            // TODO: m_owner_thread = GetCurrentThreadPointer(m_kernel);
            self.m_owner_thread.store(1, Ordering::Relaxed); // placeholder
        }

        self.m_lock_count += 1;
    }

    /// Unlock the scheduler lock.
    /// Mirrors upstream `KAbstractSchedulerLock::Unlock()`.
    ///
    /// TODO: Requires SchedulerType::EnableScheduling and UpdateHighestPriorityThreads.
    pub fn unlock(&mut self) {
        debug_assert!(self.m_lock_count > 0);

        self.m_lock_count -= 1;
        if self.m_lock_count == 0 {
            std::sync::atomic::fence(Ordering::SeqCst);

            // TODO: let cores_needing_scheduling =
            //     SchedulerType::UpdateHighestPriorityThreads(m_kernel);

            self.m_owner_thread.store(0, Ordering::Relaxed);
            self.m_spin_lock.unlock();

            // TODO: SchedulerType::EnableScheduling(m_kernel, cores_needing_scheduling);
        }
    }

    /// Get the current lock count.
    pub fn get_lock_count(&self) -> i32 {
        self.m_lock_count
    }
}

/// KScopedSchedulerLock — RAII wrapper for the scheduler lock.
/// Mirrors upstream `KScopedSchedulerLock` (typedef of KScopedLock<KSchedulerLockType>).
pub struct KScopedSchedulerLock<'a> {
    lock: &'a mut KAbstractSchedulerLock,
}

impl<'a> KScopedSchedulerLock<'a> {
    pub fn new(lock: &'a mut KAbstractSchedulerLock) -> Self {
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
        let mut lock = KAbstractSchedulerLock::new(0);
        assert_eq!(lock.get_lock_count(), 0);
        lock.lock();
        assert_eq!(lock.get_lock_count(), 1);
        lock.unlock();
        assert_eq!(lock.get_lock_count(), 0);
    }
}
