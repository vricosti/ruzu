//! Port of zuyu/src/core/hle/kernel/k_light_lock.h and k_light_lock.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-11
//!
//! KLightLock — lightweight lock used within the kernel.
//! Uses an atomic tag for fast-path locking, with slow paths that interact
//! with the scheduler when contention occurs.
//!
//! The slow paths (LockSlowPath, UnlockSlowPath) require KThread, KScheduler,
//! and KThreadQueue, so they are stubbed until those types are ported.

use std::sync::atomic::{AtomicUsize, Ordering};

/// KLightLock — lightweight kernel lock.
///
/// Mirrors upstream `Kernel::KLightLock`.
/// Uses an atomic tag where:
/// - 0 = unlocked
/// - cur_thread pointer = locked by that thread
/// - cur_thread | 1 = locked with waiters
pub struct KLightLock {
    m_tag: AtomicUsize,
    /// Opaque kernel handle until KernelCore is ported.
    // TODO: Replace with reference to KernelCore.
    m_kernel: usize,
}

impl KLightLock {
    pub fn new(kernel: usize) -> Self {
        Self {
            m_tag: AtomicUsize::new(0),
            m_kernel: kernel,
        }
    }

    /// Lock the light lock.
    /// Mirrors upstream `KLightLock::Lock()`.
    ///
    /// Fast path: CAS from 0 to cur_thread.
    /// Slow path: requires KThread scheduler integration (stubbed).
    pub fn lock(&self) {
        // TODO: Get actual current thread pointer from KernelCore.
        // For now, use a thread-local unique ID as a stand-in.
        let cur_thread = current_thread_id();

        loop {
            let mut old_tag = self.m_tag.load(Ordering::Relaxed);

            loop {
                let new_tag = if old_tag == 0 {
                    cur_thread
                } else {
                    old_tag | 1
                };

                match self.m_tag.compare_exchange_weak(
                    old_tag,
                    new_tag,
                    Ordering::Acquire,
                    Ordering::Relaxed,
                ) {
                    Ok(_) => break,
                    Err(actual) => old_tag = actual,
                }
            }

            if old_tag == 0 || self.lock_slow_path(old_tag | 1, cur_thread) {
                break;
            }
        }
    }

    /// Unlock the light lock.
    /// Mirrors upstream `KLightLock::Unlock()`.
    pub fn unlock(&self) {
        let cur_thread = current_thread_id();

        let mut expected = cur_thread;
        match self.m_tag.compare_exchange(expected, 0, Ordering::Release, Ordering::Relaxed) {
            Ok(_) => {}
            Err(_) => {
                self.unlock_slow_path(cur_thread);
            }
        }
    }

    /// Slow path for locking — pends current thread on the owner.
    /// Mirrors upstream `KLightLock::LockSlowPath(...)`.
    ///
    /// TODO: Requires KThread, KScopedSchedulerLock, KThreadQueue.
    pub fn lock_slow_path(&self, _owner: usize, _cur_thread: usize) -> bool {
        // TODO: Implement once KThread and KScheduler are ported.
        // For now, spin-wait as a temporary fallback.
        loop {
            let tag = self.m_tag.load(Ordering::Relaxed);
            if tag == 0 {
                return false; // Retry the fast path
            }
            std::hint::spin_loop();
        }
    }

    /// Slow path for unlocking — passes lock to next waiter.
    /// Mirrors upstream `KLightLock::UnlockSlowPath(...)`.
    ///
    /// TODO: Requires KThread, KScopedSchedulerLock.
    pub fn unlock_slow_path(&self, _cur_thread: usize) {
        // TODO: Implement once KThread and KScheduler are ported.
        // For now, just force-unlock.
        self.m_tag.store(0, Ordering::Release);
    }

    /// Check if the lock is currently held.
    /// Mirrors upstream `KLightLock::IsLocked()`.
    pub fn is_locked(&self) -> bool {
        self.m_tag.load(Ordering::Relaxed) != 0
    }

    /// Check if the lock is held by the current thread.
    /// Mirrors upstream `KLightLock::IsLockedByCurrentThread()`.
    pub fn is_locked_by_current_thread(&self) -> bool {
        let cur_thread = current_thread_id();
        (self.m_tag.load(Ordering::Relaxed) | 1) == (cur_thread | 1)
    }
}

/// Get a unique ID for the current thread.
/// This is a temporary stand-in for the actual KThread pointer.
fn current_thread_id() -> usize {
    // Use thread-local storage to assign a unique non-zero ID to each thread.
    thread_local! {
        static THREAD_ID: usize = {
            use std::sync::atomic::AtomicUsize;
            static NEXT_ID: AtomicUsize = AtomicUsize::new(2); // Start at 2 to avoid 0 and 1
            NEXT_ID.fetch_add(2, Ordering::Relaxed) // Even numbers, so | 1 gives odd
        };
    }
    THREAD_ID.with(|id| *id)
}

/// KScopedLightLock — RAII wrapper for KLightLock.
/// Mirrors upstream `using KScopedLightLock = KScopedLock<KLightLock>`.
pub struct KScopedLightLock<'a> {
    lock: &'a KLightLock,
}

impl<'a> KScopedLightLock<'a> {
    pub fn new(lock: &'a KLightLock) -> Self {
        lock.lock();
        Self { lock }
    }
}

impl Drop for KScopedLightLock<'_> {
    fn drop(&mut self) {
        self.lock.unlock();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_light_lock_basic() {
        let lock = KLightLock::new(0);
        assert!(!lock.is_locked());
        lock.lock();
        assert!(lock.is_locked());
        assert!(lock.is_locked_by_current_thread());
        lock.unlock();
        assert!(!lock.is_locked());
    }

    #[test]
    fn test_scoped_light_lock() {
        let lock = KLightLock::new(0);
        {
            let _guard = KScopedLightLock::new(&lock);
            assert!(lock.is_locked());
        }
        assert!(!lock.is_locked());
    }
}
