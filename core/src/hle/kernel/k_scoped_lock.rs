//! Port of zuyu/src/core/hle/kernel/k_scoped_lock.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-11
//!
//! KScopedLock — RAII scoped lock that calls Lock() on construction and Unlock() on drop.
//! Mirrors the C++ `KScopedLock<T>` template constrained by the `KLockable` concept.
//!
//! In Rust, the trait bound is expressed via the `KLockable` trait.
//! The scoped lock type aliases (KScopedSpinLock, etc.) are defined in
//! the modules that define the corresponding lock types.

use super::k_spin_lock::{KAlignedSpinLock, KNotAlignedSpinLock, KSpinLock};

/// Trait matching the C++ `KLockable` concept.
/// Requires Lock() and Unlock() methods.
pub trait KLockable {
    fn lock(&self);
    fn unlock(&self);
}

impl KLockable for KSpinLock {
    fn lock(&self) {
        KSpinLock::lock(self);
    }
    fn unlock(&self) {
        KSpinLock::unlock(self);
    }
}

/// KScopedLock — RAII guard that calls Lock on construction and Unlock on drop.
/// Mirrors `Kernel::KScopedLock<T>`.
pub struct KScopedLock<'a, T: KLockable> {
    m_lock: &'a T,
}

impl<'a, T: KLockable> KScopedLock<'a, T> {
    /// Create a new scoped lock, immediately locking the underlying lock.
    pub fn new(lock: &'a T) -> Self {
        lock.lock();
        Self { m_lock: lock }
    }
}

impl<T: KLockable> Drop for KScopedLock<'_, T> {
    fn drop(&mut self) {
        self.m_lock.unlock();
    }
}

/// Type aliases matching upstream.
pub type KScopedSpinLock<'a> = KScopedLock<'a, KSpinLock>;
pub type KScopedAlignedSpinLock<'a> = KScopedLock<'a, KAlignedSpinLock>;
pub type KScopedNotAlignedSpinLock<'a> = KScopedLock<'a, KNotAlignedSpinLock>;

#[cfg(test)]
mod tests {
    use super::super::k_spin_lock::KSpinLock;
    use super::*;

    #[test]
    fn test_scoped_lock() {
        let lock = KSpinLock::new();
        {
            let _guard = KScopedLock::new(&lock);
            // Lock is held here.
            assert!(!lock.try_lock());
        }
        // Lock is released after guard is dropped.
        assert!(lock.try_lock());
        lock.unlock();
    }
}
