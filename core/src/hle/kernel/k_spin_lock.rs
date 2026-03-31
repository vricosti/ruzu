//! Port of zuyu/src/core/hle/kernel/k_spin_lock.h and k_spin_lock.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-11
//!
//! KSpinLock — a simple spin lock wrapping a std::Mutex.
//! The upstream C++ implementation wraps std::mutex with Lock/Unlock/TryLock methods.
//! KAlignedSpinLock and KNotAlignedSpinLock are aliases (upstream: "Alias for now").

use parking_lot::{lock_api::RawMutex as RawMutexTrait, Mutex};

/// KSpinLock — wraps a mutex with Lock/Unlock/TryLock interface.
/// Mirrors upstream `Kernel::KSpinLock`.
pub struct KSpinLock {
    m_lock: Mutex<()>,
}

impl KSpinLock {
    pub fn new() -> Self {
        Self {
            m_lock: Mutex::new(()),
        }
    }

    /// Lock the spin lock. Mirrors upstream `KSpinLock::Lock()`.
    pub fn lock(&self) {
        // The upstream pattern uses paired Lock/Unlock calls.
        // We use the raw mutex interface from parking_lot to match this.
        unsafe {
            self.m_lock.raw().lock();
        }
    }

    /// Unlock the spin lock. Mirrors upstream `KSpinLock::Unlock()`.
    pub fn unlock(&self) {
        unsafe {
            self.m_lock.raw().unlock();
        }
    }

    /// Try to lock. Returns true if the lock was acquired.
    /// Mirrors upstream `KSpinLock::TryLock()`.
    pub fn try_lock(&self) -> bool {
        unsafe { self.m_lock.raw() }.try_lock()
    }
}

impl Default for KSpinLock {
    fn default() -> Self {
        Self::new()
    }
}

// Aliases matching upstream.
// Upstream has cache-line aligned and non-aligned variants; aliased here.
pub type KAlignedSpinLock = KSpinLock;
pub type KNotAlignedSpinLock = KSpinLock;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_spin_lock_lock_unlock() {
        let lock = KSpinLock::new();
        lock.lock();
        lock.unlock();
    }

    #[test]
    fn test_spin_lock_try_lock() {
        let lock = KSpinLock::new();
        assert!(lock.try_lock());
        // Lock is held — try_lock should fail.
        assert!(!lock.try_lock());
        lock.unlock();
        // Now it should succeed again.
        assert!(lock.try_lock());
        lock.unlock();
    }
}
