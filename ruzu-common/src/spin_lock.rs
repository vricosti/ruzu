//! Port of zuyu/src/common/spin_lock.h and zuyu/src/common/spin_lock.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

use std::sync::atomic::{AtomicBool, Ordering};

/// SpinLock class
/// A lock similar to mutex that forces a thread to spin wait instead of calling the
/// supervisor. Should be used on short sequences of code.
pub struct SpinLock {
    locked: AtomicBool,
}

impl SpinLock {
    pub const fn new() -> Self {
        Self {
            locked: AtomicBool::new(false),
        }
    }

    pub fn lock(&self) {
        while self
            .locked
            .compare_exchange_weak(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            // Spin with a hint
            thread_pause();
        }
    }

    pub fn unlock(&self) {
        self.locked.store(false, Ordering::Release);
    }

    pub fn try_lock(&self) -> bool {
        self.locked
            .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
    }
}

impl Default for SpinLock {
    fn default() -> Self {
        Self::new()
    }
}

// Safety: SpinLock is designed to be shared between threads
unsafe impl Send for SpinLock {}
unsafe impl Sync for SpinLock {}

#[inline]
fn thread_pause() {
    std::hint::spin_loop();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lock_unlock() {
        let lock = SpinLock::new();
        lock.lock();
        lock.unlock();
    }

    #[test]
    fn test_try_lock() {
        let lock = SpinLock::new();
        assert!(lock.try_lock());
        assert!(!lock.try_lock());
        lock.unlock();
        assert!(lock.try_lock());
        lock.unlock();
    }
}
