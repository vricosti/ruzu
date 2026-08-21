// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GPU syncpoint manager.
//!
//! The Tegra X1 GPU uses syncpoints for CPU–GPU synchronization. Each syncpoint
//! is a monotonically increasing 32-bit counter. The GPU increments syncpoints
//! via commands in the pushbuffer, and the CPU can block until a syncpoint
//! reaches a given threshold.

use std::sync::atomic::{AtomicU32, Ordering};
use std::time::{Duration, Instant};

use parking_lot::Mutex;

/// Number of hardware syncpoints on Tegra X1.
pub const NUM_SYNCPOINTS: usize = 192;

/// A waiter blocked on a syncpoint reaching a threshold.
struct Waiter {
    threshold: u32,
    /// Signaled when the syncpoint reaches the threshold.
    condvar: std::sync::Arc<(std::sync::Mutex<bool>, std::sync::Condvar)>,
}

/// Manages GPU syncpoints for CPU–GPU synchronization.
pub struct SyncpointManager {
    values: Box<[AtomicU32; NUM_SYNCPOINTS]>,
    waiters: Mutex<Vec<Vec<Waiter>>>,
}

impl SyncpointManager {
    pub fn new() -> Self {
        // Initialize all syncpoints to 0.
        let values: Box<[AtomicU32; NUM_SYNCPOINTS]> = {
            let mut v: Vec<AtomicU32> = Vec::with_capacity(NUM_SYNCPOINTS);
            for _ in 0..NUM_SYNCPOINTS {
                v.push(AtomicU32::new(0));
            }
            v.try_into().unwrap_or_else(|_| unreachable!())
        };

        let mut waiters_vec = Vec::with_capacity(NUM_SYNCPOINTS);
        for _ in 0..NUM_SYNCPOINTS {
            waiters_vec.push(Vec::new());
        }

        Self {
            values,
            waiters: Mutex::new(waiters_vec),
        }
    }

    /// Atomically increment a syncpoint and wake any waiters whose threshold is met.
    /// Returns the new value.
    pub fn increment(&self, id: u32) -> u32 {
        let idx = id as usize;
        if idx >= NUM_SYNCPOINTS {
            log::warn!("syncpoint: increment out of range id={}", id);
            return 0;
        }

        let new_val = self.values[idx].fetch_add(1, Ordering::Release) + 1;
        log::trace!("syncpoint[{}] incremented to {}", id, new_val);

        // Wake waiters whose threshold has been reached.
        let mut waiters = self.waiters.lock();
        waiters[idx].retain(|w| {
            if syncpoint_reached(new_val, w.threshold) {
                let (lock, cvar) = &*w.condvar;
                let mut signaled = lock.lock().unwrap();
                *signaled = true;
                cvar.notify_one();
                false // remove from waiters
            } else {
                true // keep waiting
            }
        });

        new_val
    }

    /// Read the current value of a syncpoint.
    pub fn get_value(&self, id: u32) -> u32 {
        let idx = id as usize;
        if idx >= NUM_SYNCPOINTS {
            return 0;
        }
        self.values[idx].load(Ordering::Acquire)
    }

    /// Block until a syncpoint reaches `threshold` or `timeout` expires.
    /// Returns `true` if the threshold was reached, `false` on timeout.
    pub fn wait(&self, id: u32, threshold: u32, timeout: Duration) -> bool {
        let idx = id as usize;
        if idx >= NUM_SYNCPOINTS {
            return false;
        }

        // Fast path: already reached.
        let current = self.values[idx].load(Ordering::Acquire);
        if syncpoint_reached(current, threshold) {
            return true;
        }

        // Register a waiter.
        let pair = std::sync::Arc::new((std::sync::Mutex::new(false), std::sync::Condvar::new()));
        {
            let mut waiters = self.waiters.lock();
            waiters[idx].push(Waiter {
                threshold,
                condvar: pair.clone(),
            });
        }

        // Block until signaled or timeout.
        let (lock, cvar) = &*pair;
        let mut signaled = lock.lock().unwrap();
        let deadline = Instant::now() + timeout;

        while !*signaled {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                // Timeout — remove our waiter.
                let mut waiters = self.waiters.lock();
                waiters[idx].retain(|w| !std::sync::Arc::ptr_eq(&w.condvar, &pair));
                return false;
            }
            let result = cvar.wait_timeout(signaled, remaining).unwrap();
            signaled = result.0;
        }

        true
    }
}

impl Default for SyncpointManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Check if a syncpoint value has reached a threshold, handling wrap-around.
/// Uses signed comparison: `(value - threshold) as i32 >= 0`.
fn syncpoint_reached(value: u32, threshold: u32) -> bool {
    (value.wrapping_sub(threshold) as i32) >= 0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_increment() {
        let mgr = SyncpointManager::new();
        assert_eq!(mgr.get_value(0), 0);
        assert_eq!(mgr.increment(0), 1);
        assert_eq!(mgr.increment(0), 2);
        assert_eq!(mgr.get_value(0), 2);
    }

    #[test]
    fn test_get_value_initial() {
        let mgr = SyncpointManager::new();
        for i in 0..NUM_SYNCPOINTS as u32 {
            assert_eq!(mgr.get_value(i), 0);
        }
    }

    #[test]
    fn test_out_of_range() {
        let mgr = SyncpointManager::new();
        assert_eq!(mgr.get_value(NUM_SYNCPOINTS as u32), 0);
        assert_eq!(mgr.increment(NUM_SYNCPOINTS as u32), 0);
    }

    #[test]
    fn test_wait_already_reached() {
        let mgr = SyncpointManager::new();
        mgr.increment(5);
        mgr.increment(5);
        // Threshold 1 is already reached (current = 2).
        assert!(mgr.wait(5, 1, Duration::from_millis(10)));
    }

    #[test]
    fn test_wait_timeout() {
        let mgr = SyncpointManager::new();
        // Threshold 10 not reached, should timeout.
        let start = Instant::now();
        assert!(!mgr.wait(3, 10, Duration::from_millis(50)));
        assert!(start.elapsed() >= Duration::from_millis(40));
    }

    #[test]
    fn test_wait_then_increment() {
        use std::sync::Arc;
        use std::thread;

        let mgr = Arc::new(SyncpointManager::new());
        let mgr2 = mgr.clone();

        let handle = thread::spawn(move || {
            // Wait for syncpoint 1 to reach threshold 3.
            mgr2.wait(1, 3, Duration::from_secs(2))
        });

        // Give the waiter time to register.
        thread::sleep(Duration::from_millis(20));

        mgr.increment(1); // 1
        mgr.increment(1); // 2
        mgr.increment(1); // 3 → should wake

        assert!(handle.join().unwrap());
        assert_eq!(mgr.get_value(1), 3);
    }

    #[test]
    fn test_syncpoint_reached_wraparound() {
        // Normal case
        assert!(syncpoint_reached(5, 3));
        assert!(syncpoint_reached(3, 3));
        assert!(!syncpoint_reached(2, 3));

        // Wrap-around: value just wrapped past threshold
        assert!(syncpoint_reached(0, u32::MAX));
    }
}
