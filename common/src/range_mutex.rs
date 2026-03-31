// SPDX-FileCopyrightText: 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/common/range_mutex.h
//!
//! A range-based mutex that allows concurrent access to non-overlapping
//! address ranges. When a `ScopedRangeLock` is created it blocks until no
//! existing lock overlaps its range. When dropped it releases the range and
//! wakes waiters.

use std::sync::{Condvar, Mutex};

/// Internal record of a currently held range lock.
struct RangeEntry {
    address: u64,
    size: u64,
}

/// A mutex that allows multiple threads to lock non-overlapping address ranges
/// concurrently. Overlapping ranges will block until the conflict is resolved.
pub struct RangeMutex {
    inner: Mutex<Vec<RangeEntry>>,
    cv: Condvar,
}

impl RangeMutex {
    pub fn new() -> Self {
        Self {
            inner: Mutex::new(Vec::new()),
            cv: Condvar::new(),
        }
    }

    /// Lock the given address range. Blocks until no existing lock overlaps.
    fn lock(&self, address: u64, size: u64) {
        let mut list = self.inner.lock().unwrap();
        list = self
            .cv
            .wait_while(list, |list| Self::has_intersection(list, address, size))
            .unwrap();
        list.push(RangeEntry { address, size });
    }

    /// Unlock the given address range and notify waiters.
    fn unlock(&self, address: u64, size: u64) {
        {
            let mut list = self.inner.lock().unwrap();
            if let Some(pos) = list
                .iter()
                .position(|e| e.address == address && e.size == size)
            {
                list.swap_remove(pos);
            }
        }
        self.cv.notify_all();
    }

    /// Check if any entry in the list overlaps with [address, address+size).
    fn has_intersection(list: &[RangeEntry], address: u64, size: u64) -> bool {
        let cur_begin = address;
        let cur_last = address + size - 1;

        for other in list.iter() {
            let other_begin = other.address;
            let other_last = other.address + other.size - 1;

            if cur_begin <= other_last && other_begin <= cur_last {
                return true;
            }
        }
        false
    }
}

impl Default for RangeMutex {
    fn default() -> Self {
        Self::new()
    }
}

/// RAII guard that locks an address range on creation and unlocks on drop.
/// Mirrors the C++ `ScopedRangeLock`.
pub struct ScopedRangeLock<'a> {
    mutex: &'a RangeMutex,
    address: u64,
    size: u64,
}

impl<'a> ScopedRangeLock<'a> {
    /// Create a new scoped range lock. If `size > 0` the range is locked
    /// immediately, blocking until no overlap exists.
    pub fn new(mutex: &'a RangeMutex, address: u64, size: u64) -> Self {
        if size > 0 {
            mutex.lock(address, size);
        }
        Self {
            mutex,
            address,
            size,
        }
    }

    pub fn get_address(&self) -> u64 {
        self.address
    }

    pub fn get_size(&self) -> u64 {
        self.size
    }
}

impl<'a> Drop for ScopedRangeLock<'a> {
    fn drop(&mut self) {
        if self.size > 0 {
            self.mutex.unlock(self.address, self.size);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use std::thread;

    #[test]
    fn test_non_overlapping_ranges() {
        let mutex = Arc::new(RangeMutex::new());
        let m1 = mutex.clone();
        let m2 = mutex.clone();

        let t1 = thread::spawn(move || {
            let _lock = ScopedRangeLock::new(&m1, 0, 100);
            thread::sleep(std::time::Duration::from_millis(10));
        });
        let t2 = thread::spawn(move || {
            let _lock = ScopedRangeLock::new(&m2, 200, 100);
            thread::sleep(std::time::Duration::from_millis(10));
        });

        t1.join().unwrap();
        t2.join().unwrap();
    }

    #[test]
    fn test_zero_size_does_not_lock() {
        let mutex = RangeMutex::new();
        let _lock = ScopedRangeLock::new(&mutex, 0, 0);
        // Should not deadlock since size is 0
        let _lock2 = ScopedRangeLock::new(&mutex, 0, 0);
    }
}
