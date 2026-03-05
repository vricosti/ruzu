//! Port of zuyu/src/common/bounded_threadsafe_queue.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

use std::collections::VecDeque;
use std::sync::{Condvar, Mutex};

/// Default capacity matching the C++ `detail::DefaultCapacity`.
pub const DEFAULT_CAPACITY: usize = 0x1000;

/// Bounded SPSC queue with blocking operations.
///
/// Mirrors the C++ `Common::SPSCQueue<T, Capacity>` from
/// `bounded_threadsafe_queue.h`. Uses separate mutexes + condvars for the
/// producer and consumer sides to minimize contention, matching the C++
/// design. The capacity is fixed at construction time.
pub struct BoundedSPSCQueue<T> {
    capacity: usize,
    data: Mutex<VecDeque<T>>,
    producer_cv: Condvar,
    consumer_cv: Condvar,
}

impl<T> BoundedSPSCQueue<T> {
    /// Create a new bounded SPSC queue with the given capacity.
    pub fn new(capacity: usize) -> Self {
        assert!(
            capacity > 0 && (capacity & (capacity - 1)) == 0,
            "Capacity must be a power of two."
        );
        Self {
            capacity,
            data: Mutex::new(VecDeque::with_capacity(capacity)),
            producer_cv: Condvar::new(),
            consumer_cv: Condvar::new(),
        }
    }

    /// Create with the default capacity (0x1000).
    pub fn with_default_capacity() -> Self {
        Self::new(DEFAULT_CAPACITY)
    }

    /// Try to emplace (push) a value. Returns `false` if the queue is full.
    pub fn try_emplace(&self, value: T) -> bool {
        let mut data = self.data.lock().unwrap();
        if data.len() >= self.capacity {
            return false;
        }
        data.push_back(value);
        drop(data);
        self.consumer_cv.notify_one();
        true
    }

    /// Push a value, blocking until space is available.
    pub fn emplace_wait(&self, value: T) {
        let mut data = self.data.lock().unwrap();
        while data.len() >= self.capacity {
            data = self.producer_cv.wait(data).unwrap();
        }
        data.push_back(value);
        drop(data);
        self.consumer_cv.notify_one();
    }

    /// Try to pop a value. Returns `None` if the queue is empty.
    pub fn try_pop(&self) -> Option<T> {
        let mut data = self.data.lock().unwrap();
        let result = data.pop_front();
        if result.is_some() {
            drop(data);
            self.producer_cv.notify_one();
        }
        result
    }

    /// Pop a value, blocking until one is available.
    pub fn pop_wait(&self) -> T {
        let mut data = self.data.lock().unwrap();
        while data.is_empty() {
            data = self.consumer_cv.wait(data).unwrap();
        }
        let value = data.pop_front().unwrap();
        drop(data);
        self.producer_cv.notify_one();
        value
    }

    /// Pop a value, blocking until one is available or `should_stop` returns true.
    pub fn pop_wait_with_stop<F>(&self, should_stop: F) -> Option<T>
    where
        F: Fn() -> bool,
    {
        let mut data = self.data.lock().unwrap();
        while data.is_empty() {
            if should_stop() {
                return None;
            }
            data = self.consumer_cv.wait(data).unwrap();
        }
        let value = data.pop_front().unwrap();
        drop(data);
        self.producer_cv.notify_one();
        Some(value)
    }
}

/// Bounded MPSC queue (multiple producers, single consumer).
///
/// Mirrors the C++ `Common::MPSCQueue<T, Capacity>`.
pub struct BoundedMPSCQueue<T> {
    inner: BoundedSPSCQueue<T>,
    write_mutex: Mutex<()>,
}

impl<T> BoundedMPSCQueue<T> {
    pub fn new(capacity: usize) -> Self {
        Self {
            inner: BoundedSPSCQueue::new(capacity),
            write_mutex: Mutex::new(()),
        }
    }

    pub fn with_default_capacity() -> Self {
        Self::new(DEFAULT_CAPACITY)
    }

    pub fn try_emplace(&self, value: T) -> bool {
        let _lock = self.write_mutex.lock().unwrap();
        self.inner.try_emplace(value)
    }

    pub fn emplace_wait(&self, value: T) {
        let _lock = self.write_mutex.lock().unwrap();
        self.inner.emplace_wait(value);
    }

    pub fn try_pop(&self) -> Option<T> {
        self.inner.try_pop()
    }

    pub fn pop_wait(&self) -> T {
        self.inner.pop_wait()
    }

    pub fn pop_wait_with_stop<F>(&self, should_stop: F) -> Option<T>
    where
        F: Fn() -> bool,
    {
        self.inner.pop_wait_with_stop(should_stop)
    }
}

/// Bounded MPMC queue (multiple producers, multiple consumers).
///
/// Mirrors the C++ `Common::MPMCQueue<T, Capacity>`.
pub struct BoundedMPMCQueue<T> {
    inner: BoundedSPSCQueue<T>,
    write_mutex: Mutex<()>,
    read_mutex: Mutex<()>,
}

impl<T> BoundedMPMCQueue<T> {
    pub fn new(capacity: usize) -> Self {
        Self {
            inner: BoundedSPSCQueue::new(capacity),
            write_mutex: Mutex::new(()),
            read_mutex: Mutex::new(()),
        }
    }

    pub fn with_default_capacity() -> Self {
        Self::new(DEFAULT_CAPACITY)
    }

    pub fn try_emplace(&self, value: T) -> bool {
        let _lock = self.write_mutex.lock().unwrap();
        self.inner.try_emplace(value)
    }

    pub fn emplace_wait(&self, value: T) {
        let _lock = self.write_mutex.lock().unwrap();
        self.inner.emplace_wait(value);
    }

    pub fn try_pop(&self) -> Option<T> {
        let _lock = self.read_mutex.lock().unwrap();
        self.inner.try_pop()
    }

    pub fn pop_wait(&self) -> T {
        let _lock = self.read_mutex.lock().unwrap();
        self.inner.pop_wait()
    }

    pub fn pop_wait_with_stop<F>(&self, should_stop: F) -> Option<T>
    where
        F: Fn() -> bool,
    {
        let _lock = self.read_mutex.lock().unwrap();
        self.inner.pop_wait_with_stop(should_stop)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    #[test]
    fn test_try_emplace_pop() {
        let q = BoundedSPSCQueue::new(4);
        assert!(q.try_emplace(1));
        assert!(q.try_emplace(2));
        assert!(q.try_emplace(3));
        assert!(q.try_emplace(4));
        assert!(!q.try_emplace(5)); // full

        assert_eq!(q.try_pop(), Some(1));
        assert_eq!(q.try_pop(), Some(2));
        assert!(q.try_emplace(5)); // space freed
    }

    #[test]
    fn test_blocking_pop() {
        let q = Arc::new(BoundedSPSCQueue::new(4));
        let q2 = q.clone();

        let handle = std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_millis(10));
            q2.try_emplace(42);
        });

        let val = q.pop_wait();
        assert_eq!(val, 42);
        handle.join().unwrap();
    }

    #[test]
    fn test_mpsc() {
        let q = Arc::new(BoundedMPSCQueue::new(16));
        let q1 = q.clone();
        let q2 = q.clone();

        let h1 = std::thread::spawn(move || {
            for i in 0..4 {
                q1.emplace_wait(i);
            }
        });
        let h2 = std::thread::spawn(move || {
            for i in 10..14 {
                q2.emplace_wait(i);
            }
        });

        h1.join().unwrap();
        h2.join().unwrap();

        let mut values = Vec::new();
        while let Some(v) = q.try_pop() {
            values.push(v);
        }
        assert_eq!(values.len(), 8);
    }
}
