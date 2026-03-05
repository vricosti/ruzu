//! Port of zuyu/src/common/threadsafe_queue.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

use std::collections::VecDeque;
use std::sync::{Arc, Condvar, Mutex};

/// Single-producer single-consumer queue with condition variable notification.
///
/// Mirrors the C++ `Common::SPSCQueue<T>`. Uses a mutex + condvar for blocking
/// waits (the C++ version uses a lock-free linked list for the data path but
/// still needs a mutex/condvar for `Wait`/`PopWait`). This Rust version uses a
/// `VecDeque` under a `Mutex` for simplicity and safety, which is functionally
/// equivalent.
pub struct SPSCQueue<T> {
    inner: Mutex<VecDeque<T>>,
    condvar: Condvar,
}

impl<T> Default for SPSCQueue<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> SPSCQueue<T> {
    pub fn new() -> Self {
        Self {
            inner: Mutex::new(VecDeque::new()),
            condvar: Condvar::new(),
        }
    }

    pub fn size(&self) -> usize {
        self.inner.lock().unwrap().len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.lock().unwrap().is_empty()
    }

    /// Push a value and notify one waiter.
    pub fn push(&self, value: T) {
        {
            let mut queue = self.inner.lock().unwrap();
            queue.push_back(value);
        }
        self.condvar.notify_one();
    }

    /// Try to pop a value. Returns `None` if the queue is empty.
    pub fn pop(&self) -> Option<T> {
        let mut queue = self.inner.lock().unwrap();
        queue.pop_front()
    }

    /// Peek at the front element without removing it.
    pub fn front(&self) -> Option<T>
    where
        T: Clone,
    {
        let queue = self.inner.lock().unwrap();
        queue.front().cloned()
    }

    /// Block until the queue is non-empty, then pop the front element.
    pub fn pop_wait(&self) -> T {
        let mut queue = self.inner.lock().unwrap();
        while queue.is_empty() {
            queue = self.condvar.wait(queue).unwrap();
        }
        queue.pop_front().unwrap()
    }

    /// Block until the queue is non-empty or `should_stop` returns true.
    /// Returns `None` if stopped before an element was available.
    pub fn pop_wait_with_stop<F>(&self, should_stop: F) -> Option<T>
    where
        F: Fn() -> bool,
    {
        let mut queue = self.inner.lock().unwrap();
        while queue.is_empty() {
            if should_stop() {
                return None;
            }
            queue = self.condvar.wait(queue).unwrap();
        }
        queue.pop_front()
    }

    /// Clear the queue (not thread-safe with concurrent readers in C++,
    /// but here the mutex makes it safe).
    pub fn clear(&self) {
        let mut queue = self.inner.lock().unwrap();
        queue.clear();
    }
}

/// Multi-producer single-consumer queue.
///
/// Mirrors the C++ `Common::MPSCQueue<T>`. Wraps an `SPSCQueue` with an
/// additional write lock. In Rust, our `SPSCQueue` already uses a `Mutex`,
/// so `MPSCQueue` is essentially a type alias with the same interface,
/// confirming the intended MPSC usage pattern.
pub struct MPSCQueue<T> {
    inner: SPSCQueue<T>,
}

impl<T> Default for MPSCQueue<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> MPSCQueue<T> {
    pub fn new() -> Self {
        Self {
            inner: SPSCQueue::new(),
        }
    }

    pub fn size(&self) -> usize {
        self.inner.size()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn push(&self, value: T) {
        self.inner.push(value);
    }

    pub fn pop(&self) -> Option<T> {
        self.inner.pop()
    }

    pub fn front(&self) -> Option<T>
    where
        T: Clone,
    {
        self.inner.front()
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

    pub fn clear(&self) {
        self.inner.clear();
    }
}

/// Thread-safe wrapper that can be shared across threads.
impl<T: Send> SPSCQueue<T> {
    /// Wrap this queue in an `Arc` for sharing between threads.
    pub fn into_shared(self) -> Arc<Self> {
        Arc::new(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_pop() {
        let q = SPSCQueue::new();
        q.push(1);
        q.push(2);
        assert_eq!(q.size(), 2);
        assert_eq!(q.pop(), Some(1));
        assert_eq!(q.pop(), Some(2));
        assert_eq!(q.pop(), None);
    }

    #[test]
    fn test_pop_wait() {
        let q = Arc::new(SPSCQueue::new());
        let q2 = q.clone();

        let handle = std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_millis(10));
            q2.push(42);
        });

        let val = q.pop_wait();
        assert_eq!(val, 42);
        handle.join().unwrap();
    }

    #[test]
    fn test_mpsc() {
        let q = Arc::new(MPSCQueue::new());
        let q1 = q.clone();
        let q2 = q.clone();

        let h1 = std::thread::spawn(move || q1.push(1));
        let h2 = std::thread::spawn(move || q2.push(2));

        h1.join().unwrap();
        h2.join().unwrap();
        assert_eq!(q.size(), 2);
    }
}
