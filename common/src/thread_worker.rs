// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/common/thread_worker.h
//!
//! Provides `StatefulThreadWorker<S>` -- a thread pool where each worker owns
//! a piece of per-thread state `S`, and `ThreadWorker` (the stateless variant).
//!
//! Tasks are queued and distributed to worker threads. `wait_for_requests`
//! blocks until all queued work has been processed.

use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::thread::{self, JoinHandle};

/// Internal shared state between the pool and its workers.
struct SharedState<S: Send + 'static> {
    queue: Mutex<VecDeque<Box<dyn FnOnce(&mut S) + Send>>>,
    /// Signalled when a new task is available or stop is requested.
    condition: Condvar,
    /// Signalled when the queue becomes empty or a worker stops.
    wait_condition: Condvar,
    work_scheduled: AtomicUsize,
    work_done: AtomicUsize,
    workers_stopped: AtomicUsize,
    workers_queued: usize,
    stop: AtomicBool,
}

/// A thread pool where each worker thread owns per-thread state of type `S`.
///
/// Tasks are closures `FnOnce(&mut S)` that receive a mutable reference to
/// the worker's state.
///
/// The stateless variant `ThreadWorker` uses `S = ()`.
pub struct StatefulThreadWorker<S: Send + 'static> {
    shared: Arc<SharedState<S>>,
    threads: Vec<JoinHandle<()>>,
}

impl<S: Send + 'static> StatefulThreadWorker<S> {
    /// Create a new pool with `num_workers` threads. Each worker's state is
    /// created by calling `state_maker()`.
    pub fn new<F>(num_workers: usize, name: String, state_maker: F) -> Self
    where
        F: Fn() -> S + Send + Clone + 'static,
    {
        let shared = Arc::new(SharedState {
            queue: Mutex::new(VecDeque::new()),
            condition: Condvar::new(),
            wait_condition: Condvar::new(),
            work_scheduled: AtomicUsize::new(0),
            work_done: AtomicUsize::new(0),
            workers_stopped: AtomicUsize::new(0),
            workers_queued: num_workers,
            stop: AtomicBool::new(false),
        });

        let mut threads = Vec::with_capacity(num_workers);
        for _ in 0..num_workers {
            let shared_clone = shared.clone();
            let name_clone = name.clone();
            let maker = state_maker.clone();
            threads.push(thread::Builder::new().name(name_clone).spawn(move || {
                let mut state = maker();
                loop {
                    let task;
                    {
                        let mut queue = shared_clone.queue.lock().unwrap();
                        if queue.is_empty() {
                            shared_clone.wait_condition.notify_all();
                        }
                        queue = shared_clone
                            .condition
                            .wait_while(queue, |q| {
                                q.is_empty() && !shared_clone.stop.load(Ordering::Acquire)
                            })
                            .unwrap();
                        if shared_clone.stop.load(Ordering::Acquire) {
                            break;
                        }
                        task = queue.pop_front().unwrap();
                    }
                    task(&mut state);
                    shared_clone.work_done.fetch_add(1, Ordering::Release);
                }
                shared_clone.workers_stopped.fetch_add(1, Ordering::Release);
                shared_clone.wait_condition.notify_all();
            }).expect("failed to spawn worker thread"));
        }

        Self { shared, threads }
    }

    /// Queue a task for execution by one of the worker threads.
    pub fn queue_work<F>(&self, work: F)
    where
        F: FnOnce(&mut S) + Send + 'static,
    {
        {
            let mut queue = self.shared.queue.lock().unwrap();
            queue.push_back(Box::new(work));
            self.shared.work_scheduled.fetch_add(1, Ordering::Release);
        }
        self.shared.condition.notify_one();
    }

    /// Block until all queued work has been completed or all workers have
    /// stopped.
    pub fn wait_for_requests(&self) {
        let queue = self.shared.queue.lock().unwrap();
        let _guard = self
            .shared
            .wait_condition
            .wait_while(queue, |_| {
                let stopped = self.shared.workers_stopped.load(Ordering::Acquire);
                let done = self.shared.work_done.load(Ordering::Acquire);
                let scheduled = self.shared.work_scheduled.load(Ordering::Acquire);
                stopped < self.shared.workers_queued && done < scheduled
            })
            .unwrap();
    }
}

impl<S: Send + 'static> Drop for StatefulThreadWorker<S> {
    fn drop(&mut self) {
        self.shared.stop.store(true, Ordering::Release);
        self.shared.condition.notify_all();
        for handle in self.threads.drain(..) {
            let _ = handle.join();
        }
    }
}

/// Stateless thread worker -- equivalent to upstream `ThreadWorker`.
pub type ThreadWorker = StatefulThreadWorker<()>;

impl ThreadWorker {
    /// Create a stateless thread worker pool.
    pub fn new_stateless(num_workers: usize, name: String) -> Self {
        Self::new(num_workers, name, || ())
    }

    /// Queue a stateless task.
    pub fn queue_stateless_work<F>(&self, work: F)
    where
        F: FnOnce() + Send + 'static,
    {
        self.queue_work(move |_: &mut ()| work());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicU32;

    #[test]
    fn test_stateless_worker() {
        let counter = Arc::new(AtomicU32::new(0));
        let worker = ThreadWorker::new_stateless(2, "test-worker".to_string());

        for _ in 0..100 {
            let c = counter.clone();
            worker.queue_stateless_work(move || {
                c.fetch_add(1, Ordering::SeqCst);
            });
        }

        worker.wait_for_requests();
        assert_eq!(counter.load(Ordering::SeqCst), 100);
    }

    #[test]
    fn test_stateful_worker() {
        // Each worker accumulates a local sum.
        let worker = StatefulThreadWorker::new(1, "stateful-test".to_string(), || 0u64);

        let result = Arc::new(AtomicU64::new(0));
        use std::sync::atomic::AtomicU64;

        for i in 0..10u64 {
            let r = result.clone();
            worker.queue_work(move |state: &mut u64| {
                *state += i;
                r.store(*state, Ordering::SeqCst);
            });
        }

        worker.wait_for_requests();
        // Sum of 0..10 = 45
        assert_eq!(result.load(Ordering::SeqCst), 45);
    }
}
