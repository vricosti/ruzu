// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/textures/workers.h` and `workers.cpp`.
//!
//! Provides a shared thread worker pool for texture transcoding operations
//! (ASTC decompression, BCN compression, etc.).

use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex, OnceLock};

// ── Thread Worker Pool ───────────────────────────────────────────────────────

/// Work item for the thread pool.
type WorkItem = Box<dyn FnOnce() + Send>;

/// A simple thread worker pool for texture transcoding.
///
/// Port of `Common::ThreadWorker` usage in `workers.cpp`.
///
/// The upstream implementation returns a static `Common::ThreadWorker` with
/// `max(hardware_concurrency, 2) / 2` threads named "ImageTranscode".
pub struct ThreadWorker {
    num_threads: usize,
    /// Work queue shared between producer and worker threads.
    queue: Arc<Mutex<Vec<WorkItem>>>,
    /// Condition variable to wake worker threads.
    condvar: Arc<Condvar>,
    /// Number of work items currently being processed.
    active_count: Arc<AtomicUsize>,
    /// Condition variable for waiting until all work is done.
    done_condvar: Arc<Condvar>,
    /// Stop flag for worker threads.
    stop_flag: Arc<AtomicBool>,
    /// Worker thread handles.
    threads: Vec<std::thread::JoinHandle<()>>,
}

impl ThreadWorker {
    /// Create a new worker pool with the specified number of threads.
    pub fn new(num_threads: usize) -> Self {
        let queue = Arc::new(Mutex::new(Vec::<WorkItem>::new()));
        let condvar = Arc::new(Condvar::new());
        let active_count = Arc::new(AtomicUsize::new(0));
        let done_condvar = Arc::new(Condvar::new());
        let stop_flag = Arc::new(AtomicBool::new(false));

        let mut threads = Vec::with_capacity(num_threads);
        for i in 0..num_threads {
            let queue = Arc::clone(&queue);
            let condvar = Arc::clone(&condvar);
            let active_count = Arc::clone(&active_count);
            let done_condvar = Arc::clone(&done_condvar);
            let stop_flag = Arc::clone(&stop_flag);

            let handle = std::thread::Builder::new()
                .name(format!("ImageTranscode:{}", i))
                .spawn(move || loop {
                    let work = {
                        let mut locked = queue.lock().unwrap();
                        loop {
                            if stop_flag.load(Ordering::Relaxed) {
                                return;
                            }
                            if let Some(item) = locked.pop() {
                                break Some(item);
                            }
                            locked = condvar.wait(locked).unwrap();
                        }
                    };

                    if let Some(work) = work {
                        active_count.fetch_add(1, Ordering::Release);
                        work();
                        active_count.fetch_sub(1, Ordering::Release);
                        done_condvar.notify_all();
                    }
                })
                .expect("Failed to spawn ImageTranscode thread");
            threads.push(handle);
        }

        Self {
            num_threads,
            queue,
            condvar,
            active_count,
            done_condvar,
            stop_flag,
            threads,
        }
    }

    /// Number of worker threads in the pool.
    pub fn num_threads(&self) -> usize {
        self.num_threads
    }

    /// Queue work to be executed by the thread pool.
    ///
    /// Port of `Common::ThreadWorker::QueueWork`.
    pub fn queue_work<F: FnOnce() + Send + 'static>(&self, work: F) {
        let mut locked = self.queue.lock().unwrap();
        locked.push(Box::new(work));
        self.condvar.notify_one();
    }

    /// Wait for all queued work to complete.
    ///
    /// Port of `Common::ThreadWorker::WaitForRequests`.
    pub fn wait_for_requests(&self) {
        let locked = self.queue.lock().unwrap();
        let _guard = self
            .done_condvar
            .wait_while(locked, |queue| {
                !queue.is_empty() || self.active_count.load(Ordering::Acquire) > 0
            })
            .unwrap();
    }
}

impl Drop for ThreadWorker {
    fn drop(&mut self) {
        self.stop_flag.store(true, Ordering::Release);
        self.condvar.notify_all();
        for handle in self.threads.drain(..) {
            handle.join().ok();
        }
    }
}

/// Global singleton thread worker pool for texture transcoding.
///
/// Port of the static `Common::ThreadWorker` in `GetThreadWorkers()`.
static THREAD_WORKERS: OnceLock<ThreadWorker> = OnceLock::new();

/// Returns a reference to the shared texture transcoding thread worker pool.
///
/// Port of `Tegra::Texture::GetThreadWorkers()`.
///
/// The pool is lazily initialized with `max(hardware_concurrency, 2) / 2` threads.
pub fn get_thread_workers() -> &'static ThreadWorker {
    THREAD_WORKERS.get_or_init(|| {
        let num_threads = std::cmp::max(
            std::thread::available_parallelism()
                .map(|n| n.get())
                .unwrap_or(2),
            2,
        ) / 2;
        ThreadWorker::new(num_threads)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicU32;

    #[test]
    fn worker_pool_initializes() {
        let workers = get_thread_workers();
        assert!(workers.num_threads() >= 1);
    }

    #[test]
    fn worker_executes_work() {
        let worker = ThreadWorker::new(2);
        let counter = Arc::new(AtomicU32::new(0));

        for _ in 0..10 {
            let counter = Arc::clone(&counter);
            worker.queue_work(move || {
                counter.fetch_add(1, Ordering::Relaxed);
            });
        }

        worker.wait_for_requests();
        assert_eq!(counter.load(Ordering::Relaxed), 10);
    }
}
