// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/textures/workers.h` and `workers.cpp`.
//!
//! Provides a shared thread worker pool for texture transcoding operations
//! (ASTC decompression, BCN compression, etc.).

use std::sync::OnceLock;

// ── Thread Worker Pool ───────────────────────────────────────────────────────

/// A simple thread worker pool for texture transcoding.
///
/// Port of `Common::ThreadWorker` usage in `workers.cpp`.
///
/// The upstream implementation returns a static `Common::ThreadWorker` with
/// `max(hardware_concurrency, 2) / 2` threads named "ImageTranscode".
pub struct ThreadWorker {
    num_threads: usize,
    // TODO: Implement actual thread pool using std::thread or rayon
}

impl ThreadWorker {
    /// Create a new worker pool with the specified number of threads.
    pub fn new(num_threads: usize) -> Self {
        Self { num_threads }
    }

    /// Number of worker threads in the pool.
    pub fn num_threads(&self) -> usize {
        self.num_threads
    }

    /// Queue work to be executed by the thread pool.
    ///
    /// Port of `Common::ThreadWorker::QueueWork`.
    pub fn queue_work<F: FnOnce() + Send + 'static>(&self, _work: F) {
        // TODO: Dispatch to actual thread pool
        todo!("ThreadWorker::queue_work not yet implemented")
    }

    /// Wait for all queued work to complete.
    ///
    /// Port of `Common::ThreadWorker::WaitForRequests`.
    pub fn wait_for_requests(&self) {
        // TODO: Wait for thread pool completion
        todo!("ThreadWorker::wait_for_requests not yet implemented")
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

    #[test]
    fn worker_pool_initializes() {
        let workers = get_thread_workers();
        assert!(workers.num_threads() >= 1);
    }
}
