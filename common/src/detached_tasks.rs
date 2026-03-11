// SPDX-FileCopyrightText: 2018 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/common/detached_tasks.h and zuyu/src/common/detached_tasks.cpp
//!
//! A background task manager that ensures all detached tasks complete before
//! program exit. A single `DetachedTasks` instance should be created in main
//! and `wait_for_all_tasks()` called before shutdown.

use std::sync::{Arc, Condvar, Mutex, OnceLock};
use std::thread;

/// Global singleton, matching the C++ `DetachedTasks* DetachedTasks::instance`.
static INSTANCE: OnceLock<Arc<DetachedTasksInner>> = OnceLock::new();

struct DetachedTasksInner {
    mutex: Mutex<i32>,
    cv: Condvar,
}

impl std::fmt::Debug for DetachedTasksInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DetachedTasksInner").finish()
    }
}

/// A background task manager. Ensures all tasks added via `add_task` complete
/// before the manager is dropped.
///
/// Only one `DetachedTasks` instance should exist at a time (matching upstream
/// `ASSERT(instance == nullptr)`).
pub struct DetachedTasks {
    inner: Arc<DetachedTasksInner>,
}

impl DetachedTasks {
    /// Create the global `DetachedTasks` instance.
    ///
    /// # Panics
    /// Panics if an instance already exists.
    pub fn new() -> Self {
        let inner = Arc::new(DetachedTasksInner {
            mutex: Mutex::new(0),
            cv: Condvar::new(),
        });
        INSTANCE
            .set(inner.clone())
            .expect("DetachedTasks instance already exists");
        Self { inner }
    }

    /// Block until all outstanding tasks have completed.
    pub fn wait_for_all_tasks(&self) {
        let guard = self.inner.mutex.lock().unwrap();
        let _guard = self
            .inner
            .cv
            .wait_while(guard, |count| *count != 0)
            .unwrap();
    }

    /// Submit a task to run on a detached background thread.
    ///
    /// The task is tracked so that `wait_for_all_tasks` / `Drop` will block
    /// until it finishes.
    ///
    /// # Panics
    /// Panics if no `DetachedTasks` instance exists.
    pub fn add_task<F>(task: F)
    where
        F: FnOnce() + Send + 'static,
    {
        let inner = INSTANCE
            .get()
            .expect("DetachedTasks::add_task called before instance created")
            .clone();

        {
            let mut count = inner.mutex.lock().unwrap();
            *count += 1;
        }

        thread::spawn(move || {
            task();
            {
                let mut count = inner.mutex.lock().unwrap();
                *count -= 1;
            }
            inner.cv.notify_all();
        });
    }
}

impl Drop for DetachedTasks {
    fn drop(&mut self) {
        self.wait_for_all_tasks();
        // Verify count is zero.
        let count = self.inner.mutex.lock().unwrap();
        assert_eq!(*count, 0, "DetachedTasks dropped with outstanding tasks");
        // Note: we cannot reset the OnceLock in stable Rust, so only one
        // DetachedTasks instance can exist per process lifetime. This matches
        // the typical usage where a single instance lives in main().
    }
}

#[cfg(test)]
mod tests {
    // Note: Because DetachedTasks uses a global OnceLock, tests that create
    // instances cannot be run in parallel within the same process. In practice
    // the singleton pattern means only one test can use it.
    //
    // We test the inner mechanics directly instead.

    use super::*;
    use std::sync::atomic::{AtomicU32, Ordering};

    #[test]
    fn test_inner_task_tracking() {
        let inner = Arc::new(DetachedTasksInner {
            mutex: Mutex::new(0),
            cv: Condvar::new(),
        });

        let counter = Arc::new(AtomicU32::new(0));

        for _ in 0..5 {
            let inner_clone = inner.clone();
            let counter_clone = counter.clone();
            {
                let mut count = inner.mutex.lock().unwrap();
                *count += 1;
            }
            thread::spawn(move || {
                counter_clone.fetch_add(1, Ordering::SeqCst);
                {
                    let mut count = inner_clone.mutex.lock().unwrap();
                    *count -= 1;
                }
                inner_clone.cv.notify_all();
            });
        }

        // Wait for all tasks.
        let guard = inner.mutex.lock().unwrap();
        let _guard = inner.cv.wait_while(guard, |count| *count != 0).unwrap();

        assert_eq!(counter.load(Ordering::SeqCst), 5);
    }
}
