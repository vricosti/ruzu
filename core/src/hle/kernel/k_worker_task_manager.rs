//! Port of zuyu/src/core/hle/kernel/k_worker_task_manager.h and k_worker_task_manager.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-11
//!
//! KWorkerTaskManager — manages a background worker thread that executes
//! deferred kernel tasks (KWorkerTask).
//!
//! The C++ version uses Common::ThreadWorker for the background thread pool.

use std::sync::{Arc, Mutex};
use std::thread;

/// Worker type enum matching upstream `KWorkerTaskManager::WorkerType`.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WorkerType {
    Exit = 0,
    Count = 1,
}

/// KWorkerTaskManager — manages background execution of kernel worker tasks.
///
/// Mirrors upstream `Kernel::KWorkerTaskManager`.
/// Uses a simple channel-based task queue instead of Common::ThreadWorker.
pub struct KWorkerTaskManager {
    /// Task sender for queueing work.
    sender: std::sync::mpsc::Sender<Box<dyn FnOnce() + Send>>,
    /// Worker thread handle.
    _worker: Option<thread::JoinHandle<()>>,
}

impl KWorkerTaskManager {
    /// Create a new KWorkerTaskManager with one background worker thread.
    /// Mirrors upstream `KWorkerTaskManager::KWorkerTaskManager()`.
    pub fn new() -> Self {
        let (sender, receiver) = std::sync::mpsc::channel::<Box<dyn FnOnce() + Send>>();

        let worker = thread::Builder::new()
            .name("KWorkerTaskManager".to_string())
            .spawn(move || {
                while let Ok(task) = receiver.recv() {
                    task();
                }
            })
            .expect("Failed to spawn KWorkerTaskManager thread");

        Self {
            sender,
            _worker: Some(worker),
        }
    }

    /// Add a task to the worker queue (static dispatch).
    /// Mirrors upstream `KWorkerTaskManager::AddTask(KernelCore& kernel, WorkerType type, KWorkerTask* task)`.
    ///
    /// TODO: The full implementation acquires the scheduler lock before queueing.
    /// TODO: Takes a KWorkerTask once it's fully ported.
    pub fn add_task_static(
        _kernel: usize,
        worker_type: WorkerType,
        task: Box<dyn FnOnce() + Send>,
    ) {
        debug_assert!(worker_type as u32 <= WorkerType::Count as u32);
        // TODO: kernel.WorkerTaskManager().add_task(kernel, task);
        // For now, just execute directly as a placeholder.
        task();
    }

    /// Add a task to the worker queue (instance method).
    /// Mirrors upstream `KWorkerTaskManager::AddTask(KernelCore& kernel, KWorkerTask* task)`.
    pub fn add_task(&self, task: Box<dyn FnOnce() + Send>) {
        // TODO: KScopedSchedulerLock sl(kernel);
        let _ = self.sender.send(task);
    }
}

impl Default for KWorkerTaskManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicBool, Ordering};

    #[test]
    fn test_worker_type_values() {
        assert_eq!(WorkerType::Exit as u32, 0);
        assert_eq!(WorkerType::Count as u32, 1);
    }

    #[test]
    fn test_worker_task_manager_executes_task() {
        let manager = KWorkerTaskManager::new();
        let flag = Arc::new(AtomicBool::new(false));
        let flag_clone = flag.clone();

        manager.add_task(Box::new(move || {
            flag_clone.store(true, Ordering::SeqCst);
        }));

        // Give the worker thread time to execute.
        std::thread::sleep(std::time::Duration::from_millis(50));
        assert!(flag.load(Ordering::SeqCst));
    }
}
