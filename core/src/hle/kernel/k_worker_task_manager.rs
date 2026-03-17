//! Port of zuyu/src/core/hle/kernel/k_worker_task_manager.h and k_worker_task_manager.cpp
//! Status: Partial (async queue wired; KernelCore ownership not yet ported)
//! Derniere synchro: 2026-03-11
//!
//! KWorkerTaskManager — manages a background worker thread that executes
//! deferred kernel tasks (KWorkerTask).
//!
//! The C++ version uses Common::ThreadWorker for the background thread pool.

use std::sync::OnceLock;
use std::sync::{Arc, Condvar, Mutex};
use std::thread;

enum WorkerMessage {
    Task(Box<dyn FnOnce() + Send>),
}

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
    sender: std::sync::mpsc::Sender<WorkerMessage>,
    /// Worker thread handle.
    _worker: Option<thread::JoinHandle<()>>,
    /// Number of queued/running tasks.
    pending_tasks: Arc<(Mutex<usize>, Condvar)>,
}

impl KWorkerTaskManager {
    /// Create a new KWorkerTaskManager with one background worker thread.
    /// Mirrors upstream `KWorkerTaskManager::KWorkerTaskManager()`.
    pub fn new() -> Self {
        let (sender, receiver) = std::sync::mpsc::channel::<WorkerMessage>();
        let pending_tasks = Arc::new((Mutex::new(0usize), Condvar::new()));
        let pending_tasks_worker = pending_tasks.clone();

        let worker = thread::Builder::new()
            .name("KWorkerTaskManager".to_string())
            .spawn(move || {
                while let Ok(message) = receiver.recv() {
                    match message {
                        WorkerMessage::Task(task) => {
                            task();
                            let (lock, cv) = &*pending_tasks_worker;
                            let mut pending = lock.lock().unwrap();
                            debug_assert!(*pending > 0);
                            *pending -= 1;
                            if *pending == 0 {
                                cv.notify_all();
                            }
                        }
                    }
                }
            })
            .expect("Failed to spawn KWorkerTaskManager thread");

        Self {
            sender,
            _worker: Some(worker),
            pending_tasks,
        }
    }

    fn global() -> &'static KWorkerTaskManager {
        static GLOBAL: OnceLock<KWorkerTaskManager> = OnceLock::new();
        GLOBAL.get_or_init(KWorkerTaskManager::new)
    }

    /// Add a task to the worker queue (static dispatch).
    /// Mirrors upstream `KWorkerTaskManager::AddTask(KernelCore& kernel, WorkerType type, KWorkerTask* task)`.
    pub fn add_task_static(
        _kernel: usize,
        worker_type: WorkerType,
        task: Box<dyn FnOnce() + Send>,
    ) {
        debug_assert!(worker_type as u32 <= WorkerType::Count as u32);
        Self::global().add_task(task);
    }

    /// Add a task to the worker queue (instance method).
    /// Mirrors upstream `KWorkerTaskManager::AddTask(KernelCore& kernel, KWorkerTask* task)`.
    pub fn add_task(&self, task: Box<dyn FnOnce() + Send>) {
        let (lock, _) = &*self.pending_tasks;
        *lock.lock().unwrap() += 1;
        let _ = self.sender.send(WorkerMessage::Task(task));
    }

    pub fn wait_for_idle(&self) {
        let (lock, cv) = &*self.pending_tasks;
        let mut pending = lock.lock().unwrap();
        while *pending != 0 {
            pending = cv.wait(pending).unwrap();
        }
    }

    pub fn wait_for_global_idle() {
        Self::global().wait_for_idle();
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

        manager.wait_for_idle();
        assert!(flag.load(Ordering::SeqCst));
    }
}
