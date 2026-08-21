//! Port of zuyu/src/core/hle/kernel/k_worker_task.h and k_worker_task.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-11
//!
//! KWorkerTask — base for kernel objects that perform deferred work
//! (KThread and KProcess). Extends KSynchronizationObject.
//!
//! The DoWorkerTask() method dispatches to the concrete type's DoWorkerTaskImpl()
//! via DynamicCast; this requires KThread and KProcess to be ported.

use super::k_synchronization_object::KSynchronizationObject;

/// KWorkerTask — kernel object that can perform deferred worker tasks.
///
/// Mirrors upstream `Kernel::KWorkerTask : public KSynchronizationObject`.
pub struct KWorkerTask {
    pub base: KSynchronizationObject,
}

impl KWorkerTask {
    /// Construct a new KWorkerTask.
    /// Mirrors upstream `KWorkerTask::KWorkerTask(KernelCore& kernel)`.
    pub fn new(kernel: usize) -> Self {
        Self {
            base: KSynchronizationObject::new(kernel),
        }
    }

    /// Dispatch the worker task to the concrete type's implementation.
    /// Mirrors upstream `KWorkerTask::DoWorkerTask()`.
    ///
    /// In C++, this does:
    /// ```cpp
    /// if (auto* thread = this->DynamicCast<KThread*>()) {
    ///     thread->DoWorkerTaskImpl();
    /// } else {
    ///     auto* process = this->DynamicCast<KProcess*>();
    ///     process->DoWorkerTaskImpl();
    /// }
    /// ```
    ///
    /// Upstream: DynamicCast to KThread or KProcess and call DoWorkerTaskImpl().
    /// KThread and KProcess are ported but the dynamic dispatch is not wired.
    pub fn do_worker_task(&self) {
        // Upstream dispatches to KThread::DoWorkerTaskImpl or KProcess::DoWorkerTaskImpl
        // based on dynamic cast. In the host-emulated model, worker tasks are handled
        // by the service layer directly.
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_worker_task_creation() {
        let task = KWorkerTask::new(0);
        assert_eq!(task.base.base.base.m_kernel, 0);
    }
}
