// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of Eden `src/core/hle/service/os/process.h` and `process.cpp`.
//!
//! Process — manages a kernel process lifecycle.

use std::sync::Arc;

use crate::hle::kernel::k_process::ProcessLock;
use crate::hle::kernel::k_process::{KProcess, ProcessState};
use crate::hle::kernel::svc_types::ProcessActivity;
use crate::loader::loader::{AppLoader, ResultStatus, System as LoaderSystem};

/// Process — wraps a KProcess for service-level lifecycle management.
///
/// Upstream stores a `KProcess*` and `System&`. We store an optional
/// `Arc<ProcessLock>` reference.
pub struct Process {
    system: Option<crate::core::SystemRef>,
    main_thread_priority: i32,
    main_thread_stack_size: u64,
    process_started: bool,
    /// Reference to the kernel process object.
    /// Upstream: `KProcess* m_process`.
    process: Option<Arc<ProcessLock>>,
}

impl Process {
    pub fn new() -> Self {
        Self {
            system: None,
            main_thread_priority: 0,
            main_thread_stack_size: 0,
            process_started: false,
            process: None,
        }
    }

    /// Create with a KProcess reference.
    pub fn with_process(process: Arc<ProcessLock>) -> Self {
        Self {
            system: None,
            main_thread_priority: 0,
            main_thread_stack_size: 0,
            process_started: false,
            process: Some(process),
        }
    }

    /// Set the process reference.
    pub fn set_process(&mut self, process: Arc<ProcessLock>) {
        self.process = Some(process);
    }

    /// Insert process modules into memory and take ownership of the initialized
    /// kernel process. Port of upstream `Process::Initialize`.
    pub fn initialize(
        &mut self,
        system: crate::core::SystemRef,
        loader: &mut dyn AppLoader,
        out_load_result: &mut ResultStatus,
    ) -> bool {
        self.finalize();

        let system_ref = system.get();
        let Some(kernel) = system_ref.kernel() else {
            *out_load_result = ResultStatus::ErrorNotInitialized;
            return false;
        };

        let mut process = KProcess::new();
        process.create_memory(system_ref);
        process.process_id = kernel.create_new_user_process_id();

        let mut loader_system = LoaderSystem {
            content_provider: system_ref.get_content_provider().cloned(),
            filesystem_controller: Some(system_ref.get_filesystem_controller()),
        };
        let (load_result, load_parameters) = loader.load(&mut process, &mut loader_system);
        *out_load_result = load_result;
        if load_result != ResultStatus::Success {
            return false;
        }
        let Some(load_parameters) = load_parameters else {
            *out_load_result = ResultStatus::ErrorNotInitialized;
            return false;
        };

        let process = Arc::new(ProcessLock::from_value(process));
        process.lock().unwrap().bind_self_reference(&process);
        kernel.register_process(Arc::clone(&process));
        if let Some(global_scheduler_context) = kernel.global_scheduler_context() {
            process
                .lock()
                .unwrap()
                .set_global_scheduler_context(Arc::clone(global_scheduler_context));
        }
        if let Some(scheduler) = kernel.scheduler(0) {
            process.lock().unwrap().attach_scheduler(scheduler);
        }
        let shared_memory = process.lock().unwrap().get_shared_memory();
        process
            .lock()
            .unwrap()
            .initialize_interfaces(shared_memory, system_ref.core_timing_shared());

        self.system = Some(system);
        self.main_thread_priority = load_parameters.main_thread_priority;
        self.main_thread_stack_size = load_parameters.main_thread_stack_size;
        self.process_started = false;
        self.process = Some(process);
        true
    }

    /// Check if the process has been initialized.
    pub fn is_initialized(&self) -> bool {
        self.process.is_some()
    }

    /// Run the process.
    pub fn run(&mut self) -> bool {
        if self.process_started {
            return false;
        }
        if let (Some(system), Some(process)) = (self.system, self.process.as_ref()) {
            let Some(kernel) = system.get().kernel() else {
                return false;
            };
            let main_thread_id = kernel.create_new_thread_id();
            let main_object_id = kernel.create_new_object_id() as u64;
            let is_64bit = process.lock().unwrap().is_64bit();
            let kernel_ptr = kernel as *const crate::hle::kernel::kernel::KernelCore as usize;
            let guest_thread_func: Option<Box<dyn FnOnce() + Send>> = Some(Box::new(move || {
                // SAFETY: System owns KernelCore and outlives every guest process.
                let kernel =
                    unsafe { &*(kernel_ptr as *const crate::hle::kernel::kernel::KernelCore) };
                if kernel.is_multicore() {
                    crate::cpu_manager::CpuManager::multi_core_run_guest_thread(kernel);
                } else {
                    crate::cpu_manager::CpuManager::single_core_run_guest_thread_entry(kernel);
                }
            }));
            if let Err(result) = process.lock().unwrap().run(
                self.main_thread_priority,
                self.main_thread_stack_size as usize,
                main_thread_id,
                main_object_id,
                is_64bit,
                guest_thread_func,
            ) {
                log::error!("Process::run failed with result 0x{result:X}");
                return false;
            }
        }
        self.process_started = true;
        true
    }

    /// Terminate the process.
    pub fn terminate(&mut self) {
        if let Some(ref process) = self.process {
            process.lock().unwrap().terminate();
        }
    }

    /// Finalize and release the process.
    pub fn finalize(&mut self) {
        self.terminate();
        if let (Some(system), Some(process)) = (self.system, self.process.as_ref()) {
            if let Some(kernel) = system.get().kernel() {
                kernel.remove_process(process);
            }
        }
        self.main_thread_priority = 0;
        self.main_thread_stack_size = 0;
        self.process_started = false;
        self.process = None;
        self.system = None;
    }

    /// Check if the process is running.
    pub fn is_running(&self) -> bool {
        if let Some(ref process) = self.process {
            let p = process.lock().unwrap();
            matches!(
                p.get_state(),
                ProcessState::Running | ProcessState::RunningAttached | ProcessState::DebugBreak
            )
        } else {
            false
        }
    }

    /// Check if the process is terminated.
    pub fn is_terminated(&self) -> bool {
        if let Some(ref process) = self.process {
            let p = process.lock().unwrap();
            p.get_state() == ProcessState::Terminated
        } else {
            false
        }
    }

    /// Get the process ID.
    pub fn get_process_id(&self) -> u64 {
        if let Some(ref process) = self.process {
            process.lock().unwrap().get_process_id()
        } else {
            0
        }
    }

    /// Get the program ID.
    pub fn get_program_id(&self) -> u64 {
        if let Some(ref process) = self.process {
            process.lock().unwrap().get_program_id()
        } else {
            0
        }
    }

    /// Suspend or resume the process.
    pub fn suspend(&self, suspended: bool) {
        if let Some(ref process) = self.process {
            let mut p = process.lock().unwrap();
            let activity = if suspended {
                ProcessActivity::Paused
            } else {
                ProcessActivity::Runnable
            };
            let _ = p.set_activity(activity);
        }
    }

    /// Reset the process signal.
    pub fn reset_signal(&self) {
        if let Some(ref process) = self.process {
            process.lock().unwrap().reset();
        }
    }

    /// Get a clone of the KProcess reference.
    pub fn get_process(&self) -> Option<Arc<ProcessLock>> {
        self.process.clone()
    }

    /// Upstream: `Kernel::KProcess* GetHandle() const`.
    pub fn get_handle(&self) -> Option<Arc<ProcessLock>> {
        self.process.clone()
    }
}

impl Default for Process {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for Process {
    fn drop(&mut self) {
        self.finalize();
    }
}

#[cfg(test)]
mod tests {
    use super::Process;

    #[test]
    fn terminate_does_not_make_a_started_process_restartable() {
        let mut process = Process::new();
        assert!(process.run());
        process.terminate();
        assert!(!process.run());
    }
}
