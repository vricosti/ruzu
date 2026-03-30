// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/os/process.h
//! Port of zuyu/src/core/hle/service/os/process.cpp
//!
//! Process — manages a kernel process lifecycle.

use std::sync::{Arc, Mutex};

use crate::hle::kernel::k_process::{KProcess, ProcessState};

/// Process — wraps a KProcess for service-level lifecycle management.
///
/// Upstream stores a `KProcess*` and `System&`. We store an optional
/// `Arc<Mutex<KProcess>>` reference.
pub struct Process {
    main_thread_priority: i32,
    main_thread_stack_size: u64,
    process_started: bool,
    /// Reference to the kernel process object.
    /// Upstream: `KProcess* m_process`.
    process: Option<Arc<Mutex<KProcess>>>,
}

impl Process {
    pub fn new() -> Self {
        Self {
            main_thread_priority: 0,
            main_thread_stack_size: 0,
            process_started: false,
            process: None,
        }
    }

    /// Create with a KProcess reference.
    pub fn with_process(process: Arc<Mutex<KProcess>>) -> Self {
        Self {
            main_thread_priority: 0,
            main_thread_stack_size: 0,
            process_started: false,
            process: Some(process),
        }
    }

    /// Set the process reference.
    pub fn set_process(&mut self, process: Arc<Mutex<KProcess>>) {
        self.process = Some(process);
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
        self.process_started = true;
        true
    }

    /// Terminate the process.
    pub fn terminate(&mut self) {
        if let Some(ref process) = self.process {
            process.lock().unwrap().terminate();
        }
        self.process_started = false;
    }

    /// Finalize and release the process.
    pub fn finalize(&mut self) {
        self.terminate();
        self.main_thread_priority = 0;
        self.main_thread_stack_size = 0;
        self.process = None;
    }

    /// Check if the process is running.
    pub fn is_running(&self) -> bool {
        if let Some(ref process) = self.process {
            let p = process.lock().unwrap();
            p.get_state() == ProcessState::Running
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
            if suspended {
                p.set_suspended(true);
            } else {
                p.set_suspended(false);
            }
        }
    }

    /// Reset the process signal.
    pub fn reset_signal(&self) {
        if let Some(ref process) = self.process {
            process.lock().unwrap().reset();
        }
    }

    /// Get a clone of the KProcess reference.
    pub fn get_process(&self) -> Option<Arc<Mutex<KProcess>>> {
        self.process.clone()
    }

    /// Upstream: `Kernel::KProcess* GetHandle() const`.
    pub fn get_handle(&self) -> Option<Arc<Mutex<KProcess>>> {
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
