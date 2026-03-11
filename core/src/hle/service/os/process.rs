// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/os/process.h
//! Port of zuyu/src/core/hle/service/os/process.cpp
//!
//! Process — manages a kernel process lifecycle.

/// Process — wraps a KProcess for service-level lifecycle management.
///
/// Corresponds to `Process` in upstream process.h / process.cpp.
pub struct Process {
    main_thread_priority: i32,
    main_thread_stack_size: u64,
    process_started: bool,
    // TODO: KProcess handle, System reference
}

impl Process {
    pub fn new() -> Self {
        Self {
            main_thread_priority: 0,
            main_thread_stack_size: 0,
            process_started: false,
        }
    }

    /// Check if the process has been initialized.
    pub fn is_initialized(&self) -> bool {
        // TODO: check m_process != null
        false
    }

    /// Run the process.
    pub fn run(&mut self) -> bool {
        if self.process_started {
            return false;
        }
        // TODO: start the process
        self.process_started = true;
        true
    }

    /// Terminate the process.
    pub fn terminate(&mut self) {
        // TODO: terminate the process
    }

    /// Finalize and release the process.
    pub fn finalize(&mut self) {
        self.terminate();
        self.main_thread_priority = 0;
        self.main_thread_stack_size = 0;
        self.process_started = false;
    }

    /// Check if the process is running.
    pub fn is_running(&self) -> bool {
        false // TODO
    }

    /// Check if the process is terminated.
    pub fn is_terminated(&self) -> bool {
        false // TODO
    }

    /// Get the process ID.
    pub fn get_process_id(&self) -> u64 {
        0 // TODO
    }

    /// Get the program ID.
    pub fn get_program_id(&self) -> u64 {
        0 // TODO
    }

    /// Suspend or resume the process.
    pub fn suspend(&self, _suspended: bool) {
        // TODO
    }

    /// Reset the process signal.
    pub fn reset_signal(&self) {
        // TODO
    }
}

impl Drop for Process {
    fn drop(&mut self) {
        self.finalize();
    }
}
