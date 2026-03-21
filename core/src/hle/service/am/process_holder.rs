// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/process_holder.h
//! Port of zuyu/src/core/hle/service/am/process_holder.cpp

use std::sync::{Arc, Mutex};

use crate::hle::service::os::multi_wait_holder::MultiWaitHolder;
use crate::hle::service::os::process::Process;

use super::applet::Applet;

/// Port of ProcessHolder
///
/// Holds a reference to an Applet and its Process for multi-wait observation.
///
/// Upstream inherits from MultiWaitHolder (initialized with process.GetHandle())
/// and Common::IntrusiveListBaseNode<ProcessHolder>. The intrusive list membership
/// is handled externally in Rust (e.g., stored in a Vec or linked list by the
/// applet manager).
pub struct ProcessHolder {
    /// The Applet this holder is associated with.
    /// Upstream: `Applet& m_applet`.
    applet: Arc<Mutex<Applet>>,
    /// The Process this holder is observing.
    /// Upstream: `Process& m_process`.
    process: Arc<Mutex<Process>>,
    /// MultiWaitHolder base for synchronization.
    /// Upstream initializes this with `process.GetHandle()`.
    multi_wait_holder: MultiWaitHolder,
}

impl ProcessHolder {
    /// Creates a new ProcessHolder.
    ///
    /// Upstream: `ProcessHolder::ProcessHolder(Applet& applet, Process& process)`
    /// initializes `MultiWaitHolder(process.GetHandle())`.
    pub fn new(applet: Arc<Mutex<Applet>>, process: Arc<Mutex<Process>>) -> Self {
        Self {
            applet,
            process,
            multi_wait_holder: MultiWaitHolder::new(),
        }
    }

    /// Get a reference to the applet.
    pub fn get_applet(&self) -> &Arc<Mutex<Applet>> {
        &self.applet
    }

    /// Get a reference to the process.
    pub fn get_process(&self) -> &Arc<Mutex<Process>> {
        &self.process
    }

    /// Get a reference to the multi-wait holder.
    pub fn get_multi_wait_holder(&self) -> &MultiWaitHolder {
        &self.multi_wait_holder
    }

    /// Get a mutable reference to the multi-wait holder.
    pub fn get_multi_wait_holder_mut(&mut self) -> &mut MultiWaitHolder {
        &mut self.multi_wait_holder
    }
}
