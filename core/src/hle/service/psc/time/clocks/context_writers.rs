// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/clocks/context_writers.h/.cpp
//!
//! ContextWriter and its concrete implementations: LocalSystemClockContextWriter,
//! NetworkSystemClockContextWriter, EphemeralNetworkSystemClockContextWriter.

use std::sync::Mutex;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::psc::time::common::SystemClockContext;

/// ContextWriter trait matching the upstream abstract ContextWriter class.
pub trait ContextWriter: Send + Sync {
    fn write(&mut self, context: &SystemClockContext) -> ResultCode;
    fn signal_all_nodes(&self);
}

/// LocalSystemClockContextWriter writes local system clock context to shared memory.
pub struct LocalSystemClockContextWriter {
    mutex: Mutex<()>,
    in_use: bool,
    context: SystemClockContext,
    /// Callback to set context in shared memory.
    /// Matches upstream: m_shared_memory.SetLocalSystemContext(context)
    set_shared_memory: Option<Box<dyn Fn(&SystemClockContext) + Send + Sync>>,
    /// Callback to signal all operation events.
    signal_fn: Option<Box<dyn Fn() + Send + Sync>>,
}

impl LocalSystemClockContextWriter {
    pub fn new() -> Self {
        Self {
            mutex: Mutex::new(()),
            in_use: false,
            context: SystemClockContext::default(),
            set_shared_memory: None,
            signal_fn: None,
        }
    }

    pub fn set_shared_memory_callback(
        &mut self,
        cb: Box<dyn Fn(&SystemClockContext) + Send + Sync>,
    ) {
        self.set_shared_memory = Some(cb);
    }

    pub fn set_signal_callback(&mut self, cb: Box<dyn Fn() + Send + Sync>) {
        self.signal_fn = Some(cb);
    }
}

impl ContextWriter for LocalSystemClockContextWriter {
    fn write(&mut self, context: &SystemClockContext) -> ResultCode {
        if self.in_use {
            if *context == self.context {
                return RESULT_SUCCESS;
            }
            self.context = *context;
        } else {
            self.context = *context;
            self.in_use = true;
        }

        if let Some(ref cb) = self.set_shared_memory {
            cb(context);
        }

        self.signal_all_nodes();
        RESULT_SUCCESS
    }

    fn signal_all_nodes(&self) {
        if let Some(ref signal) = self.signal_fn {
            signal();
        }
    }
}

/// NetworkSystemClockContextWriter writes network system clock context to shared memory.
pub struct NetworkSystemClockContextWriter {
    mutex: Mutex<()>,
    in_use: bool,
    context: SystemClockContext,
    /// Callback to set context in shared memory.
    set_shared_memory: Option<Box<dyn Fn(&SystemClockContext) + Send + Sync>>,
    /// Callback to signal all operation events.
    signal_fn: Option<Box<dyn Fn() + Send + Sync>>,
    // Note: upstream also holds a SystemClockCore& to call GetCurrentTime.
    // That call's return value is discarded (result unused), so we omit it.
}

impl NetworkSystemClockContextWriter {
    pub fn new() -> Self {
        Self {
            mutex: Mutex::new(()),
            in_use: false,
            context: SystemClockContext::default(),
            set_shared_memory: None,
            signal_fn: None,
        }
    }

    pub fn set_shared_memory_callback(
        &mut self,
        cb: Box<dyn Fn(&SystemClockContext) + Send + Sync>,
    ) {
        self.set_shared_memory = Some(cb);
    }

    pub fn set_signal_callback(&mut self, cb: Box<dyn Fn() + Send + Sync>) {
        self.signal_fn = Some(cb);
    }
}

impl ContextWriter for NetworkSystemClockContextWriter {
    fn write(&mut self, context: &SystemClockContext) -> ResultCode {
        // Upstream: calls m_system_clock.GetCurrentTime(&time) but discards result
        if self.in_use {
            if *context == self.context {
                return RESULT_SUCCESS;
            }
            self.context = *context;
        } else {
            self.context = *context;
            self.in_use = true;
        }

        if let Some(ref cb) = self.set_shared_memory {
            cb(context);
        }

        self.signal_all_nodes();
        RESULT_SUCCESS
    }

    fn signal_all_nodes(&self) {
        if let Some(ref signal) = self.signal_fn {
            signal();
        }
    }
}

/// EphemeralNetworkSystemClockContextWriter: signals nodes without shared memory.
pub struct EphemeralNetworkSystemClockContextWriter {
    mutex: Mutex<()>,
    in_use: bool,
    context: SystemClockContext,
    signal_fn: Option<Box<dyn Fn() + Send + Sync>>,
}

impl EphemeralNetworkSystemClockContextWriter {
    pub fn new() -> Self {
        Self {
            mutex: Mutex::new(()),
            in_use: false,
            context: SystemClockContext::default(),
            signal_fn: None,
        }
    }

    pub fn set_signal_callback(&mut self, cb: Box<dyn Fn() + Send + Sync>) {
        self.signal_fn = Some(cb);
    }
}

impl ContextWriter for EphemeralNetworkSystemClockContextWriter {
    fn write(&mut self, context: &SystemClockContext) -> ResultCode {
        if self.in_use {
            if *context == self.context {
                return RESULT_SUCCESS;
            }
            self.context = *context;
        } else {
            self.context = *context;
            self.in_use = true;
        }

        self.signal_all_nodes();
        RESULT_SUCCESS
    }

    fn signal_all_nodes(&self) {
        if let Some(ref signal) = self.signal_fn {
            signal();
        }
    }
}
