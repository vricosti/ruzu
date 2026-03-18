// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/clocks/context_writers.h/.cpp
//!
//! ContextWriter and its concrete implementations: LocalSystemClockContextWriter,
//! NetworkSystemClockContextWriter, EphemeralNetworkSystemClockContextWriter.
//!
//! Each writer maintains a list of OperationEvent subscribers. When a context
//! changes, `signal_all_nodes()` iterates the list and signals every registered
//! event — matching the upstream intrusive-list multi-subscriber pattern.

use std::sync::Mutex;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::psc::time::common::{OperationEvent, SystemClockContext};

// =========================================================================
// Base ContextWriter — manages the OperationEvent list
// =========================================================================

/// Shared base state for all context writers.
///
/// Corresponds to the `ContextWriter` base class in upstream context_writers.h,
/// which owns an `OperationEventList` (intrusive list) and a mutex.
struct ContextWriterBase {
    mutex: Mutex<()>,
    operation_events: Vec<OperationEvent>,
}

impl ContextWriterBase {
    fn new() -> Self {
        Self {
            mutex: Mutex::new(()),
            operation_events: Vec::new(),
        }
    }

    /// Signal all linked operation events.
    ///
    /// Corresponds to `ContextWriter::SignalAllNodes()` in upstream.
    /// Iterates the list and calls `Signal()` on each event's KEvent.
    fn signal_all_nodes(&self) {
        let _lock = self.mutex.lock().unwrap();
        for operation in &self.operation_events {
            operation.signal();
        }
    }

    /// Link an operation event to this writer.
    ///
    /// Corresponds to `ContextWriter::Link(OperationEvent&)` in upstream.
    /// Pushes the event onto the intrusive list (here: Vec).
    fn link(&mut self, operation_event: OperationEvent) {
        let _lock = self.mutex.lock().unwrap();
        self.operation_events.push(operation_event);
    }
}

// =========================================================================
// ContextWriter trait
// =========================================================================

/// ContextWriter trait matching the upstream abstract ContextWriter class.
pub trait ContextWriter: Send + Sync {
    /// Write a new context. If the context changed, update shared memory
    /// (if applicable) and signal all linked operation events.
    fn write(&mut self, context: &SystemClockContext) -> ResultCode;

    /// Signal all linked operation events.
    fn signal_all_nodes(&self);

    /// Link an operation event to this writer for notifications.
    fn link(&mut self, operation_event: OperationEvent);
}

// =========================================================================
// LocalSystemClockContextWriter
// =========================================================================

/// LocalSystemClockContextWriter writes local system clock context to shared memory
/// and signals all linked operation events.
///
/// Corresponds to `LocalSystemClockContextWriter` in upstream context_writers.h.
pub struct LocalSystemClockContextWriter {
    base: ContextWriterBase,
    in_use: bool,
    context: SystemClockContext,
    /// Callback to set context in shared memory.
    /// Matches upstream: `m_shared_memory.SetLocalSystemContext(context)`
    set_shared_memory: Option<Box<dyn Fn(&SystemClockContext) + Send + Sync>>,
}

impl LocalSystemClockContextWriter {
    pub fn new() -> Self {
        Self {
            base: ContextWriterBase::new(),
            in_use: false,
            context: SystemClockContext::default(),
            set_shared_memory: None,
        }
    }

    pub fn set_shared_memory_callback(
        &mut self,
        cb: Box<dyn Fn(&SystemClockContext) + Send + Sync>,
    ) {
        self.set_shared_memory = Some(cb);
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
        self.base.signal_all_nodes();
    }

    fn link(&mut self, operation_event: OperationEvent) {
        self.base.link(operation_event);
    }
}

// =========================================================================
// NetworkSystemClockContextWriter
// =========================================================================

/// NetworkSystemClockContextWriter writes network system clock context to shared
/// memory and signals all linked operation events.
///
/// Corresponds to `NetworkSystemClockContextWriter` in upstream context_writers.h.
///
/// Note: upstream also holds a `SystemClockCore&` and calls `GetCurrentTime(&time)`
/// in `Write()`, but the result is discarded (`[[maybe_unused]]`). We preserve
/// this call site for parity but currently it's a no-op.
pub struct NetworkSystemClockContextWriter {
    base: ContextWriterBase,
    in_use: bool,
    context: SystemClockContext,
    /// Callback to set context in shared memory.
    /// Matches upstream: `m_shared_memory.SetNetworkSystemContext(context)`
    set_shared_memory: Option<Box<dyn Fn(&SystemClockContext) + Send + Sync>>,
    /// Callback matching upstream `m_system_clock.GetCurrentTime(&time)`.
    /// Return value is discarded in upstream. Kept for parity.
    get_current_time: Option<Box<dyn Fn() -> i64 + Send + Sync>>,
}

impl NetworkSystemClockContextWriter {
    pub fn new() -> Self {
        Self {
            base: ContextWriterBase::new(),
            in_use: false,
            context: SystemClockContext::default(),
            set_shared_memory: None,
            get_current_time: None,
        }
    }

    pub fn set_shared_memory_callback(
        &mut self,
        cb: Box<dyn Fn(&SystemClockContext) + Send + Sync>,
    ) {
        self.set_shared_memory = Some(cb);
    }

    pub fn set_get_current_time_callback(
        &mut self,
        cb: Box<dyn Fn() -> i64 + Send + Sync>,
    ) {
        self.get_current_time = Some(cb);
    }
}

impl ContextWriter for NetworkSystemClockContextWriter {
    fn write(&mut self, context: &SystemClockContext) -> ResultCode {
        // Upstream: s64 time{}; [[maybe_unused]] auto res = m_system_clock.GetCurrentTime(&time);
        if let Some(ref get_time) = self.get_current_time {
            let _time = get_time();
        }

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
        self.base.signal_all_nodes();
    }

    fn link(&mut self, operation_event: OperationEvent) {
        self.base.link(operation_event);
    }
}

// =========================================================================
// EphemeralNetworkSystemClockContextWriter
// =========================================================================

/// EphemeralNetworkSystemClockContextWriter signals linked operation events
/// without writing to shared memory.
///
/// Corresponds to `EphemeralNetworkSystemClockContextWriter` in upstream
/// context_writers.h.
pub struct EphemeralNetworkSystemClockContextWriter {
    base: ContextWriterBase,
    in_use: bool,
    context: SystemClockContext,
}

impl EphemeralNetworkSystemClockContextWriter {
    pub fn new() -> Self {
        Self {
            base: ContextWriterBase::new(),
            in_use: false,
            context: SystemClockContext::default(),
        }
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
        self.base.signal_all_nodes();
    }

    fn link(&mut self, operation_event: OperationEvent) {
        self.base.link(operation_event);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hle::service::psc::time::common::SteadyClockTimePoint;

    #[test]
    fn local_writer_skips_duplicate_context() {
        let mut writer = LocalSystemClockContextWriter::new();
        let ctx = SystemClockContext {
            offset: 100,
            steady_time_point: SteadyClockTimePoint::default(),
        };
        assert_eq!(writer.write(&ctx), RESULT_SUCCESS);
        // Same context again — should short-circuit
        assert_eq!(writer.write(&ctx), RESULT_SUCCESS);
    }

    #[test]
    fn writer_can_link_multiple_events() {
        let mut writer = LocalSystemClockContextWriter::new();
        writer.link(OperationEvent::new());
        writer.link(OperationEvent::new());
        writer.link(OperationEvent::new());

        // signal_all_nodes should iterate all 3 (no panic)
        writer.signal_all_nodes();
    }

    #[test]
    fn ephemeral_writer_has_no_shared_memory() {
        let mut writer = EphemeralNetworkSystemClockContextWriter::new();
        let ctx = SystemClockContext {
            offset: 42,
            steady_time_point: SteadyClockTimePoint::default(),
        };
        // Should succeed without any shared memory callback
        assert_eq!(writer.write(&ctx), RESULT_SUCCESS);
    }

    #[test]
    fn network_writer_calls_get_current_time() {
        use std::sync::atomic::{AtomicBool, Ordering};
        use std::sync::Arc;

        let called = Arc::new(AtomicBool::new(false));
        let called_clone = Arc::clone(&called);

        let mut writer = NetworkSystemClockContextWriter::new();
        writer.set_get_current_time_callback(Box::new(move || {
            called_clone.store(true, Ordering::Relaxed);
            12345
        }));

        let ctx = SystemClockContext {
            offset: 1,
            steady_time_point: SteadyClockTimePoint::default(),
        };
        writer.write(&ctx);
        assert!(called.load(Ordering::Relaxed));
    }
}
