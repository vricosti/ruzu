// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/os/event.h
//! Port of zuyu/src/core/hle/service/os/event.cpp
//!
//! Event wrapper for kernel KEvent.

use std::sync::{Arc, Condvar, Mutex};

use crate::hle::kernel::k_process::KProcess;
use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::kernel::k_scheduler::KScheduler;

/// Bridge from a service-layer Event to a kernel KReadableEvent.
///
/// Upstream `Event` holds a `KEvent*` and `Signal()` calls `m_event->Signal()`
/// which in turn calls `KReadableEvent::Signal()`. This bridge replicates that
/// chain: when the service Event is signaled, we also signal the kernel readable
/// event to wake any threads blocked in WaitSynchronization.
struct KernelEventBridge {
    readable_event: Arc<Mutex<KReadableEvent>>,
    process: Arc<Mutex<KProcess>>,
    scheduler: Arc<Mutex<KScheduler>>,
}

/// Event — wraps a kernel event for service use.
///
/// Upstream stores a `KEvent*` and calls `m_event->Signal()` / `m_event->Clear()`.
/// The optional `kernel_bridge` allows signaling a kernel KReadableEvent,
/// which wakes threads blocked in WaitSynchronization on the event handle.
pub struct Event {
    signaled: Arc<Mutex<bool>>,
    cv: Arc<Condvar>,
    kernel_bridge: Mutex<Option<KernelEventBridge>>,
}

impl Event {
    pub fn new() -> Self {
        Self {
            signaled: Arc::new(Mutex::new(false)),
            cv: Arc::new(Condvar::new()),
            kernel_bridge: Mutex::new(None),
        }
    }

    /// Create an Event bridged to a kernel KReadableEvent.
    ///
    /// When `signal()` is called, the kernel readable event is also signaled,
    /// waking any threads blocked in WaitSynchronization on this event's handle.
    pub fn new_with_kernel_event(
        readable_event: Arc<Mutex<KReadableEvent>>,
        process: Arc<Mutex<KProcess>>,
        scheduler: Arc<Mutex<KScheduler>>,
    ) -> Self {
        Self {
            signaled: Arc::new(Mutex::new(false)),
            cv: Arc::new(Condvar::new()),
            kernel_bridge: Mutex::new(Some(KernelEventBridge {
                readable_event,
                process,
                scheduler,
            })),
        }
    }

    /// Install a kernel bridge lazily.
    ///
    /// This keeps service owners faithful to upstream `Event` ownership while allowing
    /// the kernel readable-end to be materialized only when an IPC handle is requested.
    pub fn attach_kernel_event(
        &self,
        readable_event: Arc<Mutex<KReadableEvent>>,
        process: Arc<Mutex<KProcess>>,
        scheduler: Arc<Mutex<KScheduler>>,
    ) {
        let mut bridge = self.kernel_bridge.lock().unwrap();
        if bridge.is_none() {
            *bridge = Some(KernelEventBridge {
                readable_event,
                process,
                scheduler,
            });
        }
    }

    pub fn kernel_object_id(&self) -> Option<u64> {
        self.kernel_bridge
            .lock()
            .unwrap()
            .as_ref()
            .map(|bridge| bridge.readable_event.lock().unwrap().object_id)
    }

    /// Signal the event. Wakes all waiters.
    /// Port of upstream `Event::Signal()` → `m_event->Signal()`.
    pub fn signal(&self) {
        let trace_boot = std::env::var_os("RUZU_APPLET_BOOT_TRACE")
            .is_some_and(|value| value != std::ffi::OsStr::new("0"));
        {
            let mut signaled = self.signaled.lock().unwrap();
            *signaled = true;
            self.cv.notify_all();
        }
        // Drop signaled lock before acquiring kernel locks to avoid deadlock.

        // Bridge to kernel: signal the KReadableEvent to wake WaitSynchronization.
        if let Some(ref bridge) = *self.kernel_bridge.lock().unwrap() {
            let readable_object_id = bridge.readable_event.lock().unwrap().object_id;
            if trace_boot {
                log::info!(
                    "Service::Event::signal: bridging readable_object_id={} begin",
                    readable_object_id
                );
            }
            log::trace!(
                "Service::Event::signal bridging readable_object_id={}",
                readable_object_id
            );
            KReadableEvent::signal_from_host_arc(
                &bridge.readable_event,
                &bridge.process,
                &bridge.scheduler,
            );
            if trace_boot {
                log::info!(
                    "Service::Event::signal: bridge signal complete readable_object_id={}",
                    readable_object_id
                );
            }
        }
    }

    /// Clear the event (reset signaled state).
    /// Port of upstream `Event::Clear()` → `m_event->Clear()`.
    pub fn clear(&self) {
        {
            let mut signaled = self.signaled.lock().unwrap();
            *signaled = false;
        }

        // Bridge to kernel: clear the readable end too, matching upstream
        // Event::Clear() -> KEvent::Clear().
        if let Some(ref bridge) = *self.kernel_bridge.lock().unwrap() {
            let process = bridge.process.lock().unwrap();
            bridge.readable_event.lock().unwrap().clear();
            drop(process);
        }
    }

    /// Check if the event is currently signaled.
    pub fn is_signaled(&self) -> bool {
        *self.signaled.lock().unwrap()
    }

    /// Wait for the event to be signaled.
    pub fn wait(&self) {
        let guard = self.signaled.lock().unwrap();
        let _guard = self.cv.wait_while(guard, |s| !*s).unwrap();
    }

    /// Wait for the event with a timeout. Returns true if signaled, false if timed out.
    pub fn wait_timeout(&self, timeout: std::time::Duration) -> bool {
        let guard = self.signaled.lock().unwrap();
        let (guard, _result) = self.cv.wait_timeout_while(guard, timeout, |s| !*s).unwrap();
        *guard
    }
}

impl Default for Event {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn clear_bridges_to_kernel_readable_event() {
        let mut process = KProcess::new();
        let readable = Arc::new(Mutex::new(KReadableEvent::new()));
        readable.lock().unwrap().initialize(1, 2);
        process.register_readable_event_object(2, Arc::clone(&readable));

        let process = Arc::new(Mutex::new(process));
        let scheduler = Arc::new(Mutex::new(KScheduler::new(0)));
        let event = Event::new_with_kernel_event(
            Arc::clone(&readable),
            Arc::clone(&process),
            Arc::clone(&scheduler),
        );

        event.signal();
        assert!(event.is_signaled());
        assert!(readable.lock().unwrap().is_signaled());

        event.clear();
        assert!(!event.is_signaled());
        assert!(!readable.lock().unwrap().is_signaled());
    }
}
