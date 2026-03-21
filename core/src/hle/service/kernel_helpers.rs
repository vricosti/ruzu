// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/kernel_helpers.h and kernel_helpers.cpp
//! Status: COMPLET
//!
//! Contains:
//! - ServiceContext: provides kernel resource management for services (event creation/destruction)
//!
//! Upstream creates real KEvent objects via KernelCore. Here we use the
//! service-layer Event wrapper (os/event.rs) which provides Condvar-based
//! signal/clear/wait semantics matching KEvent behavior.

use std::collections::BTreeMap;
use std::sync::Arc;

use super::os::event::Event;

/// Provides kernel resource management for HLE services.
///
/// Corresponds to upstream `Service::KernelHelpers::ServiceContext`.
///
/// Upstream holds `KernelCore& m_kernel` and `KProcess* m_process`, and creates
/// KEvent objects via `KEvent::Create(kernel)`. We store created events in a map
/// keyed by handle ID, using the service-layer Event wrapper.
pub struct ServiceContext {
    name: String,
    events: BTreeMap<u32, Arc<Event>>,
    next_handle: u32,
}

impl ServiceContext {
    /// Creates a new ServiceContext.
    ///
    /// Upstream: obtains current process from kernel, or creates one if none exists.
    pub fn new(name: String) -> Self {
        Self {
            name,
            events: BTreeMap::new(),
            next_handle: 1, // 0 is reserved as invalid
        }
    }

    /// Returns the name of this service context.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Creates a new kernel event and returns its handle.
    ///
    /// Upstream: reserves event from process resource limit via
    /// KScopedResourceReservation, creates KEvent via KEvent::Create(kernel),
    /// initializes it, registers it, and returns the event.
    pub fn create_event(&mut self, name: String) -> u32 {
        let handle = self.next_handle;
        self.next_handle += 1;

        let event = Arc::new(Event::new());
        self.events.insert(handle, event);

        log::debug!(
            "ServiceContext '{}': created event '{}' with handle {}",
            self.name,
            name,
            handle,
        );

        handle
    }

    /// Get a reference to a created event by handle.
    pub fn get_event(&self, handle: u32) -> Option<Arc<Event>> {
        self.events.get(&handle).cloned()
    }

    /// Closes a kernel event.
    ///
    /// Upstream: closes the readable event and the event itself.
    pub fn close_event(&mut self, event_handle: u32) {
        if self.events.remove(&event_handle).is_some() {
            log::debug!(
                "ServiceContext '{}': closed event handle {}",
                self.name,
                event_handle,
            );
        }
    }
}

impl Drop for ServiceContext {
    fn drop(&mut self) {
        // Close all remaining events.
        let handles: Vec<u32> = self.events.keys().copied().collect();
        for handle in handles {
            self.close_event(handle);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_service_context_creation() {
        let ctx = ServiceContext::new("test_service".to_string());
        assert_eq!(ctx.name(), "test_service");
    }

    #[test]
    fn test_create_and_close_event() {
        let mut ctx = ServiceContext::new("test".to_string());
        let handle = ctx.create_event("my_event".to_string());
        assert!(handle > 0);
        assert!(ctx.get_event(handle).is_some());

        ctx.close_event(handle);
        assert!(ctx.get_event(handle).is_none());
    }

    #[test]
    fn test_event_signal_clear() {
        let mut ctx = ServiceContext::new("test".to_string());
        let handle = ctx.create_event("sig_event".to_string());
        let event = ctx.get_event(handle).unwrap();

        assert!(!event.is_signaled());
        event.signal();
        assert!(event.is_signaled());
        event.clear();
        assert!(!event.is_signaled());
    }
}
