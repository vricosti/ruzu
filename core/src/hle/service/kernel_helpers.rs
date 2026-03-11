// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/kernel_helpers.h and kernel_helpers.cpp
//! Status: Structural stub
//!
//! Contains:
//! - ServiceContext: provides kernel resource management for services (event creation/destruction)
//!
//! The upstream implementation interacts deeply with the kernel (KEvent, KProcess,
//! KScopedResourceReservation). This is stubbed until kernel integration is wired up.

/// Provides kernel resource management for HLE services.
///
/// Corresponds to upstream `Service::KernelHelpers::ServiceContext`.
///
/// This struct manages the lifecycle of kernel events and other resources needed by services.
/// In the full implementation, it holds a reference to KernelCore and a process context.
pub struct ServiceContext {
    name: String,
    // TODO: kernel: reference to KernelCore
    // TODO: process: reference to KProcess
    // TODO: process_created: bool
}

impl ServiceContext {
    /// Creates a new ServiceContext.
    ///
    /// In the full implementation this would obtain the current process from the kernel,
    /// or create a new one if none exists.
    pub fn new(name: String) -> Self {
        // TODO: obtain current process from kernel, or create one
        Self { name }
    }

    /// Returns the name of this service context.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Creates a new kernel event.
    ///
    /// In the full implementation this reserves an event from the process resource limit,
    /// creates a KEvent, initializes it, and returns it.
    ///
    /// Returns a handle (stub returns 0 for now).
    pub fn create_event(&self, _name: String) -> u32 {
        // TODO: implement when kernel integration is ready
        log::warn!(
            "ServiceContext::create_event stubbed for context '{}'",
            self.name
        );
        0
    }

    /// Closes a kernel event.
    ///
    /// In the full implementation this closes the readable event and the event itself.
    pub fn close_event(&self, _event_handle: u32) {
        // TODO: implement when kernel integration is ready
        log::warn!(
            "ServiceContext::close_event stubbed for context '{}'",
            self.name
        );
    }
}

impl Drop for ServiceContext {
    fn drop(&mut self) {
        // TODO: if process_created, close the process
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
}
