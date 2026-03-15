// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/sm/sm_controller.h and sm_controller.cpp
//! Status: Structural port
//!
//! Contains:
//! - Controller: the "IpcController" service that handles domain conversion, session cloning,
//!   and pointer buffer size queries.

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{ResponseBuilder, ResponseBuilderFlags};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC Controller service.
///
/// Corresponds to upstream `Service::SM::Controller`.
/// Handles domain conversion, session cloning, and pointer buffer queries.
///
/// https://switchbrew.org/wiki/IPC_Marshalling
pub struct Controller {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl Controller {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (
                0,
                Some(Controller::convert_current_object_to_domain_handler),
                "ConvertCurrentObjectToDomain",
            ),
            (1, None, "CopyFromCurrentDomain"),
            (
                2,
                Some(Controller::clone_current_object_handler),
                "CloneCurrentObject",
            ),
            (
                3,
                Some(Controller::query_pointer_buffer_size_handler),
                "QueryPointerBufferSize",
            ),
            (
                4,
                Some(Controller::clone_current_object_ex_handler),
                "CloneCurrentObjectEx",
            ),
        ]);
        let handlers_tipc = BTreeMap::new();

        Self {
            handlers,
            handlers_tipc,
        }
    }

    // --- Handler trampolines ---

    fn convert_current_object_to_domain_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let controller =
            unsafe { &*(this as *const dyn ServiceFramework as *const Controller) };
        controller.convert_current_object_to_domain(ctx);
    }

    fn clone_current_object_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let controller =
            unsafe { &*(this as *const dyn ServiceFramework as *const Controller) };
        controller.clone_current_object(ctx);
    }

    fn query_pointer_buffer_size_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let controller =
            unsafe { &*(this as *const dyn ServiceFramework as *const Controller) };
        controller.query_pointer_buffer_size(ctx);
    }

    fn clone_current_object_ex_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let controller =
            unsafe { &*(this as *const dyn ServiceFramework as *const Controller) };
        controller.clone_current_object_ex(ctx);
    }

    // --- Actual handler implementations ---

    /// Converts the current session object to a domain.
    fn convert_current_object_to_domain(&self, ctx: &mut HLERequestContext) {
        if let Some(manager) = ctx.get_manager() {
            let mgr = manager.lock().unwrap();
            assert!(!mgr.is_domain(), "Session is already a domain");
        }
        log::debug!("Controller::ConvertCurrentObjectToDomain called");

        if let Some(manager) = ctx.get_manager() {
            manager
                .lock()
                .unwrap()
                .convert_to_domain_on_request_end();
        }

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(1); // Converted sessions start with 1 request handler
    }

    /// Clones the current session object.
    ///
    /// Matches upstream: creates a new KSession, copies the session handler
    /// from the current manager, and pushes the client session as a move handle.
    fn clone_current_object(&self, ctx: &mut HLERequestContext) {
        log::debug!("Controller::CloneCurrentObject called");

        // Clone the current session's handler into a new client session.
        let handler = ctx.get_manager()
            .and_then(|m| m.lock().unwrap().session_handler().cloned());

        let session_handle = if let Some(handler) = handler {
            ctx.create_session_for_service(handler).unwrap_or(0)
        } else {
            log::warn!("CloneCurrentObject: no session handler to clone");
            0
        };

        let mut rb = ResponseBuilder::new_with_flags(
            ctx, 2, 0, 1,
            ResponseBuilderFlags::AlwaysMoveHandles,
        );
        rb.push_result(RESULT_SUCCESS);
        rb.push_move_objects(session_handle);
    }

    /// Clones the current session object (extended variant).
    fn clone_current_object_ex(&self, ctx: &mut HLERequestContext) {
        log::debug!("Controller::CloneCurrentObjectEx called");
        self.clone_current_object(ctx);
    }

    /// Queries the pointer buffer size.
    fn query_pointer_buffer_size(&self, ctx: &mut HLERequestContext) {
        log::warn!("Controller::QueryPointerBufferSize (STUBBED) called");

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u16(0x8000);
    }
}

impl SessionRequestHandler for Controller {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        self.handle_sync_request_impl(ctx)
    }

    fn service_name(&self) -> &str {
        "IpcController"
    }
}

impl ServiceFramework for Controller {
    fn get_service_name(&self) -> &str {
        "IpcController"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_controller_creation() {
        let controller = Controller::new();
        assert_eq!(controller.get_service_name(), "IpcController");
        assert_eq!(controller.handlers().len(), 5);
    }

    #[test]
    fn test_query_pointer_buffer_size() {
        let controller = Controller::new();
        let mut ctx = HLERequestContext::new();
        controller.query_pointer_buffer_size(&mut ctx);
        // Basic smoke test: should not panic.
    }
}
