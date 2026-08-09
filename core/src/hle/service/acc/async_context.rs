// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/acc/async_context.h
//! Port of zuyu/src/core/hle/service/acc/async_context.cpp

use std::collections::BTreeMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::os::event::Event;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for IAsyncContext
pub mod commands {
    pub const GET_SYSTEM_EVENT: u32 = 0;
    pub const CANCEL: u32 = 1;
    pub const HAS_DONE: u32 = 2;
    pub const GET_RESULT: u32 = 3;
}

/// IAsyncContext base for async account operations.
///
/// Corresponds to `IAsyncContext` in upstream `async_context.h`.
pub trait AsyncContext: Send + Sync + 'static {
    fn is_complete(&self) -> bool;
    fn cancel(&self);
    fn get_result(&self) -> ResultCode;
}

/// Base implementation shared by all IAsyncContext types.
pub struct AsyncContextBase<T: AsyncContext> {
    implementation: T,
    is_complete: AtomicBool,
    /// Completion event, signaled when the async operation finishes.
    /// Upstream: `Kernel::KEvent* m_event`.
    completion_event: Arc<Event>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl<T: AsyncContext> AsyncContextBase<T> {
    pub fn new(implementation: T) -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::get_system_event_handler), "GetSystemEvent"),
            (1, Some(Self::cancel_handler), "Cancel"),
            (2, Some(Self::has_done_handler), "HasDone"),
            (3, Some(Self::get_result_handler), "GetResult"),
        ]);

        Self {
            implementation,
            is_complete: AtomicBool::new(false),
            completion_event: Arc::new(Event::new()),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub(crate) fn mark_complete(&self) {
        self.is_complete.store(true, Ordering::SeqCst);
        self.completion_event.signal();
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    fn get_system_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAsyncContext::GetSystemEvent called");
        let service = Self::as_self(this);
        let object_id = service.completion_event.copy_object_id(ctx).unwrap_or(0);
        let mut response = ResponseBuilder::new(ctx, 2, 1, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_copy_object_id(object_id);
    }

    fn cancel_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAsyncContext::Cancel called");
        let service = Self::as_self(this);
        service.implementation.cancel();
        service.mark_complete();
        let mut response = ResponseBuilder::new(ctx, 2, 0, 0);
        response.push_result(RESULT_SUCCESS);
    }

    fn has_done_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAsyncContext::HasDone called");
        let service = Self::as_self(this);
        service
            .is_complete
            .store(service.implementation.is_complete(), Ordering::SeqCst);
        let mut response = ResponseBuilder::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_bool(service.is_complete.load(Ordering::SeqCst));
    }

    fn get_result_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAsyncContext::GetResult called");
        let service = Self::as_self(this);
        let mut response = ResponseBuilder::new(ctx, 3, 0, 0);
        response.push_result(service.implementation.get_result());
    }
}

impl<T: AsyncContext> SessionRequestHandler for AsyncContextBase<T> {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "IAsyncContext"
    }
}

impl<T: AsyncContext> ServiceFramework for AsyncContextBase<T> {
    fn get_service_name(&self) -> &str {
        "IAsyncContext"
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

    struct CompleteContext;

    impl AsyncContext for CompleteContext {
        fn is_complete(&self) -> bool {
            true
        }

        fn cancel(&self) {}

        fn get_result(&self) -> ResultCode {
            RESULT_SUCCESS
        }
    }

    #[test]
    fn upstream_handlers_are_owned_by_async_context_base() {
        let context = AsyncContextBase::new(CompleteContext);
        for command_id in 0..=3 {
            assert!(context
                .handlers
                .get(&command_id)
                .unwrap()
                .handler_callback
                .is_some());
        }
    }
}
