// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/acc/async_context.h
//! Port of zuyu/src/core/hle/service/acc/async_context.cpp

use std::collections::BTreeMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
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
pub trait AsyncContext {
    fn is_complete(&self) -> bool;
    fn cancel(&mut self);
    fn get_result(&self) -> ResultCode;
}

/// Base implementation shared by all IAsyncContext types.
pub struct AsyncContextBase {
    pub is_complete: AtomicBool,
    /// Completion event, signaled when the async operation finishes.
    /// Upstream: `Kernel::KEvent* m_event`.
    pub completion_event: Arc<Event>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl AsyncContextBase {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, None, "GetSystemEvent"),
            (1, None, "Cancel"),
            (2, None, "HasDone"),
            (3, None, "GetResult"),
        ]);

        Self {
            is_complete: AtomicBool::new(false),
            completion_event: Arc::new(Event::new()),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn mark_complete(&self) {
        self.is_complete.store(true, Ordering::Release);
        self.completion_event.signal();
    }

    pub fn has_done(&self) -> bool {
        self.is_complete.load(Ordering::Acquire)
    }

    pub fn get_system_event(&self) -> ResultCode {
        log::debug!("IAsyncContext::get_system_event called");
        // Upstream: returns the readable event handle.
        // The event itself is available via self.completion_event.
        RESULT_SUCCESS
    }

    /// Get a reference to the completion event (for IPC handle return).
    pub fn get_event(&self) -> Arc<Event> {
        self.completion_event.clone()
    }
}

impl SessionRequestHandler for AsyncContextBase {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "acc::IAsyncContext"
    }
}

impl ServiceFramework for AsyncContextBase {
    fn get_service_name(&self) -> &str {
        "acc::IAsyncContext"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
