// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/acc/async_context.h
//! Port of zuyu/src/core/hle/service/acc/async_context.cpp

use std::sync::atomic::{AtomicBool, Ordering};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

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
    // TODO: completion_event: KEvent, service_context
}

impl AsyncContextBase {
    pub fn new() -> Self {
        Self {
            is_complete: AtomicBool::new(false),
        }
    }

    pub fn mark_complete(&self) {
        self.is_complete.store(true, Ordering::Release);
        // TODO: signal completion_event
    }

    pub fn has_done(&self) -> bool {
        self.is_complete.load(Ordering::Acquire)
    }

    pub fn get_system_event(&self) -> ResultCode {
        log::debug!("IAsyncContext::get_system_event called");
        // TODO: return completion_event readable event
        RESULT_SUCCESS
    }
}
