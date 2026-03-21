// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/lock_accessor.h
//! Port of zuyu/src/core/hle/service/am/service/lock_accessor.cpp

use std::collections::BTreeMap;
use std::sync::Mutex;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for ILockAccessor:
/// - 1: TryLock
/// - 2: Unlock
/// - 3: GetEvent
/// - 4: IsLocked
pub struct ILockAccessor {
    /// Matches upstream `bool m_is_locked`.
    is_locked: Mutex<bool>,
    service_context: crate::hle::service::kernel_helpers::ServiceContext,
    event_handle: u32,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ILockAccessor {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (1, Some(Self::try_lock_handler), "TryLock"),
            (2, Some(Self::unlock_handler), "Unlock"),
            (3, Some(Self::get_event_handler), "GetEvent"),
            (4, Some(Self::is_locked_handler), "IsLocked"),
        ]);
        let mut service_context = crate::hle::service::kernel_helpers::ServiceContext::new(
            "ILockAccessor".to_string(),
        );
        let event_handle = service_context.create_event(
            "ILockAccessor:Event".to_string(),
        );
        // Upstream signals the event in the constructor.
        if let Some(event) = service_context.get_event(event_handle) {
            event.signal();
        }
        Self {
            is_locked: Mutex::new(false),
            service_context,
            event_handle,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Port of ILockAccessor::TryLock
    /// Upstream: takes `bool return_handle`, returns `Out<bool> out_is_locked` and
    /// optionally `OutCopyHandle<KReadableEvent> out_handle`.
    fn try_lock_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let accessor = unsafe { &*(this as *const dyn ServiceFramework as *const ILockAccessor) };
        let mut rp = RequestParser::new(ctx);
        let return_handle = rp.pop_bool();
        log::info!("ILockAccessor::TryLock called, return_handle={}", return_handle);

        let is_locked;
        {
            let mut locked = accessor.is_locked.lock().unwrap();
            if *locked {
                is_locked = false;
            } else {
                *locked = true;
                is_locked = true;
            }
        }

        if return_handle {
            if let Some(handle) = ctx.create_readable_event_handle(false) {
                let mut rb = ResponseBuilder::new(ctx, 3, 1, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_bool(is_locked);
                rb.push_copy_objects(handle);
            }
        } else {
            let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
            rb.push_result(RESULT_SUCCESS);
            rb.push_bool(is_locked);
        }
    }

    /// Port of ILockAccessor::Unlock
    fn unlock_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let accessor = unsafe { &*(this as *const dyn ServiceFramework as *const ILockAccessor) };
        log::info!("ILockAccessor::Unlock called");

        {
            let mut locked = accessor.is_locked.lock().unwrap();
            *locked = false;
        }

        // Upstream: m_event.Signal()
        if let Some(event) = accessor.service_context.get_event(accessor.event_handle) {
            event.signal();
        }

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Port of ILockAccessor::GetEvent
    /// Upstream returns m_event.GetHandle() as a copy handle.
    fn get_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let _accessor = unsafe { &*(this as *const dyn ServiceFramework as *const ILockAccessor) };
        log::info!("ILockAccessor::GetEvent called");

        if let Some(handle) = ctx.create_readable_event_handle(false) {
            let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
            rb.push_result(RESULT_SUCCESS);
            rb.push_copy_objects(handle);
        }
    }

    /// Port of ILockAccessor::IsLocked
    fn is_locked_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let accessor = unsafe { &*(this as *const dyn ServiceFramework as *const ILockAccessor) };
        let locked = *accessor.is_locked.lock().unwrap();
        log::info!("ILockAccessor::IsLocked called, is_locked={}", locked);

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(locked);
    }
}

impl SessionRequestHandler for ILockAccessor {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
}

impl ServiceFramework for ILockAccessor {
    fn get_service_name(&self) -> &str {
        "am::ILockAccessor"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
