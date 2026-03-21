// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/lock_accessor.h
//! Port of zuyu/src/core/hle/service/am/service/lock_accessor.cpp

use std::collections::BTreeMap;
use std::sync::Mutex;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for ILockAccessor:
/// - 1: TryLock
/// - 2: Unlock
/// - 3: GetEvent
/// - 4: IsLocked
pub struct ILockAccessor {
    /// Matches upstream `bool m_is_locked`.
    is_locked: Mutex<bool>,
    // TODO: ServiceContext, Event, for TryLock and GetEvent
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ILockAccessor {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (1, None, "TryLock"),   // Needs event handle support
            (2, Some(Self::unlock_handler), "Unlock"),
            (3, None, "GetEvent"),  // Needs event handle support
            (4, Some(Self::is_locked_handler), "IsLocked"),
        ]);
        Self {
            is_locked: Mutex::new(false),
            handlers,
            handlers_tipc: BTreeMap::new(),
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

        // TODO: m_event.Signal() when event support is available

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
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
