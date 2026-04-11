// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/lock_accessor.h
//! Port of zuyu/src/core/hle/service/am/service/lock_accessor.cpp

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex, Weak};

use crate::hle::kernel::k_event::KEvent;
use crate::hle::kernel::k_process::KProcess;
use crate::hle::kernel::k_readable_event::KReadableEvent;
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
    owner_process: Weak<Mutex<KProcess>>,
    event_object_id: u64,
    readable_event_object_id: u64,
    event: Arc<Mutex<KEvent>>,
    readable_event: Arc<Mutex<KReadableEvent>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ILockAccessor {
    pub fn new(owner_process: Arc<Mutex<KProcess>>) -> Self {
        use std::sync::atomic::{AtomicU64, Ordering};

        static NEXT_LOCK_ACCESSOR_EVENT_OBJECT_ID: AtomicU64 = AtomicU64::new(0x2300_0000);

        let handlers = build_handler_map(&[
            (1, Some(Self::try_lock_handler), "TryLock"),
            (2, Some(Self::unlock_handler), "Unlock"),
            (3, Some(Self::get_event_handler), "GetEvent"),
            (4, Some(Self::is_locked_handler), "IsLocked"),
        ]);

        let event_object_id = NEXT_LOCK_ACCESSOR_EVENT_OBJECT_ID.fetch_add(1, Ordering::Relaxed);
        let readable_event_object_id =
            NEXT_LOCK_ACCESSOR_EVENT_OBJECT_ID.fetch_add(1, Ordering::Relaxed);

        let mut event = KEvent::new();
        let mut readable_event = KReadableEvent::new();

        let owner_process_id = owner_process.lock().unwrap().get_process_id();
        event.initialize(owner_process_id, readable_event_object_id);
        readable_event.initialize(event_object_id, readable_event_object_id);
        readable_event.is_signaled = true;

        let event = Arc::new(Mutex::new(event));
        let readable_event = Arc::new(Mutex::new(readable_event));

        {
            let mut process = owner_process.lock().unwrap();
            process.register_event_object(event_object_id, Arc::clone(&event));
            process.register_readable_event_object(
                readable_event_object_id,
                Arc::clone(&readable_event),
            );
        }

        Self {
            is_locked: Mutex::new(false),
            owner_process: Arc::downgrade(&owner_process),
            event_object_id,
            readable_event_object_id,
            event,
            readable_event,
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
        log::info!(
            "ILockAccessor::TryLock called, return_handle={}",
            return_handle
        );

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

        let handle = if return_handle {
            Some(
                ctx.copy_handle_for_readable_event(Arc::clone(&accessor.readable_event))
                    .unwrap_or(0),
            )
        } else {
            None
        };

        let mut rb = ResponseBuilder::new(ctx, 3, if return_handle { 1 } else { 0 }, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(is_locked);

        if let Some(handle) = handle {
            rb.push_copy_objects(handle);
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

        if let Some(owner_process) = accessor.owner_process.upgrade() {
            let mut process = owner_process.lock().unwrap();
            let scheduler = process.scheduler.as_ref().and_then(|weak| weak.upgrade());
            if let Some(scheduler) = scheduler {
                accessor
                    .event
                    .lock()
                    .unwrap()
                    .signal(&mut process, &scheduler);
            } else {
                accessor.readable_event.lock().unwrap().is_signaled = true;
            }
        }

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Port of ILockAccessor::GetEvent
    /// Upstream returns m_event.GetHandle() as a copy handle.
    fn get_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let accessor = unsafe { &*(this as *const dyn ServiceFramework as *const ILockAccessor) };
        log::info!("ILockAccessor::GetEvent called");

        let handle = ctx
            .copy_handle_for_readable_event(Arc::clone(&accessor.readable_event))
            .unwrap_or(0);
        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(handle);
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hle::kernel::k_scheduler::KScheduler;

    #[test]
    fn constructor_creates_persistent_signaled_readable_event() {
        let owner_process = Arc::new(Mutex::new(KProcess::new()));
        owner_process
            .lock()
            .unwrap()
            .attach_scheduler(&Arc::new(Mutex::new(KScheduler::new(0))));

        let accessor = ILockAccessor::new(Arc::clone(&owner_process));

        assert!(owner_process
            .lock()
            .unwrap()
            .get_event_by_object_id(accessor.event_object_id)
            .is_some());
        assert!(owner_process
            .lock()
            .unwrap()
            .get_readable_event_by_object_id(accessor.readable_event_object_id)
            .is_some());
        assert!(accessor.readable_event.lock().unwrap().is_signaled);
    }
}
