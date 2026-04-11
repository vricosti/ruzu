// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/power_state_service.h
//! Port of zuyu/src/core/hle/service/psc/time/power_state_service.cpp
//!
//! IPowerStateRequestHandler: handles power state requests for "time:p".

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use super::power_state_request_manager::PowerStateRequestManager;
use crate::core::SystemRef;
use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for IPowerStateRequestHandler.
///
/// Corresponds to the function table in upstream power_state_service.cpp constructor.
pub mod commands {
    pub const GET_POWER_STATE_REQUEST_EVENT_READABLE_HANDLE: u32 = 0;
    pub const GET_AND_CLEAR_POWER_STATE_REQUEST: u32 = 1;
}

/// IPowerStateRequestHandler service.
///
/// Corresponds to `IPowerStateRequestHandler` in upstream power_state_service.h.
/// Upstream holds a reference to `PowerStateRequestManager&
/// m_power_state_request_manager` and delegates all operations to it.
pub struct PowerStateRequestHandler {
    system: SystemRef,
    /// Reference to the power state request manager.
    /// Corresponds to `PowerStateRequestManager& m_power_state_request_manager`
    /// in upstream.
    power_state_request_manager: Arc<PowerStateRequestManager>,
    readable_event: Mutex<Option<Arc<Mutex<KReadableEvent>>>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl PowerStateRequestHandler {
    pub fn new(
        system: SystemRef,
        power_state_request_manager: Arc<PowerStateRequestManager>,
    ) -> Self {
        let handlers = build_handler_map(&[
            (
                commands::GET_POWER_STATE_REQUEST_EVENT_READABLE_HANDLE,
                Some(Self::get_power_state_request_event_readable_handle_handler),
                "GetPowerStateRequestEventReadableHandle",
            ),
            (
                commands::GET_AND_CLEAR_POWER_STATE_REQUEST,
                Some(Self::get_and_clear_power_state_request_handler),
                "GetAndClearPowerStateRequest",
            ),
        ]);
        Self {
            system,
            power_state_request_manager,
            readable_event: Mutex::new(None),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// GetPowerStateRequestEventReadableHandle (cmd 0).
    ///
    /// Corresponds to `IPowerStateRequestHandler::GetPowerStateRequestEventReadableHandle`
    /// in upstream power_state_service.cpp.
    /// Upstream returns `&m_power_state_request_manager.GetReadableEvent()`.
    /// We return the event Arc for the caller to hand out as a copy handle.
    pub fn get_power_state_request_event_readable_handle(
        &self,
        ctx: &HLERequestContext,
    ) -> Option<u32> {
        log::debug!("IPowerStateRequestHandler::GetPowerStateRequestEventReadableHandle called");
        if let Some(readable_event) = self.readable_event.lock().unwrap().as_ref() {
            return ctx.copy_handle_for_readable_event(Arc::clone(readable_event));
        }

        let (handle, readable_event) = ctx.create_readable_event(false)?;
        let owner_process = ctx.owner_process_arc()?;
        let scheduler = self.system.get().scheduler_arc();
        self.power_state_request_manager
            .get_event()
            .attach_kernel_event(Arc::clone(&readable_event), owner_process, scheduler);
        *self.readable_event.lock().unwrap() = Some(readable_event);
        Some(handle)
    }

    /// GetAndClearPowerStateRequest (cmd 1).
    ///
    /// Corresponds to `IPowerStateRequestHandler::GetAndClearPowerStateRequest`
    /// in upstream power_state_service.cpp.
    /// Delegates to the PowerStateRequestManager.
    pub fn get_and_clear_power_state_request(&self) -> (bool, u32) {
        log::debug!("IPowerStateRequestHandler::GetAndClearPowerStateRequest called");
        self.power_state_request_manager
            .get_and_clear_power_state_request()
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    fn get_power_state_request_event_readable_handle_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        match service.get_power_state_request_event_readable_handle(ctx) {
            Some(handle) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
                rb.push_result(crate::hle::result::RESULT_SUCCESS);
                rb.push_copy_objects(handle);
            }
            None => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(crate::hle::result::RESULT_SUCCESS);
            }
        }
    }

    fn get_and_clear_power_state_request_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        let (cleared, priority) = service.get_and_clear_power_state_request();
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(crate::hle::result::RESULT_SUCCESS);
        rb.push_bool(cleared);
        if cleared {
            rb.push_u32(priority);
        } else {
            rb.push_u32(0);
        }
    }
}

impl SessionRequestHandler for PowerStateRequestHandler {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "time:p"
    }
}

impl ServiceFramework for PowerStateRequestHandler {
    fn get_service_name(&self) -> &str {
        "time:p"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
