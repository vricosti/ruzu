// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/power_state_service.h
//! Port of zuyu/src/core/hle/service/psc/time/power_state_service.cpp
//!
//! IPowerStateRequestHandler: handles power state requests for "time:p".

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::os::event::Event;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use super::power_state_request_manager::PowerStateRequestManager;

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
    /// Reference to the power state request manager.
    /// Corresponds to `PowerStateRequestManager& m_power_state_request_manager`
    /// in upstream.
    power_state_request_manager: Arc<PowerStateRequestManager>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl PowerStateRequestHandler {
    pub fn new(power_state_request_manager: Arc<PowerStateRequestManager>) -> Self {
        let handlers = build_handler_map(&[
            (commands::GET_POWER_STATE_REQUEST_EVENT_READABLE_HANDLE, None, "GetPowerStateRequestEventReadableHandle"),
            (commands::GET_AND_CLEAR_POWER_STATE_REQUEST, None, "GetAndClearPowerStateRequest"),
        ]);
        Self {
            power_state_request_manager,
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
    pub fn get_power_state_request_event_readable_handle(&self) -> Arc<Event> {
        log::debug!("IPowerStateRequestHandler::GetPowerStateRequestEventReadableHandle called");
        self.power_state_request_manager.get_event()
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
