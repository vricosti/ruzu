// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/power_state_service.h
//! Port of zuyu/src/core/hle/service/psc/time/power_state_service.cpp
//!
//! IPowerStateRequestHandler: handles power state requests for "time:p".

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

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
pub struct PowerStateRequestHandler {
    /// Whether a power state request is pending.
    pending: bool,
    /// Priority of the pending request.
    priority: u32,
}

impl PowerStateRequestHandler {
    pub fn new() -> Self {
        Self {
            pending: false,
            priority: 0,
        }
    }

    /// GetPowerStateRequestEventReadableHandle (cmd 0).
    ///
    /// Corresponds to `IPowerStateRequestHandler::GetPowerStateRequestEventReadableHandle`
    /// in upstream power_state_service.cpp.
    pub fn get_power_state_request_event_readable_handle(&self) -> ResultCode {
        log::debug!("IPowerStateRequestHandler::GetPowerStateRequestEventReadableHandle called");
        // TODO: Return the readable event from power_state_request_manager
        RESULT_SUCCESS
    }

    /// GetAndClearPowerStateRequest (cmd 1).
    ///
    /// Corresponds to `IPowerStateRequestHandler::GetAndClearPowerStateRequest`
    /// in upstream power_state_service.cpp.
    pub fn get_and_clear_power_state_request(&mut self) -> (bool, u32) {
        log::debug!("IPowerStateRequestHandler::GetAndClearPowerStateRequest called");
        if self.pending {
            let priority = self.priority;
            self.pending = false;
            self.priority = 0;
            (true, priority)
        } else {
            (false, 0)
        }
    }

    /// Set a pending power state request (used by the power state request manager).
    pub fn set_power_state_request(&mut self, priority: u32) {
        self.pending = true;
        self.priority = priority;
    }
}
