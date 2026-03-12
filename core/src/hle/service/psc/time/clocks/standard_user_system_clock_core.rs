// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/clocks/standard_user_system_clock_core.h/.cpp
//!
//! StandardUserSystemClockCore: user-facing system clock that coordinates between
//! local and network system clocks.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::psc::time::common::{SteadyClockTimePoint, SystemClockContext};
use crate::hle::service::psc::time::errors::RESULT_NOT_IMPLEMENTED;
use super::standard_local_system_clock_core::StandardLocalSystemClockCore;
use super::standard_network_system_clock_core::StandardNetworkSystemClockCore;

/// StandardUserSystemClockCore coordinates between local and network clocks.
///
/// Upstream holds references to both clocks and a KEvent for signaling.
/// Here we store indices/callbacks instead of direct references, since
/// the clocks are owned by the TimeManager.
pub struct StandardUserSystemClockCore {
    automatic_correction: bool,
    time_point: SteadyClockTimePoint,
    // TODO: Kernel::KEvent for signaling when wired to kernel
}

impl StandardUserSystemClockCore {
    pub fn new() -> Self {
        Self {
            automatic_correction: false,
            time_point: SteadyClockTimePoint::default(),
        }
    }

    pub fn get_automatic_correction(&self) -> bool {
        self.automatic_correction
    }

    /// Set automatic correction mode.
    ///
    /// If enabling and the network clock source matches, copies the network
    /// context to the local clock. This requires access to both clocks,
    /// passed as mutable references.
    pub fn set_automatic_correction(
        &mut self,
        automatic_correction: bool,
        local: &mut StandardLocalSystemClockCore,
        network: &StandardNetworkSystemClockCore,
    ) -> ResultCode {
        if self.automatic_correction == automatic_correction {
            return RESULT_SUCCESS;
        }
        if !network.clock.check_clock_source_matches() {
            return RESULT_SUCCESS;
        }

        let context = match network.clock.get_context() {
            Ok(c) => c,
            Err(e) => return e,
        };
        let rc = local.clock.set_context_and_write(&context);
        if rc != RESULT_SUCCESS {
            return rc;
        }

        self.automatic_correction = automatic_correction;
        RESULT_SUCCESS
    }

    /// Get the system clock context.
    ///
    /// If automatic correction is enabled and the network clock matches,
    /// copies network context to local, then returns local context.
    /// Otherwise returns local context directly.
    pub fn get_context(
        &self,
        local: &mut StandardLocalSystemClockCore,
        network: &StandardNetworkSystemClockCore,
    ) -> Result<SystemClockContext, ResultCode> {
        if !self.automatic_correction {
            return local.clock.get_context();
        }

        if !network.clock.check_clock_source_matches() {
            return local.clock.get_context();
        }

        let context = network.clock.get_context()?;
        let rc = local.clock.set_context_and_write(&context);
        if rc != RESULT_SUCCESS {
            return Err(rc);
        }

        local.clock.get_context()
    }

    /// SetContext is not implemented (returns ResultNotImplemented), matching upstream.
    pub fn set_context(&mut self, _context: &SystemClockContext) -> ResultCode {
        RESULT_NOT_IMPLEMENTED
    }

    pub fn get_time_point(&self) -> SteadyClockTimePoint {
        self.time_point
    }

    pub fn set_time_point_and_signal(&mut self, time_point: &SteadyClockTimePoint) {
        self.time_point = *time_point;
        // TODO: m_event->Signal() when KEvent is wired
    }
}
