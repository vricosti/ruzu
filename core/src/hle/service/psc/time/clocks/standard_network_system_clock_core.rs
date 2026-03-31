// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/clocks/standard_network_system_clock_core.h/.cpp
//!
//! StandardNetworkSystemClockCore: network system clock with accuracy tracking.

use super::system_clock_core::SystemClockCore;
use crate::hle::result::ResultCode;
use crate::hle::service::psc::time::common::{SteadyClockTimePoint, SystemClockContext};

/// 10 days in nanoseconds (default sufficient accuracy).
/// Matches upstream: std::chrono::duration_cast<nanoseconds>(days(10)).count()
const DEFAULT_SUFFICIENT_ACCURACY_NS: i64 = 10 * 24 * 60 * 60 * 1_000_000_000;

/// StandardNetworkSystemClockCore wraps a SystemClockCore with network-clock-specific
/// accuracy checking.
pub struct StandardNetworkSystemClockCore {
    pub clock: SystemClockCore,
    sufficient_accuracy: i64,
}

impl StandardNetworkSystemClockCore {
    pub fn new(
        get_time_point: Box<dyn Fn() -> Result<SteadyClockTimePoint, ResultCode> + Send + Sync>,
    ) -> Self {
        Self {
            clock: SystemClockCore::new(get_time_point),
            sufficient_accuracy: DEFAULT_SUFFICIENT_ACCURACY_NS,
        }
    }

    /// Initialize the network system clock.
    /// Sets the context and updates sufficient accuracy.
    /// Matches upstream StandardNetworkSystemClockCore::Initialize.
    pub fn initialize(&mut self, context: &SystemClockContext, accuracy: i64) {
        if self.clock.set_context_and_write(context) != crate::hle::result::RESULT_SUCCESS {
            log::error!("StandardNetworkSystemClockCore: Failed to SetContext");
        }
        self.sufficient_accuracy = accuracy;
        self.clock.set_initialized();
    }

    /// Check if the network clock's accuracy is within the sufficient threshold.
    /// Matches upstream StandardNetworkSystemClockCore::IsAccuracySufficient.
    pub fn is_accuracy_sufficient(&self) -> bool {
        if !self.clock.is_initialized() {
            return false;
        }

        let current_time_point = match self.clock.get_current_time_point() {
            Ok(tp) => tp,
            Err(_) => return false,
        };
        let context = match self.clock.get_context() {
            Ok(c) => c,
            Err(_) => return false,
        };

        let span = crate::hle::service::psc::time::common::get_span_between_time_points(
            &context.steady_time_point,
            &current_time_point,
        );

        match span {
            Some(seconds) => {
                // Convert seconds to nanoseconds and compare against sufficient_accuracy
                let ns = seconds * 1_000_000_000;
                ns < self.sufficient_accuracy
            }
            None => false,
        }
    }
}
