// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/clocks/standard_local_system_clock_core.h/.cpp
//!
//! StandardLocalSystemClockCore: local system clock, derived from SystemClockCore.

use crate::hle::result::ResultCode;
use crate::hle::service::psc::time::common::{SteadyClockTimePoint, SystemClockContext};
use super::system_clock_core::SystemClockCore;

/// StandardLocalSystemClockCore wraps a SystemClockCore with local-clock-specific
/// initialization logic.
pub struct StandardLocalSystemClockCore {
    pub clock: SystemClockCore,
}

impl StandardLocalSystemClockCore {
    pub fn new(
        get_time_point: Box<
            dyn Fn() -> Result<SteadyClockTimePoint, ResultCode> + Send + Sync,
        >,
    ) -> Self {
        Self {
            clock: SystemClockCore::new(get_time_point),
        }
    }

    /// Initialize the local system clock.
    ///
    /// If the context's steady time point matches the current time point, set the context.
    /// Otherwise, set the current time from the provided time value.
    /// Matches upstream StandardLocalSystemClockCore::Initialize.
    pub fn initialize(&mut self, context: &SystemClockContext, time: i64) {
        let time_point = self.clock.get_current_time_point();
        if let Ok(tp) = time_point {
            if context.steady_time_point.id_matches(&tp) {
                let _ = self.clock.set_context_and_write(context);
            } else if self.clock.set_current_time(time) != crate::hle::result::RESULT_SUCCESS {
                log::error!("StandardLocalSystemClockCore: Failed to SetCurrentTime");
            }
        } else if self.clock.set_current_time(time) != crate::hle::result::RESULT_SUCCESS {
            log::error!("StandardLocalSystemClockCore: Failed to SetCurrentTime");
        }

        self.clock.set_initialized();
    }
}
