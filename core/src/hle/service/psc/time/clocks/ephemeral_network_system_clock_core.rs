// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/clocks/ephemeral_network_system_clock_core.h
//!
//! EphemeralNetworkSystemClockCore: an ephemeral network system clock.
//! Upstream has no additional methods beyond SystemClockCore.

use crate::hle::result::ResultCode;
use crate::hle::service::psc::time::common::SteadyClockTimePoint;
use super::system_clock_core::SystemClockCore;

/// EphemeralNetworkSystemClockCore wraps SystemClockCore with no additional behavior.
/// Upstream: `class EphemeralNetworkSystemClockCore : public SystemClockCore { ... };`
/// with only a constructor that forwards to SystemClockCore.
pub struct EphemeralNetworkSystemClockCore {
    pub clock: SystemClockCore,
}

impl EphemeralNetworkSystemClockCore {
    pub fn new(
        get_time_point: Box<
            dyn Fn() -> Result<SteadyClockTimePoint, ResultCode> + Send + Sync,
        >,
    ) -> Self {
        Self {
            clock: SystemClockCore::new(get_time_point),
        }
    }
}
