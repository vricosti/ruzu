//! Port of zuyu/src/core/hle/kernel/svc/svc_tick.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-20
//!
//! SVC handler for GetSystemTick.

use crate::core::System;

/// Returns the total CPU ticks elapsed since the CPU was powered-on.
///
/// Returns the value of cntpct_el0 (https://switchbrew.org/wiki/SVC#svcGetSystemTick).
pub fn get_system_tick(system: &System) -> i64 {
    log::trace!("svc::GetSystemTick called");

    // Upstream: return static_cast<int64_t>(system.CoreTiming().GetClockTicks());
    system.core_timing().lock().unwrap().get_clock_ticks() as i64
}
