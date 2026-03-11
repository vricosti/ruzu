//! Port of zuyu/src/core/hle/kernel/svc/svc_tick.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handler for GetSystemTick.

/// Returns the total CPU ticks elapsed since the CPU was powered-on.
///
/// Returns the value of cntpct_el0 (https://switchbrew.org/wiki/SVC#svcGetSystemTick).
pub fn get_system_tick() -> i64 {
    log::trace!("svc::GetSystemTick called");

    // TODO: system.CoreTiming().GetClockTicks() as i64
    log::warn!("svc::GetSystemTick: kernel timing not yet implemented");
    0
}
