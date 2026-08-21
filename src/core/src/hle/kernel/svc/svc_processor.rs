//! Port of zuyu/src/core/hle/kernel/svc/svc_processor.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-20
//!
//! SVC handler for GetCurrentProcessorNumber.

use crate::core::System;

/// Get which CPU core is executing the current thread.
pub fn get_current_processor_number(system: &System) -> i32 {
    log::trace!("svc::GetCurrentProcessorNumber called");

    // Upstream: return static_cast<int32_t>(system.CurrentPhysicalCore().CoreIndex());
    match system.kernel() {
        Some(kernel) => kernel.current_physical_core_index() as i32,
        None => 0,
    }
}
