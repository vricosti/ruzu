//! Port of zuyu/src/core/hle/kernel/svc/svc_processor.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handler for GetCurrentProcessorNumber.

/// Get which CPU core is executing the current thread.
pub fn get_current_processor_number() -> i32 {
    log::trace!("svc::GetCurrentProcessorNumber called");

    // TODO: system.CurrentPhysicalCore().CoreIndex() as i32
    log::warn!("svc::GetCurrentProcessorNumber: kernel object access not yet implemented");
    0
}
