//! Port of zuyu/src/core/hle/kernel/k_interrupt_manager.h/.cpp
//! Status: COMPLET (stub — runtime dependencies not yet available)
//! Derniere synchro: 2026-03-11
//!
//! KInterruptManager: namespace-level functions for handling inter-processor
//! interrupts and scheduling interrupts. Upstream is a namespace, not a class.

use crate::hardware_properties;

/// Handle an interrupt on the given core.
///
/// Upstream: clears the interrupt on the physical core, checks if the
/// current thread's user disable count requires pinning, and requests
/// rescheduling.
pub fn handle_interrupt(_core_id: i32) {
    // TODO: Implement once KernelCore, PhysicalCore, KThread, KScheduler are available.
    // kernel.PhysicalCore(core_id).ClearInterrupt();
    // ... pin thread if needed ...
    // kernel.CurrentScheduler()->RequestScheduleOnInterrupt();
}

/// Send an inter-processor interrupt to the cores indicated by the bitmask.
pub fn send_inter_processor_interrupt(_core_mask: u64) {
    for core_id in 0..hardware_properties::NUM_CPU_CORES as usize {
        if _core_mask & (1u64 << core_id) != 0 {
            // TODO: kernel.PhysicalCore(core_id).Interrupt();
        }
    }
}
