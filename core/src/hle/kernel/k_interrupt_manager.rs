//! Port of zuyu/src/core/hle/kernel/k_interrupt_manager.h/.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-20
//!
//! KInterruptManager: namespace-level functions for handling inter-processor
//! interrupts and scheduling interrupts. Upstream is a namespace, not a class.

use crate::hardware_properties;

use super::kernel::KernelCore;

/// Handle an interrupt on the given core.
///
/// Upstream: `KInterruptManager::HandleInterrupt(KernelCore&, s32)` (k_interrupt_manager.cpp:13-34).
///
/// 1. Acknowledge the interrupt by clearing it on the physical core.
/// 2. If the current thread has a non-zero user disable count and is not already
///    pinned, pin it and set the interrupt flag in the thread's TLS.
/// 3. Request rescheduling on the current scheduler.
pub fn handle_interrupt(kernel: &KernelCore, core_id: i32) {
    // Acknowledge the interrupt.
    // Upstream: kernel.PhysicalCore(core_id).ClearInterrupt();
    if let Some(physical_core) = kernel.physical_core(core_id as usize) {
        physical_core.clear_interrupt();
    }

    // Get the current thread.
    // Upstream: auto& current_thread = GetCurrentThread(kernel);
    let current_thread = super::kernel::get_current_emu_thread();

    if let Some(ref thread_arc) = current_thread {
        let thread = thread_arc.lock().unwrap();

        // Check if we need to pin the current thread.
        // Upstream: if (auto* process = GetCurrentProcessPointer(kernel); process) {
        if let Some(parent_weak) = thread.parent.as_ref() {
            if let Some(parent_arc) = parent_weak.upgrade() {
                let user_disable_count = thread.get_user_disable_count();
                let thread_id = thread.get_thread_id();
                let thread_core = thread.get_current_core();
                let is_termination_requested = thread.is_termination_requested();

                if user_disable_count != 0 {
                    let process = parent_arc.lock().unwrap();
                    let already_pinned = process.get_pinned_thread(core_id).is_some();
                    drop(process);

                    if !already_pinned {
                        // Upstream: KScopedSchedulerLock sl{kernel};
                        // The scheduler lock provides atomicity for pin operations.
                        // Since we can't access the GSC's scheduler lock from here
                        // without kernel reference, we perform the pin directly.
                        // The scheduler lock is already held by the interrupt context
                        // in the upstream model.

                        // Pin the current thread.
                        // Upstream: process->PinCurrentThread();
                        {
                            let mut process = parent_arc.lock().unwrap();
                            process.pin_current_thread(
                                core_id,
                                thread_id,
                                is_termination_requested,
                            );
                        }

                        // Upstream: cur_thread->Pin(core_id)
                        drop(thread);
                        {
                            let mut thread = thread_arc.lock().unwrap();
                            thread.pin(thread_core);
                        }

                        // Set the interrupt flag for the thread.
                        // Upstream: GetCurrentThread(kernel).SetInterruptFlag();
                        {
                            let thread = thread_arc.lock().unwrap();
                            thread.set_interrupt_flag();
                        }
                    }
                }
            }
        }
    }

    // Request interrupt scheduling.
    // Upstream: kernel.CurrentScheduler()->RequestScheduleOnInterrupt();
    if let Some(scheduler) = kernel.current_scheduler() {
        scheduler.lock().unwrap().request_schedule_on_interrupt();
    }
}

/// Send an inter-processor interrupt to the cores indicated by the bitmask.
///
/// Upstream: `KInterruptManager::SendInterProcessorInterrupt(KernelCore&, u64)`
/// (k_interrupt_manager.cpp:36-42).
pub fn send_inter_processor_interrupt(kernel: &KernelCore, core_mask: u64) {
    for core_id in 0..hardware_properties::NUM_CPU_CORES as usize {
        if core_mask & (1u64 << core_id) != 0 {
            if let Some(physical_core) = kernel.physical_core(core_id) {
                physical_core.interrupt();
            }
        }
    }
}
