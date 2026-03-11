//! Port of zuyu/src/core/hle/kernel/svc/svc_thread.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for thread operations.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::{Handle, INVALID_HANDLE};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

fn is_valid_virtual_core_id(core_id: i32) -> bool {
    (0..4).contains(&core_id) // Core::Hardware::NUM_CPU_CORES = 4
}

/// Creates a new thread.
pub fn create_thread(
    out_handle: &mut Handle,
    entry_point: u64,
    arg: u64,
    stack_bottom: u64,
    priority: i32,
    mut core_id: i32,
) -> ResultCode {
    log::debug!(
        "svc::CreateThread called entry=0x{:08X}, arg=0x{:08X}, stack=0x{:08X}, prio=0x{:08X}, core=0x{:08X}",
        entry_point, arg, stack_bottom, priority, core_id
    );

    // Adjust core id, if it's the default magic.
    if core_id == IDEAL_CORE_USE_PROCESS_VALUE {
        // TODO: core_id = process.GetIdealCoreId()
        core_id = 0; // placeholder
    }

    // Validate arguments.
    if !is_valid_virtual_core_id(core_id) {
        return RESULT_INVALID_CORE_ID;
    }
    // TODO: R_UNLESS(((1 << core_id) & process.GetCoreMask()) != 0, ResultInvalidCoreId)

    if !(HIGHEST_THREAD_PRIORITY..=LOWEST_THREAD_PRIORITY).contains(&priority) {
        return RESULT_INVALID_PRIORITY;
    }
    // TODO: R_UNLESS(process.CheckThreadPriority(priority), ResultInvalidPriority)

    // TODO: Reserve thread from resource limit.
    // TODO: KThread::Create, InitializeUserThread, Commit, CloneFpuStatus, Register, AddToHandleTable
    *out_handle = 0;
    log::warn!("svc::CreateThread: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Starts the thread for the provided handle.
pub fn start_thread(thread_handle: Handle) -> ResultCode {
    log::debug!("svc::StartThread called thread=0x{:08X}", thread_handle);

    // TODO: Get thread from handle, call thread->Run()
    log::warn!("svc::StartThread: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Called when a thread exits.
pub fn exit_thread() {
    // TODO: Get current thread, remove from global scheduler, call Exit()
    log::warn!("svc::ExitThread: kernel object access not yet implemented");
}

/// Sleeps the current thread.
pub fn sleep_thread(ns: i64) {
    log::trace!("svc::SleepThread called nanoseconds={}", ns);

    if ns > 0 {
        // Convert timeout to ticks and sleep.
        // TODO: kernel.HardwareTimer().GetTick() + offset + 2
        // TODO: GetCurrentThread(kernel).Sleep(timeout)
        log::warn!("svc::SleepThread(sleep): kernel object access not yet implemented");
    } else {
        let yield_type = ns;
        match yield_type {
            0 => {
                // YieldType::WithoutCoreMigration
                // TODO: KScheduler::YieldWithoutCoreMigration(kernel)
            }
            -1 => {
                // YieldType::WithCoreMigration
                // TODO: KScheduler::YieldWithCoreMigration(kernel)
            }
            -2 => {
                // YieldType::ToAnyThread
                // TODO: KScheduler::YieldToAnyThread(kernel)
            }
            _ => {
                // Nintendo does nothing for invalid values.
            }
        }
    }
}

/// Gets the thread context.
pub fn get_thread_context3(out_context: u64, thread_handle: Handle) -> ResultCode {
    log::debug!(
        "svc::GetThreadContext3 called, out_context=0x{:08X}, thread_handle=0x{:X}",
        out_context, thread_handle
    );

    // TODO: Get thread from handle.
    // TODO: Require thread is in current process, not current thread.
    // TODO: thread->GetThreadContext3(&context)
    // TODO: Write context to user memory.
    log::warn!("svc::GetThreadContext3: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Gets the priority for the specified thread.
pub fn get_thread_priority(out_priority: &mut i32, handle: Handle) -> ResultCode {
    log::trace!("svc::GetThreadPriority called");

    // TODO: Get thread from handle, get priority.
    *out_priority = 0;
    log::warn!("svc::GetThreadPriority: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Sets the priority for the specified thread.
pub fn set_thread_priority(thread_handle: Handle, priority: i32) -> ResultCode {
    if !(HIGHEST_THREAD_PRIORITY..=LOWEST_THREAD_PRIORITY).contains(&priority) {
        return RESULT_INVALID_PRIORITY;
    }
    // TODO: process.CheckThreadPriority(priority)
    // TODO: Get thread from handle, thread->SetBasePriority(priority)
    log::warn!("svc::SetThreadPriority: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Gets the thread list.
pub fn get_thread_list(
    out_num_threads: &mut i32,
    _out_thread_ids: u64,
    out_thread_ids_size: i32,
    debug_handle: Handle,
) -> ResultCode {
    // TODO: Handle debug_handle case when debug events are supported.
    if debug_handle != INVALID_HANDLE {
        log::warn!("svc::GetThreadList: debug handle not yet supported");
    }

    log::debug!(
        "svc::GetThreadList called. out_thread_ids_size={}",
        out_thread_ids_size
    );

    if (out_thread_ids_size as u32 & 0xF000_0000) != 0 {
        log::error!("Supplied size outside [0, 0x0FFFFFFF] range. size={}", out_thread_ids_size);
        return RESULT_OUT_OF_RANGE;
    }

    // TODO: Get thread list from current process, copy to user memory.
    *out_num_threads = 0;
    log::warn!("svc::GetThreadList: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Gets the thread core mask.
pub fn get_thread_core_mask(
    out_core_id: &mut i32,
    out_affinity_mask: &mut u64,
    thread_handle: Handle,
) -> ResultCode {
    log::trace!("svc::GetThreadCoreMask called, handle=0x{:08X}", thread_handle);

    // TODO: Get thread from handle, thread->GetCoreMask(out_core_id, out_affinity_mask)
    *out_core_id = 0;
    *out_affinity_mask = 0;
    log::warn!("svc::GetThreadCoreMask: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Sets the thread core mask.
pub fn set_thread_core_mask(
    thread_handle: Handle,
    mut core_id: i32,
    mut affinity_mask: u64,
) -> ResultCode {
    if core_id == IDEAL_CORE_USE_PROCESS_VALUE {
        // TODO: core_id = GetCurrentProcess(kernel).GetIdealCoreId()
        core_id = 0; // placeholder
        affinity_mask = 1u64 << core_id;
    } else {
        // TODO: Validate affinity_mask against process core mask.
        if affinity_mask == 0 {
            return RESULT_INVALID_COMBINATION;
        }

        if is_valid_virtual_core_id(core_id) {
            if ((1u64 << core_id) & affinity_mask) == 0 {
                return RESULT_INVALID_COMBINATION;
            }
        } else if core_id != IDEAL_CORE_NO_UPDATE && core_id != IDEAL_CORE_DONT_CARE {
            return RESULT_INVALID_CORE_ID;
        }
    }

    // TODO: Get thread from handle, thread->SetCoreMask(core_id, affinity_mask)
    log::warn!("svc::SetThreadCoreMask: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Gets the ID for the specified thread.
pub fn get_thread_id(out_thread_id: &mut u64, thread_handle: Handle) -> ResultCode {
    // TODO: Get thread from handle, *out_thread_id = thread->GetId()
    *out_thread_id = 0;
    log::warn!("svc::GetThreadId: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}
