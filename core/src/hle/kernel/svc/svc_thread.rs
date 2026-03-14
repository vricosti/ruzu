//! Port of zuyu/src/core/hle/kernel/svc/svc_thread.cpp
//! Status: Partial (kernel object-backed thread create/start/query path)
//! Derniere synchro: 2026-03-14
//!
//! SVC handlers for thread operations.

use std::sync::{Arc, Mutex};

use crate::hle::kernel::k_thread::KThread;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::{Handle, PseudoHandle, INVALID_HANDLE};
use crate::hle::kernel::svc_dispatch::SvcContext;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

fn is_valid_virtual_core_id(core_id: i32) -> bool {
    (0..4).contains(&core_id)
}

fn resolve_thread_handle(ctx: &SvcContext, handle: Handle) -> Option<Arc<Mutex<KThread>>> {
    if handle == PseudoHandle::CurrentThread as Handle {
        return ctx.current_thread();
    }

    let process = ctx.current_process.lock().unwrap();
    let object_id = process.handle_table.get_object(handle)?;
    process.get_thread_by_object_id(object_id)
}

fn resolve_current_thread(ctx: &SvcContext) -> Option<Arc<Mutex<KThread>>> {
    ctx.current_thread()
}

/// Creates a new thread.
pub fn create_thread(
    ctx: &SvcContext,
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

    {
        let process = ctx.current_process.lock().unwrap();
        if core_id == IDEAL_CORE_USE_PROCESS_VALUE {
            core_id = process.get_ideal_core_id();
        }

        if !is_valid_virtual_core_id(core_id) {
            return RESULT_INVALID_CORE_ID;
        }
        if ((1u64 << core_id) & process.get_core_mask()) == 0 {
            return RESULT_INVALID_CORE_ID;
        }
        if !(HIGHEST_THREAD_PRIORITY..=LOWEST_THREAD_PRIORITY).contains(&priority) {
            return RESULT_INVALID_PRIORITY;
        }
        if !process.check_thread_priority(priority) {
            return RESULT_INVALID_PRIORITY;
        }
    }

    let object_id = ctx.next_object_id.fetch_add(1, std::sync::atomic::Ordering::Relaxed) as u64;
    let thread_id = ctx
        .next_thread_id
        .fetch_add(1, std::sync::atomic::Ordering::Relaxed);

    let thread = Arc::new(Mutex::new(KThread::new()));
    {
        let mut new_thread = thread.lock().unwrap();
        let result = new_thread.initialize_user_thread(
            entry_point,
            arg,
            stack_bottom,
            priority,
            core_id,
            &ctx.current_process,
            thread_id,
            object_id,
            ctx.is_64bit,
        );
        if result != RESULT_SUCCESS.get_inner_value() {
            return ResultCode::new(result);
        }

        let current_thread = ctx.current_thread().expect("current thread must exist");
        let current_thread = current_thread.lock().unwrap();
        new_thread.clone_fpu_status_from(&current_thread);
    }

    let thread_tls_address = {
        let thread = thread.lock().unwrap();
        thread.get_tls_address()
    };

    let mut process = ctx.current_process.lock().unwrap();
    let handle_table_result = process.ensure_handle_table_initialized();
    if handle_table_result != RESULT_SUCCESS.get_inner_value() {
        let _ = process.delete_thread_local_region(thread_tls_address);
        return ResultCode::new(handle_table_result);
    }

    process.register_thread_object(thread);
    match process.handle_table.add(object_id) {
        Ok(handle) => {
            *out_handle = handle;
            ctx.scheduler.lock().unwrap().request_schedule();
            RESULT_SUCCESS
        }
        Err(_) => {
            process.unregister_thread_object_by_object_id(object_id);
            let _ = process.delete_thread_local_region(thread_tls_address);
            RESULT_OUT_OF_HANDLES
        }
    }
}

/// Starts the thread for the provided handle.
pub fn start_thread(ctx: &SvcContext, thread_handle: Handle) -> ResultCode {
    log::debug!("svc::StartThread called thread=0x{:08X}", thread_handle);

    let Some(thread) = resolve_thread_handle(ctx, thread_handle) else {
        return RESULT_INVALID_HANDLE;
    };

    let result = thread.lock().unwrap().run();
    if result == RESULT_SUCCESS.get_inner_value() {
        ctx.scheduler.lock().unwrap().request_schedule();
    }
    ResultCode::new(result)
}

/// Called when a thread exits.
pub fn exit_thread(ctx: &SvcContext) {
    let Some(thread) = resolve_current_thread(ctx) else {
        log::warn!("svc::ExitThread: current thread missing");
        return;
    };

    thread.lock().unwrap().exit();
    ctx.scheduler.lock().unwrap().request_schedule();
}

/// Sleeps the current thread.
pub fn sleep_thread(ctx: &SvcContext, ns: i64) {
    log::trace!("svc::SleepThread called nanoseconds={}", ns);

    if ns > 0 {
        let Some(thread) = resolve_current_thread(ctx) else {
            log::warn!("svc::SleepThread(sleep): current thread missing");
            return;
        };

        let result = thread.lock().unwrap().sleep(ns);
        if result != RESULT_SUCCESS.get_inner_value() {
            log::warn!("svc::SleepThread(sleep) failed: {:#x}", result);
            return;
        }

        ctx.scheduler.lock().unwrap().request_schedule();
        return;
    }

    let Some(current_thread_id) = ctx.current_thread_id() else {
        log::warn!("svc::SleepThread(yield): current thread missing");
        return;
    };
    let mut scheduler = ctx.scheduler.lock().unwrap();
    if ns == YieldType::WithoutCoreMigration as i64 {
        scheduler.yield_without_core_migration(&ctx.current_process, current_thread_id);
    } else if ns == YieldType::WithCoreMigration as i64 {
        scheduler.yield_with_core_migration(&ctx.current_process, current_thread_id);
    } else if ns == YieldType::ToAnyThread as i64 {
        scheduler.yield_to_any_thread(&ctx.current_process, current_thread_id);
    }
}

/// Gets the thread context.
pub fn get_thread_context3(_ctx: &SvcContext, out_context: u64, thread_handle: Handle) -> ResultCode {
    log::debug!(
        "svc::GetThreadContext3 called, out_context=0x{:08X}, thread_handle=0x{:X}",
        out_context, thread_handle
    );
    RESULT_NOT_IMPLEMENTED
}

/// Gets the priority for the specified thread.
pub fn get_thread_priority(
    ctx: &SvcContext,
    out_priority: &mut i32,
    handle: Handle,
) -> ResultCode {
    let Some(thread) = resolve_thread_handle(ctx, handle) else {
        return RESULT_INVALID_HANDLE;
    };

    *out_priority = thread.lock().unwrap().get_priority();
    RESULT_SUCCESS
}

/// Sets the priority for the specified thread.
pub fn set_thread_priority(ctx: &SvcContext, thread_handle: Handle, priority: i32) -> ResultCode {
    if !(HIGHEST_THREAD_PRIORITY..=LOWEST_THREAD_PRIORITY).contains(&priority) {
        return RESULT_INVALID_PRIORITY;
    }

    {
        let process = ctx.current_process.lock().unwrap();
        if !process.check_thread_priority(priority) {
            return RESULT_INVALID_PRIORITY;
        }
    }

    let Some(thread) = resolve_thread_handle(ctx, thread_handle) else {
        return RESULT_INVALID_HANDLE;
    };
    thread.lock().unwrap().set_base_priority(priority);
    RESULT_SUCCESS
}

/// Gets the thread list.
pub fn get_thread_list(
    ctx: &SvcContext,
    out_num_threads: &mut i32,
    _out_thread_ids: u64,
    out_thread_ids_size: i32,
    debug_handle: Handle,
) -> ResultCode {
    if debug_handle != INVALID_HANDLE {
        log::warn!("svc::GetThreadList: debug handle not yet supported");
    }
    if (out_thread_ids_size as u32 & 0xF000_0000) != 0 {
        return RESULT_OUT_OF_RANGE;
    }

    *out_num_threads = ctx.current_process.lock().unwrap().thread_list.len() as i32;
    RESULT_NOT_IMPLEMENTED
}

/// Gets the thread core mask.
pub fn get_thread_core_mask(
    ctx: &SvcContext,
    out_core_id: &mut i32,
    out_affinity_mask: &mut u64,
    thread_handle: Handle,
) -> ResultCode {
    let Some(thread) = resolve_thread_handle(ctx, thread_handle) else {
        return RESULT_INVALID_HANDLE;
    };
    let (core_id, affinity_mask) = thread.lock().unwrap().get_core_mask();
    *out_core_id = core_id;
    *out_affinity_mask = affinity_mask;
    RESULT_SUCCESS
}

/// Sets the thread core mask.
pub fn set_thread_core_mask(
    ctx: &SvcContext,
    thread_handle: Handle,
    mut core_id: i32,
    mut affinity_mask: u64,
) -> ResultCode {
    let process = ctx.current_process.lock().unwrap();
    if core_id == IDEAL_CORE_USE_PROCESS_VALUE {
        core_id = process.get_ideal_core_id();
        affinity_mask = 1u64 << core_id;
    } else {
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
    drop(process);

    let Some(thread) = resolve_thread_handle(ctx, thread_handle) else {
        return RESULT_INVALID_HANDLE;
    };
    let result = ResultCode::new(thread.lock().unwrap().set_core_mask(core_id, affinity_mask));
    result
}

/// Gets the ID for the specified thread.
pub fn get_thread_id(ctx: &SvcContext, out_thread_id: &mut u64, thread_handle: Handle) -> ResultCode {
    let Some(thread) = resolve_thread_handle(ctx, thread_handle) else {
        return RESULT_INVALID_HANDLE;
    };
    *out_thread_id = thread.lock().unwrap().get_thread_id();
    RESULT_SUCCESS
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::{AtomicU32, AtomicU64};

    use super::*;
    use crate::hle::kernel::k_process::KProcess;

    fn test_context() -> SvcContext {
        let mut process = KProcess::new();
        process.capabilities.core_mask = 0xF;
        process.capabilities.priority_mask = u64::MAX;
        process.flags = 0;
        process.allocate_code_memory(0x200000, 0x20000);
        process.initialize_handle_table();
        process.initialize_thread_local_region_base(0x240000);

        let process = Arc::new(Mutex::new(process));
        let current_thread = Arc::new(Mutex::new(KThread::new()));
        let scheduler = Arc::new(Mutex::new(crate::hle::kernel::k_scheduler::KScheduler::new(0)));
        {
            let mut thread = current_thread.lock().unwrap();
            thread.initialize_main_thread(0x200000, 0x250000, 0, 0x23f000, &process, 1, 1, false);
            thread.set_priority(44);
            thread.set_base_priority(44);
        }
        process
            .lock()
            .unwrap()
            .register_thread_object(current_thread.clone());
        scheduler.lock().unwrap().initialize(1, 0, 0);

        let shared_memory = process.lock().unwrap().get_shared_memory();

        SvcContext {
            shared_memory,
            code_base: 0x200000,
            code_size: 0x20000,
            stack_base: 0x240000,
            stack_size: 0x10000,
            program_id: 1,
            tls_base: 0x23f000,
            current_process: process,
            scheduler,
            next_thread_id: Arc::new(AtomicU64::new(2)),
            next_object_id: Arc::new(AtomicU32::new(2)),
            is_64bit: false,
        }
    }

    #[test]
    fn create_and_start_thread_registers_kernel_objects() {
        let ctx = test_context();
        let mut handle = 0;
        let result = create_thread(&ctx, &mut handle, 0x201000, 0x1234, 0x260000, 44, 0);
        assert_eq!(result, RESULT_SUCCESS);
        assert_ne!(handle, INVALID_HANDLE);

        let process = ctx.current_process.lock().unwrap();
        let object_id = process.handle_table.get_object(handle).unwrap();
        let thread = process.get_thread_by_object_id(object_id).unwrap();
        drop(process);

        assert_eq!(thread.lock().unwrap().get_state(), crate::hle::kernel::k_thread::ThreadState::INITIALIZED);

        let result = start_thread(&ctx, handle);
        assert_eq!(result, RESULT_SUCCESS);
        assert_eq!(thread.lock().unwrap().get_state(), crate::hle::kernel::k_thread::ThreadState::RUNNABLE);
    }

    #[test]
    fn yield_switches_to_other_equal_priority_thread() {
        let ctx = test_context();
        let mut handle = 0;
        let result = create_thread(&ctx, &mut handle, 0x201000, 0x1234, 0x260000, 44, 0);
        assert_eq!(result, RESULT_SUCCESS);
        assert_eq!(start_thread(&ctx, handle), RESULT_SUCCESS);

        let current_thread_id = ctx.current_thread_id().unwrap();
        let next_before_yield = ctx
            .scheduler
            .lock()
            .unwrap()
            .select_next_thread_id(&ctx.current_process, current_thread_id);
        assert_eq!(next_before_yield, Some(current_thread_id));

        sleep_thread(&ctx, YieldType::WithoutCoreMigration as i64);

        let next_after_yield = ctx
            .scheduler
            .lock()
            .unwrap()
            .select_next_thread_id(&ctx.current_process, current_thread_id);
        assert_eq!(next_after_yield, Some(2));
    }
}
