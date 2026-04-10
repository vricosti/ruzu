//! Port of zuyu/src/core/hle/kernel/svc/svc_thread.cpp
//! Status: Partial (kernel object-backed thread create/start/query path)
//! Derniere synchro: 2026-03-14
//!
//! SVC handlers for thread operations.

use std::sync::{Arc, Mutex};

use crate::core::System;
use crate::hle::kernel::k_memory_block::{KMemoryPermission, KMemoryState, PAGE_SIZE};
use crate::hle::kernel::k_resource_limit::LimitableResource;
use crate::hle::kernel::k_scoped_resource_reservation::KScopedResourceReservation;
use crate::hle::kernel::k_thread::KThread;
use crate::hle::kernel::k_typed_address::KProcessAddress;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::{Handle, PseudoHandle, INVALID_HANDLE};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

const FALLBACK_USER_THREAD_STACK_SIZE: u64 = 0x100000;

fn is_valid_virtual_core_id(core_id: i32) -> bool {
    (0..4).contains(&core_id)
}

fn sleep_timeout_tick_from_ns(current_tick: i64, ns: i64) -> i64 {
    debug_assert!(ns > 0);

    let offset_tick = ns;
    if offset_tick > 0 {
        let timeout = current_tick.saturating_add(offset_tick).saturating_add(2);
        if timeout <= 0 {
            i64::MAX
        } else {
            timeout
        }
    } else {
        i64::MAX
    }
}

fn resolve_thread_handle(system: &System, handle: Handle) -> Option<Arc<Mutex<KThread>>> {
    if handle == PseudoHandle::CurrentThread as Handle {
        return system.current_thread();
    }

    let process = system.current_process_arc().lock().unwrap();
    let object_id = process.handle_table.get_object(handle)?;
    process.get_thread_by_object_id(object_id)
}

fn resolve_current_thread(system: &System) -> Option<Arc<Mutex<KThread>>> {
    system.current_thread()
}

/// Creates a new thread.
pub fn create_thread(
    system: &System,
    out_handle: &mut Handle,
    entry_point: u64,
    arg: u64,
    stack_bottom: u64,
    priority: i32,
    mut core_id: i32,
) -> ResultCode {
    let requested_core_id = core_id;
    log::debug!(
        "svc::CreateThread called entry=0x{:08X}, arg=0x{:08X}, stack=0x{:08X}, prio={}, core={}",
        entry_point,
        arg,
        stack_bottom,
        priority,
        core_id
    );

    let current_process = system.current_process_arc().clone();

    {
        let process = current_process.lock().unwrap();
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

        log::trace!(
            "svc::CreateThread resolved entry=0x{:08X} prio={} requested_core={} effective_core={} process_ideal_core={}",
            entry_point,
            priority,
            requested_core_id,
            core_id,
            process.get_ideal_core_id(),
        );
    }

    let thread_reservation_timeout = crate::hle::kernel::kernel::get_current_hardware_tick()
        .unwrap_or(i64::MAX)
        .saturating_add(100_000_000);
    let mut thread_reservation = {
        let process = current_process.lock().unwrap();
        KScopedResourceReservation::new_with_timeout(
            process.resource_limit.clone(),
            LimitableResource::ThreadCountMax,
            1,
            thread_reservation_timeout,
        )
    };
    if !thread_reservation.succeeded() {
        return RESULT_LIMIT_REACHED;
    }

    let object_id = system.kernel().unwrap().create_new_object_id() as u64;
    let thread_id = system.kernel().unwrap().create_new_thread_id();

    {
        let mut process = current_process.lock().unwrap();
        ensure_user_stack_mapping(&mut process, stack_bottom);
    }

    // Create the guest thread fiber entry — matches upstream
    // `system.GetCpuManager().GetGuestThreadFunc()`.
    let guest_thread_func: Option<Box<dyn FnOnce() + Send>> = {
        let kernel = system.kernel().expect("kernel must exist");
        let kp = kernel as *const crate::hle::kernel::kernel::KernelCore as usize;
        let is_multicore = kernel.is_multicore();
        Some(Box::new(move || {
            let kernel = unsafe { &*(kp as *const crate::hle::kernel::kernel::KernelCore) };
            if is_multicore {
                crate::cpu_manager::CpuManager::multi_core_run_guest_thread(kernel);
            } else {
                crate::cpu_manager::CpuManager::single_core_run_guest_thread_entry(kernel);
            }
        }))
    };

    let thread = Arc::new(Mutex::new(KThread::new()));
    {
        let mut new_thread = thread.lock().unwrap();
        let result = new_thread.initialize_user_thread_with_init_func(
            entry_point,
            arg,
            stack_bottom,
            priority,
            core_id,
            &current_process,
            thread_id,
            object_id,
            system.runtime_is_64bit(),
            guest_thread_func,
        );
        if result != RESULT_SUCCESS.get_inner_value() {
            return ResultCode::new(result);
        }

        let current_thread = system.current_thread().expect("current thread must exist");
        let current_thread = current_thread.lock().unwrap();
        new_thread.clone_fpu_status_from(&current_thread);
    }

    thread_reservation.commit();

    let thread_tls_address = {
        let thread = thread.lock().unwrap();
        thread.get_tls_address()
    };

    let mut process = current_process.lock().unwrap();
    let handle_table_result = process.ensure_handle_table_initialized();
    if handle_table_result != RESULT_SUCCESS.get_inner_value() {
        let _ = process.delete_thread_local_region(thread_tls_address);
        return ResultCode::new(handle_table_result);
    }

    // Register the thread with the GlobalSchedulerContext so the scheduler can
    // find it by thread_id during fiber dispatch.
    // Upstream: KThread::Register(kernel, thread) adds to kernel object list.
    if let Some(ref gsc) = process.global_scheduler_context {
        gsc.lock().unwrap().add_thread(Arc::clone(&thread));
    }

    process.register_thread_object(thread);
    match process.handle_table.add(object_id) {
        Ok(handle) => {
            *out_handle = handle;
            RESULT_SUCCESS
        }
        Err(_) => {
            process.unregister_thread_object_by_object_id(object_id);
            let _ = process.delete_thread_local_region(thread_tls_address);
            RESULT_OUT_OF_HANDLES
        }
    }
}

fn ensure_user_stack_mapping(
    process: &mut crate::hle::kernel::k_process::KProcess,
    stack_top: u64,
) {
    if stack_top == 0 {
        return;
    }

    let probe_addr = stack_top.saturating_sub(0x10);
    let shared_probe = process.process_memory.read().unwrap().read_bytes(probe_addr, 0x10);
    let memory_probe = process
        .page_table
        .get_base()
        .m_memory
        .as_ref()
        .map(|memory| {
            let memory = memory.lock().unwrap();
            let mut bytes = [0u8; 0x10];
            let valid = memory.is_valid_virtual_address_range(probe_addr, bytes.len() as u64);
            if valid {
                let _ = memory.read_block(probe_addr, &mut bytes);
            }
            (valid, bytes)
        });

    let stack_top_page = (stack_top + PAGE_SIZE as u64 - 1) & !((PAGE_SIZE as u64) - 1);
    let stack_base = stack_top_page.saturating_sub(FALLBACK_USER_THREAD_STACK_SIZE);
    if stack_base >= stack_top_page {
        return;
    }

    let size = (stack_top_page - stack_base) as usize;
    let query_addr = stack_top.saturating_sub(1) as usize;
    let Some(info) = process.page_table.query_info(query_addr) else {
        return;
    };
    let heap_start = process.page_table.get_heap_region_start().get();
    let heap_size = process.page_table.get_current_heap_size();
    let heap_end = heap_start.saturating_add(heap_size as u64);
    let in_heap = probe_addr >= heap_start && probe_addr < heap_end;

    log::trace!(
        "svc::CreateThread stack probe top={:#x} query_state={:?} heap=[{:#x}..{:#x}) in_heap={} shared={:02x?} memory={:?}",
        stack_top,
        info.get_state(),
        heap_start,
        heap_end,
        in_heap,
        shared_probe,
        memory_probe
    );

    if info.get_state() == KMemoryState::FREE {
        let iter_start = probe_addr.saturating_sub((PAGE_SIZE as u64) * 4) as usize;
        for block in process
            .page_table
            .get_base()
            .get_memory_block_manager()
            .find_iterator(iter_start)
            .take(6)
        {
            let block_info = block.get_memory_info();
            log::trace!(
                "svc::CreateThread nearby block [{:#x}..{:#x}) state={:?} perm={:?} attr={:?}",
                block_info.m_address,
                block_info.m_address + block_info.m_size,
                block_info.m_state,
                block_info.m_permission,
                block_info.m_attribute
            );
        }
    }

    if info.get_state() != KMemoryState::FREE {
        return;
    }

    let num_pages = size / PAGE_SIZE;
    let result = process.page_table.map_pages_at_address(
        KProcessAddress::new(stack_base),
        num_pages,
        KMemoryState::STACK,
        KMemoryPermission::USER_READ_WRITE,
    );
    if result != RESULT_SUCCESS.get_inner_value() {
        log::warn!(
            "svc::CreateThread: failed to map fallback stack [{:#x}..{:#x}) result={:#x}",
            stack_base,
            stack_top_page,
            result
        );
        return;
    }

    log::trace!(
        "svc::CreateThread: mapped fallback user stack [{:#x}..{:#x})",
        stack_base,
        stack_top_page
    );
}

/// Starts the thread for the provided handle.
pub fn start_thread(system: &System, thread_handle: Handle) -> ResultCode {
    log::debug!("svc::StartThread called thread=0x{:08X}", thread_handle);

    let Some(thread) = resolve_thread_handle(system, thread_handle) else {
        return RESULT_INVALID_HANDLE;
    };

    let result = KThread::run_thread(&thread);
    {
        let thread_guard = thread.lock().unwrap();
        log::trace!(
            "svc::StartThread result handle=0x{:08X} tid={} state={:?} core_id={} current_core={} affinity=0x{:X} result={:#x}",
            thread_handle,
            thread_guard.get_thread_id(),
            thread_guard.get_state(),
            thread_guard.get_active_core(),
            thread_guard.get_current_core(),
            thread_guard.physical_affinity_mask.get_affinity_mask(),
            result,
        );
    }
    ResultCode::new(result)
}

/// Called when a thread exits.
pub fn exit_thread(system: &System) {
    let Some(thread) = resolve_current_thread(system) else {
        log::warn!("svc::ExitThread: current thread missing");
        return;
    };

    thread.lock().unwrap().exit();
    system.scheduler_arc().lock().unwrap().request_schedule();
}

/// Sleeps the current thread.
pub fn sleep_thread(system: &System, ns: i64) {
    log::trace!("svc::SleepThread called nanoseconds={}", ns);

    if ns > 0 {
        let Some(current_thread_id) = system.current_thread_id() else {
            log::warn!("svc::SleepThread(sleep): current thread missing");
            return;
        };
        log::trace!(
            "svc::SleepThread(sleep): tid={} before get_current_hardware_tick",
            current_thread_id
        );

        let current_tick = system
            .kernel()
            .and_then(|_| crate::hle::kernel::kernel::get_current_hardware_tick())
            .unwrap_or(i64::MAX);
        log::trace!(
            "svc::SleepThread(sleep): tid={} current_tick={}",
            current_thread_id,
            current_tick
        );
        let timeout = sleep_timeout_tick_from_ns(current_tick, ns);
        log::trace!(
            "svc::SleepThread(sleep): tid={} timeout_tick={} before thread.sleep",
            current_thread_id,
            timeout
        );
        let Some(result) =
            crate::hle::kernel::kernel::with_current_thread_fast_mut(|thread| thread.sleep(timeout))
        else {
            log::warn!("svc::SleepThread(sleep): current thread cache missing");
            return;
        };
        log::trace!(
            "svc::SleepThread(sleep): tid={} thread.sleep returned result=0x{:x}",
            current_thread_id,
            result
        );
        if result != RESULT_SUCCESS.get_inner_value() {
            log::warn!("svc::SleepThread(sleep) failed: {:#x}", result);
            return;
        }

        log::trace!("svc::SleepThread(sleep): tid={} ns={} -> wait armed", current_thread_id, ns);
        return;
    }

    let Some(current_thread_id) = system.current_thread_id() else {
        log::warn!("svc::SleepThread(yield): current thread missing");
        return;
    };
    if let Some(thread) = resolve_current_thread(system) {
        let thread = thread.lock().unwrap();
        log::trace!(
            "svc::SleepThread(yield) ctx: tid={} pc=0x{:08X} lr=0x{:08X} sp=0x{:08X}",
            current_thread_id,
            thread.thread_context.pc as u32,
            thread.thread_context.lr as u32,
            thread.thread_context.sp as u32,
        );
    }
    log::trace!(
        "svc::SleepThread(yield): tid={} ns={} branch={}",
        current_thread_id,
        ns,
        match ns {
            x if x == YieldType::WithoutCoreMigration as i64 => "without_core_migration",
            x if x == YieldType::WithCoreMigration as i64 => "with_core_migration",
            x if x == YieldType::ToAnyThread as i64 => "to_any_thread",
            _ => "invalid_noop",
        }
    );
    let current_process = system.current_process_arc();
    let sched_arc = system.scheduler_arc();
    let mut scheduler = sched_arc.lock().unwrap();
    if ns == YieldType::WithoutCoreMigration as i64 {
        scheduler.yield_without_core_migration(current_process, current_thread_id);
    } else if ns == YieldType::WithCoreMigration as i64 {
        scheduler.yield_with_core_migration(current_process, current_thread_id);
    } else if ns == YieldType::ToAnyThread as i64 {
        scheduler.yield_to_any_thread(current_process, current_thread_id);
    }
}

/// Gets the thread context.
pub fn get_thread_context3(
    _system: &System,
    out_context: u64,
    thread_handle: Handle,
) -> ResultCode {
    log::debug!(
        "svc::GetThreadContext3 called, out_context=0x{:08X}, thread_handle=0x{:X}",
        out_context,
        thread_handle
    );
    RESULT_NOT_IMPLEMENTED
}

/// Gets the priority for the specified thread.
pub fn get_thread_priority(system: &System, out_priority: &mut i32, handle: Handle) -> ResultCode {
    let Some(thread) = resolve_thread_handle(system, handle) else {
        return RESULT_INVALID_HANDLE;
    };

    *out_priority = thread.lock().unwrap().get_priority();
    RESULT_SUCCESS
}

/// Sets the priority for the specified thread.
pub fn set_thread_priority(system: &System, thread_handle: Handle, priority: i32) -> ResultCode {
    if !(HIGHEST_THREAD_PRIORITY..=LOWEST_THREAD_PRIORITY).contains(&priority) {
        return RESULT_INVALID_PRIORITY;
    }

    {
        let process = system.current_process_arc().lock().unwrap();
        if !process.check_thread_priority(priority) {
            return RESULT_INVALID_PRIORITY;
        }
    }

    let Some(thread) = resolve_thread_handle(system, thread_handle) else {
        return RESULT_INVALID_HANDLE;
    };
    thread.lock().unwrap().set_base_priority(priority);
    RESULT_SUCCESS
}

/// Gets the thread list.
pub fn get_thread_list(
    system: &System,
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

    *out_num_threads = system
        .current_process_arc()
        .lock()
        .unwrap()
        .thread_list
        .len() as i32;
    RESULT_NOT_IMPLEMENTED
}

/// Gets the thread core mask.
pub fn get_thread_core_mask(
    system: &System,
    out_core_id: &mut i32,
    out_affinity_mask: &mut u64,
    thread_handle: Handle,
) -> ResultCode {
    let Some(thread) = resolve_thread_handle(system, thread_handle) else {
        return RESULT_INVALID_HANDLE;
    };
    let (core_id, affinity_mask) = thread.lock().unwrap().get_core_mask();
    *out_core_id = core_id;
    *out_affinity_mask = affinity_mask;
    RESULT_SUCCESS
}

/// Sets the thread core mask.
pub fn set_thread_core_mask(
    system: &System,
    thread_handle: Handle,
    mut core_id: i32,
    mut affinity_mask: u64,
) -> ResultCode {
    let process = system.current_process_arc().lock().unwrap();
    if core_id == IDEAL_CORE_USE_PROCESS_VALUE {
        core_id = process.get_ideal_core_id();
        affinity_mask = 1u64 << core_id;
    } else {
        let process_core_mask = process.get_core_mask();
        if (affinity_mask | process_core_mask) != process_core_mask {
            return RESULT_INVALID_CORE_ID;
        }
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

    let Some(thread) = resolve_thread_handle(system, thread_handle) else {
        return RESULT_INVALID_HANDLE;
    };
    let result = ResultCode::new(thread.lock().unwrap().set_core_mask(core_id, affinity_mask));
    result
}

/// Gets the ID for the specified thread.
pub fn get_thread_id(
    system: &System,
    out_thread_id: &mut u64,
    thread_handle: Handle,
) -> ResultCode {
    let Some(thread) = resolve_thread_handle(system, thread_handle) else {
        return RESULT_INVALID_HANDLE;
    };
    *out_thread_id = thread.lock().unwrap().get_thread_id();
    RESULT_SUCCESS
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::System;
    use crate::hle::kernel::k_process::KProcess;
    use crate::hle::kernel::k_resource_limit::{create_resource_limit_for_process, LimitableResource};
    use crate::hle::kernel::k_thread::ThreadState;
    use crate::hle::kernel::k_worker_task_manager::KWorkerTaskManager;

    fn test_system() -> System {
        let mut system = System::new_for_test();

        let mut process = KProcess::new();
        process.capabilities.core_mask = 0xF;
        process.capabilities.priority_mask = u64::MAX;
        process.flags = 0;
        process.allocate_code_memory(0x200000, 0x20000);
        process.initialize_handle_table();
        process.initialize_thread_local_region_base(0x240000);
        process.resource_limit = Some(Arc::new(Mutex::new(create_resource_limit_for_process(
            0x1_0000_0000,
        ))));

        let process = Arc::new(Mutex::new(process));
        let current_thread = Arc::new(Mutex::new(KThread::new()));
        let scheduler = Arc::new(Mutex::new(
            crate::hle::kernel::k_scheduler::KScheduler::new(0),
        ));
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
        // Push main thread into priority queue (it's already RUNNABLE).
        process.lock().unwrap().push_back_to_priority_queue(1);
        scheduler.lock().unwrap().initialize(1, 0, 0);

        let shared_memory = process.lock().unwrap().get_shared_memory();

        system.set_current_process_arc(process);
        system.set_scheduler_arc(scheduler);
        system.set_shared_process_memory(shared_memory);
        system.set_runtime_program_id(1);
        system.set_runtime_64bit(false);
        system
    }

    #[test]
    fn create_and_start_thread_registers_kernel_objects() {
        let system = test_system();
        let mut handle = 0;
        let result = create_thread(&system, &mut handle, 0x201000, 0x1234, 0x260000, 44, 0);
        assert_eq!(result, RESULT_SUCCESS);
        assert_ne!(handle, INVALID_HANDLE);

        let process = system.current_process_arc().lock().unwrap();
        let object_id = process.handle_table.get_object(handle).unwrap();
        let thread = process.get_thread_by_object_id(object_id).unwrap();
        drop(process);

        assert_eq!(
            thread.lock().unwrap().get_state(),
            crate::hle::kernel::k_thread::ThreadState::INITIALIZED
        );

        let result = start_thread(&system, handle);
        assert_eq!(result, RESULT_SUCCESS);
        assert_eq!(
            thread.lock().unwrap().get_state(),
            crate::hle::kernel::k_thread::ThreadState::RUNNABLE
        );
    }

    #[test]
    fn sleep_thread_uses_upstream_absolute_timeout_tick() {
        assert_eq!(sleep_timeout_tick_from_ns(1234, 10), 1246);
    }

    #[test]
    fn sleep_thread_positive_requests_reschedule_and_blocks_current_thread() {
        let system = test_system();

        assert!(!system.scheduler_arc().lock().unwrap().needs_scheduling());

        sleep_thread(&system, 10);

        let current_thread = system.current_thread().unwrap();
        let thread = current_thread.lock().unwrap();
        assert_eq!(thread.get_state(), ThreadState::WAITING);
        assert_eq!(
            thread.get_wait_reason_for_debugging(),
            crate::hle::kernel::k_thread::ThreadWaitReasonForDebugging::Sleep
        );
        drop(thread);

        assert!(system.scheduler_arc().lock().unwrap().needs_scheduling());
    }

    #[test]
    fn yield_switches_to_other_equal_priority_thread() {
        let system = test_system();
        let mut handle = 0;
        let result = create_thread(&system, &mut handle, 0x201000, 0x1234, 0x260000, 44, 0);
        assert_eq!(result, RESULT_SUCCESS);
        assert_eq!(start_thread(&system, handle), RESULT_SUCCESS);

        // Get the new thread's ID by looking up its handle.
        let new_thread_id = {
            let process = system.current_process_arc().lock().unwrap();
            let object_id = process.handle_table.get_object(handle).unwrap();
            let thread = process.get_thread_by_object_id(object_id).unwrap();
            let tid = thread.lock().unwrap().get_thread_id();
            tid
        };

        let current_thread_id = system.current_thread_id().unwrap();
        let next_before_yield = system
            .scheduler_arc()
            .lock()
            .unwrap()
            .select_next_thread_id(system.current_process_arc(), current_thread_id);
        assert_eq!(next_before_yield, Some(current_thread_id));

        sleep_thread(&system, YieldType::WithoutCoreMigration as i64);

        let next_after_yield = system
            .scheduler_arc()
            .lock()
            .unwrap()
            .select_next_thread_id(system.current_process_arc(), current_thread_id);
        assert_eq!(next_after_yield, Some(new_thread_id));
    }

    #[test]
    fn yield_exits_current_thread_when_termination_was_requested() {
        let system = test_system();
        let mut handle = 0;
        let result = create_thread(&system, &mut handle, 0x201000, 0x1234, 0x260000, 44, 0);
        assert_eq!(result, RESULT_SUCCESS);
        assert_eq!(start_thread(&system, handle), RESULT_SUCCESS);

        let current_thread = system.current_thread().unwrap();
        current_thread.lock().unwrap().request_terminate();

        sleep_thread(&system, YieldType::WithoutCoreMigration as i64);
        KWorkerTaskManager::wait_for_global_idle();

        let thread = current_thread.lock().unwrap();
        assert_eq!(thread.get_state(), ThreadState::TERMINATED);
        assert!(thread.is_signaled());
    }

    #[test]
    fn create_thread_returns_limit_reached_when_thread_count_reservation_fails() {
        let system = test_system();
        {
            let process = system.current_process_arc();
            let process = process.lock().unwrap();
            let resource_limit = process.resource_limit.as_ref().unwrap().clone();
            let mut resource_limit = resource_limit.lock().unwrap();
            resource_limit.set_limit_value(LimitableResource::ThreadCountMax, 0).unwrap();
        }

        let mut handle = INVALID_HANDLE;
        let result = create_thread(&system, &mut handle, 0x201000, 0x1234, 0x260000, 44, 0);
        assert_eq!(result, RESULT_LIMIT_REACHED);
        assert_eq!(handle, INVALID_HANDLE);
    }
}
