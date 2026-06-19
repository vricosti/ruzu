//! Port of zuyu/src/core/hle/kernel/svc/svc_thread.cpp
//! Status: Partial (kernel object-backed thread create/start/query path)
//! Derniere synchro: 2026-03-14
//!
//! SVC handlers for thread operations.

use std::sync::{Arc, Mutex};

use super::super::k_process::ProcessLock;
use crate::core::System;
use crate::hle::kernel::k_light_lock::KScopedLightLock;
use crate::hle::kernel::k_resource_limit::LimitableResource;
use crate::hle::kernel::k_scoped_resource_reservation::KScopedResourceReservation;
use crate::hle::kernel::k_thread::{KThread, KThreadLock};
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::{Handle, PseudoHandle, INVALID_HANDLE};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

#[derive(Clone)]
struct ThreadLifecycleEntry {
    tid: u64,
    parent_tid: u64,
    handle: Handle,
    entry_point: u64,
    arg: u64,
    stack_bottom: u64,
    priority: i32,
    requested_core_id: i32,
    effective_core_id: i32,
    created_order: u64,
    started_order: u64,
    start_result: u32,
    start_core_id: i32,
    start_current_core: i32,
    start_affinity: u64,
}

static THREAD_LIFECYCLE_PROFILE: std::sync::OnceLock<
    Mutex<std::collections::BTreeMap<u64, ThreadLifecycleEntry>>,
> = std::sync::OnceLock::new();
static THREAD_LIFECYCLE_CREATE_ORDER: std::sync::atomic::AtomicU64 =
    std::sync::atomic::AtomicU64::new(0);
static THREAD_LIFECYCLE_START_ORDER: std::sync::atomic::AtomicU64 =
    std::sync::atomic::AtomicU64::new(0);

fn thread_lifecycle_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("RUZU_PROFILE_THREAD_LIFECYCLE").is_some())
}

#[allow(clippy::too_many_arguments)]
fn record_thread_create_profile(
    tid: u64,
    parent_tid: u64,
    handle: Handle,
    entry_point: u64,
    arg: u64,
    stack_bottom: u64,
    priority: i32,
    requested_core_id: i32,
    effective_core_id: i32,
) {
    if !thread_lifecycle_enabled() {
        return;
    }

    let order =
        THREAD_LIFECYCLE_CREATE_ORDER.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1;
    let map =
        THREAD_LIFECYCLE_PROFILE.get_or_init(|| Mutex::new(std::collections::BTreeMap::new()));
    map.lock().unwrap().insert(
        tid,
        ThreadLifecycleEntry {
            tid,
            parent_tid,
            handle,
            entry_point,
            arg,
            stack_bottom,
            priority,
            requested_core_id,
            effective_core_id,
            created_order: order,
            started_order: 0,
            start_result: u32::MAX,
            start_core_id: -1,
            start_current_core: -1,
            start_affinity: 0,
        },
    );
}

fn record_thread_start_profile(
    tid: u64,
    result: u32,
    core_id: i32,
    current_core: i32,
    affinity: u64,
) {
    if !thread_lifecycle_enabled() {
        return;
    }

    let order = THREAD_LIFECYCLE_START_ORDER.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1;
    let map =
        THREAD_LIFECYCLE_PROFILE.get_or_init(|| Mutex::new(std::collections::BTreeMap::new()));
    let mut guard = map.lock().unwrap();
    if let Some(entry) = guard.get_mut(&tid) {
        entry.started_order = order;
        entry.start_result = result;
        entry.start_core_id = core_id;
        entry.start_current_core = current_core;
        entry.start_affinity = affinity;
    } else {
        guard.insert(
            tid,
            ThreadLifecycleEntry {
                tid,
                parent_tid: 0,
                handle: INVALID_HANDLE,
                entry_point: 0,
                arg: 0,
                stack_bottom: 0,
                priority: 0,
                requested_core_id: -1,
                effective_core_id: -1,
                created_order: 0,
                started_order: order,
                start_result: result,
                start_core_id: core_id,
                start_current_core: current_core,
                start_affinity: affinity,
            },
        );
    }
}

pub fn dump_thread_lifecycle_profile() {
    let Some(map) = THREAD_LIFECYCLE_PROFILE.get() else {
        return;
    };
    let mut rows: Vec<ThreadLifecycleEntry> = map.lock().unwrap().values().cloned().collect();
    if rows.is_empty() {
        return;
    }
    rows.sort_by_key(|entry| entry.created_order);
    let started = rows.iter().filter(|entry| entry.started_order != 0).count();
    eprintln!(
        "[THREAD_LIFECYCLE] created={} started={}",
        rows.len(),
        started
    );
    for entry in rows {
        eprintln!(
            "[THREAD_LIFECYCLE] create#{:<3} start#{:<3} tid={:<4} parent={:<4} handle=0x{:X} entry=0x{:X} arg=0x{:X} stack=0x{:X} prio={} requested_core={} effective_core={} start_core={} current_core={} affinity=0x{:X} result=0x{:X}",
            entry.created_order,
            entry.started_order,
            entry.tid,
            entry.parent_tid,
            entry.handle,
            entry.entry_point,
            entry.arg,
            entry.stack_bottom,
            entry.priority,
            entry.requested_core_id,
            entry.effective_core_id,
            entry.start_core_id,
            entry.start_current_core,
            entry.start_affinity,
            entry.start_result,
        );
    }
}

fn should_trace_sleep_debug() -> bool {
    std::env::var_os("RUZU_TRACE_SLEEP").is_some()
}

fn should_trace_sleep_backtrace_once(tid: u64) -> bool {
    static DID_TRACE_TID73: std::sync::atomic::AtomicBool =
        std::sync::atomic::AtomicBool::new(false);
    tid == 73 && !DID_TRACE_TID73.swap(true, std::sync::atomic::Ordering::Relaxed)
}

fn zero_sleep_coerce_ns(tid: u64) -> Option<i64> {
    let raw = std::env::var("RUZU_COERCE_ZERO_SLEEP_TID").ok()?;
    let matches = raw.split(',').any(|value| {
        let value = value.trim();
        if value.is_empty() {
            return false;
        }
        let parsed = value
            .strip_prefix("0x")
            .or_else(|| value.strip_prefix("0X"))
            .and_then(|hex| u64::from_str_radix(hex, 16).ok())
            .or_else(|| value.parse::<u64>().ok());
        parsed == Some(tid)
    });
    if !matches {
        return None;
    }
    std::env::var("RUZU_COERCE_ZERO_SLEEP_NS")
        .ok()
        .and_then(|value| value.parse::<i64>().ok())
        .filter(|ns| *ns > 0)
        .or(Some(1_000_000))
}

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

fn resolve_thread_handle(system: &System, handle: Handle) -> Option<Arc<KThreadLock>> {
    if handle == PseudoHandle::CurrentThread as Handle {
        return system.current_thread();
    }

    let process = system.current_process_arc().lock().unwrap();
    let object_id = process.handle_table.get_object(handle)?;
    process.get_thread_by_object_id(object_id)
}

fn resolve_current_thread(system: &System) -> Option<Arc<KThreadLock>> {
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
    // Dump first 32 bytes of the thread's `arg` context — useful to identify
    // singleton thread contexts (like game-side audio/main-loop orchestrators
    // whose `arg` lands in .data instead of heap). Logged at info level when
    // RUZU_TRACE_THREAD_ARG=1.
    if std::env::var_os("RUZU_TRACE_THREAD_ARG").is_some() {
        let process = system.current_process_arc();
        let process = process.lock().unwrap();
        if let Some(memory) = process.get_memory().as_ref() {
            let m = memory.lock().unwrap();
            let mut bytes = String::with_capacity(64);
            for i in 0..32u64 {
                use std::fmt::Write;
                let _ = write!(bytes, "{:02x}", m.read_8(arg + i));
                if i % 4 == 3 {
                    bytes.push(' ');
                }
            }
            log::info!(
                "svc::CreateThread arg=0x{:08X} prio={} bytes=[{}]",
                arg,
                priority,
                bytes.trim()
            );
        }
    }

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

    let thread = Arc::new(KThreadLock::new(KThread::new()));
    {
        let process_state_lock = {
            let process = current_process.lock().unwrap();
            process.get_state_lock()
        };
        let _process_state_guard = KScopedLightLock::new(process_state_lock.as_ref());
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
    }

    {
        let mut new_thread = thread.lock().unwrap();
        // Cache the owning process raw pointer for scheduler-lock-protected
        // paths (matches upstream's `KProcess*` access from KThread). The
        // `KProcess` is pinned by the Arc so the pointer stays valid for the
        // thread's lifetime.
        let parent_ptr = {
            let mut process_guard = current_process.lock().unwrap();
            (&mut *process_guard) as *mut crate::hle::kernel::k_process::KProcess as usize
        };
        new_thread.set_parent_raw_ptr(parent_ptr);

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
            let parent_tid = system
                .current_thread()
                .map(|t| t.lock().unwrap().get_thread_id())
                .unwrap_or(0);
            record_thread_create_profile(
                thread_id,
                parent_tid,
                handle,
                entry_point,
                arg,
                stack_bottom,
                priority,
                requested_core_id,
                core_id,
            );
            // RUZU_SVC_TRACE_THREAD: matches zuyu's [SVC_CREATE_THREAD] line
            // so the cross-emulator diff can count creations 1:1.
            if std::env::var_os("RUZU_SVC_TRACE_THREAD").is_some() {
                log::warn!(
                    "[SVC_CREATE_THREAD] tid={} parent_tid={} entry=0x{:X} stack=0x{:X} prio={} core={}",
                    thread_id, parent_tid, entry_point, stack_bottom, priority, core_id
                );
            }
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
pub fn start_thread(system: &System, thread_handle: Handle) -> ResultCode {
    log::debug!("svc::StartThread called thread=0x{:08X}", thread_handle);

    let Some(thread) = resolve_thread_handle(system, thread_handle) else {
        return RESULT_INVALID_HANDLE;
    };

    // RUZU_PROFILE_STARTTHREAD_GAP=1 — record StartThread timestamp keyed by
    // child tid. Pairs with svc_dispatch.rs:first-SVC log to measure the JIT
    // startup gap (parent unlocks faster than child contends → no WAIT_MASK
    // → missing svcArbitrateUnlock at SVC #745, see
    // project_mk8d_thread_id_order_fix_2026_05_17).
    if std::env::var_os("RUZU_PROFILE_STARTTHREAD_GAP").is_some() {
        let child_tid = thread.lock().unwrap().get_thread_id();
        let now_ns = std::time::Instant::now();
        crate::hle::kernel::startthread_gap::set_start(child_tid, now_ns);
    }

    {
        let thread_guard = thread.lock().unwrap();
        crate::hle::kernel::k_scheduler::record_start_thread_sched_attempt(
            thread_guard.get_thread_id(),
            system.current_thread_id().unwrap_or(0),
            thread_guard.priority,
            thread_guard.get_active_core(),
            thread_guard.physical_affinity_mask.get_affinity_mask(),
        );
    }

    let result = KThread::run_thread(&thread);
    {
        let thread_guard = thread.lock().unwrap();
        let tid = thread_guard.get_thread_id();
        let state = thread_guard.get_state();
        let active_core = thread_guard.get_active_core();
        let current_core = thread_guard.get_current_core();
        let affinity = thread_guard.physical_affinity_mask.get_affinity_mask();
        crate::hle::kernel::k_scheduler::record_start_thread_sched_result(tid, result);
        record_thread_start_profile(tid, result, active_core, current_core, affinity);
        log::trace!(
            "svc::StartThread result handle=0x{:08X} tid={} state={:?} core_id={} current_core={} affinity=0x{:X} result={:#x}",
            thread_handle, tid, state, active_core, current_core, affinity, result,
        );
        // `RUZU_SVC_TRACE_THREAD=1` — counterpart to zuyu's `StartThread
        // result` info log so the cross-emulator diff can confirm each
        // created thread actually receives a StartThread.
        if std::env::var_os("RUZU_SVC_TRACE_THREAD").is_some() {
            log::warn!(
                "[SVC_START_THREAD] tid={} state={:?} core_id={} current_core={} affinity=0x{:X} result={:#x}",
                tid, state, active_core, current_core, affinity, result,
            );
        }
    }

    // EXPERIMENT (RUZU_STARTTHREAD_DELAY_US=N): pause the parent for N
    // microseconds AFTER svcStartThread returns to give the newly-RUNNABLE
    // child a chance to issue its first SVC before the parent continues.
    //
    // Background: MK8D wedge investigation traced the bug to a 157µs gap
    // between the child being scheduled and its first SVC. During this
    // gap the parent (tid=73) completes its critical section via
    // guest-userspace fastpath and releases the mutex. By the time the
    // child runs, the mutex is free → no contention → no wait_mask →
    // parent's libnx unlock skips svcArbitrateUnlock → downstream lost
    // wakeup at 0x7F2C0BCC. Zuyu's child reaches its first SVC in ~89µs
    // (fast enough to contend before parent unlocks).
    //
    // This is a *diagnostic experiment* — if pausing here unwedges MK8D,
    // the wedge is confirmed as a JIT-startup latency timing race and
    // the proper fix is to either reduce child JIT-startup overhead or
    // pre-compile entry blocks. If MK8D still wedges, the timing race
    // hypothesis is wrong and something else is going on.
    if let Ok(spec) = std::env::var("RUZU_STARTTHREAD_DELAY_US") {
        if let Ok(us) = spec.parse::<u64>() {
            if us > 0 {
                if std::env::var_os("RUZU_TRACE_STARTTHREAD_DELAY").is_some() {
                    log::info!(
                        "svc::StartThread(0x{:08X}): pausing parent for {}µs",
                        thread_handle,
                        us
                    );
                }
                std::thread::sleep(std::time::Duration::from_micros(us));
            }
        }
    }

    // EXPERIMENT (RUZU_STARTTHREAD_RESCHEDULE=1): force an actual scheduler
    // fiber handoff after making the child runnable. A host sleep above pauses
    // the whole core OS thread; it does not necessarily run the child fiber.
    // This tests whether ruzu is missing upstream's KScopedSchedulerLock
    // unlock-time RescheduleCurrentCore path after KThread::Run().
    if result == 0 && std::env::var_os("RUZU_STARTTHREAD_RESCHEDULE").is_some() {
        if let Some(kernel) = system.kernel() {
            if let Some(scheduler_arc) = kernel.current_scheduler() {
                let sched_ptr = {
                    let guard = scheduler_arc.lock().unwrap();
                    &*guard as *const crate::hle::kernel::k_scheduler::KScheduler
                        as *mut crate::hle::kernel::k_scheduler::KScheduler
                };
                unsafe {
                    crate::hle::kernel::k_scheduler::KScheduler::reschedule_current_core_raw(
                        sched_ptr,
                    );
                }
            }
        }
    }

    ResultCode::new(result)
}

/// Called when a thread exits.
pub fn exit_thread(system: &System) {
    let Some(thread) = resolve_current_thread(system) else {
        log::warn!("svc::ExitThread: current thread missing");
        return;
    };

    // Upstream `ExitThread` removes the current thread from
    // `GlobalSchedulerContext` before entering `KThread::Exit()`, so the global
    // scheduler cannot pick it again while termination is being finalized.
    let (thread_id, global_scheduler_context) = {
        let thread_guard = thread.lock().unwrap();
        (
            thread_guard.get_thread_id(),
            thread_guard
                .global_scheduler_context
                .as_ref()
                .and_then(std::sync::Weak::upgrade),
        )
    };
    if let Some(gsc) = global_scheduler_context {
        gsc.lock().unwrap().remove_thread(thread_id);
    }

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
        crate::hle::kernel::sleep_timing::record_sleep_start(current_thread_id, ns);
        if should_trace_sleep_debug() {
            if let Some(current_thread) = system.current_thread() {
                let core_index = current_thread.lock().unwrap().get_current_core().max(0) as usize;
                let process = system.current_process_arc().lock().unwrap();
                if let Some(cpu) = process.get_arm_interface(core_index) {
                    let mut ctx = crate::arm::arm_interface::ThreadContext::default();
                    cpu.get_context(&mut ctx);
                    log::info!(
                        "svc::SleepThread(sleep) ctx: tid={} pc=0x{:08X} lr=0x{:08X} sp=0x{:08X} r0=0x{:08X} r1=0x{:08X} r2=0x{:08X} r3=0x{:08X} r4=0x{:08X} r5=0x{:08X} r6=0x{:08X} r7=0x{:08X}",
                        current_thread_id,
                        ctx.pc,
                        ctx.lr,
                        ctx.sp,
                        ctx.r[0] as u32,
                        ctx.r[1] as u32,
                        ctx.r[2] as u32,
                        ctx.r[3] as u32,
                        ctx.r[4] as u32,
                        ctx.r[5] as u32,
                        ctx.r[6] as u32,
                        ctx.r[7] as u32,
                    );
                    if should_trace_sleep_backtrace_once(current_thread_id) {
                        let bt = crate::arm::debug::get_backtrace_from_context(&process, &ctx);
                        for (index, entry) in bt.iter().take(12).enumerate() {
                            log::info!(
                                "svc::SleepThread(sleep) bt[{}]: tid={} module={} addr=0x{:X} orig=0x{:X} off=0x{:X} symbol={}",
                                index,
                                current_thread_id,
                                entry.module,
                                entry.address,
                                entry.original_address,
                                entry.offset,
                                entry.name,
                            );
                        }
                    }
                }
            }
        }
        if should_trace_sleep_debug() {
            log::info!(
                "svc::SleepThread(sleep) tid={} ns={} stage=begin",
                current_thread_id,
                ns
            );
        }
        log::trace!(
            "svc::SleepThread(sleep): tid={} before get_current_hardware_tick",
            current_thread_id
        );

        let current_tick = system
            .kernel()
            .and_then(|_| crate::hle::kernel::kernel::get_current_hardware_tick())
            .expect("svc::SleepThread requires a live kernel hardware timer");
        let timeout = sleep_timeout_tick_from_ns(current_tick, ns);
        if should_trace_sleep_debug() {
            log::info!(
                "svc::SleepThread(sleep) tid={} ns={} current_tick={} timeout_tick={}",
                current_thread_id,
                ns,
                current_tick,
                timeout
            );
        }
        log::trace!(
            "svc::SleepThread(sleep): tid={} current_tick={}",
            current_thread_id,
            current_tick
        );
        log::trace!(
            "svc::SleepThread(sleep): tid={} timeout_tick={} before thread.sleep",
            current_thread_id,
            timeout
        );
        let Some(result) = crate::hle::kernel::k_thread::sleep_current_thread(timeout) else {
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
        if should_trace_sleep_debug() {
            log::info!(
                "svc::SleepThread(sleep) tid={} ns={} stage=armed",
                current_thread_id,
                ns
            );
        }

        log::trace!(
            "svc::SleepThread(sleep): tid={} ns={} -> wait armed",
            current_thread_id,
            ns
        );
        return;
    }

    let Some(current_thread_id) = system.current_thread_id() else {
        log::warn!("svc::SleepThread(yield): current thread missing");
        return;
    };
    if ns == YieldType::WithoutCoreMigration as i64 {
        if let Some(coerced_ns) = zero_sleep_coerce_ns(current_thread_id) {
            sleep_thread(system, coerced_ns);
            return;
        }
    }
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
    let scheduler_ptr = {
        let mut scheduler = sched_arc.lock().unwrap();
        &mut *scheduler as *mut crate::hle::kernel::k_scheduler::KScheduler
    };
    // Upstream calls the static KScheduler yield helpers directly; no host-side
    // scheduler mutex is held while KScopedSchedulerLock unlock callbacks run.
    // Keep the Rust Arc<Mutex<KScheduler>> out of the yield body, otherwise the
    // scheduler-lock release path deadlocks when it re-enters scheduler update.
    unsafe {
        if ns == YieldType::WithoutCoreMigration as i64 {
            (*scheduler_ptr).yield_without_core_migration(current_process, current_thread_id);
        } else if ns == YieldType::WithCoreMigration as i64 {
            (*scheduler_ptr).yield_with_core_migration(current_process, current_thread_id);
        } else if ns == YieldType::ToAnyThread as i64 {
            (*scheduler_ptr).yield_to_any_thread(current_process, current_thread_id);
        }
    }
}

/// Gets the thread context.
pub fn get_thread_context3(system: &System, out_context: u64, thread_handle: Handle) -> ResultCode {
    log::debug!(
        "svc::GetThreadContext3 called, out_context=0x{:08X}, thread_handle=0x{:X}",
        out_context,
        thread_handle
    );

    let Some(current_thread) = system.current_thread() else {
        return RESULT_INVALID_HANDLE;
    };
    let Some(thread) = resolve_thread_handle(system, thread_handle) else {
        return RESULT_INVALID_HANDLE;
    };

    if std::sync::Arc::ptr_eq(&thread, &current_thread) {
        return RESULT_BUSY;
    }

    let mut context = crate::hle::kernel::k_thread::ThreadContext::default();
    let result = thread.lock().unwrap().get_thread_context3(&mut context);
    let result = ResultCode::new(result);
    if result.is_error() {
        return result;
    }

    let context_bytes = unsafe {
        std::slice::from_raw_parts(
            (&context as *const crate::hle::kernel::k_thread::ThreadContext).cast::<u8>(),
            std::mem::size_of::<crate::hle::kernel::svc_types::ThreadContext>(),
        )
    };

    let Some(memory) = system.get_svc_memory() else {
        return RESULT_INVALID_POINTER;
    };
    if !memory
        .lock()
        .unwrap()
        .write_block(out_context, context_bytes)
    {
        return RESULT_INVALID_POINTER;
    }

    RESULT_SUCCESS
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
    use crate::hle::kernel::k_resource_limit::{
        create_resource_limit_for_process, LimitableResource,
    };
    use crate::hle::kernel::k_thread::ThreadState;
    use crate::hle::kernel::k_thread_local_page::KThreadLocalPage;
    use crate::hle::kernel::k_typed_address::KProcessAddress;
    use crate::hle::kernel::k_worker_task_manager::KWorkerTaskManager;

    fn test_system() -> System {
        let mut system = System::new_for_test();

        let mut process = KProcess::new();
        process.capabilities.core_mask = 0xF;
        process.capabilities.priority_mask = u64::MAX;
        process.flags = 0;
        process.allocate_code_memory(0x200000, 0x20000);
        process.initialize_handle_table();
        process
            .thread_local_pages
            .push(KThreadLocalPage::new(KProcessAddress::new(0x240000)));
        process.resource_limit = Some(Arc::new(Mutex::new(create_resource_limit_for_process(
            0x1_0000_0000,
        ))));

        let process = Arc::new(ProcessLock::from_value(process));
        let current_thread = Arc::new(KThreadLock::new(KThread::new()));
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
        crate::hle::kernel::kernel::set_current_emu_thread(Some(&current_thread));
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
    #[ignore = "requires a fully kernel-owned scheduler/current-core fixture"]
    fn sleep_thread_positive_requests_reschedule_and_blocks_current_thread() {
        let system = test_system();

        assert!(!system.scheduler_arc().lock().unwrap().needs_scheduling());

        {
            let scheduler_lock = crate::hle::kernel::kernel::scheduler_lock().unwrap();
            let _guard =
                crate::hle::kernel::k_scheduler_lock::KScopedSchedulerLock::new(scheduler_lock);
            sleep_thread(&system, 10);
        }

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
    #[ignore = "requires a fully kernel-owned scheduler/current-core fixture"]
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

        {
            let scheduler_lock = crate::hle::kernel::kernel::scheduler_lock().unwrap();
            let _guard =
                crate::hle::kernel::k_scheduler_lock::KScopedSchedulerLock::new(scheduler_lock);
            sleep_thread(&system, YieldType::WithoutCoreMigration as i64);
        }

        let next_after_yield = system
            .scheduler_arc()
            .lock()
            .unwrap()
            .select_next_thread_id(system.current_process_arc(), current_thread_id);
        assert_eq!(next_after_yield, Some(new_thread_id));
    }

    #[test]
    #[ignore = "requires a fully kernel-owned scheduler/current-core fixture"]
    fn yield_exits_current_thread_when_termination_was_requested() {
        let system = test_system();
        let mut handle = 0;
        let result = create_thread(&system, &mut handle, 0x201000, 0x1234, 0x260000, 44, 0);
        assert_eq!(result, RESULT_SUCCESS);
        assert_eq!(start_thread(&system, handle), RESULT_SUCCESS);

        let current_thread = system.current_thread().unwrap();
        current_thread.lock().unwrap().request_terminate();

        {
            let scheduler_lock = crate::hle::kernel::kernel::scheduler_lock().unwrap();
            let _guard =
                crate::hle::kernel::k_scheduler_lock::KScopedSchedulerLock::new(scheduler_lock);
            sleep_thread(&system, YieldType::WithoutCoreMigration as i64);
        }
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
            resource_limit
                .set_limit_value(LimitableResource::ThreadCountMax, 0)
                .unwrap();
        }

        let mut handle = INVALID_HANDLE;
        let result = create_thread(&system, &mut handle, 0x201000, 0x1234, 0x260000, 44, 0);
        assert_eq!(result, RESULT_LIMIT_REACHED);
        assert_eq!(handle, INVALID_HANDLE);
    }
}
