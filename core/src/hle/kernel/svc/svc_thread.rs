//! Port of zuyu/src/core/hle/kernel/svc/svc_thread.cpp
//! Status: Partial (kernel object-backed thread create/start/query path)
//! Derniere synchro: 2026-03-14
//!
//! SVC handlers for thread operations.

use std::sync::{Arc, Mutex, OnceLock};

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
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("RUZU_TRACE_SLEEP").is_some())
}

fn trace_sleep_tid_filter() -> Option<u64> {
    static FILTER: OnceLock<Option<u64>> = OnceLock::new();
    *FILTER.get_or_init(|| {
        std::env::var("RUZU_TRACE_SLEEP_BT_TID")
            .ok()
            .and_then(|value| value.parse::<u64>().ok())
    })
}

fn trace_sleep_ns_filter() -> Option<i64> {
    static FILTER: OnceLock<Option<i64>> = OnceLock::new();
    *FILTER.get_or_init(|| {
        std::env::var("RUZU_TRACE_SLEEP_BT_NS")
            .ok()
            .and_then(|value| value.parse::<i64>().ok())
    })
}

fn trace_sleep_filter_matches(tid: u64, ns: i64) -> bool {
    trace_sleep_tid_filter().is_none_or(|target| target == tid)
        && trace_sleep_ns_filter().is_none_or(|target| target == ns)
}

fn should_trace_sleep_backtrace_once(tid: u64, ns: i64) -> bool {
    static DID_TRACE: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);
    trace_sleep_tid_filter() == Some(tid)
        && trace_sleep_ns_filter().is_none_or(|target| target == ns)
        && !DID_TRACE.swap(true, std::sync::atomic::Ordering::Relaxed)
}

fn should_trace_mii_wait_periodic(tid: u64, ns: i64) -> bool {
    static COUNT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    static EVERY: OnceLock<Option<u64>> = OnceLock::new();
    let Some(every) = *EVERY.get_or_init(|| {
        std::env::var("RUZU_TRACE_MII_WAIT_EVERY")
            .ok()
            .and_then(|value| value.parse::<u64>().ok())
            .filter(|value| *value > 0)
    }) else {
        return false;
    };
    if !trace_sleep_filter_matches(tid, ns) {
        return false;
    }
    let count = COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1;
    count == 1 || count % every == 0
}

fn should_trace_mii_wait_changes(tid: u64, ns: i64) -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    if !*ENABLED.get_or_init(|| std::env::var_os("RUZU_TRACE_MII_WAIT_CHANGES").is_some()) {
        return false;
    }
    trace_sleep_filter_matches(tid, ns)
}

fn should_trace_mii_resource_changes(tid: u64, ns: i64) -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    if !*ENABLED.get_or_init(|| std::env::var_os("RUZU_TRACE_MII_RESOURCE_CHANGES").is_some()) {
        return false;
    }
    trace_sleep_filter_matches(tid, ns)
}

fn should_trace_sleep_callsite_changes(tid: u64) -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    if !*ENABLED.get_or_init(|| std::env::var_os("RUZU_TRACE_SLEEP_CALLSITE_CHANGES").is_some()) {
        return false;
    }
    trace_sleep_tid_filter().is_none_or(|target| target == tid)
}

fn should_trace_mk8d_logo_state(tid: u64, ns: i64) -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    if !*ENABLED.get_or_init(|| std::env::var_os("RUZU_TRACE_MK8D_LOGO_STATE").is_some()) {
        return false;
    }
    trace_sleep_filter_matches(tid, ns)
}

fn should_trace_mk8d_logo_table() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("RUZU_TRACE_MK8D_LOGO_TABLE").is_some())
}

fn mk8d_logo_caller_targets() -> &'static [u32] {
    static TARGETS: OnceLock<Vec<u32>> = OnceLock::new();
    TARGETS.get_or_init(|| {
        std::env::var("RUZU_TRACE_MK8D_LOGO_CALLER")
            .ok()
            .into_iter()
            .flat_map(|value| {
                value
                    .split(',')
                    .filter_map(|part| {
                        let part = part.trim();
                        if part.is_empty() {
                            return None;
                        }
                        u32::from_str_radix(part.trim_start_matches("0x"), 16).ok()
                    })
                    .collect::<Vec<_>>()
            })
            .collect()
    })
}

fn should_trace_mk8d_logo_caller() -> bool {
    !mk8d_logo_caller_targets().is_empty()
}

fn mk8d_logo_table_due() -> bool {
    static LAST_MS: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    let now_ms = (crate::hle::kernel::trace_format::elapsed_secs() * 1000.0) as u64;
    let last = LAST_MS.load(std::sync::atomic::Ordering::Relaxed);
    if now_ms.saturating_sub(last) < 1000 {
        return false;
    }
    LAST_MS
        .compare_exchange(
            last,
            now_ms,
            std::sync::atomic::Ordering::Relaxed,
            std::sync::atomic::Ordering::Relaxed,
        )
        .is_ok()
}

#[derive(Clone, Copy, Default, PartialEq, Eq)]
struct SleepCallsiteTrace {
    ns: i64,
    pc: u32,
    lr: u32,
    sp: u32,
    fp: u32,
    r: [u32; 9],
}

fn sleep_callsite_changed(tid: u64, current: SleepCallsiteTrace) -> bool {
    static LAST: std::sync::OnceLock<Mutex<std::collections::BTreeMap<u64, SleepCallsiteTrace>>> =
        std::sync::OnceLock::new();

    let mut last = LAST
        .get_or_init(|| Mutex::new(std::collections::BTreeMap::new()))
        .lock()
        .unwrap();
    let changed = last.get(&tid).copied() != Some(current);
    if changed {
        last.insert(tid, current);
    }
    changed
}

#[derive(Clone, Copy, Default, PartialEq, Eq)]
struct Mk8dLogoStateTrace {
    word0: u32,
    word_4300: u32,
    byte_4304: u8,
    word_4308: u32,
    byte_430a: u8,
    word_430c: u32,
    byte_430d: u8,
    word_4314: u32,
    byte_4319: u8,
    byte_4259: u8,
}

fn mk8d_logo_state_changed(base: u32, current: Mk8dLogoStateTrace) -> bool {
    static LAST: std::sync::OnceLock<Mutex<std::collections::BTreeMap<u32, Mk8dLogoStateTrace>>> =
        std::sync::OnceLock::new();

    let mut last = LAST
        .get_or_init(|| Mutex::new(std::collections::BTreeMap::new()))
        .lock()
        .unwrap();
    let changed = last.get(&base).copied() != Some(current);
    if changed {
        last.insert(base, current);
    }
    changed
}

#[derive(Clone, Copy, Default, PartialEq, Eq)]
struct Mk8dLogoCallerTrace {
    word_0590: u32,
    word_0594: u32,
    word_0598: u32,
    byte_059c: u8,
    word_05a0: u32,
    word_05a4: u32,
}

fn mk8d_logo_caller_changed(caller: u32, current: Mk8dLogoCallerTrace) -> bool {
    static LAST: std::sync::OnceLock<Mutex<std::collections::BTreeMap<u32, Mk8dLogoCallerTrace>>> =
        std::sync::OnceLock::new();

    let mut last = LAST
        .get_or_init(|| Mutex::new(std::collections::BTreeMap::new()))
        .lock()
        .unwrap();
    let changed = last.get(&caller).copied() != Some(current);
    if changed {
        last.insert(caller, current);
    }
    changed
}

fn mk8d_logo_caller_due(caller: u32) -> bool {
    static LAST_MS: std::sync::OnceLock<Mutex<std::collections::BTreeMap<u32, u64>>> =
        std::sync::OnceLock::new();

    let now_ms = (crate::hle::kernel::trace_format::elapsed_secs() * 1000.0) as u64;
    let mut last = LAST_MS
        .get_or_init(|| Mutex::new(std::collections::BTreeMap::new()))
        .lock()
        .unwrap();
    let entry = last.entry(caller).or_default();
    if now_ms.saturating_sub(*entry) < 1000 {
        return false;
    }
    *entry = now_ms;
    true
}

#[derive(Clone, Copy, Default, PartialEq, Eq)]
struct MiiWaitEntryTrace {
    entry: u32,
    flag_1d9: u8,
    word_1d8: u32,
    pending: [u32; 3],
}

fn mii_wait_entry_changed(index: usize, current: MiiWaitEntryTrace) -> bool {
    static LAST: std::sync::OnceLock<Mutex<[Option<MiiWaitEntryTrace>; 13]>> =
        std::sync::OnceLock::new();

    let mut last = LAST.get_or_init(|| Mutex::new([None; 13])).lock().unwrap();
    let changed = last[index] != Some(current);
    if changed {
        last[index] = Some(current);
    }
    changed
}

#[derive(Clone, Copy, Default, PartialEq, Eq)]
struct MiiResourceTrace {
    resource: u32,
    resource_state: u32,
    resource_data: u32,
    resource_data_word0: u32,
    flag_bc: u8,
    flag_bd: u8,
}

fn mii_resource_changed(category: u64, resource_index: u64, current: MiiResourceTrace) -> bool {
    static LAST: std::sync::OnceLock<
        Mutex<std::collections::BTreeMap<(u64, u64), MiiResourceTrace>>,
    > = std::sync::OnceLock::new();

    let mut last = LAST
        .get_or_init(|| Mutex::new(std::collections::BTreeMap::new()))
        .lock()
        .unwrap();
    let key = (category, resource_index);
    let changed = last.get(&key).copied() != Some(current);
    if changed {
        last.insert(key, current);
    }
    changed
}

#[derive(Clone, Copy, Default, PartialEq, Eq)]
struct MiiQueueTrace {
    manager: u32,
    queue: u32,
    capacity: u32,
    read_index: u32,
    count: u32,
    head: [u32; 3],
}

fn mii_queue_changed(current: MiiQueueTrace) -> bool {
    static LAST: std::sync::OnceLock<Mutex<Option<MiiQueueTrace>>> = std::sync::OnceLock::new();

    let mut last = LAST.get_or_init(|| Mutex::new(None)).lock().unwrap();
    let changed = *last != Some(current);
    if changed {
        *last = Some(current);
    }
    changed
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

fn trace_thread_core_mask(
    stage: u64,
    caller_tid: u64,
    target: Option<&Arc<KThreadLock>>,
    handle: Handle,
    core_id: i32,
    affinity_mask: u64,
    result: ResultCode,
) {
    if !common::trace::is_enabled(common::trace::cat::THREAD_CORE_MASK) {
        return;
    }
    let (target_tid, state, active_core, current_core, pinned, waiter_count) =
        if let Some(thread) = target {
            let thread = thread.lock().unwrap();
            (
                thread.get_thread_id(),
                thread.get_raw_state().bits() as u64,
                thread.get_active_core() as u64,
                thread.get_current_core() as u64,
                thread.stack_parameters.is_pinned as u64,
                thread.pinned_waiter_list.len() as u64,
            )
        } else {
            (0, 0, u64::MAX, u64::MAX, 0, 0)
        };
    common::trace::emit_raw(
        common::trace::cat::THREAD_CORE_MASK,
        &[
            stage,
            caller_tid,
            target_tid,
            handle as u64,
            core_id as u32 as u64,
            affinity_mask,
            state,
            active_core,
            current_core,
            result.get_inner_value() as u64,
            pinned,
            waiter_count,
        ],
    );
}

#[allow(clippy::too_many_arguments)]
fn trace_create_thread(
    stage: u64,
    caller_tid: u64,
    entry_point: u64,
    arg: u64,
    stack_bottom: u64,
    priority: i32,
    requested_core_id: i32,
    effective_core_id: i32,
    process_core_mask: u64,
    result: ResultCode,
    out_handle: Handle,
    extra: u64,
) {
    if !common::trace::is_enabled(common::trace::cat::CREATE_THREAD) {
        return;
    }
    common::trace::emit_raw(
        common::trace::cat::CREATE_THREAD,
        &[
            stage,
            caller_tid,
            entry_point,
            arg,
            stack_bottom,
            priority as u32 as u64,
            requested_core_id as u32 as u64,
            effective_core_id as u32 as u64,
            process_core_mask,
            result.get_inner_value() as u64,
            out_handle as u64,
            extra,
        ],
    );
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
    let caller_tid = system.current_thread_id().unwrap_or(0);
    trace_create_thread(
        1,
        caller_tid,
        entry_point,
        arg,
        stack_bottom,
        priority,
        requested_core_id,
        core_id,
        0,
        RESULT_SUCCESS,
        0,
        0,
    );
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
        let process_core_mask = process.get_core_mask();

        if !is_valid_virtual_core_id(core_id) {
            trace_create_thread(
                2,
                caller_tid,
                entry_point,
                arg,
                stack_bottom,
                priority,
                requested_core_id,
                core_id,
                process_core_mask,
                RESULT_INVALID_CORE_ID,
                0,
                0,
            );
            return RESULT_INVALID_CORE_ID;
        }
        if ((1u64 << core_id) & process_core_mask) == 0 {
            trace_create_thread(
                2,
                caller_tid,
                entry_point,
                arg,
                stack_bottom,
                priority,
                requested_core_id,
                core_id,
                process_core_mask,
                RESULT_INVALID_CORE_ID,
                0,
                1,
            );
            return RESULT_INVALID_CORE_ID;
        }
        if !(HIGHEST_THREAD_PRIORITY..=LOWEST_THREAD_PRIORITY).contains(&priority) {
            trace_create_thread(
                3,
                caller_tid,
                entry_point,
                arg,
                stack_bottom,
                priority,
                requested_core_id,
                core_id,
                process_core_mask,
                RESULT_INVALID_PRIORITY,
                0,
                0,
            );
            return RESULT_INVALID_PRIORITY;
        }
        if !process.check_thread_priority(priority) {
            trace_create_thread(
                3,
                caller_tid,
                entry_point,
                arg,
                stack_bottom,
                priority,
                requested_core_id,
                core_id,
                process_core_mask,
                RESULT_INVALID_PRIORITY,
                0,
                1,
            );
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
        trace_create_thread(
            4,
            caller_tid,
            entry_point,
            arg,
            stack_bottom,
            priority,
            requested_core_id,
            core_id,
            0,
            RESULT_LIMIT_REACHED,
            0,
            0,
        );
        return RESULT_LIMIT_REACHED;
    }

    let object_id = system.kernel().unwrap().create_new_object_id() as u64;
    let thread_id = system.kernel().unwrap().create_new_thread_id();
    let process_is_64bit = current_process.lock().unwrap().is_64bit();

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
            process_is_64bit,
            guest_thread_func,
        );
        if result != RESULT_SUCCESS.get_inner_value() {
            trace_create_thread(
                5,
                caller_tid,
                entry_point,
                arg,
                stack_bottom,
                priority,
                requested_core_id,
                core_id,
                0,
                ResultCode::new(result),
                0,
                thread_id,
            );
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
        trace_create_thread(
            6,
            caller_tid,
            entry_point,
            arg,
            stack_bottom,
            priority,
            requested_core_id,
            core_id,
            process.get_core_mask(),
            ResultCode::new(handle_table_result),
            0,
            thread_id,
        );
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
            trace_create_thread(
                7,
                caller_tid,
                entry_point,
                arg,
                stack_bottom,
                priority,
                requested_core_id,
                core_id,
                process.get_core_mask(),
                RESULT_SUCCESS,
                handle,
                thread_id,
            );
            RESULT_SUCCESS
        }
        Err(_) => {
            process.unregister_thread_object_by_object_id(object_id);
            let _ = process.delete_thread_local_region(thread_tls_address);
            trace_create_thread(
                8,
                caller_tid,
                entry_point,
                arg,
                stack_bottom,
                priority,
                requested_core_id,
                core_id,
                process.get_core_mask(),
                RESULT_OUT_OF_HANDLES,
                0,
                thread_id,
            );
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
        let trace_sleep_backtrace = should_trace_sleep_backtrace_once(current_thread_id, ns);
        let trace_mii_wait_periodic = should_trace_mii_wait_periodic(current_thread_id, ns);
        let trace_mii_wait_changes = should_trace_mii_wait_changes(current_thread_id, ns);
        let trace_mii_resource_changes = should_trace_mii_resource_changes(current_thread_id, ns);
        let trace_sleep_callsite_changes = should_trace_sleep_callsite_changes(current_thread_id);
        let trace_mk8d_logo_state = should_trace_mk8d_logo_state(current_thread_id, ns);
        let trace_mk8d_logo_caller = should_trace_mk8d_logo_caller();
        if should_trace_sleep_debug()
            || trace_sleep_backtrace
            || trace_sleep_callsite_changes
            || trace_mk8d_logo_state
            || trace_mk8d_logo_caller
            || trace_mii_wait_periodic
            || trace_mii_wait_changes
            || trace_mii_resource_changes
        {
            if let Some(current_thread) = system.current_thread() {
                let core_index = current_thread.lock().unwrap().get_current_core().max(0) as usize;
                let process = system.current_process_arc().lock().unwrap();
                if let Some(cpu) = process.get_arm_interface(core_index) {
                    let mut ctx = crate::arm::arm_interface::ThreadContext::default();
                    cpu.get_context(&mut ctx);
                    if should_trace_sleep_debug() {
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
                    }
                    if trace_mk8d_logo_state {
                        if let Some(memory) = process.get_memory() {
                            let memory = memory.lock().unwrap();
                            let stack_words = std::array::from_fn::<_, 16, _>(|index| {
                                let addr = ctx.sp + index as u64 * 4;
                                if addr >= 0x1000 && memory.is_valid_virtual_address(addr) {
                                    memory.read_32(addr)
                                } else {
                                    0
                                }
                            });
                            let mut candidates = std::collections::BTreeSet::new();
                            for value in ctx
                                .r
                                .iter()
                                .take(13)
                                .copied()
                                .chain(stack_words.into_iter().map(u64::from))
                            {
                                if value >= 0x1000 {
                                    candidates.insert(value as u32);
                                }
                                if value >= 0x5314 {
                                    candidates.insert((value - 0x4314) as u32);
                                }
                            }
                            for candidate in candidates {
                                let base = candidate as u64;
                                if !memory.is_valid_virtual_address_range(base, 0x431A) {
                                    continue;
                                }
                                let current = Mk8dLogoStateTrace {
                                    word0: memory.read_32(base),
                                    word_4300: memory.read_32(base + 0x4300),
                                    byte_4304: memory.read_8(base + 0x4304),
                                    word_4308: memory.read_32(base + 0x4308),
                                    byte_430a: memory.read_8(base + 0x430A),
                                    word_430c: memory.read_32(base + 0x430C),
                                    byte_430d: memory.read_8(base + 0x430D),
                                    word_4314: memory.read_32(base + 0x4314),
                                    byte_4319: memory.read_8(base + 0x4319),
                                    byte_4259: memory.read_8(base + 0x4259),
                                };
                                let plausible_logo_object = (0x00E0_0000..=0x00F5_FFFF)
                                    .contains(&current.word0)
                                    && current.byte_4259 <= 1
                                    && current.byte_4319 <= 1
                                    && current.word_4314 <= 0x40;
                                let changed = plausible_logo_object
                                    && mk8d_logo_state_changed(candidate, current);
                                if changed {
                                    log::warn!(
                                        "svc::SleepThread(sleep) mk8d_logo_state: tid={} ns={} base=0x{:08X} word0=0x{:08X} w4300=0x{:08X} b4304=0x{:02X} w4308=0x{:08X} b430A=0x{:02X} w430C=0x{:08X} b430D=0x{:02X} w4314=0x{:08X} b4319=0x{:02X} b4259=0x{:02X}",
                                        current_thread_id,
                                        ns,
                                        candidate,
                                        current.word0,
                                        current.word_4300,
                                        current.byte_4304,
                                        current.word_4308,
                                        current.byte_430a,
                                        current.word_430c,
                                        current.byte_430d,
                                        current.word_4314,
                                        current.byte_4319,
                                        current.byte_4259,
                                    );
                                }
                                if plausible_logo_object
                                    && should_trace_mk8d_logo_table()
                                    && matches!(current.word_4314, 7 | 0xA)
                                    && current.word_4308 & 0x0010_0000 == 0
                                    && (changed || mk8d_logo_table_due())
                                {
                                    let mut entries = String::new();
                                    for index in 0..12u64 {
                                        let handle = memory.read_32(base + 0x180 + index * 4);
                                        let offset = memory.read_32(base + 0xCC + index * 0x10);
                                        let res_addr = base
                                            .wrapping_add((offset as u64) * 4)
                                            .wrapping_add(index * 0x10)
                                            .wrapping_add(0xC4);
                                        let resource =
                                            if memory.is_valid_virtual_address_range(res_addr, 4) {
                                                memory.read_32(res_addr)
                                            } else {
                                                0
                                            };
                                        let data = if resource >= 0x1000
                                            && memory.is_valid_virtual_address_range(
                                                resource as u64 + 0x8AC,
                                                4,
                                            ) {
                                            memory.read_32(resource as u64 + 0x8AC)
                                        } else {
                                            0
                                        };
                                        use std::fmt::Write;
                                        let _ = write!(
                                            entries,
                                            " [{}]h=0x{:08X}/off=0x{:08X}/res=0x{:08X}/data=0x{:08X}",
                                            index, handle, offset, resource, data
                                        );
                                    }
                                    log::warn!(
                                        "svc::SleepThread(sleep) mk8d_logo_table: tid={} base=0x{:08X}{}",
                                        current_thread_id,
                                        candidate,
                                        entries,
                                    );
                                }
                            }
                        }
                    }
                    if trace_mk8d_logo_caller {
                        if let Some(memory) = process.get_memory() {
                            let memory = memory.lock().unwrap();
                            for &caller in mk8d_logo_caller_targets() {
                                let addr = caller as u64;
                                if !memory.is_valid_virtual_address_range(addr + 0x590, 0x18) {
                                    continue;
                                }
                                let current = Mk8dLogoCallerTrace {
                                    word_0590: memory.read_32(addr + 0x590),
                                    word_0594: memory.read_32(addr + 0x594),
                                    word_0598: memory.read_32(addr + 0x598),
                                    byte_059c: memory.read_8(addr + 0x59C),
                                    word_05a0: memory.read_32(addr + 0x5A0),
                                    word_05a4: memory.read_32(addr + 0x5A4),
                                };
                                if mk8d_logo_caller_changed(caller, current)
                                    || mk8d_logo_caller_due(caller)
                                {
                                    log::warn!(
                                        "svc::SleepThread(sleep) mk8d_logo_caller: tid={} ns={} caller=0x{:08X} pc=0x{:08X} lr=0x{:08X} w0590=0x{:08X} w0594=0x{:08X} w0598=0x{:08X} b059C=0x{:02X} w05A0=0x{:08X} w05A4=0x{:08X}",
                                        current_thread_id,
                                        ns,
                                        caller,
                                        ctx.pc,
                                        ctx.lr,
                                        current.word_0590,
                                        current.word_0594,
                                        current.word_0598,
                                        current.byte_059c,
                                        current.word_05a0,
                                        current.word_05a4,
                                    );
                                }
                            }
                        }
                    }
                    if trace_sleep_callsite_changes {
                        let current = SleepCallsiteTrace {
                            ns,
                            pc: ctx.pc as u32,
                            lr: ctx.lr as u32,
                            sp: ctx.sp as u32,
                            fp: ctx.fp as u32,
                            r: [
                                ctx.r[4] as u32,
                                ctx.r[5] as u32,
                                ctx.r[6] as u32,
                                ctx.r[7] as u32,
                                ctx.r[8] as u32,
                                ctx.r[9] as u32,
                                ctx.r[10] as u32,
                                ctx.r[11] as u32,
                                ctx.r[12] as u32,
                            ],
                        };
                        if sleep_callsite_changed(current_thread_id, current) {
                            log::warn!(
                                "svc::SleepThread(sleep) callsite_change: tid={} ns={} pc=0x{:08X} lr=0x{:08X} sp=0x{:08X} fp=0x{:08X} r4-r12=[{:08X},{:08X},{:08X},{:08X},{:08X},{:08X},{:08X},{:08X},{:08X}]",
                                current_thread_id,
                                ns,
                                ctx.pc,
                                ctx.lr,
                                ctx.sp,
                                ctx.fp,
                                current.r[0],
                                current.r[1],
                                current.r[2],
                                current.r[3],
                                current.r[4],
                                current.r[5],
                                current.r[6],
                                current.r[7],
                                current.r[8],
                            );
                            if std::env::var_os("RUZU_TRACE_SLEEP_CALLSITE_STACK").is_some() {
                                if let Some(memory) = process.get_memory() {
                                    let memory = memory.lock().unwrap();
                                    let words = std::array::from_fn::<_, 16, _>(|index| {
                                        let addr = ctx.sp + index as u64 * 4;
                                        if addr >= 0x1000 {
                                            memory.read_32(addr)
                                        } else {
                                            0
                                        }
                                    });
                                    log::warn!(
                                        "svc::SleepThread(sleep) callsite_stack: tid={} sp=0x{:08X} words=[{:08X},{:08X},{:08X},{:08X},{:08X},{:08X},{:08X},{:08X},{:08X},{:08X},{:08X},{:08X},{:08X},{:08X},{:08X},{:08X}]",
                                        current_thread_id,
                                        ctx.sp,
                                        words[0],
                                        words[1],
                                        words[2],
                                        words[3],
                                        words[4],
                                        words[5],
                                        words[6],
                                        words[7],
                                        words[8],
                                        words[9],
                                        words[10],
                                        words[11],
                                        words[12],
                                        words[13],
                                        words[14],
                                            words[15],
                                        );
                                    if std::env::var_os("RUZU_TRACE_SLEEP_CALLSITE_OBJECTS")
                                        .is_some()
                                    {
                                        let mut candidates = std::collections::BTreeSet::new();
                                        for value in current.r {
                                            if value >= 0x1000 {
                                                candidates.insert(value);
                                            }
                                        }
                                        for value in [
                                            words[2], words[4], words[6], words[7], words[8],
                                            words[10], words[12],
                                        ] {
                                            if value >= 0x1000 {
                                                candidates.insert(value);
                                            }
                                        }
                                        for candidate in candidates {
                                            let base = candidate as u64;
                                            if !memory.is_valid_virtual_address_range(base, 0x431A)
                                            {
                                                continue;
                                            }
                                            let word0 = memory.read_32(base);
                                            let word_4300 = memory.read_32(base + 0x4300);
                                            let byte_4304 = memory.read_8(base + 0x4304);
                                            let word_4308 = memory.read_32(base + 0x4308);
                                            let byte_430a = memory.read_8(base + 0x430A);
                                            let ptr_4314 = memory.read_32(base + 0x4314);
                                            let byte_4319 = memory.read_8(base + 0x4319);
                                            let byte_4259 = memory.read_8(base + 0x4259);
                                            log::warn!(
                                                "svc::SleepThread(sleep) callsite_object: tid={} base=0x{:08X} word0=0x{:08X} w4300=0x{:08X} b4304=0x{:02X} w4308=0x{:08X} b430A=0x{:02X} ptr4314=0x{:08X} b4319=0x{:02X} b4259=0x{:02X}",
                                                current_thread_id,
                                                candidate,
                                                word0,
                                                word_4300,
                                                byte_4304,
                                                word_4308,
                                                byte_430a,
                                                ptr_4314,
                                                byte_4319,
                                                byte_4259,
                                            );
                                        }
                                    }
                                }
                            }
                            if std::env::var_os("RUZU_TRACE_SLEEP_CALLSITE_BT").is_some() {
                                let bt =
                                    crate::arm::debug::get_backtrace_from_context(&process, &ctx);
                                for (index, entry) in bt.iter().take(12).enumerate() {
                                    log::warn!(
                                        "svc::SleepThread(sleep) callsite_bt[{}]: tid={} module={} addr=0x{:X} orig=0x{:X} off=0x{:X} symbol={}",
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
                    if trace_sleep_backtrace
                        || trace_mii_wait_periodic
                        || trace_mii_wait_changes
                        || trace_mii_resource_changes
                    {
                        if std::env::var_os("RUZU_A32_TRACE_AFTER_SLEEP_BT").is_some() {
                            crate::arm::dynarmic::arm_dynarmic_32::arm_trace_after_watch();
                        }
                        if trace_sleep_backtrace {
                            log::info!(
                                "svc::SleepThread(sleep) trace_ctx: tid={} pc=0x{:08X} lr=0x{:08X} sp=0x{:08X} fp=0x{:08X} ns={} r0=0x{:08X} r1=0x{:08X} r2=0x{:08X} r3=0x{:08X} r4=0x{:08X} r5=0x{:08X} r6=0x{:08X} r7=0x{:08X} r8=0x{:08X} r9=0x{:08X} r10=0x{:08X} r11=0x{:08X} r12=0x{:08X}",
                                current_thread_id,
                                ctx.pc,
                                ctx.lr,
                                ctx.sp,
                                ctx.fp,
                                ns,
                                ctx.r[0] as u32,
                                ctx.r[1] as u32,
                                ctx.r[2] as u32,
                                ctx.r[3] as u32,
                                ctx.r[4] as u32,
                                ctx.r[5] as u32,
                                ctx.r[6] as u32,
                                ctx.r[7] as u32,
                                ctx.r[8] as u32,
                                ctx.r[9] as u32,
                                ctx.r[10] as u32,
                                ctx.r[11] as u32,
                                ctx.r[12] as u32,
                            );
                        }
                        if let Some(memory) = process.get_memory() {
                            let memory = memory.lock().unwrap();
                            let owner = ctx.r[5] as u64;
                            let state = ctx.r[4] as u64;
                            if owner >= 0x1000 {
                                let table = memory.read_32(owner + 0x34) as u64;
                                if trace_sleep_backtrace || trace_mii_wait_periodic {
                                    log::info!(
                                        "svc::SleepThread(sleep) mii_wait: tid={} owner=0x{:08X} table=0x{:08X} state=0x{:08X}",
                                        current_thread_id,
                                        owner as u32,
                                        table as u32,
                                        state as u32,
                                    );
                                }
                                if table >= 0x1000 {
                                    for index in 0..13u64 {
                                        let entry = memory.read_32(table + index * 4) as u64;
                                        let flag = if entry >= 0x1000 {
                                            memory.read_8(entry + 0x1D9)
                                        } else {
                                            0
                                        };
                                        let word_1d8 = if entry >= 0x1000 {
                                            memory.read_32(entry + 0x1D8)
                                        } else {
                                            0
                                        };
                                        let pending_0 = if state >= 0x1000 {
                                            memory.read_32(state + index * 0x20)
                                        } else {
                                            0
                                        };
                                        let pending_1 = if state >= 0x1000 {
                                            memory.read_32(state + 0x1A0 + index * 0x20)
                                        } else {
                                            0
                                        };
                                        let pending_2 = if state >= 0x1000 {
                                            memory.read_32(state + 0x340 + index * 0x20)
                                        } else {
                                            0
                                        };
                                        let changed = trace_mii_wait_changes
                                            && mii_wait_entry_changed(
                                                index as usize,
                                                MiiWaitEntryTrace {
                                                    entry: entry as u32,
                                                    flag_1d9: flag,
                                                    word_1d8,
                                                    pending: [pending_0, pending_1, pending_2],
                                                },
                                            );
                                        if trace_sleep_backtrace || trace_mii_wait_periodic {
                                            log::warn!(
                                                "svc::SleepThread(sleep) mii_wait_entry: tid={} index={} entry=0x{:08X} flag_1d9=0x{:02X} word_1d8=0x{:08X} pending=[0x{:08X},0x{:08X},0x{:08X}]",
                                                current_thread_id,
                                                index,
                                                entry as u32,
                                                flag,
                                                word_1d8,
                                                pending_0,
                                                pending_1,
                                                pending_2,
                                            );
                                        } else if changed {
                                            log::warn!(
                                                "svc::SleepThread(sleep) mii_wait_change: tid={} owner=0x{:08X} table=0x{:08X} state=0x{:08X} index={} entry=0x{:08X} flag_1d9=0x{:02X} word_1d8=0x{:08X} pending=[0x{:08X},0x{:08X},0x{:08X}]",
                                                current_thread_id,
                                                owner as u32,
                                                table as u32,
                                                state as u32,
                                                index,
                                                entry as u32,
                                                flag,
                                                word_1d8,
                                                pending_0,
                                                pending_1,
                                                pending_2,
                                            );
                                        }
                                        if std::env::var_os("RUZU_TRACE_MII_WAIT_STATE_WORDS")
                                            .is_some()
                                            && (trace_sleep_backtrace
                                                || trace_mii_wait_periodic
                                                || changed)
                                        {
                                            for (variant, base) in [
                                                (0u64, state + index * 0x20),
                                                (1u64, state + 0x1A0 + index * 0x20),
                                                (2u64, state + 0x340 + index * 0x20),
                                            ] {
                                                let pending = match variant {
                                                    0 => pending_0,
                                                    1 => pending_1,
                                                    _ => pending_2,
                                                };
                                                if pending == 0 || base < 0x1000 {
                                                    continue;
                                                }
                                                log::info!(
                                                    "svc::SleepThread(sleep) mii_wait_state: tid={} index={} variant={} base=0x{:08X} words=[{:08X},{:08X},{:08X},{:08X},{:08X},{:08X},{:08X},{:08X}]",
                                                    current_thread_id,
                                                    index,
                                                    variant,
                                                    base as u32,
                                                    memory.read_32(base),
                                                    memory.read_32(base + 4),
                                                    memory.read_32(base + 8),
                                                    memory.read_32(base + 12),
                                                    memory.read_32(base + 16),
                                                    memory.read_32(base + 20),
                                                    memory.read_32(base + 24),
                                                    memory.read_32(base + 28),
                                                );
                                                if std::env::var_os(
                                                    "RUZU_TRACE_MII_WAIT_RESOURCE_SLOT",
                                                )
                                                .is_some()
                                                {
                                                    let category = match variant {
                                                        0 => 7u64,
                                                        1 => 8u64,
                                                        _ => 10u64,
                                                    };
                                                    let variant_kind = memory.read_32(base + 4);
                                                    let resource_index = index
                                                        + if variant_kind == 1 { 0 } else { 13 };
                                                    // MK8D v0: `0x9680E0` obtains the resource
                                                    // manager via `0x94380C`, then returns
                                                    // `[system+0xE0+3*4]`. This is debug-only and
                                                    // mirrors the guest code path being traced.
                                                    let global_slot = 0x00ED3818u64;
                                                    let global_holder_ptr =
                                                        memory.read_32(global_slot) as u64;
                                                    let global_holder =
                                                        if global_holder_ptr >= 0x1000 {
                                                            memory.read_32(global_holder_ptr) as u64
                                                        } else {
                                                            0
                                                        };
                                                    let system_ptr = if global_holder >= 0x1000 {
                                                        memory.read_32(global_holder + 0x14) as u64
                                                    } else {
                                                        0
                                                    };
                                                    let manager = if system_ptr >= 0x1000 {
                                                        memory.read_32(system_ptr + 0xE0 + 3 * 4)
                                                            as u64
                                                    } else {
                                                        0
                                                    };
                                                    let resource_table = if manager >= 0x1000 {
                                                        memory.read_32(manager + 0x10C) as u64
                                                    } else {
                                                        0
                                                    };
                                                    let category_base = if resource_table >= 0x1000
                                                    {
                                                        resource_table + 0x34 + category * 56
                                                    } else {
                                                        0
                                                    };
                                                    let slot_count = if category_base >= 0x1000 {
                                                        memory.read_32(category_base + 4)
                                                    } else {
                                                        0
                                                    };
                                                    let slot_array = if category_base >= 0x1000 {
                                                        memory.read_32(category_base + 8) as u64
                                                    } else {
                                                        0
                                                    };
                                                    let slot = if slot_array >= 0x1000 {
                                                        slot_array + resource_index * 72
                                                    } else {
                                                        0
                                                    };
                                                    let resource = if slot >= 0x1000 {
                                                        memory.read_32(slot + 4) as u64
                                                    } else {
                                                        0
                                                    };
                                                    let resource_data = if resource >= 0x1000 {
                                                        memory.read_32(resource + 4) as u64
                                                    } else {
                                                        0
                                                    };
                                                    log::info!(
                                                        "svc::SleepThread(sleep) mii_wait_resource: tid={} index={} variant={} category={} resource_index={} manager=0x{:08X} table=0x{:08X} slot_count={} slot_array=0x{:08X} slot=0x{:08X} resource=0x{:08X} res_data=0x{:08X} res_data0=0x{:08X} res_words=[{:08X},{:08X},{:08X},{:08X},{:08X}] res_flags=[bc={:02X},bd={:02X}]",
                                                        current_thread_id,
                                                        index,
                                                        variant,
                                                        category,
                                                        resource_index,
                                                        manager as u32,
                                                        resource_table as u32,
                                                        slot_count,
                                                        slot_array as u32,
                                                        slot as u32,
                                                        resource as u32,
                                                        resource_data as u32,
                                                        if resource_data >= 0x1000 { memory.read_32(resource_data) } else { 0 },
                                                        if resource >= 0x1000 { memory.read_32(resource) } else { 0 },
                                                        if resource >= 0x1000 { memory.read_32(resource + 4) } else { 0 },
                                                        if resource >= 0x1000 { memory.read_32(resource + 0xC0) } else { 0 },
                                                        if resource >= 0x1000 { memory.read_32(resource + 0xC4) } else { 0 },
                                                        if resource >= 0x1000 { memory.read_32(resource + 0xC8) } else { 0 },
                                                        if resource >= 0x1000 { memory.read_8(resource + 0xBC) } else { 0 },
                                                        if resource >= 0x1000 { memory.read_8(resource + 0xBD) } else { 0 },
                                                    );
                                                }
                                            }
                                        }
                                        if trace_mii_resource_changes {
                                            for (variant, base, pending) in [
                                                (0u64, state + index * 0x20, pending_0),
                                                (1u64, state + 0x1A0 + index * 0x20, pending_1),
                                                (2u64, state + 0x340 + index * 0x20, pending_2),
                                            ] {
                                                if pending == 0 || base < 0x1000 {
                                                    continue;
                                                }
                                                let category = match variant {
                                                    0 => 7u64,
                                                    1 => 8u64,
                                                    _ => 10u64,
                                                };
                                                let variant_kind = memory.read_32(base + 4);
                                                let resource_index =
                                                    index + if variant_kind == 1 { 0 } else { 13 };
                                                // MK8D v0 debug path: mirrors guest `0x9680E0`
                                                // resource-manager lookup used by the code under
                                                // investigation. This stays env-gated and must not
                                                // become generic emulator behavior.
                                                let global_slot = 0x00ED3818u64;
                                                let global_holder_ptr =
                                                    memory.read_32(global_slot) as u64;
                                                let global_holder = if global_holder_ptr >= 0x1000 {
                                                    memory.read_32(global_holder_ptr) as u64
                                                } else {
                                                    0
                                                };
                                                let system_ptr = if global_holder >= 0x1000 {
                                                    memory.read_32(global_holder + 0x14) as u64
                                                } else {
                                                    0
                                                };
                                                let manager = if system_ptr >= 0x1000 {
                                                    memory.read_32(system_ptr + 0xE0 + 3 * 4) as u64
                                                } else {
                                                    0
                                                };
                                                let resource_table = if manager >= 0x1000 {
                                                    memory.read_32(manager + 0x10C) as u64
                                                } else {
                                                    0
                                                };
                                                if manager >= 0x1000 {
                                                    let queue = memory.read_32(manager + 0x3B0);
                                                    let capacity = memory.read_32(manager + 0x3B4);
                                                    let read_index =
                                                        memory.read_32(manager + 0x3B8);
                                                    let count = memory.read_32(manager + 0x3BC);
                                                    let mut head = [0u32; 3];
                                                    if queue >= 0x1000
                                                        && capacity > 0
                                                        && capacity < 0x1000
                                                    {
                                                        for (slot, value) in
                                                            head.iter_mut().enumerate()
                                                        {
                                                            let index = (read_index + slot as u32)
                                                                % capacity;
                                                            *value = memory.read_32(
                                                                queue as u64 + index as u64 * 4,
                                                            );
                                                        }
                                                    }
                                                    let current = MiiQueueTrace {
                                                        manager: manager as u32,
                                                        queue,
                                                        capacity,
                                                        read_index,
                                                        count,
                                                        head,
                                                    };
                                                    if mii_queue_changed(current) {
                                                        log::warn!(
                                                            "svc::SleepThread(sleep) mii_queue_change: tid={} manager=0x{:08X} queue=0x{:08X} cap={} read={} count={} head=[0x{:08X},0x{:08X},0x{:08X}]",
                                                            current_thread_id,
                                                            manager as u32,
                                                            queue,
                                                            capacity,
                                                            read_index,
                                                            count,
                                                            head[0],
                                                            head[1],
                                                            head[2],
                                                        );
                                                    }
                                                }
                                                let category_base = if resource_table >= 0x1000 {
                                                    resource_table + 0x34 + category * 56
                                                } else {
                                                    0
                                                };
                                                let slot_array = if category_base >= 0x1000 {
                                                    memory.read_32(category_base + 8) as u64
                                                } else {
                                                    0
                                                };
                                                let slot = if slot_array >= 0x1000 {
                                                    slot_array + resource_index * 72
                                                } else {
                                                    0
                                                };
                                                let resource = if slot >= 0x1000 {
                                                    memory.read_32(slot + 4) as u64
                                                } else {
                                                    0
                                                };
                                                let resource_state = if resource >= 0x1000 {
                                                    memory.read_32(resource)
                                                } else {
                                                    0
                                                };
                                                let resource_data = if resource >= 0x1000 {
                                                    memory.read_32(resource + 4) as u64
                                                } else {
                                                    0
                                                };
                                                let resource_data_word0 = if resource_data >= 0x1000
                                                {
                                                    memory.read_32(resource_data)
                                                } else {
                                                    0
                                                };
                                                let flag_bc = if resource >= 0x1000 {
                                                    memory.read_8(resource + 0xBC)
                                                } else {
                                                    0
                                                };
                                                let flag_bd = if resource >= 0x1000 {
                                                    memory.read_8(resource + 0xBD)
                                                } else {
                                                    0
                                                };
                                                let current = MiiResourceTrace {
                                                    resource: resource as u32,
                                                    resource_state,
                                                    resource_data: resource_data as u32,
                                                    resource_data_word0,
                                                    flag_bc,
                                                    flag_bd,
                                                };
                                                if mii_resource_changed(
                                                    category,
                                                    resource_index,
                                                    current,
                                                ) {
                                                    log::warn!(
                                                        "svc::SleepThread(sleep) mii_resource_change: tid={} index={} variant={} category={} resource_index={} pending=0x{:08X} base=0x{:08X} manager=0x{:08X} table=0x{:08X} slot=0x{:08X} resource=0x{:08X} res_state=0x{:08X} res_data=0x{:08X} res_data0=0x{:08X} flags=[bc={:02X},bd={:02X}]",
                                                        current_thread_id,
                                                        index,
                                                        variant,
                                                        category,
                                                        resource_index,
                                                        pending,
                                                        base as u32,
                                                        manager as u32,
                                                        resource_table as u32,
                                                        slot as u32,
                                                        resource as u32,
                                                        resource_state,
                                                        resource_data as u32,
                                                        resource_data_word0,
                                                        flag_bc,
                                                        flag_bd,
                                                    );
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            if trace_sleep_backtrace {
                                let mut words = [0u32; 64];
                                for (index, word) in words.iter_mut().enumerate() {
                                    *word = memory.read_32(ctx.sp + (index as u64 * 4));
                                }
                                for (row, chunk) in words.chunks(8).enumerate() {
                                    log::info!(
                                        "svc::SleepThread(sleep) stack: tid={} sp+0x{:03X}: {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}",
                                        current_thread_id,
                                        row * 0x20,
                                        chunk[0],
                                        chunk[1],
                                        chunk[2],
                                        chunk[3],
                                        chunk[4],
                                        chunk[5],
                                        chunk[6],
                                        chunk[7],
                                    );
                                }
                            }
                        }
                        if trace_sleep_backtrace {
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

    let scheduler_lock = super::super::kernel::scheduler_lock()
        .expect("scheduler_lock must exist - kernel not initialized?");
    let _scheduler_guard =
        super::super::k_scheduler_lock::KScopedSchedulerLock::new(scheduler_lock);

    let process_arc = system.current_process_arc();
    let mut process = process_arc.lock().unwrap();
    if !process.check_thread_priority(priority) {
        return RESULT_INVALID_PRIORITY;
    }

    let thread_id = if thread_handle == PseudoHandle::CurrentThread as Handle {
        let Some(thread) = system.current_thread() else {
            return RESULT_INVALID_HANDLE;
        };
        thread.lock().unwrap().get_thread_id()
    } else {
        let Some(object_id) = process.handle_table.get_object(thread_handle) else {
            return RESULT_INVALID_HANDLE;
        };
        let Some(thread) = process.get_thread_by_object_id(object_id) else {
            return RESULT_INVALID_HANDLE;
        };
        thread.lock().unwrap().get_thread_id()
    };

    KThread::set_base_priority_with_process(&mut process, thread_id, priority);
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
    let caller_tid = system.current_thread_id().unwrap_or(0);
    trace_thread_core_mask(
        1,
        caller_tid,
        None,
        thread_handle,
        core_id,
        affinity_mask,
        RESULT_SUCCESS,
    );
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
        trace_thread_core_mask(
            3,
            caller_tid,
            None,
            thread_handle,
            core_id,
            affinity_mask,
            RESULT_INVALID_HANDLE,
        );
        return RESULT_INVALID_HANDLE;
    };
    trace_thread_core_mask(
        2,
        caller_tid,
        Some(&thread),
        thread_handle,
        core_id,
        affinity_mask,
        RESULT_SUCCESS,
    );
    let result = ResultCode::new(thread.lock().unwrap().set_core_mask(core_id, affinity_mask));
    trace_thread_core_mask(
        3,
        caller_tid,
        Some(&thread),
        thread_handle,
        core_id,
        affinity_mask,
        result,
    );
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
