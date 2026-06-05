//! Port of zuyu/src/core/hle/kernel/svc/svc_ipc.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for IPC (Inter-Process Communication) operations.

use std::sync::{Arc, Mutex, OnceLock};

use super::super::k_process::ProcessLock;
use crate::core::System;
use crate::hle::ipc;
use crate::hle::kernel::k_event::KEvent;
use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::kernel::k_resource_limit::LimitableResource;
use crate::hle::kernel::k_scoped_resource_reservation::KScopedResourceReservation;
use crate::hle::kernel::k_synchronization_object;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::{Handle, INVALID_HANDLE};
use crate::hle::kernel::trace_format;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{complete_sync_request, HLERequestContext};

fn should_trace_reply_receive_debug() -> bool {
    std::env::var_os("RUZU_TRACE_REPLY_RECV").is_some()
}

fn ipc_timeout_tick_from_ns(current_tick: i64, timeout_ns: i64) -> i64 {
    debug_assert!(timeout_ns > 0);

    let timeout = current_tick.saturating_add(timeout_ns).saturating_add(2);
    if timeout <= 0 {
        i64::MAX
    } else {
        timeout
    }
}

fn should_trace_sync_handle(session_handle: Handle) -> bool {
    std::env::var("RUZU_LOG_SVC_SYNC_HANDLE")
        .ok()
        .and_then(|value| {
            let trimmed = value.trim_start_matches("0x").trim_start_matches("0X");
            u32::from_str_radix(trimmed, 16)
                .ok()
                .or_else(|| value.parse::<u32>().ok())
        })
        .is_some_and(|target| target == session_handle)
}

fn should_use_host_thread_ipc(session_handle: Handle) -> bool {
    if std::env::var_os("RUZU_INLINE_IPC").is_some() {
        return false;
    }
    if std::env::var_os("RUZU_SERVER_THREAD_IPC_ALL").is_some() {
        return true;
    }
    let Some(spec) = std::env::var_os("RUZU_SERVER_THREAD_IPC_HANDLE") else {
        return false;
    };
    spec.to_string_lossy().split(',').any(|raw| {
        let value = raw.trim();
        if value.is_empty() {
            return false;
        }
        let hex = value
            .strip_prefix("0x")
            .or_else(|| value.strip_prefix("0X"))
            .unwrap_or(value);
        u32::from_str_radix(hex, 16)
            .ok()
            .or_else(|| value.parse::<u32>().ok())
            .is_some_and(|target| target == session_handle)
    })
}

fn should_use_host_thread_ipc_for_service(session_handle: Handle, service_name: Option<&str>) -> bool {
    if should_use_host_thread_ipc(session_handle) {
        return true;
    }
    if std::env::var_os("RUZU_INLINE_IPC").is_some() {
        return false;
    }

    if service_name != Some("IHOSBinderDriver") {
        return false;
    }

    // Binder host-thread routing is closer to upstream's ServerManager model,
    // but the current Rust path can lose MK8D's first queued buffer request
    // before audio/render startup. Keep it available for targeted debugging
    // without making normal boots depend on the incomplete host-thread path.
    std::env::var_os("RUZU_SERVER_THREAD_IPC_BINDER").is_some()
}

fn should_resolve_host_thread_service_name() -> bool {
    std::env::var_os("RUZU_INLINE_IPC").is_none()
        && std::env::var_os("RUZU_SERVER_THREAD_IPC_BINDER").is_some()
}

fn parse_trace_filter_list(env_key: &str) -> Option<Vec<u64>> {
    let spec = std::env::var(env_key).ok()?;
    let mut values = Vec::new();
    for raw in spec.split(',') {
        let value = raw.trim();
        if value.is_empty() || value == "*" {
            return None;
        }
        let hex = value
            .strip_prefix("0x")
            .or_else(|| value.strip_prefix("0X"))
            .unwrap_or(value);
        let Some(parsed) = u64::from_str_radix(hex, 16)
            .ok()
            .or_else(|| value.parse::<u64>().ok())
        else {
            continue;
        };
        values.push(parsed);
    }
    if values.is_empty() {
        None
    } else {
        Some(values)
    }
}

fn trace_filter_matches(value: u64, filter: &Option<Vec<u64>>) -> bool {
    filter
        .as_ref()
        .is_none_or(|values| values.iter().any(|&candidate| candidate == value))
}

fn should_emit_host_thread_ipc(session_handle: Handle) -> bool {
    static HANDLE_FILTER: OnceLock<Option<Vec<u64>>> = OnceLock::new();
    common::trace::is_enabled(common::trace::cat::HOST_THREAD_IPC)
        && trace_filter_matches(
            session_handle as u64,
            HANDLE_FILTER
                .get_or_init(|| parse_trace_filter_list("RUZU_TRACE_HOST_THREAD_IPC_HANDLE")),
        )
}

fn should_emit_svc_ipc_progress(session_handle: Handle, tid: u64) -> bool {
    static HANDLE_FILTER: OnceLock<Option<Vec<u64>>> = OnceLock::new();
    static TID_FILTER: OnceLock<Option<Vec<u64>>> = OnceLock::new();
    common::trace::is_enabled(common::trace::cat::SVC_IPC_PROGRESS)
        && trace_filter_matches(
            session_handle as u64,
            HANDLE_FILTER
                .get_or_init(|| parse_trace_filter_list("RUZU_TRACE_SVC_IPC_PROGRESS_HANDLE")),
        )
        && trace_filter_matches(
            tid,
            TID_FILTER.get_or_init(|| parse_trace_filter_list("RUZU_TRACE_SVC_IPC_PROGRESS_TID")),
        )
}

/// Diagnostic helper for the host-thread IPC routing path. Gated by
/// `RUZU_TRACE_HOST_THREAD_IPC=1`. Each stage label corresponds to a step
/// in the upstream `KClientSession::SendSyncRequest` →
/// `KServerSession::OnRequest` → `ServerManager::OnSessionEvent` →
/// `SendReplyHLE` pipeline.
fn trace_host_thread_ipc(stage: &str, session_handle: Handle) {
    if should_emit_host_thread_ipc(session_handle) {
        let stage_id = match stage {
            "enqueue_begin" => 1,
            "registered_with_host_thread" => 2,
            "enqueue_end" => 3,
            "client_begin_wait" => 4,
            "client_resumed" => 5,
            "spawn_fallback_begin" => 6,
            "before_scheduler_lock" => 7,
            "after_scheduler_lock" => 8,
            "before_process_lock" => 9,
            "after_process_lock" => 10,
            "before_client_session_lock" => 11,
            "after_client_session_lock" => 12,
            "after_send_sync_request_with_process" => 13,
            "before_current_thread_lock" => 14,
            "after_current_thread_lock" => 15,
            "after_begin_wait_guarded" => 16,
            "before_parent_lookup" => 17,
            "after_parent_lookup" => 18,
            "before_parent_session_lock" => 19,
            "after_parent_session_lock" => 20,
            "before_on_request" => 21,
            "after_on_request" => 22,
            "before_locked_section_end" => 35,
            "after_locked_section_end" => 36,
            "before_belt_signal" => 37,
            "after_belt_signal" => 38,
            _ => 0,
        };
        common::trace::emit_raw(
            common::trace::cat::HOST_THREAD_IPC,
            &[stage_id, session_handle as u64],
        );
    }
}

fn trace_svc_ipc_progress(
    stage: u64,
    session_handle: Handle,
    client_object_id: u64,
    message_address: u64,
    aux0: u64,
    aux1: u64,
    aux2: u64,
) {
    let tid = crate::hle::kernel::kernel::get_current_thread_id_fast().unwrap_or(0);
    if !should_emit_svc_ipc_progress(session_handle, tid) {
        return;
    }
    common::trace::emit_raw(
        common::trace::cat::SVC_IPC_PROGRESS,
        &[
            stage,
            tid,
            session_handle as u64,
            client_object_id,
            message_address,
            aux0,
            aux1,
            aux2,
        ],
    );
}

fn yield_after_ipc_if_requested(system: &System) {
    // RUZU_YIELD_AFTER_IPC=1 — diagnostic for MK8D post-IPC ordering
    // divergence. Upstream `KServerSession::OnRequest` transitions the client
    // through WAITING under the scheduler lock, giving the scheduler a chance
    // to run another runnable guest thread before the caller continues. Apply
    // the experiment after every SendSyncRequest path, including host-thread
    // IPC, so it actually tests the current default path.
    if std::env::var_os("RUZU_YIELD_AFTER_IPC").is_some() {
        let current_process = system.current_process_arc();
        if let Some(current_thread_id) = system.current_thread_id() {
            let sched_arc = system.scheduler_arc();
            let scheduler_ptr = {
                let mut scheduler = sched_arc.lock().unwrap();
                &mut *scheduler as *mut crate::hle::kernel::k_scheduler::KScheduler
            };
            unsafe {
                (*scheduler_ptr).yield_without_core_migration(current_process, current_thread_id);
            }
        }
    }
}

fn reschedule_after_inline_ipc_if_needed(system: &System) {
    if std::env::var_os("RUZU_DISABLE_INLINE_IPC_RESCHEDULE").is_some() {
        return;
    }
    let Some(kernel) = system.kernel() else {
        return;
    };
    let Some(scheduler) = kernel.current_scheduler() else {
        return;
    };
    let sched_ptr = {
        let mut scheduler = scheduler.lock().unwrap();
        &mut *scheduler as *mut crate::hle::kernel::k_scheduler::KScheduler
    };
    unsafe {
        crate::hle::kernel::k_scheduler::KScheduler::schedule_raw_if_needed(sched_ptr);
    }
}

/// Spawn-based fallback for the host-thread IPC routing path, invoked when
/// the target session is orphan (no `ServerManager` wiring, hence no
/// pending-registration queue and no wakeup event). Mirrors the
/// upstream-shape (park the client, run handler off-fiber, end wait on
/// reply) by spawning a one-shot worker thread. Rare in practice — most
/// service sessions inherit ownership through `connect_to_named_port` /
/// `push_ipc_interface`. Skipped entirely when host-thread routing is off.
fn host_thread_ipc_spawn_fallback(
    system: &System,
    session_handle: Handle,
    client_session: Arc<Mutex<crate::hle::kernel::k_client_session::KClientSession>>,
    message_address: u64,
) -> ResultCode {
    trace_host_thread_ipc("spawn_fallback_begin", session_handle);
    let (server_session, manager) = {
        let mut process = system.current_process_arc().lock().unwrap();
        let send_result = client_session
            .lock()
            .unwrap()
            .send_sync_request_with_process(&mut process, message_address as usize, 0);
        if send_result != 0 {
            return ResultCode::new(send_result);
        }
        let parent_id = match locked_get_parent_id(&client_session) {
            Some(parent_id) => parent_id,
            None => return RESULT_INVALID_HANDLE,
        };
        let Some(parent_session) = process.get_session_by_object_id(parent_id) else {
            return RESULT_INVALID_HANDLE;
        };
        let server_session = parent_session.lock().unwrap().get_server_session().clone();
        let Some(manager) = server_session.lock().unwrap().get_manager().cloned() else {
            return RESULT_INVALID_HANDLE;
        };
        (server_session, manager)
    };

    if let Some(current_thread) = system.current_thread() {
        let mut thread = current_thread.lock().unwrap();
        thread.set_wait_reason_for_debugging(
            crate::hle::kernel::k_thread::ThreadWaitReasonForDebugging::Ipc,
        );
        thread.begin_wait();
    }

    let service_manager = system.service_manager().unwrap();
    std::thread::spawn(move || {
        let receive_result = {
            let mut server_session = server_session.lock().unwrap();
            server_session.receive_request_hle(Arc::clone(&manager))
        };
        let Ok((mut context, request_manager, _request_message_address)) = receive_result else {
            return;
        };
        context.set_service_manager(service_manager);
        let _ = complete_sync_request(&request_manager, &mut context);
        let _ = server_session.lock().unwrap().send_reply();
    });

    if let Some(kernel) = system.kernel() {
        if let Some(scheduler) = kernel.current_scheduler() {
            let sched_ptr = {
                let mut scheduler = scheduler.lock().unwrap();
                &mut *scheduler as *mut crate::hle::kernel::k_scheduler::KScheduler
            };
            unsafe {
                crate::hle::kernel::k_scheduler::KScheduler::reschedule_current_core_raw(sched_ptr);
            }
        }
    }

    system
        .current_thread()
        .map(|thread| ResultCode::new(thread.lock().unwrap().get_wait_result()))
        .unwrap_or(RESULT_INVALID_HANDLE)
}

fn format_ipc_trace_words(system: &System, message_address: u64, words: usize) -> Option<String> {
    if message_address == 0 || words == 0 {
        return None;
    }

    let memory = {
        let thread = system.current_thread()?;
        let parent = {
            let thread_guard = thread.lock().unwrap();
            thread_guard.parent.as_ref()?.clone()
        }
        .upgrade()?;
        let process = parent.lock().unwrap();
        process.page_table.get_base().m_memory.clone()?
    };
    let mem = memory.lock().unwrap();
    let mut formatted = String::with_capacity(words * 9);
    for i in 0..words {
        let word = mem.read_32(message_address + (i as u64 * 4));
        use std::fmt::Write;
        let _ = write!(formatted, "{:08x} ", word.swap_bytes());
    }
    Some(formatted.trim_end().to_string())
}

fn format_ipc_trace_slice(words: &[u32]) -> Option<String> {
    if words.is_empty() {
        return None;
    }

    let mut formatted = String::with_capacity(words.len() * 9);
    for word in words {
        use std::fmt::Write;
        let _ = write!(formatted, "{:08x} ", word.swap_bytes());
    }
    Some(formatted.trim_end().to_string())
}

fn trace_ipc_buffer(system: &System, label: &str, message_address: u64) {
    if !trace_format::is_svc_trace_enabled() {
        return;
    }

    let Some(payload) = format_ipc_trace_words(system, message_address, 16) else {
        return;
    };
    eprintln!(
        "[{:>10.6}] {} [0x{:x}]: {}",
        trace_format::elapsed_secs(),
        label,
        message_address,
        payload
    );
}

/// TLS_RSP_BUF: response bytes as staged in the HLERequestContext command buffer
/// *before* guest-memory writeback. Useful for inspecting what a handler intended
/// to send regardless of partial-writeback behavior.
fn trace_ipc_response_buffer(context: &HLERequestContext, message_address: u64) {
    if !trace_format::is_svc_trace_enabled() {
        return;
    }

    let words = &context.command_buffer()[..16];
    let Some(payload) = format_ipc_trace_slice(words) else {
        return;
    };
    eprintln!(
        "[{:>10.6}] TLS_RSP_BUF [0x{:x}]: {}",
        trace_format::elapsed_secs(),
        message_address,
        payload
    );
}

/// TLS_RSP_TLS: response bytes as observed in guest TLS *after* writeback. This
/// matches what the client thread will actually read back from its message
/// buffer and is what zuyu's single-label `TLS_RSP` trace captures.
fn trace_ipc_response_tls(system: &System, message_address: u64) {
    if !trace_format::is_svc_trace_enabled() {
        return;
    }

    let Some(payload) = format_ipc_trace_words(system, message_address, 16) else {
        return;
    };
    eprintln!(
        "[{:>10.6}] TLS_RSP_TLS [0x{:x}]: {}",
        trace_format::elapsed_secs(),
        message_address,
        payload
    );
}

/// Lock `client_session`, recording the acquisition in the per-object wait-for
/// graph (RUZU_LOCK_ORDER=1) keyed by the Mutex address, then read parent_id.
/// At a wedge the graph shows who holds the exact client_session a blocked
/// thread is waiting on.
fn locked_get_parent_id(
    cs: &Arc<Mutex<crate::hle::kernel::k_client_session::KClientSession>>,
) -> Option<u64> {
    let addr = Arc::as_ptr(cs) as usize;
    let gtid = crate::hle::kernel::kernel::get_current_thread_id_fast()
        .map(|t| t as i64)
        .unwrap_or(0);
    common::lock_order::obj_wait("client_session", addr, gtid);
    let g = cs.lock().unwrap();
    let _h = common::lock_order::obj_held("client_session", addr, gtid);
    g.get_parent_id()
}

fn send_sync_request_impl(
    system: &System,
    session_handle: Handle,
    message_address: u64,
) -> ResultCode {
    // `RUZU_PROFILE_IPC_PHASES=1` — time each phase of send_sync_request_impl
    // so we can see which Mutex acquisition or sub-step is the bottleneck.
    // Used in the MK8D wedge investigation to localize where 7ms/call goes
    // when the handler itself is <100us.
    let profile_phases = std::env::var_os("RUZU_PROFILE_IPC_PHASES").is_some();
    let phase_t0 = if profile_phases {
        Some(std::time::Instant::now())
    } else {
        None
    };
    let mut phase_last = phase_t0;
    let mut record_phase = |label: &'static str, last: &mut Option<std::time::Instant>| {
        if let Some(t) = last {
            record_ipc_phase(label, t.elapsed());
            *last = Some(std::time::Instant::now());
        }
    };
    trace_ipc_buffer(system, "TLS_REQ", message_address);
    trace_svc_ipc_progress(1, session_handle, 0, message_address, 0, 0, 0);
    let trace_sync = should_trace_sync_handle(session_handle);
    let (client_session, session_object_id, parent_id) = {
        let process = system.current_process_arc().lock().unwrap();
        let Some(object_id) = process.handle_table.get_object(session_handle) else {
            log::error!(
                "  SendSyncRequest: handle {:#x} not in handle table",
                session_handle
            );
            return RESULT_INVALID_HANDLE;
        };
        let Some(client_session) = process.get_client_session_by_object_id(object_id) else {
            log::error!(
                "  SendSyncRequest: object_id {} not a client session",
                object_id
            );
            return RESULT_INVALID_HANDLE;
        };
        let Some(parent_id) = process.get_client_session_parent_id(object_id) else {
            if trace_sync {
                log::info!("svc::SendSyncRequest stage=missing_parent_id");
            }
            return RESULT_INVALID_HANDLE;
        };
        process
            .num_ipc_messages
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        (client_session, object_id, parent_id)
    };
    trace_svc_ipc_progress(
        2,
        session_handle,
        session_object_id,
        message_address,
        0,
        0,
        0,
    );
    record_phase("01_resolve_client_session", &mut phase_last);
    if trace_sync {
        log::info!("svc::SendSyncRequest stage=resolved_client_session");
    }
    // Upstream `KClientSession` owns a direct `m_parent` pointer. Ruzu stores
    // the equivalent parent object id in `KProcess` next to the client-session
    // object registration so the hot SVC path does not need to lock the client
    // endpoint just to discover its parent.

    let explicit_host_thread_routing = should_use_host_thread_ipc(session_handle);
    let trace_manager_resolution = should_emit_svc_ipc_progress(
        session_handle,
        crate::hle::kernel::kernel::get_current_thread_id_fast().unwrap_or(0),
    );
    let needs_manager_resolution = explicit_host_thread_routing
        || should_resolve_host_thread_service_name()
        || trace_manager_resolution;

    // ---- Host-thread IPC routing (default, upstream-faithful) ----
    //
    // Upstream `KClientSession::SendSyncRequest` always parks the calling
    // guest thread (`BeginWait`) and lets the owning `ServerManager`'s
    // host fiber consume the request via `ReceiveRequestHLE` →
    // `SendReplyHLE` → `client_thread->EndWait()`. Ruzu mirrors that by
    // pushing the session to the owning ServerManager's
    // pending-registration queue, signaling its wakeup event, parking the
    // guest, and yielding the fiber. The handler runs on the host fiber;
    // `send_reply` ends the wait; the guest resumes here.
    //
    // When the target session has no ServerManager wiring (orphan,
    // typically unit tests / `test_system()` harnesses), we fall back
    // to the spawn-based path that mirrors the same park-the-client /
    // handler-off-fiber shape. Set `RUZU_INLINE_IPC=1` to opt out of
    // host-thread routing entirely (legacy synchronous path) — kept as
    // a kill-switch for regressions and to preserve unit-test inline
    // execution where applicable.
    let server_session_and_manager: Option<(
        Arc<Mutex<crate::hle::kernel::k_server_session::KServerSession>>,
        Arc<Mutex<crate::hle::service::hle_ipc::SessionRequestManager>>,
    )> = if needs_manager_resolution {
        trace_svc_ipc_progress(
            11,
            session_handle,
            session_object_id,
            message_address,
            parent_id,
            0,
            0,
        );
        let process = system.current_process_arc().lock().unwrap();
        trace_svc_ipc_progress(
            12,
            session_handle,
            session_object_id,
            message_address,
            parent_id,
            0,
            0,
        );
        process
            .get_session_by_object_id(parent_id)
            .and_then(|parent_session| {
                trace_svc_ipc_progress(
                    13,
                    session_handle,
                    session_object_id,
                    message_address,
                    parent_id,
                    0,
                    0,
                );
                let guard = parent_session.lock().unwrap();
                trace_svc_ipc_progress(
                    14,
                    session_handle,
                    session_object_id,
                    message_address,
                    parent_id,
                    0,
                    0,
                );
                let server_session = Arc::clone(guard.get_server_session());
                trace_svc_ipc_progress(
                    15,
                    session_handle,
                    session_object_id,
                    message_address,
                    parent_id,
                    Arc::as_ptr(&server_session) as u64,
                    0,
                );
                let manager = server_session.lock().unwrap().get_manager().cloned()?;
                trace_svc_ipc_progress(
                    16,
                    session_handle,
                    session_object_id,
                    message_address,
                    parent_id,
                    Arc::as_ptr(&server_session) as u64,
                    Arc::as_ptr(&manager) as u64,
                );
                Some((server_session, manager))
            })
    } else {
        None
    };
    trace_svc_ipc_progress(
        17,
        session_handle,
        session_object_id,
        message_address,
        server_session_and_manager.is_some() as u64,
        0,
        0,
    );
    let host_thread_service_name = if should_resolve_host_thread_service_name()
        || trace_manager_resolution
    {
        server_session_and_manager.as_ref().and_then(|(_, manager)| {
            trace_svc_ipc_progress(
                18,
                session_handle,
                session_object_id,
                message_address,
                Arc::as_ptr(manager) as u64,
                0,
                0,
            );
            let manager = manager.lock().unwrap();
            trace_svc_ipc_progress(
                19,
                session_handle,
                session_object_id,
                message_address,
                0,
                0,
                0,
            );
            manager
                .session_handler()
                .map(|handler| handler.service_name().to_string())
        })
    } else {
        None
    };
    trace_svc_ipc_progress(
        20,
        session_handle,
        session_object_id,
        message_address,
        host_thread_service_name.is_some() as u64,
        0,
        0,
    );
    let host_thread_routing = explicit_host_thread_routing
        || should_use_host_thread_ipc_for_service(session_handle, host_thread_service_name.as_deref());
    let host_thread_targets: Option<(
        Arc<Mutex<crate::hle::kernel::k_server_session::KServerSession>>,
        Arc<Mutex<crate::hle::service::hle_ipc::SessionRequestManager>>,
        crate::hle::service::hle_ipc::PendingRegistrationQueue,
        Arc<crate::hle::service::os::event::Event>,
    )> = if host_thread_routing {
        let with_queue_wakeup =
            server_session_and_manager
                .as_ref()
                .and_then(|(server_session, manager)| {
                    let (queue, wakeup) = {
                        let g = manager.lock().unwrap();
                        (
                            g.pending_registrations().cloned(),
                            g.server_wakeup().cloned(),
                        )
                    };
                    Some((
                        Arc::clone(server_session),
                        Arc::clone(manager),
                        queue?,
                        wakeup?,
                    ))
                });

        if with_queue_wakeup.is_none() && server_session_and_manager.is_some() {
            // Orphan target under host-thread routing: park the client and
            // run the handler on a one-shot worker so the guest-side shape
            // still matches the kernel-backed wait the regular path uses.
            return host_thread_ipc_spawn_fallback(
                system,
                session_handle,
                client_session,
                message_address,
            );
        }
        with_queue_wakeup
    } else {
        None
    };

    if let Some((server_session, manager, queue, wakeup)) = host_thread_targets {
        trace_host_thread_ipc("enqueue_begin", session_handle);

        // One-time wiring per KServerSession:
        //  1. Push (server_session, manager) to the queue so the host fiber
        //     calls register_session on it (adds to multi_wait).
        //  2. Set manager_wakeup on the KServerSession so subsequent
        //     `notify_available` calls auto-signal the wakeup_event.
        // register_session is idempotent (no duplicate entry in multi_wait)
        // so re-pushing on every IPC is safe — but unnecessary work.
        //
        // The wait-wake guard remains as a safety net for replies that race
        // ahead of the explicit fiber handoff below. The actual request enqueue
        // and WAITING transition are performed together under the scheduler
        // lock below, matching upstream `KServerSession::OnRequest`.
        if let Some(current_thread) = system.current_thread() {
            let _lo_t = common::lock_order::guard("thread");
            current_thread.lock().unwrap().arm_wait_wake_guard();
        }

        let needs_setup = {
            let _lo_ss = common::lock_order::guard("server_session");
            server_session.lock().unwrap().manager_wakeup.is_none()
        };
        if needs_setup {
            server_session
                .lock()
                .unwrap()
                .set_manager_wakeup(Arc::downgrade(&wakeup));
            queue
                .lock()
                .unwrap()
                .push((Arc::clone(&server_session), Arc::clone(&manager)));
            wakeup.signal();
            trace_host_thread_ipc("registered_with_host_thread", session_handle);
        }

        // Enqueue the request and park the current client under the scheduler
        // lock, matching upstream `KServerSession::OnRequest`: push request,
        // notify if the queue was empty, then `BeginWait` for sync requests in
        // one scheduler-locked critical section. Without this, ruzu can expose
        // the request to the host service thread before the guest thread has
        // transitioned to WAITING.
        let mut already_woken = false;
        {
            trace_host_thread_ipc("before_scheduler_lock", session_handle);
            let _scheduler_lock = crate::hle::kernel::kernel::scheduler_lock()
                .map(crate::hle::kernel::k_scheduler_lock::KScopedSchedulerLock::new);
            trace_host_thread_ipc("after_scheduler_lock", session_handle);
            trace_host_thread_ipc("before_process_lock", session_handle);
            let _lo_p = common::lock_order::guard("process");
            let mut process = system.current_process_arc().lock().unwrap();
            trace_host_thread_ipc("after_process_lock", session_handle);
            trace_host_thread_ipc("before_parent_lookup", session_handle);
            let Some(parent_session) = process.get_session_by_object_id(parent_id) else {
                return RESULT_INVALID_HANDLE;
            };
            trace_host_thread_ipc("after_parent_lookup", session_handle);
            let request = Arc::new(Mutex::new(
                crate::hle::kernel::k_session_request::KSessionRequest::new(),
            ));
            request.lock().unwrap().initialize_with_process(
                &mut process,
                None,
                message_address as usize,
                0,
            );
            trace_host_thread_ipc("before_parent_session_lock", session_handle);
            let _lo_ps = common::lock_order::guard("parent_session");
            let session = parent_session.lock().unwrap();
            trace_host_thread_ipc("after_parent_session_lock", session_handle);
            trace_host_thread_ipc("before_on_request", session_handle);
            let send_result = session.on_request_with_process(&mut process, request);
            trace_host_thread_ipc("after_on_request", session_handle);
            trace_host_thread_ipc("after_send_sync_request_with_process", session_handle);
            if send_result != 0 {
                return ResultCode::new(send_result);
            }

            // RUZU_TRACE_PLU_IPC=1 — fresh-session diagnostic. Sample while
            // still holding the scheduler lock, before `BeginWait`, so the
            // host service thread cannot consume the request before the trace
            // observes whether it landed in the watched server session.
            if needs_setup && common::trace::is_enabled(common::trace::cat::PLU_IPC) {
                let tid = crate::hle::kernel::kernel::get_current_thread_id_fast().unwrap_or(0);
                let (sig, req_len, cur_req, manager_wakeup_live) = {
                    let s = server_session.lock().unwrap();
                    (
                        s.is_signaled(),
                        s.request_list.len(),
                        s.current_request.is_some(),
                        s.manager_wakeup.as_ref().map(|w| w.strong_count() > 0),
                    )
                };
                common::trace::emit_raw(
                    common::trace::cat::PLU_IPC,
                    &[
                        1,
                        tid,
                        session_object_id,
                        Arc::as_ptr(&server_session) as u64,
                        Arc::as_ptr(&wakeup) as u64,
                        needs_setup as u64,
                        sig as u64,
                        req_len as u64,
                        cur_req as u64,
                        manager_wakeup_live.unwrap_or(false) as u64,
                    ],
                );
            }

            if let Some(current_thread) = system.current_thread() {
                trace_host_thread_ipc("before_current_thread_lock", session_handle);
                let mut thread = current_thread.lock().unwrap();
                trace_host_thread_ipc("after_current_thread_lock", session_handle);
                thread.set_wait_reason_for_debugging(
                    crate::hle::kernel::k_thread::ThreadWaitReasonForDebugging::Ipc,
                );
                already_woken = !thread.begin_wait_guarded();
                trace_host_thread_ipc("after_begin_wait_guarded", session_handle);
            }
            trace_host_thread_ipc("before_locked_section_end", session_handle);
        }
        trace_host_thread_ipc("after_locked_section_end", session_handle);
        // Belt-and-suspenders signal (notify_available also signals, but
        // signaling twice is harmless and avoids any ordering surprise).
        trace_host_thread_ipc("before_belt_signal", session_handle);
        wakeup.signal();
        trace_host_thread_ipc("after_belt_signal", session_handle);
        trace_host_thread_ipc("enqueue_end", session_handle);

        // `begin_wait_guarded` consumes the pending-wake flag armed above: if
        // the host fiber already replied (end_wait ran while this thread was
        // still RUNNABLE, a no-op that recorded a pending wake), the park is
        // skipped and we fall straight through to read the wait result —
        // closing the signal-before-park lost-wakeup race that previously
        // wedged ~50% of boots in `pl:u` shared-font init.
        trace_host_thread_ipc("client_begin_wait", session_handle);
        if already_woken {
            return system
                .current_thread()
                .map(|thread| ResultCode::new(thread.lock().unwrap().get_wait_result()))
                .unwrap_or(RESULT_INVALID_HANDLE);
        }

        // Yield the current fiber. The owning ServerManager's host fiber
        // processes the request and `send_reply` ends the wait on the
        // client. The scheduler eventually re-picks this fiber and we
        // resume just after this call.
        if let Some(kernel) = system.kernel() {
            if let Some(scheduler) = kernel.current_scheduler() {
                let sched_ptr = {
                    let mut scheduler = scheduler.lock().unwrap();
                    &mut *scheduler as *mut crate::hle::kernel::k_scheduler::KScheduler
                };
                unsafe {
                    crate::hle::kernel::k_scheduler::KScheduler::reschedule_current_core_raw(
                        sched_ptr,
                    );
                }
            }
        }
        trace_host_thread_ipc("client_resumed", session_handle);

        return system
            .current_thread()
            .map(|thread| ResultCode::new(thread.lock().unwrap().get_wait_result()))
            .unwrap_or(RESULT_INVALID_HANDLE);
    }
    // Inline fallback: orphan session (no ServerManager wiring). Used by
    // unit tests that drive `send_sync_request` directly without spinning
    // up a host fiber. Behavior is non-upstream but converges with the
    // host-thread path because the same handler runs end-to-end.
    let (request_manager, mut context, request_message_address) = if std::env::var_os(
        "RUZU_LEGACY_INLINE_NOTIFY",
    )
    .is_some()
    {
        let (server_session, manager) = {
            let mut process = system.current_process_arc().lock().unwrap();
            record_phase("02_process_lock_legacy_inline", &mut phase_last);
            if trace_sync {
                log::info!("svc::SendSyncRequest stage=legacy_enqueue_request");
            }
            let send_result = client_session
                .lock()
                .unwrap()
                .send_sync_request_with_process(&mut process, message_address as usize, 0);
            record_phase("03_legacy_send_sync_request_with_process", &mut phase_last);
            if send_result != 0 {
                return ResultCode::new(send_result);
            }

            let parent_id = match locked_get_parent_id(&client_session) {
                Some(parent_id) => parent_id,
                None => {
                    if trace_sync {
                        log::info!("svc::SendSyncRequest stage=legacy_missing_parent_id");
                    }
                    return RESULT_INVALID_HANDLE;
                }
            };
            let Some(parent_session) = process.get_session_by_object_id(parent_id) else {
                if trace_sync {
                    log::info!(
                        "svc::SendSyncRequest stage=legacy_missing_parent_session parent_id={:#x}",
                        parent_id
                    );
                }
                return RESULT_INVALID_HANDLE;
            };
            let server_session = parent_session.lock().unwrap().get_server_session().clone();
            let manager = match server_session.lock().unwrap().get_manager().cloned() {
                Some(manager) => manager,
                None => {
                    if trace_sync {
                        log::info!(
                            "svc::SendSyncRequest stage=legacy_missing_server_manager parent_id={:#x}",
                            parent_id
                        );
                    }
                    return RESULT_INVALID_HANDLE;
                }
            };
            (server_session, manager)
        };
        if trace_sync {
            log::info!("svc::SendSyncRequest stage=legacy_receive_request_hle_begin");
        }

        record_phase("04_legacy_resolve_server_session_manager", &mut phase_last);
        let receive_result = {
            let mut server_session = server_session.lock().unwrap();
            server_session.receive_request_hle(Arc::clone(&manager))
        };
        record_phase("05_legacy_receive_request_hle", &mut phase_last);
        match receive_result {
            Ok((context, manager, request_message_address)) => {
                if trace_sync {
                    log::info!("svc::SendSyncRequest stage=legacy_receive_request_hle_end");
                }
                (manager, context, request_message_address)
            }
            Err(_) => return RESULT_INVALID_HANDLE,
        }
    } else {
        let (server_session, manager, request) = {
            let _lo_p = common::lock_order::guard("process");
            let mut process = system.current_process_arc().lock().unwrap();
            record_phase("02_process_lock_2", &mut phase_last);
            trace_svc_ipc_progress(
                3,
                session_handle,
                session_object_id,
                message_address,
                0,
                0,
                0,
            );
            if trace_sync {
                log::info!("svc::SendSyncRequest stage=enqueue_request");
            }
            let request = Arc::new(Mutex::new(
                crate::hle::kernel::k_session_request::KSessionRequest::new(),
            ));
            request.lock().unwrap().initialize_with_process(
                &mut process,
                None,
                message_address as usize,
                0,
            );
            record_phase("03_prepare_inline_request", &mut phase_last);
            let Some(parent_session) = process.get_session_by_object_id(parent_id) else {
                if trace_sync {
                    log::info!(
                        "svc::SendSyncRequest stage=missing_parent_session parent_id={:#x}",
                        parent_id
                    );
                }
                return RESULT_INVALID_HANDLE;
            };
            let server_session = parent_session.lock().unwrap().get_server_session().clone();
            let manager = match server_session.lock().unwrap().get_manager().cloned() {
                Some(manager) => manager,
                None => {
                    if trace_sync {
                        log::info!(
                            "svc::SendSyncRequest stage=missing_server_manager parent_id={:#x}",
                            parent_id
                        );
                    }
                    return RESULT_INVALID_HANDLE;
                }
            };
            (server_session, manager, request)
        };
        if trace_sync {
            log::info!("svc::SendSyncRequest stage=receive_request_hle_begin");
        }

        // Do not notify the host ServerManager on the inline fallback path.
        // This path consumes the request synchronously on the caller's host
        // thread; waking the real ServerManager exposes the same request to two
        // dispatch paths and can make a stale SFCO reply look like a new IPC.
        // `receive_inline_request_hle` pushes and receives under the same
        // `KServerSession` mutex, while the owner `KProcess` mutex is already
        // released to avoid the current-thread parent lookup deadlock.
        record_phase("04_resolve_server_session_manager", &mut phase_last);
        trace_svc_ipc_progress(
            4,
            session_handle,
            session_object_id,
            message_address,
            0,
            0,
            0,
        );
        let receive_result = {
            let mut server_session = server_session.lock().unwrap();
            server_session.receive_inline_request_hle(request, Arc::clone(&manager))
        };
        record_phase("05_receive_request_hle", &mut phase_last);
        match receive_result {
            Ok((context, manager, request_message_address)) => {
                trace_svc_ipc_progress(
                    5,
                    session_handle,
                    session_object_id,
                    message_address,
                    request_message_address,
                    0,
                    0,
                );
                if trace_sync {
                    log::info!("svc::SendSyncRequest stage=receive_request_hle_end");
                }
                (manager, context, request_message_address)
            }
            Err(_) => return RESULT_INVALID_HANDLE,
        }
    };

    let service_manager = system.service_manager().unwrap();
    context.set_service_manager(service_manager);

    let (is_domain, session_handler_name) = {
        let manager = request_manager.lock().unwrap();
        let handler_name = manager
            .session_handler()
            .map(|handler| handler.service_name().to_string())
            .unwrap_or_else(|| "<none>".to_string());
        (manager.is_domain(), handler_name)
    };

    log::trace!(
        "  SendSyncRequest: handle={:#x} message={:#x} service={} cmd_type={:?} is_domain={} parsed_cmd={}",
        session_handle,
        request_message_address,
        session_handler_name,
        context.get_command_type(),
        is_domain,
        context.get_command(),
    );
    // Env-gated SVC-level trace: every SendSyncRequest with ASCII-decoded
    // request TLS preview. Useful when an IPC is suspected lost between libnx
    // and the service dispatcher: this fires BEFORE any service routing, so
    // any guest-issued SendSyncRequest will appear here.
    if std::env::var_os("RUZU_TRACE_SVC_IPC").is_some() {
        let mut printable = String::new();
        let mem_opt = (|| -> Option<_> {
            let thread = system.current_thread()?;
            let parent = {
                let g = thread.lock().unwrap();
                g.parent.as_ref()?.clone()
            }
            .upgrade()?;
            let process = parent.lock().unwrap();
            process.page_table.get_base().m_memory.clone()
        })();
        if let Some(mem_arc) = mem_opt {
            let mem = mem_arc.lock().unwrap();
            for i in 0..256u64 {
                let word = mem.read_32(request_message_address + i);
                let b = (word & 0xff) as u8;
                if b >= 0x20 && b < 0x7f {
                    printable.push(b as char);
                } else if b == 0 && !printable.is_empty() && !printable.ends_with(' ') {
                    printable.push(' ');
                }
            }
        }
        eprintln!(
            "[SVC_IPC] handle={:#x} service={} cmd={} dom={} ascii={:?}",
            session_handle,
            session_handler_name,
            context.get_command(),
            is_domain,
            printable.trim(),
        );
    }

    if trace_sync {
        log::info!("svc::SendSyncRequest stage=complete_sync_request_begin");
    }
    trace_svc_ipc_progress(
        6,
        session_handle,
        session_object_id,
        message_address,
        request_message_address,
        context.get_command() as u64,
        0,
    );
    let result = complete_sync_request(&request_manager, &mut context);
    trace_svc_ipc_progress(
        7,
        session_handle,
        session_object_id,
        message_address,
        request_message_address,
        context.get_command() as u64,
        result.get_inner_value() as u64,
    );
    record_phase("06_complete_sync_request_handler", &mut phase_last);
    if trace_sync {
        log::info!(
            "svc::SendSyncRequest stage=complete_sync_request_end result={:#x}",
            result.get_inner_value()
        );
    }
    // Write-back is performed inside ServiceFrameworkBase::handle_sync_request_impl,
    // matching upstream where only service.cpp:148 calls WriteToOutgoingCommandBuffer.
    // The remaining Rust-only explicit write-back is the `StubSuccess` fallback in
    // `complete_sync_request`.
    //
    // Emit both response views so `scripts/svc_diff.py` can pick whichever one
    // lines up with the zuyu reference trace for a given investigation pass.
    trace_ipc_response_buffer(&context, message_address);
    trace_ipc_response_tls(system, message_address);

    if let Some(parent_session) = system
        .current_process_arc()
        .lock()
        .unwrap()
        .get_session_by_object_id(
            client_session
                .lock()
                .unwrap()
                .get_parent_id()
                .unwrap_or(session_object_id),
        )
    {
        let server_session = parent_session.lock().unwrap().get_server_session().clone();
        if trace_sync {
            log::info!("svc::SendSyncRequest stage=send_reply_begin");
        }
        trace_svc_ipc_progress(
            8,
            session_handle,
            session_object_id,
            message_address,
            request_message_address,
            0,
            0,
        );
        let _ = server_session.lock().unwrap().send_reply();
        trace_svc_ipc_progress(
            9,
            session_handle,
            session_object_id,
            message_address,
            request_message_address,
            0,
            0,
        );
        if trace_sync {
            log::info!("svc::SendSyncRequest stage=send_reply_end");
        }
    }
    record_phase("07_send_reply", &mut phase_last);

    // Upstream `PhysicalCore::RunThread` returns to the scheduler after every
    // SVC, and the normal IPC path parks the caller while the server session
    // handles the request. Ruzu's legacy inline HLE IPC fallback does not park
    // the caller, so explicitly consume any pending same-core scheduling request
    // once the inline handler and reply have completed. This keeps clean runs
    // moving without the heavier experimental unconditional preemption that was
    // previously guarded by `RUZU_RESCHEDULE_AFTER_IPC`.
    reschedule_after_inline_ipc_if_needed(system);

    if result == crate::hle::service::ipc_helpers::RESULT_SESSION_CLOSED {
        return RESULT_SUCCESS;
    }

    trace_svc_ipc_progress(
        10,
        session_handle,
        session_object_id,
        message_address,
        result.get_inner_value() as u64,
        0,
        0,
    );
    result
}

/// Makes a blocking IPC call to a service.
///
/// Matches upstream `SendSyncRequest` → `SendSyncRequestImpl`:
/// 1. Get current thread and TLS address
/// 2. Resolve client session from handle
/// 3. Create HLERequestContext with thread/memory references
/// 4. Read command buffer from TLS, dispatch to handler
/// 5. Write response back to TLS (inside write_to_outgoing_command_buffer)
pub fn send_sync_request(system: &System, session_handle: Handle) -> ResultCode {
    let tls_address = match system.current_thread() {
        Some(thread) => thread.lock().unwrap().get_tls_address().get(),
        None => return RESULT_INVALID_HANDLE,
    };
    // `RUZU_TRACE_IPC_DIFF=<path>` — dump every IPC's TLS bytes (request
    // before dispatch, response after) to <path> as JSON Lines. Used to
    // byte-diff two runs (inline vs host-thread) via scripts/ipc_diff.py
    // and find the first IPC that diverges. Capture is path-agnostic: same
    // wrapper for inline and host-thread routing, since both eventually
    // return from send_sync_request_impl with the response written back to
    // the same TLS address.
    let diff_capture_req = if ipc_diff_capture_enabled() {
        Some(read_tls_bytes(system, tls_address, 256))
    } else {
        None
    };

    let result = if std::env::var_os("RUZU_PROFILE_IPC").is_some() {
        // `RUZU_PROFILE_IPC=1` — measure wall-clock time per SendSyncRequest
        // and dump a per-handle histogram (count, total_us, avg_us, max_us)
        // on a SIGUSR1 or process exit. Used to find HLE handlers that are
        // bottlenecking the producer thread.
        let start = std::time::Instant::now();
        let r = send_sync_request_impl(system, session_handle, tls_address);
        record_ipc_profile(session_handle, start.elapsed());
        r
    } else {
        send_sync_request_impl(system, session_handle, tls_address)
    };

    if let Some(req) = diff_capture_req {
        let rsp = read_tls_bytes(system, tls_address, 256);
        record_ipc_diff(session_handle, &req, &rsp, result);
    }

    yield_after_ipc_if_requested(system);

    // Inter-IPC OS-scheduler yield. Discovered while diagnosing
    // host-thread IPC's flaky 25 %-success rate on MK8D: when the guest
    // core OS thread chains IPCs back-to-back, the Linux scheduler keeps
    // it on-CPU and host service OS threads (HLE:audio, HLE:nvservices,
    // ...) only get brief slivers of time. State that those threads
    // should advance never does, and the guest poll-loops on stale
    // memory.
    //
    // A `std::thread::sleep(1µs)` between IPCs translates to a
    // `clock_nanosleep` syscall — guaranteed to release the CPU back to
    // the OS scheduler so other OS threads can run. Empirically this
    // raises MK8D's host-thread reliability from ~50 % to 16/16 (100 %).
    // `std::thread::yield_now()` (`sched_yield`) is too weak: Linux can
    // reschedule the same thread if no equal-priority work is ready, so
    // it only buys ~50 % success.
    //
    // Cost: ~50-100 µs per IPC × ~5 000 boot IPCs ≈ 250-500 ms added to
    // boot. Negligible compared to MK8D's 11-second boot when host-thread
    // routing is explicitly enabled. Do not apply it to the inline path:
    // handlers already run on the guest core OS thread there, and MK8D can
    // issue hundreds of thousands of tight IPC polls during boot.
    //
    // `RUZU_HOST_THREAD_IPC_SLEEP_US=<n>` overrides the default 1 µs;
    // set to `0` to disable entirely (for performance investigation).
    let host_thread_ipc_enabled = std::env::var_os("RUZU_SERVER_THREAD_IPC_ALL").is_some()
        || std::env::var_os("RUZU_SERVER_THREAD_IPC_HANDLE").is_some();
    if host_thread_ipc_enabled && std::env::var_os("RUZU_INLINE_IPC").is_none() {
        let us = std::env::var("RUZU_HOST_THREAD_IPC_SLEEP_US")
            .ok()
            .and_then(|s| s.parse::<u64>().ok())
            .unwrap_or(1);
        if us > 0 {
            std::thread::sleep(std::time::Duration::from_micros(us));
        }
    }

    result
}

/// ===== IPC byte-diff capture (RUZU_TRACE_IPC_DIFF) =====
///
/// Hooks every `send_sync_request` to dump the TLS request bytes (before
/// dispatch) and response bytes (after dispatch) to a JSONL file. Each line:
///   {"seq":N,"handle":"0xXXXX","tid":T,"req_len":L,"rsp_len":L,"req":"hex","rsp":"hex","result":0xR}
///
/// Used by `scripts/ipc_diff.py` to byte-diff two runs (inline vs
/// host-thread) and find the first IPC where they diverge — the most
/// actionable diagnostic for chasing the host-thread deadlock.
static IPC_DIFF_FILE: std::sync::OnceLock<
    Option<std::sync::Mutex<std::io::BufWriter<std::fs::File>>>,
> = std::sync::OnceLock::new();
static IPC_DIFF_SEQ: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

fn ipc_diff_capture_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("RUZU_TRACE_IPC_DIFF").is_some())
}

fn ipc_diff_writer() -> Option<&'static std::sync::Mutex<std::io::BufWriter<std::fs::File>>> {
    IPC_DIFF_FILE
        .get_or_init(|| {
            let path = std::env::var("RUZU_TRACE_IPC_DIFF").ok()?;
            let file = std::fs::File::create(&path).ok()?;
            Some(std::sync::Mutex::new(std::io::BufWriter::new(file)))
        })
        .as_ref()
}

fn read_tls_bytes(system: &System, address: u64, len: usize) -> Vec<u8> {
    let memory = (|| -> Option<_> {
        let thread = system.current_thread()?;
        let parent = {
            let guard = thread.lock().unwrap();
            guard.parent.as_ref()?.clone()
        }
        .upgrade()?;
        let process = parent.lock().unwrap();
        process.page_table.get_base().m_memory.clone()
    })();
    let Some(memory) = memory else {
        return Vec::new();
    };
    let mut buf = vec![0u8; len];
    let mem = memory.lock().unwrap();
    let _ = mem.read_block(address, &mut buf);
    buf
}

fn record_ipc_diff(session_handle: Handle, req: &[u8], rsp: &[u8], result: ResultCode) {
    let Some(writer) = ipc_diff_writer() else {
        return;
    };
    let seq = IPC_DIFF_SEQ.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let tid = std::thread::current()
        .name()
        .map(|s| s.to_string())
        .unwrap_or_default();

    use std::fmt::Write as _;
    let mut line = String::with_capacity(req.len() * 2 + rsp.len() * 2 + 128);
    line.push_str("{\"seq\":");
    let _ = write!(line, "{}", seq);
    line.push_str(",\"handle\":\"0x");
    let _ = write!(line, "{:X}", session_handle);
    line.push_str("\",\"tid\":\"");
    line.push_str(&tid);
    line.push_str("\",\"req_len\":");
    let _ = write!(line, "{}", req.len());
    line.push_str(",\"rsp_len\":");
    let _ = write!(line, "{}", rsp.len());
    line.push_str(",\"req\":\"");
    for b in req {
        let _ = write!(line, "{:02x}", b);
    }
    line.push_str("\",\"rsp\":\"");
    for b in rsp {
        let _ = write!(line, "{:02x}", b);
    }
    line.push_str("\",\"result\":\"0x");
    let _ = write!(line, "{:X}", result.get_inner_value());
    line.push_str("\"}\n");

    use std::io::Write as _;
    let mut w = writer.lock().unwrap();
    let _ = w.write_all(line.as_bytes());
    let _ = w.flush();
}

/// Per-handle IPC profile aggregator. `RUZU_PROFILE_IPC=1` populates this;
/// `dump_ipc_profile()` prints the top-N hottest handles on demand
/// (registered as a SIGUSR2 handler in `ruzu_cmd::main`, or call at exit).
static IPC_PROFILE: std::sync::OnceLock<
    std::sync::Mutex<std::collections::HashMap<u32, IpcProfileEntry>>,
> = std::sync::OnceLock::new();

#[derive(Default, Clone)]
struct IpcProfileEntry {
    count: u64,
    total_ns: u64,
    max_ns: u64,
}

fn record_ipc_profile(session_handle: u32, elapsed: std::time::Duration) {
    let map = IPC_PROFILE.get_or_init(|| std::sync::Mutex::new(std::collections::HashMap::new()));
    let ns = elapsed.as_nanos() as u64;
    let mut guard = map.lock().unwrap();
    let entry = guard.entry(session_handle).or_default();
    entry.count += 1;
    entry.total_ns += ns;
    if ns > entry.max_ns {
        entry.max_ns = ns;
    }
}

/// Per-phase aggregator for `RUZU_PROFILE_IPC_PHASES`. Keyed by phase label.
static IPC_PHASE_PROFILE: std::sync::OnceLock<
    std::sync::Mutex<std::collections::HashMap<&'static str, IpcProfileEntry>>,
> = std::sync::OnceLock::new();

pub(crate) fn record_ipc_phase(label: &'static str, elapsed: std::time::Duration) {
    let map =
        IPC_PHASE_PROFILE.get_or_init(|| std::sync::Mutex::new(std::collections::HashMap::new()));
    let ns = elapsed.as_nanos() as u64;
    let mut guard = map.lock().unwrap();
    let entry = guard.entry(label).or_default();
    entry.count += 1;
    entry.total_ns = entry.total_ns.saturating_add(ns);
    if ns > entry.max_ns {
        entry.max_ns = ns;
    }
}

pub fn dump_ipc_phase_profile() {
    let Some(map) = IPC_PHASE_PROFILE.get() else {
        return;
    };
    let entries: Vec<(&'static str, IpcProfileEntry)> = {
        let guard = map.lock().unwrap();
        guard.iter().map(|(k, v)| (*k, v.clone())).collect()
    };
    if entries.is_empty() {
        return;
    }
    let mut entries = entries;
    entries.sort_by_key(|(k, _)| *k); // alphabetical by phase label for ordered output
    eprintln!("[IPC_PHASE_PROFILE] per-phase wall-clock (sorted by label):");
    for (label, e) in entries.iter() {
        eprintln!(
            "[IPC_PHASE_PROFILE]   phase={:<36} count={:<7} total={:>9.2}ms avg={:>8.2}us max={:>8.2}us",
            label,
            e.count,
            e.total_ns as f64 / 1e6,
            e.total_ns as f64 / e.count as f64 / 1e3,
            e.max_ns as f64 / 1e3,
        );
    }
}

/// Print the IPC-profile histogram. Sorted by total_ns descending.
/// Top 20 entries by default. Called on SIGUSR2 / exit.
pub fn dump_ipc_profile() {
    let Some(map) = IPC_PROFILE.get() else {
        return;
    };
    let snap: Vec<(u32, IpcProfileEntry)> = {
        let guard = map.lock().unwrap();
        guard.iter().map(|(h, e)| (*h, e.clone())).collect()
    };
    let mut snap = snap;
    snap.sort_by_key(|(_, e)| std::cmp::Reverse(e.total_ns));
    eprintln!("[IPC_PROFILE] top handles by total time:");
    for (h, e) in snap.iter().take(20) {
        eprintln!(
            "[IPC_PROFILE]   handle=0x{:X}  count={}  total={:.2}ms  avg={:.1}us  max={:.1}us",
            h,
            e.count,
            e.total_ns as f64 / 1e6,
            e.total_ns as f64 / e.count as f64 / 1e3,
            e.max_ns as f64 / 1e3,
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::System;
    use crate::hle::ipc;
    use crate::hle::kernel::k_process::KProcess;
    use crate::hle::kernel::k_thread::{KThread, ThreadState};
    use crate::hle::kernel::k_typed_address::KProcessAddress;
    use crate::hle::kernel::svc::svc_port;
    use crate::hle::result::RESULT_SUCCESS;
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::sm::sm::ServiceManager;
    use std::sync::atomic::Ordering;
    use std::sync::{Arc, Mutex};

    fn test_system() -> System {
        let mut system = System::new_for_test();

        let mut process = KProcess::new();
        process.process_id = 100;
        process.capabilities.core_mask = 0xF;
        process.capabilities.priority_mask = u64::MAX;
        process.flags = 0;
        process.initialize_handle_table();
        process.allocate_code_memory(0x200000, 0x60000);

        let process = Arc::new(ProcessLock::from_value(process));
        let current_thread = Arc::new(KThreadLock::new(KThread::new()));
        {
            let mut thread = current_thread.lock().unwrap();
            thread.thread_id = 1;
            thread.object_id = 1;
            thread.parent = Some(Arc::downgrade(&process));
            thread.tls_address = KProcessAddress::new(0x2395000);
            thread
                .thread_state
                .store(ThreadState::RUNNABLE.bits(), Ordering::Relaxed);
        }
        {
            let mut process_guard = process.lock().unwrap();
            process_guard.register_thread_object(current_thread);
            let mut mem = process_guard.process_memory.write().unwrap();
            mem.allocate(0x200000, 0x600000);
            mem.allocate(0x2395000, 0x4000);
        }

        let scheduler = Arc::new(Mutex::new(
            crate::hle::kernel::k_scheduler::KScheduler::new(0),
        ));
        scheduler.lock().unwrap().initialize(1, 0, 0);
        let shared_memory = process.lock().unwrap().get_shared_memory();

        system.set_current_process_arc(process);
        system.set_scheduler_arc(scheduler);
        system.set_shared_process_memory(shared_memory);
        system.set_runtime_program_id(1);
        system.set_runtime_64bit(false);
        system
    }

    fn get_tls_base(system: &System) -> u64 {
        system
            .current_thread()
            .unwrap()
            .lock()
            .unwrap()
            .get_tls_address()
            .get()
    }

    fn write_named_port(system: &System, address: u64, name: &str) {
        let mut mem = system.shared_process_memory().write().unwrap();
        for (index, byte) in name.as_bytes().iter().copied().enumerate() {
            mem.write_8(address + index as u64, byte);
        }
        mem.write_8(address + name.len() as u64, 0);
    }

    fn write_sm_initialize_request(system: &System) {
        let tls_base = get_tls_base(system);
        let request_type = ipc::CommandType::Request as u32;
        let sfci_magic = u32::from_le_bytes([b'S', b'F', b'C', b'I']);
        let mut mem = system.shared_process_memory().write().unwrap();
        mem.write_32(tls_base, request_type);
        mem.write_32(tls_base + 4, 0);
        mem.write_32(tls_base + 0x10, sfci_magic);
        mem.write_32(tls_base + 0x14, 0);
        mem.write_32(tls_base + 0x18, 0);
        mem.write_32(tls_base + 0x1C, 0);
    }

    fn write_sm_get_service_request(system: &System, name: &str) {
        let tls_base = get_tls_base(system);
        let request_type = ipc::CommandType::Request as u32;
        let sfci_magic = u32::from_le_bytes([b'S', b'F', b'C', b'I']);
        let mut name_buf = [0u8; 8];
        let copy_len = name.len().min(name_buf.len());
        name_buf[..copy_len].copy_from_slice(&name.as_bytes()[..copy_len]);

        let mut mem = system.shared_process_memory().write().unwrap();
        mem.write_32(tls_base, request_type);
        mem.write_32(tls_base + 4, 0);
        mem.write_32(tls_base + 0x10, sfci_magic);
        mem.write_32(tls_base + 0x14, 0);
        mem.write_32(tls_base + 0x18, 1);
        mem.write_32(tls_base + 0x1C, 0);
        mem.write_64(tls_base + 0x20, u64::from_le_bytes(name_buf));
    }

    fn write_control_query_pointer_buffer_size_request(system: &System) {
        let tls_base = get_tls_base(system);
        write_control_query_pointer_buffer_size_request_at(system, tls_base);
    }

    fn write_control_query_pointer_buffer_size_request_at(system: &System, base: u64) {
        let control_type = ipc::CommandType::Control as u32;
        let sfci_magic = u32::from_le_bytes([b'S', b'F', b'C', b'I']);
        let mut mem = system.shared_process_memory().write().unwrap();
        mem.write_32(base, control_type);
        mem.write_32(base + 4, 0);
        mem.write_32(base + 0x10, sfci_magic);
        mem.write_32(base + 0x14, 0);
        mem.write_32(base + 0x18, 3);
        mem.write_32(base + 0x1C, 0);
    }

    #[test]
    fn format_ipc_trace_words_matches_zuyu_grouping() {
        let system = test_system();
        let tls_base = get_tls_base(&system);
        {
            let mut mem = system.shared_process_memory().write().unwrap();
            mem.write_32(tls_base, 0x0111_0004);
            mem.write_32(tls_base + 4, 0x0000_0c0b);
            mem.write_32(tls_base + 8, 0x4000_a078);
            mem.write_32(tls_base + 12, 0);
        }

        let formatted = format_ipc_trace_words(&system, tls_base, 4).unwrap();
        assert_eq!(formatted, "04001101 0b0c0000 78a00040 00000000");
    }

    #[test]
    fn send_sync_request_dispatches_sm_initialize_over_tls() {
        let system = test_system();
        let tls_base = get_tls_base(&system);
        let port_name = 0x2395800;
        write_named_port(&system, port_name, "sm:");

        let mut session_handle = 0;
        assert_eq!(
            svc_port::connect_to_named_port(&system, &mut session_handle, port_name),
            RESULT_SUCCESS
        );

        write_sm_initialize_request(&system);
        assert_eq!(send_sync_request(&system, session_handle), RESULT_SUCCESS);

        let mem = system.shared_process_memory().read().unwrap();
        assert_eq!(
            mem.read_32(tls_base + 0x18),
            RESULT_SUCCESS.get_inner_value()
        );
        assert_eq!(mem.read_32(tls_base + 0x1C), 0);
    }

    #[test]
    fn send_sync_request_dispatches_control_query_pointer_buffer_size_for_service_session() {
        let system = test_system();
        let tls_base = get_tls_base(&system);
        let current_thread = system
            .current_process_arc()
            .lock()
            .unwrap()
            .get_thread_by_thread_id(1)
            .unwrap();
        let mut request_context = HLERequestContext::new_with_thread(current_thread, tls_base);
        let service_manager = system.service_manager().unwrap();
        request_context.set_service_manager(service_manager);
        let lm_handler: SessionRequestHandlerPtr = Arc::new(crate::hle::service::lm::lm::LM::new());
        let lm_handle = request_context
            .create_session_for_service(lm_handler)
            .unwrap();

        write_control_query_pointer_buffer_size_request(&system);
        assert_eq!(send_sync_request(&system, lm_handle), RESULT_SUCCESS);

        let mem = system.shared_process_memory().read().unwrap();
        assert_eq!(
            mem.read_32(tls_base + 0x18),
            RESULT_SUCCESS.get_inner_value()
        );
        assert_eq!(mem.read_32(tls_base + 0x1C), 0);
        assert_eq!(mem.read_32(tls_base + 0x20), 0x8000);
    }

    #[test]
    fn send_sync_request_uses_server_session_manager_not_client_session_field() {
        let system = test_system();
        let tls_base = get_tls_base(&system);
        let current_thread = system
            .current_process_arc()
            .lock()
            .unwrap()
            .get_thread_by_thread_id(1)
            .unwrap();
        let mut request_context = HLERequestContext::new_with_thread(current_thread, tls_base);
        let service_manager = system.service_manager().unwrap();
        request_context.set_service_manager(service_manager);
        let lm_handler: SessionRequestHandlerPtr = Arc::new(crate::hle::service::lm::lm::LM::new());
        let lm_handle = request_context
            .create_session_for_service(lm_handler)
            .unwrap();

        {
            let process = system.current_process_arc();
            let process = process.lock().unwrap();
            let client_session_object_id = process.handle_table.get_object(lm_handle).unwrap();
            let client_session = process
                .get_client_session_by_object_id(client_session_object_id)
                .unwrap();
            client_session.lock().unwrap().request_manager = None;
        }

        write_control_query_pointer_buffer_size_request(&system);
        assert_eq!(send_sync_request(&system, lm_handle), RESULT_SUCCESS);

        let mem = system.shared_process_memory().read().unwrap();
        assert_eq!(
            mem.read_32(tls_base + 0x18),
            RESULT_SUCCESS.get_inner_value()
        );
        assert_eq!(mem.read_32(tls_base + 0x20), 0x8000);
    }

    #[test]
    fn send_sync_request_with_user_buffer_dispatches_on_message_buffer() {
        let system = test_system();
        let message = 0x23A0000u64;
        {
            let process = system.current_process_arc();
            let process_guard = process.lock().unwrap();
            let mut mem = process_guard.process_memory.write().unwrap();
            mem.allocate(message, PAGE_SIZE as usize);
        }

        let current_thread = system
            .current_process_arc()
            .lock()
            .unwrap()
            .get_thread_by_thread_id(1)
            .unwrap();
        let mut request_context = HLERequestContext::new_with_thread(current_thread, message);
        let service_manager = system.service_manager().unwrap();
        request_context.set_service_manager(service_manager);
        let lm_handler: SessionRequestHandlerPtr = Arc::new(crate::hle::service::lm::lm::LM::new());
        let lm_handle = request_context
            .create_session_for_service(lm_handler)
            .unwrap();

        write_control_query_pointer_buffer_size_request_at(&system, message);
        assert_eq!(
            send_sync_request_with_user_buffer(&system, message, PAGE_SIZE, lm_handle),
            RESULT_SUCCESS
        );

        let mem = system.shared_process_memory().read().unwrap();
        assert_eq!(
            mem.read_32(message + 0x18),
            RESULT_SUCCESS.get_inner_value()
        );
        assert_eq!(mem.read_32(message + 0x1C), 0);
        assert_eq!(mem.read_32(message + 0x20), 0x8000);
    }

    #[test]
    fn send_async_request_with_user_buffer_returns_real_readable_event_handle() {
        let system = test_system();
        let tls_base = get_tls_base(&system);
        let current_thread = system
            .current_process_arc()
            .lock()
            .unwrap()
            .get_thread_by_thread_id(1)
            .unwrap();
        let mut request_context = HLERequestContext::new_with_thread(current_thread, tls_base);
        let service_manager = system.service_manager().unwrap();
        request_context.set_service_manager(service_manager);
        let lm_handler: SessionRequestHandlerPtr = Arc::new(crate::hle::service::lm::lm::LM::new());
        let lm_handle = request_context
            .create_session_for_service(lm_handler)
            .unwrap();

        let mut out_event_handle = 0;
        assert_eq!(
            send_async_request_with_user_buffer(
                &system,
                &mut out_event_handle,
                0x2395000,
                0x80,
                lm_handle
            ),
            RESULT_SUCCESS
        );
        assert_ne!(out_event_handle, 0);

        let process = system.current_process_arc();
        let process = process.lock().unwrap();
        let readable_object_id = process.handle_table.get_object(out_event_handle).unwrap();
        assert!(process
            .get_readable_event_by_object_id(readable_object_id)
            .is_some());

        let client_session_object_id = process.handle_table.get_object(lm_handle).unwrap();
        let client_session = process
            .get_client_session_by_object_id(client_session_object_id)
            .unwrap();
        let parent_id = process
            .get_client_session_parent_id(client_session_object_id)
            .unwrap();
        let parent_session = process.get_session_by_object_id(parent_id).unwrap();
        let server_session = parent_session.lock().unwrap().get_server_session().clone();
        drop(process);

        assert_eq!(server_session.lock().unwrap().receive_request(), 0);
        let current_request = server_session
            .lock()
            .unwrap()
            .get_current_request()
            .expect("async request");
        let current_request = current_request.lock().unwrap();
        assert!(current_request.get_event_id().is_some());
        assert_eq!(current_request.get_address(), 0x2395000);
        assert_eq!(current_request.get_size(), 0x80);
    }
}

/// Sends a sync request with a user-provided message buffer.
///
/// Upstream: Validates message buffer alignment, locks the page table for IPC,
/// sends the sync request using the locked buffer, then unlocks.
pub fn send_sync_request_with_user_buffer(
    system: &System,
    message: u64,
    buffer_size: u64,
    session_handle: Handle,
) -> ResultCode {
    // Validate that the message buffer is page aligned and does not overflow.
    if message % PAGE_SIZE != 0 {
        return RESULT_INVALID_ADDRESS;
    }
    if buffer_size == 0 {
        return RESULT_INVALID_SIZE;
    }
    if buffer_size % PAGE_SIZE != 0 {
        return RESULT_INVALID_SIZE;
    }
    if message >= message.wrapping_add(buffer_size) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // Lock the message buffer in the process page table.
    {
        let mut process = system.current_process_arc().lock().unwrap();
        let msg_addr = crate::hle::kernel::k_typed_address::KProcessAddress::new(message);
        let mut paddr: u64 = 0;
        let lock_result =
            process
                .page_table
                .lock_for_ipc_user_buffer(&mut paddr, msg_addr, buffer_size as usize);
        if lock_result != 0 {
            return ResultCode::new(lock_result);
        }
    }

    // Upstream SendSyncRequestWithUserBuffer dispatches on the user-provided message
    // buffer, not the caller TLS command buffer.
    let result = send_sync_request_impl(system, session_handle, message);

    // Unlock the message buffer.
    {
        let mut process = system.current_process_arc().lock().unwrap();
        let msg_addr = crate::hle::kernel::k_typed_address::KProcessAddress::new(message);
        let unlock_result = process
            .page_table
            .unlock_for_ipc_user_buffer(msg_addr, buffer_size as usize);
        if result == RESULT_SUCCESS && unlock_result != 0 {
            return ResultCode::new(unlock_result);
        }
    }

    result
}

/// Sends an async request with a user buffer.
///
/// Upstream: Creates an event, gets client session, sends async request.
/// The readable event is returned to the caller via out_event_handle.
///
/// Needs: KEvent::Create, KEvent::Initialize, KEvent::Register, KClientSession::SendAsyncRequest.
/// These kernel objects are not yet fully ported, so this returns the event handle
/// but the async send is deferred.
pub fn send_async_request_with_user_buffer(
    system: &System,
    out_event_handle: &mut Handle,
    message: u64,
    buffer_size: u64,
    session_handle: Handle,
) -> ResultCode {
    let mut process = system.current_process_arc().lock().unwrap();
    let mut event_reservation = KScopedResourceReservation::new(
        process.resource_limit.clone(),
        LimitableResource::EventCountMax,
        1,
    );

    if !event_reservation.succeeded() {
        return RESULT_LIMIT_REACHED;
    }

    let Some(session_object_id) = process.handle_table.get_object(session_handle) else {
        return RESULT_INVALID_HANDLE;
    };
    let Some(client_session) = process.get_client_session_by_object_id(session_object_id) else {
        return RESULT_INVALID_HANDLE;
    };
    if process
        .get_client_session_parent_id(session_object_id)
        .is_none()
    {
        return RESULT_INVALID_HANDLE;
    }

    let event_object_id = system.kernel().unwrap().create_new_object_id() as u64;
    let readable_event_object_id = system.kernel().unwrap().create_new_object_id() as u64;
    let event = Arc::new(Mutex::new(KEvent::new()));
    let readable_event = Arc::new(Mutex::new(KReadableEvent::new()));

    readable_event
        .lock()
        .unwrap()
        .initialize(event_object_id, readable_event_object_id);
    event
        .lock()
        .unwrap()
        .initialize(process.process_id, readable_event_object_id);

    process.register_event_object(event_object_id, Arc::clone(&event));
    process.register_readable_event_object(readable_event_object_id, Arc::clone(&readable_event));

    let readable_handle = match process.handle_table.add(readable_event_object_id) {
        Ok(handle) => handle,
        Err(_) => {
            process.unregister_event_object_by_object_id(event_object_id);
            process.unregister_readable_event_object_by_object_id(readable_event_object_id);
            return RESULT_OUT_OF_HANDLES;
        }
    };

    let send_result = client_session
        .lock()
        .unwrap()
        .send_async_request_with_process(
            &mut process,
            event_object_id,
            message as usize,
            buffer_size as usize,
        );
    if send_result != 0 {
        process.handle_table.remove(readable_handle);
        process.unregister_event_object_by_object_id(event_object_id);
        process.unregister_readable_event_object_by_object_id(readable_event_object_id);
        return ResultCode::new(send_result);
    }

    event_reservation.commit();
    *out_event_handle = readable_handle;
    RESULT_SUCCESS
}

/// Replies and receives IPC messages.
///
/// Upstream: Validates handle count, copies handles from user memory, resolves
/// synchronization objects, optionally sends a reply to reply_target, then
/// waits for a message on any of the provided server sessions.
pub fn reply_and_receive(
    system: &System,
    out_index: &mut i32,
    handles: u64,
    num_handles: i32,
    reply_target: Handle,
    timeout_ns: i64,
) -> ResultCode {
    let current_thread_id = system.current_thread_id();
    if should_trace_reply_receive_debug() {
        log::info!(
            "svc::ReplyAndReceive tid={:?} handles=0x{:X} num_handles={} reply_target=0x{:X} timeout_ns={}",
            current_thread_id,
            handles,
            num_handles,
            reply_target,
            timeout_ns
        );
    }
    let timeout = if timeout_ns > 0 {
        let current_tick = system
            .kernel()
            .and_then(|_| crate::hle::kernel::kernel::get_current_hardware_tick())
            .unwrap_or(i64::MAX);
        let timeout_tick = ipc_timeout_tick_from_ns(current_tick, timeout_ns);
        if should_trace_reply_receive_debug() {
            log::info!(
                "svc::ReplyAndReceive tid={:?} current_tick={} timeout_tick={}",
                current_thread_id,
                current_tick,
                timeout_tick
            );
        }
        timeout_tick
    } else {
        timeout_ns
    };

    let result = reply_and_receive_impl(
        system,
        out_index,
        0,
        0,
        handles,
        num_handles,
        reply_target,
        timeout,
    );
    if should_trace_reply_receive_debug() {
        log::info!(
            "svc::ReplyAndReceive tid={:?} result=0x{:08X} out_index={}",
            current_thread_id,
            result.get_inner_value(),
            *out_index
        );
    }
    result
}

/// Replies and receives with a user-provided message buffer.
///
/// Upstream: Same as ReplyAndReceive but with a locked user buffer for the
/// message instead of using the TLS.
pub fn reply_and_receive_with_user_buffer(
    system: &System,
    out_index: &mut i32,
    message: u64,
    buffer_size: u64,
    handles: u64,
    num_handles: i32,
    reply_target: Handle,
    timeout_ns: i64,
) -> ResultCode {
    // Validate that the message buffer is page aligned and does not overflow.
    if message % PAGE_SIZE != 0 {
        return RESULT_INVALID_ADDRESS;
    }
    if buffer_size == 0 {
        return RESULT_INVALID_SIZE;
    }
    if buffer_size % PAGE_SIZE != 0 {
        return RESULT_INVALID_SIZE;
    }
    if message >= message.wrapping_add(buffer_size) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    let current_thread_id = system.current_thread_id();
    if should_trace_reply_receive_debug() {
        log::info!(
            "svc::ReplyAndReceiveWithUserBuffer tid={:?} message=0x{:X} buffer_size=0x{:X} handles=0x{:X} num_handles={} reply_target=0x{:X} timeout_ns={}",
            current_thread_id,
            message,
            buffer_size,
            handles,
            num_handles,
            reply_target,
            timeout_ns
        );
    }

    // Lock the message buffer.
    {
        let mut process = system.current_process_arc().lock().unwrap();
        let msg_addr = crate::hle::kernel::k_typed_address::KProcessAddress::new(message);
        let mut paddr: u64 = 0;
        let lock_result =
            process
                .page_table
                .lock_for_ipc_user_buffer(&mut paddr, msg_addr, buffer_size as usize);
        if lock_result != 0 {
            return ResultCode::new(lock_result);
        }
    }

    let timeout = if timeout_ns > 0 {
        let current_tick = system
            .kernel()
            .and_then(|_| crate::hle::kernel::kernel::get_current_hardware_tick())
            .unwrap_or(i64::MAX);
        let timeout_tick = ipc_timeout_tick_from_ns(current_tick, timeout_ns);
        if should_trace_reply_receive_debug() {
            log::info!(
                "svc::ReplyAndReceiveWithUserBuffer tid={:?} current_tick={} timeout_tick={}",
                current_thread_id,
                current_tick,
                timeout_tick
            );
        }
        timeout_tick
    } else {
        timeout_ns
    };

    // Perform the reply/receive.
    let result = reply_and_receive_impl(
        system,
        out_index,
        message,
        buffer_size,
        handles,
        num_handles,
        reply_target,
        timeout,
    );
    if should_trace_reply_receive_debug() {
        log::info!(
            "svc::ReplyAndReceiveWithUserBuffer tid={:?} result=0x{:08X} out_index={}",
            current_thread_id,
            result.get_inner_value(),
            *out_index
        );
    }

    // Unlock the message buffer.
    {
        let mut process = system.current_process_arc().lock().unwrap();
        let msg_addr = crate::hle::kernel::k_typed_address::KProcessAddress::new(message);
        let unlock_result = process
            .page_table
            .unlock_for_ipc_user_buffer(msg_addr, buffer_size as usize);
        if result == RESULT_SUCCESS && unlock_result != 0 {
            return ResultCode::new(unlock_result);
        }
    }

    result
}

/// Internal implementation of ReplyAndReceive.
///
/// Upstream: Validates handle count, reads handles from user memory, resolves
/// synchronization objects, sends reply if reply_target is valid, then
/// enters a wait loop for incoming messages.
fn reply_and_receive_impl(
    system: &System,
    out_index: &mut i32,
    _message: u64,
    _buffer_size: u64,
    handles: u64,
    num_handles: i32,
    reply_target: Handle,
    timeout_ns: i64,
) -> ResultCode {
    use crate::hle::kernel::svc_common::ARGUMENT_HANDLE_COUNT_MAX;

    // Ensure number of handles is valid.
    if num_handles < 0 || num_handles > ARGUMENT_HANDLE_COUNT_MAX {
        return RESULT_OUT_OF_RANGE;
    }

    let process_arc = system.current_process_arc();
    let current_thread = match system.current_thread() {
        Some(thread) => thread,
        None => return RESULT_INVALID_HANDLE,
    };
    let scheduler = system.scheduler_arc();

    let mut process = process_arc.lock().unwrap();

    let mut handle_values = Vec::with_capacity(num_handles as usize);
    if num_handles > 0 {
        let handle_bytes = num_handles as usize * std::mem::size_of::<Handle>();
        if let Some(memory) = process.page_table.get_base().m_memory.as_ref() {
            let memory = memory.lock().unwrap();
            if !memory.is_valid_virtual_address_range(handles, handle_bytes as u64) {
                return RESULT_INVALID_POINTER;
            }
            for i in 0..num_handles as usize {
                handle_values.push(memory.read_32(handles + (i * 4) as u64));
            }
        } else {
            let memory = process.process_memory.read().unwrap();
            if !memory.is_valid_range(handles, handle_bytes) {
                return RESULT_INVALID_POINTER;
            }
            for i in 0..num_handles as usize {
                handle_values.push(memory.read_32(handles + (i * 4) as u64));
            }
        }
    }

    let mut object_ids = Vec::with_capacity(num_handles as usize);
    for handle in &handle_values {
        let Some(object_id) = process.handle_table.get_object(*handle) else {
            return RESULT_INVALID_HANDLE;
        };
        object_ids.push(object_id);
    }

    if reply_target != INVALID_HANDLE {
        let Some(reply_object_id) = process.handle_table.get_object(reply_target) else {
            return RESULT_INVALID_HANDLE;
        };
        let Some(server_session) = process.get_server_session_by_object_id(reply_object_id) else {
            return RESULT_INVALID_HANDLE;
        };
        let reply_result = server_session.lock().unwrap().send_reply_with_message(
            _message as usize,
            _buffer_size as usize,
            0,
            false,
        );
        if reply_result != 0 {
            *out_index = -1;
            return ResultCode::new(reply_result);
        }
    }

    drop(process);

    loop {
        let wait_result = k_synchronization_object::wait(
            &process_arc,
            &current_thread,
            &scheduler,
            out_index,
            object_ids.clone(),
            timeout_ns,
        );
        if wait_result != RESULT_SUCCESS {
            return wait_result;
        }

        if *out_index < 0 || (*out_index as usize) >= object_ids.len() {
            return RESULT_INVALID_HANDLE;
        }

        let object_id = object_ids[*out_index as usize];
        let process = process_arc.lock().unwrap();
        let Some(server_session) = process.get_server_session_by_object_id(object_id) else {
            return RESULT_SUCCESS;
        };
        drop(process);

        let receive_result = server_session.lock().unwrap().receive_request_with_message(
            _message as usize,
            _buffer_size as usize,
            0,
        );
        if receive_result == RESULT_NOT_FOUND.get_inner_value() {
            continue;
        }
        if receive_result != 0 {
            return ResultCode::new(receive_result);
        }
        return RESULT_SUCCESS;
    }
}

#[cfg(test)]
mod reply_receive_tests {
    use super::ipc_timeout_tick_from_ns;

    #[test]
    fn ipc_timeout_tick_from_ns_matches_upstream_saturation_rule() {
        assert_eq!(ipc_timeout_tick_from_ns(100, 5), 107);
        assert_eq!(ipc_timeout_tick_from_ns(i64::MAX - 1, 10), i64::MAX);
    }
}
