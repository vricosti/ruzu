//! Port of zuyu/src/core/hle/kernel/svc/svc_ipc.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for IPC (Inter-Process Communication) operations.

use std::sync::{Arc, Mutex};

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

static SERVER_THREAD_IPC_COUNT: std::sync::atomic::AtomicU64 =
    std::sync::atomic::AtomicU64::new(0);

fn should_use_server_thread_ipc(session_handle: Handle) -> bool {
    let Some(spec) = std::env::var_os("RUZU_SERVER_THREAD_IPC_HANDLE") else {
        return false;
    };
    let spec = spec.to_string_lossy();
    let matches = spec.split(',').any(|raw| {
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
    });
    if !matches {
        return false;
    }
    let count = SERVER_THREAD_IPC_COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1;
    let limit = std::env::var("RUZU_SERVER_THREAD_IPC_LIMIT")
        .ok()
        .and_then(|value| value.parse::<u64>().ok())
        .unwrap_or(u64::MAX);
    count <= limit
}

fn trace_server_thread_ipc(stage: &str, session_handle: Handle) {
    if std::env::var_os("RUZU_TRACE_SERVER_THREAD_IPC").is_some() {
        eprintln!("[SERVER_THREAD_IPC] handle=0x{session_handle:X} stage={stage}");
    }
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
    let trace_sync = should_trace_sync_handle(session_handle);
    let (client_session, session_object_id) = {
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
        process
            .num_ipc_messages
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        (client_session, object_id)
    };
    record_phase("01_resolve_client_session", &mut phase_last);
    if trace_sync {
        log::info!("svc::SendSyncRequest stage=resolved_client_session");
    }

    if should_use_server_thread_ipc(session_handle) {
        trace_server_thread_ipc("enqueue_begin", session_handle);
        let (server_session, manager) = {
            let mut process = system.current_process_arc().lock().unwrap();
            let send_result = client_session
                .lock()
                .unwrap()
                .send_sync_request_with_process(&mut process, message_address as usize, 0);
            if send_result != 0 {
                return ResultCode::new(send_result);
            }

            let parent_id = match client_session.lock().unwrap().get_parent_id() {
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
        trace_server_thread_ipc("enqueue_end", session_handle);

        if let Some(current_thread) = system.current_thread() {
            let mut thread = current_thread.lock().unwrap();
            thread.set_wait_reason_for_debugging(
                crate::hle::kernel::k_thread::ThreadWaitReasonForDebugging::Ipc,
            );
            thread.begin_wait();
        }
        trace_server_thread_ipc("client_begin_wait", session_handle);

        let service_manager = system.service_manager().unwrap();
        std::thread::spawn(move || {
            trace_server_thread_ipc("worker_receive_begin", session_handle);
            let receive_result = {
                let mut server_session = server_session.lock().unwrap();
                server_session.receive_request_hle(Arc::clone(&manager))
            };
            let Ok((mut context, request_manager, _request_message_address)) = receive_result
            else {
                trace_server_thread_ipc("worker_receive_failed", session_handle);
                return;
            };
            trace_server_thread_ipc("worker_dispatch_begin", session_handle);
            context.set_service_manager(service_manager);
            let _ = complete_sync_request(&request_manager, &mut context);
            trace_server_thread_ipc("worker_reply_begin", session_handle);
            let _ = server_session.lock().unwrap().send_reply();
            trace_server_thread_ipc("worker_done", session_handle);
        });
        trace_server_thread_ipc("worker_spawned", session_handle);

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

        return system
            .current_thread()
            .map(|thread| ResultCode::new(thread.lock().unwrap().get_wait_result()))
            .unwrap_or(RESULT_INVALID_HANDLE);
    }

    let (request_manager, mut context, request_message_address) = {
        let (server_session, manager) = {
            let mut process = system.current_process_arc().lock().unwrap();
            record_phase("02_process_lock_2", &mut phase_last);
            if trace_sync {
                log::info!("svc::SendSyncRequest stage=enqueue_request");
            }
            let send_result = client_session
                .lock()
                .unwrap()
                .send_sync_request_with_process(&mut process, message_address as usize, 0);
            record_phase("03_send_sync_request_with_process", &mut phase_last);
            if send_result != 0 {
                return ResultCode::new(send_result);
            }

            let parent_id = match client_session.lock().unwrap().get_parent_id() {
                Some(parent_id) => parent_id,
                None => {
                    if trace_sync {
                        log::info!("svc::SendSyncRequest stage=missing_parent_id");
                    }
                    return RESULT_INVALID_HANDLE;
                }
            };
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
            (server_session, manager)
        };
        if trace_sync {
            log::info!("svc::SendSyncRequest stage=receive_request_hle_begin");
        }

        // Do not hold the owner `KProcess` lock across `receive_request_hle()`.
        // That path resolves the request client thread through kernel process
        // lookup, which would re-enter the same process mutex and deadlock on
        // the first synchronous IPC to services like "sm:".
        record_phase("04_resolve_server_session_manager", &mut phase_last);
        let receive_result = {
            let mut server_session = server_session.lock().unwrap();
            server_session.receive_request_hle(Arc::clone(&manager))
        };
        record_phase("05_receive_request_hle", &mut phase_last);
        match receive_result {
            Ok((context, manager, request_message_address)) => {
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
    let result = complete_sync_request(&request_manager, &mut context);
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
        let _ = server_session.lock().unwrap().send_reply();
        if trace_sync {
            log::info!("svc::SendSyncRequest stage=send_reply_end");
        }
    }
    record_phase("07_send_reply", &mut phase_last);

    // RUZU_RESCHEDULE_AFTER_IPC=1 — Quick experiment for MK8D wedge investigation
    // (project_mk8d_lost_wakeup_cv_69a545ac_2026_05_21). Upstream's
    // KServerSession::OnRequest transitions the caller through ThreadState::WAITING
    // during the IPC (via BeginWait inside KScopedSchedulerLock), which causes the
    // scheduler to dispatch other RUNNABLE threads of equal-or-lower priority on
    // the same core when the lock drops. ruzu's HLE IPC path runs synchronously
    // with no thread state transition, so the scheduler is never invoked between
    // SVCs. This experimental knob calls reschedule_current_core() at the end of
    // each HLE IPC to give the scheduler a chance to dispatch waiting threads.
    if std::env::var_os("RUZU_RESCHEDULE_AFTER_IPC").is_some() {
        if let Some(kernel) = system.kernel() {
            if let Some(scheduler) = kernel.current_scheduler() {
                let sched_ptr = {
                    let guard = scheduler.lock().unwrap();
                    &*guard as *const crate::hle::kernel::k_scheduler::KScheduler
                        as *mut crate::hle::kernel::k_scheduler::KScheduler
                };
                // preempt_single_core yields to the scheduler fiber and lets it
                // pick the next runnable thread. Mirrors upstream's preemption
                // tick behavior (RotateScheduledQueue) but on every HLE IPC
                // rather than every 10ms. Heavy-handed but tests whether giving
                // the scheduler an opportunity to run other threads is what we
                // need for the MK8D tid=86 dispatch race.
                unsafe {
                    (*sched_ptr).preempt_single_core();
                }
            }
        }
    }

    // RUZU_YIELD_AFTER_IPC=1 — diagnostic for the MK8D post-StartThread
    // ordering divergence. Unlike RUZU_RESCHEDULE_AFTER_IPC, this uses the
    // scheduler's queue-rotation yield path, approximating the visible effect
    // of upstream `KServerSession::OnRequest` putting the client thread into
    // IPC wait while the server handles the request.
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

    if result == crate::hle::service::ipc_helpers::RESULT_SESSION_CLOSED {
        return RESULT_SUCCESS;
    }

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
    // `RUZU_PROFILE_IPC=1` — measure wall-clock time per SendSyncRequest
    // and dump a per-handle histogram (count, total_us, avg_us, max_us)
    // on a SIGUSR1 or process exit. Used to find HLE handlers that are
    // bottlenecking the producer thread (project_kernel_race_attack_surface_2026_05_14).
    if std::env::var_os("RUZU_PROFILE_IPC").is_some() {
        let start = std::time::Instant::now();
        let result = send_sync_request_impl(system, session_handle, tls_address);
        record_ipc_profile(session_handle, start.elapsed());
        result
    } else {
        send_sync_request_impl(system, session_handle, tls_address)
    }
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
        let parent_id = client_session.lock().unwrap().get_parent_id().unwrap();
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
    if client_session.lock().unwrap().get_parent_id().is_none() {
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
