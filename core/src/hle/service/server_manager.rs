// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/server_manager.h and server_manager.cpp
//!
//! Contains:
//! - ServerManager: manages server ports and sessions for HLE services
//! - Session: wrapper pairing a KServerSession with a SessionRequestManager
//!
//! Upstream uses MultiWait/MultiWaitHolder for the event loop.
//! The Rust port still approximates that structure, but now blocks the guest
//! service thread on a kernel-readable wakeup event instead of keeping it
//! runnable in a round-robin yield loop.
//!
//! IPC dispatch in ruzu is being migrated toward upstream's
//! `KClientSession::SendSyncRequest` → `KServerSession::OnRequest` →
//! `ServerManager::OnSessionEvent` flow. Binder and explicit host-thread
//! routes use this event loop; non-routed services still keep a temporary
//! inline fallback in `svc_ipc.rs`.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, Weak};
use std::thread::JoinHandle;
use std::time::{Duration, Instant};

use crate::core::SystemRef;
use crate::hle::kernel::k_event::KEvent;
use crate::hle::kernel::k_port::KPort;
use crate::hle::kernel::k_process::KProcess;
use crate::hle::kernel::k_process::ProcessLock;
use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::kernel::k_scheduler::KScheduler;
use crate::hle::kernel::k_server_session::KServerSession;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{
    self, HLERequestContext, PendingRegistrationQueue, SessionRequestHandlerFactory,
    SessionRequestHandlerPtr, SessionRequestManager,
};
use crate::hle::service::os::event::Event;
use crate::hle::service::os::multi_wait::MultiWait;
use crate::hle::service::os::multi_wait_holder::MultiWaitHolder;
use crate::hle::service::sm::sm::ServiceManager;

/// Tag for MultiWaitHolder user data, matching upstream `UserDataTag`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
enum UserDataTag {
    Port = 0,
    Session = 1,
    DeferEvent = 2,
}

/// Session wrapper pairing a KServerSession with its SessionRequestManager.
///
/// Matches upstream `Service::Session` (server_manager.cpp).
/// Upstream also stores an HLERequestContext for in-flight requests.
struct Session {
    id: u64,
    holder: Box<MultiWaitHolder>,
    server_session: Arc<Mutex<KServerSession>>,
    manager: Arc<Mutex<SessionRequestManager>>,
    /// Stored context for in-flight requests.
    /// Upstream: `HLERequestContext context` stored per-session.
    context: Option<HLERequestContext>,
}

#[derive(Default)]
struct LoopStats {
    wakeup_hits: u64,
    deferral_hits: u64,
    port_hits: u64,
    session_hits: u64,
    idle_timeouts: u64,
}

struct SharedSessionEvent {
    session_id: u64,
    server_session: Arc<Mutex<KServerSession>>,
    manager: Arc<Mutex<SessionRequestManager>>,
    service_manager: Option<Arc<Mutex<ServiceManager>>>,
    server_name: String,
}

enum SelectedSharedEvent {
    Session(SharedSessionEvent),
    Locked(*mut MultiWaitHolder),
}

fn ipc_phase_timer() -> Option<Instant> {
    std::env::var_os("RUZU_PROFILE_IPC_PHASES")
        .is_some()
        .then(Instant::now)
}

fn record_ipc_phase(label: &'static str, last: &mut Option<Instant>) {
    if let Some(start) = last {
        crate::hle::kernel::svc::svc_ipc::record_ipc_phase(label, start.elapsed());
        *last = Some(Instant::now());
    }
}

fn assert_receive_request_hle_result(server_name: &str, session_id: u64, result: u32) -> ! {
    panic!(
        "ServerManager({}): unexpected ReceiveRequestHLE result for session {}: {:#x}",
        server_name, session_id, result
    );
}

impl Session {
    fn new(
        id: u64,
        server_session: Arc<Mutex<KServerSession>>,
        manager: Arc<Mutex<SessionRequestManager>>,
    ) -> Self {
        let mut holder = Box::new(MultiWaitHolder::from_server_session(server_session.clone()));
        holder.set_user_data(UserDataTag::Session as usize);
        Self {
            id,
            holder,
            server_session,
            manager,
            context: None,
        }
    }

    fn holder_ptr(&self) -> *const MultiWaitHolder {
        &*self.holder as *const MultiWaitHolder
    }
}

/// Port wrapper pairing a waited server port with its handler factory.
///
/// Matches upstream `Service::Port` ownership in `server_manager.cpp`.
struct Port {
    holder: Box<MultiWaitHolder>,
    port: Arc<Mutex<KPort>>,
    server_port_object_id: Option<u64>,
    named_client_port_object_id: Option<u64>,
    registered_in_process: bool,
    handler_factory: SessionRequestHandlerFactory,
}

impl Port {
    fn new(
        port: Arc<Mutex<KPort>>,
        server_port_object_id: Option<u64>,
        named_client_port_object_id: Option<u64>,
        handler_factory: SessionRequestHandlerFactory,
    ) -> Self {
        let mut holder = Box::new(MultiWaitHolder::from_server_port(
            port.clone(),
            server_port_object_id,
        ));
        holder.set_user_data(UserDataTag::Port as usize);
        Self {
            holder,
            port,
            server_port_object_id,
            named_client_port_object_id,
            registered_in_process: false,
            handler_factory,
        }
    }

    fn holder_ptr(&self) -> *const MultiWaitHolder {
        &*self.holder as *const MultiWaitHolder
    }

    fn create_handler(&self) -> SessionRequestHandlerPtr {
        (self.handler_factory)()
    }
}

/// Manages server ports and sessions for HLE services.
///
/// Port of upstream `Service::ServerManager`.
/// Upstream uses MultiWait for the event loop (WaitSignaled → Process).
/// We implement the same pattern with our MultiWait/MultiWaitHolder.
pub struct ServerManager {
    /// Reference to the System, matching upstream `Core::System& m_system`.
    system: SystemRef,

    /// Service name for thread identification.
    name: String,

    /// Ensures only one host thread selects a `MultiWaitHolder` at a time.
    /// Upstream: `Mutex m_selection_mutex`.
    selection_mutex: Arc<Mutex<()>>,

    /// Active managed server ports.
    ports: Vec<Port>,

    /// Active sessions.
    sessions: Vec<Session>,

    /// Stable Rust equivalent of upstream `Session*` identity.
    next_session_id: u64,

    /// Wakeup event — signaled to wake the event loop when new items are linked.
    /// Upstream: `Kernel::KEvent* m_wakeup_event`.
    wakeup_event: Arc<Event>,

    /// Deferral event — signaled when deferred requests should be retried.
    /// Upstream: `Kernel::KEvent* m_deferral_event`.
    deferral_event: Option<Arc<Mutex<KEvent>>>,

    /// The main multi-wait for the event loop.
    /// Upstream: `MultiWait m_multi_wait`.
    multi_wait: MultiWait,

    /// Deferred list — items to be linked into multi_wait on next iteration.
    /// Upstream: `MultiWait m_deferred_list` + `std::mutex m_deferred_list_mutex`.
    deferred_list: Mutex<MultiWait>,

    /// Deferred sessions awaiting retry.
    /// Upstream: `std::list<Session*> m_deferred_sessions`.
    deferred_sessions: Vec<u64>,

    /// Wakeup holder in the multi-wait.
    /// Upstream: `std::optional<MultiWaitHolder> m_wakeup_holder`.
    wakeup_holder: Option<Box<MultiWaitHolder>>,

    /// Deferral holder in the multi-wait.
    /// Upstream: `std::optional<MultiWaitHolder> m_deferral_holder`.
    deferral_holder: Option<Box<MultiWaitHolder>>,

    /// Shared queue of pending session registrations. External handlers
    /// push to this Arc instead of locking `Mutex<ServerManager>` (which is
    /// held for the lifetime of `loop_process` by the host thread). The host
    /// thread drains the queue at the start of `wait_and_process_impl`.
    ///
    /// Forward-compatible workaround for sm.rs / sm_controller.rs /
    /// ipc_helpers.rs that previously called `register_session` directly on a
    /// locked ServerManager (deadlock). Once ServerManager's master mutex is
    /// fully refactored into per-field locks (upstream pattern), this queue
    /// can be removed in favor of direct calls.
    pending_registrations: PendingRegistrationQueue,

    /// Stop flag. Upstream: `std::stop_source m_stop_source`.
    stop_requested: Arc<AtomicBool>,

    /// Whether the server has been stopped.
    stopped: AtomicBool,

    /// Whether `LoopProcess` has entered the event loop at least once.
    loop_started: AtomicBool,

    /// Additional host threads requested by upstream call sites such as
    /// `Sockets::LoopProcess`.
    ///
    /// Upstream: `std::vector<std::jthread> m_threads` is populated by
    /// `StartAdditionalHostThreads(...)`. Rust records the requested threads
    /// here first, then activates them after `loop_process_shared(...)` has
    /// prepared holder linkage on the final shared owner.
    pending_additional_host_threads: Vec<(String, usize)>,

    /// Owned handles for additional host threads.
    /// Upstream: `m_threads`.
    host_threads: Vec<JoinHandle<()>>,

    /// Shared self-owner used where upstream passes `*this` into
    /// `SessionRequestManager(server_manager)`.
    self_reference: Option<Weak<Mutex<ServerManager>>>,

    /// Bounded local instrumentation for diagnosing service-thread spin loops.
    /// Disabled unless `RUZU_SM_SPIN_TRACE` is set.
    loop_stats: LoopStats,
}

impl ServerManager {
    fn host_thread_session_consumption_enabled_from_flags(all: bool) -> bool {
        all
    }

    fn host_thread_session_consumption_enabled() -> bool {
        Self::host_thread_session_consumption_enabled_from_flags(
            std::env::var_os("RUZU_SERVER_THREAD_IPC_ALL").is_some(),
        )
    }

    fn host_thread_service_filter_matches(service_name: &str) -> bool {
        let Some(spec) = std::env::var_os("RUZU_SERVER_THREAD_IPC_SERVICE") else {
            return false;
        };
        spec.to_string_lossy().split(',').any(|raw| {
            let value = raw.trim();
            !value.is_empty() && (value == "*" || value == service_name)
        })
    }

    fn should_link_session_holder_for_host_thread(
        server_session: &Arc<Mutex<KServerSession>>,
        manager: &Arc<Mutex<SessionRequestManager>>,
    ) -> bool {
        if Self::host_thread_session_consumption_enabled() {
            return true;
        }

        // Per-handle host-thread routing wires this wakeup immediately before
        // queueing the exact target session for registration. Link only that
        // prepared session, not every session created while the env var exists.
        if server_session.lock().unwrap().manager_wakeup.is_some() {
            return true;
        }

        if std::env::var_os("RUZU_SERVER_THREAD_IPC_SERVICE").is_none() {
            return false;
        }

        let service_name = manager
            .lock()
            .unwrap()
            .session_handler()
            .map(|handler| handler.service_name().to_string());
        let Some(service_name) = service_name else {
            return false;
        };

        if Self::default_host_thread_service_matches(&service_name) {
            return true;
        }

        Self::host_thread_service_filter_matches(&service_name)
    }

    fn default_host_thread_service_matches(service_name: &str) -> bool {
        Self::default_host_thread_service_matches_from_flags(
            service_name,
            std::env::var_os("RUZU_INLINE_IPC").is_some(),
            std::env::var_os("RUZU_DISABLE_SERVER_THREAD_IPC_BINDER").is_some(),
        )
    }

    pub(crate) fn default_host_thread_service_matches_from_flags(
        service_name: &str,
        inline: bool,
        disable_binder: bool,
    ) -> bool {
        let _ = (service_name, inline, disable_binder);
        false
    }

    fn trace_ipc(&self, stage: &str) {
        if std::env::var_os("RUZU_TRACE_SERVER_MANAGER_IPC").is_some() {
            eprintln!("[SERVER_MANAGER_IPC] manager={} stage={stage}", self.name);
        }
    }

    fn trace_ipc_counts(&self, stage: &str) {
        if std::env::var_os("RUZU_TRACE_SERVER_MANAGER_IPC").is_some() {
            let holders = self.multi_wait.holders_snapshot();
            let signaled = holders
                .iter()
                .filter(|holder| unsafe { (*(**holder)).is_signaled() })
                .count();
            if signaled == 0 {
                return;
            }
            eprintln!(
                "[SERVER_MANAGER_IPC] manager={} stage={stage} holders={} signaled={}",
                self.name,
                holders.len(),
                signaled
            );
        }
    }

    fn boot_trace_enabled(&self) -> bool {
        std::env::var_os("RUZU_APPLET_BOOT_TRACE")
            .is_some_and(|value| value != std::ffi::OsStr::new("0"))
    }

    fn current_process(&self) -> Option<Arc<ProcessLock>> {
        let Some(current_thread) = self.system.get().current_thread() else {
            return self.system.get().current_process_arc_opt();
        };
        let thread_guard = current_thread.lock().unwrap();
        thread_guard
            .parent
            .as_ref()
            .and_then(Weak::upgrade)
            .or_else(|| self.system.get().current_process_arc_opt())
    }

    fn current_process_and_scheduler(&self) -> Option<(Arc<ProcessLock>, Arc<Mutex<KScheduler>>)> {
        let current_thread = self.system.get().current_thread()?;
        let thread_guard = current_thread.lock().unwrap();
        let process = thread_guard.parent.as_ref().and_then(Weak::upgrade)?;
        let scheduler = thread_guard
            .scheduler
            .as_ref()
            .and_then(Weak::upgrade)
            .or_else(|| {
                process
                    .lock()
                    .unwrap()
                    .scheduler
                    .as_ref()
                    .and_then(Weak::upgrade)
            })?;
        Some((process, scheduler))
    }

    fn signal_kernel_event(&self, event: &Arc<Mutex<KEvent>>) {
        let Some(process) = (!self.system.is_null())
            .then(|| self.current_process())
            .flatten()
        else {
            return;
        };
        KEvent::signal_arc(event, &process);
    }

    fn clear_kernel_event(&self, event: &Arc<Mutex<KEvent>>) {
        let Some(process) = (!self.system.is_null())
            .then(|| self.current_process())
            .flatten()
        else {
            return;
        };
        KEvent::clear_arc(event, &process);
    }

    /// Creates a new ServerManager.
    /// Port of upstream `ServerManager::ServerManager(Core::System& system)`.
    fn new(system: SystemRef) -> Self {
        let wakeup_event = Arc::new(Event::new());

        let mut wakeup_holder = Box::new(MultiWaitHolder::from_event(wakeup_event.clone()));
        wakeup_holder.set_user_data(usize::MAX); // sentinel, not a real tag

        Self {
            system,
            name: String::new(),
            selection_mutex: Arc::new(Mutex::new(())),
            ports: Vec::new(),
            sessions: Vec::new(),
            next_session_id: 1,
            wakeup_event,
            deferral_event: None,
            multi_wait: MultiWait::new(),
            deferred_list: Mutex::new(MultiWait::new()),
            deferred_sessions: Vec::new(),
            wakeup_holder: Some(wakeup_holder),
            deferral_holder: None,
            stop_requested: Arc::new(AtomicBool::new(false)),
            stopped: AtomicBool::new(false),
            loop_started: AtomicBool::new(false),
            pending_registrations: Arc::new(Mutex::new(Vec::new())),
            pending_additional_host_threads: Vec::new(),
            host_threads: Vec::new(),
            self_reference: None,
            loop_stats: LoopStats::default(),
        }
    }

    /// Creates a `ServerManager` in its final shared Rust owner before service
    /// registration.
    ///
    /// Upstream service `LoopProcess` functions construct a
    /// `std::unique_ptr<ServerManager>` before `RegisterNamedService` /
    /// `ManageNamedPort`, so callbacks can refer to a stable manager pointee
    /// while services are registered. This is the Rust counterpart for service
    /// loops; `new(...)` remains only the low-level constructor used before
    /// binding the shared owner.
    pub fn new_shared(system: SystemRef) -> Arc<Mutex<Self>> {
        let manager = Arc::new(Mutex::new(Self::new(system)));
        manager.lock().unwrap().bind_self_reference(&manager);
        manager
    }

    /// Return the system owner for service-dispatch helpers.
    ///
    /// Upstream `ServiceFrameworkBase` stores `Core::System&` directly; ruzu
    /// reaches the same owner through the `ServerManager` attached to the
    /// request context.
    pub fn system(&self) -> SystemRef {
        self.system
    }

    /// Returns a clone of the pending-registration queue Arc. Used by code
    /// that constructs a `SessionRequestManager` while it has direct access
    /// to `&self` (e.g., `on_port_event` inside the host thread).
    pub fn pending_registrations_arc(&self) -> PendingRegistrationQueue {
        Arc::clone(&self.pending_registrations)
    }

    /// Returns a clone of the wakeup_event Arc. Used for the same wiring as
    /// `pending_registrations_arc`.
    pub fn wakeup_event_arc(&self) -> Arc<Event> {
        Arc::clone(&self.wakeup_event)
    }

    pub fn stop_requested_arc(&self) -> Arc<AtomicBool> {
        Arc::clone(&self.stop_requested)
    }

    fn service_owner_weak(&self) -> Weak<Mutex<ServerManager>> {
        self.self_reference.as_ref().cloned().unwrap_or_default()
    }

    /// Drain queued session registrations (called by host thread inside
    /// `wait_and_process_impl`). External handlers can push to the queue
    /// without locking `Mutex<ServerManager>`, then this drain runs them.
    fn drain_pending_registrations(&mut self) {
        let drained: Vec<_> = {
            let mut queue = self.pending_registrations.lock().unwrap();
            std::mem::take(&mut *queue)
        };
        if !drained.is_empty() {
            self.trace_ipc("drain_pending_registrations");
        }
        for (server_session, manager) in drained {
            let _ = self.register_session(server_session, manager);
        }
    }

    fn spin_trace_enabled(&self) -> bool {
        std::env::var_os("RUZU_SM_SPIN_TRACE").is_some()
    }

    fn log_loop_stats_if_needed(&self, reason: &str) {
        if !self.spin_trace_enabled() {
            return;
        }

        let total = self.loop_stats.wakeup_hits
            + self.loop_stats.deferral_hits
            + self.loop_stats.port_hits
            + self.loop_stats.session_hits
            + self.loop_stats.idle_timeouts;
        if total == 0 || total % 10 != 0 {
            return;
        }

        log::warn!(
            "ServerManager({}): spin stats reason={} wakeup={} deferral={} port={} session={} idle={}",
            self.name,
            reason,
            self.loop_stats.wakeup_hits,
            self.loop_stats.deferral_hits,
            self.loop_stats.port_hits,
            self.loop_stats.session_hits,
            self.loop_stats.idle_timeouts
        );
    }

    pub fn bind_self_reference(&mut self, manager: &Arc<Mutex<ServerManager>>) {
        self.self_reference = Some(Arc::downgrade(manager));
        self.rebuild_wait_holder_linkage_after_move();
    }

    /// Rebuild intrusive wait-list linkage after the manager has reached its
    /// final shared owner.
    ///
    /// Upstream constructs `ServerManager` behind a stable pointee, so
    /// `MultiWaitHolder` keeps valid `MultiWait*` backlinks for its lifetime.
    /// Rust moves `ServerManager` into `Arc<Mutex<_>>` in
    /// `KernelCore::run_server(...)`, which changes the address of
    /// `m_multi_wait` / `m_deferred_list` after some ports/deferral holders may
    /// already have been linked. Clear the stale backlinks and rebuild the
    /// current lists before the event loop starts.
    fn rebuild_wait_holder_linkage_after_move(&mut self) {
        self.multi_wait.holders.clear();
        let deferred_list = self.deferred_list.get_mut().unwrap();
        deferred_list.holders.clear();

        if let Some(holder) = self.wakeup_holder.as_deref_mut() {
            holder.reset_multi_wait_linkage_for_owner_move();
        }
        if let Some(holder) = self.deferral_holder.as_deref_mut() {
            holder.reset_multi_wait_linkage_for_owner_move();
            holder.link_to_multi_wait(deferred_list as *mut MultiWait);
        }
        for port in &mut self.ports {
            port.holder.reset_multi_wait_linkage_for_owner_move();
            port.holder
                .link_to_multi_wait(deferred_list as *mut MultiWait);
        }
        for session in &mut self.sessions {
            session.holder.reset_multi_wait_linkage_for_owner_move();
            if Self::should_link_session_holder_for_host_thread(
                &session.server_session,
                &session.manager,
            ) {
                session
                    .holder
                    .link_to_multi_wait(deferred_list as *mut MultiWait);
            }
        }
    }

    /// Get the service manager from System.
    fn service_manager(&self) -> Option<Arc<Mutex<ServiceManager>>> {
        if self.system.is_null() {
            return None;
        }
        self.system.get().service_manager()
    }

    pub(crate) fn thread_name(&self) -> String {
        if self.name.is_empty() {
            "ServiceServer".to_string()
        } else {
            format!("HLE:{}", self.name)
        }
    }

    /// Registers a session with a manager.
    /// Port of upstream `ServerManager::RegisterSession`.
    pub fn register_session(
        &mut self,
        server_session: Arc<Mutex<KServerSession>>,
        manager: Arc<Mutex<SessionRequestManager>>,
    ) -> ResultCode {
        log::debug!("ServerManager({}): register_session", self.name);
        // Idempotent: skip if this server_session is already registered.
        // Drain callers (drain_pending_registrations) can push the same session
        // multiple times if `svc_ipc.rs`'s host-thread routing path enqueues
        // it on every IPC without checking whether it's already in
        // self.sessions. Without this guard, the session would appear N times
        // in `multi_wait` and `on_session_event` would be invoked N times for
        // a single IPC arrival.
        if self
            .sessions
            .iter()
            .any(|s| Arc::ptr_eq(&s.server_session, &server_session))
        {
            return RESULT_SUCCESS;
        }
        // Mirror the session into the host fiber's KProcess so the kernel-
        // backed `MultiWait::wait_any` can resolve its parent_id. Sessions are
        // typically created via `push_ipc_interface` /
        // `create_session_with_manager_object_id`, which registers the new
        // `KSession` in the GUEST process (the caller's process). When the
        // owning ServerManager's host fiber later calls `wait_any`, the
        // resolver looks the parent_id up in *its* process — the host service
        // KProcess — and gets `RESULT_INVALID_HANDLE` because the session is
        // not registered there. The result is that `wait_any` returns `None`,
        // the loop spins, and every other guest-service fiber sharing the
        // same guest core starves.
        //
        // Upstream's `MultiWait::WaitAny` works on native `KSession*`
        // pointers and is process-agnostic; the ruzu port resolves through
        // object ids per-process. The least-invasive bridge is to also
        // register the session in the current host fiber's KProcess so the
        // kernel-backed wait succeeds.
        if !self.system.is_null() {
            if let Some(parent_id) = server_session.lock().unwrap().get_parent_id() {
                if let Some(current_thread) = self.system.get().current_thread() {
                    let process = current_thread
                        .lock()
                        .unwrap()
                        .parent
                        .as_ref()
                        .and_then(|parent| parent.upgrade());
                    if let Some(process) = process {
                        let already_known = {
                            let process_guard = process.lock().unwrap();
                            process_guard
                                .get_server_session_by_object_id(parent_id)
                                .is_some()
                        };
                        if !already_known {
                            // We need the owning KSession Arc (not just the
                            // server-side). Find which process registered this
                            // session object id, then pull the KSession Arc out
                            // of that process so we can mirror it here.
                            let kernel_session = self.system.get().kernel().and_then(|kernel| {
                                kernel
                                    .get_session_owner_process_id(parent_id)
                                    .and_then(|owner_id| kernel.get_process_by_id(owner_id))
                                    .and_then(|owner_process| {
                                        owner_process
                                            .lock()
                                            .unwrap()
                                            .get_session_by_object_id(parent_id)
                                    })
                            });
                            if let Some(ksession) = kernel_session {
                                process
                                    .lock()
                                    .unwrap()
                                    .register_session_object(parent_id, ksession);
                            }
                        }
                    }
                }
            }
        }
        let session_id = self.next_session_id;
        self.next_session_id = self.next_session_id.wrapping_add(1).max(1);
        let mut session = Session::new(session_id, server_session, manager);
        // Reactive session consumption is only safe when svc_ipc.rs routes the
        // same session through the host-thread path. Non-routed sessions still
        // use the inline fallback, where linking this holder would create a
        // second receive_request_hle consumer. Upstream has a single consumer
        // (ServerManager); ruzu links only sessions whose routing policy has
        // already moved to that path.
        if Self::should_link_session_holder_for_host_thread(
            &session.server_session,
            &session.manager,
        ) {
            self.link_to_deferred_list_holder(&mut session.holder);
        }
        self.sessions.push(session);
        RESULT_SUCCESS
    }

    /// Borrow the wakeup event as a Weak so that owners of `KServerSession`
    /// can wire reactive wakeup without an Arc clone leak.
    pub fn wakeup_event_weak(&self) -> std::sync::Weak<Event> {
        Arc::downgrade(&self.wakeup_event)
    }

    /// Registers a named service with the global ServiceManager.
    /// Port of upstream `ServerManager::RegisterNamedService`.
    pub fn register_named_service(
        &mut self,
        service_name: &str,
        handler_factory: SessionRequestHandlerFactory,
        max_sessions: u32,
    ) -> ResultCode {
        if self.name.is_empty() {
            self.name = service_name.to_string();
        }

        let sm = match self.service_manager() {
            Some(sm) => sm,
            None => return RESULT_SUCCESS,
        };

        let server_manager = self.service_owner_weak();
        let (port, deferral_event) = {
            let mut sm_guard = sm.lock().unwrap();
            let result = sm_guard.register_service_with_port(
                service_name.to_string(),
                max_sessions,
                handler_factory,
            );
            let deferral_event = sm_guard.deferral_event_clone();
            let port = match result {
                Ok(port) => port,
                Err(result) => {
                    log::warn!(
                        "ServerManager({}): failed to register '{}': {:#x}",
                        self.name,
                        service_name,
                        result.get_inner_value()
                    );
                    return result;
                }
            };
            if server_manager.upgrade().is_some() {
                sm_guard.set_service_ownership(
                    service_name,
                    crate::hle::service::sm::sm::ServiceOwnership {
                        queue: self.pending_registrations_arc(),
                        wakeup: self.wakeup_event_arc(),
                        server_manager,
                    },
                );
            }
            (port, deferral_event)
        };
        if let Some(event) = deferral_event {
            self.signal_kernel_event(&event);
        }

        let (client_port_object_id, server_port_object_id) = (!self.system.is_null())
            .then(|| self.system.get().kernel())
            .flatten()
            .map(|kernel| {
                let client_object_id = kernel.create_new_object_id() as u64;
                kernel.register_kernel_object(client_object_id);
                let object_id = kernel.create_new_object_id() as u64;
                kernel.register_kernel_object(object_id);
                (Some(client_object_id), Some(object_id))
            })
            .unwrap_or((None, None));
        if let Some(client_port_object_id) = client_port_object_id {
            sm.lock()
                .unwrap()
                .set_service_port_client_port_object_id(service_name, client_port_object_id);
        }

        let sm_for_handler = Arc::clone(&sm);
        let service_name_owned = service_name.to_string();
        let handler_factory: SessionRequestHandlerFactory = Box::new(move || {
            sm_for_handler
                .lock()
                .unwrap()
                .get_service(&service_name_owned)
                .expect("registered service must resolve to a handler")
        });
        let mut server = Port::new(port, server_port_object_id, None, handler_factory);
        self.link_to_deferred_list_holder(&mut server.holder);
        self.ports.push(server);

        RESULT_SUCCESS
    }

    /// Registers a named service with a shared handler instance.
    pub fn register_named_service_handler(
        &mut self,
        service_name: &str,
        handler: SessionRequestHandlerPtr,
        max_sessions: u32,
    ) -> ResultCode {
        let handler_clone = handler.clone();
        let factory: SessionRequestHandlerFactory = Box::new(move || handler_clone.clone());
        self.register_named_service(service_name, factory, max_sessions)
    }

    /// Manages a named port (standalone, not registered with SM).
    /// Port of upstream `ServerManager::ManageNamedPort`.
    pub fn manage_named_port(
        &mut self,
        port_name: &str,
        handler_factory: SessionRequestHandlerFactory,
        max_sessions: u32,
    ) -> ResultCode {
        let Some(kernel) = (!self.system.is_null())
            .then(|| self.system.get().kernel())
            .flatten()
        else {
            return RESULT_SUCCESS;
        };

        let port = Arc::new(Mutex::new(KPort::new()));
        port.lock()
            .unwrap()
            .initialize(max_sessions as i32, false, 0);

        let client_port_object_id = kernel.create_new_object_id() as u64;
        kernel.register_kernel_object(client_port_object_id);

        if let Some(gd) = kernel.object_name_global_data() {
            if gd
                .new_from_name(client_port_object_id as usize, port_name)
                .is_err()
            {
                kernel.unregister_kernel_object(client_port_object_id);
                return crate::hle::kernel::svc::svc_results::RESULT_INVALID_STATE;
            }
        }

        let server_port_object_id = kernel.create_new_object_id() as u64;
        kernel.register_kernel_object(server_port_object_id);

        let server_manager = self.service_owner_weak();
        if server_manager.upgrade().is_some() {
            if let Some(sm) = self.service_manager() {
                sm.lock().unwrap().set_service_ownership(
                    port_name,
                    crate::hle::service::sm::sm::ServiceOwnership {
                        queue: self.pending_registrations_arc(),
                        wakeup: self.wakeup_event_arc(),
                        server_manager,
                    },
                );
            }
        }

        let mut server = Port::new(
            port,
            Some(server_port_object_id),
            Some(client_port_object_id),
            handler_factory,
        );
        self.link_to_deferred_list_holder(&mut server.holder);
        self.ports.push(server);
        RESULT_SUCCESS
    }

    pub(crate) fn ensure_kernel_port_registrations(&mut self) {
        if self.system.is_null() {
            return;
        }

        let Some(process) = self.current_process() else {
            return;
        };

        self.ensure_kernel_port_registrations_for_process(process);
    }

    pub(crate) fn ensure_kernel_port_registrations_for_process(
        &mut self,
        process: Arc<ProcessLock>,
    ) {
        let mut process = process.lock().unwrap();
        for port in &mut self.ports {
            if port.registered_in_process {
                continue;
            }
            if let Some(server_port_object_id) = port.server_port_object_id {
                process.register_server_port_object(server_port_object_id, Arc::clone(&port.port));
            }
            if let Some(client_port_object_id) = port.named_client_port_object_id {
                process.register_client_port_object(client_port_object_id, Arc::clone(&port.port));
            }
            port.registered_in_process = true;
        }
    }

    /// Manages deferral events.
    /// Port of upstream `ServerManager::ManageDeferral(KEvent**)`.
    pub fn manage_deferral(&mut self) -> (ResultCode, Option<Arc<Mutex<KEvent>>>) {
        let Some(kernel) = (!self.system.is_null())
            .then(|| self.system.get().kernel())
            .flatten()
        else {
            return (RESULT_SUCCESS, None);
        };
        let Some(process) = self.current_process() else {
            return (RESULT_SUCCESS, None);
        };

        let event_object_id = kernel.create_new_object_id() as u64;
        let readable_event_object_id = kernel.create_new_object_id() as u64;

        let event = Arc::new(Mutex::new(KEvent::new()));
        event.lock().unwrap().initialize(
            process.lock().unwrap().get_process_id(),
            readable_event_object_id,
        );

        let readable_event = Arc::new(Mutex::new(KReadableEvent::new()));
        readable_event
            .lock()
            .unwrap()
            .initialize(event_object_id, readable_event_object_id);

        {
            let mut process = process.lock().unwrap();
            process.register_event_object(event_object_id, Arc::clone(&event));
            process.register_readable_event_object(
                readable_event_object_id,
                Arc::clone(&readable_event),
            );
        }
        kernel.register_kernel_object(event_object_id);
        kernel.register_kernel_object(readable_event_object_id);

        self.deferral_event = Some(Arc::clone(&event));

        let mut holder = Box::new(MultiWaitHolder::from_readable_event(readable_event));
        holder.set_user_data(UserDataTag::DeferEvent as usize);

        self.link_to_deferred_list_holder(&mut holder);
        self.deferral_holder = Some(holder);

        (RESULT_SUCCESS, Some(event))
    }

    /// Link a holder to the deferred list and signal the wakeup event.
    /// Port of upstream `ServerManager::LinkToDeferredList`.
    fn link_to_deferred_list_holder(&self, holder: &mut MultiWaitHolder) {
        let mut deferred_list = self.deferred_list.lock().unwrap();
        holder.link_to_multi_wait(&mut *deferred_list as *mut MultiWait);
        self.signal_wakeup_event();
    }

    /// Link a holder pointer to the deferred list and wake the event loop.
    ///
    /// This mirrors upstream `LinkToDeferredList(MultiWaitHolder*)` when the
    /// caller only has a stable holder pointer and cannot keep a Rust borrow of
    /// the owning `Session`/`Port` across the deferred-list lock.
    fn link_holder_ptr_to_deferred_list(&self, holder: *mut MultiWaitHolder) {
        let mut deferred_list = self.deferred_list.lock().unwrap();
        unsafe {
            (*holder).link_to_multi_wait(&mut *deferred_list as *mut MultiWait);
        }
        self.signal_wakeup_event();
    }

    /// Move all items from the deferred list to the main multi-wait.
    /// Port of upstream `ServerManager::LinkDeferred`.
    fn link_deferred(&mut self) {
        let mut deferred_list = self.deferred_list.lock().unwrap();
        self.multi_wait.move_all(&mut deferred_list);
        drop(deferred_list);
        self.trace_ipc_counts("link_deferred");
    }

    /// Main loop for processing server events.
    /// Port of upstream `ServerManager::LoopProcess`.
    pub fn loop_process(&mut self) -> ResultCode {
        self.prepare_loop_process();

        while !self.stop_requested.load(Ordering::Relaxed) {
            self.wait_and_process_impl();
        }
        self.finish_loop_process();
        RESULT_SUCCESS
    }

    /// Runs `LoopProcess` from the shared owner without holding the
    /// `ServerManager` mutex for the lifetime of the event loop.
    pub fn loop_process_shared(manager: &Arc<Mutex<ServerManager>>) -> ResultCode {
        manager.lock().unwrap().prepare_loop_process();
        Self::activate_additional_host_threads(manager);
        let result = Self::loop_process_impl_shared(manager);
        manager.lock().unwrap().finish_loop_process();
        result
    }

    /// Port of upstream `ServerManager::LoopProcessImpl` for the shared Rust owner.
    ///
    /// Additional host threads call this directly, matching upstream
    /// `StartAdditionalHostThreads(... [&] { this->LoopProcessImpl(); })`.
    /// Only the main `LoopProcess` caller runs the prepare/finish wrapper.
    fn loop_process_impl_shared(manager: &Arc<Mutex<ServerManager>>) -> ResultCode {
        loop {
            let selected = {
                let mut guard = manager.lock().unwrap();
                if guard.stop_requested.load(Ordering::Relaxed) {
                    return RESULT_SUCCESS;
                }
                guard
                    .wait_signaled()
                    .map(|holder| guard.prepare_shared_event(holder))
            };

            match selected {
                Some(SelectedSharedEvent::Session(event)) => {
                    Self::process_session_event_shared(manager, event);
                }
                Some(SelectedSharedEvent::Locked(holder)) => {
                    let mut guard = manager.lock().unwrap();
                    guard.process(holder);
                }
                None => {}
            }
        }
    }

    fn prepare_loop_process(&mut self) {
        if self.spin_trace_enabled() && !self.system.is_null() {
            if let Some(kernel) = self.system.get().kernel() {
                log::warn!(
                    "ServerManager({}): is_guest_core={}",
                    self.name,
                    kernel.is_current_thread_guest_core()
                );
            }
        }
        self.ensure_kernel_event_bridge(&self.wakeup_event);
        self.ensure_kernel_port_registrations();

        // Link the permanent wakeup holder into `multi_wait` before entering
        // the loop. Upstream constructs the wakeup holder already bound to the
        // wait list; our split construction requires doing it here so that
        // `wait_any()` has at least one waitable even before any session or
        // port has been registered. Without this, service managers with no
        // sessions yet hit `holders.is_empty()` in `timed_wait_impl`, which
        // returns `None` immediately, and `loop_process` spins at 100% CPU
        // with zero voluntary context switches.
        if let Some(holder) = self.wakeup_holder.as_deref_mut() {
            holder.link_to_multi_wait(&mut self.multi_wait as *mut MultiWait);
        }

        log::info!("ServerManager({}): entering event loop", self.name);
        self.loop_started.store(true, Ordering::Release);
    }

    fn finish_loop_process(&mut self) {
        self.stopped.store(true, Ordering::Release);
        log::info!("ServerManager({}): event loop exited", self.name);
    }

    /// Wait for a signaled event and process it.
    /// Port of upstream `ServerManager::WaitAndProcessImpl`.
    fn wait_and_process_impl(&mut self) -> bool {
        let mut phase_last = ipc_phase_timer();
        let Some(signaled_holder) = self.wait_signaled() else {
            record_ipc_phase("server_01_wait_signaled_none", &mut phase_last);
            return false;
        };
        record_ipc_phase("server_01_wait_signaled", &mut phase_last);
        let processed = self.process(signaled_holder);
        record_ipc_phase("server_02_process_holder", &mut phase_last);
        processed
    }

    /// Port of upstream `ServerManager::WaitSignaled`.
    fn wait_signaled(&mut self) -> Option<*mut MultiWaitHolder> {
        let selection_mutex = Arc::clone(&self.selection_mutex);
        let _selection_guard = selection_mutex.lock().unwrap();

        loop {
            if std::env::var_os("RUZU_TRACE_SERVER_MANAGER_LOOP").is_some() {
                let pending = self.pending_registrations.lock().unwrap().len();
                eprintln!(
                    "[SERVER_MANAGER_LOOP] manager={} stage=iter pending_q={} wakeup_signaled={}",
                    self.name,
                    pending,
                    self.wakeup_event.is_signaled()
                );
            }

            self.ensure_kernel_port_registrations();
            self.drain_pending_registrations();
            self.link_deferred();

            if self.stop_requested.load(Ordering::Relaxed) {
                return None;
            }

            if let Some(kernel) = self.system.get().kernel() {
                let Some(selected) = self.multi_wait.wait_any(kernel) else {
                    continue;
                };
                unsafe {
                    (*selected).unlink_from_multi_wait();
                }
                if self.is_wakeup_holder(selected) {
                    self.consume_wakeup_holder(selected);
                    continue;
                }
                return Some(selected);
            }

            let signaled = self.wakeup_event.wait_timeout(Duration::from_millis(100));
            self.loop_stats.idle_timeouts += 1;
            self.log_loop_stats_if_needed("missing_kernel_idle_timeout");
            if signaled {
                continue;
            }
            return None;
        }
    }

    fn is_wakeup_holder(&self, selected: *mut MultiWaitHolder) -> bool {
        self.wakeup_holder
            .as_ref()
            .is_some_and(|holder| std::ptr::eq(&**holder as *const MultiWaitHolder, selected))
    }

    fn session_index_by_id(&self, session_id: u64) -> Option<usize> {
        self.sessions
            .iter()
            .position(|session| session.id == session_id)
    }

    /// Destroy a managed session.
    ///
    /// Port of upstream `ServerManager::DestroySession(Session*)`.
    fn destroy_session(&mut self, session_index: usize) {
        if session_index >= self.sessions.len() {
            return;
        }

        let session_id = self.sessions[session_index].id;
        self.sessions.remove(session_index);

        // Upstream stores raw `Session*` entries in `m_deferred_sessions`; a
        // destroyed session pointer must no longer be retried. Rust stores
        // stable ids, so remove matching deferred ids at the ownership boundary.
        self.deferred_sessions.retain(|&id| id != session_id);
    }

    fn consume_wakeup_holder(&mut self, selected: *mut MultiWaitHolder) {
        self.loop_stats.wakeup_hits += 1;
        self.trace_ipc("wait_signaled_wakeup");
        self.log_loop_stats_if_needed("wakeup");
        self.wakeup_event.clear();
        unsafe {
            (*selected).link_to_multi_wait(&mut self.multi_wait as *mut MultiWait);
        }
    }

    /// Port of upstream `ServerManager::Process(MultiWaitHolder*)`.
    fn process(&mut self, selected: *mut MultiWaitHolder) -> bool {
        let selected = selected as *const MultiWaitHolder;
        let user_data = unsafe { (*selected).get_user_data() };

        match user_data {
            tag if tag == UserDataTag::Session as usize => {
                let Some(session_index) = self
                    .sessions
                    .iter()
                    .position(|session| std::ptr::eq(session.holder_ptr(), selected))
                else {
                    panic!(
                        "ServerManager({}): session holder was not registered",
                        self.name
                    );
                };
                self.loop_stats.session_hits += 1;
                self.log_loop_stats_if_needed("session");
                self.on_session_event(session_index);
                true
            }
            tag if tag == UserDataTag::Port as usize => {
                let Some(port_index) = self
                    .ports
                    .iter()
                    .position(|port| std::ptr::eq(port.holder_ptr(), selected))
                else {
                    panic!(
                        "ServerManager({}): port holder was not registered",
                        self.name
                    );
                };
                self.loop_stats.port_hits += 1;
                self.log_loop_stats_if_needed("port");
                self.on_port_event(port_index);
                true
            }
            tag if tag == UserDataTag::DeferEvent as usize => {
                self.loop_stats.deferral_hits += 1;
                self.log_loop_stats_if_needed("deferral");
                self.on_deferral_event();
                true
            }
            _ => panic!(
                "ServerManager({}): unknown MultiWaitHolder user data {:#x}",
                self.name, user_data
            ),
        }
    }

    fn prepare_shared_event(&mut self, selected: *mut MultiWaitHolder) -> SelectedSharedEvent {
        let selected_const = selected as *const MultiWaitHolder;
        let user_data = unsafe { (*selected_const).get_user_data() };

        if user_data == UserDataTag::Session as usize {
            if let Some(session_index) = self
                .sessions
                .iter()
                .position(|session| std::ptr::eq(session.holder_ptr(), selected_const))
            {
                self.loop_stats.session_hits += 1;
                self.log_loop_stats_if_needed("session");
                let session = &self.sessions[session_index];
                return SelectedSharedEvent::Session(SharedSessionEvent {
                    session_id: session.id,
                    server_session: Arc::clone(&session.server_session),
                    manager: Arc::clone(&session.manager),
                    service_manager: self.service_manager(),
                    server_name: self.name.clone(),
                });
            }
        }

        SelectedSharedEvent::Locked(selected)
    }

    fn process_session_event_shared(
        manager_owner: &Arc<Mutex<ServerManager>>,
        event: SharedSessionEvent,
    ) -> bool {
        let mut phase_last = ipc_phase_timer();
        let result = event
            .server_session
            .lock()
            .unwrap()
            .receive_request_hle(Arc::clone(&event.manager));
        record_ipc_phase("server_03_receive_request_hle", &mut phase_last);

        let (mut context, _, _) = match result {
            Ok(result) => result,
            Err(result) => {
                if result
                    == crate::hle::service::ipc_helpers::RESULT_SESSION_CLOSED.get_inner_value()
                {
                    let mut owner = manager_owner.lock().unwrap();
                    if let Some(session_index) = owner.session_index_by_id(event.session_id) {
                        owner.destroy_session(session_index);
                    }
                    return true;
                }

                log::warn!(
                    "ServerManager({}): session {} receive_request_hle failed (result={:#x})",
                    event.server_name,
                    event.session_id,
                    result
                );
                assert_receive_request_hle_result(&event.server_name, event.session_id, result);
            }
        };

        record_ipc_phase("server_04_store_context", &mut phase_last);
        context.set_session_request_manager(Arc::clone(&event.manager));
        if let Some(sm) = event.service_manager {
            context.set_service_manager(sm);
        }
        context.set_is_deferred_value(false);
        record_ipc_phase("server_07_prepare_context", &mut phase_last);

        if std::env::var_os("RUZU_TRACE_HOST_THREAD_IPC").is_some() {
            let (svc, dom) = {
                let g = event.manager.lock().unwrap();
                let svc = g
                    .session_handler()
                    .map(|h| h.service_name().to_string())
                    .unwrap_or_else(|| "<none>".to_string());
                (svc, g.is_domain())
            };
            eprintln!(
                "[HOST_THREAD_IPC] dispatch manager={} service={} cmd={} dom={}",
                event.server_name,
                svc,
                context.get_command(),
                dom
            );
        }

        let service_result = hle_ipc::complete_sync_request(&event.manager, &mut context);
        record_ipc_phase("server_08_hle_complete_sync_request", &mut phase_last);

        if context.get_is_deferred() {
            log::debug!(
                "ServerManager({}): session {} deferred",
                event.server_name,
                event.session_id
            );
            let mut owner = manager_owner.lock().unwrap();
            if let Some(session_index) = owner.session_index_by_id(event.session_id) {
                owner.sessions[session_index].context = Some(context);
                owner.deferred_sessions.push(event.session_id);
            }
            record_ipc_phase("server_09_deferred", &mut phase_last);
            return true;
        }

        let reply_result =
            crate::hle::kernel::k_server_session::KServerSession::send_reply_hle_unlocked(
                &event.server_session,
            );
        record_ipc_phase("server_10_send_reply_hle", &mut phase_last);

        let mut owner = manager_owner.lock().unwrap();
        let Some(session_index) = owner.session_index_by_id(event.session_id) else {
            return true;
        };

        if reply_result == crate::hle::service::ipc_helpers::RESULT_SESSION_CLOSED.get_inner_value()
            || service_result == crate::hle::service::ipc_helpers::RESULT_SESSION_CLOSED
        {
            log::debug!(
                "ServerManager({}): session {} closed after dispatch",
                owner.name,
                event.session_id
            );
            owner.destroy_session(session_index);
            record_ipc_phase("server_11_session_closed", &mut phase_last);
            return true;
        }

        assert_eq!(
            reply_result,
            RESULT_SUCCESS.get_inner_value(),
            "ServerManager({}): unexpected SendReplyHLE result for session {}: {:#x}",
            owner.name,
            event.session_id,
            reply_result
        );
        assert_eq!(
            service_result,
            RESULT_SUCCESS,
            "ServerManager({}): unexpected service dispatch result for session {}: {:#x}",
            owner.name,
            event.session_id,
            service_result.get_inner_value()
        );

        log::trace!(
            "ServerManager({}): session {} completed (service_result={:#x}, reply_result={:#x})",
            owner.name,
            event.session_id,
            service_result.get_inner_value(),
            reply_result
        );

        let holder_ptr = {
            let holder = &mut *owner.sessions[session_index].holder as *mut MultiWaitHolder;
            unsafe {
                if (*holder).is_linked() {
                    std::ptr::null_mut()
                } else {
                    holder
                }
            }
        };
        if !holder_ptr.is_null() {
            owner.link_holder_ptr_to_deferred_list(holder_ptr);
        }
        record_ipc_phase("server_12_relink_session", &mut phase_last);
        true
    }

    pub fn signal_wakeup_event(&self) {
        self.wakeup_event.signal();
    }

    fn ensure_kernel_event_bridge(&self, event: &Arc<Event>) {
        if event.kernel_object_id().is_some() || self.system.is_null() {
            return;
        }

        let Some(current_thread) = self.system.get().current_thread() else {
            if self.boot_trace_enabled() {
                log::info!(
                    "ServerManager({}): ensure_kernel_event_bridge skipped (no current_thread)",
                    self.name
                );
            }
            return;
        };
        let (process, scheduler) = {
            let thread_guard = current_thread.lock().unwrap();
            let process = thread_guard
                .parent
                .as_ref()
                .and_then(|parent| parent.upgrade());
            let scheduler = thread_guard
                .scheduler
                .as_ref()
                .and_then(|scheduler| scheduler.upgrade())
                .or_else(|| {
                    process.as_ref().and_then(|process| {
                        process
                            .lock()
                            .unwrap()
                            .scheduler
                            .as_ref()
                            .and_then(|scheduler| scheduler.upgrade())
                    })
                });
            let (Some(process), Some(scheduler)) = (process, scheduler) else {
                if self.boot_trace_enabled() {
                    log::info!(
                        "ServerManager({}): ensure_kernel_event_bridge skipped (process/scheduler missing)",
                        self.name
                    );
                }
                return;
            };
            (process, scheduler)
        };

        let Some(kernel) = self.system.get().kernel() else {
            if self.boot_trace_enabled() {
                log::info!(
                    "ServerManager({}): ensure_kernel_event_bridge skipped (no kernel)",
                    self.name
                );
            }
            return;
        };

        let object_id = kernel.create_new_object_id() as u64;
        let mut readable_event = KReadableEvent::new();
        readable_event.initialize(0, object_id);
        let readable_event = Arc::new(Mutex::new(readable_event));

        process
            .lock()
            .unwrap()
            .register_readable_event_object(object_id, Arc::clone(&readable_event));
        kernel.register_kernel_object(object_id);
        event.attach_kernel_event(readable_event, process, scheduler);
        if self.boot_trace_enabled() {
            log::info!(
                "ServerManager({}): attached kernel bridge object_id={}",
                self.name,
                object_id
            );
        }
    }

    /// Handle a server-port event (incoming connection).
    /// Port of upstream `ServerManager::OnPortEvent`.
    fn on_port_event(&mut self, port_index: usize) {
        if port_index >= self.ports.len() {
            return;
        }

        let server_session_object_id = {
            let mut port_guard = self.ports[port_index].port.lock().unwrap();
            let Some(server_session_object_id) = port_guard.server.accept_session() else {
                let holder_ptr = self.ports[port_index].holder_ptr() as *mut MultiWaitHolder;
                self.link_holder_ptr_to_deferred_list(holder_ptr);
                return;
            };
            server_session_object_id
        };

        let Some(kernel) = (!self.system.is_null())
            .then(|| self.system.get().kernel())
            .flatten()
        else {
            return;
        };
        let Some(owner_process_id) = kernel.get_session_owner_process_id(server_session_object_id)
        else {
            return;
        };
        let Some(process_arc) = kernel.get_process_by_id(owner_process_id) else {
            return;
        };

        let server_session = {
            let process = process_arc.lock().unwrap();
            match process.get_server_session_by_object_id(server_session_object_id) {
                Some(server_session) => server_session,
                None => return,
            }
        };

        let existing_manager = server_session.lock().unwrap().get_manager().cloned();
        let manager = if let Some(manager) = existing_manager {
            manager
        } else {
            let handler = self.ports[port_index].create_handler();
            // Use the *_full constructor and pass our own pending-registration
            // queue + wakeup_event Arcs directly. We're inside `&mut self` on
            // the host thread, so we can read these fields without locking
            // anything. This makes child SessionRequestManagers propagate the
            // queue down through push_ipc_interface / clone / sm::GetService.
            let queue = self.pending_registrations_arc();
            let wakeup = self.wakeup_event_arc();
            let manager = self
                .self_reference
                .as_ref()
                .and_then(Weak::upgrade)
                .map(|sm_arc| {
                    SessionRequestManager::new_with_server_manager_full(sm_arc, queue, wakeup)
                })
                .map(|manager| Arc::new(Mutex::new(manager)))
                .unwrap_or_else(|| Arc::new(Mutex::new(SessionRequestManager::new())));
            manager.lock().unwrap().set_session_handler(handler);
            server_session.lock().unwrap().set_manager(manager.clone());
            manager
        };
        let _ = self.register_session(server_session, manager);

        let holder_ptr = self.ports[port_index].holder_ptr() as *mut MultiWaitHolder;
        self.link_holder_ptr_to_deferred_list(holder_ptr);
    }

    /// Handle a session event (incoming IPC request).
    /// Port of upstream `ServerManager::OnSessionEvent`.
    fn on_session_event(&mut self, session_index: usize) {
        let mut phase_last = ipc_phase_timer();
        self.trace_ipc("on_session_event");
        let manager = self.sessions[session_index].manager.clone();
        let result = self.sessions[session_index]
            .server_session
            .lock()
            .unwrap()
            .receive_request_hle(manager);
        record_ipc_phase("server_03_receive_request_hle", &mut phase_last);

        match result {
            Ok((context, _, _request_message_address)) => {
                self.sessions[session_index].context = Some(context);
                record_ipc_phase("server_04_store_context", &mut phase_last);
            }
            Err(result) => {
                if result
                    == crate::hle::service::ipc_helpers::RESULT_SESSION_CLOSED.get_inner_value()
                {
                    log::debug!(
                        "ServerManager({}): session {} closed (result={}), removing",
                        self.name,
                        session_index,
                        result
                    );
                    self.destroy_session(session_index);
                    return;
                }

                log::warn!(
                    "ServerManager({}): session {} receive_request_hle failed (result={:#x})",
                    self.name,
                    session_index,
                    result
                );
                let session_id = self.sessions[session_index].id;
                assert_receive_request_hle_result(&self.name, session_id, result);
            }
        }

        self.complete_sync_request(session_index);
        record_ipc_phase("server_05_complete_sync_request", &mut phase_last);
    }

    /// Handle a deferral event — retry deferred sessions.
    /// Port of upstream `ServerManager::OnDeferralEvent`.
    fn on_deferral_event(&mut self) {
        if let Some(ref event) = self.deferral_event {
            self.clear_kernel_event(event);
        }

        let deferred = std::mem::take(&mut self.deferred_sessions);
        log::debug!(
            "ServerManager({}): retrying {} deferred sessions",
            self.name,
            deferred.len()
        );
        let deferral_holder_ptr = self
            .deferral_holder
            .as_deref_mut()
            .map(|holder| holder as *mut MultiWaitHolder);
        if let Some(deferral_holder_ptr) = deferral_holder_ptr {
            self.link_holder_ptr_to_deferred_list(deferral_holder_ptr);
        }
        for session_id in deferred {
            if let Some(session_index) = self.session_index_by_id(session_id) {
                self.complete_sync_request(session_index);
            }
        }
    }

    /// Complete a sync request, handling deferral.
    /// Port of upstream `ServerManager::CompleteSyncRequest`.
    ///
    /// Upstream flow:
    /// 1. `session->GetContext()->SetIsDeferred(false)`
    /// 2. `session->GetManager()->CompleteSyncRequest(server_session, context)`
    /// 3. If deferred: add to deferred_sessions, return
    /// 4. Else: `server_session->SendReplyHLE()`, re-link session
    ///
    /// Binder and explicit host-thread IPC routes reach this method through
    /// the ServerManager event loop. Non-routed services still use the
    /// temporary inline fallback in `svc_ipc.rs` until they can be promoted
    /// without creating a second request consumer.
    fn complete_sync_request(&mut self, session_index: usize) {
        let mut phase_last = ipc_phase_timer();
        self.trace_ipc("complete_sync_request");
        if session_index >= self.sessions.len() {
            return;
        }

        let manager = self.sessions[session_index].manager.clone();
        let Some(mut context) = self.sessions[session_index].context.take() else {
            log::warn!(
                "ServerManager({}): session {} missing HLE request context",
                self.name,
                session_index
            );
            return;
        };
        record_ipc_phase("server_06_take_context", &mut phase_last);

        context.set_session_request_manager(manager.clone());
        if let Some(sm) = self.service_manager() {
            context.set_service_manager(sm);
        }
        context.set_is_deferred_value(false);
        record_ipc_phase("server_07_prepare_context", &mut phase_last);

        if std::env::var_os("RUZU_TRACE_HOST_THREAD_IPC").is_some() {
            let (svc, dom) = {
                let g = manager.lock().unwrap();
                let svc = g
                    .session_handler()
                    .map(|h| h.service_name().to_string())
                    .unwrap_or_else(|| "<none>".to_string());
                (svc, g.is_domain())
            };
            eprintln!(
                "[HOST_THREAD_IPC] dispatch manager={} service={} cmd={} dom={}",
                self.name,
                svc,
                context.get_command(),
                dom
            );
        }

        // Upstream-faithful: the host service thread keeps its own dummy
        // KThread as CURRENT_THREAD throughout handler dispatch. The IPC
        // client thread is reachable explicitly via `context.get_thread()`
        // for handlers that need it; the client's process memory is
        // already wired into `context.memory` at construction (see
        // `HLERequestContext::new_with_thread → owner_process_memory`).
        //
        // Earlier ports used a `ClientThreadImpersonationGuard` to swap
        // CURRENT_THREAD to the client for the duration of the handler.
        // That breaks every scheduler invariant tied to dispatch-count
        // accounting: the client guest thread arrives with `count == 1`
        // from the guest scheduler that issued the SVC, and the host
        // thread's scheduler-lock cycle (`disable_scheduling` →
        // `enable_scheduling` → `reschedule_current_hle_thread`)
        // increments to `count == 2`, tripping the `count == 1` assertion
        // in `KScheduler::reschedule_current_hle_thread`. Audit of every
        // `system.current_thread()` / `system.current_process()` reader
        // under `core/src/hle/service/` shows no handler-side code that
        // actually depends on impersonation (only diagnostic logging in
        // `audio/audio_renderer.rs`), so removal is safe.
        let service_result = hle_ipc::complete_sync_request(&manager, &mut context);
        record_ipc_phase("server_08_hle_complete_sync_request", &mut phase_last);

        // Check if the request was deferred.
        if context.get_is_deferred() {
            log::debug!(
                "ServerManager({}): session {} deferred",
                self.name,
                session_index
            );
            self.sessions[session_index].context = Some(context);
            self.deferred_sessions.push(self.sessions[session_index].id);
            record_ipc_phase("server_09_deferred", &mut phase_last);
            return;
        }

        // Write-back is performed inside ServiceFrameworkBase::handle_sync_request_impl
        // (or explicitly in the CloseVirtualHandle / StubSuccess branches), matching
        // upstream server_manager.cpp:385 which only calls SendReplyHLE here.

        let reply_result =
            crate::hle::kernel::k_server_session::KServerSession::send_reply_hle_unlocked(
                &self.sessions[session_index].server_session,
            );
        self.trace_ipc("send_reply_done");
        record_ipc_phase("server_10_send_reply_hle", &mut phase_last);

        // Check for session close.
        if reply_result == crate::hle::service::ipc_helpers::RESULT_SESSION_CLOSED.get_inner_value()
            || service_result == crate::hle::service::ipc_helpers::RESULT_SESSION_CLOSED
        {
            log::debug!(
                "ServerManager({}): session {} closed after dispatch",
                self.name,
                session_index
            );
            self.destroy_session(session_index);
            record_ipc_phase("server_11_session_closed", &mut phase_last);
            return;
        }

        assert_eq!(
            reply_result,
            RESULT_SUCCESS.get_inner_value(),
            "ServerManager({}): unexpected SendReplyHLE result for session {}: {:#x}",
            self.name,
            session_index,
            reply_result
        );
        assert_eq!(
            service_result,
            RESULT_SUCCESS,
            "ServerManager({}): unexpected service dispatch result for session {}: {:#x}",
            self.name,
            session_index,
            service_result.get_inner_value()
        );

        log::trace!(
            "ServerManager({}): session {} completed (service_result={:#x}, reply_result={:#x})",
            self.name,
            session_index,
            service_result.get_inner_value(),
            reply_result
        );

        if session_index < self.sessions.len() {
            let holder_ptr = {
                let holder = &mut *self.sessions[session_index].holder as *mut MultiWaitHolder;
                unsafe {
                    if (*holder).is_linked() {
                        std::ptr::null_mut()
                    } else {
                        holder
                    }
                }
            };
            if !holder_ptr.is_null() {
                self.link_holder_ptr_to_deferred_list(holder_ptr);
            }
        }
        record_ipc_phase("server_12_relink_session", &mut phase_last);
    }

    /// Starts additional host threads for processing.
    /// Port of upstream `ServerManager::StartAdditionalHostThreads`.
    pub fn start_additional_host_threads(&mut self, name: &str, num_threads: usize) {
        log::debug!(
            "ServerManager({}): start_additional_host_threads({}, {})",
            self.name,
            name,
            num_threads
        );
        self.pending_additional_host_threads
            .push((name.to_string(), num_threads));
    }

    /// Activate pending additional host threads once the manager has a shared owner.
    ///
    /// Port of upstream `StartAdditionalHostThreads`: each requested thread
    /// runs the same manager's `LoopProcessImpl`.
    pub fn activate_additional_host_threads(manager: &Arc<Mutex<ServerManager>>) {
        let (system, pending) = {
            let mut guard = manager.lock().unwrap();
            let pending = std::mem::take(&mut guard.pending_additional_host_threads);
            (guard.system, pending)
        };

        if system.is_null() {
            return;
        }

        let Some(kernel) = system.get().kernel() else {
            return;
        };

        for (name, num_threads) in pending {
            for i in 0..num_threads {
                let thread_name = format!("{}:{}", name, i + 1);
                let manager_for_thread = Arc::clone(manager);
                let handle = kernel.run_on_host_core_thread(
                    &thread_name,
                    Box::new(move || {
                        let _ = ServerManager::loop_process_impl_shared(&manager_for_thread);
                    }),
                );
                manager.lock().unwrap().host_threads.push(handle);
            }
        }
    }

    /// Request the server to stop.
    pub fn request_stop(&self) {
        self.stop_requested.store(true, Ordering::Release);
        for handle in &self.host_threads {
            handle.thread().unpark();
        }
        self.signal_wakeup_event();
    }

    pub fn loop_started(&self) -> bool {
        self.loop_started.load(Ordering::Acquire)
    }

    pub fn is_stopped(&self) -> bool {
        self.stopped.load(Ordering::Acquire)
    }

    pub fn join_host_threads(&mut self) {
        for handle in self.host_threads.drain(..) {
            if let Err(err) = handle.join() {
                log::warn!(
                    "ServerManager({}): additional host thread join failed: {:?}",
                    self.name,
                    err
                );
            }
        }
    }

    #[cfg(test)]
    pub(crate) fn stop_requested_for_test(&self) -> bool {
        self.stop_requested.load(Ordering::Acquire)
    }

    /// Runs a manager that was already constructed in its final shared owner.
    ///
    /// This lets migrated service loops register services after
    /// `bind_self_reference(...)`, matching upstream's stable manager pointee
    /// during service registration.
    pub fn run_server_shared(manager: Arc<Mutex<ServerManager>>) {
        let system_ref = {
            let guard = manager.lock().unwrap();
            if guard.system.is_null() {
                log::warn!("ServerManager::run_server_shared called with null system");
                return;
            }
            guard.system
        };
        system_ref.get().run_server_shared(manager);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestSessionHandler;

    impl crate::hle::service::hle_ipc::SessionRequestHandler for TestSessionHandler {
        fn handle_sync_request(&self, _context: &mut HLERequestContext) -> ResultCode {
            RESULT_SUCCESS
        }
    }

    struct FailingSessionHandler;

    impl crate::hle::service::hle_ipc::SessionRequestHandler for FailingSessionHandler {
        fn handle_sync_request(&self, _context: &mut HLERequestContext) -> ResultCode {
            crate::hle::kernel::svc::svc_results::RESULT_INVALID_STATE
        }
    }

    struct ClearCurrentRequestHandler {
        server_session: Arc<Mutex<KServerSession>>,
    }

    impl crate::hle::service::hle_ipc::SessionRequestHandler for ClearCurrentRequestHandler {
        fn handle_sync_request(&self, _context: &mut HLERequestContext) -> ResultCode {
            self.server_session.lock().unwrap().current_request = None;
            RESULT_SUCCESS
        }
    }

    #[test]
    fn request_stop_sets_flag_and_signals_wakeup_event() {
        let manager = ServerManager::new(SystemRef::null());

        assert!(!manager.stop_requested_for_test());
        assert!(!manager.wakeup_event.is_signaled());
        manager.request_stop();

        assert!(manager.stop_requested_for_test());
        assert!(manager.wakeup_event.is_signaled());
    }

    #[test]
    fn start_additional_host_threads_records_pending_threads() {
        let mut manager = ServerManager::new(SystemRef::null());

        manager.start_additional_host_threads("bsdsocket", 2);

        assert_eq!(
            manager.pending_additional_host_threads,
            vec![("bsdsocket".to_string(), 2)]
        );
    }

    #[test]
    fn host_thread_session_consumption_requires_explicit_gate() {
        assert!(!ServerManager::host_thread_session_consumption_enabled_from_flags(false));
        assert!(ServerManager::host_thread_session_consumption_enabled_from_flags(true));
    }

    #[test]
    fn default_host_thread_session_services_are_not_promoted_implicitly() {
        for service_name in [
            "IHOSBinderDriver",
            "pl:s",
            "pl:u",
            "audren:u",
            "IAudioRenderer",
            "IAudioDevice",
            "vi:m",
            "vi:s",
            "vi:u",
            "IApplicationDisplayService",
            "vi::IManagerDisplayService",
            "vi::ISystemDisplayService",
            "apm",
            "apm:am",
            "apm:sys",
            "set",
            "set:cal",
            "set:fd",
            "set:sys",
            "lm",
            "aoc:u",
            "time:u",
            "time:a",
            "time:r",
            "time:s",
            "time:m",
            "time:su",
            "time:al",
            "pctl",
            "pctl:a",
            "pctl:r",
            "pctl:s",
            "prepo:a",
            "prepo:a2",
            "prepo:m",
            "prepo:s",
            "prepo:u",
            "friend:a",
            "friend:m",
            "friend:s",
            "friend:u",
            "friend:v",
        ] {
            assert!(
                !ServerManager::default_host_thread_service_matches_from_flags(
                    service_name,
                    false,
                    false
                ),
                "{service_name}"
            );
            assert!(
                !ServerManager::default_host_thread_service_matches_from_flags(
                    service_name,
                    false,
                    true
                ),
                "{service_name}"
            );
        }
    }

    #[test]
    fn server_manager_holders_use_upstream_process_tags() {
        let server_session = Arc::new(Mutex::new(KServerSession::new()));
        server_session.lock().unwrap().initialize(0x1000);
        let request_manager = Arc::new(Mutex::new(SessionRequestManager::new()));
        let session = Session::new(1, server_session, request_manager);
        assert_eq!(
            session.holder.get_user_data(),
            UserDataTag::Session as usize
        );

        let mut port = KPort::new();
        port.initialize(1, false, 0);
        let port = Port::new(
            Arc::new(Mutex::new(port)),
            Some(0x2000),
            Some(0x2001),
            Box::new(|| Arc::new(TestSessionHandler)),
        );
        assert_eq!(port.holder.get_user_data(), UserDataTag::Port as usize);
    }

    #[test]
    #[should_panic(expected = "unknown MultiWaitHolder user data")]
    fn process_panics_on_unknown_wait_holder_tag() {
        let mut manager = ServerManager::new(SystemRef::null());
        let event = Arc::new(Event::new());
        let mut holder = MultiWaitHolder::from_event(event);
        holder.set_user_data(0xdead);

        manager.process(&mut holder as *mut MultiWaitHolder);
    }

    #[test]
    #[should_panic(expected = "session holder was not registered")]
    fn process_panics_on_unregistered_session_holder() {
        let mut manager = ServerManager::new(SystemRef::null());
        let event = Arc::new(Event::new());
        let mut holder = MultiWaitHolder::from_event(event);
        holder.set_user_data(UserDataTag::Session as usize);

        manager.process(&mut holder as *mut MultiWaitHolder);
    }

    #[test]
    fn register_session_keeps_default_inline_session_holder_unlinked() {
        let mut manager = ServerManager::new(SystemRef::null());
        let server_session = Arc::new(Mutex::new(KServerSession::new()));
        server_session.lock().unwrap().initialize(0x1000);
        let request_manager = Arc::new(Mutex::new(SessionRequestManager::new()));

        assert_eq!(
            manager.register_session(Arc::clone(&server_session), request_manager),
            RESULT_SUCCESS
        );

        assert_eq!(manager.sessions.len(), 1);
        assert!(!manager.sessions[0].holder.is_linked());
    }

    #[test]
    fn deferred_session_ids_survive_session_vector_removal() {
        let mut manager = ServerManager::new(SystemRef::null());
        for object_id in [0x1000, 0x1001] {
            let server_session = Arc::new(Mutex::new(KServerSession::new()));
            server_session.lock().unwrap().initialize(object_id);
            let request_manager = Arc::new(Mutex::new(SessionRequestManager::new()));
            assert_eq!(
                manager.register_session(server_session, request_manager),
                RESULT_SUCCESS
            );
        }

        let deferred_id = manager.sessions[1].id;
        manager.deferred_sessions.push(deferred_id);
        manager.destroy_session(0);

        assert_eq!(manager.session_index_by_id(deferred_id), Some(0));
        assert_eq!(manager.deferred_sessions, vec![deferred_id]);
    }

    #[test]
    fn destroy_session_removes_matching_deferred_id() {
        let mut manager = ServerManager::new(SystemRef::null());
        let server_session = Arc::new(Mutex::new(KServerSession::new()));
        server_session.lock().unwrap().initialize(0x1000);
        let request_manager = Arc::new(Mutex::new(SessionRequestManager::new()));
        assert_eq!(
            manager.register_session(server_session, request_manager),
            RESULT_SUCCESS
        );

        let destroyed_id = manager.sessions[0].id;
        manager.deferred_sessions.push(destroyed_id);
        assert!(!manager.wakeup_event.is_signaled());
        manager.destroy_session(0);

        assert!(manager.sessions.is_empty());
        assert!(manager.deferred_sessions.is_empty());
        assert!(!manager.wakeup_event.is_signaled());
    }

    #[test]
    #[should_panic(expected = "unexpected service dispatch result")]
    fn complete_sync_request_asserts_unexpected_service_error() {
        let mut manager = ServerManager::new(SystemRef::null());
        let server_session = Arc::new(Mutex::new(KServerSession::new()));
        server_session.lock().unwrap().initialize(0x1000);
        let request_manager = Arc::new(Mutex::new(SessionRequestManager::new()));
        request_manager
            .lock()
            .unwrap()
            .set_session_handler(Arc::new(FailingSessionHandler));
        assert_eq!(
            manager.register_session(Arc::clone(&server_session), request_manager),
            RESULT_SUCCESS
        );

        let client_thread = Arc::new(crate::hle::kernel::k_thread::KThreadLock::new(
            crate::hle::kernel::k_thread::KThread::new(),
        ));
        let mut request = crate::hle::kernel::k_session_request::KSessionRequest::new();
        request.thread = Some(Arc::downgrade(&client_thread));
        request.thread_id = Some(1);
        server_session.lock().unwrap().current_request = Some(Arc::new(Mutex::new(request)));
        manager.sessions[0].context = Some(HLERequestContext::new());

        manager.complete_sync_request(0);
    }

    #[test]
    #[should_panic(expected = "unexpected ReceiveRequestHLE result")]
    fn on_session_event_asserts_unexpected_receive_error() {
        let mut manager = ServerManager::new(SystemRef::null());
        let server_session = Arc::new(Mutex::new(KServerSession::new()));
        server_session.lock().unwrap().initialize(0x1000);
        let request_manager = Arc::new(Mutex::new(SessionRequestManager::new()));
        assert_eq!(
            manager.register_session(server_session, request_manager),
            RESULT_SUCCESS
        );

        manager.on_session_event(0);
    }

    #[test]
    #[should_panic(expected = "unexpected ReceiveRequestHLE result")]
    fn shared_session_event_asserts_unexpected_receive_error() {
        let manager = Arc::new(Mutex::new(ServerManager::new(SystemRef::null())));
        let server_session = Arc::new(Mutex::new(KServerSession::new()));
        server_session.lock().unwrap().initialize(0x1000);
        let request_manager = Arc::new(Mutex::new(SessionRequestManager::new()));
        {
            let mut guard = manager.lock().unwrap();
            assert_eq!(
                guard.register_session(Arc::clone(&server_session), Arc::clone(&request_manager)),
                RESULT_SUCCESS
            );
        }

        ServerManager::process_session_event_shared(
            &manager,
            SharedSessionEvent {
                session_id: 1,
                server_session,
                manager: request_manager,
                service_manager: None,
                server_name: "test".to_string(),
            },
        );
    }

    #[test]
    #[should_panic(expected = "unexpected service dispatch result")]
    fn shared_session_event_asserts_unexpected_service_error() {
        let manager = Arc::new(Mutex::new(ServerManager::new(SystemRef::null())));
        let server_session = Arc::new(Mutex::new(KServerSession::new()));
        server_session.lock().unwrap().initialize(0x1000);
        let request_manager = Arc::new(Mutex::new(SessionRequestManager::new()));
        request_manager
            .lock()
            .unwrap()
            .set_session_handler(Arc::new(FailingSessionHandler));
        {
            let mut guard = manager.lock().unwrap();
            assert_eq!(
                guard.register_session(Arc::clone(&server_session), Arc::clone(&request_manager)),
                RESULT_SUCCESS
            );
        }

        let client_thread = Arc::new(crate::hle::kernel::k_thread::KThreadLock::new(
            crate::hle::kernel::k_thread::KThread::new(),
        ));
        let mut request = crate::hle::kernel::k_session_request::KSessionRequest::new();
        request.thread = Some(Arc::downgrade(&client_thread));
        request.thread_id = Some(1);
        {
            server_session
                .lock()
                .unwrap()
                .request_list
                .push_back(Arc::new(Mutex::new(request)));
        }

        ServerManager::process_session_event_shared(
            &manager,
            SharedSessionEvent {
                session_id: 1,
                server_session,
                manager: request_manager,
                service_manager: None,
                server_name: "test".to_string(),
            },
        );
    }

    #[test]
    #[should_panic(expected = "unexpected SendReplyHLE result")]
    fn shared_session_event_asserts_unexpected_reply_error() {
        let manager = Arc::new(Mutex::new(ServerManager::new(SystemRef::null())));
        let server_session = Arc::new(Mutex::new(KServerSession::new()));
        server_session.lock().unwrap().initialize(0x1000);
        let request_manager = Arc::new(Mutex::new(SessionRequestManager::new()));
        request_manager
            .lock()
            .unwrap()
            .set_session_handler(Arc::new(ClearCurrentRequestHandler {
                server_session: Arc::clone(&server_session),
            }));
        {
            let mut guard = manager.lock().unwrap();
            assert_eq!(
                guard.register_session(Arc::clone(&server_session), Arc::clone(&request_manager)),
                RESULT_SUCCESS
            );
        }

        let client_thread = Arc::new(crate::hle::kernel::k_thread::KThreadLock::new(
            crate::hle::kernel::k_thread::KThread::new(),
        ));
        let mut request = crate::hle::kernel::k_session_request::KSessionRequest::new();
        request.thread = Some(Arc::downgrade(&client_thread));
        request.thread_id = Some(1);
        {
            server_session
                .lock()
                .unwrap()
                .request_list
                .push_back(Arc::new(Mutex::new(request)));
        }

        ServerManager::process_session_event_shared(
            &manager,
            SharedSessionEvent {
                session_id: 1,
                server_session,
                manager: request_manager,
                service_manager: None,
                server_name: "test".to_string(),
            },
        );
    }

    #[test]
    fn wait_holder_rebuild_keeps_default_inline_session_holder_unlinked() {
        let mut manager = ServerManager::new(SystemRef::null());
        let server_session = Arc::new(Mutex::new(KServerSession::new()));
        server_session.lock().unwrap().initialize(0x1000);
        let request_manager = Arc::new(Mutex::new(SessionRequestManager::new()));

        assert_eq!(
            manager.register_session(Arc::clone(&server_session), request_manager),
            RESULT_SUCCESS
        );
        manager.rebuild_wait_holder_linkage_after_move();

        assert_eq!(manager.sessions.len(), 1);
        assert!(!manager.sessions[0].holder.is_linked());
    }

    #[test]
    fn service_owner_weak_is_live_after_manager_is_bound() {
        let manager = Arc::new(Mutex::new(ServerManager::new(SystemRef::null())));
        manager.lock().unwrap().bind_self_reference(&manager);

        let owner = manager.lock().unwrap().service_owner_weak();

        assert!(owner.upgrade().is_some());
    }

    #[test]
    fn new_shared_binds_service_owner_before_registration() {
        let manager = ServerManager::new_shared(SystemRef::null());

        let owner = manager.lock().unwrap().service_owner_weak();

        let upgraded = owner
            .upgrade()
            .expect("new_shared should bind a live ServerManager owner");
        assert!(Arc::ptr_eq(&upgraded, &manager));
    }
}
