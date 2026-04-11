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
//! IPC dispatch in ruzu currently flows through svc_ipc::send_sync_request()
//! → hle_ipc::complete_sync_request() synchronously. The ServerManager event
//! loop handles session lifecycle, port management, and deferred requests.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, Weak};
use std::thread::JoinHandle;
use std::time::Duration;

use crate::core::SystemRef;
use crate::hle::kernel::k_port::KPort;
use crate::hle::kernel::k_process::KProcess;
use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::kernel::k_scheduler::KScheduler;
use crate::hle::kernel::k_server_session::KServerSession;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{
    self, HLERequestContext, SessionRequestHandlerFactory, SessionRequestHandlerPtr,
    SessionRequestManager,
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
    holder: Box<MultiWaitHolder>,
    server_session: Arc<Mutex<KServerSession>>,
    manager: Arc<Mutex<SessionRequestManager>>,
    /// Stored context for in-flight requests.
    /// Upstream: `HLERequestContext context` stored per-session.
    context: Option<HLERequestContext>,
}

impl Session {
    fn new(
        server_session: Arc<Mutex<KServerSession>>,
        manager: Arc<Mutex<SessionRequestManager>>,
    ) -> Self {
        let mut holder = Box::new(MultiWaitHolder::from_server_session(server_session.clone()));
        holder.set_user_data(UserDataTag::Session as usize);
        Self {
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

    /// Active managed server ports.
    ports: Vec<Port>,

    /// Active sessions.
    sessions: Vec<Session>,

    /// Wakeup event — signaled to wake the event loop when new items are linked.
    /// Upstream: `Kernel::KEvent* m_wakeup_event`.
    wakeup_event: Arc<Event>,

    /// Deferral event — signaled when deferred requests should be retried.
    /// Upstream: `Kernel::KEvent* m_deferral_event`.
    deferral_event: Option<Arc<Event>>,

    /// The main multi-wait for the event loop.
    /// Upstream: `MultiWait m_multi_wait`.
    multi_wait: MultiWait,

    /// Deferred list — items to be linked into multi_wait on next iteration.
    /// Upstream: `MultiWait m_deferred_list` + `std::mutex m_deferred_list_mutex`.
    deferred_list: Mutex<MultiWait>,

    /// Deferred sessions awaiting retry.
    /// Upstream: `std::list<Session*> m_deferred_sessions`.
    deferred_sessions: Vec<usize>,

    /// Wakeup holder in the multi-wait.
    /// Upstream: `std::optional<MultiWaitHolder> m_wakeup_holder`.
    wakeup_holder: Option<Box<MultiWaitHolder>>,

    /// Deferral holder in the multi-wait.
    /// Upstream: `std::optional<MultiWaitHolder> m_deferral_holder`.
    deferral_holder: Option<Box<MultiWaitHolder>>,

    /// Stop flag. Upstream: `std::stop_source m_stop_source`.
    stop_requested: AtomicBool,

    /// Whether the server has been stopped.
    stopped: AtomicBool,

    /// Additional host threads requested by upstream call sites such as
    /// `Sockets::LoopProcess`.
    ///
    /// Upstream: `std::vector<std::jthread> m_threads` is populated by
    /// `StartAdditionalHostThreads(...)`. Rust records the requested threads
    /// here first, then activates them once the `ServerManager` has been moved
    /// into its shared owner in `KernelCore::run_server(...)`.
    pending_additional_host_threads: Vec<(String, usize)>,

    /// Owned handles for additional host threads.
    /// Upstream: `m_threads`.
    host_threads: Vec<JoinHandle<()>>,

    /// Shared stop flag for additional host threads.
    /// This is a bounded Rust adaptation of upstream `std::stop_source`.
    additional_host_thread_stop: Arc<AtomicBool>,

    /// Shared self-owner used where upstream passes `*this` into
    /// `SessionRequestManager(server_manager)`.
    self_reference: Option<Weak<Mutex<ServerManager>>>,
}

impl ServerManager {
    /// Creates a new ServerManager.
    /// Port of upstream `ServerManager::ServerManager(Core::System& system)`.
    pub fn new(system: SystemRef) -> Self {
        let wakeup_event = Arc::new(Event::new());

        let mut wakeup_holder = Box::new(MultiWaitHolder::from_event(wakeup_event.clone()));
        wakeup_holder.set_user_data(usize::MAX); // sentinel, not a real tag

        Self {
            system,
            name: String::new(),
            ports: Vec::new(),
            sessions: Vec::new(),
            wakeup_event,
            deferral_event: None,
            multi_wait: MultiWait::new(),
            deferred_list: Mutex::new(MultiWait::new()),
            deferred_sessions: Vec::new(),
            wakeup_holder: Some(wakeup_holder),
            deferral_holder: None,
            stop_requested: AtomicBool::new(false),
            stopped: AtomicBool::new(false),
            pending_additional_host_threads: Vec::new(),
            host_threads: Vec::new(),
            additional_host_thread_stop: Arc::new(AtomicBool::new(false)),
            self_reference: None,
        }
    }

    pub fn bind_self_reference(&mut self, manager: &Arc<Mutex<ServerManager>>) {
        self.self_reference = Some(Arc::downgrade(manager));
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
        let mut session = Session::new(server_session, manager);
        self.link_to_deferred_list_holder(&mut session.holder);
        self.sessions.push(session);
        RESULT_SUCCESS
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

        let port = match sm.lock().unwrap().register_service_with_port(
            service_name.to_string(),
            max_sessions,
            handler_factory,
        ) {
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

        let server_port_object_id = (!self.system.is_null())
            .then(|| self.system.get().kernel())
            .flatten()
            .map(|kernel| {
                let object_id = kernel.create_new_object_id() as u64;
                kernel.register_kernel_object(object_id);
                object_id
            });

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

    fn ensure_kernel_port_registrations(&mut self) {
        if self.system.is_null() {
            return;
        }

        let Some(current_thread) = self.system.get().current_thread() else {
            return;
        };
        let Some(process) = current_thread
            .lock()
            .unwrap()
            .parent
            .as_ref()
            .and_then(|parent| parent.upgrade())
        else {
            return;
        };

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
    pub fn manage_deferral(&mut self) -> (ResultCode, Option<Arc<Event>>) {
        let event = Arc::new(Event::new());
        self.deferral_event = Some(event.clone());

        let mut holder = Box::new(MultiWaitHolder::from_event(event.clone()));
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
    }

    /// Main loop for processing server events.
    /// Port of upstream `ServerManager::LoopProcess`.
    pub fn loop_process(&mut self) -> ResultCode {
        self.ensure_kernel_event_bridge(&self.wakeup_event);
        if let Some(event) = self.deferral_event.as_ref() {
            self.ensure_kernel_event_bridge(event);
        }
        self.ensure_kernel_port_registrations();
        log::info!("ServerManager({}): entering event loop", self.name);
        while !self.stop_requested.load(Ordering::Relaxed) {
            self.wait_and_process_impl();
        }
        self.stopped.store(true, Ordering::Release);
        log::info!("ServerManager({}): event loop exited", self.name);
        RESULT_SUCCESS
    }

    /// Wait for a signaled event and process it.
    /// Port of upstream `ServerManager::WaitAndProcessImpl`.
    fn wait_and_process_impl(&mut self) -> bool {
        self.ensure_kernel_port_registrations();
        self.link_deferred();
        if self.stop_requested.load(Ordering::Relaxed) {
            return false;
        }

        if !self.system.is_null() {
            if let Some(kernel) = self.system.get().kernel() {
                if let Some(selected) = self.multi_wait.wait_any(kernel) {
                    unsafe {
                        (*selected).unlink_from_multi_wait();
                    }
                    return self.process_holder(selected);
                }
                return false;
            }
        }

        // No current guest thread / null system (tests and local harnesses).
        // Keep the historical fallback here instead of blocking on guest wait
        // primitives when there is no real kernel-backed service thread to
        // suspend.
        if !self.system.is_null() {
            if let Some(kernel) = self.system.get().kernel() {
                if let Some(selected) = self.multi_wait.try_wait_any(kernel) {
                    unsafe {
                        (*selected).unlink_from_multi_wait();
                    }
                    return self.process_holder(selected);
                }
            }
        }
        std::thread::yield_now();
        false
    }

    fn process_holder(&mut self, selected: *mut MultiWaitHolder) -> bool {
        let selected = selected as *const MultiWaitHolder;

        if self
            .wakeup_holder
            .as_ref()
            .is_some_and(|holder| std::ptr::eq(&**holder as *const MultiWaitHolder, selected))
        {
            self.wakeup_event.clear();
            unsafe {
                (*selected.cast_mut()).link_to_multi_wait(&mut self.multi_wait as *mut MultiWait);
            }
            return true;
        }

        if self
            .deferral_holder
            .as_ref()
            .is_some_and(|holder| std::ptr::eq(&**holder as *const MultiWaitHolder, selected))
        {
            if let Some(ref event) = self.deferral_event {
                event.clear();
            }
            self.on_deferral_event();
            return true;
        }

        if let Some(port_index) = self
            .ports
            .iter()
            .position(|port| std::ptr::eq(port.holder_ptr(), selected))
        {
            self.on_port_event(port_index);
            return true;
        }

        if let Some(session_index) = self
            .sessions
            .iter()
            .position(|session| std::ptr::eq(session.holder_ptr(), selected))
        {
            self.on_session_event(session_index);
            return true;
        }

        false
    }

    fn signal_wakeup_event(&self) {
        self.wakeup_event.signal();
    }

    fn ensure_kernel_event_bridge(&self, event: &Arc<Event>) {
        if event.kernel_object_id().is_some() || self.system.is_null() {
            return;
        }

        let Some(current_thread) = self.system.get().current_thread() else {
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
                return;
            };
            (process, scheduler)
        };

        let Some(kernel) = self.system.get().kernel() else {
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
    }

    /// Handle a server-port event (incoming connection).
    /// Port of upstream `ServerManager::OnPortEvent`.
    fn on_port_event(&mut self, port_index: usize) {
        if port_index >= self.ports.len() {
            return;
        }

        let handler = self.ports[port_index].create_handler();
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

        let manager = self
            .self_reference
            .as_ref()
            .and_then(Weak::upgrade)
            .map(SessionRequestManager::new_with_server_manager)
            .map(|manager| Arc::new(Mutex::new(manager)))
            .unwrap_or_else(|| Arc::new(Mutex::new(SessionRequestManager::new())));
        manager.lock().unwrap().set_session_handler(handler);
        server_session.lock().unwrap().set_manager(manager.clone());
        let _ = self.register_session(server_session, manager);

        let holder_ptr = self.ports[port_index].holder_ptr() as *mut MultiWaitHolder;
        self.link_holder_ptr_to_deferred_list(holder_ptr);
    }

    /// Handle a session event (incoming IPC request).
    /// Port of upstream `ServerManager::OnSessionEvent`.
    fn on_session_event(&mut self, session_index: usize) {
        let session = &self.sessions[session_index];
        let result = session.server_session.lock().unwrap().receive_request();

        if result != 0 {
            // Session closed or no pending requests — remove it.
            log::debug!(
                "ServerManager({}): session {} closed (result={}), removing",
                self.name,
                session_index,
                result
            );
            self.sessions.remove(session_index);
            return;
        }

        self.complete_sync_request(session_index);
    }

    /// Handle a deferral event — retry deferred sessions.
    /// Port of upstream `ServerManager::OnDeferralEvent`.
    fn on_deferral_event(&mut self) {
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
        for session_index in deferred {
            if session_index < self.sessions.len() {
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
    /// In ruzu, IPC dispatch happens synchronously in svc_ipc::send_sync_request()
    /// which calls hle_ipc::complete_sync_request() directly. This path handles
    /// sessions that flow through the ServerManager's event loop (e.g., from
    /// port accept or deferred retry).
    fn complete_sync_request(&mut self, session_index: usize) {
        if session_index >= self.sessions.len() {
            return;
        }

        let manager = self.sessions[session_index].manager.clone();

        // Create a context for the dispatch. The ServerManager thread doesn't
        // have a guest thread context, so we use a minimal context with the
        // process memory from the system.
        let mut context = if !self.system.is_null() {
            let thread = self.system.get().current_thread();

            if let Some(thread) = thread {
                let tls = thread.lock().unwrap().get_tls_address().get();
                HLERequestContext::new_with_thread(thread, tls)
            } else {
                // No guest thread — create minimal context.
                HLERequestContext::new_with_thread(
                    Arc::new(std::sync::Mutex::new(
                        crate::hle::kernel::k_thread::KThread::new(),
                    )),
                    0,
                )
            }
        } else {
            // Null system — can't dispatch without memory access.
            log::warn!(
                "ServerManager({}): complete_sync_request with null system",
                self.name
            );
            return;
        };

        context.set_session_request_manager(manager.clone());
        if let Some(sm) = self.service_manager() {
            context.set_service_manager(sm);
        }
        context.set_is_deferred_value(false);
        context.populate_from_incoming_command_buffer(&[]);

        let result = hle_ipc::complete_sync_request(&manager, &mut context);

        // Check if the request was deferred.
        if context.get_is_deferred() {
            log::debug!(
                "ServerManager({}): session {} deferred",
                self.name,
                session_index
            );
            self.deferred_sessions.push(session_index);
            return;
        }

        // Write response back.
        context.write_to_outgoing_command_buffer();

        // Check for session close.
        if result == crate::hle::service::ipc_helpers::RESULT_SESSION_CLOSED {
            log::debug!(
                "ServerManager({}): session {} closed after dispatch",
                self.name,
                session_index
            );
            self.sessions.remove(session_index);
            return;
        }

        log::trace!(
            "ServerManager({}): session {} completed (result={:#x})",
            self.name,
            session_index,
            result.get_inner_value()
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
    /// This is a bounded Rust adaptation of upstream `StartAdditionalHostThreads`:
    /// the extra host threads exist with real dummy `KThread` owners, but they do
    /// not yet run the full concurrent `LoopProcessImpl()` model on the same
    /// `ServerManager` instance.
    pub fn activate_additional_host_threads(manager: &Arc<Mutex<ServerManager>>) {
        let (system, stop_flag, pending) = {
            let mut guard = manager.lock().unwrap();
            let pending = std::mem::take(&mut guard.pending_additional_host_threads);
            (
                guard.system,
                Arc::clone(&guard.additional_host_thread_stop),
                pending,
            )
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
                let stop = Arc::clone(&stop_flag);
                let handle = kernel.run_on_host_core_thread(
                    &thread_name,
                    Box::new(move || {
                        while !stop.load(Ordering::Acquire) {
                            std::thread::park_timeout(Duration::from_millis(50));
                        }
                    }),
                );
                manager.lock().unwrap().host_threads.push(handle);
            }
        }
    }

    /// Request the server to stop.
    pub fn request_stop(&self) {
        self.stop_requested.store(true, Ordering::Release);
        self.additional_host_thread_stop
            .store(true, Ordering::Release);
        for handle in &self.host_threads {
            handle.thread().unpark();
        }
        self.signal_wakeup_event();
    }

    #[cfg(test)]
    pub(crate) fn stop_requested_for_test(&self) -> bool {
        self.stop_requested.load(Ordering::Acquire)
    }

    /// Runs a server manager to completion.
    /// Port of upstream `ServerManager::RunServer(unique_ptr<ServerManager>)`.
    ///
    /// Upstream forwards ownership to `System::RunServer()` which then enters
    /// `KernelCore::RunServer()`.
    pub fn run_server(server_manager: ServerManager) {
        if server_manager.system.is_null() {
            log::warn!("ServerManager::run_server called with null system");
            return;
        }

        let system_ref = server_manager.system;
        system_ref.get().run_server(server_manager);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
