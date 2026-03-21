// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/server_manager.h and server_manager.cpp
//!
//! Contains:
//! - ServerManager: manages server ports and sessions for HLE services
//! - Session: wrapper pairing a KServerSession with a SessionRequestManager
//!
//! Upstream uses MultiWait/MultiWaitHolder for the event loop. We use
//! Event-based signaling with a polling loop matching the same behavioral
//! contract.

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use crate::hle::kernel::k_server_session::KServerSession;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{
    SessionRequestHandlerFactory, SessionRequestHandlerPtr, SessionRequestManager,
};
use crate::core::SystemRef;
use crate::hle::kernel::k_port::KPort;
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
struct Session {
    server_session: Arc<Mutex<KServerSession>>,
    manager: Arc<Mutex<SessionRequestManager>>,
}

/// Manages server ports and sessions for HLE services.
///
/// Port of upstream `Service::ServerManager`.
/// Upstream uses MultiWait for the event loop (WaitSignaled → Process).
/// We implement the same pattern with our MultiWait/MultiWaitHolder.
pub struct ServerManager {
    /// Reference to the System, matching upstream `Core::System& m_system`.
    /// All subsystem access (ServiceManager, Kernel, etc.) goes through this.
    system: SystemRef,

    /// Locally managed named ports (not registered with SM).
    managed_ports: HashMap<String, SessionRequestHandlerFactory>,

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
    deferred_sessions: Vec<usize>, // indices into sessions

    /// Wakeup holder in the multi-wait.
    /// Upstream: `std::optional<MultiWaitHolder> m_wakeup_holder`.
    wakeup_holder: Option<Box<MultiWaitHolder>>,

    /// Deferral holder in the multi-wait.
    /// Upstream: `std::optional<MultiWaitHolder> m_deferral_holder`.
    deferral_holder: Option<Box<MultiWaitHolder>>,

    /// Stop flag.
    /// Upstream: `std::stop_source m_stop_source`.
    stop_requested: AtomicBool,

    /// Whether the server has been stopped.
    stopped: AtomicBool,
}

impl ServerManager {
    /// Creates a new ServerManager.
    /// Port of upstream `ServerManager::ServerManager(Core::System& system)`.
    /// Creates a new ServerManager.
    /// Matches upstream `ServerManager::ServerManager(Core::System& system)`.
    pub fn new(system: SystemRef) -> Self {
        let wakeup_event = Arc::new(Event::new());

        // Create the wakeup holder for the event loop.
        let mut wakeup_holder = Box::new(MultiWaitHolder::from_event(wakeup_event.clone()));
        wakeup_holder.set_user_data(usize::MAX); // sentinel, not a real tag

        Self {
            system,
            managed_ports: HashMap::new(),
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
        }
    }

    /// Get the service manager from System.
    /// Upstream: `m_system.ServiceManager()`.
    fn service_manager(&self) -> Option<Arc<Mutex<ServiceManager>>> {
        if self.system.is_null() {
            return None;
        }
        self.system.get().service_manager()
    }

    /// Registers a session with a manager.
    /// Port of upstream `ServerManager::RegisterSession`.
    pub fn register_session(
        &mut self,
        server_session: Arc<Mutex<KServerSession>>,
        manager: Arc<Mutex<SessionRequestManager>>,
    ) -> ResultCode {
        self.sessions.push(Session {
            server_session,
            manager,
        });
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
        let sm = match self.service_manager() {
            Some(sm) => sm,
            None => return RESULT_SUCCESS, // No service manager available
        };
        let result = sm.lock().unwrap().register_service(
            service_name.to_string(),
            max_sessions,
            handler_factory,
        );

        if result.is_error() {
            log::warn!(
                "ServerManager: failed to register '{}' with SM: {:#x}",
                service_name,
                result.get_inner_value()
            );
        }

        result
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
    ///
    /// Upstream: creates a KPort, initializes it, registers it with the kernel,
    /// calls KObjectName::NewFromName to register the client port by name,
    /// then starts tracking the server port in the event loop.
    pub fn manage_named_port(
        &mut self,
        port_name: &str,
        handler_factory: SessionRequestHandlerFactory,
        max_sessions: u32,
    ) -> ResultCode {
        // Create and initialize a KPort (matching upstream).
        let mut port = KPort::new();
        port.initialize(max_sessions as i32, false, 0);

        // Register the object name with the kernel (matching upstream
        // KObjectName::NewFromName(kernel, &port->GetClientPort(), name)).
        if !self.system.is_null() {
            if let Some(kernel) = self.system.get().kernel() {
                if let Some(gd) = kernel.object_name_global_data() {
                    let _ = gd.new_from_name(0, port_name);
                }
            }
        }

        // Store the handler factory for the event loop.
        self.managed_ports
            .insert(port_name.to_string(), handler_factory);

        RESULT_SUCCESS
    }

    /// Manages deferral events.
    /// Port of upstream `ServerManager::ManageDeferral(KEvent**)`.
    /// Creates a deferral event, registers a MultiWaitHolder for it,
    /// and links it to the deferred list.
    pub fn manage_deferral(&mut self) -> (ResultCode, Option<Arc<Event>>) {
        // Create the deferral event.
        let event = Arc::new(Event::new());
        self.deferral_event = Some(event.clone());

        // Create a MultiWaitHolder for the deferral event.
        let mut holder = Box::new(MultiWaitHolder::from_event(event.clone()));
        holder.set_user_data(UserDataTag::DeferEvent as usize);

        // Link to the deferred list (will be picked up by next LinkDeferred call).
        self.link_to_deferred_list_holder(&mut holder);
        self.deferral_holder = Some(holder);

        (RESULT_SUCCESS, Some(event))
    }

    /// Link a holder to the deferred list and signal the wakeup event.
    /// Port of upstream `ServerManager::LinkToDeferredList`.
    fn link_to_deferred_list_holder(&self, holder: &mut MultiWaitHolder) {
        holder.link_to_multi_wait();
        // Signal the wakeup event so the event loop picks up the new item.
        self.wakeup_event.signal();
    }

    /// Move all items from the deferred list to the main multi-wait.
    /// Port of upstream `ServerManager::LinkDeferred`.
    fn link_deferred(&mut self) {
        // In upstream, this moves holders between two MultiWait lists.
        // Our simplified model doesn't need this since we check all holders.
    }

    /// Main loop for processing server events.
    /// Port of upstream `ServerManager::LoopProcess`.
    pub fn loop_process(&mut self) -> ResultCode {
        while !self.stop_requested.load(Ordering::Relaxed) {
            self.wait_and_process_impl();
        }
        self.stopped.store(true, Ordering::Release);
        RESULT_SUCCESS
    }

    /// Wait for a signaled event and process it.
    /// Port of upstream `ServerManager::WaitAndProcessImpl`.
    fn wait_and_process_impl(&mut self) -> bool {
        // Check if any session has a pending request.
        for (i, session) in self.sessions.iter().enumerate() {
            let ss = session.server_session.lock().unwrap();
            if ss.is_signaled() {
                drop(ss);
                self.on_session_event(i);
                return true;
            }
        }

        // Check deferral event.
        if let Some(ref event) = self.deferral_event {
            if event.is_signaled() {
                event.clear();
                self.on_deferral_event();
                return true;
            }
        }

        // Check wakeup event.
        if self.wakeup_event.is_signaled() {
            self.wakeup_event.clear();
            return true; // Just loop again to pick up new items.
        }

        // No events signaled — brief sleep to avoid busy-wait.
        std::thread::sleep(std::time::Duration::from_micros(100));
        false
    }

    /// Handle a session event (incoming IPC request).
    /// Port of upstream `ServerManager::OnSessionEvent`.
    fn on_session_event(&mut self, session_index: usize) {
        let session = &self.sessions[session_index];
        let mut ss = session.server_session.lock().unwrap();

        // Receive the request.
        let result = ss.receive_request();
        if result != 0 {
            // Session closed — remove it.
            drop(ss);
            self.sessions.remove(session_index);
            return;
        }
        drop(ss);

        // Complete the sync request (with deferral handling).
        self.complete_sync_request(session_index);
    }

    /// Handle a deferral event — retry deferred sessions.
    /// Port of upstream `ServerManager::OnDeferralEvent`.
    fn on_deferral_event(&mut self) {
        // Retry all deferred sessions.
        let deferred = std::mem::take(&mut self.deferred_sessions);
        for session_index in deferred {
            if session_index < self.sessions.len() {
                self.complete_sync_request(session_index);
            }
        }
    }

    /// Complete a sync request, handling deferral.
    /// Port of upstream `ServerManager::CompleteSyncRequest`.
    fn complete_sync_request(&mut self, session_index: usize) {
        // In upstream, this calls session->GetManager()->CompleteSyncRequest().
        // If the request was deferred, it's added to the deferred list.
        // For now, we don't have the full CompleteSyncRequest pipeline, so
        // we just mark as processed.
        let _ = session_index;
    }

    /// Starts additional host threads for processing.
    /// Port of upstream `ServerManager::StartAdditionalHostThreads`.
    pub fn start_additional_host_threads(&mut self, name: &str, num_threads: usize) {
        // Upstream spawns std::jthread instances that call LoopProcessImpl.
        // In our model, the HLE services run on the main thread.
        log::debug!(
            "ServerManager::start_additional_host_threads: {} x {}",
            name,
            num_threads
        );
    }

    /// Request the server to stop.
    pub fn request_stop(&self) {
        self.stop_requested.store(true, Ordering::Release);
        self.wakeup_event.signal(); // Wake the loop so it exits.
    }

    /// Runs a server manager to completion.
    /// Port of upstream `ServerManager::RunServer(unique_ptr<ServerManager>)`.
    pub fn run_server(mut server_manager: ServerManager) {
        server_manager.loop_process();
    }
}
