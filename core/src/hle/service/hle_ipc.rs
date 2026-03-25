// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/hle_ipc.h and hle_ipc.cpp
//! Status: Structural port with stubs for kernel-dependent methods
//!
//! Contains:
//! - SessionRequestHandler: trait for HLE session handlers
//! - SessionRequestManager: manages domain state and handler dispatch
//! - HLERequestContext: in-flight IPC request context

use std::sync::{Arc, Mutex, Weak};

use crate::hle::ipc;
use crate::hle::kernel::k_readable_event::KReadableEvent;

/// Reference to a kernel object for IPC handle translation.
///
/// Upstream stores raw `KAutoObject*` in `outgoing_copy_objects` / `outgoing_move_objects`.
/// Handle creation is deferred to `WriteToOutgoingCommandBuffer()` where
/// `handle_table.Add(object)` translates objects to handles.
///
/// We store either:
/// - `ObjectId(u64)` — a kernel object ID needing `handle_table.Add()` translation
/// - `Handle(u32)` — a pre-resolved handle (for services that already have one)
#[derive(Clone, Copy, Debug)]
pub enum KAutoObjectRef {
    /// A kernel object ID that needs handle_table.Add() translation.
    ObjectId(u64),
    /// A pre-resolved handle value.
    Handle(u32),
}

impl KAutoObjectRef {
    /// Resolve to a handle value. For ObjectId, would call handle_table.Add();
    /// for Handle, returns the value directly.
    pub fn resolve(&self) -> u32 {
        match self {
            KAutoObjectRef::ObjectId(id) => {
                // In a full implementation, this would call handle_table.Add(*id).
                // For now, treat the object ID as the handle value.
                *id as u32
            }
            KAutoObjectRef::Handle(h) => *h,
        }
    }
}
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::sm::sm::ServiceManager;

/// Handle type alias matching upstream `Kernel::Handle`.
pub type Handle = u32;

/// Trait implemented by HLE session handlers.
///
/// Corresponds to upstream `SessionRequestHandler`. This can be provided to a ServerSession
/// in order to hook into several relevant events (such as a new connection or a SyncRequest)
/// so they can be implemented in the emulator.
pub trait SessionRequestHandler: Send + Sync {
    /// Handles a sync request from the emulated application.
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode;

    /// Returns the service name, for logging.
    fn service_name(&self) -> &str {
        std::any::type_name::<Self>()
    }

    /// Downcast support for service-to-service direct handler access.
    /// Matches upstream `ServiceManager::GetService<T>()` which returns
    /// a typed `shared_ptr<T>` to the handler object.
    /// Default implementation panics — override for types that need downcasting.
    fn as_any(&self) -> &dyn std::any::Any {
        panic!("as_any() not implemented for {}", self.service_name())
    }
}

pub type SessionRequestHandlerPtr = Arc<dyn SessionRequestHandler>;
pub type SessionRequestHandlerWeakPtr = Weak<dyn SessionRequestHandler>;
pub type SessionRequestHandlerFactory = Box<dyn Fn() -> SessionRequestHandlerPtr + Send + Sync>;

/// Manages the underlying HLE requests for a session, and whether (or not) the session should be
/// treated as a domain. This is managed separately from server sessions, as this state is shared
/// when objects are cloned.
///
/// Corresponds to upstream `SessionRequestManager`.
pub struct SessionRequestManager {
    convert_to_domain: bool,
    is_domain: bool,
    is_initialized_for_sm: bool,
    session_handler: Option<SessionRequestHandlerPtr>,
    domain_handlers: Vec<Option<SessionRequestHandlerPtr>>,
    /// Reference to the owning ServerManager.
    /// Matches upstream: `Service::ServerManager& server_manager`.
    server_manager: Option<Arc<Mutex<super::server_manager::ServerManager>>>,
}

#[derive(Clone)]
enum PreparedSyncRequest {
    Session(SessionRequestHandlerPtr),
    Domain(SessionRequestHandlerPtr),
    CloseVirtualHandle(usize),
    StubSuccess,
}

impl SessionRequestManager {
    pub fn new() -> Self {
        Self {
            convert_to_domain: false,
            is_domain: false,
            is_initialized_for_sm: false,
            session_handler: None,
            domain_handlers: Vec::new(),
            server_manager: None,
        }
    }

    /// Create with a ServerManager reference, matching upstream constructor:
    /// `SessionRequestManager(KernelCore& kernel, ServerManager& server_manager)`.
    pub fn new_with_server_manager(server_manager: Arc<Mutex<super::server_manager::ServerManager>>) -> Self {
        Self {
            convert_to_domain: false,
            is_domain: false,
            is_initialized_for_sm: false,
            session_handler: None,
            domain_handlers: Vec::new(),
            server_manager: Some(server_manager),
        }
    }

    /// Get the ServerManager reference. Matches upstream `GetServerManager()`.
    pub fn get_server_manager(&self) -> Option<&Arc<Mutex<super::server_manager::ServerManager>>> {
        self.server_manager.as_ref()
    }

    pub fn is_domain(&self) -> bool {
        self.is_domain
    }

    pub fn convert_to_domain(&mut self) {
        self.domain_handlers = vec![self.session_handler.clone()];
        self.is_domain = true;
    }

    pub fn convert_to_domain_on_request_end(&mut self) {
        self.convert_to_domain = true;
    }

    pub fn domain_handler_count(&self) -> usize {
        self.domain_handlers.len()
    }

    pub fn has_session_handler(&self) -> bool {
        self.session_handler.is_some()
    }

    pub fn session_handler(&self) -> Option<&SessionRequestHandlerPtr> {
        self.session_handler.as_ref()
    }

    pub fn close_domain_handler(&mut self, index: usize) {
        if index < self.domain_handler_count() {
            self.domain_handlers[index] = None;
        } else {
            log::error!("Unexpected handler index {}", index);
        }
    }

    pub fn domain_handler(&self, index: usize) -> Option<&SessionRequestHandlerPtr> {
        assert!(
            index < self.domain_handler_count(),
            "Unexpected handler index {}",
            index
        );
        self.domain_handlers[index].as_ref()
    }

    pub fn append_domain_handler(&mut self, handler: SessionRequestHandlerPtr) {
        log::debug!(
            "AppendDomainHandler: index={} handler={}",
            self.domain_handlers.len() + 1,
            handler.service_name()
        );
        self.domain_handlers.push(Some(handler));
    }

    pub fn set_session_handler(&mut self, handler: SessionRequestHandlerPtr) {
        self.session_handler = Some(handler);
    }

    pub fn has_session_request_handler(&self, context: &HLERequestContext) -> bool {
        if self.is_domain() && context.has_domain_message_header() {
            let message_header = context.get_domain_message_header().unwrap();
            let object_id = message_header.object_id() as usize;

            if object_id > self.domain_handler_count() {
                log::error!("object_id {} is too big!", object_id);
                return false;
            }
            self.domain_handlers
                .get(object_id - 1)
                .map_or(false, |h| h.is_some())
        } else {
            self.session_handler.is_some()
        }
    }

    fn prepare_sync_request(&self, context: &HLERequestContext) -> PreparedSyncRequest {
        if !self.has_session_request_handler(context) {
            log::error!("Session handler is invalid, stubbing response!");
            return PreparedSyncRequest::StubSuccess;
        }

        if self.is_domain() && context.has_domain_message_header() {
            let domain_message_header = context.get_domain_message_header().unwrap();
            let object_id = domain_message_header.object_id() as usize;
            let command = domain_message_header.command();

            match command {
                ipc::DomainCommandType::SendMessage => {
                    if object_id > self.domain_handler_count() {
                        log::error!(
                            "object_id {} is too big! This probably means a recent service call \
                             needed to return a new interface!",
                            object_id
                        );
                        return PreparedSyncRequest::StubSuccess;
                    }
                    if let Some(Some(handler)) = self.domain_handlers.get(object_id - 1) {
                        log::debug!(
                            "HandleDomainSyncRequest: object_id={} cmd={} handler={}",
                            object_id,
                            context.get_command(),
                            handler.service_name()
                        );
                        return PreparedSyncRequest::Domain(handler.clone());
                    }
                    log::error!("Domain handler at index {} is null", object_id - 1);
                    return PreparedSyncRequest::StubSuccess;
                }
                ipc::DomainCommandType::CloseVirtualHandle => {
                    log::debug!("CloseVirtualHandle, object_id=0x{:08X}", object_id);
                    return PreparedSyncRequest::CloseVirtualHandle(object_id - 1);
                }
            }
        }

        // If domain but no domain header, fall through to session handler.
        // Matches upstream hle_ipc.cpp:62-63: "If there is no domain header,
        // the regular session handler is used"
        // This is normal for Control commands and also happens for certain
        // Request messages that the session handler dispatches directly.

        match &self.session_handler {
            Some(handler) => PreparedSyncRequest::Session(handler.clone()),
            None => PreparedSyncRequest::StubSuccess,
        }
    }

    fn finish_sync_request(&mut self) {
        if self.convert_to_domain {
            assert!(
                !self.is_domain(),
                "ServerSession is already a domain instance."
            );
            self.convert_to_domain();
            self.convert_to_domain = false;
        }
    }

    pub fn get_is_initialized_for_sm(&self) -> bool {
        self.is_initialized_for_sm
    }

    pub fn set_is_initialized_for_sm(&mut self) {
        self.is_initialized_for_sm = true;
    }
}

pub fn complete_sync_request(
    manager: &Arc<Mutex<SessionRequestManager>>,
    context: &mut HLERequestContext,
) -> ResultCode {
    let dispatch = {
        let guard = manager.lock().unwrap();
        guard.prepare_sync_request(context)
    };

    let result = match dispatch {
        PreparedSyncRequest::Session(handler) | PreparedSyncRequest::Domain(handler) => {
            handler.handle_sync_request(context)
        }
        PreparedSyncRequest::CloseVirtualHandle(index) => {
            manager.lock().unwrap().close_domain_handler(index);
            let mut rb = super::ipc_helpers::ResponseBuilder::new(context, 2, 0, 0);
            rb.push_result(RESULT_SUCCESS);
            RESULT_SUCCESS
        }
        PreparedSyncRequest::StubSuccess => {
            let mut rb = super::ipc_helpers::ResponseBuilder::new(context, 2, 0, 0);
            rb.push_result(RESULT_SUCCESS);
            RESULT_SUCCESS
        }
    };

    manager.lock().unwrap().finish_sync_request();
    result
}

/// Class containing information about an in-flight IPC request being handled by an HLE service
/// implementation.
///
/// Corresponds to upstream `HLERequestContext`.
///
/// Upstream stores `KThread* thread`, `Core::Memory::Memory& memory`, `KernelCore& kernel`,
/// and `KServerSession* server_session`. We store the thread, shared memory, and TLS address
/// to match the upstream access pattern (thread→process→handle_table, memory write-back).
pub struct HLERequestContext {
    /// IPC command buffer.
    pub cmd_buf: [u32; ipc::COMMAND_BUFFER_LENGTH],

    /// The requesting thread. Matches upstream `KThread* thread`.
    thread: Option<Arc<std::sync::Mutex<crate::hle::kernel::k_thread::KThread>>>,
    /// Guest memory (legacy flat buffer, used as fallback when Memory bridge is not available).
    shared_memory: Option<crate::hle::kernel::k_process::SharedProcessMemory>,
    /// Guest memory bridge. Matches upstream `Core::Memory::Memory& memory`.
    /// When present, this is the authoritative path for TLS reads/writes,
    /// matching upstream `memory.GetPointer(tls_address)`.
    memory: Option<Arc<std::sync::Mutex<crate::memory::memory::Memory>>>,
    /// TLS address for command buffer read/write.
    /// Upstream derives this from `thread->GetTlsAddress()`.
    tls_address: u64,

    command_header: Option<ipc::CommandHeader>,
    handle_descriptor_header: Option<ipc::HandleDescriptorHeader>,
    data_payload_header: Option<ipc::DataPayloadHeader>,
    domain_message_header: Option<ipc::DomainMessageHeader>,

    buffer_x_descriptors: Vec<ipc::BufferDescriptorX>,
    buffer_a_descriptors: Vec<ipc::BufferDescriptorABW>,
    buffer_b_descriptors: Vec<ipc::BufferDescriptorABW>,
    buffer_w_descriptors: Vec<ipc::BufferDescriptorABW>,
    buffer_c_descriptors: Vec<ipc::BufferDescriptorC>,

    incoming_move_handles: Vec<Handle>,
    incoming_copy_handles: Vec<Handle>,

    pub(crate) outgoing_move_objects: Vec<KAutoObjectRef>,
    pub(crate) outgoing_copy_objects: Vec<KAutoObjectRef>,
    outgoing_domain_objects: Vec<Option<SessionRequestHandlerPtr>>,

    command: u32,
    pid: u64,
    pub write_size: u32,
    pub data_payload_offset: u32,
    pub handles_offset: u32,
    pub domain_offset: u32,

    manager: Option<Arc<Mutex<SessionRequestManager>>>,
    service_manager: Option<Arc<Mutex<ServiceManager>>>,
    is_deferred: bool,

    /// Temporary: holds the server session from the last create_session_with_manager call,
    /// so that push_ipc_interface can register it with the ServerManager.
    pub(crate) last_created_server_session: Option<Arc<Mutex<crate::hle::kernel::k_server_session::KServerSession>>>,
}

impl HLERequestContext {
    /// Create a context with thread/memory access, matching upstream constructor.
    ///
    /// Upstream: `HLERequestContext(KernelCore&, Memory&, KServerSession*, KThread*)`
    pub fn new_with_thread(
        thread: Arc<std::sync::Mutex<crate::hle::kernel::k_thread::KThread>>,
        shared_memory: crate::hle::kernel::k_process::SharedProcessMemory,
        tls_address: u64,
    ) -> Self {
        let mut ctx = Self {
            cmd_buf: [0u32; ipc::COMMAND_BUFFER_LENGTH],
            thread: Some(thread),
            shared_memory: Some(shared_memory),
            memory: None,
            tls_address,
            command_header: None,
            handle_descriptor_header: None,
            data_payload_header: None,
            domain_message_header: None,
            buffer_x_descriptors: Vec::new(),
            buffer_a_descriptors: Vec::new(),
            buffer_b_descriptors: Vec::new(),
            buffer_w_descriptors: Vec::new(),
            buffer_c_descriptors: Vec::new(),
            incoming_move_handles: Vec::new(),
            incoming_copy_handles: Vec::new(),
            outgoing_move_objects: Vec::new(),
            outgoing_copy_objects: Vec::new(),
            outgoing_domain_objects: Vec::new(),
            command: 0,
            pid: 0,
            write_size: 0,
            data_payload_offset: 0,
            handles_offset: 0,
            domain_offset: 0,
            manager: None,
            service_manager: None,
            is_deferred: false,
            last_created_server_session: None,
        };
        ctx.cmd_buf[0] = 0;
        ctx
    }

    /// Create a context without thread/memory (for tests or contexts where
    /// the caller manages TLS read/write externally).
    pub fn new() -> Self {
        Self {
            cmd_buf: [0u32; ipc::COMMAND_BUFFER_LENGTH],
            thread: None,
            shared_memory: None,
            memory: None,
            tls_address: 0,
            command_header: None,
            handle_descriptor_header: None,
            data_payload_header: None,
            domain_message_header: None,
            buffer_x_descriptors: Vec::new(),
            buffer_a_descriptors: Vec::new(),
            buffer_b_descriptors: Vec::new(),
            buffer_w_descriptors: Vec::new(),
            buffer_c_descriptors: Vec::new(),
            incoming_move_handles: Vec::new(),
            incoming_copy_handles: Vec::new(),
            outgoing_move_objects: Vec::new(),
            outgoing_copy_objects: Vec::new(),
            outgoing_domain_objects: Vec::new(),
            command: 0,
            pid: 0,
            write_size: 0,
            data_payload_offset: 0,
            handles_offset: 0,
            domain_offset: 0,
            manager: None,
            service_manager: None,
            is_deferred: false,
            last_created_server_session: None,
        }
    }

    /// Returns a pointer to the IPC command buffer for this request.
    pub fn command_buffer(&self) -> &[u32; ipc::COMMAND_BUFFER_LENGTH] {
        &self.cmd_buf
    }

    /// Returns the requesting thread.
    ///
    /// Matches upstream ownership where `HLERequestContext` carries `KThread* thread`.
    pub fn get_thread(
        &self,
    ) -> Option<Arc<std::sync::Mutex<crate::hle::kernel::k_thread::KThread>>> {
        self.thread.clone()
    }

    /// Returns the requesting process shared memory backing.
    pub fn get_shared_memory(
        &self,
    ) -> Option<crate::hle::kernel::k_process::SharedProcessMemory> {
        self.shared_memory.clone()
    }

    /// Returns a mutable pointer to the IPC command buffer.
    pub fn command_buffer_mut(&mut self) -> &mut [u32; ipc::COMMAND_BUFFER_LENGTH] {
        &mut self.cmd_buf
    }

    pub fn get_hipc_command(&self) -> u32 {
        self.command
    }

    pub fn get_tipc_command(&self) -> u32 {
        if let Some(ref header) = self.command_header {
            let raw_type = header.command_type_raw();
            raw_type.wrapping_sub(ipc::CommandType::TipcCommandRegion as u32)
        } else {
            0
        }
    }

    pub fn get_command(&self) -> u32 {
        if let Some(ref header) = self.command_header {
            if header.is_tipc() {
                self.get_tipc_command()
            } else {
                self.get_hipc_command()
            }
        } else {
            self.get_hipc_command()
        }
    }

    pub fn is_tipc(&self) -> bool {
        self.command_header
            .as_ref()
            .map_or(false, |h| h.is_tipc())
    }

    pub fn get_command_type(&self) -> ipc::CommandType {
        self.command_header
            .as_ref()
            .map_or(ipc::CommandType::Invalid, |h| h.command_type())
    }

    pub fn get_pid(&self) -> u64 {
        self.pid
    }

    pub fn get_data_payload_offset(&self) -> u32 {
        self.data_payload_offset
    }

    pub fn buffer_descriptor_x(&self) -> &[ipc::BufferDescriptorX] {
        &self.buffer_x_descriptors
    }

    pub fn buffer_descriptor_a(&self) -> &[ipc::BufferDescriptorABW] {
        &self.buffer_a_descriptors
    }

    pub fn buffer_descriptor_b(&self) -> &[ipc::BufferDescriptorABW] {
        &self.buffer_b_descriptors
    }

    pub fn buffer_descriptor_c(&self) -> &[ipc::BufferDescriptorC] {
        &self.buffer_c_descriptors
    }

    pub fn get_domain_message_header(&self) -> Option<&ipc::DomainMessageHeader> {
        self.domain_message_header.as_ref()
    }

    pub fn has_domain_message_header(&self) -> bool {
        self.domain_message_header.is_some()
    }

    pub fn get_copy_handle(&self, index: usize) -> Handle {
        self.incoming_copy_handles[index]
    }

    pub fn get_move_handle(&self, index: usize) -> Handle {
        self.incoming_move_handles[index]
    }

    pub fn add_move_object(&mut self, obj: KAutoObjectRef) {
        self.outgoing_move_objects.push(obj);
    }

    pub fn add_copy_object(&mut self, obj: KAutoObjectRef) {
        self.outgoing_copy_objects.push(obj);
    }

    /// Convenience: add a move object from a raw handle.
    pub fn add_move_handle(&mut self, handle: Handle) {
        self.outgoing_move_objects.push(KAutoObjectRef::Handle(handle));
    }

    /// Convenience: add a copy object from a raw handle.
    pub fn add_copy_handle(&mut self, handle: Handle) {
        self.outgoing_copy_objects.push(KAutoObjectRef::Handle(handle));
    }

    pub fn add_domain_object(&mut self, object: SessionRequestHandlerPtr) {
        self.outgoing_domain_objects.push(Some(object));
    }

    /// Add a null domain object entry. Upstream writes domain object ID = 0 for nullptr
    /// OutInterface parameters (e.g., when a command returns an error but the method
    /// signature has Out<SharedPointer<T>>). The response layout is compile-time fixed,
    /// so the slot must always be present.
    pub fn add_null_domain_object(&mut self) {
        self.outgoing_domain_objects.push(None);
    }

    /// Set the Memory bridge for TLS reads/writes.
    /// Matches upstream `Core::Memory::Memory& memory` passed to the HLERequestContext constructor.
    pub fn set_memory(&mut self, memory: Arc<std::sync::Mutex<crate::memory::memory::Memory>>) {
        self.memory = Some(memory);
    }

    pub fn set_session_request_manager(&mut self, manager: Arc<Mutex<SessionRequestManager>>) {
        self.manager = Some(manager);
    }

    pub fn get_manager(&self) -> Option<&Arc<Mutex<SessionRequestManager>>> {
        self.manager.as_ref()
    }

    pub fn set_service_manager(&mut self, service_manager: Arc<Mutex<ServiceManager>>) {
        self.service_manager = Some(service_manager);
    }

    pub fn get_service_manager(&self) -> Option<&Arc<Mutex<ServiceManager>>> {
        self.service_manager.as_ref()
    }

    /// Creates a full KSession (server + client) for a service handler.
    /// Registers the client session in the process handle table and links
    /// the server session to the manager.
    ///
    /// Matches upstream flow:
    /// 1. KSession::Create(kernel) → creates session with server + client
    /// 2. session->Initialize(nullptr, 0)
    /// 3. ServerManager::RegisterSession(server_session, manager)
    /// 4. handle_table.Add(client_session)
    pub fn create_session_for_service(
        &mut self,
        handler: SessionRequestHandlerPtr,
    ) -> Option<Handle> {
        let manager = Arc::new(std::sync::Mutex::new(SessionRequestManager::new()));
        manager.lock().unwrap().set_session_handler(handler);
        self.create_session_with_manager(manager)
    }

    /// Creates a full KSession that shares an existing SessionRequestManager.
    /// Used by CloneCurrentObject to replicate upstream behavior where the clone
    /// is registered with the SAME manager as the parent session.
    ///
    /// Matches upstream `Controller::CloneCurrentObject`:
    /// ```cpp
    /// session_manager->GetServerManager().RegisterSession(
    ///     &session->GetServerSession(), session_manager);
    /// ```
    pub fn create_session_with_manager(
        &mut self,
        manager: Arc<std::sync::Mutex<SessionRequestManager>>,
    ) -> Option<Handle> {
        log::info!("HLERequestContext::create_session_with_manager: begin");
        let thread = self.thread.as_ref()?;
        let thread_guard = thread.lock().unwrap();
        let parent = thread_guard.parent.as_ref()?.upgrade()?;
        let mut process = parent.lock().unwrap();

        static NEXT_SESSION_OBJECT_ID: std::sync::atomic::AtomicU64 =
            std::sync::atomic::AtomicU64::new(0x1000_0000);
        let object_id = NEXT_SESSION_OBJECT_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        // Create the full KSession (owns both server and client endpoints).
        // Matches upstream: KSession::Create(kernel) + Initialize(nullptr, 0).
        let session = Arc::new(std::sync::Mutex::new(
            crate::hle::kernel::k_session::KSession::new(),
        ));
        {
            let mut ksession = session.lock().unwrap();
            ksession.initialize(None, 0);
            ksession.server.lock().unwrap().initialize(object_id);
            ksession
                .client
                .lock()
                .unwrap()
                .initialize_with_manager(object_id, manager.clone());
        }
        log::info!(
            "HLERequestContext::create_session_with_manager: endpoints initialized object_id={:#x}",
            object_id
        );

        let server_session = session.lock().unwrap().get_server_session().clone();
        let client_session = session.lock().unwrap().get_client_session().clone();

        // Register the server session with its manager.
        // Matches upstream: ServerManager.RegisterSession(&serverSession, manager).
        log::info!("HLERequestContext::create_session_with_manager: registering session");
        server_session.lock().unwrap().set_manager(manager.clone());
        client_session
            .lock()
            .unwrap()
            .initialize_with_manager(object_id, manager.clone());

        // Store server session for push_ipc_interface to register with ServerManager.
        self.last_created_server_session = Some(server_session.clone());

        // Register the owning session and the client endpoint in the process object tables.
        log::info!("HLERequestContext::create_session_with_manager: registering process objects");
        process.register_session_object(object_id, session);
        process.register_client_session_object(object_id, client_session);
        let handle = process.handle_table.add(object_id).ok()?;
        log::info!(
            "HLERequestContext::create_session_with_manager: done handle={:#x}",
            handle
        );

        Some(handle)
    }

    /// Creates a KReadableEvent and registers it in the current process handle table.
    ///
    /// Matches the ownership of upstream `ServiceContext::CreateEvent`, while returning both the
    /// readable-end object and the process handle so service owners can keep persistent event
    /// objects instead of manufacturing one-off handles per request.
    pub fn create_readable_event(
        &self,
        signaled: bool,
    ) -> Option<(Handle, Arc<Mutex<KReadableEvent>>)> {
        let thread = self.thread.as_ref()?;
        let thread_guard = thread.lock().unwrap();
        let parent = thread_guard.parent.as_ref()?.upgrade()?;
        let mut process = parent.lock().unwrap();

        static NEXT_EVENT_ID: std::sync::atomic::AtomicU64 =
            std::sync::atomic::AtomicU64::new(0x2000_0000);
        let object_id = NEXT_EVENT_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        // Create and register a KReadableEvent.
        let readable = Arc::new(Mutex::new(KReadableEvent::new()));
        {
            let mut re = readable.lock().unwrap();
            re.initialize(0, object_id);
            if signaled {
                re.is_signaled = true;
            }
        }
        process.register_readable_event_object(object_id, readable.clone());

        let handle = process.handle_table.add(object_id).ok()?;
        Some((handle, readable))
    }

    /// Creates a signaled KReadableEvent and registers it in the process
    /// handle table. Returns the handle for use in copy handle responses.
    ///
    /// Matches upstream `ServiceContext::CreateEvent` + signal pattern.
    pub fn create_readable_event_handle(&self, signaled: bool) -> Option<Handle> {
        self.create_readable_event(signaled).map(|(handle, _)| handle)
    }

    pub fn get_is_deferred(&self) -> bool {
        self.is_deferred
    }

    pub fn set_is_deferred(&mut self) {
        self.is_deferred = true;
    }

    pub fn set_is_deferred_value(&mut self, value: bool) {
        self.is_deferred = value;
    }

    /// Helper function to get the size of the input buffer.
    pub fn get_read_buffer_size(&self, buffer_index: usize) -> usize {
        let is_buffer_a = self.buffer_a_descriptors.len() > buffer_index
            && self.buffer_a_descriptors[buffer_index].size() > 0;
        if is_buffer_a {
            self.buffer_a_descriptors[buffer_index].size() as usize
        } else if self.buffer_x_descriptors.len() > buffer_index {
            self.buffer_x_descriptors[buffer_index].size() as usize
        } else {
            0
        }
    }

    /// Helper function to get the size of the output buffer.
    pub fn get_write_buffer_size(&self, buffer_index: usize) -> usize {
        let is_buffer_b = self.buffer_b_descriptors.len() > buffer_index
            && self.buffer_b_descriptors[buffer_index].size() > 0;
        if is_buffer_b {
            self.buffer_b_descriptors[buffer_index].size() as usize
        } else if self.buffer_c_descriptors.len() > buffer_index {
            self.buffer_c_descriptors[buffer_index].size() as usize
        } else {
            0
        }
    }

    /// Helper function to test whether the input buffer at buffer_index can be read.
    pub fn can_read_buffer(&self, buffer_index: usize) -> bool {
        let is_buffer_a = self.buffer_a_descriptors.len() > buffer_index
            && self.buffer_a_descriptors[buffer_index].size() > 0;
        if is_buffer_a {
            self.buffer_a_descriptors.len() > buffer_index
        } else {
            self.buffer_x_descriptors.len() > buffer_index
        }
    }

    /// Helper function to test whether the output buffer at buffer_index can be written.
    pub fn can_write_buffer(&self, buffer_index: usize) -> bool {
        let is_buffer_b = self.buffer_b_descriptors.len() > buffer_index
            && self.buffer_b_descriptors[buffer_index].size() > 0;
        if is_buffer_b {
            self.buffer_b_descriptors.len() > buffer_index
        } else {
            self.buffer_c_descriptors.len() > buffer_index
        }
    }

    // -- ReadBuffer / WriteBuffer --
    //
    // Matches upstream methods on HLERequestContext (hle_ipc.h lines 261-328,
    // hle_ipc.cpp lines 317-506).
    //
    // Upstream uses `Core::Memory::Memory& memory` (page-table-aware) to
    // read/write guest buffers. Ruzu uses the same `Memory` bridge when
    // available, falling back to `shared_memory` (ProcessMemoryData) for
    // test paths.
    //
    // Upstream `ReadBuffer` returns `std::span<const u8>` (zero-copy).
    // We return `Vec<u8>` (copy) because the Memory bridge is behind a Mutex.
    // See DIFF_WITH_YUZU.md for rationale.

    /// Returns a reference to the Memory bridge.
    ///
    /// Matches upstream `Core::Memory::Memory& GetMemory() const`.
    pub fn get_memory(&self) -> Option<&Arc<std::sync::Mutex<crate::memory::memory::Memory>>> {
        self.memory.as_ref()
    }

    /// Read from buffer descriptor A at the given index.
    ///
    /// Matches upstream `HLERequestContext::ReadBufferA(size_t buffer_index)`.
    pub fn read_buffer_a(&self, buffer_index: usize) -> Vec<u8> {
        if buffer_index >= self.buffer_a_descriptors.len() {
            log::error!("BufferDescriptorA invalid buffer_index {}", buffer_index);
            return Vec::new();
        }
        let size = self.buffer_a_descriptors[buffer_index].size() as usize;
        let address = self.buffer_a_descriptors[buffer_index].address();
        self.read_guest_memory(address, size)
    }

    /// Read from buffer descriptor X at the given index.
    ///
    /// Matches upstream `HLERequestContext::ReadBufferX(size_t buffer_index)`.
    pub fn read_buffer_x(&self, buffer_index: usize) -> Vec<u8> {
        if buffer_index >= self.buffer_x_descriptors.len() {
            log::error!("BufferDescriptorX invalid buffer_index {}", buffer_index);
            return Vec::new();
        }
        let size = self.buffer_x_descriptors[buffer_index].size() as usize;
        let address = self.buffer_x_descriptors[buffer_index].address();
        self.read_guest_memory(address, size)
    }

    /// Read from the appropriate buffer descriptor (A if available, else X).
    ///
    /// Matches upstream `HLERequestContext::ReadBuffer(size_t buffer_index)`.
    /// Returns a copy (see `ReadBufferCopy` in upstream for equivalent semantics).
    pub fn read_buffer(&self, buffer_index: usize) -> Vec<u8> {
        let is_buffer_a = self.buffer_a_descriptors.len() > buffer_index
            && self.buffer_a_descriptors[buffer_index].size() > 0;
        let is_buffer_x = self.buffer_x_descriptors.len() > buffer_index
            && self.buffer_x_descriptors[buffer_index].size() > 0;

        if is_buffer_a && is_buffer_x {
            log::warn!(
                "ReadBuffer: both A and X descriptors available, a.size={}, x.size={}",
                self.buffer_a_descriptors[buffer_index].size(),
                self.buffer_x_descriptors[buffer_index].size()
            );
        }

        if is_buffer_a {
            self.read_buffer_a(buffer_index)
        } else {
            self.read_buffer_x(buffer_index)
        }
    }

    /// Write to the appropriate buffer descriptor (B if available, else C).
    ///
    /// Matches upstream `HLERequestContext::WriteBuffer(const void*, size_t, size_t)`.
    /// Returns the number of bytes written.
    pub fn write_buffer(&self, data: &[u8], buffer_index: usize) -> usize {
        if data.is_empty() {
            log::warn!("WriteBuffer: skip empty buffer write");
            return 0;
        }

        let is_buffer_b = self.buffer_b_descriptors.len() > buffer_index
            && self.buffer_b_descriptors[buffer_index].size() > 0;
        let buffer_size = self.get_write_buffer_size(buffer_index);
        let mut size = data.len();
        if size > buffer_size {
            log::error!(
                "WriteBuffer: size ({:#x}) > buffer_size ({:#x})",
                size,
                buffer_size
            );
            size = buffer_size;
        }

        if is_buffer_b {
            self.write_buffer_b(&data[..size], buffer_index)
        } else {
            self.write_buffer_c(&data[..size], buffer_index)
        }
    }

    /// Write to buffer descriptor B at the given index.
    ///
    /// Matches upstream `HLERequestContext::WriteBufferB(const void*, size_t, size_t)`.
    pub fn write_buffer_b(&self, data: &[u8], buffer_index: usize) -> usize {
        if buffer_index >= self.buffer_b_descriptors.len() || data.is_empty() {
            return 0;
        }
        let buffer_size = self.buffer_b_descriptors[buffer_index].size() as usize;
        let size = data.len().min(buffer_size);
        let address = self.buffer_b_descriptors[buffer_index].address();
        self.write_guest_memory(address, &data[..size]);
        size
    }

    /// Write to buffer descriptor C at the given index.
    ///
    /// Matches upstream `HLERequestContext::WriteBufferC(const void*, size_t, size_t)`.
    pub fn write_buffer_c(&self, data: &[u8], buffer_index: usize) -> usize {
        if buffer_index >= self.buffer_c_descriptors.len() || data.is_empty() {
            return 0;
        }
        let buffer_size = self.buffer_c_descriptors[buffer_index].size() as usize;
        let size = data.len().min(buffer_size);
        let address = self.buffer_c_descriptors[buffer_index].address();
        self.write_guest_memory(address, &data[..size]);
        size
    }

    /// Read `size` bytes from guest virtual address using the Memory bridge.
    ///
    /// Uses `Memory::read_block` (page-table-aware) when available, falls back
    /// to `ProcessMemoryData` for test paths.
    fn read_guest_memory(&self, address: u64, size: usize) -> Vec<u8> {
        if size == 0 || address == 0 {
            return Vec::new();
        }
        let mut buf = vec![0u8; size];
        if let Some(ref memory) = self.memory {
            memory.lock().unwrap().read_block(address, &mut buf);
        } else if let Some(ref shared_memory) = self.shared_memory {
            let mem = shared_memory.read().unwrap();
            let block = mem.read_block(address, size);
            let copy_len = block.len().min(size);
            buf[..copy_len].copy_from_slice(&block[..copy_len]);
        } else {
            log::error!(
                "read_guest_memory: no memory accessor available for addr={:#x} size={:#x}",
                address,
                size
            );
        }
        buf
    }

    /// Write bytes to guest virtual address using the Memory bridge.
    fn write_guest_memory(&self, address: u64, data: &[u8]) {
        if data.is_empty() || address == 0 {
            return;
        }
        if let Some(ref memory) = self.memory {
            memory.lock().unwrap().write_block(address, data);
        } else if let Some(ref shared_memory) = self.shared_memory {
            let mut mem = shared_memory.write().unwrap();
            mem.write_block(address, data);
        } else {
            log::error!(
                "write_guest_memory: no memory accessor available for addr={:#x} size={:#x}",
                address,
                data.len()
            );
        }
    }

    /// Populates this context from the thread's TLS command buffer.
    ///
    /// Matches upstream `PopulateFromIncomingCommandBuffer` which reads from
    /// `thread->GetTlsAddress()`. When constructed with `new_with_thread`,
    /// reads directly from guest memory. Falls back to the provided buffer
    /// if no memory is available (test path).
    pub fn populate_from_incoming_command_buffer(&mut self, src_cmdbuf: &[u32]) {
        if let Some(ref memory) = self.memory {
            // Read command buffer from guest TLS via Memory bridge (upstream path).
            // Matches upstream: u32* cmd_buf = reinterpret_cast<u32*>(memory.GetPointer(tls_address))
            let m = memory.lock().unwrap();
            for i in 0..ipc::COMMAND_BUFFER_LENGTH {
                self.cmd_buf[i] = m.read_32(self.tls_address + (i as u64 * 4));
            }
        } else if let Some(ref mem) = self.shared_memory {
            // Fallback: read from ProcessMemoryData (test path or pre-Memory setup).
            let mem = mem.read().unwrap();
            for i in 0..ipc::COMMAND_BUFFER_LENGTH {
                self.cmd_buf[i] = mem.read_32(self.tls_address + (i as u64 * 4));
            }
        } else {
            // Test/fallback path: copy from provided buffer.
            let len = src_cmdbuf.len().min(ipc::COMMAND_BUFFER_LENGTH);
            self.cmd_buf[..len].copy_from_slice(&src_cmdbuf[..len]);
        }
        self.parse_command_buffer(true);
    }

    /// Parses the command buffer header fields.
    fn parse_command_buffer(&mut self, incoming: bool) {
        let mut index: usize = 0;

        // Parse command header (2 words).
        if index + 1 >= ipc::COMMAND_BUFFER_LENGTH {
            return;
        }
        let header = ipc::CommandHeader {
            raw_low: self.cmd_buf[index],
            raw_high: self.cmd_buf[index + 1],
        };
        index += 2;
        self.command_header = Some(header);

        if header.is_close_command() {
            return;
        }

        // Handle descriptor header.
        if header.enable_handle_descriptor() {
            if index >= ipc::COMMAND_BUFFER_LENGTH {
                return;
            }
            let hdh = ipc::HandleDescriptorHeader {
                raw: self.cmd_buf[index],
            };
            index += 1;
            self.handle_descriptor_header = Some(hdh);

            if hdh.send_current_pid() {
                // Upstream sets the client PID from the requesting thread's owner process
                // when the handle descriptor requests SendCurrentPid, then skips the
                // placeholder words in the incoming command buffer.
                self.pid = self
                    .thread
                    .as_ref()
                    .and_then(|thread| {
                        thread
                            .lock()
                            .unwrap()
                            .parent
                            .as_ref()
                            .and_then(Weak::upgrade)
                    })
                    .map(|process| process.lock().unwrap().get_process_id())
                    .unwrap_or(0);
                index += 2;
            }

            if incoming {
                let num_copy = hdh.num_handles_to_copy() as usize;
                let num_move = hdh.num_handles_to_move() as usize;
                self.incoming_copy_handles.clear();
                self.incoming_move_handles.clear();

                for _ in 0..num_copy {
                    if index < ipc::COMMAND_BUFFER_LENGTH {
                        self.incoming_copy_handles.push(self.cmd_buf[index]);
                        index += 1;
                    }
                }
                for _ in 0..num_move {
                    if index < ipc::COMMAND_BUFFER_LENGTH {
                        self.incoming_move_handles.push(self.cmd_buf[index]);
                        index += 1;
                    }
                }
            } else {
                let num_copy = hdh.num_handles_to_copy() as usize;
                let num_move = hdh.num_handles_to_move() as usize;
                index += num_copy + num_move;
            }
        }

        // Buffer descriptors X.
        let num_x = header.num_buf_x_descriptors() as usize;
        self.buffer_x_descriptors.clear();
        for _ in 0..num_x {
            if index + 1 < ipc::COMMAND_BUFFER_LENGTH {
                self.buffer_x_descriptors.push(ipc::BufferDescriptorX {
                    raw: self.cmd_buf[index],
                    address_bits_0_31: self.cmd_buf[index + 1],
                });
                index += 2;
            }
        }

        // Buffer descriptors A.
        let num_a = header.num_buf_a_descriptors() as usize;
        self.buffer_a_descriptors.clear();
        for _ in 0..num_a {
            if index + 2 < ipc::COMMAND_BUFFER_LENGTH {
                self.buffer_a_descriptors.push(ipc::BufferDescriptorABW {
                    size_bits_0_31: self.cmd_buf[index],
                    address_bits_0_31: self.cmd_buf[index + 1],
                    raw_word2: self.cmd_buf[index + 2],
                });
                index += 3;
            }
        }

        // Buffer descriptors B.
        let num_b = header.num_buf_b_descriptors() as usize;
        self.buffer_b_descriptors.clear();
        for _ in 0..num_b {
            if index + 2 < ipc::COMMAND_BUFFER_LENGTH {
                self.buffer_b_descriptors.push(ipc::BufferDescriptorABW {
                    size_bits_0_31: self.cmd_buf[index],
                    address_bits_0_31: self.cmd_buf[index + 1],
                    raw_word2: self.cmd_buf[index + 2],
                });
                index += 3;
            }
        }

        // Buffer descriptors W.
        let num_w = header.num_buf_w_descriptors() as usize;
        self.buffer_w_descriptors.clear();
        for _ in 0..num_w {
            if index + 2 < ipc::COMMAND_BUFFER_LENGTH {
                self.buffer_w_descriptors.push(ipc::BufferDescriptorABW {
                    size_bits_0_31: self.cmd_buf[index],
                    address_bits_0_31: self.cmd_buf[index + 1],
                    raw_word2: self.cmd_buf[index + 2],
                });
                index += 3;
            }
        }

        let buffer_c_offset = index + header.data_size() as usize;

        if !header.is_tipc() {
            // Padding to align to 16 bytes (4 words).
            if index & 3 != 0 {
                index += 4 - (index & 3);
            }

            // Domain message header check.
            // For simplicity, we check the manager if available.
            let is_manager_domain = self
                .manager
                .as_ref()
                .map_or(false, |m| m.lock().unwrap().is_domain());

            if is_manager_domain
                && ((header.command_type() == ipc::CommandType::Request
                    || header.command_type() == ipc::CommandType::RequestWithContext)
                    || !incoming)
            {
                if incoming || self.domain_message_header.is_some() {
                    if index + 3 < ipc::COMMAND_BUFFER_LENGTH {
                        self.domain_message_header = Some(ipc::DomainMessageHeader {
                            raw: [
                                self.cmd_buf[index],
                                self.cmd_buf[index + 1],
                                self.cmd_buf[index + 2],
                                self.cmd_buf[index + 3],
                            ],
                        });
                        index += 4;
                    }
                }
            }

            // Data payload header (2 words).
            if index + 1 < ipc::COMMAND_BUFFER_LENGTH {
                self.data_payload_header = Some(ipc::DataPayloadHeader {
                    magic: self.cmd_buf[index],
                    _padding: self.cmd_buf[index + 1],
                });
                index += 2;
            }

            self.data_payload_offset = index as u32;

            // Check for CloseVirtualHandle (no further data).
            if let Some(ref dmh) = self.domain_message_header {
                if dmh.command() == ipc::DomainCommandType::CloseVirtualHandle {
                    return;
                }
            }
        }

        // Buffer descriptors C.
        let mut c_index = buffer_c_offset;
        self.buffer_c_descriptors.clear();
        let c_flags_raw = header.buf_c_descriptor_flags();
        if c_flags_raw as u32 > ipc::BufferDescriptorCFlag::InlineDescriptor as u32 {
            if c_flags_raw == ipc::BufferDescriptorCFlag::OneDescriptor {
                if c_index + 1 < ipc::COMMAND_BUFFER_LENGTH {
                    self.buffer_c_descriptors.push(ipc::BufferDescriptorC {
                        address_bits_0_31: self.cmd_buf[c_index],
                        raw_word1: self.cmd_buf[c_index + 1],
                    });
                }
            } else {
                // Number of C descriptors = flags - 2
                let raw_flags = bit_field_extract(header.raw_high, 10, 4);
                let num_c = raw_flags.wrapping_sub(2) as usize;
                if num_c < 14 {
                    for _ in 0..num_c {
                        if c_index + 1 < ipc::COMMAND_BUFFER_LENGTH {
                            self.buffer_c_descriptors.push(ipc::BufferDescriptorC {
                                address_bits_0_31: self.cmd_buf[c_index],
                                raw_word1: self.cmd_buf[c_index + 1],
                            });
                            c_index += 2;
                        }
                    }
                }
            }
        }

        // Parse the command ID from the data payload.
        if !header.is_tipc() {
            let dp_offset = self.data_payload_offset as usize;
            if dp_offset < ipc::COMMAND_BUFFER_LENGTH {
                self.command = self.cmd_buf[dp_offset];
                // Skip 1 more word (command is u64, high part unused).
            }
        }
    }

    /// Writes data from this context back to the requesting thread's TLS.
    ///
    /// Matches upstream `HLERequestContext::WriteToOutgoingCommandBuffer()`:
    /// 1. Translate outgoing copy objects → handles via handle_table.Add()
    /// 2. Translate outgoing move objects → handles (+ close original)
    /// 3. Write domain object IDs into the buffer
    /// 4. Write the command buffer back to guest TLS memory
    pub fn write_to_outgoing_command_buffer(&mut self) -> ResultCode {
        let mut current_offset = self.handles_offset as usize;

        // Translate outgoing copy objects to handles.
        // Upstream: handle_table.Add(object) for each, writes handle to cmd_buf.
        for obj_ref in &self.outgoing_copy_objects {
            if current_offset < ipc::COMMAND_BUFFER_LENGTH {
                let handle = obj_ref.resolve();
                self.cmd_buf[current_offset] = handle;
                current_offset += 1;
            }
        }

        // Translate outgoing move objects to handles.
        // Upstream: handle_table.Add(object) + object->Close() for each.
        for obj_ref in &self.outgoing_move_objects {
            if current_offset < ipc::COMMAND_BUFFER_LENGTH {
                let handle = obj_ref.resolve();
                self.cmd_buf[current_offset] = handle;
                current_offset += 1;
            }
        }

        // Write domain objects to the command buffer (after raw untranslated data).
        // Matches upstream: domain objects go at domain_offset - count.
        let is_domain = self
            .manager
            .as_ref()
            .map_or(false, |m| m.lock().unwrap().is_domain());

        if is_domain {
            let num_domain_objects = self.outgoing_domain_objects.len();
            current_offset = self.domain_offset as usize - num_domain_objects;

            let domain_objects = std::mem::take(&mut self.outgoing_domain_objects);
            for object in domain_objects {
                if current_offset < ipc::COMMAND_BUFFER_LENGTH {
                    // Matches upstream hle_ipc.cpp:301-307:
                    // if (object) { AppendDomainHandler; cmd_buf = count; }
                    // else { cmd_buf = 0; }
                    if let Some(handler) = object {
                        if let Some(manager) = &self.manager {
                            manager.lock().unwrap().append_domain_handler(handler);
                            let count = manager.lock().unwrap().domain_handler_count();
                            self.cmd_buf[current_offset] = count as u32;
                        } else {
                            self.cmd_buf[current_offset] = 0;
                        }
                    } else {
                        // Null domain object — write 0 as the domain object ID.
                        self.cmd_buf[current_offset] = 0;
                    }
                    current_offset += 1;
                }
            }
        }

        // Write the command buffer back to guest TLS memory.
        // Matches upstream: memory.WriteBlock(thread->GetTlsAddress(), cmd_buf.data(), write_size * sizeof(u32))
        let write_words = (self.write_size as usize).min(ipc::COMMAND_BUFFER_LENGTH);
        if let Some(ref memory) = self.memory {
            // Write via Memory bridge (upstream path).
            let m = memory.lock().unwrap();
            for i in 0..write_words {
                m.write_32(self.tls_address + (i as u64 * 4), self.cmd_buf[i]);
            }
        } else if let Some(ref mem) = self.shared_memory {
            // Fallback: write to ProcessMemoryData (test path or pre-Memory setup).
            let mut mem = mem.write().unwrap();
            for i in 0..write_words {
                mem.write_32(self.tls_address + (i as u64 * 4), self.cmd_buf[i]);
            }
        }

        RESULT_SUCCESS
    }

    /// Returns a description of the current IPC command for debugging.
    pub fn description(&self) -> String {
        match &self.command_header {
            None => "No command header available".to_string(),
            Some(header) => {
                format!(
                    "IPC::CommandHeader: Type:{}, X(Pointer):{}, A(Send):{}, B(Receive):{}, \
                     C(ReceiveList):{}, data_size:{}",
                    header.command_type_raw(),
                    header.num_buf_x_descriptors(),
                    header.num_buf_a_descriptors(),
                    header.num_buf_b_descriptors(),
                    self.buffer_c_descriptors.len(),
                    header.data_size(),
                )
            }
        }
    }
}

/// Helper to extract unsigned bits from a value. Mirrors common::bit_field::extract_unsigned.
fn bit_field_extract(value: u32, position: usize, bits: usize) -> u32 {
    (value >> position) & ((1u32 << bits) - 1)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hle::kernel::k_process::KProcess;
    use crate::hle::kernel::k_thread::KThread;

    #[test]
    fn test_session_request_manager_default() {
        let mgr = SessionRequestManager::new();
        assert!(!mgr.is_domain());
        assert!(!mgr.has_session_handler());
        assert!(!mgr.get_is_initialized_for_sm());
        assert_eq!(mgr.domain_handler_count(), 0);
    }

    #[test]
    fn test_hle_request_context_default() {
        let ctx = HLERequestContext::new();
        assert_eq!(ctx.cmd_buf[0], 0);
        assert!(!ctx.is_tipc());
        assert!(!ctx.has_domain_message_header());
        assert!(!ctx.get_is_deferred());
    }

    #[test]
    fn test_send_current_pid_uses_requesting_thread_process_id() {
        let process = Arc::new(Mutex::new(KProcess::new()));
        process.lock().unwrap().process_id = 0x51;

        let thread = Arc::new(Mutex::new(KThread::new()));
        thread.lock().unwrap().parent = Some(Arc::downgrade(&process));

        let shared_memory = process.lock().unwrap().get_shared_memory();
        let tls_address = 0x2000;
        {
            let mut mem = shared_memory.write().unwrap();
            mem.allocate(0x2000, 0x1000);
            mem.write_32(tls_address, ipc::CommandType::Request as u32);
            mem.write_32(tls_address + 4, 1u32 << 31);
            mem.write_32(tls_address + 8, 1);
            mem.write_32(tls_address + 12, 0);
            mem.write_32(tls_address + 16, 0);
        }

        let mut ctx = HLERequestContext::new_with_thread(thread, shared_memory, tls_address);
        ctx.populate_from_incoming_command_buffer(&[]);

        assert_eq!(ctx.get_pid(), 0x51);
    }

    #[test]
    fn write_to_outgoing_command_buffer_only_writes_write_size_words() {
        let process = Arc::new(Mutex::new(KProcess::new()));
        let thread = Arc::new(Mutex::new(KThread::new()));
        thread.lock().unwrap().parent = Some(Arc::downgrade(&process));

        let shared_memory = process.lock().unwrap().get_shared_memory();
        let tls_address = 0x3000;
        {
            let mut mem = shared_memory.write().unwrap();
            mem.allocate(tls_address, 0x1000);
            mem.write_32(tls_address + 16, 0xdead_beef);
        }

        let mut ctx = HLERequestContext::new_with_thread(thread, shared_memory.clone(), tls_address);
        ctx.write_size = 4;
        ctx.cmd_buf[0] = 0x1111_1111;
        ctx.cmd_buf[1] = 0x2222_2222;
        ctx.cmd_buf[2] = 0x3333_3333;
        ctx.cmd_buf[3] = 0x4444_4444;
        ctx.cmd_buf[4] = 0;

        assert_eq!(ctx.write_to_outgoing_command_buffer(), RESULT_SUCCESS);

        let mem = shared_memory.read().unwrap();
        assert_eq!(mem.read_32(tls_address), 0x1111_1111);
        assert_eq!(mem.read_32(tls_address + 4), 0x2222_2222);
        assert_eq!(mem.read_32(tls_address + 8), 0x3333_3333);
        assert_eq!(mem.read_32(tls_address + 12), 0x4444_4444);
        assert_eq!(mem.read_32(tls_address + 16), 0xdead_beef);
    }
}
