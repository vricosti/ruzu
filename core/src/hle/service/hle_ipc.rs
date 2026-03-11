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
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

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
        ""
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
}

impl SessionRequestManager {
    pub fn new() -> Self {
        Self {
            convert_to_domain: false,
            is_domain: false,
            is_initialized_for_sm: false,
            session_handler: None,
            domain_handlers: Vec::new(),
        }
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

    pub fn complete_sync_request(&mut self, context: &mut HLERequestContext) -> ResultCode {
        let mut result = RESULT_SUCCESS;

        if self.has_session_request_handler(context) {
            if self.is_domain() && context.has_domain_message_header() {
                result = self.handle_domain_sync_request(context);
            } else if self.has_session_handler() {
                if let Some(handler) = &self.session_handler {
                    result = handler.handle_sync_request(context);
                }
            }
        } else {
            log::error!("Session handler is invalid, stubbing response!");
            let mut rb = super::ipc_helpers::ResponseBuilder::new(context, 2, 0, 0);
            rb.push_result(RESULT_SUCCESS);
        }

        if self.convert_to_domain {
            assert!(
                !self.is_domain(),
                "ServerSession is already a domain instance."
            );
            self.convert_to_domain();
            self.convert_to_domain = false;
        }

        result
    }

    pub fn handle_domain_sync_request(&mut self, context: &mut HLERequestContext) -> ResultCode {
        if !context.has_domain_message_header() {
            return RESULT_SUCCESS;
        }

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
                    return RESULT_SUCCESS;
                }
                if let Some(Some(handler)) = self.domain_handlers.get(object_id - 1) {
                    handler.handle_sync_request(context)
                } else {
                    log::error!("Domain handler at index {} is null", object_id - 1);
                    RESULT_SUCCESS
                }
            }
            ipc::DomainCommandType::CloseVirtualHandle => {
                log::debug!("CloseVirtualHandle, object_id=0x{:08X}", object_id);
                self.close_domain_handler(object_id - 1);

                let mut rb = super::ipc_helpers::ResponseBuilder::new(context, 2, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                RESULT_SUCCESS
            }
        }
    }

    pub fn get_is_initialized_for_sm(&self) -> bool {
        self.is_initialized_for_sm
    }

    pub fn set_is_initialized_for_sm(&mut self) {
        self.is_initialized_for_sm = true;
    }
}

/// Class containing information about an in-flight IPC request being handled by an HLE service
/// implementation.
///
/// Corresponds to upstream `HLERequestContext`.
pub struct HLERequestContext {
    /// IPC command buffer.
    pub cmd_buf: [u32; ipc::COMMAND_BUFFER_LENGTH],

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

    outgoing_move_objects: Vec<Handle>,
    outgoing_copy_objects: Vec<Handle>,
    outgoing_domain_objects: Vec<SessionRequestHandlerPtr>,

    command: u32,
    pid: u64,
    pub write_size: u32,
    pub data_payload_offset: u32,
    pub handles_offset: u32,
    pub domain_offset: u32,

    manager: Option<Arc<Mutex<SessionRequestManager>>>,
    is_deferred: bool,
}

impl HLERequestContext {
    pub fn new() -> Self {
        let mut ctx = Self {
            cmd_buf: [0u32; ipc::COMMAND_BUFFER_LENGTH],
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
            is_deferred: false,
        };
        ctx.cmd_buf[0] = 0;
        ctx
    }

    /// Returns a pointer to the IPC command buffer for this request.
    pub fn command_buffer(&self) -> &[u32; ipc::COMMAND_BUFFER_LENGTH] {
        &self.cmd_buf
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

    pub fn add_move_object(&mut self, handle: Handle) {
        self.outgoing_move_objects.push(handle);
    }

    pub fn add_copy_object(&mut self, handle: Handle) {
        self.outgoing_copy_objects.push(handle);
    }

    pub fn add_domain_object(&mut self, object: SessionRequestHandlerPtr) {
        self.outgoing_domain_objects.push(object);
    }

    pub fn set_session_request_manager(&mut self, manager: Arc<Mutex<SessionRequestManager>>) {
        self.manager = Some(manager);
    }

    pub fn get_manager(&self) -> Option<&Arc<Mutex<SessionRequestManager>>> {
        self.manager.as_ref()
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

    /// Populates this context with data from the incoming command buffer.
    /// Simplified version: copies the raw command buffer and parses the header.
    pub fn populate_from_incoming_command_buffer(&mut self, src_cmdbuf: &[u32]) {
        let len = src_cmdbuf.len().min(ipc::COMMAND_BUFFER_LENGTH);
        self.cmd_buf[..len].copy_from_slice(&src_cmdbuf[..len]);
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
                // Skip 2 words for PID
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

    /// Writes data from this context back to the requesting process/thread.
    /// In the full implementation this would write back to guest memory.
    /// For now, this is a stub.
    pub fn write_to_outgoing_command_buffer(&self) -> ResultCode {
        // TODO: implement full write-back when kernel integration is ready
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
}
