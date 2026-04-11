//! Port of zuyu/src/core/hle/kernel/k_server_session.h / k_server_session.cpp
//! Status: Partial (structural port)
//!
//! KServerSession: the server endpoint of a session, handles incoming requests.
//!
//! In upstream, the ServerManager links a KServerSession to a SessionRequestManager
//! via RegisterSession(). When a client sends a request, the server session
//! dispatches to the handler via the manager.

use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

use crate::hle::kernel::k_memory_block::{KMemoryPermission, KMemoryState, PAGE_SIZE};
use crate::hle::kernel::k_session_request::KSessionRequest;
use crate::hle::kernel::k_synchronization_object;
use crate::hle::kernel::k_synchronization_object::SynchronizationObjectState;
use crate::hle::kernel::k_typed_address::KProcessAddress;
use crate::hle::kernel::message_buffer::{MessageBuffer, MESSAGE_BUFFER_SIZE};
use crate::hle::kernel::svc::svc_results::{
    RESULT_INVALID_COMBINATION, RESULT_INVALID_STATE, RESULT_MESSAGE_TOO_LARGE, RESULT_NOT_FOUND,
    RESULT_RECEIVE_LIST_BROKEN, RESULT_SESSION_CLOSED,
};
use crate::hle::kernel::svc_common::INVALID_HANDLE;
use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestManager};

const POINTER_TRANSFER_BUFFER_ALIGNMENT: usize = 0x10;

struct ReceiveList {
    recv_list_count: u32,
    data: Vec<u32>,
    msg_buffer_end: u64,
    msg_buffer_space_end: u64,
}

impl ReceiveList {
    fn get_entry_count(header: &crate::hle::kernel::message_buffer::MessageHeader) -> usize {
        match header.get_receive_list_count() {
            x if x == crate::hle::kernel::message_buffer::ReceiveListCountType::None as u32 => 0,
            x if x
                == crate::hle::kernel::message_buffer::ReceiveListCountType::ToMessageBuffer
                    as u32 =>
            {
                0
            }
            x if x
                == crate::hle::kernel::message_buffer::ReceiveListCountType::ToSingleBuffer
                    as u32 =>
            {
                1
            }
            x => {
                (x - crate::hle::kernel::message_buffer::RECEIVE_LIST_COUNT_TYPE_COUNT_OFFSET)
                    as usize
            }
        }
    }

    fn new(
        dst_words: &[u32],
        dst_address: u64,
        dst_header: &crate::hle::kernel::message_buffer::MessageHeader,
        dst_special_header: &crate::hle::kernel::message_buffer::SpecialHeader,
        msg_size: usize,
        out_offset: usize,
    ) -> Self {
        let recv_list_count = dst_header.get_receive_list_count();
        let msg_buffer_end = dst_address + (std::mem::size_of::<u32>() * out_offset) as u64;
        let msg_buffer_space_end = dst_address + msg_size as u64;
        let recv_list_index =
            crate::hle::kernel::message_buffer::MessageBuffer::get_receive_list_index(
                dst_header,
                dst_special_header,
            );
        let entry_words = Self::get_entry_count(dst_header)
            * (crate::hle::kernel::message_buffer::ReceiveListEntry::DATA_SIZE
                / std::mem::size_of::<u32>());
        let data = if entry_words == 0 {
            Vec::new()
        } else {
            dst_words[recv_list_index..recv_list_index + entry_words].to_vec()
        };
        Self {
            recv_list_count,
            data,
            msg_buffer_end,
            msg_buffer_space_end,
        }
    }

    fn is_index(&self) -> bool {
        self.recv_list_count
            > crate::hle::kernel::message_buffer::RECEIVE_LIST_COUNT_TYPE_COUNT_OFFSET
    }

    fn is_to_message_buffer(&self) -> bool {
        self.recv_list_count
            == crate::hle::kernel::message_buffer::ReceiveListCountType::ToMessageBuffer as u32
    }

    fn get_buffer(&self, size: usize, key: &mut i32) -> u64 {
        match self.recv_list_count {
            x if x == crate::hle::kernel::message_buffer::ReceiveListCountType::None as u32 => 0,
            x if x
                == crate::hle::kernel::message_buffer::ReceiveListCountType::ToMessageBuffer
                    as u32 =>
            {
                let buf = (self.msg_buffer_end + (*key as u64))
                    .next_multiple_of(POINTER_TRANSFER_BUFFER_ALIGNMENT as u64);
                if buf < buf.saturating_add(size as u64)
                    && buf.saturating_add(size as u64) <= self.msg_buffer_space_end
                {
                    *key = (buf + size as u64 - self.msg_buffer_end) as i32;
                    buf
                } else {
                    0
                }
            }
            x if x
                == crate::hle::kernel::message_buffer::ReceiveListCountType::ToSingleBuffer
                    as u32 =>
            {
                let entry = crate::hle::kernel::message_buffer::ReceiveListEntry::from_raw(
                    self.data[0],
                    self.data[1],
                );
                let buf = (entry.get_address() + (*key as u64))
                    .next_multiple_of(POINTER_TRANSFER_BUFFER_ALIGNMENT as u64);
                let entry_addr = entry.get_address();
                let entry_size = entry.get_size() as u64;
                if buf < buf.saturating_add(size as u64)
                    && entry_addr < entry_addr.saturating_add(entry_size)
                    && buf.saturating_add(size as u64) <= entry_addr.saturating_add(entry_size)
                {
                    *key = (buf + size as u64 - entry_addr) as i32;
                    buf
                } else {
                    0
                }
            }
            _ => {
                let index = *key as usize;
                let max_entries = (self.recv_list_count
                    - crate::hle::kernel::message_buffer::RECEIVE_LIST_COUNT_TYPE_COUNT_OFFSET)
                    as usize;
                if index >= max_entries {
                    return 0;
                }
                let entry = crate::hle::kernel::message_buffer::ReceiveListEntry::from_raw(
                    self.data[index * 2],
                    self.data[index * 2 + 1],
                );
                if entry.get_address() < entry.get_address().saturating_add(entry.get_size() as u64)
                    && entry.get_size() >= size
                {
                    entry.get_address()
                } else {
                    0
                }
            }
        }
    }
}

/// The server session object.
/// Matches upstream `KServerSession` class (k_server_session.h).
pub struct KServerSession {
    /// Parent KSession ID.
    pub parent_id: Option<u64>,
    /// Pending request list (upstream: intrusive list of KSessionRequest).
    pub request_list: VecDeque<Arc<Mutex<KSessionRequest>>>,
    /// Currently being processed request.
    pub current_request: Option<Arc<Mutex<KSessionRequest>>>,
    /// Whether the client side of the parent session has been closed.
    ///
    /// Upstream reaches this via `m_parent->IsClientClosed()` because
    /// `KServerSession` owns an inline parent pointer. Rust stores the closure
    /// state locally to avoid re-entering process/session registries from
    /// `is_signaled()`, which is called while higher-level wait code may
    /// already hold process/session locks.
    pub client_closed: bool,
    /// The session request manager linked via ServerManager::RegisterSession.
    /// Matches upstream where ServerManager stores the Session wrapper that
    /// pairs the KServerSession with a SessionRequestManager.
    pub manager: Option<Arc<Mutex<SessionRequestManager>>>,
    /// Waitable synchronization state owned by KSynchronizationObject upstream.
    pub sync_object: SynchronizationObjectState,
}

impl KServerSession {
    fn get_map_alias_memory_state(attribute: u32) -> Option<KMemoryState> {
        match attribute {
            x if x == crate::hle::kernel::message_buffer::MapAliasAttribute::Ipc as u32 => {
                Some(KMemoryState::IPC)
            }
            x if x
                == crate::hle::kernel::message_buffer::MapAliasAttribute::NonSecureIpc as u32 =>
            {
                Some(KMemoryState::NON_SECURE_IPC)
            }
            x if x
                == crate::hle::kernel::message_buffer::MapAliasAttribute::NonDeviceIpc as u32 =>
            {
                Some(KMemoryState::NON_DEVICE_IPC)
            }
            _ => None,
        }
    }

    fn encode_map_alias_descriptor(address: u64, size: usize, attribute: u32) -> [u32; 3] {
        [
            size as u32,
            address as u32,
            (attribute & 0x3)
                | ((((address >> 36) & 0x7) as u32) << 2)
                | ((((size as u64 >> 32) & 0xF) as u32) << 24)
                | ((((address >> 32) & 0xF) as u32) << 28),
        ]
    }

    fn copy_between_processes(
        src_process: &crate::hle::kernel::k_process::KProcess,
        src_address: usize,
        dst_process: &mut crate::hle::kernel::k_process::KProcess,
        dst_address: usize,
        size: usize,
    ) {
        let bytes = if let Some(memory) = src_process.get_memory() {
            let mut bytes = vec![0u8; size];
            memory
                .lock()
                .unwrap()
                .read_block(src_address as u64, &mut bytes);
            bytes
        } else {
            src_process
                .process_memory
                .read()
                .unwrap()
                .read_bytes(src_address as u64, size)
        };
        if let Some(memory) = dst_process.get_memory() {
            memory
                .lock()
                .unwrap()
                .write_block(dst_address as u64, &bytes);
        } else {
            dst_process
                .process_memory
                .write()
                .unwrap()
                .write_block(dst_address as u64, &bytes);
        }
    }

    fn process_receive_message_pointer_descriptors(
        dst_words: &mut [u32],
        src_words: &[u32],
        src_header: &crate::hle::kernel::message_buffer::MessageHeader,
        src_special_header: &crate::hle::kernel::message_buffer::SpecialHeader,
        dst_header: &crate::hle::kernel::message_buffer::MessageHeader,
        dst_special_header: &crate::hle::kernel::message_buffer::SpecialHeader,
        dst_address: usize,
        dst_size: usize,
        dst_process: &mut crate::hle::kernel::k_process::KProcess,
        src_process: &crate::hle::kernel::k_process::KProcess,
    ) -> u32 {
        let mut src_words = src_words.to_vec();
        let src_message = MessageBuffer::new(&mut src_words);
        let src_end_offset = crate::hle::kernel::message_buffer::MessageBuffer::get_raw_data_index(
            src_header,
            src_special_header,
        ) + src_header.get_raw_count() as usize;
        let dst_recv_list = ReceiveList::new(
            dst_words,
            dst_address as u64,
            dst_header,
            dst_special_header,
            dst_size,
            src_end_offset,
        );
        let mut dst_message = MessageBuffer::new(dst_words);
        let mut pointer_key = 0;
        let mut offset =
            crate::hle::kernel::message_buffer::MessageBuffer::get_pointer_descriptor_index(
                src_header,
                src_special_header,
            );

        for _ in 0..src_header.get_pointer_count() {
            let cur_offset = offset;
            let src_desc = crate::hle::kernel::message_buffer::PointerDescriptor::from_raw([
                src_words[cur_offset],
                src_words[cur_offset + 1],
            ]);
            offset += crate::hle::kernel::message_buffer::PointerDescriptor::DATA_SIZE
                / std::mem::size_of::<u32>();

            let recv_size = src_desc.get_size();
            let mut recv_pointer = 0u64;
            if recv_size > 0 {
                if dst_recv_list.is_index() {
                    pointer_key = src_desc.get_index();
                }
                recv_pointer = dst_recv_list.get_buffer(recv_size, &mut pointer_key);
                if recv_pointer == 0 {
                    return crate::hle::kernel::svc::svc_results::RESULT_OUT_OF_RESOURCE
                        .get_inner_value();
                }
                Self::copy_between_processes(
                    src_process,
                    src_desc.get_address() as usize,
                    dst_process,
                    recv_pointer as usize,
                    recv_size,
                );
            }

            dst_message.set(
                cur_offset,
                &[
                    ((src_desc.get_index() as u32) & 0xF)
                        | ((((recv_pointer >> 32) & 0xF) as u32) << 12)
                        | (((recv_size as u32) & 0xFFFF) << 16)
                        | ((((recv_pointer >> 36) & 0x7) as u32) << 6),
                    recv_pointer as u32,
                ],
            );
        }

        crate::hle::result::RESULT_SUCCESS.get_inner_value()
    }

    fn process_send_message_pointer_descriptors(
        dst_words: &mut [u32],
        src_words: &[u32],
        src_header: &crate::hle::kernel::message_buffer::MessageHeader,
        src_special_header: &crate::hle::kernel::message_buffer::SpecialHeader,
        dst_header: &crate::hle::kernel::message_buffer::MessageHeader,
        dst_special_header: &crate::hle::kernel::message_buffer::SpecialHeader,
        dst_address: usize,
        dst_size: usize,
        dst_process: &mut crate::hle::kernel::k_process::KProcess,
        src_process: &crate::hle::kernel::k_process::KProcess,
    ) -> u32 {
        let mut src_words = src_words.to_vec();
        let src_message = MessageBuffer::new(&mut src_words);
        let src_end_offset = crate::hle::kernel::message_buffer::MessageBuffer::get_raw_data_index(
            src_header,
            src_special_header,
        ) + src_header.get_raw_count() as usize;
        let dst_recv_list = ReceiveList::new(
            dst_words,
            dst_address as u64,
            dst_header,
            dst_special_header,
            dst_size,
            src_end_offset,
        );
        let mut dst_message = MessageBuffer::new(dst_words);
        let mut pointer_key = 0;
        let mut offset =
            crate::hle::kernel::message_buffer::MessageBuffer::get_pointer_descriptor_index(
                src_header,
                src_special_header,
            );

        for _ in 0..src_header.get_pointer_count() {
            let cur_offset = offset;
            let src_desc = crate::hle::kernel::message_buffer::PointerDescriptor::from_raw([
                src_words[cur_offset],
                src_words[cur_offset + 1],
            ]);
            offset += crate::hle::kernel::message_buffer::PointerDescriptor::DATA_SIZE
                / std::mem::size_of::<u32>();

            let recv_size = src_desc.get_size();
            let mut recv_pointer = 0u64;
            if recv_size > 0 {
                if dst_recv_list.is_index() {
                    pointer_key = src_desc.get_index();
                }
                recv_pointer = dst_recv_list.get_buffer(recv_size, &mut pointer_key);
                if recv_pointer == 0 {
                    return crate::hle::kernel::svc::svc_results::RESULT_OUT_OF_RESOURCE
                        .get_inner_value();
                }
                Self::copy_between_processes(
                    src_process,
                    src_desc.get_address() as usize,
                    dst_process,
                    recv_pointer as usize,
                    recv_size,
                );
            }

            dst_message.set(
                cur_offset,
                &[
                    ((src_desc.get_index() as u32) & 0xF)
                        | ((((recv_pointer >> 32) & 0xF) as u32) << 12)
                        | (((recv_size as u32) & 0xFFFF) << 16)
                        | ((((recv_pointer >> 36) & 0x7) as u32) << 6),
                    recv_pointer as u32,
                ],
            );
        }

        crate::hle::result::RESULT_SUCCESS.get_inner_value()
    }

    fn process_receive_message_map_alias_descriptors(
        dst_words: &mut [u32],
        src_words: &[u32],
        src_header: &crate::hle::kernel::message_buffer::MessageHeader,
        src_special_header: &crate::hle::kernel::message_buffer::SpecialHeader,
        request: &mut KSessionRequest,
        dst_process: &mut crate::hle::kernel::k_process::KProcess,
        src_process: &mut crate::hle::kernel::k_process::KProcess,
    ) -> u32 {
        let mut dst_message = MessageBuffer::new(dst_words);
        let map_alias_index =
            crate::hle::kernel::message_buffer::MessageBuffer::get_map_alias_descriptor_index(
                src_header,
                src_special_header,
            );

        for i in 0..src_header.get_map_alias_count() {
            let cur_offset = map_alias_index
                + i as usize
                    * (crate::hle::kernel::message_buffer::MapAliasDescriptor::DATA_SIZE
                        / std::mem::size_of::<u32>());
            let src_desc = crate::hle::kernel::message_buffer::MapAliasDescriptor::from_raw([
                src_words[cur_offset],
                src_words[cur_offset + 1],
                src_words[cur_offset + 2],
            ]);
            let src_address = KProcessAddress::new(src_desc.get_address());
            let size = src_desc.get_size() as usize;
            let Some(dst_state) = Self::get_map_alias_memory_state(src_desc.get_attribute()) else {
                return RESULT_INVALID_COMBINATION.get_inner_value();
            };
            let perm = if i >= src_header.get_send_count() {
                KMemoryPermission::USER_READ_WRITE
            } else {
                KMemoryPermission::USER_READ
            };
            let send = i < src_header.get_send_count()
                || i >= src_header.get_send_count() + src_header.get_receive_count();

            let mut dst_address = KProcessAddress::new(0);
            let rc = dst_process.page_table.setup_for_ipc(
                &mut dst_address,
                size,
                src_address,
                &mut src_process.page_table,
                perm,
                dst_state,
                send,
            );
            if rc != crate::hle::result::RESULT_SUCCESS.get_inner_value() {
                return rc;
            }

            let push_rc = if perm == KMemoryPermission::USER_READ {
                request
                    .mappings
                    .push_send(src_address, dst_address, size, dst_state)
            } else if send {
                request
                    .mappings
                    .push_exchange(src_address, dst_address, size, dst_state)
            } else {
                request
                    .mappings
                    .push_receive(src_address, dst_address, size, dst_state)
            };
            if push_rc != crate::hle::result::RESULT_SUCCESS.get_inner_value() {
                if size > 0 {
                    let _ =
                        dst_process
                            .page_table
                            .cleanup_for_ipc_server(dst_address, size, dst_state);
                    let _ =
                        src_process
                            .page_table
                            .cleanup_for_ipc_client(src_address, size, dst_state);
                }
                return push_rc;
            }

            dst_message.set(
                cur_offset,
                &Self::encode_map_alias_descriptor(
                    dst_address.get(),
                    size,
                    src_desc.get_attribute(),
                ),
            );
        }

        crate::hle::result::RESULT_SUCCESS.get_inner_value()
    }

    fn process_send_message_receive_mapping(
        src_process: &crate::hle::kernel::k_process::KProcess,
        dst_process: &mut crate::hle::kernel::k_process::KProcess,
        client_address: KProcessAddress,
        server_address: KProcessAddress,
        size: usize,
        _src_state: KMemoryState,
    ) {
        if size == 0 {
            return;
        }

        let client_address = client_address.get();
        let server_address = server_address.get();
        let aligned_dst_start = client_address & !((PAGE_SIZE as u64) - 1);
        let aligned_dst_end = (client_address + size as u64).next_multiple_of(PAGE_SIZE as u64);
        let mapping_dst_start = client_address.next_multiple_of(PAGE_SIZE as u64);
        let mapping_dst_end = (client_address + size as u64) & !((PAGE_SIZE as u64) - 1);
        let mapping_src_end = (server_address + size as u64) & !((PAGE_SIZE as u64) - 1);

        if aligned_dst_start != mapping_dst_start {
            debug_assert!(client_address < mapping_dst_start);
            let copy_size = std::cmp::min(size, (mapping_dst_start - client_address) as usize);
            Self::copy_between_processes(
                src_process,
                server_address as usize,
                dst_process,
                client_address as usize,
                copy_size,
            );
        }

        if mapping_dst_end < aligned_dst_end
            && (aligned_dst_start == mapping_dst_start || aligned_dst_start < mapping_dst_end)
        {
            let copy_size = (client_address + size as u64 - mapping_dst_end) as usize;
            Self::copy_between_processes(
                src_process,
                mapping_src_end as usize,
                dst_process,
                mapping_dst_end as usize,
                copy_size,
            );
        }
    }

    fn process_send_message_receive_mappings(
        request: &KSessionRequest,
        src_process: &crate::hle::kernel::k_process::KProcess,
        dst_process: &mut crate::hle::kernel::k_process::KProcess,
    ) {
        for index in 0..request.mappings.get_receive_count() {
            let mapping = request.mappings.get_receive_mapping(index);
            Self::process_send_message_receive_mapping(
                src_process,
                dst_process,
                mapping.client_address,
                mapping.server_address,
                mapping.size,
                mapping.state,
            );
        }
        for index in 0..request.mappings.get_exchange_count() {
            let mapping = request.mappings.get_exchange_mapping(index);
            Self::process_send_message_receive_mapping(
                src_process,
                dst_process,
                mapping.client_address,
                mapping.server_address,
                mapping.size,
                mapping.state,
            );
        }
    }

    fn cleanup_special_data(
        dst_process: &mut crate::hle::kernel::k_process::KProcess,
        dst_words: &mut [u32],
        dst_buffer_size: usize,
    ) {
        if dst_words.len() < 2 {
            return;
        }

        let header = crate::hle::kernel::message_buffer::MessageHeader::from_raw([
            dst_words[0],
            dst_words[1],
        ]);
        let special_header = if header.get_has_special_header() && dst_words.len() > 2 {
            crate::hle::kernel::message_buffer::SpecialHeader::new_with_flag(
                (dst_words[2] & 1) != 0,
                ((dst_words[2] >> 1) & 0xF) as i32,
                ((dst_words[2] >> 5) & 0xF) as i32,
                true,
            )
        } else {
            crate::hle::kernel::message_buffer::SpecialHeader::new_with_flag(false, 0, 0, false)
        };

        if crate::hle::kernel::message_buffer::MessageBuffer::get_message_buffer_size(
            &header,
            &special_header,
        ) > dst_buffer_size
        {
            return;
        }

        if !header.get_has_special_header() {
            return;
        }

        let mut message = MessageBuffer::new(dst_words);
        let mut offset = crate::hle::kernel::message_buffer::MessageBuffer::get_special_data_index(
            &special_header,
        );
        if special_header.get_has_process_id() {
            offset = message.set_process_id(offset, 0);
        }
        let handle_count =
            special_header.get_copy_handle_count() + special_header.get_move_handle_count();
        for _ in 0..handle_count {
            let handle = message.get_handle(offset);
            if handle != INVALID_HANDLE {
                dst_process.handle_table.remove(handle);
            }
            offset = message.set_handle(offset, INVALID_HANDLE);
        }
    }

    fn restore_destination_header(
        dst_words: &mut [u32],
        dst_header: &crate::hle::kernel::message_buffer::MessageHeader,
        dst_special_header: &crate::hle::kernel::message_buffer::SpecialHeader,
    ) {
        if dst_words.len() < 2 {
            return;
        }
        dst_words[0] = dst_header.get_data()[0];
        dst_words[1] = dst_header.get_data()[1];
        if dst_header.get_has_special_header() && dst_words.len() > 2 {
            dst_words[2] = (dst_special_header.get_has_process_id() as u32)
                | ((dst_special_header.get_copy_handle_count() as u32) << 1)
                | ((dst_special_header.get_move_handle_count() as u32) << 5);
        }
    }

    fn set_message_header_and_special_header(
        dst_words: &mut [u32],
        src_header: &crate::hle::kernel::message_buffer::MessageHeader,
        src_special_header: &crate::hle::kernel::message_buffer::SpecialHeader,
    ) {
        if dst_words.len() < 2 {
            return;
        }

        let mut message = MessageBuffer::new(dst_words);
        let _ = message.set_message_header(src_header);
        if src_header.get_has_special_header() && message.get_buffer_size() > 2 {
            let _ = message.set(
                2,
                &[src_special_header.get_has_process_id() as u32
                    | ((src_special_header.get_copy_handle_count() as u32) << 1)
                    | ((src_special_header.get_move_handle_count() as u32) << 5)],
            );
        }
    }

    fn mark_receive_list_broken(current_end_offset: usize, dst_recv_list_idx: usize) -> bool {
        current_end_offset > dst_recv_list_idx
    }

    fn process_message_special_data(
        dst_words: &mut [u32],
        src_words: &[u32],
        src_header: &crate::hle::kernel::message_buffer::MessageHeader,
        src_special_header: &crate::hle::kernel::message_buffer::SpecialHeader,
        dst_process: &mut crate::hle::kernel::k_process::KProcess,
        src_process: &mut crate::hle::kernel::k_process::KProcess,
        move_handle_allowed: bool,
    ) -> u32 {
        if !src_header.get_has_special_header() {
            return crate::hle::result::RESULT_SUCCESS.get_inner_value();
        }

        let mut src_words = src_words.to_vec();
        let src_message = MessageBuffer::new(&mut src_words);
        let mut dst_message = MessageBuffer::new(dst_words);
        let mut offset = crate::hle::kernel::message_buffer::MessageBuffer::get_special_data_index(
            src_special_header,
        );

        if src_special_header.get_has_process_id() {
            offset = dst_message.set_process_id(offset, src_process.process_id);
        }

        for _ in 0..src_special_header.get_copy_handle_count() {
            let src_handle = src_message.get_handle(offset);
            let dst_handle = if src_handle != INVALID_HANDLE {
                let Some(object_id) = src_process.handle_table.get_object(src_handle) else {
                    return crate::hle::kernel::svc::svc_results::RESULT_INVALID_HANDLE
                        .get_inner_value();
                };
                match dst_process.handle_table.add(object_id) {
                    Ok(handle) => handle,
                    Err(rc) => return rc,
                }
            } else {
                INVALID_HANDLE
            };
            offset = dst_message.set_handle(offset, dst_handle);
        }

        if src_special_header.get_move_handle_count() != 0 && !move_handle_allowed {
            return RESULT_INVALID_COMBINATION.get_inner_value();
        }

        for _ in 0..src_special_header.get_move_handle_count() {
            let src_handle = src_message.get_handle(offset);
            let dst_handle = if src_handle != INVALID_HANDLE {
                let Some(object_id) = src_process.handle_table.get_object(src_handle) else {
                    return crate::hle::kernel::svc::svc_results::RESULT_INVALID_HANDLE
                        .get_inner_value();
                };
                let dst_handle = match dst_process.handle_table.add(object_id) {
                    Ok(handle) => handle,
                    Err(rc) => return rc,
                };
                src_process.handle_table.remove(src_handle);
                dst_handle
            } else {
                INVALID_HANDLE
            };
            offset = dst_message.set_handle(offset, dst_handle);
        }

        crate::hle::result::RESULT_SUCCESS.get_inner_value()
    }

    fn message_words_from_process(
        process: &crate::hle::kernel::k_process::KProcess,
        message_address: usize,
        buffer_size: usize,
    ) -> Vec<u32> {
        let bytes = if let Some(memory) = process.get_memory() {
            let mut bytes = vec![0u8; buffer_size];
            memory
                .lock()
                .unwrap()
                .read_block(message_address as u64, &mut bytes);
            bytes
        } else {
            let memory = process.process_memory.read().unwrap();
            memory.read_bytes(message_address as u64, buffer_size)
        };

        let mut words = vec![0u32; buffer_size / std::mem::size_of::<u32>()];
        for (index, chunk) in bytes.chunks_exact(4).enumerate() {
            words[index] = u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
        }
        words
    }

    fn write_message_words_to_process(
        process: &mut crate::hle::kernel::k_process::KProcess,
        message_address: usize,
        words: &[u32],
        byte_size: usize,
    ) {
        let bytes: Vec<u8> = words[..byte_size / std::mem::size_of::<u32>()]
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();

        if let Some(memory) = process.get_memory() {
            memory
                .lock()
                .unwrap()
                .write_block(message_address as u64, &bytes);
        } else {
            process
                .process_memory
                .write()
                .unwrap()
                .write_block(message_address as u64, &bytes);
        }
    }

    fn parse_message_headers(
        words: &[u32],
    ) -> (
        crate::hle::kernel::message_buffer::MessageHeader,
        crate::hle::kernel::message_buffer::SpecialHeader,
    ) {
        let header =
            crate::hle::kernel::message_buffer::MessageHeader::from_raw([words[0], words[1]]);
        let special_header = if header.get_has_special_header() && words.len() > 2 {
            crate::hle::kernel::message_buffer::SpecialHeader::new_with_flag(
                (words[2] & 1) != 0,
                ((words[2] >> 1) & 0xF) as i32,
                ((words[2] >> 5) & 0xF) as i32,
                true,
            )
        } else {
            crate::hle::kernel::message_buffer::SpecialHeader::new_with_flag(false, 0, 0, false)
        };
        (header, special_header)
    }

    fn request_message_address_and_size(
        thread: &crate::hle::kernel::k_thread::KThread,
        requested_message: usize,
        requested_size: usize,
    ) -> (usize, usize) {
        if requested_message != 0 {
            (requested_message, requested_size)
        } else {
            (
                thread.get_tls_address().get() as usize,
                MESSAGE_BUFFER_SIZE * std::mem::size_of::<u32>(),
            )
        }
    }

    fn try_receive_message_raw(
        server_message: usize,
        server_buffer_size: usize,
        request: &Arc<Mutex<KSessionRequest>>,
    ) -> Option<(u32, bool)> {
        let current_thread = crate::hle::kernel::kernel::get_current_thread_pointer()?;
        let server_process = {
            let thread = current_thread.lock().unwrap();
            thread.parent.as_ref()?.upgrade()?
        };
        let client_thread = Self::resolve_request_client_thread(request)?;
        let client_process = {
            let thread = client_thread.lock().unwrap();
            thread.parent.as_ref()?.upgrade()?
        };

        let (dst_address, dst_size) = {
            let thread = current_thread.lock().unwrap();
            Self::request_message_address_and_size(&thread, server_message, server_buffer_size)
        };
        let (src_address, src_size) = {
            let request = request.lock().unwrap();
            if request.get_address() != 0 {
                (request.get_address(), request.get_size())
            } else {
                let thread = client_thread.lock().unwrap();
                Self::request_message_address_and_size(&thread, 0, 0)
            }
        };

        let src_words = {
            let process = client_process.lock().unwrap();
            Self::message_words_from_process(&process, src_address, src_size)
        };
        let dst_word_capacity = dst_size / std::mem::size_of::<u32>();
        let original_dst_words = {
            let process = server_process.lock().unwrap();
            Self::message_words_from_process(&process, dst_address, dst_size)
        };
        let (src_header, src_special_header) = Self::parse_message_headers(&src_words);
        let (dst_header, dst_special_header) = Self::parse_message_headers(&original_dst_words);

        let dst_recv_list_idx =
            crate::hle::kernel::message_buffer::MessageBuffer::get_receive_list_index(
                &dst_header,
                &dst_special_header,
            );
        if crate::hle::kernel::message_buffer::MessageBuffer::get_message_buffer_size(
            &src_header,
            &src_special_header,
        ) > src_size
        {
            return Some((RESULT_INVALID_COMBINATION.get_inner_value(), false));
        }
        if crate::hle::kernel::message_buffer::MessageBuffer::get_message_buffer_size(
            &dst_header,
            &dst_special_header,
        ) > dst_size
        {
            return Some((RESULT_INVALID_COMBINATION.get_inner_value(), false));
        }
        if src_header.get_has_special_header() && src_special_header.get_move_handle_count() != 0 {
            return Some((RESULT_INVALID_COMBINATION.get_inner_value(), false));
        }
        if dst_header.get_receive_list_offset() != 0
            && (dst_header.get_receive_list_offset() as usize)
                < crate::hle::kernel::message_buffer::MessageBuffer::get_raw_data_index(
                    &dst_header,
                    &dst_special_header,
                ) + dst_header.get_raw_count() as usize
        {
            return Some((RESULT_INVALID_COMBINATION.get_inner_value(), false));
        }
        let src_end_offset = crate::hle::kernel::message_buffer::MessageBuffer::get_raw_data_index(
            &src_header,
            &src_special_header,
        ) + src_header.get_raw_count() as usize;
        let copy_size = src_end_offset * std::mem::size_of::<u32>();
        if dst_size < copy_size {
            return Some((RESULT_MESSAGE_TOO_LARGE.get_inner_value(), false));
        }
        let mut recv_list_broken = false;

        let mut dst_words = original_dst_words;
        if dst_words.len() < dst_word_capacity {
            dst_words.resize(dst_word_capacity, 0);
        }
        Self::set_message_header_and_special_header(
            &mut dst_words,
            &src_header,
            &src_special_header,
        );
        let mut server_process = server_process.lock().unwrap();
        let mut client_process = client_process.lock().unwrap();
        let processed_special_data = src_header.get_has_special_header();
        let special_result = Self::process_message_special_data(
            &mut dst_words,
            &src_words,
            &src_header,
            &src_special_header,
            &mut server_process,
            &mut client_process,
            false,
        );
        if processed_special_data {
            recv_list_broken = Self::mark_receive_list_broken(
                crate::hle::kernel::message_buffer::MessageBuffer::get_pointer_descriptor_index(
                    &src_header,
                    &src_special_header,
                ),
                dst_recv_list_idx,
            );
        }
        if special_result != crate::hle::result::RESULT_SUCCESS.get_inner_value() {
            if processed_special_data {
                Self::cleanup_special_data(&mut server_process, &mut dst_words, dst_size);
            }
            if !recv_list_broken {
                Self::restore_destination_header(&mut dst_words, &dst_header, &dst_special_header);
            }
            let _ = Self::cleanup_server_map(request, Some(&mut server_process));
            let _ = Self::cleanup_client_map(request, Some(&mut client_process));
            return Some((special_result, recv_list_broken));
        }
        let pointer_result = Self::process_receive_message_pointer_descriptors(
            &mut dst_words,
            &src_words,
            &src_header,
            &src_special_header,
            &dst_header,
            &dst_special_header,
            dst_address,
            dst_size,
            &mut server_process,
            &client_process,
        );
        if src_header.get_pointer_count() > 0 {
            recv_list_broken = Self::mark_receive_list_broken(
                crate::hle::kernel::message_buffer::MessageBuffer::get_map_alias_descriptor_index(
                    &src_header,
                    &src_special_header,
                ),
                dst_recv_list_idx,
            );
        }
        if pointer_result != crate::hle::result::RESULT_SUCCESS.get_inner_value() {
            if processed_special_data {
                Self::cleanup_special_data(&mut server_process, &mut dst_words, dst_size);
            }
            if !recv_list_broken {
                Self::restore_destination_header(&mut dst_words, &dst_header, &dst_special_header);
            }
            let _ = Self::cleanup_server_map(request, Some(&mut server_process));
            let _ = Self::cleanup_client_map(request, Some(&mut client_process));
            return Some((pointer_result, recv_list_broken));
        }
        let map_alias_result = {
            let current_request = request.lock().unwrap();
            drop(current_request);
            let mut request = request.lock().unwrap();
            Self::process_receive_message_map_alias_descriptors(
                &mut dst_words,
                &src_words,
                &src_header,
                &src_special_header,
                &mut request,
                &mut server_process,
                &mut client_process,
            )
        };
        if src_header.get_map_alias_count() > 0 {
            recv_list_broken = Self::mark_receive_list_broken(
                crate::hle::kernel::message_buffer::MessageBuffer::get_raw_data_index(
                    &src_header,
                    &src_special_header,
                ),
                dst_recv_list_idx,
            );
        }
        if map_alias_result != crate::hle::result::RESULT_SUCCESS.get_inner_value() {
            if processed_special_data {
                Self::cleanup_special_data(&mut server_process, &mut dst_words, dst_size);
            }
            if !recv_list_broken {
                Self::restore_destination_header(&mut dst_words, &dst_header, &dst_special_header);
            }
            let _ = Self::cleanup_server_map(request, Some(&mut server_process));
            let _ = Self::cleanup_client_map(request, Some(&mut client_process));
            return Some((map_alias_result, recv_list_broken));
        }
        let raw_data_index = crate::hle::kernel::message_buffer::MessageBuffer::get_raw_data_index(
            &src_header,
            &src_special_header,
        );
        if src_header.get_raw_count() > 0 {
            dst_words[raw_data_index..src_end_offset]
                .copy_from_slice(&src_words[raw_data_index..src_end_offset]);
        }
        Self::write_message_words_to_process(
            &mut server_process,
            dst_address,
            &dst_words,
            copy_size,
        );
        if src_header.get_raw_count() > 0 {
            recv_list_broken = Self::mark_receive_list_broken(src_end_offset, dst_recv_list_idx);
        }
        Some((
            crate::hle::result::RESULT_SUCCESS.get_inner_value(),
            recv_list_broken,
        ))
    }

    fn try_send_message_raw(
        server_message: usize,
        server_buffer_size: usize,
        request: &Arc<Mutex<KSessionRequest>>,
    ) -> Option<u32> {
        let current_thread = crate::hle::kernel::kernel::get_current_thread_pointer()?;
        let server_process = {
            let thread = current_thread.lock().unwrap();
            thread.parent.as_ref()?.upgrade()?
        };
        let client_thread = Self::resolve_request_client_thread(request)?;
        let client_process = {
            let thread = client_thread.lock().unwrap();
            thread.parent.as_ref()?.upgrade()?
        };

        let (src_address, src_size) = {
            let thread = current_thread.lock().unwrap();
            Self::request_message_address_and_size(&thread, server_message, server_buffer_size)
        };
        let (dst_address, dst_size) = {
            let request = request.lock().unwrap();
            if request.get_address() != 0 {
                (request.get_address(), request.get_size())
            } else {
                let thread = client_thread.lock().unwrap();
                Self::request_message_address_and_size(&thread, 0, 0)
            }
        };

        let src_words = {
            let process = server_process.lock().unwrap();
            Self::message_words_from_process(&process, src_address, src_size)
        };
        let dst_word_capacity = dst_size / std::mem::size_of::<u32>();
        let original_dst_words = {
            let process = client_process.lock().unwrap();
            Self::message_words_from_process(&process, dst_address, dst_size)
        };
        let (src_header, src_special_header) = Self::parse_message_headers(&src_words);
        let (dst_header, dst_special_header) = Self::parse_message_headers(&original_dst_words);

        if crate::hle::kernel::message_buffer::MessageBuffer::get_message_buffer_size(
            &src_header,
            &src_special_header,
        ) > src_size
        {
            return Some(RESULT_INVALID_COMBINATION.get_inner_value());
        }
        if crate::hle::kernel::message_buffer::MessageBuffer::get_message_buffer_size(
            &dst_header,
            &dst_special_header,
        ) > dst_size
        {
            return Some(RESULT_INVALID_COMBINATION.get_inner_value());
        }
        if src_header.get_send_count() != 0
            || src_header.get_receive_count() != 0
            || src_header.get_exchange_count() != 0
        {
            return Some(RESULT_INVALID_COMBINATION.get_inner_value());
        }
        if dst_header.get_receive_list_offset() != 0
            && (dst_header.get_receive_list_offset() as usize)
                < crate::hle::kernel::message_buffer::MessageBuffer::get_raw_data_index(
                    &dst_header,
                    &dst_special_header,
                ) + dst_header.get_raw_count() as usize
        {
            return Some(RESULT_INVALID_COMBINATION.get_inner_value());
        }
        let src_end_offset = crate::hle::kernel::message_buffer::MessageBuffer::get_raw_data_index(
            &src_header,
            &src_special_header,
        ) + src_header.get_raw_count() as usize;
        let copy_size = src_end_offset * std::mem::size_of::<u32>();
        if dst_size < copy_size {
            return Some(RESULT_MESSAGE_TOO_LARGE.get_inner_value());
        }

        let mut dst_words = original_dst_words;
        if dst_words.len() < dst_word_capacity {
            dst_words.resize(dst_word_capacity, 0);
        }
        Self::set_message_header_and_special_header(
            &mut dst_words,
            &src_header,
            &src_special_header,
        );
        let mut client_process = client_process.lock().unwrap();
        let mut server_process = server_process.lock().unwrap();
        let processed_special_data = src_header.get_has_special_header();
        let special_result = Self::process_message_special_data(
            &mut dst_words,
            &src_words,
            &src_header,
            &src_special_header,
            &mut client_process,
            &mut server_process,
            true,
        );
        if special_result != crate::hle::result::RESULT_SUCCESS.get_inner_value() {
            if processed_special_data {
                Self::cleanup_special_data(&mut client_process, &mut dst_words, dst_size);
            }
            let _ = Self::cleanup_server_map(request, Some(&mut server_process));
            let _ = Self::cleanup_client_map(request, Some(&mut client_process));
            return Some(special_result);
        }
        let pointer_result = Self::process_send_message_pointer_descriptors(
            &mut dst_words,
            &src_words,
            &src_header,
            &src_special_header,
            &dst_header,
            &dst_special_header,
            dst_address,
            dst_size,
            &mut client_process,
            &server_process,
        );
        if pointer_result != crate::hle::result::RESULT_SUCCESS.get_inner_value() {
            if processed_special_data {
                Self::cleanup_special_data(&mut client_process, &mut dst_words, dst_size);
            }
            let _ = Self::cleanup_server_map(request, Some(&mut server_process));
            let _ = Self::cleanup_client_map(request, Some(&mut client_process));
            return Some(pointer_result);
        }
        {
            let request = request.lock().unwrap();
            Self::process_send_message_receive_mappings(
                &request,
                &server_process,
                &mut client_process,
            );
            let map_alias_index =
                crate::hle::kernel::message_buffer::MessageBuffer::get_map_alias_descriptor_index(
                    &src_header,
                    &src_special_header,
                );
            for i in 0..src_header.get_map_alias_count() {
                let cur_offset = map_alias_index
                    + i as usize
                        * (crate::hle::kernel::message_buffer::MapAliasDescriptor::DATA_SIZE
                            / std::mem::size_of::<u32>());
                let mut dst_message = MessageBuffer::new(&mut dst_words);
                dst_message.set(cur_offset, &[0, 0, 0]);
            }
        }
        let raw_data_index = crate::hle::kernel::message_buffer::MessageBuffer::get_raw_data_index(
            &src_header,
            &src_special_header,
        );
        if src_header.get_raw_count() > 0 {
            dst_words[raw_data_index..src_end_offset]
                .copy_from_slice(&src_words[raw_data_index..src_end_offset]);
        }
        Self::write_message_words_to_process(
            &mut client_process,
            dst_address,
            &dst_words,
            copy_size,
        );
        let cleanup_server_map = Self::cleanup_server_map(request, Some(&mut server_process));
        if cleanup_server_map != crate::hle::result::RESULT_SUCCESS.get_inner_value() {
            if processed_special_data {
                Self::cleanup_special_data(&mut client_process, &mut dst_words, dst_size);
                Self::write_message_words_to_process(
                    &mut client_process,
                    dst_address,
                    &dst_words,
                    copy_size,
                );
            }
            return Some(cleanup_server_map);
        }
        let cleanup_client_map = Self::cleanup_client_map(request, Some(&mut client_process));
        if cleanup_client_map != crate::hle::result::RESULT_SUCCESS.get_inner_value() {
            if processed_special_data {
                Self::cleanup_special_data(&mut client_process, &mut dst_words, dst_size);
                Self::write_message_words_to_process(
                    &mut client_process,
                    dst_address,
                    &dst_words,
                    copy_size,
                );
            }
            return Some(cleanup_client_map);
        }
        Some(crate::hle::result::RESULT_SUCCESS.get_inner_value())
    }

    fn cleanup_server_handles(message: usize, mut buffer_size: usize, _message_paddr: u64) -> u32 {
        let Some(thread) = crate::hle::kernel::kernel::get_current_thread_pointer() else {
            return RESULT_INVALID_STATE.get_inner_value();
        };

        let (process, tls_address) = {
            let thread = thread.lock().unwrap();
            let Some(process) = thread.parent.as_ref().and_then(|process| process.upgrade()) else {
                return RESULT_INVALID_STATE.get_inner_value();
            };
            (process, thread.get_tls_address().get() as usize)
        };

        let message_address = if message != 0 { message } else { tls_address };
        if message == 0 {
            buffer_size = MESSAGE_BUFFER_SIZE * std::mem::size_of::<u32>();
        }

        let mut words = vec![0u32; buffer_size / std::mem::size_of::<u32>()];
        {
            let process = process.lock().unwrap();
            let bytes = if let Some(memory) = process.get_memory() {
                let mut bytes = vec![0u8; buffer_size];
                memory
                    .lock()
                    .unwrap()
                    .read_block(message_address as u64, &mut bytes);
                bytes
            } else {
                let memory = process.process_memory.read().unwrap();
                memory.read_bytes(message_address as u64, buffer_size)
            };
            for (index, chunk) in bytes.chunks_exact(4).enumerate() {
                words[index] = u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
            }
        }

        let header =
            crate::hle::kernel::message_buffer::MessageHeader::from_raw([words[0], words[1]]);
        let special_header = if header.get_has_special_header() && words.len() > 2 {
            crate::hle::kernel::message_buffer::SpecialHeader::new_with_flag(
                (words[2] & 1) != 0,
                ((words[2] >> 1) & 0xF) as i32,
                ((words[2] >> 5) & 0xF) as i32,
                true,
            )
        } else {
            crate::hle::kernel::message_buffer::SpecialHeader::new_with_flag(false, 0, 0, false)
        };

        if crate::hle::kernel::message_buffer::MessageBuffer::get_message_buffer_size(
            &header,
            &special_header,
        ) > buffer_size
        {
            return RESULT_INVALID_COMBINATION.get_inner_value();
        }

        if header.get_has_special_header() {
            let message = MessageBuffer::new(&mut words);
            let mut offset =
                crate::hle::kernel::message_buffer::MessageBuffer::get_special_data_index(
                    &special_header,
                );
            if special_header.get_has_process_id() {
                offset += std::mem::size_of::<u64>() / std::mem::size_of::<u32>();
            }
            if special_header.get_copy_handle_count() > 0 {
                offset += (std::mem::size_of::<crate::hle::kernel::svc_common::Handle>()
                    * special_header.get_copy_handle_count() as usize)
                    / std::mem::size_of::<u32>();
            }

            let mut process = process.lock().unwrap();
            for _ in 0..special_header.get_move_handle_count() {
                let handle = message.get_handle(offset);
                if handle != INVALID_HANDLE {
                    process.handle_table.remove(handle);
                }
                offset += std::mem::size_of::<crate::hle::kernel::svc_common::Handle>()
                    / std::mem::size_of::<u32>();
            }
        }

        crate::hle::result::RESULT_SUCCESS.get_inner_value()
    }

    fn resolve_request_server_process(
        request: &Arc<Mutex<KSessionRequest>>,
    ) -> Option<Arc<Mutex<crate::hle::kernel::k_process::KProcess>>> {
        let server_process_id = request.lock().unwrap().get_server_process_id()?;
        let kernel = crate::hle::kernel::kernel::get_kernel_ref()?;
        kernel.get_process_by_id(server_process_id)
    }

    fn resolve_request_client_process(
        request: &Arc<Mutex<KSessionRequest>>,
    ) -> Option<Arc<Mutex<crate::hle::kernel::k_process::KProcess>>> {
        let client_process_id = request.lock().unwrap().get_client_process_id()?;
        let kernel = crate::hle::kernel::kernel::get_kernel_ref()?;
        kernel.get_process_by_id(client_process_id)
    }

    fn cleanup_server_map(
        request: &Arc<Mutex<KSessionRequest>>,
        server_process: Option<&mut crate::hle::kernel::k_process::KProcess>,
    ) -> u32 {
        let Some(server_process) = server_process else {
            return crate::hle::result::RESULT_SUCCESS.get_inner_value();
        };

        let request = request.lock().unwrap();
        for index in 0..request.mappings.get_send_count() {
            let mapping = request.mappings.get_send_mapping(index);
            let rc = server_process.page_table.cleanup_for_ipc_server(
                mapping.server_address,
                mapping.size,
                mapping.state,
            );
            if rc != crate::hle::result::RESULT_SUCCESS.get_inner_value() {
                return rc;
            }
        }
        for index in 0..request.mappings.get_receive_count() {
            let mapping = request.mappings.get_receive_mapping(index);
            let rc = server_process.page_table.cleanup_for_ipc_server(
                mapping.server_address,
                mapping.size,
                mapping.state,
            );
            if rc != crate::hle::result::RESULT_SUCCESS.get_inner_value() {
                return rc;
            }
        }
        for index in 0..request.mappings.get_exchange_count() {
            let mapping = request.mappings.get_exchange_mapping(index);
            let rc = server_process.page_table.cleanup_for_ipc_server(
                mapping.server_address,
                mapping.size,
                mapping.state,
            );
            if rc != crate::hle::result::RESULT_SUCCESS.get_inner_value() {
                return rc;
            }
        }

        crate::hle::result::RESULT_SUCCESS.get_inner_value()
    }

    fn cleanup_client_map(
        request: &Arc<Mutex<KSessionRequest>>,
        client_process: Option<&mut crate::hle::kernel::k_process::KProcess>,
    ) -> u32 {
        let Some(client_process) = client_process else {
            return crate::hle::result::RESULT_SUCCESS.get_inner_value();
        };

        let request = request.lock().unwrap();
        for index in 0..request.mappings.get_send_count() {
            let mapping = request.mappings.get_send_mapping(index);
            let rc = client_process.page_table.cleanup_for_ipc_client(
                mapping.client_address,
                mapping.size,
                mapping.state,
            );
            if rc != crate::hle::result::RESULT_SUCCESS.get_inner_value() {
                return rc;
            }
        }
        for index in 0..request.mappings.get_receive_count() {
            let mapping = request.mappings.get_receive_mapping(index);
            let rc = client_process.page_table.cleanup_for_ipc_client(
                mapping.client_address,
                mapping.size,
                mapping.state,
            );
            if rc != crate::hle::result::RESULT_SUCCESS.get_inner_value() {
                return rc;
            }
        }
        for index in 0..request.mappings.get_exchange_count() {
            let mapping = request.mappings.get_exchange_mapping(index);
            let rc = client_process.page_table.cleanup_for_ipc_client(
                mapping.client_address,
                mapping.size,
                mapping.state,
            );
            if rc != crate::hle::result::RESULT_SUCCESS.get_inner_value() {
                return rc;
            }
        }

        crate::hle::result::RESULT_SUCCESS.get_inner_value()
    }

    fn cleanup_map(request: &Arc<Mutex<KSessionRequest>>) -> u32 {
        if let Some(server_process) = Self::resolve_request_server_process(request) {
            let mut server_process = server_process.lock().unwrap();
            let rc = Self::cleanup_server_map(request, Some(&mut server_process));
            if rc != crate::hle::result::RESULT_SUCCESS.get_inner_value() {
                return rc;
            }
        }

        if let Some(client_process) = Self::resolve_request_client_process(request) {
            let mut client_process = client_process.lock().unwrap();
            return Self::cleanup_client_map(request, Some(&mut client_process));
        }

        crate::hle::result::RESULT_SUCCESS.get_inner_value()
    }

    fn clear_current_request_and_notify(&mut self) {
        self.current_request = None;
        if !self.request_list.is_empty() {
            self.notify_available(crate::hle::result::RESULT_SUCCESS.get_inner_value());
        }
    }

    fn finalize_request(request: &Arc<Mutex<KSessionRequest>>) {
        request.lock().unwrap().finalize();
    }

    fn async_cleanup_result(cleanup_result: u32) -> ResultCode {
        if cleanup_result == crate::hle::result::RESULT_SUCCESS.get_inner_value() {
            RESULT_SESSION_CLOSED
        } else {
            ResultCode::new(cleanup_result)
        }
    }

    fn complete_aborted_request(request: &Arc<Mutex<KSessionRequest>>, result: ResultCode) {
        let client_thread = Self::resolve_request_client_thread(request);
        let (event_id, client_process_id, client_message, client_buffer_size) = {
            let request = request.lock().unwrap();
            (
                request.get_event_id(),
                request.get_client_process_id(),
                request.get_address(),
                request.get_size(),
            )
        };

        if let Some(event_id) = event_id {
            if let Some(client_process_id) = client_process_id {
                if let Some(kernel) = crate::hle::kernel::kernel::get_kernel_ref() {
                    if let Some(process_arc) = kernel.get_process_by_id(client_process_id) {
                        let mut process = process_arc.lock().unwrap();
                        Self::reply_async_error(
                            &mut process,
                            client_message,
                            client_buffer_size,
                            result,
                        );
                        let _ = process.page_table.unlock_for_ipc_user_buffer(
                            KProcessAddress::new(client_message as u64),
                            client_buffer_size,
                        );
                        if let Some(event) = process.get_event_by_object_id(event_id) {
                            if let Some(scheduler) = kernel
                                .current_scheduler()
                                .cloned()
                                .or_else(|| kernel.scheduler(0).cloned())
                            {
                                let _ = event.lock().unwrap().signal(&mut process, &scheduler);
                            }
                        }
                    }
                }
            }
        } else if let Some(client_thread) = client_thread {
            let mut client_thread = client_thread.lock().unwrap();
            if !client_thread.is_termination_requested() {
                client_thread.end_wait(result.get_inner_value());
            }
        }
    }

    fn fail_receive_request(
        &mut self,
        request: Arc<Mutex<KSessionRequest>>,
        result: ResultCode,
    ) -> u32 {
        self.fail_receive_request_with_server_result(request, result, result.get_inner_value())
    }

    fn fail_receive_request_with_server_result(
        &mut self,
        request: Arc<Mutex<KSessionRequest>>,
        client_result: ResultCode,
        server_result: u32,
    ) -> u32 {
        self.clear_current_request_and_notify();
        Self::complete_aborted_request(&request, client_result);
        Self::finalize_request(&request);
        server_result
    }

    fn resolve_request_client_thread(
        request: &Arc<Mutex<KSessionRequest>>,
    ) -> Option<Arc<Mutex<crate::hle::kernel::k_thread::KThread>>> {
        request.lock().unwrap().get_thread()
    }

    fn reply_async_error(
        process: &mut crate::hle::kernel::k_process::KProcess,
        message_address: usize,
        _message_size: usize,
        result: ResultCode,
    ) {
        let mut words = [0u32; MESSAGE_BUFFER_SIZE];
        {
            let mut message = MessageBuffer::new(&mut words);
            message.set_async_result(result);
        }

        let mut bytes = [0u8; 12];
        for (chunk, word) in bytes.chunks_exact_mut(4).zip(words[..3].iter().copied()) {
            chunk.copy_from_slice(&word.to_le_bytes());
        }
        if let Some(memory) = process.get_memory() {
            memory
                .lock()
                .unwrap()
                .write_block(message_address as u64, &bytes);
        } else {
            process
                .process_memory
                .write()
                .unwrap()
                .write_block(message_address as u64, &bytes);
        }
    }

    pub fn new() -> Self {
        Self {
            parent_id: None,
            request_list: VecDeque::new(),
            current_request: None,
            client_closed: false,
            manager: None,
            sync_object: SynchronizationObjectState::new(),
        }
    }

    /// Initialize with a parent session.
    pub fn initialize(&mut self, parent_id: u64) {
        self.parent_id = Some(parent_id);
        self.current_request = None;
        self.client_closed = false;
        self.request_list.clear();
        self.sync_object.clear_waiters();
    }

    /// Set the session request manager.
    /// Called by ServerManager::RegisterSession to link the server session
    /// to its HLE handler.
    pub fn set_manager(&mut self, manager: Arc<Mutex<SessionRequestManager>>) {
        self.manager = Some(manager);
    }

    /// Get the session request manager.
    pub fn get_manager(&self) -> Option<&Arc<Mutex<SessionRequestManager>>> {
        self.manager.as_ref()
    }

    pub fn get_parent_id(&self) -> Option<u64> {
        self.parent_id
    }

    /// Is the server session signaled (has pending requests)?
    /// Matches upstream `KServerSession::IsSignaled`.
    pub fn is_signaled(&self) -> bool {
        self.client_closed || (!self.request_list.is_empty() && self.current_request.is_none())
    }

    /// Called when the client side is closed.
    /// Port of upstream `KServerSession::OnClientClosed`.
    /// Upstream signals the session and wakes waiting threads.
    ///
    /// Callers should migrate to `on_client_closed_with_process(...)` when they
    /// already hold the owner `KProcess`. This fallback path re-enters the
    /// kernel registry to rediscover the owner process and is unsafe under a
    /// held process lock.
    pub fn on_client_closed(&mut self) {
        self.client_closed = true;
        self.cleanup_requests();
        self.notify_available(RESULT_SESSION_CLOSED.get_inner_value());
    }

    /// Port of upstream `KServerSession::OnClientClosed`, when the owner
    /// process is already known by the caller.
    pub fn on_client_closed_with_process(
        &mut self,
        process: &mut crate::hle::kernel::k_process::KProcess,
    ) {
        self.client_closed = true;
        self.cleanup_requests();
        self.notify_available_in_process(process, RESULT_SESSION_CLOSED.get_inner_value());
    }

    /// Enqueue a request.
    /// Port of upstream `KServerSession::OnRequest`.
    ///
    /// Callers should migrate to `on_request_with_process(...)` when they
    /// already hold the owner `KProcess`. This fallback path re-enters the
    /// kernel registry to rediscover the owner process and is unsafe under a
    /// held process lock.
    pub fn on_request(&mut self, request: Arc<Mutex<KSessionRequest>>) -> u32 {
        let was_empty = self.request_list.is_empty();
        self.request_list.push_back(request);
        if was_empty {
            self.notify_available(crate::hle::result::RESULT_SUCCESS.get_inner_value());
        }
        0 // ResultSuccess
    }

    /// Port of upstream `KServerSession::OnRequest`, when the owner process is
    /// already known by the caller.
    pub fn on_request_with_process(
        &mut self,
        process: &mut crate::hle::kernel::k_process::KProcess,
        request: Arc<Mutex<KSessionRequest>>,
    ) -> u32 {
        let was_empty = self.request_list.is_empty();
        self.request_list.push_back(request);
        if was_empty {
            self.notify_available_in_process(
                process,
                crate::hle::result::RESULT_SUCCESS.get_inner_value(),
            );
        }
        0 // ResultSuccess
    }

    /// Send a reply to the current request.
    /// Port of upstream `KServerSession::SendReply`.
    ///
    /// Rust now keeps the `server_message` / `server_buffer_size` /
    /// `server_message_paddr` transport boundary in this owner file, even
    /// though the full upstream message-buffer translation still remains to be
    /// ported.
    pub fn send_reply_with_message(
        &mut self,
        server_message: usize,
        server_buffer_size: usize,
        _server_message_paddr: u64,
        is_hle: bool,
    ) -> u32 {
        let trace_reply = is_hle && std::env::var_os("RUZU_LOG_SYNC_REPLY").is_some();
        let Some(request) = self.current_request.take() else {
            return RESULT_INVALID_STATE.get_inner_value();
        };
        if trace_reply {
            log::info!("KServerSession::send_reply_with_message stage=took_current_request");
        }
        if self.current_request.is_none() {
            self.clear_current_request_and_notify();
        }
        if trace_reply {
            log::info!("KServerSession::send_reply_with_message stage=cleared_current_request");
        }

        let client_thread = Self::resolve_request_client_thread(&request);
        let (event_id, client_process_id, client_message, client_buffer_size) = {
            let request = request.lock().unwrap();
            (
                request.get_event_id(),
                request.get_client_process_id(),
                request.get_address(),
                request.get_size(),
            )
        };
        if trace_reply {
            log::info!(
                "KServerSession::send_reply_with_message stage=resolved_request client_thread={} event_id={:?} client_process_id={:?} addr={:#x} size={:#x}",
                client_thread.is_some(),
                event_id,
                client_process_id,
                client_message,
                client_buffer_size
            );
        }
        let closed = client_thread.is_none() || self.client_closed;

        let mut result = crate::hle::result::RESULT_SUCCESS.get_inner_value();
        if !closed && !is_hle {
            if let Some(send_result) =
                Self::try_send_message_raw(server_message, server_buffer_size, &request)
            {
                result = send_result;
            }
        } else if closed && !is_hle {
            result = Self::cleanup_server_handles(
                server_message,
                server_buffer_size,
                _server_message_paddr,
            );
            let cleanup_map_result = Self::cleanup_map(&request);
            if result == crate::hle::result::RESULT_SUCCESS.get_inner_value() {
                result = cleanup_map_result;
            }
        }

        let mut client_result = result;
        if closed {
            if result == crate::hle::result::RESULT_SUCCESS.get_inner_value() {
                result = RESULT_SESSION_CLOSED.get_inner_value();
                client_result = RESULT_SESSION_CLOSED.get_inner_value();
            } else {
                result = crate::hle::result::RESULT_SUCCESS.get_inner_value();
            }
        } else {
            result = crate::hle::result::RESULT_SUCCESS.get_inner_value();
        }
        if trace_reply {
            log::info!(
                "KServerSession::send_reply_with_message stage=selected_results closed={} client_result={:#x} result={:#x}",
                closed,
                client_result,
                result
            );
        }

        if let Some(event_id) = event_id {
            if let Some(client_process_id) = client_process_id {
                if let Some(kernel) = crate::hle::kernel::kernel::get_kernel_ref() {
                    if let Some(process_arc) = kernel.get_process_by_id(client_process_id) {
                        let mut process = process_arc.lock().unwrap();
                        if client_result != crate::hle::result::RESULT_SUCCESS.get_inner_value() {
                            Self::reply_async_error(
                                &mut process,
                                client_message,
                                client_buffer_size,
                                ResultCode::new(client_result),
                            );
                        }
                        let _ = process.page_table.unlock_for_ipc_user_buffer(
                            KProcessAddress::new(client_message as u64),
                            client_buffer_size,
                        );
                        if let Some(event) = process.get_event_by_object_id(event_id) {
                            if let Some(scheduler) = kernel
                                .current_scheduler()
                                .cloned()
                                .or_else(|| kernel.scheduler(0).cloned())
                            {
                                if trace_reply {
                                    log::info!(
                                        "KServerSession::send_reply_with_message stage=signal_async_event"
                                    );
                                }
                                let _ = event.lock().unwrap().signal(&mut process, &scheduler);
                            }
                        }
                    }
                }
            }
        } else if let Some(client_thread) = &client_thread {
            if trace_reply {
                log::info!("KServerSession::send_reply_with_message stage=before_lock_client_thread");
            }
            let mut client_thread = client_thread.lock().unwrap();
            if trace_reply {
                log::info!(
                    "KServerSession::send_reply_with_message stage=locked_client_thread state={:?}",
                    client_thread.get_state()
                );
            }
            if !client_thread.is_termination_requested()
                && client_thread.get_state() == crate::hle::kernel::k_thread::ThreadState::WAITING
            {
                if trace_reply {
                    log::info!("KServerSession::send_reply_with_message stage=before_end_wait");
                }
                client_thread.end_wait(client_result);
                if trace_reply {
                    log::info!("KServerSession::send_reply_with_message stage=after_end_wait");
                }
            } else if trace_reply {
                log::info!(
                    "KServerSession::send_reply_with_message stage=skip_end_wait state={:?} termination_requested={}",
                    client_thread.get_state(),
                    client_thread.is_termination_requested()
                );
            }
        }

        {
            let mut request = request.lock().unwrap();
            request.clear_thread();
            request.clear_event();
            request.finalize();
        }
        if trace_reply {
            log::info!("KServerSession::send_reply_with_message stage=finalized_request");
        }

        result
    }

    /// HLE convenience wrapper matching upstream `SendReplyHLE()`.
    pub fn send_reply(&mut self) -> u32 {
        self.send_reply_with_message(0, 0, 0, true)
    }

    /// Receive the next pending request.
    /// Port of upstream `KServerSession::ReceiveRequest`.
    ///
    /// Rust now keeps the `server_message` / `server_buffer_size` /
    /// `server_message_paddr` transport boundary in this owner file, even
    /// though the full upstream message-buffer translation and
    /// `HLERequestContext` construction still remain to be ported.
    pub fn receive_request_with_message(
        &mut self,
        server_message: usize,
        server_buffer_size: usize,
        _server_message_paddr: u64,
    ) -> u32 {
        if self.client_closed {
            return RESULT_SESSION_CLOSED.get_inner_value();
        }
        if self.current_request.is_some() {
            return RESULT_NOT_FOUND.get_inner_value();
        }

        let Some(request) = self.request_list.pop_front() else {
            return RESULT_NOT_FOUND.get_inner_value();
        };

        if Self::resolve_request_client_thread(&request).is_none() {
            return self.fail_receive_request(request, RESULT_SESSION_CLOSED);
        }

        if let Some(current_thread) = crate::hle::kernel::kernel::get_current_thread_pointer() {
            let process_id = current_thread
                .lock()
                .unwrap()
                .parent
                .as_ref()
                .and_then(|process| process.upgrade())
                .map(|process| process.lock().unwrap().process_id);
            if let Some(process_id) = process_id {
                request.lock().unwrap().set_server_process(process_id);
            }
        }
        self.current_request = Some(Arc::clone(&request));
        let (receive_result, recv_list_broken) =
            Self::try_receive_message_raw(server_message, server_buffer_size, &request)
                .unwrap_or((crate::hle::result::RESULT_SUCCESS.get_inner_value(), false));
        if receive_result != crate::hle::result::RESULT_SUCCESS.get_inner_value() {
            return self.fail_receive_request_with_server_result(
                request,
                ResultCode::new(receive_result),
                if recv_list_broken {
                    RESULT_RECEIVE_LIST_BROKEN.get_inner_value()
                } else {
                    RESULT_NOT_FOUND.get_inner_value()
                },
            );
        }

        self.current_request = Some(request);
        crate::hle::result::RESULT_SUCCESS.get_inner_value()
    }

    /// HLE receive path corresponding to upstream
    /// `ReceiveRequest(..., out_context, manager)`.
    pub fn receive_request_hle(
        &mut self,
        manager: Arc<Mutex<SessionRequestManager>>,
    ) -> Result<(HLERequestContext, Arc<Mutex<SessionRequestManager>>, u64), u32> {
        if self.client_closed {
            return Err(RESULT_SESSION_CLOSED.get_inner_value());
        }
        if self.current_request.is_some() {
            return Err(RESULT_NOT_FOUND.get_inner_value());
        }

        let Some(current_request) = self.request_list.pop_front() else {
            return Err(RESULT_NOT_FOUND.get_inner_value());
        };
        let Some(client_thread) = Self::resolve_request_client_thread(&current_request) else {
            {
                let mut request = current_request.lock().unwrap();
                request.finalize();
            }
            if !self.request_list.is_empty() {
                self.notify_available(crate::hle::result::RESULT_SUCCESS.get_inner_value());
            }
            return Err(RESULT_SESSION_CLOSED.get_inner_value());
        };

        self.current_request = Some(Arc::clone(&current_request));
        let request_message_address = {
            let request = current_request.lock().unwrap();
            let request_address = request.get_address() as u64;
            if request_address != 0 {
                request_address
            } else {
                client_thread.lock().unwrap().get_tls_address().get()
            }
        };

        let mut context =
            HLERequestContext::new_with_thread(client_thread, request_message_address);
        context.set_session_request_manager(Arc::clone(&manager));
        context.populate_from_incoming_command_buffer(&[]);
        Ok((context, manager, request_message_address))
    }

    /// HLE convenience wrapper matching upstream `ReceiveRequestHLE()`.
    pub fn receive_request(&mut self) -> u32 {
        self.receive_request_with_message(0, 0, 0)
    }

    pub fn get_current_request(&self) -> Option<Arc<Mutex<KSessionRequest>>> {
        self.current_request.clone()
    }

    /// Clean up pending requests.
    pub fn cleanup_requests(&mut self) {
        if let Some(request) = self.current_request.take() {
            let cleanup_result = Self::cleanup_map(&request);
            Self::complete_aborted_request(&request, Self::async_cleanup_result(cleanup_result));
            Self::finalize_request(&request);
        }
        while let Some(request) = self.request_list.pop_front() {
            let cleanup_result = Self::cleanup_map(&request);
            Self::complete_aborted_request(&request, Self::async_cleanup_result(cleanup_result));
            Self::finalize_request(&request);
        }
        self.request_list.clear();
    }

    pub fn link_waiter(
        &mut self,
        process: &mut crate::hle::kernel::k_process::KProcess,
        thread_id: u64,
    ) {
        let Some(object_id) = self.parent_id else {
            return;
        };
        self.sync_object.link_waiter(
            process,
            crate::hle::kernel::k_synchronization_object::SynchronizationWaitNode {
                object_id,
                handle:
                    crate::hle::kernel::k_synchronization_object::SynchronizationWaitNodeHandle {
                        thread_id,
                        wait_index: 0,
                    },
            },
        );
    }

    pub fn unlink_waiter(
        &mut self,
        process: &mut crate::hle::kernel::k_process::KProcess,
        thread_id: u64,
    ) {
        let Some(object_id) = self.parent_id else {
            return;
        };
        self.sync_object
            .unlink_waiter(process, thread_id, object_id, 0);
    }

    /// Fallback waiter notification path for callers that do not already have
    /// the owner `KProcess`.
    ///
    /// Callers should migrate to `notify_available_in_process(...)` through the
    /// public `*_with_process(...)` entry points. This variant re-enters the
    /// kernel registry to rediscover the owner process and is unsafe under a
    /// held process lock.
    fn notify_available(&mut self, result: u32) -> bool {
        let Some(object_id) = self.parent_id else {
            return false;
        };
        let Some(kernel) = crate::hle::kernel::kernel::get_kernel_ref() else {
            return false;
        };
        let Some(owner_process_id) = kernel.get_session_owner_process_id(object_id) else {
            return false;
        };
        let Some(process_arc) = kernel.get_process_by_id(owner_process_id) else {
            return false;
        };
        let mut process = process_arc.lock().unwrap();
        self.notify_available_in_process(&mut process, result)
    }

    fn notify_available_in_process(
        &mut self,
        process: &mut crate::hle::kernel::k_process::KProcess,
        result: u32,
    ) -> bool {
        let Some(object_id) = self.parent_id else {
            return false;
        };
        let waiter_snapshot = self.sync_object.waiter_snapshot(&process);
        let outcome = k_synchronization_object::process_waiter_snapshot(
            process,
            object_id,
            &waiter_snapshot,
            result,
        );
        for thread_id in outcome.unlink_thread_ids {
            self.unlink_waiter(process, thread_id);
        }
        outcome.woke_any
    }

    /// Destroy the server session.
    pub fn destroy(&mut self) {
        self.cleanup_requests();
    }
}

impl Default for KServerSession {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hle::kernel::k_process::KProcess;
    use crate::hle::kernel::k_readable_event::KReadableEvent;
    use crate::hle::kernel::k_scheduler::KScheduler;
    use crate::hle::kernel::k_session::KSession;
    use crate::hle::kernel::k_session_request::KSessionRequest;

    #[test]
    fn is_signaled_requires_no_current_request() {
        let mut server = KServerSession::new();
        server.initialize(0x1000);

        assert!(!server.is_signaled());
        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        server.on_request(request.clone());
        assert!(server.is_signaled());

        server.current_request = Some(request);
        assert!(!server.is_signaled());
    }

    #[test]
    fn is_signaled_when_client_closed() {
        let mut server = KServerSession::new();
        server.initialize(0x1000);

        assert!(!server.is_signaled());
        server.on_client_closed();
        assert!(server.is_signaled());
    }

    #[test]
    fn link_waiter_uses_parent_session_object_id() {
        let mut process = KProcess::new();
        let mut server = KServerSession::new();
        server.initialize(0x1000);

        let waiter = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut waiter = waiter.lock().unwrap();
            waiter.thread_id = 7;
            waiter.object_id = 8;
            waiter.synchronization_wait.begin(vec![0x1000]);
            waiter.synchronization_wait.bind_thread(7);
        }
        process.register_thread_object(waiter);

        let session = Arc::new(Mutex::new(KSession::new()));
        process.register_session_object(0x1000, session);

        server.link_waiter(&mut process, 7);
        assert_eq!(server.sync_object.waiter_snapshot(&process), vec![7]);
    }

    #[test]
    fn cleanup_requests_finalizes_pending_and_current_requests() {
        let mut server = KServerSession::new();
        server.initialize(0x1000);

        let pending = Arc::new(Mutex::new(KSessionRequest::new()));
        pending.lock().unwrap().thread_id = Some(10);
        let current = Arc::new(Mutex::new(KSessionRequest::new()));
        current.lock().unwrap().thread_id = Some(11);

        server.request_list.push_back(Arc::clone(&pending));
        server.current_request = Some(Arc::clone(&current));

        server.cleanup_requests();

        assert!(server.request_list.is_empty());
        assert!(server.current_request.is_none());
        assert_eq!(pending.lock().unwrap().get_thread_id(), None);
        assert_eq!(current.lock().unwrap().get_thread_id(), None);
    }

    #[test]
    fn send_reply_signals_async_request_event() {
        let mut process = KProcess::new();
        process.process_id = 7;
        let scheduler = Arc::new(Mutex::new(KScheduler::new(0)));
        scheduler.lock().unwrap().initialize(1, 0, 0);

        let event_id = 0x4000;
        let readable_id = 0x4001;
        let event = Arc::new(Mutex::new(crate::hle::kernel::k_event::KEvent::new()));
        event
            .lock()
            .unwrap()
            .initialize(process.process_id, readable_id);
        let readable = Arc::new(Mutex::new(KReadableEvent::new()));
        readable.lock().unwrap().initialize(event_id, readable_id);
        process.register_event_object(event_id, Arc::clone(&event));
        process.register_readable_event_object(readable_id, Arc::clone(&readable));

        let mut system = crate::core::System::new_for_test();
        system.set_scheduler_arc(Arc::clone(&scheduler));
        system.set_current_process_arc(Arc::new(Mutex::new(process)));

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request_guard = request.lock().unwrap();
            request_guard.event_id = Some(event_id);
            request_guard.client_process_id = Some(7);
        }
        server.current_request = Some(request);

        assert_eq!(server.send_reply_with_message(0x2000, 0x100, 0, false), 0);
        let process = system.current_process_arc();
        let process = process.lock().unwrap();
        let readable = process
            .get_readable_event_by_object_id(readable_id)
            .unwrap();
        assert!(readable.lock().unwrap().is_signaled());
    }

    #[test]
    fn send_reply_writes_async_error_result_for_event_request() {
        let mut setup_system = crate::core::System::new_for_test();

        let mut process = KProcess::new();
        process.process_id = 7;
        process.create_memory(&setup_system);
        let scheduler = Arc::new(Mutex::new(KScheduler::new(0)));
        scheduler.lock().unwrap().initialize(1, 0, 0);

        let event_id = 0x4000;
        let readable_id = 0x4001;
        let event = Arc::new(Mutex::new(crate::hle::kernel::k_event::KEvent::new()));
        event
            .lock()
            .unwrap()
            .initialize(process.process_id, readable_id);
        let readable = Arc::new(Mutex::new(KReadableEvent::new()));
        readable.lock().unwrap().initialize(event_id, readable_id);
        process.register_event_object(event_id, Arc::clone(&event));
        process.register_readable_event_object(readable_id, Arc::clone(&readable));

        let process = Arc::new(Mutex::new(process));
        setup_system.set_scheduler_arc(Arc::clone(&scheduler));
        setup_system.set_current_process_arc(Arc::clone(&process));

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.client_closed = true;
        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request_guard = request.lock().unwrap();
            request_guard.event_id = Some(event_id);
            request_guard.client_process_id = Some(7);
            request_guard.address = 0x2395000;
            request_guard.size = 0x1000;
        }
        server.current_request = Some(request);

        assert_eq!(
            server.send_reply_with_message(0x2000, 0x100, 0, false),
            RESULT_SESSION_CLOSED.get_inner_value()
        );

        let process = process.lock().unwrap();
        let memory = process.get_memory().unwrap();
        let mut words = {
            let memory = memory.lock().unwrap();
            [
                memory.read_32(0x2395000),
                memory.read_32(0x2395004),
                memory.read_32(0x2395008),
            ]
        };
        let message = MessageBuffer::new(&mut words);
        assert_eq!(
            message.get_async_result().get_inner_value(),
            RESULT_SESSION_CLOSED.get_inner_value()
        );
    }

    #[test]
    fn receive_request_with_message_promotes_pending_request() {
        let mut server = KServerSession::new();
        server.initialize(0x1000);

        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        request.lock().unwrap().initialize(None, 0x2395000, 0x80);
        server.request_list.push_back(Arc::clone(&request));

        assert_eq!(server.receive_request_with_message(0x2000, 0x100, 0), 0);
        let current_request = server.get_current_request().expect("current request");
        let current_request = current_request.lock().unwrap();
        assert_eq!(current_request.get_address(), 0x2395000);
        assert_eq!(current_request.get_size(), 0x80);
    }

    #[test]
    fn send_reply_with_message_requires_current_request() {
        let mut server = KServerSession::new();
        server.initialize(0x1000);
        assert_eq!(
            server.send_reply_with_message(0x2000, 0x100, 0, false),
            RESULT_INVALID_STATE.get_inner_value()
        );
    }

    #[test]
    fn receive_request_with_message_rejects_client_closed() {
        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.client_closed = true;
        assert_eq!(
            server.receive_request_with_message(0x2000, 0x100, 0),
            RESULT_SESSION_CLOSED.get_inner_value()
        );
    }

    #[test]
    fn receive_request_with_message_rejects_reentrant_receive() {
        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.current_request = Some(Arc::new(Mutex::new(KSessionRequest::new())));
        assert_eq!(
            server.receive_request_with_message(0x2000, 0x100, 0),
            RESULT_NOT_FOUND.get_inner_value()
        );
    }

    #[test]
    fn receive_request_with_message_finalizes_dead_request_and_notifies_next() {
        let mut process = KProcess::new();
        let waiter = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut waiter_guard = waiter.lock().unwrap();
            waiter_guard.thread_id = 11;
            waiter_guard.object_id = 12;
            waiter_guard.synchronization_wait.begin(vec![0x1000]);
            waiter_guard.synchronization_wait.bind_thread(11);
            waiter_guard.begin_wait();
        }
        process.register_thread_object(Arc::clone(&waiter));
        process.register_session_object(0x1000, Arc::new(Mutex::new(KSession::new())));

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.link_waiter(&mut process, 11);

        let dead_request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request_guard = dead_request.lock().unwrap();
            request_guard.thread_id = Some(77);
            request_guard.client_process_id = Some(88);
        }
        let next_request = Arc::new(Mutex::new(KSessionRequest::new()));

        server.request_list.push_back(Arc::clone(&dead_request));
        server.request_list.push_back(next_request);

        assert_eq!(
            server.receive_request_with_message(0x2000, 0x100, 0),
            RESULT_SESSION_CLOSED.get_inner_value()
        );
        assert_eq!(dead_request.lock().unwrap().get_thread_id(), None);
        assert_eq!(
            waiter.lock().unwrap().get_wait_result(),
            crate::hle::result::RESULT_SUCCESS.get_inner_value()
        );
    }

    #[test]
    fn receive_request_hle_builds_context_from_request_address() {
        let mut system = crate::core::System::new_for_test();
        let mut process = crate::hle::kernel::k_process::KProcess::new();
        process.process_id = 9;
        process.initialize_handle_table();
        process.create_memory(&system);
        process.allocate_code_memory(0x200000, 0x40000);
        let process = Arc::new(Mutex::new(process));

        let thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread_guard = thread.lock().unwrap();
            thread_guard.thread_id = 7;
            thread_guard.object_id = 8;
            thread_guard.parent = Some(Arc::downgrade(&process));
            thread_guard.tls_address =
                crate::hle::kernel::k_typed_address::KProcessAddress::new(0x2395000);
        }
        process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&thread));

        system.set_current_process_arc(Arc::clone(&process));

        let mut request_words = [0u32; crate::hle::ipc::COMMAND_BUFFER_LENGTH];
        let mut raw_high = 0u32;
        raw_high |= 4;
        request_words[0] = crate::hle::ipc::CommandType::Request as u32;
        request_words[1] = raw_high;
        request_words[4] = 0x4943_4653;
        request_words[6] = 0x1111_2222;
        request_words[7] = 0x0003;
        let request_bytes: Vec<u8> = request_words
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        process
            .lock()
            .unwrap()
            .write_block(0x2395800, &request_bytes);

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request_guard = request.lock().unwrap();
            request_guard.thread_id = Some(7);
            request_guard.client_process_id = Some(9);
            request_guard.address = 0x2395800;
            request_guard.size = 0x80;
        }
        server.request_list.push_back(request);

        let manager = Arc::new(Mutex::new(SessionRequestManager::new()));
        let (context, returned_manager, request_message_address) =
            server.receive_request_hle(Arc::clone(&manager)).unwrap();
        assert_eq!(request_message_address, 0x2395800);
        assert!(Arc::ptr_eq(&returned_manager, &manager));
        assert_eq!(
            context.command_buffer()[0],
            crate::hle::ipc::CommandType::Request as u32
        );
        assert_eq!(context.command_buffer()[6], 0x1111_2222);
    }

    #[test]
    fn receive_request_hle_ignores_zero_request_size_for_tls_backed_sync_ipc() {
        let mut system = crate::core::System::new_for_test();
        let mut process = crate::hle::kernel::k_process::KProcess::new();
        process.process_id = 9;
        process.initialize_handle_table();
        process.create_memory(&system);
        process.allocate_code_memory(0x200000, 0x40000);
        let process = Arc::new(Mutex::new(process));

        let thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread_guard = thread.lock().unwrap();
            thread_guard.thread_id = 7;
            thread_guard.object_id = 8;
            thread_guard.parent = Some(Arc::downgrade(&process));
            thread_guard.tls_address =
                crate::hle::kernel::k_typed_address::KProcessAddress::new(0x2395000);
        }
        process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&thread));

        system.set_current_process_arc(Arc::clone(&process));

        let mut request_words = [0u32; crate::hle::ipc::COMMAND_BUFFER_LENGTH];
        let mut raw_high = 0u32;
        raw_high |= 4;
        request_words[0] = crate::hle::ipc::CommandType::Request as u32;
        request_words[1] = raw_high;
        request_words[4] = 0x4943_4653;
        request_words[6] = 0x3333_4444;
        let request_bytes: Vec<u8> = request_words
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        process
            .lock()
            .unwrap()
            .write_block(0x2395200, &request_bytes);

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request_guard = request.lock().unwrap();
            request_guard.thread_id = Some(7);
            request_guard.client_process_id = Some(9);
            request_guard.address = 0x2395200;
            request_guard.size = 0;
        }
        server.request_list.push_back(request);

        let manager = Arc::new(Mutex::new(SessionRequestManager::new()));
        let (context, _, request_message_address) =
            server.receive_request_hle(Arc::clone(&manager)).unwrap();
        assert_eq!(request_message_address, 0x2395200);
        assert_eq!(
            context.command_buffer()[0],
            crate::hle::ipc::CommandType::Request as u32
        );
        assert_eq!(context.command_buffer()[6], 0x3333_4444);
    }

    #[test]
    fn send_reply_with_message_ends_sync_client_wait() {
        let mut process = crate::hle::kernel::k_process::KProcess::new();
        process.process_id = 9;
        let process = Arc::new(Mutex::new(process));

        let client_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread_guard = client_thread.lock().unwrap();
            thread_guard.thread_id = 7;
            thread_guard.object_id = 8;
            thread_guard.parent = Some(Arc::downgrade(&process));
            thread_guard.begin_wait();
        }
        process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&client_thread));

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&process));

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request_guard = request.lock().unwrap();
            request_guard.thread_id = Some(7);
            request_guard.client_process_id = Some(9);
        }
        server.current_request = Some(request);

        assert_eq!(server.send_reply_with_message(0x2000, 0x100, 0, false), 0);
        let client_thread = client_thread.lock().unwrap();
        assert_eq!(
            client_thread.get_wait_result(),
            crate::hle::result::RESULT_SUCCESS.get_inner_value()
        );
    }

    #[test]
    fn send_reply_with_message_notifies_next_waiting_server_request() {
        let mut process = crate::hle::kernel::k_process::KProcess::new();
        process.process_id = 9;

        let waiter = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut waiter_guard = waiter.lock().unwrap();
            waiter_guard.thread_id = 11;
            waiter_guard.object_id = 12;
            waiter_guard.synchronization_wait.begin(vec![0x1000]);
            waiter_guard.synchronization_wait.bind_thread(11);
            waiter_guard.begin_wait();
        }
        process.register_thread_object(Arc::clone(&waiter));
        process.register_session_object(0x1000, Arc::new(Mutex::new(KSession::new())));

        let process = Arc::new(Mutex::new(process));

        let client_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread_guard = client_thread.lock().unwrap();
            thread_guard.thread_id = 7;
            thread_guard.object_id = 8;
            thread_guard.parent = Some(Arc::downgrade(&process));
            thread_guard.begin_wait();
        }
        process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&client_thread));

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&process));

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        {
            let mut process = process.lock().unwrap();
            server.link_waiter(&mut process, 11);
        }

        let current = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request_guard = current.lock().unwrap();
            request_guard.thread_id = Some(7);
            request_guard.client_process_id = Some(9);
        }
        let pending = Arc::new(Mutex::new(KSessionRequest::new()));
        server.current_request = Some(current);
        server.request_list.push_back(pending);

        assert_eq!(server.send_reply_with_message(0x2000, 0x100, 0, false), 0);
        assert_eq!(
            waiter.lock().unwrap().get_wait_result(),
            crate::hle::result::RESULT_SUCCESS.get_inner_value()
        );
    }

    #[test]
    fn clear_current_request_and_notify_is_shared_by_receive_failure_path() {
        let mut process = KProcess::new();
        let waiter = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut waiter_guard = waiter.lock().unwrap();
            waiter_guard.thread_id = 11;
            waiter_guard.object_id = 12;
            waiter_guard.synchronization_wait.begin(vec![0x1000]);
            waiter_guard.synchronization_wait.bind_thread(11);
            waiter_guard.begin_wait();
        }
        process.register_thread_object(Arc::clone(&waiter));
        process.register_session_object(0x1000, Arc::new(Mutex::new(KSession::new())));

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.current_request = Some(Arc::new(Mutex::new(KSessionRequest::new())));
        server.link_waiter(&mut process, 11);
        server
            .request_list
            .push_back(Arc::new(Mutex::new(KSessionRequest::new())));

        server.clear_current_request_and_notify();

        assert!(server.current_request.is_none());
        assert_eq!(
            waiter.lock().unwrap().get_wait_result(),
            crate::hle::result::RESULT_SUCCESS.get_inner_value()
        );
    }

    #[test]
    fn cleanup_requests_ends_sync_client_wait_with_session_closed() {
        let mut process = crate::hle::kernel::k_process::KProcess::new();
        process.process_id = 9;
        let process = Arc::new(Mutex::new(process));

        let client_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread_guard = client_thread.lock().unwrap();
            thread_guard.thread_id = 7;
            thread_guard.object_id = 8;
            thread_guard.parent = Some(Arc::downgrade(&process));
            thread_guard.begin_wait();
        }
        process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&client_thread));

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&process));

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request_guard = request.lock().unwrap();
            request_guard.thread_id = Some(7);
            request_guard.client_process_id = Some(9);
        }
        server.current_request = Some(request);

        server.cleanup_requests();

        let client_thread = client_thread.lock().unwrap();
        assert_eq!(
            client_thread.get_wait_result(),
            RESULT_SESSION_CLOSED.get_inner_value()
        );
    }

    #[test]
    fn cleanup_map_succeeds_without_resolved_processes() {
        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request = request.lock().unwrap();
            request.set_server_process(0x1234);
            request.mappings.push_send(
                KProcessAddress::new(0x1000),
                KProcessAddress::new(0x2000),
                0x40,
                crate::hle::kernel::k_memory_block::KMemoryState::NORMAL,
            );
        }

        assert_eq!(
            KServerSession::cleanup_map(&request),
            crate::hle::result::RESULT_SUCCESS.get_inner_value()
        );
    }

    #[test]
    fn cleanup_server_handles_removes_only_move_handles_from_current_process() {
        let mut process = crate::hle::kernel::k_process::KProcess::new();
        process.process_id = 7;
        assert_eq!(process.initialize_handle_table(), 0);

        let copy_handle = process.handle_table.add(0x1111).unwrap();
        let move_handle = process.handle_table.add(0x2222).unwrap();

        let thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = thread.lock().unwrap();
            thread.thread_id = 3;
            thread.object_id = 4;
            thread.tls_address = KProcessAddress::new(0x4000);
        }

        let process = Arc::new(Mutex::new(process));
        {
            thread.lock().unwrap().parent = Some(Arc::downgrade(&process));
            process
                .lock()
                .unwrap()
                .register_thread_object(Arc::clone(&thread));
        }

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&process));
        crate::hle::kernel::kernel::set_current_emu_thread(Some(&thread));

        let mut words = [0u32; MESSAGE_BUFFER_SIZE];
        let mut message = MessageBuffer::new(&mut words);
        let header = crate::hle::kernel::message_buffer::MessageHeader::new(
            0,
            true,
            0,
            0,
            0,
            0,
            0,
            crate::hle::kernel::message_buffer::ReceiveListCountType::None as u32,
        );
        let mut offset = message.set_message_header(&header);
        offset = message.set(offset, &[1u32 << 1 | 1u32 << 5]);
        offset = message.set_handle(offset, copy_handle);
        let _ = message.set_handle(offset, move_handle);

        let bytes: Vec<u8> = words.iter().flat_map(|word| word.to_le_bytes()).collect();
        process.lock().unwrap().write_block(0x4000, &bytes);

        assert_eq!(KServerSession::cleanup_server_handles(0, 0, 0), 0);
        let process = process.lock().unwrap();
        assert!(process.handle_table.get_object(copy_handle).is_some());
        assert!(process.handle_table.get_object(move_handle).is_none());

        crate::hle::kernel::kernel::set_current_emu_thread(None);
    }

    #[test]
    fn receive_request_with_message_copies_raw_request_payload_for_simple_message() {
        let mut client_process = crate::hle::kernel::k_process::KProcess::new();
        client_process.process_id = 7;
        let client_process = Arc::new(Mutex::new(client_process));

        let client_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = client_thread.lock().unwrap();
            thread.thread_id = 3;
            thread.object_id = 4;
            thread.tls_address = KProcessAddress::new(0x3000);
            thread.parent = Some(Arc::downgrade(&client_process));
        }
        client_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&client_thread));

        let mut server_process = crate::hle::kernel::k_process::KProcess::new();
        server_process.process_id = 8;
        let server_process = Arc::new(Mutex::new(server_process));

        let server_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = server_thread.lock().unwrap();
            thread.thread_id = 5;
            thread.object_id = 6;
            thread.tls_address = KProcessAddress::new(0x5000);
            thread.parent = Some(Arc::downgrade(&server_process));
        }
        server_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&server_thread));

        let mut request_words = [0u32; MESSAGE_BUFFER_SIZE];
        {
            let mut message = MessageBuffer::new(&mut request_words);
            let header = crate::hle::kernel::message_buffer::MessageHeader::new(
                0x1234,
                false,
                0,
                0,
                0,
                0,
                2,
                crate::hle::kernel::message_buffer::ReceiveListCountType::None as u32,
            );
            let mut offset = message.set_message_header(&header);
            offset = message.set(offset, &[0xAAAA_BBBB, 0xCCCC_DDDD]);
            assert_eq!(offset, 4);
        }
        let request_bytes: Vec<u8> = request_words[..4]
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        client_process
            .lock()
            .unwrap()
            .write_block(0x3000, &request_bytes);

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&server_process));
        crate::hle::kernel::kernel::set_current_emu_thread(Some(&server_thread));

        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request = request.lock().unwrap();
            request.thread_id = Some(3);
            request.client_process_id = Some(7);
            request.address = 0;
            request.size = 0;
        }

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.request_list.push_back(request);

        assert_eq!(server.receive_request_with_message(0, 0, 0), 0);

        let copied = server_process
            .lock()
            .unwrap()
            .read_block(0x5000, 16)
            .to_vec();
        assert_eq!(copied, request_bytes);

        crate::hle::kernel::kernel::set_current_emu_thread(None);
    }

    #[test]
    fn send_reply_with_message_copies_raw_reply_payload_for_simple_message() {
        let mut client_process = crate::hle::kernel::k_process::KProcess::new();
        client_process.process_id = 7;
        let client_process = Arc::new(Mutex::new(client_process));

        let client_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = client_thread.lock().unwrap();
            thread.thread_id = 3;
            thread.object_id = 4;
            thread.tls_address = KProcessAddress::new(0x3000);
            thread.parent = Some(Arc::downgrade(&client_process));
            thread.begin_wait();
        }
        client_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&client_thread));

        let mut server_process = crate::hle::kernel::k_process::KProcess::new();
        server_process.process_id = 8;
        let server_process = Arc::new(Mutex::new(server_process));

        let server_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = server_thread.lock().unwrap();
            thread.thread_id = 5;
            thread.object_id = 6;
            thread.tls_address = KProcessAddress::new(0x5000);
            thread.parent = Some(Arc::downgrade(&server_process));
        }
        server_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&server_thread));

        let mut reply_words = [0u32; MESSAGE_BUFFER_SIZE];
        {
            let mut message = MessageBuffer::new(&mut reply_words);
            let header = crate::hle::kernel::message_buffer::MessageHeader::new(
                0x4321,
                false,
                0,
                0,
                0,
                0,
                2,
                crate::hle::kernel::message_buffer::ReceiveListCountType::None as u32,
            );
            let mut offset = message.set_message_header(&header);
            offset = message.set(offset, &[0x1111_2222, 0x3333_4444]);
            assert_eq!(offset, 4);
        }
        let reply_bytes: Vec<u8> = reply_words[..4]
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        server_process
            .lock()
            .unwrap()
            .write_block(0x5000, &reply_bytes);

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&server_process));
        crate::hle::kernel::kernel::set_current_emu_thread(Some(&server_thread));

        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request = request.lock().unwrap();
            request.thread_id = Some(3);
            request.client_process_id = Some(7);
            request.address = 0;
            request.size = 0;
        }

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.current_request = Some(request);

        assert_eq!(server.send_reply_with_message(0, 0, 0, false), 0);

        let copied = client_process
            .lock()
            .unwrap()
            .read_block(0x3000, 16)
            .to_vec();
        assert_eq!(copied, reply_bytes);

        crate::hle::kernel::kernel::set_current_emu_thread(None);
    }

    #[test]
    fn receive_request_with_message_translates_copy_handles_for_simple_special_header() {
        let mut client_process = crate::hle::kernel::k_process::KProcess::new();
        client_process.process_id = 7;
        assert_eq!(client_process.initialize_handle_table(), 0);
        let copied_object_id = 0xABCD;
        let copy_handle = client_process.handle_table.add(copied_object_id).unwrap();
        let client_process = Arc::new(Mutex::new(client_process));

        let client_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = client_thread.lock().unwrap();
            thread.thread_id = 3;
            thread.object_id = 4;
            thread.tls_address = KProcessAddress::new(0x3000);
            thread.parent = Some(Arc::downgrade(&client_process));
        }
        client_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&client_thread));

        let mut server_process = crate::hle::kernel::k_process::KProcess::new();
        server_process.process_id = 8;
        assert_eq!(server_process.initialize_handle_table(), 0);
        let server_process = Arc::new(Mutex::new(server_process));

        let server_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = server_thread.lock().unwrap();
            thread.thread_id = 5;
            thread.object_id = 6;
            thread.tls_address = KProcessAddress::new(0x5000);
            thread.parent = Some(Arc::downgrade(&server_process));
        }
        server_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&server_thread));

        let mut request_words = [0u32; MESSAGE_BUFFER_SIZE];
        {
            let mut message = MessageBuffer::new(&mut request_words);
            let header = crate::hle::kernel::message_buffer::MessageHeader::new(
                0x1234,
                true,
                0,
                0,
                0,
                0,
                0,
                crate::hle::kernel::message_buffer::ReceiveListCountType::None as u32,
            );
            let special = crate::hle::kernel::message_buffer::SpecialHeader::new(true, 1, 0);
            let mut offset = message.set_message_header(&header);
            offset = message.set(offset, &[1u32 | (1u32 << 1)]);
            offset = message.set_process_id(offset, 0);
            let _ = message.set_handle(offset, copy_handle);
            let _ = special;
        }
        let request_bytes: Vec<u8> = request_words[..5]
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        client_process
            .lock()
            .unwrap()
            .write_block(0x3000, &request_bytes);

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&server_process));
        crate::hle::kernel::kernel::set_current_emu_thread(Some(&server_thread));

        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request = request.lock().unwrap();
            request.thread_id = Some(3);
            request.client_process_id = Some(7);
        }

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.request_list.push_back(request);

        assert_eq!(server.receive_request_with_message(0, 0, 0), 0);

        let copied_words = {
            let copied = server_process
                .lock()
                .unwrap()
                .read_block(0x5000, 20)
                .to_vec();
            copied
                .chunks_exact(4)
                .map(|chunk| u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]))
                .collect::<Vec<_>>()
        };
        let copied_message = MessageBuffer::new(&mut copied_words.clone());
        assert_eq!(copied_message.get_process_id(3), 7);
        let translated_handle = copied_message.get_handle(5);
        let server_process = server_process.lock().unwrap();
        assert_eq!(
            server_process.handle_table.get_object(translated_handle),
            Some(copied_object_id)
        );

        crate::hle::kernel::kernel::set_current_emu_thread(None);
    }

    #[test]
    fn send_reply_with_message_translates_move_handles_for_simple_special_header() {
        let mut client_process = crate::hle::kernel::k_process::KProcess::new();
        client_process.process_id = 7;
        assert_eq!(client_process.initialize_handle_table(), 0);
        let client_process = Arc::new(Mutex::new(client_process));

        let client_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = client_thread.lock().unwrap();
            thread.thread_id = 3;
            thread.object_id = 4;
            thread.tls_address = KProcessAddress::new(0x3000);
            thread.parent = Some(Arc::downgrade(&client_process));
            thread.begin_wait();
        }
        client_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&client_thread));

        let mut server_process = crate::hle::kernel::k_process::KProcess::new();
        server_process.process_id = 8;
        assert_eq!(server_process.initialize_handle_table(), 0);
        let moved_object_id = 0xDCBA;
        let move_handle = server_process.handle_table.add(moved_object_id).unwrap();
        let server_process = Arc::new(Mutex::new(server_process));

        let server_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = server_thread.lock().unwrap();
            thread.thread_id = 5;
            thread.object_id = 6;
            thread.tls_address = KProcessAddress::new(0x5000);
            thread.parent = Some(Arc::downgrade(&server_process));
        }
        server_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&server_thread));

        let mut reply_words = [0u32; MESSAGE_BUFFER_SIZE];
        {
            let mut message = MessageBuffer::new(&mut reply_words);
            let header = crate::hle::kernel::message_buffer::MessageHeader::new(
                0x4321,
                true,
                0,
                0,
                0,
                0,
                0,
                crate::hle::kernel::message_buffer::ReceiveListCountType::None as u32,
            );
            let mut offset = message.set_message_header(&header);
            offset = message.set(offset, &[1u32 << 5]);
            let _ = message.set_handle(offset, move_handle);
        }
        let reply_bytes: Vec<u8> = reply_words[..4]
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        server_process
            .lock()
            .unwrap()
            .write_block(0x5000, &reply_bytes);

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&server_process));
        crate::hle::kernel::kernel::set_current_emu_thread(Some(&server_thread));

        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request = request.lock().unwrap();
            request.thread_id = Some(3);
            request.client_process_id = Some(7);
        }

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.current_request = Some(request);

        assert_eq!(server.send_reply_with_message(0, 0, 0, false), 0);

        let copied_words = {
            let copied = client_process
                .lock()
                .unwrap()
                .read_block(0x3000, 16)
                .to_vec();
            copied
                .chunks_exact(4)
                .map(|chunk| u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]))
                .collect::<Vec<_>>()
        };
        let copied_message = MessageBuffer::new(&mut copied_words.clone());
        let translated_handle = copied_message.get_handle(3);
        let client_process = client_process.lock().unwrap();
        assert_eq!(
            client_process.handle_table.get_object(translated_handle),
            Some(moved_object_id)
        );
        drop(client_process);
        assert!(server_process
            .lock()
            .unwrap()
            .handle_table
            .get_object(move_handle)
            .is_none());

        crate::hle::kernel::kernel::set_current_emu_thread(None);
    }

    #[test]
    fn receive_request_with_message_copies_pointer_descriptor_payload() {
        let mut client_process = crate::hle::kernel::k_process::KProcess::new();
        client_process.process_id = 7;
        let client_process = Arc::new(Mutex::new(client_process));

        let client_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = client_thread.lock().unwrap();
            thread.thread_id = 3;
            thread.object_id = 4;
            thread.tls_address = KProcessAddress::new(0x3000);
            thread.parent = Some(Arc::downgrade(&client_process));
        }
        client_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&client_thread));
        client_process
            .lock()
            .unwrap()
            .write_block(0x3010, &[0xAA, 0xBB, 0xCC, 0xDD]);

        let mut server_process = crate::hle::kernel::k_process::KProcess::new();
        server_process.process_id = 8;
        let server_process = Arc::new(Mutex::new(server_process));

        let server_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = server_thread.lock().unwrap();
            thread.thread_id = 5;
            thread.object_id = 6;
            thread.tls_address = KProcessAddress::new(0x5000);
            thread.parent = Some(Arc::downgrade(&server_process));
        }
        server_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&server_thread));

        let request_words = [1u32 << 16, 0, (4u32 << 16), 0x3010];
        let request_bytes: Vec<u8> = request_words
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        client_process
            .lock()
            .unwrap()
            .write_block(0x3000, &request_bytes);

        let server_buffer_words = [
            0u32,
            ((crate::hle::kernel::message_buffer::ReceiveListCountType::ToSingleBuffer as u32)
                << 10)
                | (4u32 << 20),
            0,
            0,
            0x6000,
            4u32 << 16,
        ];
        let server_buffer_bytes: Vec<u8> = server_buffer_words
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        server_process
            .lock()
            .unwrap()
            .write_block(0x5000, &server_buffer_bytes);

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&server_process));
        crate::hle::kernel::kernel::set_current_emu_thread(Some(&server_thread));

        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request = request.lock().unwrap();
            request.thread_id = Some(3);
            request.client_process_id = Some(7);
        }

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.request_list.push_back(request);

        assert_eq!(server.receive_request_with_message(0, 0, 0), 0);
        assert_eq!(
            server_process.lock().unwrap().read_block(0x6000, 4),
            &[0xAA, 0xBB, 0xCC, 0xDD]
        );

        crate::hle::kernel::kernel::set_current_emu_thread(None);
    }

    #[test]
    fn send_reply_with_message_copies_pointer_descriptor_payload() {
        let mut client_process = crate::hle::kernel::k_process::KProcess::new();
        client_process.process_id = 7;
        let client_process = Arc::new(Mutex::new(client_process));

        let client_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = client_thread.lock().unwrap();
            thread.thread_id = 3;
            thread.object_id = 4;
            thread.tls_address = KProcessAddress::new(0x3000);
            thread.parent = Some(Arc::downgrade(&client_process));
            thread.begin_wait();
        }
        client_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&client_thread));

        let mut server_process = crate::hle::kernel::k_process::KProcess::new();
        server_process.process_id = 8;
        let server_process = Arc::new(Mutex::new(server_process));

        let server_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = server_thread.lock().unwrap();
            thread.thread_id = 5;
            thread.object_id = 6;
            thread.tls_address = KProcessAddress::new(0x5000);
            thread.parent = Some(Arc::downgrade(&server_process));
        }
        server_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&server_thread));
        server_process
            .lock()
            .unwrap()
            .write_block(0x5010, &[0x11, 0x22, 0x33, 0x44]);

        let reply_words = [1u32 << 16, 0, (4u32 << 16), 0x5010];
        let reply_bytes: Vec<u8> = reply_words
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        server_process
            .lock()
            .unwrap()
            .write_block(0x5000, &reply_bytes);

        let client_buffer_words = [
            0u32,
            ((crate::hle::kernel::message_buffer::ReceiveListCountType::ToSingleBuffer as u32)
                << 10)
                | (4u32 << 20),
            0,
            0,
            0x7000,
            4u32 << 16,
        ];
        let client_buffer_bytes: Vec<u8> = client_buffer_words
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        client_process
            .lock()
            .unwrap()
            .write_block(0x3000, &client_buffer_bytes);

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&server_process));
        crate::hle::kernel::kernel::set_current_emu_thread(Some(&server_thread));

        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request = request.lock().unwrap();
            request.thread_id = Some(3);
            request.client_process_id = Some(7);
        }

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.current_request = Some(request);

        assert_eq!(server.send_reply_with_message(0, 0, 0, false), 0);
        assert_eq!(
            client_process.lock().unwrap().read_block(0x7000, 4),
            &[0x11, 0x22, 0x33, 0x44]
        );

        crate::hle::kernel::kernel::set_current_emu_thread(None);
    }

    #[test]
    fn send_reply_with_message_rejects_invalid_destination_header_size() {
        let mut client_process = crate::hle::kernel::k_process::KProcess::new();
        client_process.process_id = 7;
        let client_process = Arc::new(Mutex::new(client_process));

        let client_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = client_thread.lock().unwrap();
            thread.thread_id = 3;
            thread.object_id = 4;
            thread.tls_address = KProcessAddress::new(0x3000);
            thread.parent = Some(Arc::downgrade(&client_process));
            thread.begin_wait();
        }
        client_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&client_thread));

        let mut server_process = crate::hle::kernel::k_process::KProcess::new();
        server_process.process_id = 8;
        let server_process = Arc::new(Mutex::new(server_process));

        let server_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = server_thread.lock().unwrap();
            thread.thread_id = 5;
            thread.object_id = 6;
            thread.tls_address = KProcessAddress::new(0x5000);
            thread.parent = Some(Arc::downgrade(&server_process));
        }
        server_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&server_thread));

        let mut reply_words = [0u32; MESSAGE_BUFFER_SIZE];
        {
            let mut message = MessageBuffer::new(&mut reply_words);
            let header = crate::hle::kernel::message_buffer::MessageHeader::new(
                0x4321,
                false,
                0,
                0,
                0,
                0,
                2,
                crate::hle::kernel::message_buffer::ReceiveListCountType::None as u32,
            );
            let mut offset = message.set_message_header(&header);
            offset = message.set(offset, &[0x1111_2222, 0x3333_4444]);
            assert_eq!(offset, 4);
        }
        let reply_bytes: Vec<u8> = reply_words[..4]
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        server_process
            .lock()
            .unwrap()
            .write_block(0x5000, &reply_bytes);

        let client_buffer_words = [0xFFFF_FFFFu32, 0];
        let client_buffer_bytes: Vec<u8> = client_buffer_words
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        client_process
            .lock()
            .unwrap()
            .write_block(0x3000, &client_buffer_bytes);

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&server_process));
        crate::hle::kernel::kernel::set_current_emu_thread(Some(&server_thread));

        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request = request.lock().unwrap();
            request.thread_id = Some(3);
            request.client_process_id = Some(7);
        }

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.current_request = Some(request);

        assert_eq!(server.send_reply_with_message(0, 0, 0, false), 0);
        assert_eq!(
            client_thread.lock().unwrap().get_wait_result(),
            RESULT_INVALID_COMBINATION.get_inner_value()
        );

        crate::hle::kernel::kernel::set_current_emu_thread(None);
    }

    #[test]
    fn send_reply_with_message_rejects_invalid_destination_receive_list_offset() {
        let mut client_process = crate::hle::kernel::k_process::KProcess::new();
        client_process.process_id = 7;
        let client_process = Arc::new(Mutex::new(client_process));

        let client_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = client_thread.lock().unwrap();
            thread.thread_id = 3;
            thread.object_id = 4;
            thread.tls_address = KProcessAddress::new(0x3000);
            thread.parent = Some(Arc::downgrade(&client_process));
            thread.begin_wait();
        }
        client_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&client_thread));

        let mut server_process = crate::hle::kernel::k_process::KProcess::new();
        server_process.process_id = 8;
        let server_process = Arc::new(Mutex::new(server_process));

        let server_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = server_thread.lock().unwrap();
            thread.thread_id = 5;
            thread.object_id = 6;
            thread.tls_address = KProcessAddress::new(0x5000);
            thread.parent = Some(Arc::downgrade(&server_process));
        }
        server_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&server_thread));

        let mut reply_words = [0u32; MESSAGE_BUFFER_SIZE];
        {
            let mut message = MessageBuffer::new(&mut reply_words);
            let header = crate::hle::kernel::message_buffer::MessageHeader::new(
                0x4321,
                false,
                0,
                0,
                0,
                0,
                2,
                crate::hle::kernel::message_buffer::ReceiveListCountType::None as u32,
            );
            let mut offset = message.set_message_header(&header);
            offset = message.set(offset, &[0x1111_2222, 0x3333_4444]);
            assert_eq!(offset, 4);
        }
        let reply_bytes: Vec<u8> = reply_words[..4]
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        server_process
            .lock()
            .unwrap()
            .write_block(0x5000, &reply_bytes);

        let client_buffer_words = [0u32, 1u32 << 2];
        let client_buffer_bytes: Vec<u8> = client_buffer_words
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        client_process
            .lock()
            .unwrap()
            .write_block(0x3000, &client_buffer_bytes);

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&server_process));
        crate::hle::kernel::kernel::set_current_emu_thread(Some(&server_thread));

        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request = request.lock().unwrap();
            request.thread_id = Some(3);
            request.client_process_id = Some(7);
        }

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.current_request = Some(request);

        assert_eq!(server.send_reply_with_message(0, 0, 0, false), 0);
        assert_eq!(
            client_thread.lock().unwrap().get_wait_result(),
            RESULT_INVALID_COMBINATION.get_inner_value()
        );

        crate::hle::kernel::kernel::set_current_emu_thread(None);
    }

    #[test]
    fn send_reply_with_message_cleans_destination_handles_on_special_data_failure() {
        let mut client_process = crate::hle::kernel::k_process::KProcess::new();
        client_process.process_id = 7;
        assert_eq!(client_process.initialize_handle_table(), 0);
        let client_process = Arc::new(Mutex::new(client_process));

        let client_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = client_thread.lock().unwrap();
            thread.thread_id = 3;
            thread.object_id = 4;
            thread.tls_address = KProcessAddress::new(0x3000);
            thread.parent = Some(Arc::downgrade(&client_process));
            thread.begin_wait();
        }
        client_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&client_thread));

        let mut server_process = crate::hle::kernel::k_process::KProcess::new();
        server_process.process_id = 8;
        assert_eq!(server_process.initialize_handle_table(), 0);
        let moved_object_id = 0xDCBA;
        let valid_handle = server_process.handle_table.add(moved_object_id).unwrap();
        let invalid_handle = 0x1234u32;
        let server_process = Arc::new(Mutex::new(server_process));

        let server_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = server_thread.lock().unwrap();
            thread.thread_id = 5;
            thread.object_id = 6;
            thread.tls_address = KProcessAddress::new(0x5000);
            thread.parent = Some(Arc::downgrade(&server_process));
        }
        server_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&server_thread));

        let mut reply_words = [0u32; MESSAGE_BUFFER_SIZE];
        {
            let mut message = MessageBuffer::new(&mut reply_words);
            let header = crate::hle::kernel::message_buffer::MessageHeader::new(
                0x4321,
                true,
                0,
                0,
                0,
                0,
                0,
                crate::hle::kernel::message_buffer::ReceiveListCountType::None as u32,
            );
            let mut offset = message.set_message_header(&header);
            offset = message.set(offset, &[2u32 | (2u32 << 5)]);
            offset = message.set_handle(offset, valid_handle);
            let _ = message.set_handle(offset, invalid_handle);
        }
        let reply_bytes: Vec<u8> = reply_words[..5]
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        server_process
            .lock()
            .unwrap()
            .write_block(0x5000, &reply_bytes);

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&server_process));
        crate::hle::kernel::kernel::set_current_emu_thread(Some(&server_thread));

        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request = request.lock().unwrap();
            request.thread_id = Some(3);
            request.client_process_id = Some(7);
        }

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.current_request = Some(request);

        assert_eq!(server.send_reply_with_message(0, 0, 0, false), 0);
        assert_eq!(
            client_thread.lock().unwrap().get_wait_result(),
            crate::hle::kernel::svc::svc_results::RESULT_INVALID_HANDLE.get_inner_value()
        );
        assert!(client_process
            .lock()
            .unwrap()
            .handle_table
            .get_object(crate::hle::kernel::k_handle_table::encode_handle(0, 1))
            .is_none());

        crate::hle::kernel::kernel::set_current_emu_thread(None);
    }

    #[test]
    fn cleanup_special_data_removes_destination_handles_and_clears_words() {
        let mut process = crate::hle::kernel::k_process::KProcess::new();
        process.process_id = 7;
        assert_eq!(process.initialize_handle_table(), 0);

        let copy_handle = process.handle_table.add(0xAAA0).unwrap();
        let move_handle = process.handle_table.add(0xBBB0).unwrap();

        let mut words = [0u32; MESSAGE_BUFFER_SIZE];
        {
            let mut message = MessageBuffer::new(&mut words);
            let header = crate::hle::kernel::message_buffer::MessageHeader::new(
                0x1234,
                true,
                0,
                0,
                0,
                0,
                0,
                crate::hle::kernel::message_buffer::ReceiveListCountType::None as u32,
            );
            let mut offset = message.set_message_header(&header);
            offset = message.set(offset, &[1u32 | (1u32 << 1) | (1u32 << 5)]);
            offset = message.set_process_id(offset, process.process_id);
            offset = message.set_handle(offset, copy_handle);
            let _ = message.set_handle(offset, move_handle);
        }

        KServerSession::cleanup_special_data(
            &mut process,
            &mut words,
            MESSAGE_BUFFER_SIZE * std::mem::size_of::<u32>(),
        );

        let message = MessageBuffer::new(&mut words);
        assert_eq!(message.get_process_id(3), 0);
        assert_eq!(message.get_handle(5), INVALID_HANDLE);
        assert_eq!(message.get_handle(6), INVALID_HANDLE);
        assert!(process.handle_table.get_object(copy_handle).is_none());
        assert!(process.handle_table.get_object(move_handle).is_none());
    }

    #[test]
    fn restore_destination_header_restores_original_words() {
        let mut words = [0u32; MESSAGE_BUFFER_SIZE];
        words[0] = 0xDEAD_BEEF;
        words[1] = 0xCAFE_BABE;
        words[2] = 0xFFFF_FFFF;

        let header = crate::hle::kernel::message_buffer::MessageHeader::new(
            0x1234,
            true,
            0,
            0,
            0,
            0,
            0,
            crate::hle::kernel::message_buffer::ReceiveListCountType::None as u32,
        );
        let special = crate::hle::kernel::message_buffer::SpecialHeader::new(true, 2, 3);

        KServerSession::restore_destination_header(&mut words, &header, &special);

        assert_eq!(words[0], header.get_data()[0]);
        assert_eq!(words[1], header.get_data()[1]);
        assert_eq!(words[2], 1u32 | (2u32 << 1) | (3u32 << 5));
    }

    #[test]
    fn mark_receive_list_broken_matches_upstream_boundary() {
        assert!(!KServerSession::mark_receive_list_broken(4, 4));
        assert!(KServerSession::mark_receive_list_broken(5, 4));
    }

    #[test]
    fn async_cleanup_result_matches_upstream_selection() {
        assert_eq!(
            KServerSession::async_cleanup_result(
                crate::hle::result::RESULT_SUCCESS.get_inner_value()
            )
            .get_inner_value(),
            RESULT_SESSION_CLOSED.get_inner_value()
        );
        assert_eq!(
            KServerSession::async_cleanup_result(RESULT_INVALID_STATE.get_inner_value())
                .get_inner_value(),
            RESULT_INVALID_STATE.get_inner_value()
        );
    }

    #[test]
    fn send_reply_closed_cleanup_failure_returns_success_to_server() {
        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.client_closed = true;

        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request = request.lock().unwrap();
            request.set_server_process(0x1234);
            request.mappings.push_send(
                KProcessAddress::new(0x1000),
                KProcessAddress::new(0x2000),
                0x40,
                KMemoryState::NORMAL,
            );
        }
        server.current_request = Some(request);

        assert_eq!(
            server.send_reply_with_message(0, 0, 0, false),
            crate::hle::result::RESULT_SUCCESS.get_inner_value()
        );
    }

    #[test]
    fn process_send_message_receive_mapping_skips_aligned_middle_pages() {
        let mut src_process = KProcess::new();
        let mut dst_process = KProcess::new();

        let src_addr = 0x4000u64;
        let dst_addr = 0x8000u64;
        let size = PAGE_SIZE * 2;

        src_process.write_block(src_addr, &vec![0xAA; size]);
        dst_process.write_block(dst_addr, &vec![0x55; size]);

        KServerSession::process_send_message_receive_mapping(
            &src_process,
            &mut dst_process,
            KProcessAddress::new(dst_addr),
            KProcessAddress::new(src_addr),
            size,
            KMemoryState::IPC,
        );

        assert_eq!(dst_process.read_block(dst_addr, size), vec![0x55; size]);
    }

    #[test]
    fn process_send_message_receive_mapping_copies_only_unaligned_edges() {
        let mut src_process = KProcess::new();
        let mut dst_process = KProcess::new();

        let src_addr = 0x5003u64;
        let dst_addr = 0x9003u64;
        let size = PAGE_SIZE * 2;

        let mut src_bytes = vec![0x11; size];
        src_bytes[..PAGE_SIZE - 3].fill(0xAA);
        src_bytes[PAGE_SIZE - 3..PAGE_SIZE + 3].fill(0x11);
        src_bytes[PAGE_SIZE + 3..].fill(0xCC);
        src_process.write_block(src_addr, &src_bytes);
        dst_process.write_block(dst_addr, &vec![0x55; size]);

        KServerSession::process_send_message_receive_mapping(
            &src_process,
            &mut dst_process,
            KProcessAddress::new(dst_addr),
            KProcessAddress::new(src_addr),
            size,
            KMemoryState::IPC,
        );

        let copied = dst_process.read_block(dst_addr, size);
        assert_eq!(&copied[..PAGE_SIZE - 3], &vec![0xAA; PAGE_SIZE - 3]);
        assert_eq!(&copied[PAGE_SIZE - 3..PAGE_SIZE + 3], &vec![0x55; 6]);
        assert_eq!(&copied[PAGE_SIZE + 3..], &vec![0xCC; PAGE_SIZE - 3]);
    }

    #[test]
    fn receive_request_with_message_records_map_alias_mapping() {
        let mut client_process = crate::hle::kernel::k_process::KProcess::new();
        client_process.process_id = 7;
        let client_process = Arc::new(Mutex::new(client_process));

        let client_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = client_thread.lock().unwrap();
            thread.thread_id = 3;
            thread.object_id = 4;
            thread.tls_address = KProcessAddress::new(0x3000);
            thread.parent = Some(Arc::downgrade(&client_process));
        }
        client_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&client_thread));

        let mut server_process = crate::hle::kernel::k_process::KProcess::new();
        server_process.process_id = 8;
        let server_process = Arc::new(Mutex::new(server_process));

        let server_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = server_thread.lock().unwrap();
            thread.thread_id = 5;
            thread.object_id = 6;
            thread.tls_address = KProcessAddress::new(0x5000);
            thread.parent = Some(Arc::downgrade(&server_process));
        }
        server_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&server_thread));

        let map_words = Self::encode_map_alias_descriptor(
            0x3010,
            0x40,
            crate::hle::kernel::message_buffer::MapAliasAttribute::Ipc as u32,
        );
        let request_words = [1u32 << 20, 0, map_words[0], map_words[1], map_words[2]];
        let request_bytes: Vec<u8> = request_words
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        client_process
            .lock()
            .unwrap()
            .write_block(0x3000, &request_bytes);

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&server_process));
        crate::hle::kernel::kernel::set_current_emu_thread(Some(&server_thread));

        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request = request.lock().unwrap();
            request.thread_id = Some(3);
            request.client_process_id = Some(7);
        }

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.request_list.push_back(Arc::clone(&request));

        assert_eq!(server.receive_request_with_message(0, 0, 0), 0);
        let request = request.lock().unwrap();
        assert_eq!(request.mappings.get_send_count(), 1);
        let mapping = request.mappings.get_send_mapping(0);
        assert_eq!(mapping.client_address.get(), 0x3010);
        assert_eq!(mapping.server_address.get(), 0x3010);
        assert_eq!(mapping.size, 0x40);
        assert_eq!(mapping.state, KMemoryState::IPC);

        crate::hle::kernel::kernel::set_current_emu_thread(None);
    }

    #[test]
    fn send_reply_with_message_copies_receive_mapping_payload() {
        let mut client_process = crate::hle::kernel::k_process::KProcess::new();
        client_process.process_id = 7;
        let client_process = Arc::new(Mutex::new(client_process));

        let client_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = client_thread.lock().unwrap();
            thread.thread_id = 3;
            thread.object_id = 4;
            thread.tls_address = KProcessAddress::new(0x3000);
            thread.parent = Some(Arc::downgrade(&client_process));
            thread.begin_wait();
        }
        client_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&client_thread));

        let mut server_process = crate::hle::kernel::k_process::KProcess::new();
        server_process.process_id = 8;
        let server_process = Arc::new(Mutex::new(server_process));

        let server_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = server_thread.lock().unwrap();
            thread.thread_id = 5;
            thread.object_id = 6;
            thread.tls_address = KProcessAddress::new(0x5000);
            thread.parent = Some(Arc::downgrade(&server_process));
        }
        server_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&server_thread));
        server_process
            .lock()
            .unwrap()
            .write_block(0x5010, &[1, 2, 3, 4]);

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&server_process));
        crate::hle::kernel::kernel::set_current_emu_thread(Some(&server_thread));

        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request = request.lock().unwrap();
            request.thread_id = Some(3);
            request.client_process_id = Some(7);
            let _ = request.mappings.push_receive(
                KProcessAddress::new(0x7010),
                KProcessAddress::new(0x5010),
                4,
                KMemoryState::IPC,
            );
        }

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.current_request = Some(request);

        assert_eq!(server.send_reply_with_message(0, 0, 0, false), 0);
        assert_eq!(
            client_process.lock().unwrap().read_block(0x7010, 4),
            &[1, 2, 3, 4]
        );

        crate::hle::kernel::kernel::set_current_emu_thread(None);
    }

    #[test]
    fn receive_request_with_message_returns_receive_list_broken_on_transport_failure() {
        let mut client_process = crate::hle::kernel::k_process::KProcess::new();
        client_process.process_id = 7;
        let client_process = Arc::new(Mutex::new(client_process));

        let client_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = client_thread.lock().unwrap();
            thread.thread_id = 3;
            thread.object_id = 4;
            thread.tls_address = KProcessAddress::new(0x3000);
            thread.parent = Some(Arc::downgrade(&client_process));
        }
        client_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&client_thread));

        let mut server_process = crate::hle::kernel::k_process::KProcess::new();
        server_process.process_id = 8;
        let server_process = Arc::new(Mutex::new(server_process));

        let server_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = server_thread.lock().unwrap();
            thread.thread_id = 5;
            thread.object_id = 6;
            thread.tls_address = KProcessAddress::new(0x5000);
            thread.parent = Some(Arc::downgrade(&server_process));
        }
        server_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&server_thread));

        let request_words = [1u32 << 16, 0, (8u32 << 16), 0x3010];
        let request_bytes: Vec<u8> = request_words
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        client_process
            .lock()
            .unwrap()
            .write_block(0x3000, &request_bytes);

        let server_buffer_words = [
            0u32,
            ((crate::hle::kernel::message_buffer::ReceiveListCountType::ToSingleBuffer as u32)
                << 10)
                | (3u32 << 20),
            0,
            0x6000,
            4u32 << 16,
        ];
        let server_buffer_bytes: Vec<u8> = server_buffer_words
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        server_process
            .lock()
            .unwrap()
            .write_block(0x5000, &server_buffer_bytes);

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&server_process));
        crate::hle::kernel::kernel::set_current_emu_thread(Some(&server_thread));

        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request = request.lock().unwrap();
            request.thread_id = Some(3);
            request.client_process_id = Some(7);
        }

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.request_list.push_back(request);

        assert_eq!(
            server.receive_request_with_message(0, 0, 0),
            RESULT_RECEIVE_LIST_BROKEN.get_inner_value()
        );

        crate::hle::kernel::kernel::set_current_emu_thread(None);
    }

    #[test]
    fn receive_request_with_message_rejects_move_handles_from_client() {
        let mut client_process = crate::hle::kernel::k_process::KProcess::new();
        client_process.process_id = 7;
        assert_eq!(client_process.initialize_handle_table(), 0);
        let move_handle = client_process.handle_table.add(0xABCD).unwrap();
        let client_process = Arc::new(Mutex::new(client_process));

        let client_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = client_thread.lock().unwrap();
            thread.thread_id = 3;
            thread.object_id = 4;
            thread.tls_address = KProcessAddress::new(0x3000);
            thread.parent = Some(Arc::downgrade(&client_process));
        }
        client_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&client_thread));

        let mut server_process = crate::hle::kernel::k_process::KProcess::new();
        server_process.process_id = 8;
        let server_process = Arc::new(Mutex::new(server_process));

        let server_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = server_thread.lock().unwrap();
            thread.thread_id = 5;
            thread.object_id = 6;
            thread.tls_address = KProcessAddress::new(0x5000);
            thread.parent = Some(Arc::downgrade(&server_process));
        }
        server_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&server_thread));

        let mut request_words = [0u32; MESSAGE_BUFFER_SIZE];
        {
            let mut message = MessageBuffer::new(&mut request_words);
            let header = crate::hle::kernel::message_buffer::MessageHeader::new(
                0x1234,
                true,
                0,
                0,
                0,
                0,
                0,
                crate::hle::kernel::message_buffer::ReceiveListCountType::None as u32,
            );
            let mut offset = message.set_message_header(&header);
            offset = message.set(offset, &[1u32 << 5]);
            let _ = message.set_handle(offset, move_handle);
        }
        let request_bytes: Vec<u8> = request_words[..4]
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        client_process
            .lock()
            .unwrap()
            .write_block(0x3000, &request_bytes);

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&server_process));
        crate::hle::kernel::kernel::set_current_emu_thread(Some(&server_thread));

        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request = request.lock().unwrap();
            request.thread_id = Some(3);
            request.client_process_id = Some(7);
        }

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.request_list.push_back(request);

        assert_eq!(
            server.receive_request_with_message(0, 0, 0),
            RESULT_NOT_FOUND.get_inner_value()
        );

        crate::hle::kernel::kernel::set_current_emu_thread(None);
    }

    #[test]
    fn receive_request_with_message_cleans_server_handles_on_special_data_failure() {
        let mut client_process = crate::hle::kernel::k_process::KProcess::new();
        client_process.process_id = 7;
        assert_eq!(client_process.initialize_handle_table(), 0);
        let valid_handle = client_process.handle_table.add(0xABCD).unwrap();
        let invalid_handle = 0x1234u32;
        let client_process = Arc::new(Mutex::new(client_process));

        let client_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = client_thread.lock().unwrap();
            thread.thread_id = 3;
            thread.object_id = 4;
            thread.tls_address = KProcessAddress::new(0x3000);
            thread.parent = Some(Arc::downgrade(&client_process));
        }
        client_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&client_thread));

        let mut server_process = crate::hle::kernel::k_process::KProcess::new();
        server_process.process_id = 8;
        assert_eq!(server_process.initialize_handle_table(), 0);
        let server_process = Arc::new(Mutex::new(server_process));

        let server_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = server_thread.lock().unwrap();
            thread.thread_id = 5;
            thread.object_id = 6;
            thread.tls_address = KProcessAddress::new(0x5000);
            thread.parent = Some(Arc::downgrade(&server_process));
        }
        server_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&server_thread));

        let mut request_words = [0u32; MESSAGE_BUFFER_SIZE];
        {
            let mut message = MessageBuffer::new(&mut request_words);
            let header = crate::hle::kernel::message_buffer::MessageHeader::new(
                0x1234,
                true,
                0,
                0,
                0,
                0,
                0,
                crate::hle::kernel::message_buffer::ReceiveListCountType::None as u32,
            );
            let special = crate::hle::kernel::message_buffer::SpecialHeader::new(true, 2, 0);
            let mut offset = message.set_message_header(&header);
            offset = message.set(
                offset,
                &[1u32 | ((special.get_copy_handle_count() as u32) << 1)],
            );
            offset = message.set_process_id(offset, 0);
            offset = message.set_handle(offset, valid_handle);
            let _ = message.set_handle(offset, invalid_handle);
        }
        let request_bytes: Vec<u8> = request_words[..6]
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        client_process
            .lock()
            .unwrap()
            .write_block(0x3000, &request_bytes);

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&server_process));
        crate::hle::kernel::kernel::set_current_emu_thread(Some(&server_thread));

        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request = request.lock().unwrap();
            request.thread_id = Some(3);
            request.client_process_id = Some(7);
        }

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.request_list.push_back(request);

        assert_eq!(
            server.receive_request_with_message(0, 0, 0),
            RESULT_NOT_FOUND.get_inner_value()
        );
        assert!(server_process.lock().unwrap().handle_table.get_count() == 0);

        crate::hle::kernel::kernel::set_current_emu_thread(None);
    }

    #[test]
    fn receive_request_with_message_failure_clears_current_request_for_next_request() {
        let mut client_process = crate::hle::kernel::k_process::KProcess::new();
        client_process.process_id = 7;
        assert_eq!(client_process.initialize_handle_table(), 0);
        let move_handle = client_process.handle_table.add(0xABCD).unwrap();
        let client_process = Arc::new(Mutex::new(client_process));

        let client_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = client_thread.lock().unwrap();
            thread.thread_id = 3;
            thread.object_id = 4;
            thread.tls_address = KProcessAddress::new(0x3000);
            thread.parent = Some(Arc::downgrade(&client_process));
        }
        client_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&client_thread));

        let mut server_process = crate::hle::kernel::k_process::KProcess::new();
        server_process.process_id = 8;
        let server_process = Arc::new(Mutex::new(server_process));

        let server_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = server_thread.lock().unwrap();
            thread.thread_id = 5;
            thread.object_id = 6;
            thread.tls_address = KProcessAddress::new(0x5000);
            thread.parent = Some(Arc::downgrade(&server_process));
        }
        server_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&server_thread));

        let mut invalid_words = [0u32; MESSAGE_BUFFER_SIZE];
        {
            let mut message = MessageBuffer::new(&mut invalid_words);
            let header = crate::hle::kernel::message_buffer::MessageHeader::new(
                0x1234,
                true,
                0,
                0,
                0,
                0,
                0,
                crate::hle::kernel::message_buffer::ReceiveListCountType::None as u32,
            );
            let mut offset = message.set_message_header(&header);
            offset = message.set(offset, &[1u32 << 5]);
            let _ = message.set_handle(offset, move_handle);
        }
        let invalid_bytes: Vec<u8> = invalid_words[..4]
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();

        let mut valid_words = [0u32; MESSAGE_BUFFER_SIZE];
        {
            let mut message = MessageBuffer::new(&mut valid_words);
            let header = crate::hle::kernel::message_buffer::MessageHeader::new(
                0x9999,
                false,
                0,
                0,
                0,
                0,
                1,
                crate::hle::kernel::message_buffer::ReceiveListCountType::None as u32,
            );
            let mut offset = message.set_message_header(&header);
            let _ = message.set(offset, &[0xDEAD_BEEF]);
        }
        let valid_bytes: Vec<u8> = valid_words[..3]
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();

        client_process
            .lock()
            .unwrap()
            .write_block(0x3000, &invalid_bytes);

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&server_process));
        crate::hle::kernel::kernel::set_current_emu_thread(Some(&server_thread));

        let request1 = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request = request1.lock().unwrap();
            request.thread_id = Some(3);
            request.client_process_id = Some(7);
        }
        let request2 = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request = request2.lock().unwrap();
            request.thread_id = Some(3);
            request.client_process_id = Some(7);
        }

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.request_list.push_back(request1);
        server.request_list.push_back(request2);

        assert_eq!(
            server.receive_request_with_message(0, 0, 0),
            RESULT_NOT_FOUND.get_inner_value()
        );
        assert!(server.current_request.is_none());

        client_process
            .lock()
            .unwrap()
            .write_block(0x3000, &valid_bytes);

        assert_eq!(server.receive_request_with_message(0, 0, 0), 0);
        assert!(server.current_request.is_some());

        crate::hle::kernel::kernel::set_current_emu_thread(None);
    }

    #[test]
    fn receive_request_with_message_does_not_copy_raw_words_before_special_data_succeeds() {
        let mut client_process = crate::hle::kernel::k_process::KProcess::new();
        client_process.process_id = 7;
        assert_eq!(client_process.initialize_handle_table(), 0);
        let valid_handle = client_process.handle_table.add(0xABCD).unwrap();
        let invalid_handle = 0x1234u32;
        let client_process = Arc::new(Mutex::new(client_process));

        let client_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = client_thread.lock().unwrap();
            thread.thread_id = 3;
            thread.object_id = 4;
            thread.tls_address = KProcessAddress::new(0x3000);
            thread.parent = Some(Arc::downgrade(&client_process));
        }
        client_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&client_thread));

        let mut server_process = crate::hle::kernel::k_process::KProcess::new();
        server_process.process_id = 8;
        assert_eq!(server_process.initialize_handle_table(), 0);
        let server_process = Arc::new(Mutex::new(server_process));

        let server_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = server_thread.lock().unwrap();
            thread.thread_id = 5;
            thread.object_id = 6;
            thread.tls_address = KProcessAddress::new(0x5000);
            thread.parent = Some(Arc::downgrade(&server_process));
        }
        server_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&server_thread));

        let mut request_words = [0u32; MESSAGE_BUFFER_SIZE];
        {
            let mut message = MessageBuffer::new(&mut request_words);
            let header = crate::hle::kernel::message_buffer::MessageHeader::new(
                0x1234,
                true,
                0,
                0,
                0,
                0,
                2,
                crate::hle::kernel::message_buffer::ReceiveListCountType::None as u32,
            );
            let mut offset = message.set_message_header(&header);
            offset = message.set(offset, &[2u32 << 1]);
            offset = message.set_handle(offset, valid_handle);
            offset = message.set_handle(offset, invalid_handle);
            let _ = message.set(offset, &[0xDEAD_BEEF, 0xCAFE_BABE]);
        }
        let request_bytes: Vec<u8> = request_words[..7]
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        client_process
            .lock()
            .unwrap()
            .write_block(0x3000, &request_bytes);

        let server_buffer_words = [
            0u32,
            8u32 << 20,
            0xAAAA_AAAA,
            0xBBBB_BBBB,
            0xCCCC_CCCC,
            0x1111_1111,
            0x2222_2222,
            0x3333_3333,
        ];
        let server_buffer_bytes: Vec<u8> = server_buffer_words
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        server_process
            .lock()
            .unwrap()
            .write_block(0x5000, &server_buffer_bytes);

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&server_process));
        crate::hle::kernel::kernel::set_current_emu_thread(Some(&server_thread));

        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request = request.lock().unwrap();
            request.thread_id = Some(3);
            request.client_process_id = Some(7);
        }

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.request_list.push_back(request);

        assert_eq!(
            server.receive_request_with_message(0, 0, 0),
            RESULT_NOT_FOUND.get_inner_value()
        );

        let words = server_process
            .lock()
            .unwrap()
            .read_block(0x5000, server_buffer_bytes.len());
        let words: Vec<u32> = words
            .chunks_exact(4)
            .map(|chunk| u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]))
            .collect();
        assert_eq!(words[5], 0x1111_1111);
        assert_eq!(words[6], 0x2222_2222);

        crate::hle::kernel::kernel::set_current_emu_thread(None);
    }

    #[test]
    fn send_reply_with_message_does_not_copy_raw_words_before_special_data_succeeds() {
        let mut client_process = crate::hle::kernel::k_process::KProcess::new();
        client_process.process_id = 7;
        assert_eq!(client_process.initialize_handle_table(), 0);
        let client_process = Arc::new(Mutex::new(client_process));

        let client_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = client_thread.lock().unwrap();
            thread.thread_id = 3;
            thread.object_id = 4;
            thread.tls_address = KProcessAddress::new(0x3000);
            thread.parent = Some(Arc::downgrade(&client_process));
            thread.begin_wait();
        }
        client_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&client_thread));

        let mut server_process = crate::hle::kernel::k_process::KProcess::new();
        server_process.process_id = 8;
        assert_eq!(server_process.initialize_handle_table(), 0);
        let valid_handle = server_process.handle_table.add(0xDCBA).unwrap();
        let invalid_handle = 0x1234u32;
        let server_process = Arc::new(Mutex::new(server_process));

        let server_thread = Arc::new(Mutex::new(crate::hle::kernel::k_thread::KThread::new()));
        {
            let mut thread = server_thread.lock().unwrap();
            thread.thread_id = 5;
            thread.object_id = 6;
            thread.tls_address = KProcessAddress::new(0x5000);
            thread.parent = Some(Arc::downgrade(&server_process));
        }
        server_process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&server_thread));

        let mut reply_words = [0u32; MESSAGE_BUFFER_SIZE];
        {
            let mut message = MessageBuffer::new(&mut reply_words);
            let header = crate::hle::kernel::message_buffer::MessageHeader::new(
                0x4321,
                true,
                0,
                0,
                0,
                0,
                2,
                crate::hle::kernel::message_buffer::ReceiveListCountType::None as u32,
            );
            let mut offset = message.set_message_header(&header);
            offset = message.set(offset, &[2u32 << 5]);
            offset = message.set_handle(offset, valid_handle);
            offset = message.set_handle(offset, invalid_handle);
            let _ = message.set(offset, &[0xDEAD_BEEF, 0xCAFE_BABE]);
        }
        let reply_bytes: Vec<u8> = reply_words[..7]
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        server_process
            .lock()
            .unwrap()
            .write_block(0x5000, &reply_bytes);

        let client_buffer_words = [
            0u32,
            8u32 << 20,
            0xAAAA_AAAA,
            0xBBBB_BBBB,
            0xCCCC_CCCC,
            0x1111_1111,
            0x2222_2222,
            0x3333_3333,
        ];
        let client_buffer_bytes: Vec<u8> = client_buffer_words
            .iter()
            .flat_map(|word| word.to_le_bytes())
            .collect();
        client_process
            .lock()
            .unwrap()
            .write_block(0x3000, &client_buffer_bytes);

        let mut system = crate::core::System::new_for_test();
        system.set_current_process_arc(Arc::clone(&server_process));
        crate::hle::kernel::kernel::set_current_emu_thread(Some(&server_thread));

        let request = Arc::new(Mutex::new(KSessionRequest::new()));
        {
            let mut request = request.lock().unwrap();
            request.thread_id = Some(3);
            request.client_process_id = Some(7);
        }

        let mut server = KServerSession::new();
        server.initialize(0x1000);
        server.current_request = Some(request);

        assert_eq!(server.send_reply_with_message(0, 0, 0, false), 0);

        let words = client_process
            .lock()
            .unwrap()
            .read_block(0x3000, client_buffer_bytes.len());
        let words: Vec<u32> = words
            .chunks_exact(4)
            .map(|chunk| u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]))
            .collect();
        assert_eq!(words[5], 0x1111_1111);
        assert_eq!(words[6], 0x2222_2222);

        crate::hle::kernel::kernel::set_current_emu_thread(None);
    }
}
