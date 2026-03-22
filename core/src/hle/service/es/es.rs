// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/es/es.cpp
//!
//! ETicket service ("es").

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};
use crate::crypto::key_manager::{KeyManager, Key128, S128KeyType, Ticket, TicketData};
use crate::hle::result::{ErrorModule, ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// Upstream: `ERROR_INVALID_ARGUMENT{ErrorModule::ETicket, 2}`
pub const ERROR_INVALID_ARGUMENT: ResultCode =
    ResultCode::from_module_description(ErrorModule::ETicket, 2);

/// Upstream: `ERROR_INVALID_RIGHTS_ID{ErrorModule::ETicket, 3}`
pub const ERROR_INVALID_RIGHTS_ID: ResultCode =
    ResultCode::from_module_description(ErrorModule::ETicket, 3);

/// Serialize a Ticket to raw bytes, matching the upstream C++ memory layout.
///
/// Upstream `ctx.WriteBuffer(&ticket, write_size)` writes the raw struct bytes.
/// This function produces the same byte layout: sig_type (4 LE bytes) + sig_data + padding + TicketData.
fn ticket_to_bytes(ticket: &Ticket) -> Vec<u8> {
    match ticket {
        Ticket::Invalid => Vec::new(),
        Ticket::RSA4096 { sig_type, sig_data, _padding, data } => {
            let mut buf = Vec::with_capacity(0x500);
            buf.extend_from_slice(&(*sig_type as u32).to_le_bytes());
            buf.extend_from_slice(sig_data);
            buf.extend_from_slice(_padding);
            let data_bytes = unsafe {
                std::slice::from_raw_parts(
                    data as *const TicketData as *const u8,
                    std::mem::size_of::<TicketData>(),
                )
            };
            buf.extend_from_slice(data_bytes);
            buf
        }
        Ticket::RSA2048 { sig_type, sig_data, _padding, data } => {
            let mut buf = Vec::with_capacity(0x400);
            buf.extend_from_slice(&(*sig_type as u32).to_le_bytes());
            buf.extend_from_slice(sig_data);
            buf.extend_from_slice(_padding);
            let data_bytes = unsafe {
                std::slice::from_raw_parts(
                    data as *const TicketData as *const u8,
                    std::mem::size_of::<TicketData>(),
                )
            };
            buf.extend_from_slice(data_bytes);
            buf
        }
        Ticket::ECDSA { sig_type, sig_data, _padding, data } => {
            let mut buf = Vec::with_capacity(0x340);
            buf.extend_from_slice(&(*sig_type as u32).to_le_bytes());
            buf.extend_from_slice(sig_data);
            buf.extend_from_slice(_padding);
            let data_bytes = unsafe {
                std::slice::from_raw_parts(
                    data as *const TicketData as *const u8,
                    std::mem::size_of::<TicketData>(),
                )
            };
            buf.extend_from_slice(data_bytes);
            buf
        }
    }
}

/// ETicket service ("es").
pub struct ETicket {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    keys: Arc<Mutex<KeyManager>>,
}

impl ETicket {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (1, Some(ETicket::import_ticket_handler), "ImportTicket"),
            (2, None, "ImportTicketCertificateSet"),
            (3, None, "DeleteTicket"),
            (4, None, "DeletePersonalizedTicket"),
            (5, None, "DeleteAllCommonTicket"),
            (6, None, "DeleteAllPersonalizedTicket"),
            (7, None, "DeleteAllPersonalizedTicketEx"),
            (8, Some(ETicket::get_title_key_handler), "GetTitleKey"),
            (9, Some(ETicket::count_common_ticket_handler), "CountCommonTicket"),
            (10, Some(ETicket::count_personalized_ticket_handler), "CountPersonalizedTicket"),
            (11, Some(ETicket::list_common_ticket_rights_ids_handler), "ListCommonTicketRightsIds"),
            (12, Some(ETicket::list_personalized_ticket_rights_ids_handler), "ListPersonalizedTicketRightsIds"),
            (13, None, "ListMissingPersonalizedTicket"),
            (14, Some(ETicket::get_common_ticket_size_handler), "GetCommonTicketSize"),
            (15, Some(ETicket::get_personalized_ticket_size_handler), "GetPersonalizedTicketSize"),
            (16, Some(ETicket::get_common_ticket_data_handler), "GetCommonTicketData"),
            (17, Some(ETicket::get_personalized_ticket_data_handler), "GetPersonalizedTicketData"),
            (18, None, "OwnTicket"),
            (19, None, "GetTicketInfo"),
            (20, None, "ListLightTicketInfo"),
            (21, None, "SignData"),
            (22, None, "GetCommonTicketAndCertificateSize"),
            (23, None, "GetCommonTicketAndCertificateData"),
            (24, None, "ImportPrepurchaseRecord"),
            (25, None, "DeletePrepurchaseRecord"),
            (26, None, "DeleteAllPrepurchaseRecord"),
            (27, None, "CountPrepurchaseRecord"),
            (28, None, "ListPrepurchaseRecordRightsIds"),
            (29, None, "ListPrepurchaseRecordInfo"),
            (30, None, "CountTicket"),
            (31, None, "ListTicketRightsIds"),
            (32, None, "CountPrepurchaseRecordEx"),
            (33, None, "ListPrepurchaseRecordRightsIdsEx"),
            (34, None, "GetEncryptedTicketSize"),
            (35, None, "GetEncryptedTicketData"),
            (36, None, "DeleteAllInactiveELicenseRequiredPersonalizedTicket"),
            (37, None, "OwnTicket2"),
            (38, None, "OwnTicket3"),
            (39, None, "DeleteAllInactivePersonalizedTicket"),
            (40, None, "DeletePrepurchaseRecordByNintendoAccountId"),
            (501, None, "Unknown501"),
            (502, None, "Unknown502"),
            (503, None, "GetTitleKey"),
            (504, None, "Unknown504"),
            (508, None, "Unknown508"),
            (509, None, "Unknown509"),
            (510, None, "Unknown510"),
            (511, None, "Unknown511"),
            (1001, None, "Unknown1001"),
            (1002, None, "Unknown1001"),
            (1003, None, "Unknown1003"),
            (1004, None, "Unknown1004"),
            (1005, None, "Unknown1005"),
            (1006, None, "Unknown1006"),
            (1007, None, "Unknown1007"),
            (1009, None, "Unknown1009"),
            (1010, None, "Unknown1010"),
            (1011, None, "Unknown1011"),
            (1012, None, "Unknown1012"),
            (1013, None, "Unknown1013"),
            (1014, None, "Unknown1014"),
            (1015, None, "Unknown1015"),
            (1016, None, "Unknown1016"),
            (1017, None, "Unknown1017"),
            (1018, None, "Unknown1018"),
            (1019, None, "Unknown1019"),
            (1020, None, "Unknown1020"),
            (1021, None, "Unknown1021"),
            (1501, None, "Unknown1501"),
            (1502, None, "Unknown1502"),
            (1503, None, "Unknown1503"),
            (1504, None, "Unknown1504"),
            (1505, None, "Unknown1505"),
            (1506, None, "Unknown1506"),
            (2000, None, "Unknown2000"),
            (2001, None, "Unknown2001"),
            (2002, None, "Unknown2002"),
            (2003, None, "Unknown2003"),
            (2100, None, "Unknown2100"),
            (2501, None, "Unknown2501"),
            (2502, None, "Unknown2502"),
            (2601, None, "Unknown2601"),
            (3001, None, "Unknown3001"),
            (3002, None, "Unknown3002"),
        ]);

        let keys = KeyManager::instance();

        // Populate and synthesize tickets on construction, matching upstream constructor.
        {
            let mut km = keys.lock().unwrap();
            km.populate_tickets();
            km.synthesize_tickets();
        }

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
            keys,
        }
    }

    /// ImportTicket (cmd 1)
    ///
    /// Corresponds to upstream `ETicket::ImportTicket`.
    pub fn import_ticket(&self, raw_ticket: &[u8], _cert: &[u8]) -> Result<(), ResultCode> {
        let ticket = Ticket::read_from_bytes(raw_ticket);
        if !ticket.is_valid() {
            log::error!("The input buffer is not large enough!");
            return Err(ERROR_INVALID_ARGUMENT);
        }

        let mut km = self.keys.lock().unwrap();
        if !km.add_ticket(&ticket) {
            log::error!("The ticket could not be imported!");
            return Err(ERROR_INVALID_ARGUMENT);
        }

        Ok(())
    }

    /// Check if a rights ID is valid (non-zero).
    /// Corresponds to upstream `ETicket::CheckRightsId`.
    fn check_rights_id(rights_id: u128) -> bool {
        if rights_id == 0 {
            log::error!("The rights ID was invalid!");
            return false;
        }
        true
    }

    /// GetTitleKey (cmd 8)
    ///
    /// Corresponds to upstream `ETicket::GetTitleKey`.
    pub fn get_title_key(&self, rights_id: u128) -> Result<Key128, ResultCode> {
        // Upstream u128 is std::array<u64,2> where [0] is low, [1] is high.
        let rights_id_lo = rights_id as u64;
        let rights_id_hi = (rights_id >> 64) as u64;

        log::debug!(
            "ETicket::get_title_key called, rights_id={:016X}{:016X}",
            rights_id_hi,
            rights_id_lo
        );

        if !Self::check_rights_id(rights_id) {
            return Err(ERROR_INVALID_RIGHTS_ID);
        }

        let km = self.keys.lock().unwrap();
        let key = km.get_key_128(S128KeyType::Titlekey, rights_id_hi, rights_id_lo);

        if key == [0u8; 16] {
            log::error!(
                "The titlekey doesn't exist in the KeyManager or the rights ID was invalid!"
            );
            return Err(ERROR_INVALID_RIGHTS_ID);
        }

        Ok(key)
    }

    /// CountCommonTicket (cmd 9)
    ///
    /// Corresponds to upstream `ETicket::CountCommonTicket`.
    pub fn count_common_ticket(&self) -> u32 {
        log::debug!("ETicket::count_common_ticket called");
        let km = self.keys.lock().unwrap();
        km.get_common_tickets().len() as u32
    }

    /// CountPersonalizedTicket (cmd 10)
    ///
    /// Corresponds to upstream `ETicket::CountPersonalizedTicket`.
    pub fn count_personalized_ticket(&self) -> u32 {
        log::debug!("ETicket::count_personalized_ticket called");
        let km = self.keys.lock().unwrap();
        km.get_personalized_tickets().len() as u32
    }

    /// ListCommonTicketRightsIds (cmd 11)
    ///
    /// Corresponds to upstream `ETicket::ListCommonTicketRightsIds`.
    pub fn list_common_ticket_rights_ids(&self, max_entries: usize) -> (u32, Vec<u128>) {
        let mut km = self.keys.lock().unwrap();
        let mut out_entries = 0usize;
        if !km.get_common_tickets().is_empty() {
            out_entries = max_entries;
        }
        log::debug!(
            "ETicket::list_common_ticket_rights_ids called, entries={:016X}",
            out_entries
        );

        km.populate_tickets();
        let ids: Vec<u128> = km.get_common_tickets().keys().copied().collect();
        out_entries = ids.len().min(out_entries);
        let truncated = ids[..out_entries].to_vec();
        (out_entries as u32, truncated)
    }

    /// ListPersonalizedTicketRightsIds (cmd 12)
    ///
    /// Corresponds to upstream `ETicket::ListPersonalizedTicketRightsIds`.
    pub fn list_personalized_ticket_rights_ids(&self, max_entries: usize) -> (u32, Vec<u128>) {
        let mut km = self.keys.lock().unwrap();
        let mut out_entries = 0usize;
        if !km.get_personalized_tickets().is_empty() {
            out_entries = max_entries;
        }
        log::debug!(
            "ETicket::list_personalized_ticket_rights_ids called, entries={:016X}",
            out_entries
        );

        km.populate_tickets();
        let ids: Vec<u128> = km.get_personalized_tickets().keys().copied().collect();
        out_entries = ids.len().min(out_entries);
        let truncated = ids[..out_entries].to_vec();
        (out_entries as u32, truncated)
    }

    /// GetCommonTicketSize (cmd 14)
    ///
    /// Corresponds to upstream `ETicket::GetCommonTicketSize`.
    pub fn get_common_ticket_size(&self, rights_id: u128) -> Result<u64, ResultCode> {
        let rights_id_lo = rights_id as u64;
        let rights_id_hi = (rights_id >> 64) as u64;
        log::debug!(
            "ETicket::get_common_ticket_size called, rights_id={:016X}{:016X}",
            rights_id_hi,
            rights_id_lo
        );

        if !Self::check_rights_id(rights_id) {
            return Err(ERROR_INVALID_RIGHTS_ID);
        }

        let km = self.keys.lock().unwrap();
        let ticket = km
            .get_common_tickets()
            .get(&rights_id)
            .ok_or(ERROR_INVALID_RIGHTS_ID)?;
        Ok(ticket.get_size())
    }

    /// GetPersonalizedTicketSize (cmd 15)
    ///
    /// Corresponds to upstream `ETicket::GetPersonalizedTicketSize`.
    pub fn get_personalized_ticket_size(&self, rights_id: u128) -> Result<u64, ResultCode> {
        let rights_id_lo = rights_id as u64;
        let rights_id_hi = (rights_id >> 64) as u64;
        log::debug!(
            "ETicket::get_personalized_ticket_size called, rights_id={:016X}{:016X}",
            rights_id_hi,
            rights_id_lo
        );

        if !Self::check_rights_id(rights_id) {
            return Err(ERROR_INVALID_RIGHTS_ID);
        }

        let km = self.keys.lock().unwrap();
        let ticket = km
            .get_personalized_tickets()
            .get(&rights_id)
            .ok_or(ERROR_INVALID_RIGHTS_ID)?;
        Ok(ticket.get_size())
    }

    /// GetCommonTicketData (cmd 16)
    ///
    /// Corresponds to upstream `ETicket::GetCommonTicketData`.
    pub fn get_common_ticket_data(
        &self,
        rights_id: u128,
        write_buffer_size: u64,
    ) -> Result<(u64, Vec<u8>), ResultCode> {
        let rights_id_lo = rights_id as u64;
        let rights_id_hi = (rights_id >> 64) as u64;
        log::debug!(
            "ETicket::get_common_ticket_data called, rights_id={:016X}{:016X}",
            rights_id_hi,
            rights_id_lo
        );

        if !Self::check_rights_id(rights_id) {
            return Err(ERROR_INVALID_RIGHTS_ID);
        }

        let km = self.keys.lock().unwrap();
        let ticket = km
            .get_common_tickets()
            .get(&rights_id)
            .ok_or(ERROR_INVALID_RIGHTS_ID)?;

        let ticket_bytes = ticket_to_bytes(ticket);
        let write_size = (ticket.get_size()).min(write_buffer_size);
        let data = ticket_bytes[..write_size as usize].to_vec();
        Ok((write_size, data))
    }

    /// GetPersonalizedTicketData (cmd 17)
    ///
    /// Corresponds to upstream `ETicket::GetPersonalizedTicketData`.
    pub fn get_personalized_ticket_data(
        &self,
        rights_id: u128,
        write_buffer_size: u64,
    ) -> Result<(u64, Vec<u8>), ResultCode> {
        let rights_id_lo = rights_id as u64;
        let rights_id_hi = (rights_id >> 64) as u64;
        log::debug!(
            "ETicket::get_personalized_ticket_data called, rights_id={:016X}{:016X}",
            rights_id_hi,
            rights_id_lo
        );

        if !Self::check_rights_id(rights_id) {
            return Err(ERROR_INVALID_RIGHTS_ID);
        }

        let km = self.keys.lock().unwrap();
        let ticket = km
            .get_personalized_tickets()
            .get(&rights_id)
            .ok_or(ERROR_INVALID_RIGHTS_ID)?;

        let ticket_bytes = ticket_to_bytes(ticket);
        let write_size = (ticket.get_size()).min(write_buffer_size);
        let data = ticket_bytes[..write_size as usize].to_vec();
        Ok((write_size, data))
    }

    fn import_ticket_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const ETicket) };
        let raw_ticket = ctx.read_buffer(0);
        let cert = ctx.read_buffer(1);
        match svc.import_ticket(&raw_ticket, &cert) {
            Ok(()) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(RESULT_SUCCESS);
            }
            Err(rc) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(rc);
            }
        }
    }

    fn get_title_key_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const ETicket) };
        let mut rp = RequestParser::new(ctx);
        let rights_id = rp.pop_raw::<u128>();
        match svc.get_title_key(rights_id) {
            Ok(key) => {
                ctx.write_buffer(&key, 0);
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(RESULT_SUCCESS);
            }
            Err(rc) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(rc);
            }
        }
    }

    fn count_common_ticket_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const ETicket) };
        let count = svc.count_common_ticket();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(count);
    }

    fn count_personalized_ticket_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const ETicket) };
        let count = svc.count_personalized_ticket();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(count);
    }

    fn list_common_ticket_rights_ids_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const ETicket) };
        let max_entries = ctx.get_write_buffer_size(0) / std::mem::size_of::<u128>();
        let (count, ids) = svc.list_common_ticket_rights_ids(max_entries);
        if !ids.is_empty() {
            let id_bytes = unsafe {
                std::slice::from_raw_parts(
                    ids.as_ptr() as *const u8,
                    ids.len() * std::mem::size_of::<u128>(),
                )
            };
            ctx.write_buffer(id_bytes, 0);
        }
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(count);
    }

    fn list_personalized_ticket_rights_ids_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const ETicket) };
        let max_entries = ctx.get_write_buffer_size(0) / std::mem::size_of::<u128>();
        let (count, ids) = svc.list_personalized_ticket_rights_ids(max_entries);
        if !ids.is_empty() {
            let id_bytes = unsafe {
                std::slice::from_raw_parts(
                    ids.as_ptr() as *const u8,
                    ids.len() * std::mem::size_of::<u128>(),
                )
            };
            ctx.write_buffer(id_bytes, 0);
        }
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(count);
    }

    fn get_common_ticket_size_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const ETicket) };
        let mut rp = RequestParser::new(ctx);
        let rights_id = rp.pop_raw::<u128>();
        match svc.get_common_ticket_size(rights_id) {
            Ok(size) => {
                let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_u64(size);
            }
            Err(rc) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(rc);
            }
        }
    }

    fn get_personalized_ticket_size_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const ETicket) };
        let mut rp = RequestParser::new(ctx);
        let rights_id = rp.pop_raw::<u128>();
        match svc.get_personalized_ticket_size(rights_id) {
            Ok(size) => {
                let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_u64(size);
            }
            Err(rc) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(rc);
            }
        }
    }

    fn get_common_ticket_data_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const ETicket) };
        let mut rp = RequestParser::new(ctx);
        let rights_id = rp.pop_raw::<u128>();
        let write_buffer_size = ctx.get_write_buffer_size(0) as u64;
        match svc.get_common_ticket_data(rights_id, write_buffer_size) {
            Ok((size, data)) => {
                ctx.write_buffer(&data, 0);
                let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_u64(size);
            }
            Err(rc) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(rc);
            }
        }
    }

    fn get_personalized_ticket_data_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const ETicket) };
        let mut rp = RequestParser::new(ctx);
        let rights_id = rp.pop_raw::<u128>();
        let write_buffer_size = ctx.get_write_buffer_size(0) as u64;
        match svc.get_personalized_ticket_data(rights_id, write_buffer_size) {
            Ok((size, data)) => {
                ctx.write_buffer(&data, 0);
                let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_u64(size);
            }
            Err(rc) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(rc);
            }
        }
    }
}

impl SessionRequestHandler for ETicket {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str { "es" }
}

impl ServiceFramework for ETicket {
    fn get_service_name(&self) -> &str { "es" }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}

/// Registers "es" service.
///
/// Corresponds to `LoopProcess` in upstream `es.cpp`.
pub fn loop_process(system: crate::core::SystemRef) {
    use std::sync::Arc;
    use crate::hle::service::server_manager::ServerManager;
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;

    let mut server_manager = ServerManager::new(system);

    server_manager.register_named_service(
        "es",
        Box::new(|| -> SessionRequestHandlerPtr {
            Arc::new(ETicket::new())
        }),
        64,
    );

    ServerManager::run_server(server_manager);
}
