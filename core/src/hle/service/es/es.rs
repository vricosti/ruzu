// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/es/es.cpp
//!
//! ETicket service ("es").

use std::collections::BTreeMap;
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

/// ETicket service ("es").
pub struct ETicket {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    // TODO: KeyManager reference
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

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Stubbed: ImportTicket (cmd 1)
    pub fn import_ticket(&self, _raw_ticket: &[u8], _cert: &[u8]) -> Result<(), ResultCode> {
        log::warn!("(STUBBED) ETicket::import_ticket called");
        // TODO: implement with KeyManager
        Ok(())
    }

    /// Stubbed: GetTitleKey (cmd 8)
    pub fn get_title_key(&self, _rights_id: u128) -> Result<[u8; 16], ResultCode> {
        log::warn!("(STUBBED) ETicket::get_title_key called");
        // TODO: implement with KeyManager
        Err(ERROR_INVALID_RIGHTS_ID)
    }

    /// Stubbed: CountCommonTicket (cmd 9)
    pub fn count_common_ticket(&self) -> u32 {
        log::debug!("ETicket::count_common_ticket called");
        0
    }

    /// Stubbed: CountPersonalizedTicket (cmd 10)
    pub fn count_personalized_ticket(&self) -> u32 {
        log::debug!("ETicket::count_personalized_ticket called");
        0
    }

    /// Stubbed: ListCommonTicketRightsIds (cmd 11)
    pub fn list_common_ticket_rights_ids(&self) -> (u32, Vec<u128>) {
        log::debug!("ETicket::list_common_ticket_rights_ids called");
        (0, Vec::new())
    }

    /// Stubbed: ListPersonalizedTicketRightsIds (cmd 12)
    pub fn list_personalized_ticket_rights_ids(&self) -> (u32, Vec<u128>) {
        log::debug!("ETicket::list_personalized_ticket_rights_ids called");
        (0, Vec::new())
    }

    /// Stubbed: GetCommonTicketSize (cmd 14)
    pub fn get_common_ticket_size(&self, _rights_id: u128) -> Result<u64, ResultCode> {
        log::debug!("ETicket::get_common_ticket_size called");
        Err(ERROR_INVALID_RIGHTS_ID)
    }

    /// Stubbed: GetPersonalizedTicketSize (cmd 15)
    pub fn get_personalized_ticket_size(&self, _rights_id: u128) -> Result<u64, ResultCode> {
        log::debug!("ETicket::get_personalized_ticket_size called");
        Err(ERROR_INVALID_RIGHTS_ID)
    }

    /// Stubbed: GetCommonTicketData (cmd 16)
    pub fn get_common_ticket_data(&self, _rights_id: u128) -> Result<(u64, Vec<u8>), ResultCode> {
        log::debug!("ETicket::get_common_ticket_data called");
        Err(ERROR_INVALID_RIGHTS_ID)
    }

    /// Stubbed: GetPersonalizedTicketData (cmd 17)
    pub fn get_personalized_ticket_data(
        &self,
        _rights_id: u128,
    ) -> Result<(u64, Vec<u8>), ResultCode> {
        log::debug!("ETicket::get_personalized_ticket_data called");
        Err(ERROR_INVALID_RIGHTS_ID)
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
        let (count, ids) = svc.list_common_ticket_rights_ids();
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
        let (count, ids) = svc.list_personalized_ticket_rights_ids();
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
        match svc.get_common_ticket_data(rights_id) {
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
        match svc.get_personalized_ticket_data(rights_id) {
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
pub fn loop_process() {
    // TODO: register "es" -> ETicket with ServerManager
}
