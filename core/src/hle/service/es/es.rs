// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/es/es.cpp
//!
//! ETicket service ("es").

use crate::hle::result::{ErrorModule, ResultCode};

/// Upstream: `ERROR_INVALID_ARGUMENT{ErrorModule::ETicket, 2}`
pub const ERROR_INVALID_ARGUMENT: ResultCode =
    ResultCode::from_module_description(ErrorModule::ETicket, 2);

/// Upstream: `ERROR_INVALID_RIGHTS_ID{ErrorModule::ETicket, 3}`
pub const ERROR_INVALID_RIGHTS_ID: ResultCode =
    ResultCode::from_module_description(ErrorModule::ETicket, 3);

/// IPC command IDs for ETicket
pub mod commands {
    pub const IMPORT_TICKET: u32 = 1;
    pub const IMPORT_TICKET_CERTIFICATE_SET: u32 = 2;
    pub const DELETE_TICKET: u32 = 3;
    pub const DELETE_PERSONALIZED_TICKET: u32 = 4;
    pub const DELETE_ALL_COMMON_TICKET: u32 = 5;
    pub const DELETE_ALL_PERSONALIZED_TICKET: u32 = 6;
    pub const DELETE_ALL_PERSONALIZED_TICKET_EX: u32 = 7;
    pub const GET_TITLE_KEY: u32 = 8;
    pub const COUNT_COMMON_TICKET: u32 = 9;
    pub const COUNT_PERSONALIZED_TICKET: u32 = 10;
    pub const LIST_COMMON_TICKET_RIGHTS_IDS: u32 = 11;
    pub const LIST_PERSONALIZED_TICKET_RIGHTS_IDS: u32 = 12;
    pub const LIST_MISSING_PERSONALIZED_TICKET: u32 = 13;
    pub const GET_COMMON_TICKET_SIZE: u32 = 14;
    pub const GET_PERSONALIZED_TICKET_SIZE: u32 = 15;
    pub const GET_COMMON_TICKET_DATA: u32 = 16;
    pub const GET_PERSONALIZED_TICKET_DATA: u32 = 17;
}

/// ETicket service ("es").
pub struct ETicket {
    // TODO: KeyManager reference
}

impl ETicket {
    pub fn new() -> Self {
        Self {}
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
}

/// Registers "es" service.
///
/// Corresponds to `LoopProcess` in upstream `es.cpp`.
pub fn loop_process() {
    // TODO: register "es" -> ETicket with ServerManager
}
