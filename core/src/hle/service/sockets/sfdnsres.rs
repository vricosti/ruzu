// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/sockets/sfdnsres.h
//! Port of zuyu/src/core/hle/service/sockets/sfdnsres.cpp
//!
//! SFDNSRES service — DNS resolution ("sfdnsres").

/// IPC command table for SFDNSRES.
pub mod commands {
    pub const SET_DNS_ADDRESSES_PRIVATE_REQUEST: u32 = 0;
    pub const GET_DNS_ADDRESS_PRIVATE_REQUEST: u32 = 1;
    pub const GET_HOST_BY_NAME_REQUEST: u32 = 2;
    pub const GET_HOST_BY_ADDR_REQUEST: u32 = 3;
    pub const GET_HOST_STRING_ERROR_REQUEST: u32 = 4;
    pub const GET_GAI_STRING_ERROR_REQUEST: u32 = 5;
    pub const GET_ADDR_INFO_REQUEST: u32 = 6;
    pub const GET_NAME_INFO_REQUEST: u32 = 7;
    pub const REQUEST_CANCEL_HANDLE_REQUEST: u32 = 8;
    pub const CANCEL_REQUEST: u32 = 9;
    pub const GET_HOST_BY_NAME_REQUEST_WITH_OPTIONS: u32 = 10;
    pub const GET_HOST_BY_ADDR_REQUEST_WITH_OPTIONS: u32 = 11;
    pub const GET_ADDR_INFO_REQUEST_WITH_OPTIONS: u32 = 12;
    pub const GET_NAME_INFO_REQUEST_WITH_OPTIONS: u32 = 13;
    pub const RESOLVER_SET_OPTION_REQUEST: u32 = 14;
    pub const RESOLVER_GET_OPTION_REQUEST: u32 = 15;
}

/// SFDNSRES service.
///
/// Corresponds to `SFDNSRES` in upstream sfdnsres.h / sfdnsres.cpp.
pub struct Sfdnsres;

impl Sfdnsres {
    pub fn new() -> Self {
        Self
    }
}
