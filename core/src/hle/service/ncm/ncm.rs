// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ncm/ncm.cpp
//!
//! NCM and LR services. All commands are unimplemented stubs.

/// IPC command IDs for ILocationResolver
pub mod location_resolver_commands {
    pub const RESOLVE_PROGRAM_PATH: u32 = 0;
    pub const REDIRECT_PROGRAM_PATH: u32 = 1;
    pub const RESOLVE_APPLICATION_CONTROL_PATH: u32 = 2;
    pub const RESOLVE_APPLICATION_HTML_DOCUMENT_PATH: u32 = 3;
    pub const RESOLVE_DATA_PATH: u32 = 4;
    pub const REDIRECT_APPLICATION_CONTROL_PATH: u32 = 5;
    pub const REDIRECT_APPLICATION_HTML_DOCUMENT_PATH: u32 = 6;
    pub const RESOLVE_APPLICATION_LEGAL_INFORMATION_PATH: u32 = 7;
    pub const REDIRECT_APPLICATION_LEGAL_INFORMATION_PATH: u32 = 8;
    pub const REFRESH: u32 = 9;
}

/// IPC command IDs for LR
pub mod lr_commands {
    pub const OPEN_LOCATION_RESOLVER: u32 = 0;
    pub const OPEN_REGISTERED_LOCATION_RESOLVER: u32 = 1;
    pub const REFRESH_LOCATION_RESOLVER: u32 = 2;
    pub const OPEN_ADD_ON_CONTENT_LOCATION_RESOLVER: u32 = 3;
}

/// IPC command IDs for NCM
pub mod ncm_commands {
    pub const CREATE_CONTENT_STORAGE: u32 = 0;
    pub const CREATE_CONTENT_META_DATABASE: u32 = 1;
    pub const VERIFY_CONTENT_STORAGE: u32 = 2;
    pub const VERIFY_CONTENT_META_DATABASE: u32 = 3;
    pub const OPEN_CONTENT_STORAGE: u32 = 4;
    pub const OPEN_CONTENT_META_DATABASE: u32 = 5;
    pub const CLOSE_CONTENT_STORAGE_FORCIBLY: u32 = 6;
    pub const CLOSE_CONTENT_META_DATABASE_FORCIBLY: u32 = 7;
    pub const CLEAN_UP_CONTENT_META_DATABASE: u32 = 8;
    pub const ACTIVATE_CONTENT_STORAGE: u32 = 9;
    pub const INACTIVATE_CONTENT_STORAGE: u32 = 10;
    pub const ACTIVATE_CONTENT_META_DATABASE: u32 = 11;
    pub const INACTIVATE_CONTENT_META_DATABASE: u32 = 12;
    pub const INVALIDATE_RIGHTS_ID_CACHE: u32 = 13;
    pub const GET_MEMORY_REPORT: u32 = 14;
    pub const ACTIVATE_FS_CONTENT_STORAGE: u32 = 15;
}

pub struct ILocationResolver;
impl ILocationResolver {
    pub fn new() -> Self { Self }
}

pub struct IRegisteredLocationResolver;
impl IRegisteredLocationResolver {
    pub fn new() -> Self { Self }
}

pub struct IAddOnContentLocationResolver;
impl IAddOnContentLocationResolver {
    pub fn new() -> Self { Self }
}

pub struct LR;
impl LR {
    pub fn new() -> Self { Self }
}

pub struct NCM;
impl NCM {
    pub fn new() -> Self { Self }
}

/// Registers "lr" and "ncm" services.
///
/// Corresponds to `LoopProcess` in upstream `ncm.cpp`.
pub fn loop_process() {
    // TODO: register services with ServerManager
}
