// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/eupld/eupld.cpp
//!
//! ErrorUploadContext ("eupld:c") and ErrorUploadRequest ("eupld:r") services.
//! All commands are unimplemented stubs in upstream.

/// IPC command IDs for ErrorUploadContext
pub mod context_commands {
    pub const SET_URL: u32 = 0;
    pub const IMPORT_CRT: u32 = 1;
    pub const IMPORT_PKI: u32 = 2;
    pub const SET_AUTO_UPLOAD: u32 = 3;
    pub const GET_AUTO_UPLOAD: u32 = 4;
}

/// IPC command IDs for ErrorUploadRequest
pub mod request_commands {
    pub const INITIALIZE: u32 = 0;
    pub const UPLOAD_ALL: u32 = 1;
    pub const UPLOAD_SELECTED: u32 = 2;
    pub const GET_UPLOAD_STATUS: u32 = 3;
    pub const CANCEL_UPLOAD: u32 = 4;
    pub const GET_RESULT: u32 = 5;
}

/// ErrorUploadContext service ("eupld:c"). All stubs.
pub struct ErrorUploadContext;

impl ErrorUploadContext {
    pub fn new() -> Self {
        Self
    }
}

/// ErrorUploadRequest service ("eupld:r"). All stubs.
pub struct ErrorUploadRequest;

impl ErrorUploadRequest {
    pub fn new() -> Self {
        Self
    }
}

/// Registers "eupld:c" and "eupld:r" services.
///
/// Corresponds to `LoopProcess` in upstream `eupld.cpp`.
pub fn loop_process() {
    // TODO: register services with ServerManager
}
