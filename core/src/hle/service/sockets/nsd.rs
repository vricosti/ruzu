// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/sockets/nsd.h
//! Port of zuyu/src/core/hle/service/sockets/nsd.cpp
//!
//! NSD service — Nintendo Socket Daemon ("nsd:u", "nsd:a").

use crate::hle::result::{ErrorModule, ResultCode};

/// Result code for overflow.
///
/// Corresponds to `ResultOverflow` in upstream nsd.cpp.
pub const RESULT_OVERFLOW: ResultCode = ResultCode::from_module_description(ErrorModule::NSD, 6);

/// nn::oe::ServerEnvironmentType
///
/// Corresponds to `ServerEnvironmentType` in upstream nsd.cpp.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ServerEnvironmentType {
    Dd = 0,
    Lp = 1,
    Sd = 2,
    Sp = 3,
    Dp = 4,
}

/// nn::nsd::EnvironmentIdentifier
///
/// Corresponds to `EnvironmentIdentifier` in upstream nsd.cpp.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct EnvironmentIdentifier {
    pub identifier: [u8; 8],
}
const _: () = assert!(std::mem::size_of::<EnvironmentIdentifier>() == 0x8);

/// IPC command table for NSD.
///
/// Corresponds to the function table in upstream nsd.cpp.
pub mod commands {
    pub const GET_SETTING_URL: u32 = 5;
    pub const GET_SETTING_NAME: u32 = 10;
    pub const GET_ENVIRONMENT_IDENTIFIER: u32 = 11;
    pub const GET_DEVICE_ID: u32 = 12;
    pub const DELETE_SETTINGS: u32 = 13;
    pub const IMPORT_SETTINGS: u32 = 14;
    pub const SET_CHANGE_ENV_ID_DISABLED: u32 = 15;
    pub const RESOLVE: u32 = 20;
    pub const RESOLVE_EX: u32 = 21;
    pub const GET_NAS_SERVICE_SETTING: u32 = 30;
    pub const GET_NAS_SERVICE_SETTING_EX: u32 = 31;
    pub const GET_NAS_REQUEST_FQDN: u32 = 40;
    pub const GET_NAS_REQUEST_FQDN_EX: u32 = 41;
    pub const GET_NAS_API_FQDN: u32 = 42;
    pub const GET_NAS_API_FQDN_EX: u32 = 43;
    pub const GET_CURRENT_SETTING: u32 = 50;
    pub const WRITE_TEST_PARAMETER: u32 = 51;
    pub const READ_TEST_PARAMETER: u32 = 52;
    pub const READ_SAVE_DATA_FROM_FS_FOR_TEST: u32 = 60;
    pub const WRITE_SAVE_DATA_TO_FS_FOR_TEST: u32 = 61;
    pub const DELETE_SAVE_DATA_OF_FS_FOR_TEST: u32 = 62;
    pub const IS_CHANGE_ENV_ID_DISABLED: u32 = 63;
    pub const SET_WITHOUT_DOMAIN_EXCHANGE_FQDNS: u32 = 64;
    pub const GET_APPLICATION_SERVER_ENVIRONMENT_TYPE: u32 = 100;
    pub const SET_APPLICATION_SERVER_ENVIRONMENT_TYPE: u32 = 101;
    pub const DELETE_APPLICATION_SERVER_ENVIRONMENT_TYPE: u32 = 102;
}

/// NSD service.
///
/// Corresponds to `NSD` in upstream nsd.h / nsd.cpp.
pub struct Nsd;

impl Nsd {
    pub fn new() -> Self {
        Self
    }

    /// Resolve — the real implementation makes various substitutions.
    /// For now we return the string as-is, which is good enough when
    /// not connecting to real Nintendo servers.
    ///
    /// Corresponds to `ResolveImpl` in upstream nsd.cpp.
    fn resolve_impl(fqdn_in: &str) -> String {
        log::warn!("NSD::resolve (STUBBED) called, fqdn_in={}", fqdn_in);
        fqdn_in.to_string()
    }

    /// Common resolve logic shared between Resolve and ResolveEx.
    ///
    /// Corresponds to `ResolveCommon` in upstream nsd.cpp.
    fn resolve_common(fqdn_in: &str) -> Result<[u8; 0x100], ResultCode> {
        let res = Self::resolve_impl(fqdn_in);
        let mut fqdn_out = [0u8; 0x100];
        if res.len() >= fqdn_out.len() {
            return Err(RESULT_OVERFLOW);
        }
        fqdn_out[..res.len()].copy_from_slice(res.as_bytes());
        // NUL terminator is already zero from array init.
        Ok(fqdn_out)
    }

    /// Resolve (cmd 20).
    ///
    /// Corresponds to `NSD::Resolve` in upstream nsd.cpp.
    pub fn resolve(&self, fqdn_in: &str) -> Result<[u8; 0x100], ResultCode> {
        Self::resolve_common(fqdn_in)
    }

    /// ResolveEx (cmd 21).
    ///
    /// Corresponds to `NSD::ResolveEx` in upstream nsd.cpp.
    pub fn resolve_ex(&self, fqdn_in: &str) -> Result<[u8; 0x100], ResultCode> {
        Self::resolve_common(fqdn_in)
    }

    /// GetEnvironmentIdentifier (cmd 11).
    ///
    /// Corresponds to `NSD::GetEnvironmentIdentifier` in upstream nsd.cpp.
    pub fn get_environment_identifier(&self) -> EnvironmentIdentifier {
        EnvironmentIdentifier {
            identifier: [b'l', b'p', b'1', 0, 0, 0, 0, 0],
        }
    }

    /// GetApplicationServerEnvironmentType (cmd 100).
    ///
    /// Corresponds to `NSD::GetApplicationServerEnvironmentType` in upstream nsd.cpp.
    pub fn get_application_server_environment_type(&self) -> ServerEnvironmentType {
        ServerEnvironmentType::Lp
    }
}
