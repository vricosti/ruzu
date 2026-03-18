// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port placeholder for zuyu/src/core/hle/service/ssl/ssl_backend_schannel.cpp
//!
//! Upstream owns the Schannel-backed `CreateSSLConnectionBackend`
//! implementation in this file. The Rust port keeps ownership here, but the
//! platform implementation is still missing.

use crate::hle::result::ResultCode;

use super::ssl_backend::{SslConnectionBackend, RESULT_INTERNAL_ERROR};

pub fn create_ssl_connection_backend() -> Result<Box<dyn SslConnectionBackend>, ResultCode> {
    log::error!("Schannel SSL backend is not wired in the Rust port");
    Err(RESULT_INTERNAL_ERROR)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schannel_backend_placeholder_returns_internal_error() {
        match create_ssl_connection_backend() {
            Ok(_) => panic!("expected placeholder Schannel backend to fail"),
            Err(err) => assert_eq!(err, RESULT_INTERNAL_ERROR),
        }
    }
}
