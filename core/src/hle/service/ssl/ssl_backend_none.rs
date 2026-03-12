// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ssl/ssl_backend_none.cpp
//!
//! Fallback SSL backend when no real SSL library is available.
//! Returns ResultInternalError for all connection attempts.

use crate::hle::result::ResultCode;
use super::ssl_backend::{SslConnectionBackend, RESULT_INTERNAL_ERROR};

/// Create an SSL connection backend.
///
/// On this platform, no SSL backend is available, so this always returns
/// `ResultInternalError`.
///
/// Corresponds to `CreateSSLConnectionBackend` in upstream ssl_backend_none.cpp.
pub fn create_ssl_connection_backend() -> Result<Box<dyn SslConnectionBackend>, ResultCode> {
    log::error!(
        "Can't create SSL connection because no SSL backend is available on this platform"
    );
    Err(RESULT_INTERNAL_ERROR)
}
