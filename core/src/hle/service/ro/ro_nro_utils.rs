// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ro/ro_nro_utils.h
//! Port of zuyu/src/core/hle/service/ro/ro_nro_utils.cpp
//!
//! NRO utility functions for validating NRO/NRR data.

use crate::hle::result::ResultCode;

/// Validate an NRO header.
///
/// Corresponds to `ValidateNRO` in upstream ro_nro_utils.cpp.
pub fn validate_nro(_nro_data: &[u8]) -> ResultCode {
    log::debug!("validate_nro (STUBBED) called");
    // TODO: implement NRO validation
    ResultCode::new(0) // Success
}
