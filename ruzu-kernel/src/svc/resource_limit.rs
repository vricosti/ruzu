// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use common::{error, Handle, ResultCode};
use log::warn;

/// SVC 0x30: GetResourceLimitLimitValue
///
/// Stub — returns large values for all resource types.
pub fn svc_get_resource_limit_limit_value(
    _handle: Handle,
    resource_type: u32,
) -> Result<u64, ResultCode> {
    warn!(
        "GetResourceLimitLimitValue: resource_type={} (stub)",
        resource_type
    );
    // Return generous limits.
    Ok(0x1_0000_0000)
}

/// SVC 0x31: GetResourceLimitCurrentValue
///
/// Stub — returns 0 (no resources currently used).
pub fn svc_get_resource_limit_current_value(
    _handle: Handle,
    resource_type: u32,
) -> Result<u64, ResultCode> {
    warn!(
        "GetResourceLimitCurrentValue: resource_type={} (stub)",
        resource_type
    );
    Ok(0)
}

/// SVC 0x7D: CreateResourceLimit
///
/// Stub — not needed for standard game execution.
pub fn svc_create_resource_limit() -> Result<Handle, ResultCode> {
    warn!("CreateResourceLimit (stub)");
    Err(error::INVALID_STATE)
}

/// SVC 0x7E: SetResourceLimitLimitValue
///
/// Stub — accepts and ignores.
pub fn svc_set_resource_limit_limit_value(
    _handle: Handle,
    _resource_type: u32,
    _value: u64,
) -> ResultCode {
    warn!("SetResourceLimitLimitValue (stub)");
    ResultCode::SUCCESS
}
