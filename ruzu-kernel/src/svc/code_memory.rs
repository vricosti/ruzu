// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use log::warn;
use ruzu_common::{error, Handle, ResultCode, VAddr};

/// SVC 0x4B: CreateCodeMemory
///
/// Stub — JIT code memory not needed for standard game execution.
pub fn svc_create_code_memory(_addr: VAddr, _size: u64) -> Result<Handle, ResultCode> {
    warn!("CreateCodeMemory (stub)");
    Err(error::INVALID_STATE)
}

/// SVC 0x4C: ControlCodeMemory
///
/// Stub — JIT code memory not needed for standard game execution.
pub fn svc_control_code_memory(
    _handle: Handle,
    _operation: u32,
    _addr: VAddr,
    _size: u64,
    _perm: u32,
) -> ResultCode {
    warn!("ControlCodeMemory (stub)");
    ResultCode::SUCCESS
}
