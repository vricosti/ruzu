// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use common::ResultCode;
use log::warn;

/// SVC 0x28: ReturnFromException
///
/// Stub — returns success. Full implementation would resume from an exception context.
pub fn svc_return_from_exception(result: u64) -> ResultCode {
    warn!("ReturnFromException: result=0x{:X} (stub)", result);
    ResultCode::SUCCESS
}
