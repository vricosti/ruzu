// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use common::{error, Handle, ResultCode};
use log::warn;

/// SVC 0x40: CreateSession
///
/// Stub — not needed for normal game execution (used by services internally).
pub fn svc_create_session(_is_light: bool) -> Result<(Handle, Handle), ResultCode> {
    warn!("CreateSession (stub)");
    Err(error::INVALID_STATE)
}

/// SVC 0x41: AcceptSession
///
/// Stub — not needed for normal game execution.
pub fn svc_accept_session(_port_handle: Handle) -> Result<Handle, ResultCode> {
    warn!("AcceptSession (stub)");
    Err(error::INVALID_STATE)
}

/// SVC 0x43: ReplyAndReceive
///
/// Stub — not needed for normal game execution.
pub fn svc_reply_and_receive() -> ResultCode {
    warn!("ReplyAndReceive (stub)");
    ResultCode::SUCCESS
}
