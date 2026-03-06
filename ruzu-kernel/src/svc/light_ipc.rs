// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use common::ResultCode;
use log::warn;

/// SVC 0x20: SendSyncRequestLight
///
/// Stub — lightweight IPC not needed for MK8D.
pub fn svc_send_sync_request_light() -> ResultCode {
    warn!("SendSyncRequestLight (stub)");
    ResultCode::SUCCESS
}

/// SVC 0x42: ReplyAndReceiveLight
///
/// Stub — lightweight IPC not needed for MK8D.
pub fn svc_reply_and_receive_light() -> ResultCode {
    warn!("ReplyAndReceiveLight (stub)");
    ResultCode::SUCCESS
}
