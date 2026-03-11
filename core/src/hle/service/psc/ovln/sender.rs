// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/ovln/sender.cpp/.h

use super::ovln_types::{MessageFlags, OverlayNotification};

pub const ISENDER_COMMANDS: &[(u32, bool, &str)] = &[
    (0, true, "Send"),
    (1, false, "GetUnreceivedMessageCount"),
];

pub fn send(notification: &OverlayNotification, flags: MessageFlags) {
    let data: String = notification.iter().map(|m| format!("{:016X} ", m)).collect();
    log::warn!(
        "(STUBBED) ISender::Send called, flags={} notification={}",
        flags.raw,
        data
    );
}
