// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/system_update_interface.cpp/.h

use super::ns_types::BackgroundNetworkUpdateState;

pub const ISYSTEM_UPDATE_INTERFACE_COMMANDS: &[(u32, bool, &str)] = &[
    (0, true, "GetBackgroundNetworkUpdateState"),
    (1, true, "OpenSystemUpdateControl"),
    (2, false, "NotifyExFatDriverRequired"),
    (3, false, "ClearExFatDriverStatusForDebug"),
    (4, false, "RequestBackgroundNetworkUpdate"),
    (5, false, "NotifyBackgroundNetworkUpdate"),
    (6, false, "NotifyExFatDriverDownloadedForDebug"),
    (9, true, "GetSystemUpdateNotificationEventForContentDelivery"),
    (10, false, "NotifySystemUpdateForContentDelivery"),
    (11, false, "PrepareShutdown"),
    (12, false, "Unknown12"),
    (13, false, "Unknown13"),
    (14, false, "Unknown14"),
    (15, false, "Unknown15"),
    (16, false, "DestroySystemUpdateTask"),
    (17, false, "RequestSendSystemUpdate"),
    (18, false, "GetSendSystemUpdateProgress"),
];

pub fn get_background_network_update_state() -> BackgroundNetworkUpdateState {
    log::warn!("(STUBBED) ISystemUpdateInterface::GetBackgroundNetworkUpdateState called");
    BackgroundNetworkUpdateState::None
}
