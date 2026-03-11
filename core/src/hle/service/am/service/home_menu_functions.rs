// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/home_menu_functions.h
//! Port of zuyu/src/core/hle/service/am/service/home_menu_functions.cpp

/// IPC command table for IHomeMenuFunctions:
/// - 10: RequestToGetForeground
/// - 11: LockForeground
/// - 12: UnlockForeground
/// - 21: GetPopFromGeneralChannelEvent
/// - 41: IsRebootEnabled
/// - 110: IsForceTerminateApplicationDisabledForDebug
pub struct IHomeMenuFunctions {
    // TODO: WindowSystem reference, Applet reference, ServiceContext, Event
}

impl IHomeMenuFunctions {
    pub fn new() -> Self {
        Self {}
    }
}
