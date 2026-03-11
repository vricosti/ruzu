// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/system_applet_proxy.h
//! Port of zuyu/src/core/hle/service/am/service/system_applet_proxy.cpp

/// ISystemAppletProxy service.
pub struct ISystemAppletProxy {
    // TODO: WindowSystem reference, KProcess, Applet reference
}

impl ISystemAppletProxy {
    pub fn new() -> Self {
        Self {}
    }
}
