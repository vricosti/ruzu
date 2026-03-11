// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/application_proxy.h
//! Port of zuyu/src/core/hle/service/am/service/application_proxy.cpp

/// IPC command table for IApplicationProxy:
/// - 0: GetCommonStateGetter
/// - 1: GetSelfController
/// - 2: GetWindowController
/// - 3: GetAudioController
/// - 4: GetDisplayController
/// - 10: GetProcessWindingController
/// - 11: GetLibraryAppletCreator
/// - 20: GetApplicationFunctions
/// - 1000: GetDebugFunctions
pub struct IApplicationProxy {
    // TODO: WindowSystem reference, KProcess reference, Applet reference
}

impl IApplicationProxy {
    pub fn new() -> Self {
        Self {}
    }
}
