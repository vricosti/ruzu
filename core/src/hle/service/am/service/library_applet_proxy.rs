// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/library_applet_proxy.h
//! Port of zuyu/src/core/hle/service/am/service/library_applet_proxy.cpp

/// IPC command table for ILibraryAppletProxy:
/// - 0: GetCommonStateGetter
/// - 1: GetSelfController
/// - 2: GetWindowController
/// - 3: GetAudioController
/// - 4: GetDisplayController
/// - 10: GetProcessWindingController
/// - 11: GetLibraryAppletCreator
/// - 20: OpenLibraryAppletSelfAccessor
/// - 21: GetAppletCommonFunctions
/// - 22: GetHomeMenuFunctions
/// - 23: GetGlobalStateController
/// - 1000: GetDebugFunctions
pub struct ILibraryAppletProxy {
    // TODO: WindowSystem reference, KProcess, Applet reference
}

impl ILibraryAppletProxy {
    pub fn new() -> Self {
        Self {}
    }
}
