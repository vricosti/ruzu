// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/all_system_applet_proxies_service.h
//! Port of zuyu/src/core/hle/service/am/service/all_system_applet_proxies_service.cpp

/// IPC command table for IAllSystemAppletProxiesService ("appletAE"):
/// - 100: OpenSystemAppletProxy
/// - 200: OpenLibraryAppletProxyOld
/// - 201: OpenLibraryAppletProxy
/// - 300: OpenOverlayAppletProxy (unimplemented)
/// - 350: OpenSystemApplicationProxy (unimplemented)
/// - 400: CreateSelfLibraryAppletCreatorForDevelop (unimplemented)
/// - 410: GetSystemAppletControllerForDebug (unimplemented)
/// - 1000: GetDebugFunctions (unimplemented)
pub struct IAllSystemAppletProxiesService {
    // TODO: WindowSystem reference
}

impl IAllSystemAppletProxiesService {
    pub fn new() -> Self {
        Self {}
    }
}
