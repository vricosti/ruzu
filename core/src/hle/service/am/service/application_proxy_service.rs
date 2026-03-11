// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/application_proxy_service.h
//! Port of zuyu/src/core/hle/service/am/service/application_proxy_service.cpp

/// IPC command table for IApplicationProxyService ("appletOE"):
/// - 0: OpenApplicationProxy
pub struct IApplicationProxyService {
    // TODO: WindowSystem reference
}

impl IApplicationProxyService {
    pub fn new() -> Self {
        Self {}
    }

    /// Port of IApplicationProxyService::OpenApplicationProxy
    pub fn open_application_proxy(&self) {
        log::debug!("IApplicationProxyService::OpenApplicationProxy called");
        // TODO: GetAppletFromProcessId, create IApplicationProxy
    }
}
