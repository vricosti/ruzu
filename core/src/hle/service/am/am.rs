// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/am.h
//! Port of zuyu/src/core/hle/service/am/am.cpp
//!
//! Entry point for the AM service. Registers "appletAE" and "appletOE"
//! named services.

/// Port of Service::AM::LoopProcess
///
/// In the C++ version, this creates a WindowSystem, ButtonPoller,
/// EventObserver, and registers the two named services. Stubbed until
/// ServerManager and related infrastructure are wired.
pub fn loop_process() {
    log::info!("AM::LoopProcess called (stubbed)");
    // TODO: create WindowSystem, ButtonPoller, EventObserver
    // TODO: register "appletAE" -> IAllSystemAppletProxiesService
    // TODO: register "appletOE" -> IApplicationProxyService
}
