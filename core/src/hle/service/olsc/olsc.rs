// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/olsc/olsc.h
//! Port of zuyu/src/core/hle/service/olsc/olsc.cpp
//!
//! LoopProcess: Registers "olsc:u" and "olsc:s" services.

/// Service names registered by OLSC.
pub const SERVICE_NAME_APPLICATION: &str = "olsc:u";
pub const SERVICE_NAME_SYSTEM: &str = "olsc:s";

/// Entry point for the OLSC service module.
///
/// In upstream, this creates a ServerManager and registers two named services:
/// - "olsc:u" -> IOlscServiceForApplication
/// - "olsc:s" -> IOlscServiceForSystemService
///
/// In this port, we expose the service names as constants. The actual server loop
/// is driven by the service manager infrastructure.
pub fn loop_process() {
    log::info!("OLSC: Registering services {} and {}", SERVICE_NAME_APPLICATION, SERVICE_NAME_SYSTEM);
    // TODO: integrate with ServerManager once it is ported
}
