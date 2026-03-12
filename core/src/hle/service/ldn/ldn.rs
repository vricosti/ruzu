// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ldn/ldn.h
//! Port of zuyu/src/core/hle/service/ldn/ldn.cpp
//!
//! LoopProcess and internal service creator classes for LDN.
//! Registers: ldn:m, ldn:s, ldn:u, lp2p:app, lp2p:sys, lp2p:m

/// Service names registered by the LDN module.
pub const SERVICE_NAME_MONITOR: &str = "ldn:m";
pub const SERVICE_NAME_SYSTEM: &str = "ldn:s";
pub const SERVICE_NAME_USER: &str = "ldn:u";
pub const SERVICE_NAME_LP2P_APP: &str = "lp2p:app";
pub const SERVICE_NAME_LP2P_SYS: &str = "lp2p:sys";
pub const SERVICE_NAME_LP2P_MONITOR: &str = "lp2p:m";

/// IMonitorServiceCreator: "ldn:m" service.
///
/// | Cmd | Handler              | Name                 |
/// |-----|---------------------|----------------------|
/// | 0   | CreateMonitorService | CreateMonitorService |
pub struct IMonitorServiceCreator;

impl IMonitorServiceCreator {
    pub fn new() -> Self {
        IMonitorServiceCreator
    }
}

/// ISystemServiceCreator: "ldn:s" service.
///
/// | Cmd | Handler                                | Name                                   |
/// |-----|---------------------------------------|----------------------------------------|
/// | 0   | CreateSystemLocalCommunicationService | CreateSystemLocalCommunicationService   |
pub struct ISystemServiceCreator;

impl ISystemServiceCreator {
    pub fn new() -> Self {
        ISystemServiceCreator
    }
}

/// IUserServiceCreator: "ldn:u" service.
///
/// | Cmd | Handler                              | Name                                 |
/// |-----|-------------------------------------|--------------------------------------|
/// | 0   | CreateUserLocalCommunicationService | CreateUserLocalCommunicationService   |
pub struct IUserServiceCreator;

impl IUserServiceCreator {
    pub fn new() -> Self {
        IUserServiceCreator
    }
}

/// ISfServiceCreator: "lp2p:app" / "lp2p:sys" service.
///
/// | Cmd | Handler                   | Name                      |
/// |-----|--------------------------|---------------------------|
/// | 0   | CreateNetworkService     | CreateNetworkService      |
/// | 8   | CreateNetworkServiceMonitor | CreateNetworkServiceMonitor |
pub struct ISfServiceCreator {
    pub is_system: bool,
}

impl ISfServiceCreator {
    pub fn new(is_system: bool) -> Self {
        ISfServiceCreator { is_system }
    }
}

/// ISfMonitorServiceCreator: "lp2p:m" service.
///
/// | Cmd | Handler              | Name                 |
/// |-----|---------------------|----------------------|
/// | 0   | CreateMonitorService | CreateMonitorService |
pub struct ISfMonitorServiceCreator;

impl ISfMonitorServiceCreator {
    pub fn new() -> Self {
        ISfMonitorServiceCreator
    }
}

/// Entry point for the LDN service module.
pub fn loop_process() {
    log::info!(
        "LDN: Registering services {}, {}, {}, {}, {}, {}",
        SERVICE_NAME_MONITOR,
        SERVICE_NAME_SYSTEM,
        SERVICE_NAME_USER,
        SERVICE_NAME_LP2P_APP,
        SERVICE_NAME_LP2P_SYS,
        SERVICE_NAME_LP2P_MONITOR,
    );
    // TODO: integrate with ServerManager once it is ported
}
