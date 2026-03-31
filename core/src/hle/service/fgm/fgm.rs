// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/fgm/fgm.cpp
//!
//! FGM service and FGM_DBG service.

/// IPC command IDs for IRequest (returned by FGM::Initialize)
pub mod request_commands {
    pub const INITIALIZE: u32 = 0;
    pub const SET: u32 = 1;
    pub const GET: u32 = 2;
    pub const CANCEL: u32 = 3;
}

/// IPC command IDs for FGM
pub mod fgm_commands {
    pub const INITIALIZE: u32 = 0;
}

/// IPC command IDs for FGM_DBG
pub mod fgm_dbg_commands {
    pub const INITIALIZE: u32 = 0;
    pub const READ: u32 = 1;
    pub const CANCEL: u32 = 2;
}

/// IRequest interface returned by FGM::Initialize. All stubs.
pub struct IRequest;

impl IRequest {
    pub fn new() -> Self {
        Self
    }
}

/// FGM service ("fgm", "fgm:0", "fgm:9").
pub struct FGM {
    name: String,
}

impl FGM {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }

    /// Initialize (cmd 0) - creates an IRequest interface
    pub fn initialize(&self) -> IRequest {
        log::debug!("FGM({})::initialize called", self.name);
        IRequest::new()
    }
}

/// FGM_DBG service ("fgm:dbg"). All stubs.
pub struct FgmDbg;

impl FgmDbg {
    pub fn new() -> Self {
        Self
    }
}

/// Registers "fgm", "fgm:0", "fgm:9", "fgm:dbg" services.
///
/// Corresponds to `LoopProcess` in upstream `fgm.cpp`:
/// ```cpp
/// server_manager->RegisterNamedService("fgm", std::make_shared<FGM>(system, "fgm"));
/// server_manager->RegisterNamedService("fgm:0", std::make_shared<FGM>(system, "fgm:0"));
/// server_manager->RegisterNamedService("fgm:9", std::make_shared<FGM>(system, "fgm:9"));
/// server_manager->RegisterNamedService("fgm:dbg", std::make_shared<FGM_DBG>(system));
/// ```
///
/// FGM and FGM_DBG do not implement SessionRequestHandler yet, so we use stub services.
pub fn loop_process(system: crate::core::SystemRef) {
    let mut server_manager = crate::hle::service::server_manager::ServerManager::new(system);
    crate::hle::service::services::register_stub_services(
        &mut server_manager,
        &["fgm", "fgm:0", "fgm:9", "fgm:dbg"],
    );
    crate::hle::service::server_manager::ServerManager::run_server(server_manager);
}
