// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ngc/ngc.cpp
//!
//! NgctServiceImpl ("ngct:u") and NgcServiceImpl ("ngc:u") services.

/// IPC command IDs for NgctServiceImpl ("ngct:u")
pub mod ngct_commands {
    pub const MATCH: u32 = 0;
    pub const FILTER: u32 = 1;
}

/// IPC command IDs for NgcServiceImpl ("ngc:u")
pub mod ngc_commands {
    pub const GET_CONTENT_VERSION: u32 = 0;
    pub const CHECK: u32 = 1;
    pub const MASK: u32 = 2;
    pub const RELOAD: u32 = 3;
}

/// Upstream: `NgcContentVersion` constant.
pub const NGC_CONTENT_VERSION: u32 = 1;

/// ProfanityFilterOption. Upstream: `ProfanityFilterOption` in `ngc.cpp`.
#[repr(C)]
pub struct ProfanityFilterOption {
    pub _data: [u8; 0x20],
}

/// NgctServiceImpl ("ngct:u").
pub struct NgctServiceImpl;

impl NgctServiceImpl {
    pub fn new() -> Self {
        Self
    }

    /// Match (cmd 0) - returns false since we don't censor anything
    pub fn match_text(&self, text: &str) -> bool {
        log::warn!("(STUBBED) NgctServiceImpl::match called, text={}", text);
        false
    }

    /// Filter (cmd 1) - returns same string since we don't censor anything
    pub fn filter(&self, buffer: &[u8]) -> Vec<u8> {
        log::warn!("(STUBBED) NgctServiceImpl::filter called");
        buffer.to_vec()
    }
}

/// NgcServiceImpl ("ngc:u").
pub struct NgcServiceImpl;

impl NgcServiceImpl {
    pub fn new() -> Self {
        Self
    }

    /// GetContentVersion (cmd 0)
    pub fn get_content_version(&self) -> u32 {
        log::info!("(STUBBED) NgcServiceImpl::get_content_version called");
        NGC_CONTENT_VERSION
    }

    /// Check (cmd 1) - returns 0 flags (no profanity detected)
    pub fn check(&self, _flags: u32, _option: &ProfanityFilterOption, _input: &[u8]) -> u32 {
        log::info!("(STUBBED) NgcServiceImpl::check called");
        0
    }

    /// Mask (cmd 2) - returns input unchanged and 0 flags
    pub fn mask(
        &self,
        _flags: u32,
        _option: &ProfanityFilterOption,
        input: &[u8],
    ) -> (u32, Vec<u8>) {
        log::info!("(STUBBED) NgcServiceImpl::mask called");
        (0, input.to_vec())
    }

    /// Reload (cmd 3)
    pub fn reload(&self) {
        log::info!("(STUBBED) NgcServiceImpl::reload called");
    }
}

/// Registers "ngc:u" service.
///
/// Corresponds to `LoopProcess` in upstream `ngc.cpp`.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::server_manager::ServerManager;

    let mut server_manager = ServerManager::new(system);

    let stub = |sm: &mut ServerManager, name: &str| {
        let svc_name = name.to_string();
        sm.register_named_service(
            name,
            Box::new(move || -> SessionRequestHandlerPtr {
                std::sync::Arc::new(crate::hle::service::services::GenericStubService::new(
                    &svc_name,
                ))
            }),
            64,
        );
    };
    stub(&mut server_manager, "ngc:u");

    ServerManager::run_server(server_manager);
}
