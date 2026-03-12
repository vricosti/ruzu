// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/mii.h
//! Port of zuyu/src/core/hle/service/mii/mii.cpp
//!
//! IStaticService and LoopProcess for the Mii service.
//! Registers: mii:e and mii:u services.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Service names registered by the Mii module.
pub const SERVICE_NAME_E: &str = "mii:e";
pub const SERVICE_NAME_U: &str = "mii:u";

/// IStaticService: the main entry point for Mii operations.
///
/// | Cmd | Handler             | Name               |
/// |-----|--------------------|--------------------|
/// | 0   | GetDatabaseService | GetDatabaseService |
pub struct IStaticService {
    pub is_system: bool,
}

impl IStaticService {
    pub fn new(is_system: bool) -> Self {
        Self { is_system }
    }

    /// Cmd 0: GetDatabaseService
    pub fn get_database_service(&self) -> ResultCode {
        log::debug!(
            "IStaticService::get_database_service called, is_system={}",
            self.is_system
        );
        // TODO: create IDatabaseService
        RESULT_SUCCESS
    }
}

/// Entry point for the Mii service module.
pub fn loop_process() {
    log::info!("Mii: Registering services {} and {}", SERVICE_NAME_E, SERVICE_NAME_U);
    // TODO: integrate with ServerManager once it is ported
}
