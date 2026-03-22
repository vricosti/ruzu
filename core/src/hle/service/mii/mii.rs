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
    ///
    /// Upstream creates an IDatabaseService instance with the shared MiiManager,
    /// is_system flag, and system reference. IDatabaseService provides 26+ commands
    /// for Mii CRUD operations (IsUpdated, GetCount, Get, BuildRandom, BuildDefault,
    /// AddOrReplace, Delete, etc.) backed by MiiManager and DatabaseSessionMetadata.
    ///
    /// Requires IDatabaseService to be ported as a full ServiceFramework with
    /// MiiManager integration. The MiiManager and supporting types (CharInfo,
    /// StoreData, CoreData, Ver3StoreData) exist in mii_manager.rs and mii_types.rs
    /// but the IDatabaseService IPC wrapper has not been created yet.
    pub fn get_database_service(&self) -> ResultCode {
        log::debug!(
            "IStaticService::get_database_service called, is_system={}",
            self.is_system
        );
        log::warn!("(STUBBED) GetDatabaseService -- IDatabaseService not yet ported");
        RESULT_SUCCESS
    }
}

/// Entry point for the Mii service module.
///
/// Corresponds to `Mii::LoopProcess` in upstream mii.cpp.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::server_manager::ServerManager;
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;

    let mut server_manager = ServerManager::new(system);

    let stub = |sm: &mut ServerManager, name: &str| {
        let svc_name = name.to_string();
        sm.register_named_service(
            name,
            Box::new(move || -> SessionRequestHandlerPtr {
                std::sync::Arc::new(
                    crate::hle::service::services::GenericStubService::new(&svc_name),
                )
            }),
            64,
        );
    };
    stub(&mut server_manager, "mii:u");
    stub(&mut server_manager, "mii:e");
    ServerManager::run_server(server_manager);
}
