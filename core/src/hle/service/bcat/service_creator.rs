// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/service_creator.h
//! Port of zuyu/src/core/hle/service/bcat/service_creator.cpp
//!
//! IServiceCreator: factory for BCAT sub-services.

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use super::backend::{BcatBackend, NullBcatBackend};
use super::bcat_service::IBcatService;
use super::delivery_cache_storage_service::IDeliveryCacheStorageService;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for IServiceCreator
pub mod commands {
    pub const CREATE_BCAT_SERVICE: u32 = 0;
    pub const CREATE_DELIVERY_CACHE_STORAGE_SERVICE: u32 = 1;
    pub const CREATE_DELIVERY_CACHE_STORAGE_SERVICE_WITH_APPLICATION_ID: u32 = 2;
    pub const CREATE_DELIVERY_CACHE_PROGRESS_SERVICE: u32 = 3;
    pub const CREATE_DELIVERY_CACHE_PROGRESS_SERVICE_WITH_APPLICATION_ID: u32 = 4;
}

/// IServiceCreator corresponds to `IServiceCreator` in upstream `service_creator.h`.
pub struct IServiceCreator {
    pub service_name: String,
    backend: Arc<Mutex<dyn BcatBackend + Send>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IServiceCreator {
    pub fn new(name: &str) -> Self {
        let handlers = build_handler_map(&[
            (commands::CREATE_BCAT_SERVICE, None, "CreateBcatService"),
            (
                commands::CREATE_DELIVERY_CACHE_STORAGE_SERVICE,
                None,
                "CreateDeliveryCacheStorageService",
            ),
            (
                commands::CREATE_DELIVERY_CACHE_STORAGE_SERVICE_WITH_APPLICATION_ID,
                None,
                "CreateDeliveryCacheStorageServiceWithApplicationId",
            ),
            (
                commands::CREATE_DELIVERY_CACHE_PROGRESS_SERVICE,
                None,
                "CreateDeliveryCacheProgressService",
            ),
            (
                commands::CREATE_DELIVERY_CACHE_PROGRESS_SERVICE_WITH_APPLICATION_ID,
                None,
                "CreateDeliveryCacheProgressServiceWithApplicationId",
            ),
        ]);

        // Upstream: backend = CreateBackendFromSettings(system_, [this](u64 tid) { return fsc.GetBCATDirectory(tid); });
        // CreateBackendFromSettings always creates NullBcatBackend.
        let backend: Arc<Mutex<dyn BcatBackend + Send>> =
            Arc::new(Mutex::new(NullBcatBackend::new()));

        Self {
            service_name: name.to_string(),
            backend,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn create_bcat_service(&self, process_id: u64) -> (ResultCode, Arc<IBcatService>) {
        log::info!(
            "IServiceCreator::create_bcat_service called, process_id={}",
            process_id
        );
        let service = Arc::new(IBcatService::new(self.backend.clone()));
        (RESULT_SUCCESS, service)
    }

    pub fn create_delivery_cache_storage_service(
        &self,
        process_id: u64,
    ) -> (ResultCode, Arc<IDeliveryCacheStorageService>) {
        log::info!(
            "IServiceCreator::create_delivery_cache_storage_service called, process_id={}",
            process_id
        );
        // Upstream: const auto title_id = system.GetApplicationProcessProgramID();
        // Then creates IDeliveryCacheStorageService with fsc.GetBCATDirectory(title_id).
        // For now, use a null VFS directory since we don't have the system reference yet.
        let root: crate::file_sys::vfs::vfs_types::VirtualDir =
            Arc::new(crate::file_sys::vfs::vfs_vector::VectorVfsDirectory::new(
                Vec::new(),
                Vec::new(),
                String::new(),
                None,
            ));
        let service = Arc::new(IDeliveryCacheStorageService::new(root));
        (RESULT_SUCCESS, service)
    }

    pub fn create_delivery_cache_storage_service_with_application_id(
        &self,
        application_id: u64,
    ) -> (ResultCode, Arc<IDeliveryCacheStorageService>) {
        log::debug!(
            "IServiceCreator::create_delivery_cache_storage_service_with_application_id called, application_id={:016X}",
            application_id
        );
        // Upstream: creates IDeliveryCacheStorageService with fsc.GetBCATDirectory(application_id).
        // For now, use a null VFS directory since we don't have the system reference yet.
        let root: crate::file_sys::vfs::vfs_types::VirtualDir =
            Arc::new(crate::file_sys::vfs::vfs_vector::VectorVfsDirectory::new(
                Vec::new(),
                Vec::new(),
                String::new(),
                None,
            ));
        let service = Arc::new(IDeliveryCacheStorageService::new(root));
        (RESULT_SUCCESS, service)
    }
}

impl SessionRequestHandler for IServiceCreator {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        &self.service_name
    }
}

impl ServiceFramework for IServiceCreator {
    fn get_service_name(&self) -> &str {
        &self.service_name
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
