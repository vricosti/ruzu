// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/service_creator.h
//! Port of zuyu/src/core/hle/service/bcat/service_creator.cpp
//!
//! IServiceCreator: factory for BCAT sub-services.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

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
    // TODO: backend, fsc (FileSystemController)
}

impl IServiceCreator {
    pub fn new(name: &str) -> Self {
        Self {
            service_name: name.to_string(),
        }
    }

    pub fn create_bcat_service(&self, process_id: u64) -> ResultCode {
        log::info!(
            "IServiceCreator::create_bcat_service called, process_id={}",
            process_id
        );
        // TODO: create IBcatService
        RESULT_SUCCESS
    }

    pub fn create_delivery_cache_storage_service(&self, process_id: u64) -> ResultCode {
        log::info!(
            "IServiceCreator::create_delivery_cache_storage_service called, process_id={}",
            process_id
        );
        // TODO: create IDeliveryCacheStorageService
        RESULT_SUCCESS
    }

    pub fn create_delivery_cache_storage_service_with_application_id(
        &self,
        application_id: u64,
    ) -> ResultCode {
        log::debug!(
            "IServiceCreator::create_delivery_cache_storage_service_with_application_id called, application_id={:016X}",
            application_id
        );
        // TODO: create IDeliveryCacheStorageService
        RESULT_SUCCESS
    }
}
