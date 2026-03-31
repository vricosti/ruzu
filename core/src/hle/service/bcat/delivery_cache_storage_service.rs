// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/delivery_cache_storage_service.h
//! Port of zuyu/src/core/hle/service/bcat/delivery_cache_storage_service.cpp

use std::collections::BTreeMap;
use std::sync::Arc;

use super::bcat_types::DirectoryName;
use super::delivery_cache_directory_service::IDeliveryCacheDirectoryService;
use super::delivery_cache_file_service::IDeliveryCacheFileService;
use crate::file_sys::vfs::vfs::VfsDirectory;
use crate::file_sys::vfs::vfs_types::VirtualDir;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for IDeliveryCacheStorageService
pub mod commands {
    pub const CREATE_FILE_SERVICE: u32 = 0;
    pub const CREATE_DIRECTORY_SERVICE: u32 = 1;
    pub const ENUMERATE_DELIVERY_CACHE_DIRECTORY: u32 = 10;
}

/// IDeliveryCacheStorageService corresponds to upstream `IDeliveryCacheStorageService`.
pub struct IDeliveryCacheStorageService {
    root: VirtualDir,
    pub entries: Vec<DirectoryName>,
    pub next_read_index: usize,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IDeliveryCacheStorageService {
    pub fn new(root: VirtualDir) -> Self {
        let handlers = build_handler_map(&[
            (commands::CREATE_FILE_SERVICE, None, "CreateFileService"),
            (
                commands::CREATE_DIRECTORY_SERVICE,
                None,
                "CreateDirectoryService",
            ),
            (
                commands::ENUMERATE_DELIVERY_CACHE_DIRECTORY,
                None,
                "EnumerateDeliveryCacheDirectory",
            ),
        ]);

        Self {
            root,
            entries: Vec::new(),
            next_read_index: 0,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn create_file_service(&self) -> (ResultCode, Arc<IDeliveryCacheFileService>) {
        log::debug!("IDeliveryCacheStorageService::create_file_service called");
        let service = Arc::new(IDeliveryCacheFileService::new(self.root.clone()));
        (RESULT_SUCCESS, service)
    }

    pub fn create_directory_service(&self) -> (ResultCode, Arc<IDeliveryCacheDirectoryService>) {
        log::debug!("IDeliveryCacheStorageService::create_directory_service called");
        let service = Arc::new(IDeliveryCacheDirectoryService::new(self.root.clone()));
        (RESULT_SUCCESS, service)
    }

    pub fn enumerate_delivery_cache_directory(
        &mut self,
        out_directories: &mut [DirectoryName],
    ) -> (ResultCode, i32) {
        log::debug!(
            "IDeliveryCacheStorageService::enumerate_delivery_cache_directory called, size={:016X}",
            out_directories.len()
        );

        let count = std::cmp::min(
            out_directories.len(),
            self.entries.len() - self.next_read_index,
        );
        for i in 0..count {
            out_directories[i] = self.entries[self.next_read_index + i];
        }
        self.next_read_index += count;
        (RESULT_SUCCESS, count as i32)
    }
}

impl SessionRequestHandler for IDeliveryCacheStorageService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "IDeliveryCacheStorageService"
    }
}

impl ServiceFramework for IDeliveryCacheStorageService {
    fn get_service_name(&self) -> &str {
        "IDeliveryCacheStorageService"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
