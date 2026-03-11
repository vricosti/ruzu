// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/delivery_cache_storage_service.h
//! Port of zuyu/src/core/hle/service/bcat/delivery_cache_storage_service.cpp

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use super::bcat_types::DirectoryName;

/// IPC command IDs for IDeliveryCacheStorageService
pub mod commands {
    pub const CREATE_FILE_SERVICE: u32 = 0;
    pub const CREATE_DIRECTORY_SERVICE: u32 = 1;
    pub const ENUMERATE_DELIVERY_CACHE_DIRECTORY: u32 = 10;
}

/// IDeliveryCacheStorageService corresponds to upstream `IDeliveryCacheStorageService`.
pub struct IDeliveryCacheStorageService {
    // TODO: root: VirtualDir
    pub entries: Vec<DirectoryName>,
    pub next_read_index: usize,
}

impl IDeliveryCacheStorageService {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            next_read_index: 0,
        }
    }

    pub fn create_file_service(&self) -> ResultCode {
        log::debug!("IDeliveryCacheStorageService::create_file_service called");
        // TODO: create IDeliveryCacheFileService
        RESULT_SUCCESS
    }

    pub fn create_directory_service(&self) -> ResultCode {
        log::debug!("IDeliveryCacheStorageService::create_directory_service called");
        // TODO: create IDeliveryCacheDirectoryService
        RESULT_SUCCESS
    }

    pub fn enumerate_delivery_cache_directory(
        &mut self,
        out_directories: &mut [DirectoryName],
    ) -> (ResultCode, i32) {
        log::debug!(
            "IDeliveryCacheStorageService::enumerate_delivery_cache_directory called, size={:016X}",
            out_directories.len()
        );

        let count = std::cmp::min(out_directories.len(), self.entries.len() - self.next_read_index);
        for i in 0..count {
            out_directories[i] = self.entries[self.next_read_index + i];
        }
        self.next_read_index += count;
        (RESULT_SUCCESS, count as i32)
    }
}
