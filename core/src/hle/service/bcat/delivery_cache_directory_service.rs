// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/delivery_cache_directory_service.h
//! Port of zuyu/src/core/hle/service/bcat/delivery_cache_directory_service.cpp

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use super::bcat_result;
use super::bcat_types::*;

/// IPC command IDs for IDeliveryCacheDirectoryService
pub mod commands {
    pub const OPEN: u32 = 0;
    pub const READ: u32 = 1;
    pub const GET_COUNT: u32 = 2;
}

/// IDeliveryCacheDirectoryService corresponds to upstream `IDeliveryCacheDirectoryService`.
pub struct IDeliveryCacheDirectoryService {
    // TODO: root: VirtualDir, current_dir: Option<VirtualDir>
    pub has_open_dir: bool,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IDeliveryCacheDirectoryService {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (commands::OPEN, None, "Open"),
            (commands::READ, None, "Read"),
            (commands::GET_COUNT, None, "GetCount"),
        ]);

        Self {
            has_open_dir: false,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn open(&mut self, dir_name_raw: &DirectoryName) -> ResultCode {
        log::debug!("IDeliveryCacheDirectoryService::open called");

        let dir_verify = super::bcat_util::verify_name_valid_dir(dir_name_raw);
        if dir_verify.is_error() {
            return dir_verify;
        }
        if self.has_open_dir {
            return bcat_result::RESULT_ENTITY_ALREADY_OPEN;
        }
        // TODO: look up directory in VFS
        bcat_result::RESULT_FAILED_OPEN_ENTITY
    }

    pub fn read(
        &self,
        _out_buffer: &mut [DeliveryCacheDirectoryEntry],
    ) -> (ResultCode, i32) {
        log::debug!("IDeliveryCacheDirectoryService::read called");
        if !self.has_open_dir {
            return (bcat_result::RESULT_NO_OPEN_ENTRY, 0);
        }
        // TODO: read entries from current_dir
        (RESULT_SUCCESS, 0)
    }

    pub fn get_count(&self) -> (ResultCode, i32) {
        log::debug!("IDeliveryCacheDirectoryService::get_count called");
        if !self.has_open_dir {
            return (bcat_result::RESULT_NO_OPEN_ENTRY, 0);
        }
        // TODO: return current_dir file count
        (RESULT_SUCCESS, 0)
    }
}

impl SessionRequestHandler for IDeliveryCacheDirectoryService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "IDeliveryCacheDirectoryService"
    }
}

impl ServiceFramework for IDeliveryCacheDirectoryService {
    fn get_service_name(&self) -> &str {
        "IDeliveryCacheDirectoryService"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
