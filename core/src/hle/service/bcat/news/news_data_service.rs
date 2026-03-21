// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/news/news_data_service.h
//! Port of zuyu/src/core/hle/service/bcat/news/news_data_service.cpp

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for INewsDataService
pub mod commands {
    pub const OPEN: u32 = 0;
    pub const OPEN_WITH_NEWS_RECORD_V1: u32 = 1;
    pub const READ: u32 = 2;
    pub const GET_SIZE: u32 = 3;
    pub const OPEN_WITH_NEWS_RECORD: u32 = 1001;
}

/// INewsDataService corresponds to upstream `News::INewsDataService`.
/// All commands are nullptr (unimplemented) in upstream.
pub struct INewsDataService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl INewsDataService {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (commands::OPEN, None, "Open"),
            (commands::OPEN_WITH_NEWS_RECORD_V1, None, "OpenWithNewsRecordV1"),
            (commands::READ, None, "Read"),
            (commands::GET_SIZE, None, "GetSize"),
            (commands::OPEN_WITH_NEWS_RECORD, None, "OpenWithNewsRecord"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for INewsDataService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "INewsDataService"
    }
}

impl ServiceFramework for INewsDataService {
    fn get_service_name(&self) -> &str {
        "INewsDataService"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
