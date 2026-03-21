// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/news/news_database_service.h
//! Port of zuyu/src/core/hle/service/bcat/news/news_database_service.cpp

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for INewsDatabaseService
pub mod commands {
    pub const GET_LIST_V1: u32 = 0;
    pub const COUNT: u32 = 1;
    pub const COUNT_WITH_KEY: u32 = 2;
    pub const UPDATE_INTEGER_VALUE: u32 = 3;
    pub const UPDATE_INTEGER_VALUE_WITH_ADDITION: u32 = 4;
    pub const UPDATE_STRING_VALUE: u32 = 5;
    pub const GET_LIST: u32 = 1000;
}

/// INewsDatabaseService corresponds to upstream `News::INewsDatabaseService`.
pub struct INewsDatabaseService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl INewsDatabaseService {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (commands::GET_LIST_V1, None, "GetListV1"),
            (commands::COUNT, None, "Count"),
            (commands::COUNT_WITH_KEY, None, "CountWithKey"),
            (commands::UPDATE_INTEGER_VALUE, None, "UpdateIntegerValue"),
            (commands::UPDATE_INTEGER_VALUE_WITH_ADDITION, None, "UpdateIntegerValueWithAddition"),
            (commands::UPDATE_STRING_VALUE, None, "UpdateStringValue"),
            (commands::GET_LIST, None, "GetList"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn count(&self, _buffer_data: &[u8]) -> (ResultCode, i32) {
        log::warn!("(STUBBED) INewsDatabaseService::count called");
        (RESULT_SUCCESS, 0)
    }

    pub fn update_integer_value_with_addition(
        &self,
        _value: u32,
        _buffer_data_1: &[u8],
        _buffer_data_2: &[u8],
    ) -> ResultCode {
        log::warn!("(STUBBED) INewsDatabaseService::update_integer_value_with_addition called");
        RESULT_SUCCESS
    }

    pub fn get_list(
        &self,
        _value: u32,
        _out_buffer_data: &mut [u8],
        _buffer_data_1: &[u8],
        _buffer_data_2: &[u8],
    ) -> (ResultCode, i32) {
        log::warn!("(STUBBED) INewsDatabaseService::get_list called");
        (RESULT_SUCCESS, 0)
    }
}

impl SessionRequestHandler for INewsDatabaseService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "INewsDatabaseService"
    }
}

impl ServiceFramework for INewsDatabaseService {
    fn get_service_name(&self) -> &str {
        "INewsDatabaseService"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
