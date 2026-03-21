// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/news/news_service.h
//! Port of zuyu/src/core/hle/service/bcat/news/news_service.cpp

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for INewsService
pub mod commands {
    pub const POST_LOCAL_NEWS: u32 = 10100;
    pub const SET_PASSPHRASE: u32 = 20100;
    pub const GET_SUBSCRIPTION_STATUS: u32 = 30100;
    pub const GET_TOPIC_LIST: u32 = 30101;
    pub const UNKNOWN_30110: u32 = 30110;
    pub const IS_SYSTEM_UPDATE_REQUIRED: u32 = 30200;
    pub const UNKNOWN_30201: u32 = 30201;
    pub const UNKNOWN_30210: u32 = 30210;
    pub const REQUEST_IMMEDIATE_RECEPTION: u32 = 30300;
    pub const DECODE_ARCHIVE_FILE: u32 = 30400;
    pub const UNKNOWN_30500: u32 = 30500;
    pub const UNKNOWN_30900: u32 = 30900;
    pub const UNKNOWN_30901: u32 = 30901;
    pub const UNKNOWN_30902: u32 = 30902;
    pub const SET_SUBSCRIPTION_STATUS: u32 = 40100;
    pub const REQUEST_AUTO_SUBSCRIPTION: u32 = 40101;
    pub const CLEAR_STORAGE: u32 = 40200;
    pub const CLEAR_SUBSCRIPTION_STATUS_ALL: u32 = 40201;
    pub const GET_NEWS_DATABASE_DUMP: u32 = 90100;
}

/// INewsService corresponds to upstream `News::INewsService`.
pub struct INewsService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl INewsService {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (commands::POST_LOCAL_NEWS, None, "PostLocalNews"),
            (commands::SET_PASSPHRASE, None, "SetPassphrase"),
            (commands::GET_SUBSCRIPTION_STATUS, None, "GetSubscriptionStatus"),
            (commands::GET_TOPIC_LIST, None, "GetTopicList"),
            (commands::UNKNOWN_30110, None, "Unknown30110"),
            (commands::IS_SYSTEM_UPDATE_REQUIRED, None, "IsSystemUpdateRequired"),
            (commands::UNKNOWN_30201, None, "Unknown30201"),
            (commands::UNKNOWN_30210, None, "Unknown30210"),
            (commands::REQUEST_IMMEDIATE_RECEPTION, None, "RequestImmediateReception"),
            (commands::DECODE_ARCHIVE_FILE, None, "DecodeArchiveFile"),
            (commands::UNKNOWN_30500, None, "Unknown30500"),
            (commands::UNKNOWN_30900, None, "Unknown30900"),
            (commands::UNKNOWN_30901, None, "Unknown30901"),
            (commands::UNKNOWN_30902, None, "Unknown30902"),
            (commands::SET_SUBSCRIPTION_STATUS, None, "SetSubscriptionStatus"),
            (commands::REQUEST_AUTO_SUBSCRIPTION, None, "RequestAutoSubscription"),
            (commands::CLEAR_STORAGE, None, "ClearStorage"),
            (commands::CLEAR_SUBSCRIPTION_STATUS_ALL, None, "ClearSubscriptionStatusAll"),
            (commands::GET_NEWS_DATABASE_DUMP, None, "GetNewsDatabaseDump"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn get_subscription_status(&self, _buffer_data: &[u8]) -> (ResultCode, u32) {
        log::warn!("(STUBBED) INewsService::get_subscription_status called");
        (RESULT_SUCCESS, 0)
    }

    pub fn is_system_update_required(&self) -> (ResultCode, bool) {
        log::warn!("(STUBBED) INewsService::is_system_update_required called");
        (RESULT_SUCCESS, false)
    }

    pub fn request_auto_subscription(&self, _value: u64) -> ResultCode {
        log::warn!("(STUBBED) INewsService::request_auto_subscription called");
        RESULT_SUCCESS
    }
}

impl SessionRequestHandler for INewsService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "INewsService"
    }
}

impl ServiceFramework for INewsService {
    fn get_service_name(&self) -> &str {
        "INewsService"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
