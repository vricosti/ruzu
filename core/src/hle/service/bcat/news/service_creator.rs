// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/news/service_creator.h
//! Port of zuyu/src/core/hle/service/bcat/news/service_creator.cpp

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use super::newly_arrived_event_holder::INewlyArrivedEventHolder;
use super::news_data_service::INewsDataService;
use super::news_database_service::INewsDatabaseService;
use super::news_service::INewsService;
use super::overwrite_event_holder::IOverwriteEventHolder;

/// IPC command IDs for News::IServiceCreator
pub mod commands {
    pub const CREATE_NEWS_SERVICE: u32 = 0;
    pub const CREATE_NEWLY_ARRIVED_EVENT_HOLDER: u32 = 1;
    pub const CREATE_NEWS_DATA_SERVICE: u32 = 2;
    pub const CREATE_NEWS_DATABASE_SERVICE: u32 = 3;
    pub const CREATE_OVERWRITE_EVENT_HOLDER: u32 = 4;
}

/// News::IServiceCreator corresponds to upstream `News::IServiceCreator`.
pub struct IServiceCreator {
    pub permissions: u32,
    pub service_name: String,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IServiceCreator {
    pub fn new(permissions: u32, name: &str) -> Self {
        let handlers = build_handler_map(&[
            (commands::CREATE_NEWS_SERVICE, None, "CreateNewsService"),
            (commands::CREATE_NEWLY_ARRIVED_EVENT_HOLDER, None, "CreateNewlyArrivedEventHolder"),
            (commands::CREATE_NEWS_DATA_SERVICE, None, "CreateNewsDataService"),
            (commands::CREATE_NEWS_DATABASE_SERVICE, None, "CreateNewsDatabaseService"),
            (commands::CREATE_OVERWRITE_EVENT_HOLDER, None, "CreateOverwriteEventHolder"),
        ]);

        Self {
            permissions,
            service_name: name.to_string(),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn create_news_service(&self) -> (ResultCode, Arc<INewsService>) {
        log::info!("News::IServiceCreator::create_news_service called");
        let service = Arc::new(INewsService::new());
        (RESULT_SUCCESS, service)
    }

    pub fn create_newly_arrived_event_holder(
        &self,
    ) -> (ResultCode, Arc<INewlyArrivedEventHolder>) {
        log::info!("News::IServiceCreator::create_newly_arrived_event_holder called");
        let service = Arc::new(INewlyArrivedEventHolder::new());
        (RESULT_SUCCESS, service)
    }

    pub fn create_news_data_service(&self) -> (ResultCode, Arc<INewsDataService>) {
        log::info!("News::IServiceCreator::create_news_data_service called");
        let service = Arc::new(INewsDataService::new());
        (RESULT_SUCCESS, service)
    }

    pub fn create_news_database_service(&self) -> (ResultCode, Arc<INewsDatabaseService>) {
        log::info!("News::IServiceCreator::create_news_database_service called");
        let service = Arc::new(INewsDatabaseService::new());
        (RESULT_SUCCESS, service)
    }

    pub fn create_overwrite_event_holder(
        &self,
    ) -> (ResultCode, Arc<IOverwriteEventHolder>) {
        log::info!("News::IServiceCreator::create_overwrite_event_holder called");
        let service = Arc::new(IOverwriteEventHolder::new());
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
