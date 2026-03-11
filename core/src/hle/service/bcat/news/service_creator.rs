// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/news/service_creator.h
//! Port of zuyu/src/core/hle/service/bcat/news/service_creator.cpp

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

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
}

impl IServiceCreator {
    pub fn new(permissions: u32, name: &str) -> Self {
        Self {
            permissions,
            service_name: name.to_string(),
        }
    }

    pub fn create_news_service(&self) -> ResultCode {
        log::info!("News::IServiceCreator::create_news_service called");
        // TODO: create INewsService
        RESULT_SUCCESS
    }

    pub fn create_newly_arrived_event_holder(&self) -> ResultCode {
        log::info!("News::IServiceCreator::create_newly_arrived_event_holder called");
        // TODO: create INewlyArrivedEventHolder
        RESULT_SUCCESS
    }

    pub fn create_news_data_service(&self) -> ResultCode {
        log::info!("News::IServiceCreator::create_news_data_service called");
        // TODO: create INewsDataService
        RESULT_SUCCESS
    }

    pub fn create_news_database_service(&self) -> ResultCode {
        log::info!("News::IServiceCreator::create_news_database_service called");
        // TODO: create INewsDatabaseService
        RESULT_SUCCESS
    }

    pub fn create_overwrite_event_holder(&self) -> ResultCode {
        log::info!("News::IServiceCreator::create_overwrite_event_holder called");
        // TODO: create IOverwriteEventHolder
        RESULT_SUCCESS
    }
}
