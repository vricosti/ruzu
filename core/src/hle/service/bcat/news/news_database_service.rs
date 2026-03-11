// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/news/news_database_service.h
//! Port of zuyu/src/core/hle/service/bcat/news/news_database_service.cpp

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

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
pub struct INewsDatabaseService;

impl INewsDatabaseService {
    pub fn new() -> Self {
        Self
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
