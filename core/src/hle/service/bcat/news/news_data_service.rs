// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/news/news_data_service.h
//! Port of zuyu/src/core/hle/service/bcat/news/news_data_service.cpp

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
pub struct INewsDataService;

impl INewsDataService {
    pub fn new() -> Self {
        Self
    }
}
