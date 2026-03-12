// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/ldn/sf_monitor_service.h
//! Port of zuyu/src/core/hle/service/ldn/sf_monitor_service.cpp
//!
//! ISfMonitorService: LP2P monitor service.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use super::ldn_types::GroupInfo;

/// IPC command table for ISfMonitorService.
///
/// | Cmd | Handler      | Name         |
/// |-----|-------------|--------------|
/// | 0   | Initialize  | Initialize   |
/// | 288 | GetGroupInfo| GetGroupInfo |
/// | 320 | nullptr     | GetLinkLevel |
pub struct ISfMonitorService;

impl ISfMonitorService {
    pub fn new() -> Self {
        ISfMonitorService
    }

    /// Cmd 0: Initialize
    pub fn initialize(&self) -> (ResultCode, u32) {
        log::warn!("(STUBBED) ISfMonitorService::initialize called");
        (RESULT_SUCCESS, 0)
    }

    /// Cmd 288: GetGroupInfo
    pub fn get_group_info(&self) -> (ResultCode, GroupInfo) {
        log::warn!("(STUBBED) ISfMonitorService::get_group_info called");
        (RESULT_SUCCESS, GroupInfo::default())
    }
}
