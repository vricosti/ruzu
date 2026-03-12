// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/ldn/sf_service_monitor.h
//! Port of zuyu/src/core/hle/service/ldn/sf_service_monitor.cpp
//!
//! ISfServiceMonitor: LP2P service monitor.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use super::ldn_types::GroupInfo;

/// IPC command table for ISfServiceMonitor.
///
/// | Cmd | Handler      | Name                                    |
/// |-----|-------------|----------------------------------------|
/// | 0   | Initialize  | Initialize                             |
/// | 256 | nullptr     | AttachNetworkInterfaceStateChangeEvent  |
/// | 264 | nullptr     | GetNetworkInterfaceLastError            |
/// | 272 | nullptr     | GetRole                                |
/// | 280 | nullptr     | GetAdvertiseData                       |
/// | 281 | nullptr     | GetAdvertiseData2                      |
/// | 288 | GetGroupInfo| GetGroupInfo                           |
/// | 296 | nullptr     | GetGroupInfo2                          |
/// | 304 | nullptr     | GetGroupOwner                          |
/// | 312 | nullptr     | GetIpConfig                            |
/// | 320 | nullptr     | GetLinkLevel                           |
/// | 328 | nullptr     | AttachJoinEvent                        |
/// | 336 | nullptr     | GetMembers                             |
pub struct ISfServiceMonitor;

impl ISfServiceMonitor {
    pub fn new() -> Self {
        ISfServiceMonitor
    }

    /// Cmd 0: Initialize
    pub fn initialize(&self) -> (ResultCode, u32) {
        log::warn!("(STUBBED) ISfServiceMonitor::initialize called");
        (RESULT_SUCCESS, 0)
    }

    /// Cmd 288: GetGroupInfo
    pub fn get_group_info(&self) -> (ResultCode, GroupInfo) {
        log::warn!("(STUBBED) ISfServiceMonitor::get_group_info called");
        (RESULT_SUCCESS, GroupInfo::default())
    }
}
