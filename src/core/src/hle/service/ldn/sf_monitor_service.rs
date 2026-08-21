// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/ldn/sf_monitor_service.h
//! Port of zuyu/src/core/hle/service/ldn/sf_monitor_service.cpp
//!
//! ISfMonitorService: LP2P monitor service.

use std::collections::BTreeMap;

use super::ldn_types::GroupInfo;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for ISfMonitorService.
///
/// | Cmd | Handler      | Name         |
/// |-----|-------------|--------------|
/// | 0   | Initialize  | Initialize   |
/// | 288 | GetGroupInfo| GetGroupInfo |
/// | 320 | nullptr     | GetLinkLevel |
pub struct ISfMonitorService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ISfMonitorService {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (0, Some(Self::initialize_handler), "Initialize"),
                (288, Some(Self::get_group_info_handler), "GetGroupInfo"),
                (320, None, "GetLinkLevel"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
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

    fn initialize_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const ISfMonitorService) };
        let (result, unknown) = service.initialize();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(result);
        rb.push_u32(unknown);
    }

    fn get_group_info_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const ISfMonitorService) };
        let (result, group_info) = service.get_group_info();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
        let bytes = unsafe {
            std::slice::from_raw_parts(
                &group_info as *const GroupInfo as *const u8,
                std::mem::size_of::<GroupInfo>(),
            )
        };
        ctx.write_buffer(bytes, 0);
    }
}

impl SessionRequestHandler for ISfMonitorService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ISfMonitorService"
    }
}

impl ServiceFramework for ISfMonitorService {
    fn get_service_name(&self) -> &str {
        "ISfMonitorService"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
