// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/nvmemp.h
//! Port of zuyu/src/core/hle/service/nvdrv/nvmemp.cpp

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for NVMEMP:
/// - 0: Open
/// - 1: GetAruid
pub struct Nvmemp {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl Nvmemp {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[(0, None, "Open"), (1, None, "GetAruid")]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for Nvmemp {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "nvmemp"
    }
}

impl ServiceFramework for Nvmemp {
    fn get_service_name(&self) -> &str {
        "nvmemp"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
