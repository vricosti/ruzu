// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/library_applet_creator.h
//! Port of zuyu/src/core/hle/service/am/service/library_applet_creator.cpp

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for ILibraryAppletCreator:
/// - 0: CreateLibraryApplet
/// - 10: CreateStorage
/// - 11: CreateTransferMemoryStorage
/// - 12: CreateHandleStorage
pub struct ILibraryAppletCreator {
    // TODO: WindowSystem reference, Applet reference
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ILibraryAppletCreator {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for ILibraryAppletCreator {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        let mut rb = ResponseBuilder::new(context, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        RESULT_SUCCESS
    }
}

impl ServiceFramework for ILibraryAppletCreator {
    fn get_service_name(&self) -> &str {
        "am::ILibraryAppletCreator"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
