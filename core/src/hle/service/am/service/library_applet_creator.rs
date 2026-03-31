// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/library_applet_creator.h
//! Port of zuyu/src/core/hle/service/am/service/library_applet_creator.cpp

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for ILibraryAppletCreator:
/// - 0: CreateLibraryApplet
/// - 1: TerminateAllLibraryApplets
/// - 2: AreAnyLibraryAppletsLeft
/// - 10: CreateStorage
/// - 11: CreateTransferMemoryStorage
/// - 12: CreateHandleStorage
pub struct ILibraryAppletCreator {
    applet: Arc<Mutex<crate::hle::service::am::applet::Applet>>,
    window_system: Arc<Mutex<crate::hle::service::am::window_system::WindowSystem>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ILibraryAppletCreator {
    pub fn new(
        applet: Arc<Mutex<crate::hle::service::am::applet::Applet>>,
        window_system: Arc<Mutex<crate::hle::service::am::window_system::WindowSystem>>,
    ) -> Self {
        let handlers = build_handler_map(&[
            (0, None, "CreateLibraryApplet"),
            (1, None, "TerminateAllLibraryApplets"),
            (2, None, "AreAnyLibraryAppletsLeft"),
            (10, Some(Self::create_storage_handler), "CreateStorage"),
            (
                11,
                Some(Self::create_transfer_memory_storage_handler),
                "CreateTransferMemoryStorage",
            ),
            (12, None, "CreateHandleStorage"),
        ]);
        Self {
            applet,
            window_system,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn create_storage_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let size = rp.pop_i64();
        log::warn!("(STUBBED) CreateStorage called, size={}", size);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn create_transfer_memory_storage_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("(STUBBED) CreateTransferMemoryStorage called");

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for ILibraryAppletCreator {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
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
