// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/news/overwrite_event_holder.h
//! Port of zuyu/src/core/hle/service/bcat/news/overwrite_event_holder.cpp

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::result::RESULT_SUCCESS;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for IOverwriteEventHolder
pub mod commands {
    pub const GET: u32 = 0;
}

/// IOverwriteEventHolder corresponds to upstream `News::IOverwriteEventHolder`.
pub struct IOverwriteEventHolder {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    // TODO: overwrite_event: KEvent, service_context
}

impl IOverwriteEventHolder {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (commands::GET, None, "Get"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn get(&self) -> ResultCode {
        log::info!("IOverwriteEventHolder::get called");
        // TODO: return overwrite_event readable event handle
        RESULT_SUCCESS
    }
}

impl SessionRequestHandler for IOverwriteEventHolder {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "IOverwriteEventHolder"
    }
}

impl ServiceFramework for IOverwriteEventHolder {
    fn get_service_name(&self) -> &str {
        "IOverwriteEventHolder"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
