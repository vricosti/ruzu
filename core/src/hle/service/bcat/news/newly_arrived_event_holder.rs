// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/news/newly_arrived_event_holder.h
//! Port of zuyu/src/core/hle/service/bcat/news/newly_arrived_event_holder.cpp

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::result::RESULT_SUCCESS;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for INewlyArrivedEventHolder
pub mod commands {
    pub const GET: u32 = 0;
}

/// INewlyArrivedEventHolder corresponds to upstream `News::INewlyArrivedEventHolder`.
pub struct INewlyArrivedEventHolder {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    // TODO: arrived_event: KEvent, service_context
}

impl INewlyArrivedEventHolder {
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
        log::info!("INewlyArrivedEventHolder::get called");
        // TODO: return arrived_event readable event handle
        RESULT_SUCCESS
    }
}

impl SessionRequestHandler for INewlyArrivedEventHolder {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "INewlyArrivedEventHolder"
    }
}

impl ServiceFramework for INewlyArrivedEventHolder {
    fn get_service_name(&self) -> &str {
        "INewlyArrivedEventHolder"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
