// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/delivery_cache_progress_service.h
//! Port of zuyu/src/core/hle/service/bcat/delivery_cache_progress_service.cpp

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use super::bcat_types::DeliveryCacheProgressImpl;

/// IPC command IDs for IDeliveryCacheProgressService
pub mod commands {
    pub const GET_EVENT: u32 = 0;
    pub const GET_IMPL: u32 = 1;
}

/// IDeliveryCacheProgressService corresponds to upstream `IDeliveryCacheProgressService`.
pub struct IDeliveryCacheProgressService {
    // TODO: event reference
    pub impl_data: DeliveryCacheProgressImpl,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IDeliveryCacheProgressService {
    pub fn new(impl_data: DeliveryCacheProgressImpl) -> Self {
        let handlers = build_handler_map(&[
            (commands::GET_EVENT, None, "Get"),
            (commands::GET_IMPL, None, "Get"),
        ]);

        Self {
            impl_data,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn get_event(&self) -> ResultCode {
        log::debug!("IDeliveryCacheProgressService::get_event called");
        // TODO: return event handle
        RESULT_SUCCESS
    }

    pub fn get_impl(&self) -> (ResultCode, &DeliveryCacheProgressImpl) {
        log::debug!("IDeliveryCacheProgressService::get_impl called");
        (RESULT_SUCCESS, &self.impl_data)
    }
}

impl SessionRequestHandler for IDeliveryCacheProgressService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "IDeliveryCacheProgressService"
    }
}

impl ServiceFramework for IDeliveryCacheProgressService {
    fn get_service_name(&self) -> &str {
        "IDeliveryCacheProgressService"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
