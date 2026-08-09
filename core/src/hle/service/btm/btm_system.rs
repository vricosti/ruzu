// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/btm/btm_system.h
//! Port of zuyu/src/core/hle/service/btm/btm_system.cpp
//!
//! IBtmSystem — "btm:sys".

use crate::core::SystemRef;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use std::collections::BTreeMap;
use std::sync::Arc;

/// IBtmSystem.
pub struct IBtmSystem {
    system: SystemRef,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IBtmSystem {
    pub fn new(system: SystemRef) -> Self {
        let handlers = build_handler_map(&[(0, Some(Self::get_core_handler), "GetCore")]);

        Self {
            system,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn get_core_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IBtmSystem) };
        log::warn!("IBtmSystem::GetCore called");
        let core = super::btm_system_core::IBtmSystemCore::new(service.system);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(Arc::new(core));
    }
}

impl SessionRequestHandler for IBtmSystem {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        "btm:sys"
    }
}

impl ServiceFramework for IBtmSystem {
    fn get_service_name(&self) -> &str {
        "btm:sys"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_core_is_registered() {
        let service = IBtmSystem::new(SystemRef::null());
        assert!(service.handlers.get(&0).unwrap().handler_callback.is_some());
    }
}
