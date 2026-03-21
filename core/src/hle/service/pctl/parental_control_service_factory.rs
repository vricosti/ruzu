// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/pctl/parental_control_service_factory.h
//! Port of zuyu/src/core/hle/service/pctl/parental_control_service_factory.cpp
//!
//! IParentalControlServiceFactory — creates IParentalControlService sessions.

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IParentalControlServiceFactory.
pub mod commands {
    pub const CREATE_SERVICE: u32 = 0;
    pub const CREATE_SERVICE_WITHOUT_INITIALIZE: u32 = 1;
}

/// IParentalControlServiceFactory.
///
/// Corresponds to `IParentalControlServiceFactory` in upstream.
pub struct IParentalControlServiceFactory {
    name: String,
    capability: super::pctl_types::Capability,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IParentalControlServiceFactory {
    pub fn new(name: &str, capability: super::pctl_types::Capability) -> Self {
        let handlers = build_handler_map(&[
            (
                commands::CREATE_SERVICE,
                Some(Self::create_service_handler),
                "CreateService",
            ),
            (
                commands::CREATE_SERVICE_WITHOUT_INITIALIZE,
                Some(Self::create_service_without_initialize_handler),
                "CreateServiceWithoutInitialize",
            ),
        ]);
        Self {
            name: name.to_string(),
            capability,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn create_service_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let factory = unsafe {
            &*(this as *const dyn ServiceFramework as *const IParentalControlServiceFactory)
        };
        let service: Arc<dyn crate::hle::service::hle_ipc::SessionRequestHandler> =
            Arc::new(super::parental_control_service::IParentalControlService::new(
                crate::core::SystemRef::null(),
                factory.capability,
            ));

        // Use the same domain/non-domain dispatch pattern as IApplicationProxy.
        let is_domain = ctx
            .get_manager()
            .map_or(false, |manager| manager.lock().unwrap().is_domain());
        let move_handle = if is_domain {
            0
        } else {
            ctx.create_session_for_service(service.clone()).unwrap_or(0)
        };
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        if is_domain {
            ctx.add_domain_object(service);
        } else {
            rb.push_move_objects(move_handle);
        }
    }

    fn create_service_without_initialize_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        Self::create_service_handler(this, ctx);
    }
}

impl SessionRequestHandler for IParentalControlServiceFactory {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }

    fn service_name(&self) -> &str {
        &self.name
    }
}

impl ServiceFramework for IParentalControlServiceFactory {
    fn get_service_name(&self) -> &str {
        &self.name
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
