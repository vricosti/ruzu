// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/manager_root_service.cpp/.h

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

use super::container::Container;
use super::service_creator;
use super::vi_types::{Permission, Policy};

pub struct IManagerRootService {
    container: Arc<Container>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IManagerRootService {
    pub fn new(container: Arc<Container>) -> Self {
        Self {
            container,
            handlers: build_handler_map(&[
                (2, Some(Self::get_display_service), "GetDisplayService"),
                (
                    3,
                    Some(Self::get_display_service),
                    "GetDisplayServiceWithProxyNameExchange",
                ),
                (100, None, "PrepareFatal"),
                (101, None, "ShowFatal"),
                (102, None, "DrawFatalRectangle"),
                (103, None, "DrawFatalText32"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn create_display_service(
        &self,
        policy: Policy,
    ) -> Result<Arc<super::application_display_service::IApplicationDisplayService>, ResultCode>
    {
        service_creator::get_application_display_service(Permission::Manager, policy)?;
        Ok(Arc::new(
            super::application_display_service::IApplicationDisplayService::new(Arc::clone(
                &self.container,
            )),
        ))
    }

    fn get_display_service(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let root = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        log::debug!("IManagerRootService::GetDisplayService called");

        let mut rp = RequestParser::new(ctx);
        let Some(policy) = Policy::from_raw(rp.pop_u32()) else {
            log::error!("IManagerRootService::GetDisplayService: invalid policy");
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(super::vi_results::RESULT_PERMISSION_DENIED);
            return;
        };

        match root.create_display_service(policy) {
            Ok(display_service) => {
                let sub: Arc<dyn SessionRequestHandler> = display_service;
                super::super::am::service::application_proxy::IApplicationProxy::push_interface_response(ctx, sub);
            }
            Err(_) => {
                log::error!("GetDisplayService: permission denied");
                let mut rb = crate::hle::service::ipc_helpers::ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(super::vi_results::RESULT_PERMISSION_DENIED);
            }
        }
    }
}

impl SessionRequestHandler for IManagerRootService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        ServiceFramework::get_service_name(self)
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl ServiceFramework for IManagerRootService {
    fn get_service_name(&self) -> &str {
        "vi:m"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
