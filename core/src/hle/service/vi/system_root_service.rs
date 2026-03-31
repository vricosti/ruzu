// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/system_root_service.cpp/.h

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

use super::container::Container;
use super::service_creator;
use super::vi_types::{Permission, Policy};

pub struct ISystemRootService {
    container: Arc<Container>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ISystemRootService {
    pub fn new(container: Arc<Container>) -> Self {
        Self {
            container,
            handlers: build_handler_map(&[
                (1, Some(Self::get_display_service), "GetDisplayService"),
                (
                    3,
                    Some(Self::get_display_service),
                    "GetDisplayServiceWithProxyNameExchange",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn get_display_service(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let root = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        log::debug!("ISystemRootService::GetDisplayService called");

        let mut rp = RequestParser::new(ctx);
        let Some(policy) = Policy::from_raw(rp.pop_u32()) else {
            log::error!("ISystemRootService::GetDisplayService: invalid policy");
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(super::vi_results::RESULT_PERMISSION_DENIED);
            return;
        };

        match service_creator::get_application_display_service(Permission::System, policy) {
            Ok(()) => {
                let display_service =
                    super::application_display_service::IApplicationDisplayService::new(
                        Arc::clone(&root.container),
                    );
                let sub: Arc<dyn SessionRequestHandler> = Arc::new(display_service);
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

impl SessionRequestHandler for ISystemRootService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        ServiceFramework::get_service_name(self)
    }
}

impl ServiceFramework for ISystemRootService {
    fn get_service_name(&self) -> &str {
        "vi:s"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
