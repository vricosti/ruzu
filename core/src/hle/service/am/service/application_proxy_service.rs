// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/application_proxy_service.h
//! Port of zuyu/src/core/hle/service/am/service/application_proxy_service.cpp

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::am::am_types::FocusState;
use crate::hle::service::am::applet::Applet;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IApplicationProxyService ("appletOE"):
/// - 0: OpenApplicationProxy
pub struct IApplicationProxyService {
    // TODO: WindowSystem reference
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IApplicationProxyService {
    pub fn new() -> Self {
        let handlers =
            build_handler_map(&[(0, Some(Self::open_application_proxy_handler), "OpenApplicationProxy")]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Port of IApplicationProxyService::OpenApplicationProxy
    pub fn open_application_proxy(&self) {
        log::debug!("IApplicationProxyService::OpenApplicationProxy called");
        // TODO: GetAppletFromProcessId, create IApplicationProxy
    }

    fn open_application_proxy_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe {
            &*(this as *const dyn ServiceFramework as *const IApplicationProxyService)
        };
        service.open_application_proxy();
        // Create an Applet for this application.
        // Matches upstream: AppletManager creates the Applet and calls
        // SetFocusState(InFocus) when the application starts.
        let applet = Arc::new(Mutex::new(Applet::new(true)));
        {
            let mut applet_guard = applet.lock().unwrap();
            applet_guard.lifecycle_manager.set_focus_state(FocusState::InFocus);
        }
        let proxy = Arc::new(super::application_proxy::IApplicationProxy::new(applet));
        let is_domain = ctx
            .get_manager()
            .map_or(false, |manager| manager.lock().unwrap().is_domain());
        let move_handle = if is_domain {
            0
        } else {
            ctx.create_session_for_service(proxy.clone()).unwrap_or(0)
        };
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        if is_domain {
            ctx.add_domain_object(proxy);
        } else {
            rb.push_move_objects(move_handle);
        }
    }
}

impl SessionRequestHandler for IApplicationProxyService {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IApplicationProxyService {
    fn get_service_name(&self) -> &str {
        "appletOE"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
