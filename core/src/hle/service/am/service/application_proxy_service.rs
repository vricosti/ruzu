// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/application_proxy_service.h
//! Port of zuyu/src/core/hle/service/am/service/application_proxy_service.cpp

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::am::am_types::FocusState;
use crate::hle::service::am::applet::Applet;
use crate::hle::service::am::window_system::WindowSystem;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IApplicationProxyService ("appletOE"):
/// - 0: OpenApplicationProxy
pub struct IApplicationProxyService {
    window_system: Arc<Mutex<WindowSystem>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IApplicationProxyService {
    pub fn new(window_system: Arc<Mutex<WindowSystem>>) -> Self {
        let handlers =
            build_handler_map(&[(0, Some(Self::open_application_proxy_handler), "OpenApplicationProxy")]);
        Self {
            window_system,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Port of IApplicationProxyService::OpenApplicationProxy
    ///
    /// Upstream calls GetAppletFromProcessId(pid) via WindowSystem::GetByAppletResourceUserId
    /// to find the existing applet for the calling process, then wraps it in an
    /// IApplicationProxy. The current implementation creates a fresh Applet instead
    /// of looking up an existing one, which is sufficient for the initial application
    /// launch path but diverges from upstream for multi-process scenarios.
    pub fn open_application_proxy(&self) {
        log::debug!("IApplicationProxyService::OpenApplicationProxy called");
    }

    fn open_application_proxy_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe {
            &*(this as *const dyn ServiceFramework as *const IApplicationProxyService)
        };
        service.open_application_proxy();
        let applet = Arc::new(Mutex::new(Applet::new(true)));
        {
            let mut applet_guard = applet.lock().unwrap();
            if ctx.get_pid() != 0 {
                applet_guard.aruid.pid = ctx.get_pid();
            }
            if let Some(thread) = ctx.get_thread() {
                if let Some(process) = thread.lock().unwrap().parent.as_ref().and_then(|p| p.upgrade()) {
                    let process = process.lock().unwrap();
                    if applet_guard.aruid.pid == 0 {
                        applet_guard.aruid.pid = process.get_process_id();
                    }
                    applet_guard.program_id = process.get_program_id();
                }
            }
            applet_guard.is_process_running = true;
            applet_guard.lifecycle_manager.set_focus_state(FocusState::InFocus);
            applet_guard.update_suspension_state_locked(true);
            log::info!(
                "OpenApplicationProxy: seeded applet aruid={:#x} program_id={:#x}",
                applet_guard.aruid.pid,
                applet_guard.program_id
            );
        }
        let proxy = Arc::new(super::application_proxy::IApplicationProxy::new(
            applet,
            service.window_system.clone(),
        ));
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
