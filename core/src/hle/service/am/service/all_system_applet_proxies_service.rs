// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/all_system_applet_proxies_service.h
//! Port of zuyu/src/core/hle/service/am/service/all_system_applet_proxies_service.cpp

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::core::SystemRef;
use crate::hle::result::{ResultCode, RESULT_SUCCESS, RESULT_UNKNOWN};
use crate::hle::service::am::applet::Applet;
use crate::hle::service::am::window_system::WindowSystem;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IAllSystemAppletProxiesService ("appletAE"):
/// - 100: OpenSystemAppletProxy
/// - 200: OpenLibraryAppletProxyOld
/// - 201: OpenLibraryAppletProxy
/// - 300: OpenOverlayAppletProxy (unimplemented)
/// - 350: OpenSystemApplicationProxy (unimplemented)
/// - 400: CreateSelfLibraryAppletCreatorForDevelop (unimplemented)
/// - 410: GetSystemAppletControllerForDebug (unimplemented)
/// - 1000: GetDebugFunctions (unimplemented)
pub struct IAllSystemAppletProxiesService {
    system: SystemRef,
    window_system: Arc<Mutex<WindowSystem>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IAllSystemAppletProxiesService {
    pub fn new(system: SystemRef, window_system: Arc<Mutex<WindowSystem>>) -> Self {
        let handlers = build_handler_map(&[
            (
                100,
                Some(Self::open_system_applet_proxy_handler),
                "OpenSystemAppletProxy",
            ),
            (
                200,
                Some(Self::open_library_applet_proxy_old_handler),
                "OpenLibraryAppletProxyOld",
            ),
            (
                201,
                Some(Self::open_library_applet_proxy_handler),
                "OpenLibraryAppletProxy",
            ),
            (300, None, "OpenOverlayAppletProxy"),
            (350, None, "OpenSystemApplicationProxy"),
            (400, None, "CreateSelfLibraryAppletCreatorForDevelop"),
            (410, None, "GetSystemAppletControllerForDebug"),
            (1000, None, "GetDebugFunctions"),
        ]);
        Self {
            system,
            window_system,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn get_applet_from_process_id(&self, pid: u64) -> Option<Arc<Mutex<Applet>>> {
        self.window_system
            .lock()
            .unwrap()
            .get_by_applet_resource_user_id(pid)
    }

    fn push_interface_response(
        ctx: &mut HLERequestContext,
        object: Arc<dyn SessionRequestHandler>,
    ) {
        let is_domain = ctx
            .get_manager()
            .map_or(false, |manager| manager.lock().unwrap().is_domain());
        let move_handle = if is_domain {
            0
        } else {
            ctx.create_session_for_service(object.clone()).unwrap_or(0)
        };
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        if is_domain {
            ctx.add_domain_object(object);
        } else {
            rb.push_move_objects(move_handle);
        }
    }

    fn open_system_applet_proxy_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe {
            &*(this as *const dyn ServiceFramework as *const IAllSystemAppletProxiesService)
        };
        log::debug!("IAllSystemAppletProxiesService::OpenSystemAppletProxy");
        let pid = if ctx.get_pid() != 0 {
            ctx.get_pid()
        } else {
            ctx.get_thread()
                .and_then(|thread| {
                    thread
                        .lock()
                        .unwrap()
                        .parent
                        .as_ref()
                        .and_then(|p| p.upgrade())
                })
                .map(|process| process.lock().unwrap().get_process_id())
                .unwrap_or(0)
        };
        let Some(applet) = service.get_applet_from_process_id(pid) else {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(RESULT_UNKNOWN);
            return;
        };
        let proxy = Arc::new(super::system_applet_proxy::ISystemAppletProxy::new(
            service.system,
            applet,
            service.window_system.clone(),
        ));
        Self::push_interface_response(ctx, proxy);
    }

    fn open_library_applet_proxy_old_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe {
            &*(this as *const dyn ServiceFramework as *const IAllSystemAppletProxiesService)
        };
        log::debug!("IAllSystemAppletProxiesService::OpenLibraryAppletProxyOld");
        let pid = if ctx.get_pid() != 0 {
            ctx.get_pid()
        } else {
            ctx.get_thread()
                .and_then(|thread| {
                    thread
                        .lock()
                        .unwrap()
                        .parent
                        .as_ref()
                        .and_then(|p| p.upgrade())
                })
                .map(|process| process.lock().unwrap().get_process_id())
                .unwrap_or(0)
        };
        let Some(applet) = service.get_applet_from_process_id(pid) else {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(RESULT_UNKNOWN);
            return;
        };
        let proxy = Arc::new(super::library_applet_proxy::ILibraryAppletProxy::new(
            service.system,
            applet,
            service.window_system.clone(),
        ));
        Self::push_interface_response(ctx, proxy);
    }

    fn open_library_applet_proxy_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe {
            &*(this as *const dyn ServiceFramework as *const IAllSystemAppletProxiesService)
        };
        log::debug!("IAllSystemAppletProxiesService::OpenLibraryAppletProxy");
        let pid = if ctx.get_pid() != 0 {
            ctx.get_pid()
        } else {
            ctx.get_thread()
                .and_then(|thread| {
                    thread
                        .lock()
                        .unwrap()
                        .parent
                        .as_ref()
                        .and_then(|p| p.upgrade())
                })
                .map(|process| process.lock().unwrap().get_process_id())
                .unwrap_or(0)
        };
        let Some(applet) = service.get_applet_from_process_id(pid) else {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(RESULT_UNKNOWN);
            return;
        };
        let proxy = Arc::new(super::library_applet_proxy::ILibraryAppletProxy::new(
            service.system,
            applet,
            service.window_system.clone(),
        ));
        Self::push_interface_response(ctx, proxy);
    }
}

impl SessionRequestHandler for IAllSystemAppletProxiesService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
}

impl ServiceFramework for IAllSystemAppletProxiesService {
    fn get_service_name(&self) -> &str {
        "appletAE"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
