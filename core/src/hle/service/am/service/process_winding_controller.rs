// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/process_winding_controller.h
//! Port of zuyu/src/core/hle/service/am/service/process_winding_controller.cpp

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IProcessWindingController:
/// - 0: GetLaunchReason
/// - 11: OpenCallingLibraryApplet
pub struct IProcessWindingController {
    applet: Arc<Mutex<crate::hle::service::am::applet::Applet>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IProcessWindingController {
    pub fn new(applet: Arc<Mutex<crate::hle::service::am::applet::Applet>>) -> Self {
        let handlers = build_handler_map(&[(0, Some(Self::get_launch_reason_handler), "GetLaunchReason")]);
        Self {
            applet,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn get_launch_reason_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IProcessWindingController) };
        let applet = service.applet.lock().unwrap();
        let launch_reason = applet.launch_reason;
        drop(applet);

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        // AppletProcessLaunchReason is a repr(C) u32-sized struct; push as raw u32
        rb.push_u32(unsafe { std::mem::transmute::<_, u32>(launch_reason) });
    }
}

impl SessionRequestHandler for IProcessWindingController {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IProcessWindingController {
    fn get_service_name(&self) -> &str {
        "am::IProcessWindingController"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
