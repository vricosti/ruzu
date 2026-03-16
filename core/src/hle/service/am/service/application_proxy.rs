// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/application_proxy.h
//! Port of zuyu/src/core/hle/service/am/service/application_proxy.cpp

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::am::applet::Applet;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IApplicationProxy:
/// - 0: GetCommonStateGetter
/// - 1: GetSelfController
/// - 2: GetWindowController
/// - 3: GetAudioController
/// - 4: GetDisplayController
/// - 10: GetProcessWindingController
/// - 11: GetLibraryAppletCreator
/// - 20: GetApplicationFunctions
/// - 1000: GetDebugFunctions
pub struct IApplicationProxy {
    /// Reference to the applet.
    /// Matches upstream `const std::shared_ptr<Applet> m_applet`.
    applet: Arc<Mutex<Applet>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IApplicationProxy {
    /// Create with an applet reference.
    /// Matches upstream: `IApplicationProxy(Core::System&, std::shared_ptr<Applet>)`
    pub fn new(applet: Arc<Mutex<Applet>>) -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::get_common_state_getter_handler), "GetCommonStateGetter"),
            (1, Some(Self::get_self_controller_handler), "GetSelfController"),
            (2, Some(Self::get_window_controller_handler), "GetWindowController"),
            (3, Some(Self::get_audio_controller_handler), "GetAudioController"),
            (4, Some(Self::get_display_controller_handler), "GetDisplayController"),
            (
                10,
                Some(Self::get_process_winding_controller_handler),
                "GetProcessWindingController",
            ),
            (
                11,
                Some(Self::get_library_applet_creator_handler),
                "GetLibraryAppletCreator",
            ),
            (
                20,
                Some(Self::get_application_functions_handler),
                "GetApplicationFunctions",
            ),
            (1000, Some(Self::get_debug_functions_handler), "GetDebugFunctions"),
        ]);
        Self {
            applet,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
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

    fn get_common_state_getter_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let proxy = unsafe { &*(this as *const dyn ServiceFramework as *const IApplicationProxy) };
        // Create the system event handle and store in the LifecycleManager.
        // Matches upstream: LifecycleManager creates a KEvent via ServiceContext.
        {
            let mut applet = proxy.applet.lock().unwrap();
            if applet.lifecycle_manager.get_system_event_handle() == 0 {
                if let Some(handle) = ctx.create_readable_event_handle(true) {
                    applet.lifecycle_manager.system_event_handle = handle;
                }
            }
        }
        Self::push_interface_response(ctx, Arc::new(
            super::common_state_getter::ICommonStateGetter::new(proxy.applet.clone())
        ));
    }

    fn get_self_controller_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let proxy = unsafe { &*(this as *const dyn ServiceFramework as *const IApplicationProxy) };
        Self::push_interface_response(ctx, Arc::new(
            super::self_controller::ISelfController::new(proxy.applet.clone())
        ));
    }

    fn get_window_controller_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let proxy = unsafe { &*(_this as *const dyn ServiceFramework as *const IApplicationProxy) };
        Self::push_interface_response(
            ctx,
            Arc::new(super::window_controller::IWindowController::new(proxy.applet.clone())),
        );
    }

    fn get_audio_controller_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        Self::push_interface_response(ctx, Arc::new(super::audio_controller::IAudioController::new()));
    }

    fn get_display_controller_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        Self::push_interface_response(ctx, Arc::new(super::display_controller::IDisplayController::new()));
    }

    fn get_process_winding_controller_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        Self::push_interface_response(
            ctx,
            Arc::new(super::process_winding_controller::IProcessWindingController::new()),
        );
    }

    fn get_library_applet_creator_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        Self::push_interface_response(
            ctx,
            Arc::new(super::library_applet_creator::ILibraryAppletCreator::new()),
        );
    }

    fn get_application_functions_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let proxy = unsafe { &*(this as *const dyn ServiceFramework as *const IApplicationProxy) };
        Self::push_interface_response(
            ctx,
            Arc::new(super::application_functions::IApplicationFunctions::new(proxy.applet.clone())),
        );
    }

    fn get_debug_functions_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        Self::push_interface_response(ctx, Arc::new(super::debug_functions::IDebugFunctions::new()));
    }
}

impl SessionRequestHandler for IApplicationProxy {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IApplicationProxy {
    fn get_service_name(&self) -> &str {
        "am::IApplicationProxy"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
