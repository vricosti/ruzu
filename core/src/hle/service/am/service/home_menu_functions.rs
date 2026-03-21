// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/home_menu_functions.h
//! Port of zuyu/src/core/hle/service/am/service/home_menu_functions.cpp

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IHomeMenuFunctions:
/// - 10: RequestToGetForeground
/// - 11: LockForeground
/// - 12: UnlockForeground
/// - 20: PopFromGeneralChannel (unimplemented)
/// - 21: GetPopFromGeneralChannelEvent
/// - 30: GetHomeButtonWriterLockAccessor (unimplemented)
/// - 31: GetWriterLockAccessorEx (unimplemented)
/// - 40: IsSleepEnabled (unimplemented)
/// - 41: IsRebootEnabled
/// - 50: LaunchSystemApplet (unimplemented)
/// - 51: LaunchStarter (unimplemented)
/// - 100: PopRequestLaunchApplicationForDebug (unimplemented)
/// - 110: IsForceTerminateApplicationDisabledForDebug
/// - 200: LaunchDevMenu (unimplemented)
/// - 1000: SetLastApplicationExitReason (unimplemented)
pub struct IHomeMenuFunctions {
    applet: Arc<Mutex<crate::hle::service::am::applet::Applet>>,
    window_system: Arc<Mutex<crate::hle::service::am::window_system::WindowSystem>>,
    // TODO: ServiceContext, Event
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IHomeMenuFunctions {
    pub fn new(
        applet: Arc<Mutex<crate::hle::service::am::applet::Applet>>,
        window_system: Arc<Mutex<crate::hle::service::am::window_system::WindowSystem>>,
    ) -> Self {
        let handlers = build_handler_map(&[
            (10, Some(Self::request_to_get_foreground_handler), "RequestToGetForeground"),
            (11, Some(Self::lock_foreground_handler), "LockForeground"),
            (12, Some(Self::unlock_foreground_handler), "UnlockForeground"),
            (20, None, "PopFromGeneralChannel"),
            (21, None, "GetPopFromGeneralChannelEvent"), // Needs event handle support
            (30, None, "GetHomeButtonWriterLockAccessor"),
            (31, None, "GetWriterLockAccessorEx"),
            (40, None, "IsSleepEnabled"),
            (41, Some(Self::is_reboot_enabled_handler), "IsRebootEnabled"),
            (50, None, "LaunchSystemApplet"),
            (51, None, "LaunchStarter"),
            (100, None, "PopRequestLaunchApplicationForDebug"),
            (
                110,
                Some(Self::is_force_terminate_application_disabled_for_debug_handler),
                "IsForceTerminateApplicationDisabledForDebug",
            ),
            (200, None, "LaunchDevMenu"),
            (1000, None, "SetLastApplicationExitReason"),
        ]);
        Self {
            applet,
            window_system,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Port of IHomeMenuFunctions::RequestToGetForeground
    /// Upstream calls m_window_system.RequestHomeMenuToGetForeground() then R_SUCCEED.
    fn request_to_get_foreground_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IHomeMenuFunctions) };
        log::info!("IHomeMenuFunctions::RequestToGetForeground called");
        service.window_system.lock().unwrap().request_home_menu_to_get_foreground();

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Port of IHomeMenuFunctions::LockForeground
    /// Upstream calls m_window_system.RequestLockHomeMenuIntoForeground() then R_SUCCEED.
    fn lock_foreground_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IHomeMenuFunctions) };
        log::info!("IHomeMenuFunctions::LockForeground called");
        service.window_system.lock().unwrap().request_lock_home_menu_into_foreground();

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Port of IHomeMenuFunctions::UnlockForeground
    /// Upstream calls m_window_system.RequestUnlockHomeMenuIntoForeground() then R_SUCCEED.
    fn unlock_foreground_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IHomeMenuFunctions) };
        log::info!("IHomeMenuFunctions::UnlockForeground called");
        service.window_system.lock().unwrap().request_unlock_home_menu_into_foreground();

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Port of IHomeMenuFunctions::IsRebootEnabled
    /// Upstream returns true.
    fn is_reboot_enabled_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::info!("IHomeMenuFunctions::IsRebootEnabled called");

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(true);
    }

    /// Port of IHomeMenuFunctions::IsForceTerminateApplicationDisabledForDebug
    /// Upstream returns false.
    fn is_force_terminate_application_disabled_for_debug_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::info!("IHomeMenuFunctions::IsForceTerminateApplicationDisabledForDebug called");

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(false);
    }
}

impl SessionRequestHandler for IHomeMenuFunctions {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
}

impl ServiceFramework for IHomeMenuFunctions {
    fn get_service_name(&self) -> &str {
        "am::IHomeMenuFunctions"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
