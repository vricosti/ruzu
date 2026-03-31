// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/applet_common_functions.h
//! Port of zuyu/src/core/hle/service/am/service/applet_common_functions.cpp

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IAppletCommonFunctions:
/// - 0: SetTerminateResult (unimplemented)
/// - 10: ReadThemeStorage (unimplemented)
/// - 11: WriteThemeStorage (unimplemented)
/// - 20: PushToAppletBoundChannel (unimplemented)
/// - 21: TryPopFromAppletBoundChannel (unimplemented)
/// - 40: GetDisplayLogicalResolution (unimplemented)
/// - 42: SetDisplayMagnification (unimplemented)
/// - 50: SetHomeButtonDoubleClickEnabled
/// - 51: GetHomeButtonDoubleClickEnabled
/// - 52: IsHomeButtonShortPressedBlocked (unimplemented)
/// - 60: IsVrModeCurtainRequired (unimplemented)
/// - 61: IsSleepRequiredByHighTemperature (unimplemented)
/// - 62: IsSleepRequiredByLowBattery (unimplemented)
/// - 70: SetCpuBoostRequestPriority
/// - 80: SetHandlingCaptureButtonShortPressedMessageEnabledForApplet (unimplemented)
/// - 81: SetHandlingCaptureButtonLongPressedMessageEnabledForApplet (unimplemented)
/// - 90: OpenNamedChannelAsParent (unimplemented)
/// - 91: OpenNamedChannelAsChild (unimplemented)
/// - 100: SetApplicationCoreUsageMode (unimplemented)
/// - 300: GetCurrentApplicationId
pub struct IAppletCommonFunctions {
    applet: Option<std::sync::Arc<std::sync::Mutex<crate::hle::service::am::applet::Applet>>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IAppletCommonFunctions {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, None, "SetTerminateResult"),
            (10, None, "ReadThemeStorage"),
            (11, None, "WriteThemeStorage"),
            (20, None, "PushToAppletBoundChannel"),
            (21, None, "TryPopFromAppletBoundChannel"),
            (40, None, "GetDisplayLogicalResolution"),
            (42, None, "SetDisplayMagnification"),
            (
                50,
                Some(Self::set_home_button_double_click_enabled_handler),
                "SetHomeButtonDoubleClickEnabled",
            ),
            (
                51,
                Some(Self::get_home_button_double_click_enabled_handler),
                "GetHomeButtonDoubleClickEnabled",
            ),
            (52, None, "IsHomeButtonShortPressedBlocked"),
            (60, None, "IsVrModeCurtainRequired"),
            (61, None, "IsSleepRequiredByHighTemperature"),
            (62, None, "IsSleepRequiredByLowBattery"),
            (
                70,
                Some(Self::set_cpu_boost_request_priority_handler),
                "SetCpuBoostRequestPriority",
            ),
            (
                80,
                None,
                "SetHandlingCaptureButtonShortPressedMessageEnabledForApplet",
            ),
            (
                81,
                None,
                "SetHandlingCaptureButtonLongPressedMessageEnabledForApplet",
            ),
            (90, None, "OpenNamedChannelAsParent"),
            (91, None, "OpenNamedChannelAsChild"),
            (100, None, "SetApplicationCoreUsageMode"),
            (
                300,
                Some(Self::get_current_application_id_handler),
                "GetCurrentApplicationId",
            ),
        ]);
        Self {
            applet: None,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn with_applet(
        applet: std::sync::Arc<std::sync::Mutex<crate::hle::service::am::applet::Applet>>,
    ) -> Self {
        let handlers = build_handler_map(&[
            (0, None, "SetTerminateResult"),
            (10, None, "ReadThemeStorage"),
            (11, None, "WriteThemeStorage"),
            (20, None, "PushToAppletBoundChannel"),
            (21, None, "TryPopFromAppletBoundChannel"),
            (40, None, "GetDisplayLogicalResolution"),
            (42, None, "SetDisplayMagnification"),
            (
                50,
                Some(Self::set_home_button_double_click_enabled_handler),
                "SetHomeButtonDoubleClickEnabled",
            ),
            (
                51,
                Some(Self::get_home_button_double_click_enabled_handler),
                "GetHomeButtonDoubleClickEnabled",
            ),
            (52, None, "IsHomeButtonShortPressedBlocked"),
            (60, None, "IsVrModeCurtainRequired"),
            (61, None, "IsSleepRequiredByHighTemperature"),
            (62, None, "IsSleepRequiredByLowBattery"),
            (
                70,
                Some(Self::set_cpu_boost_request_priority_handler),
                "SetCpuBoostRequestPriority",
            ),
            (
                80,
                None,
                "SetHandlingCaptureButtonShortPressedMessageEnabledForApplet",
            ),
            (
                81,
                None,
                "SetHandlingCaptureButtonLongPressedMessageEnabledForApplet",
            ),
            (90, None, "OpenNamedChannelAsParent"),
            (91, None, "OpenNamedChannelAsChild"),
            (100, None, "SetApplicationCoreUsageMode"),
            (
                300,
                Some(Self::get_current_application_id_handler),
                "GetCurrentApplicationId",
            ),
        ]);
        Self {
            applet: Some(applet),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Port of IAppletCommonFunctions::SetHomeButtonDoubleClickEnabled
    pub fn set_home_button_double_click_enabled(&self, _enabled: bool) {
        log::warn!("(STUBBED) SetHomeButtonDoubleClickEnabled called");
    }

    /// Port of IAppletCommonFunctions::GetHomeButtonDoubleClickEnabled
    pub fn get_home_button_double_click_enabled(&self) -> bool {
        log::warn!("(STUBBED) GetHomeButtonDoubleClickEnabled called");
        false
    }

    /// Port of IAppletCommonFunctions::SetCpuBoostRequestPriority
    pub fn set_cpu_boost_request_priority(&self, _priority: i32) {
        log::debug!(
            "SetCpuBoostRequestPriority called with priority={}",
            _priority
        );
        if let Some(ref applet) = self.applet {
            applet.lock().unwrap().cpu_boost_request_priority = _priority;
        }
    }

    /// Port of IAppletCommonFunctions::GetCurrentApplicationId
    pub fn get_current_application_id(&self) -> u64 {
        let program_id = if let Some(ref applet) = self.applet {
            applet.lock().unwrap().program_id
        } else {
            0
        };
        log::debug!("GetCurrentApplicationId: {:016X}", program_id & !0xFFF);
        program_id & !0xFFF
    }

    fn set_home_button_double_click_enabled_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IAppletCommonFunctions) };
        let mut rp = RequestParser::new(ctx);
        let enabled = rp.pop_bool();
        service.set_home_button_double_click_enabled(enabled);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_home_button_double_click_enabled_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IAppletCommonFunctions) };
        let enabled = service.get_home_button_double_click_enabled();

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(enabled);
    }

    fn set_cpu_boost_request_priority_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IAppletCommonFunctions) };
        let mut rp = RequestParser::new(ctx);
        let priority = rp.pop_u32() as i32;
        service.set_cpu_boost_request_priority(priority);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_current_application_id_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IAppletCommonFunctions) };
        let app_id = service.get_current_application_id();

        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(app_id);
    }
}

impl SessionRequestHandler for IAppletCommonFunctions {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }

    fn service_name(&self) -> &str {
        "am::IAppletCommonFunctions"
    }
}

impl ServiceFramework for IAppletCommonFunctions {
    fn get_service_name(&self) -> &str {
        "am::IAppletCommonFunctions"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
