// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/applet_common_functions.h
//! Port of zuyu/src/core/hle/service/am/service/applet_common_functions.cpp

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
}

impl IAppletCommonFunctions {
    pub fn new() -> Self {
        Self { applet: None }
    }

    pub fn with_applet(
        applet: std::sync::Arc<std::sync::Mutex<crate::hle::service::am::applet::Applet>>,
    ) -> Self {
        Self {
            applet: Some(applet),
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
        log::debug!("SetCpuBoostRequestPriority called with priority={}", _priority);
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
}
