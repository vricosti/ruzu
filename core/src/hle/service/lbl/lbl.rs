// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/lbl/lbl.cpp
//!
//! LBL (backlight) service ("lbl"). Most commands are implemented with state tracking.

use std::collections::BTreeMap;
use std::sync::Mutex;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// BacklightSwitchStatus enum. Upstream: `BacklightSwitchStatus` in `lbl.cpp`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum BacklightSwitchStatus {
    Off = 0,
    On = 1,
}

/// Interior mutable state for LBL service.
struct LblState {
    vr_mode_enabled: bool,
    current_brightness: f32,
    ambient_light_value: f32,
    current_vr_brightness: f32,
    dimming: bool,
    backlight_enabled: bool,
    update_instantly: bool,
    auto_brightness: bool,
    auto_brightness_supported: bool,
}

/// LBL service ("lbl").
///
/// Corresponds to `LBL` class in upstream `lbl.cpp`.
pub struct LBL {
    state: Mutex<LblState>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl LBL {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::save_current_setting_handler), "SaveCurrentSetting"),
            (1, Some(Self::load_current_setting_handler), "LoadCurrentSetting"),
            (2, Some(Self::set_current_brightness_setting_handler), "SetCurrentBrightnessSetting"),
            (3, Some(Self::get_current_brightness_setting_handler), "GetCurrentBrightnessSetting"),
            (4, None, "ApplyCurrentBrightnessSettingToBacklight"),
            (5, None, "GetBrightnessSettingAppliedToBacklight"),
            (6, Some(Self::switch_backlight_on_handler), "SwitchBacklightOn"),
            (7, Some(Self::switch_backlight_off_handler), "SwitchBacklightOff"),
            (8, Some(Self::get_backlight_switch_status_handler), "GetBacklightSwitchStatus"),
            (9, Some(Self::enable_dimming_handler), "EnableDimming"),
            (10, Some(Self::disable_dimming_handler), "DisableDimming"),
            (11, Some(Self::is_dimming_enabled_handler), "IsDimmingEnabled"),
            (12, Some(Self::enable_auto_brightness_control_handler), "EnableAutoBrightnessControl"),
            (13, Some(Self::disable_auto_brightness_control_handler), "DisableAutoBrightnessControl"),
            (14, Some(Self::is_auto_brightness_control_enabled_handler), "IsAutoBrightnessControlEnabled"),
            (15, Some(Self::set_ambient_light_sensor_value_handler), "SetAmbientLightSensorValue"),
            (16, Some(Self::get_ambient_light_sensor_value_handler), "GetAmbientLightSensorValue"),
            (17, Some(Self::set_brightness_reflection_delay_level_handler), "SetBrightnessReflectionDelayLevel"),
            (18, Some(Self::get_brightness_reflection_delay_level_handler), "GetBrightnessReflectionDelayLevel"),
            (19, Some(Self::set_current_brightness_mapping_handler), "SetCurrentBrightnessMapping"),
            (20, Some(Self::get_current_brightness_mapping_handler), "GetCurrentBrightnessMapping"),
            (21, Some(Self::set_current_ambient_light_sensor_mapping_handler), "SetCurrentAmbientLightSensorMapping"),
            (22, Some(Self::get_current_ambient_light_sensor_mapping_handler), "GetCurrentAmbientLightSensorMapping"),
            (23, Some(Self::is_ambient_light_sensor_available_handler), "IsAmbientLightSensorAvailable"),
            (24, Some(Self::set_current_brightness_setting_for_vr_mode_handler), "SetCurrentBrightnessSettingForVrMode"),
            (25, Some(Self::get_current_brightness_setting_for_vr_mode_handler), "GetCurrentBrightnessSettingForVrMode"),
            (26, Some(Self::enable_vr_mode_handler), "EnableVrMode"),
            (27, Some(Self::disable_vr_mode_handler), "DisableVrMode"),
            (28, Some(Self::is_vr_mode_enabled_handler), "IsVrModeEnabled"),
            (29, Some(Self::is_auto_brightness_control_supported_handler), "IsAutoBrightnessControlSupported"),
        ]);

        Self {
            state: Mutex::new(LblState {
                vr_mode_enabled: false,
                current_brightness: 1.0,
                ambient_light_value: 0.0,
                current_vr_brightness: 1.0,
                dimming: true,
                backlight_enabled: true,
                update_instantly: false,
                auto_brightness: false,
                auto_brightness_supported: true,
            }),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn save_current_setting(&self) {
        log::warn!("(STUBBED) LBL::save_current_setting called");
    }

    pub fn load_current_setting(&self) {
        log::warn!("(STUBBED) LBL::load_current_setting called");
    }

    pub fn set_current_brightness_setting(&self, mut brightness: f32) {
        if !brightness.is_finite() {
            log::error!("LBL: Brightness is infinite!");
            brightness = 0.0;
        }
        log::debug!("LBL::set_current_brightness_setting called, brightness={}", brightness);
        let mut s = self.state.lock().unwrap();
        s.current_brightness = brightness;
        s.update_instantly = true;
    }

    pub fn get_current_brightness_setting(&self) -> f32 {
        let mut brightness = self.state.lock().unwrap().current_brightness;
        if !brightness.is_finite() {
            log::error!("LBL: Brightness is infinite!");
            brightness = 0.0;
        }
        log::debug!("LBL::get_current_brightness_setting called, brightness={}", brightness);
        brightness
    }

    pub fn switch_backlight_on(&self, fade_time: u64) {
        log::warn!("(STUBBED) LBL::switch_backlight_on called, fade_time={}", fade_time);
        self.state.lock().unwrap().backlight_enabled = true;
    }

    pub fn switch_backlight_off(&self, fade_time: u64) {
        log::warn!("(STUBBED) LBL::switch_backlight_off called, fade_time={}", fade_time);
        self.state.lock().unwrap().backlight_enabled = false;
    }

    pub fn get_backlight_switch_status(&self) -> BacklightSwitchStatus {
        if self.state.lock().unwrap().backlight_enabled {
            BacklightSwitchStatus::On
        } else {
            BacklightSwitchStatus::Off
        }
    }

    pub fn enable_dimming(&self) {
        log::debug!("LBL::enable_dimming called");
        self.state.lock().unwrap().dimming = true;
    }

    pub fn disable_dimming(&self) {
        log::debug!("LBL::disable_dimming called");
        self.state.lock().unwrap().dimming = false;
    }

    pub fn is_dimming_enabled(&self) -> bool {
        self.state.lock().unwrap().dimming
    }

    pub fn enable_auto_brightness_control(&self) {
        log::debug!("LBL::enable_auto_brightness_control called");
        let mut s = self.state.lock().unwrap();
        s.auto_brightness = true;
        s.update_instantly = true;
    }

    pub fn disable_auto_brightness_control(&self) {
        log::debug!("LBL::disable_auto_brightness_control called");
        self.state.lock().unwrap().auto_brightness = false;
    }

    pub fn is_auto_brightness_control_enabled(&self) -> bool {
        self.state.lock().unwrap().auto_brightness
    }

    pub fn set_ambient_light_sensor_value(&self, light_value: f32) {
        log::debug!("LBL::set_ambient_light_sensor_value called, light_value={}", light_value);
        self.state.lock().unwrap().ambient_light_value = light_value;
    }

    pub fn get_ambient_light_sensor_value(&self) -> f32 {
        self.state.lock().unwrap().ambient_light_value
    }

    pub fn set_brightness_reflection_delay_level(&self) {
        // Intentional no-op, matches upstream
        log::debug!("LBL::set_brightness_reflection_delay_level called");
    }

    pub fn get_brightness_reflection_delay_level(&self) -> f32 {
        // Intentional: hard coded to return 0.0f on hardware
        0.0
    }

    pub fn set_current_brightness_mapping(&self) {
        // Intentional no-op
        log::debug!("LBL::set_current_brightness_mapping called");
    }

    pub fn get_current_brightness_mapping(&self) {
        // Intentional no-op
        log::debug!("LBL::get_current_brightness_mapping called");
    }

    pub fn set_current_ambient_light_sensor_mapping(&self) {
        // Intentional no-op
        log::debug!("LBL::set_current_ambient_light_sensor_mapping called");
    }

    pub fn get_current_ambient_light_sensor_mapping(&self) {
        // Intentional no-op
        log::debug!("LBL::get_current_ambient_light_sensor_mapping called");
    }

    pub fn is_ambient_light_sensor_available(&self) -> bool {
        log::warn!("(STUBBED) LBL::is_ambient_light_sensor_available called");
        true
    }

    pub fn set_current_brightness_setting_for_vr_mode(&self, mut brightness: f32) {
        if !brightness.is_finite() {
            log::error!("LBL: Brightness is infinite!");
            brightness = 0.0;
        }
        log::debug!("LBL::set_current_brightness_setting_for_vr_mode called, brightness={}", brightness);
        self.state.lock().unwrap().current_vr_brightness = brightness;
    }

    pub fn get_current_brightness_setting_for_vr_mode(&self) -> f32 {
        let mut brightness = self.state.lock().unwrap().current_vr_brightness;
        if !brightness.is_finite() {
            log::error!("LBL: Brightness is infinite!");
            brightness = 0.0;
        }
        brightness
    }

    pub fn enable_vr_mode(&self) {
        log::debug!("LBL::enable_vr_mode called");
        self.state.lock().unwrap().vr_mode_enabled = true;
    }

    pub fn disable_vr_mode(&self) {
        log::debug!("LBL::disable_vr_mode called");
        self.state.lock().unwrap().vr_mode_enabled = false;
    }

    pub fn is_vr_mode_enabled(&self) -> bool {
        self.state.lock().unwrap().vr_mode_enabled
    }

    pub fn is_auto_brightness_control_supported(&self) -> bool {
        self.state.lock().unwrap().auto_brightness_supported
    }

    // --- Handler bridge functions ---

    fn save_current_setting_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        service.save_current_setting();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn load_current_setting_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        service.load_current_setting();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_current_brightness_setting_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        let mut rp = RequestParser::new(ctx);
        let brightness = rp.pop_f32();
        service.set_current_brightness_setting(brightness);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_current_brightness_setting_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        let brightness = service.get_current_brightness_setting();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_f32(brightness);
    }

    fn switch_backlight_on_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        let mut rp = RequestParser::new(ctx);
        let fade_time = rp.pop_u64();
        service.switch_backlight_on(fade_time);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn switch_backlight_off_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        let mut rp = RequestParser::new(ctx);
        let fade_time = rp.pop_u64();
        service.switch_backlight_off(fade_time);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_backlight_switch_status_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        let status = service.get_backlight_switch_status();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(status as u32);
    }

    fn enable_dimming_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        service.enable_dimming();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn disable_dimming_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        service.disable_dimming();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn is_dimming_enabled_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        let enabled = service.is_dimming_enabled();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(enabled);
    }

    fn enable_auto_brightness_control_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        service.enable_auto_brightness_control();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn disable_auto_brightness_control_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        service.disable_auto_brightness_control();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn is_auto_brightness_control_enabled_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        let enabled = service.is_auto_brightness_control_enabled();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(enabled);
    }

    fn set_ambient_light_sensor_value_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        let mut rp = RequestParser::new(ctx);
        let value = rp.pop_f32();
        service.set_ambient_light_sensor_value(value);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_ambient_light_sensor_value_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        let value = service.get_ambient_light_sensor_value();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_f32(value);
    }

    fn set_brightness_reflection_delay_level_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        service.set_brightness_reflection_delay_level();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_brightness_reflection_delay_level_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        let level = service.get_brightness_reflection_delay_level();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_f32(level);
    }

    fn set_current_brightness_mapping_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        service.set_current_brightness_mapping();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_current_brightness_mapping_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        service.get_current_brightness_mapping();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_current_ambient_light_sensor_mapping_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        service.set_current_ambient_light_sensor_mapping();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_current_ambient_light_sensor_mapping_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        service.get_current_ambient_light_sensor_mapping();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn is_ambient_light_sensor_available_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        let available = service.is_ambient_light_sensor_available();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(available);
    }

    fn set_current_brightness_setting_for_vr_mode_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        let mut rp = RequestParser::new(ctx);
        let brightness = rp.pop_f32();
        service.set_current_brightness_setting_for_vr_mode(brightness);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_current_brightness_setting_for_vr_mode_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        let brightness = service.get_current_brightness_setting_for_vr_mode();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_f32(brightness);
    }

    fn enable_vr_mode_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        service.enable_vr_mode();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn disable_vr_mode_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        service.disable_vr_mode();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn is_vr_mode_enabled_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        let enabled = service.is_vr_mode_enabled();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(enabled);
    }

    fn is_auto_brightness_control_supported_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const LBL) };
        let supported = service.is_auto_brightness_control_supported();
        // Upstream uses rb.Push<u8>(auto_brightness_supported)
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(supported);
    }
}

impl SessionRequestHandler for LBL {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str { "lbl" }
}

impl ServiceFramework for LBL {
    fn get_service_name(&self) -> &str { "lbl" }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}

/// Registers "lbl" service.
///
/// Corresponds to `LoopProcess` in upstream `lbl.cpp`.
pub fn loop_process(system: crate::core::SystemRef) {
    use std::sync::Arc;
    use crate::hle::service::server_manager::ServerManager;
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;

    let mut server_manager = ServerManager::new(system);

    server_manager.register_named_service(
        "lbl",
        Box::new(|| -> SessionRequestHandlerPtr {
            Arc::new(LBL::new())
        }),
        64,
    );

    ServerManager::run_server(server_manager);
}
