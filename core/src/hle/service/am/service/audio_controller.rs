// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/audio_controller.h
//! Port of zuyu/src/core/hle/service/am/service/audio_controller.cpp

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IAudioController:
/// - 0: SetExpectedMasterVolume
/// - 1: GetMainAppletExpectedMasterVolume
/// - 2: GetLibraryAppletExpectedMasterVolume
/// - 3: ChangeMainAppletMasterVolume
/// - 4: SetTransparentVolumeRate
pub struct IAudioController {
    main_applet_volume: f32,
    library_applet_volume: f32,
    transparent_volume_rate: f32,
    /// Volume transition fade time in nanoseconds.
    fade_time_ns: i64,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

const MIN_ALLOWED_VOLUME: f32 = 0.0;
const MAX_ALLOWED_VOLUME: f32 = 1.0;

impl IAudioController {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (
                0,
                Some(Self::set_expected_master_volume_handler),
                "SetExpectedMasterVolume",
            ),
            (
                1,
                Some(Self::get_main_applet_expected_master_volume_handler),
                "GetMainAppletExpectedMasterVolume",
            ),
            (
                2,
                Some(Self::get_library_applet_expected_master_volume_handler),
                "GetLibraryAppletExpectedMasterVolume",
            ),
            (
                3,
                Some(Self::change_main_applet_master_volume_handler),
                "ChangeMainAppletMasterVolume",
            ),
            (
                4,
                Some(Self::set_transparent_volume_rate_handler),
                "SetTransparentVolumeRate",
            ),
        ]);
        Self {
            main_applet_volume: 0.25,
            library_applet_volume: MAX_ALLOWED_VOLUME,
            transparent_volume_rate: MIN_ALLOWED_VOLUME,
            fade_time_ns: 0,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Port of IAudioController::SetExpectedMasterVolume
    pub fn set_expected_master_volume(
        &mut self,
        main_applet_volume: f32,
        library_applet_volume: f32,
    ) {
        log::debug!(
            "SetExpectedMasterVolume called. main={}, library={}",
            main_applet_volume,
            library_applet_volume
        );
        self.main_applet_volume = main_applet_volume.clamp(MIN_ALLOWED_VOLUME, MAX_ALLOWED_VOLUME);
        self.library_applet_volume =
            library_applet_volume.clamp(MIN_ALLOWED_VOLUME, MAX_ALLOWED_VOLUME);
    }

    /// Port of IAudioController::GetMainAppletExpectedMasterVolume
    pub fn get_main_applet_expected_master_volume(&self) -> f32 {
        log::debug!(
            "GetMainAppletExpectedMasterVolume called. volume={}",
            self.main_applet_volume
        );
        self.main_applet_volume
    }

    /// Port of IAudioController::GetLibraryAppletExpectedMasterVolume
    pub fn get_library_applet_expected_master_volume(&self) -> f32 {
        log::debug!(
            "GetLibraryAppletExpectedMasterVolume called. volume={}",
            self.library_applet_volume
        );
        self.library_applet_volume
    }

    /// Port of IAudioController::ChangeMainAppletMasterVolume
    pub fn change_main_applet_master_volume(&mut self, volume: f32, fade_time_ns: i64) {
        log::debug!(
            "ChangeMainAppletMasterVolume called. volume={}, fade_time_ns={}",
            volume,
            fade_time_ns
        );
        self.main_applet_volume = volume.clamp(MIN_ALLOWED_VOLUME, MAX_ALLOWED_VOLUME);
        self.fade_time_ns = fade_time_ns;
    }

    /// Port of IAudioController::SetTransparentVolumeRate
    pub fn set_transparent_volume_rate(&mut self, transparent_volume_rate: f32) {
        log::debug!(
            "SetTransparentVolumeRate called. rate={}",
            transparent_volume_rate
        );
        self.transparent_volume_rate =
            transparent_volume_rate.clamp(MIN_ALLOWED_VOLUME, MAX_ALLOWED_VOLUME);
    }

    fn set_expected_master_volume_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let _service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IAudioController) };
        let mut rp = RequestParser::new(ctx);
        let _ = (rp.pop_f32(), rp.pop_f32());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_main_applet_expected_master_volume_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IAudioController) };
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(service.get_main_applet_expected_master_volume().to_bits());
    }

    fn get_library_applet_expected_master_volume_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IAudioController) };
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(
            service
                .get_library_applet_expected_master_volume()
                .to_bits(),
        );
    }

    fn change_main_applet_master_volume_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let _service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IAudioController) };
        let mut rp = RequestParser::new(ctx);
        let _ = (rp.pop_f32(), rp.pop_i64());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_transparent_volume_rate_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let _service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IAudioController) };
        let mut rp = RequestParser::new(ctx);
        let _ = rp.pop_f32();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for IAudioController {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IAudioController {
    fn get_service_name(&self) -> &str {
        "am::IAudioController"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
