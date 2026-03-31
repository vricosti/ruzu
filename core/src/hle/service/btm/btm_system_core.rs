// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/btm/btm_system_core.h
//! Port of zuyu/src/core/hle/service/btm/btm_system_core.cpp
//!
//! IBtmSystemCore — Bluetooth system core interface.

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::kernel_helpers::ServiceContext;
use crate::hle::service::os::event::Event;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use std::collections::BTreeMap;
use std::sync::Arc;

/// IBtmSystemCore.
///
/// Upstream fields:
/// - `service_context`: provides kernel event creation/destruction
/// - `radio_event`: KEvent signaled on radio state changes
/// - `audio_device_connection_event`: KEvent signaled on audio device connection changes
/// - `m_set_sys`: shared pointer to ISystemSettingsServer (not yet wired)
pub struct IBtmSystemCore {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    service_context: ServiceContext,
    radio_event: Arc<Event>,
    audio_device_connection_event: Arc<Event>,
}

impl IBtmSystemCore {
    pub fn new() -> Self {
        let mut service_context = ServiceContext::new("IBtmSystemCore".to_string());

        let radio_handle = service_context.create_event("IBtmSystemCore::RadioEvent".to_string());
        let radio_event = service_context.get_event(radio_handle).unwrap();

        let audio_handle =
            service_context.create_event("IBtmSystemCore::AudioDeviceConnectionEvent".to_string());
        let audio_device_connection_event = service_context.get_event(audio_handle).unwrap();

        let handlers = build_handler_map(&[
            (0, None, "StartGamepadPairing"),
            (1, None, "CancelGamepadPairing"),
            (2, None, "ClearGamepadPairingDatabase"),
            (3, None, "GetPairedGamepadCount"),
            (4, None, "EnableRadio"),
            (5, None, "DisableRadio"),
            (6, None, "IsRadioEnabled"),
            (7, None, "AcquireRadioEvent"),
            (8, None, "AcquireGamepadPairingEvent"),
            (9, None, "IsGamepadPairingStarted"),
            (10, None, "StartAudioDeviceDiscovery"),
            (11, None, "StopAudioDeviceDiscovery"),
            (12, None, "IsDiscoveryingAudioDevice"),
            (13, None, "GetDiscoveredAudioDevice"),
            (14, None, "AcquireAudioDeviceConnectionEvent"),
            (15, None, "ConnectAudioDevice"),
            (16, None, "IsConnectingAudioDevice"),
            (17, None, "GetConnectedAudioDevices"),
            (18, None, "DisconnectAudioDevice"),
            (19, None, "AcquirePairedAudioDeviceInfoChangedEvent"),
            (20, None, "GetPairedAudioDevices"),
            (21, None, "RemoveAudioDevicePairing"),
            (22, None, "RequestAudioDeviceConnectionRejection"),
            (23, None, "CancelAudioDeviceConnectionRejection"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
            service_context,
            radio_event,
            audio_device_connection_event,
        }
    }

    /// StartGamepadPairing (cmd 0).
    pub fn start_gamepad_pairing(&self) -> ResultCode {
        log::warn!("IBtmSystemCore::start_gamepad_pairing (STUBBED) called");
        ResultCode::new(0)
    }

    /// CancelGamepadPairing (cmd 1).
    pub fn cancel_gamepad_pairing(&self) -> ResultCode {
        log::warn!("IBtmSystemCore::cancel_gamepad_pairing (STUBBED) called");
        ResultCode::new(0)
    }

    /// IsRadioEnabled (cmd 6).
    pub fn is_radio_enabled(&self) -> (ResultCode, bool) {
        log::debug!("IBtmSystemCore::is_radio_enabled (STUBBED) called");
        (ResultCode::new(0), true)
    }
}

impl SessionRequestHandler for IBtmSystemCore {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        "IBtmSystemCore"
    }
}

impl ServiceFramework for IBtmSystemCore {
    fn get_service_name(&self) -> &str {
        "IBtmSystemCore"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
