// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/btm/btm_system_core.h
//! Port of zuyu/src/core/hle/service/btm/btm_system_core.cpp
//!
//! IBtmSystemCore — Bluetooth system core interface.

use crate::core::SystemRef;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::kernel_helpers::ServiceContext;
use crate::hle::service::os::event::Event;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use crate::hle::service::set::system_settings_server::SystemSettingsService;
use crate::hle::service::sm::sm::ServiceManager;
use std::collections::BTreeMap;
use std::sync::Arc;

/// IBtmSystemCore.
///
/// Upstream fields:
/// - `service_context`: provides kernel event creation/destruction
/// - `radio_event`: KEvent signaled on radio state changes
/// - `audio_device_connection_event`: KEvent signaled on audio device connection changes
/// - `m_set_sys`: shared pointer to ISystemSettingsServer
pub struct IBtmSystemCore {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    service_context: ServiceContext,
    radio_event_handle: u32,
    audio_device_connection_event_handle: u32,
    radio_event: Arc<Event>,
    audio_device_connection_event: Arc<Event>,
    m_set_sys: Arc<SystemSettingsService>,
}

impl IBtmSystemCore {
    fn get_set_sys_service_from_handler(
        set_sys_handler: SessionRequestHandlerPtr,
    ) -> Arc<SystemSettingsService> {
        assert!(
            set_sys_handler.as_any().is::<SystemSettingsService>(),
            "set:sys is not an ISystemSettingsServer"
        );
        unsafe { Arc::from_raw(Arc::into_raw(set_sys_handler) as *const SystemSettingsService) }
    }

    fn get_set_sys_service(system: SystemRef) -> Arc<SystemSettingsService> {
        if system.is_null() {
            // Isolated harness adaptation: Eden always has a live service manager.
            return Arc::new(SystemSettingsService::new());
        }
        let Some(service_manager) = system.get().service_manager() else {
            // Isolated harness adaptation: Eden always has a live service manager.
            return Arc::new(SystemSettingsService::new());
        };
        let set_sys_handler =
            ServiceManager::get_service_blocking(&service_manager, system, "set:sys");
        Self::get_set_sys_service_from_handler(set_sys_handler)
    }

    pub fn new(system: SystemRef) -> Self {
        Self::new_with_set_sys_provider(|| Self::get_set_sys_service(system))
    }

    fn new_with_set_sys_provider(get_set_sys: impl FnOnce() -> Arc<SystemSettingsService>) -> Self {
        let handlers = build_handler_map(&[
            (
                0,
                Some(Self::start_gamepad_pairing_handler),
                "StartGamepadPairing",
            ),
            (
                1,
                Some(Self::cancel_gamepad_pairing_handler),
                "CancelGamepadPairing",
            ),
            (2, None, "ClearGamepadPairingDatabase"),
            (3, None, "GetPairedGamepadCount"),
            (4, Some(Self::enable_radio_handler), "EnableRadio"),
            (5, Some(Self::disable_radio_handler), "DisableRadio"),
            (6, Some(Self::is_radio_enabled_handler), "IsRadioEnabled"),
            (
                7,
                Some(Self::acquire_radio_event_handler),
                "AcquireRadioEvent",
            ),
            (8, None, "AcquireGamepadPairingEvent"),
            (9, None, "IsGamepadPairingStarted"),
            (10, None, "StartAudioDeviceDiscovery"),
            (11, None, "StopAudioDeviceDiscovery"),
            (12, None, "IsDiscoveryingAudioDevice"),
            (
                13,
                Some(Self::get_discovered_audio_device_handler),
                "GetDiscoveredAudioDevice",
            ),
            (
                14,
                Some(Self::acquire_audio_device_connection_event_handler),
                "AcquireAudioDeviceConnectionEvent",
            ),
            (15, None, "ConnectAudioDevice"),
            (16, None, "IsConnectingAudioDevice"),
            (
                17,
                Some(Self::get_connected_audio_devices_handler),
                "GetConnectedAudioDevices",
            ),
            (18, None, "DisconnectAudioDevice"),
            (19, None, "AcquirePairedAudioDeviceInfoChangedEvent"),
            (
                20,
                Some(Self::get_paired_audio_devices_handler),
                "GetPairedAudioDevices",
            ),
            (21, None, "RemoveAudioDevicePairing"),
            (
                22,
                Some(Self::request_audio_device_connection_rejection_handler),
                "RequestAudioDeviceConnectionRejection",
            ),
            (
                23,
                Some(Self::cancel_audio_device_connection_rejection_handler),
                "CancelAudioDeviceConnectionRejection",
            ),
        ]);

        let mut service_context = ServiceContext::new("IBtmSystemCore".to_string());

        let radio_handle = service_context.create_event("IBtmSystemCore::RadioEvent".to_string());
        let radio_event = service_context.get_event(radio_handle).unwrap();

        let audio_handle =
            service_context.create_event("IBtmSystemCore::AudioDeviceConnectionEvent".to_string());
        let audio_device_connection_event = service_context.get_event(audio_handle).unwrap();

        let m_set_sys = get_set_sys();

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
            service_context,
            radio_event_handle: radio_handle,
            audio_device_connection_event_handle: audio_handle,
            radio_event,
            audio_device_connection_event,
            m_set_sys,
        }
    }

    /// StartGamepadPairing (cmd 0).
    pub fn start_gamepad_pairing(&self) -> ResultCode {
        log::warn!("IBtmSystemCore::start_gamepad_pairing (STUBBED) called");
        RESULT_SUCCESS
    }

    /// CancelGamepadPairing (cmd 1).
    pub fn cancel_gamepad_pairing(&self) -> ResultCode {
        log::warn!("IBtmSystemCore::cancel_gamepad_pairing (STUBBED) called");
        RESULT_SUCCESS
    }

    pub fn enable_radio(&self) -> ResultCode {
        log::debug!("IBtmSystemCore::enable_radio called");
        self.m_set_sys
            .inner
            .lock()
            .unwrap()
            .set_bluetooth_enable_flag(true);
        RESULT_SUCCESS
    }

    pub fn disable_radio(&self) -> ResultCode {
        log::debug!("IBtmSystemCore::disable_radio called");
        self.m_set_sys
            .inner
            .lock()
            .unwrap()
            .set_bluetooth_enable_flag(false);
        RESULT_SUCCESS
    }

    /// IsRadioEnabled (cmd 6).
    pub fn is_radio_enabled(&self) -> (ResultCode, bool) {
        log::debug!("IBtmSystemCore::is_radio_enabled called");
        let enabled = self
            .m_set_sys
            .inner
            .lock()
            .unwrap()
            .get_bluetooth_enable_flag();
        (RESULT_SUCCESS, enabled)
    }

    fn get_discovered_audio_device(&self, _count: i32) -> i32 {
        log::warn!("IBtmSystemCore::get_discovered_audio_device (STUBBED) called");
        0
    }

    fn get_connected_audio_devices(&self) -> i32 {
        log::warn!("IBtmSystemCore::get_connected_audio_devices (STUBBED) called");
        0
    }

    fn get_paired_audio_devices(&self) -> i32 {
        log::warn!("IBtmSystemCore::get_paired_audio_devices (STUBBED) called");
        0
    }

    fn request_audio_device_connection_rejection(&self, aruid: u64) -> ResultCode {
        log::warn!(
            "IBtmSystemCore::request_audio_device_connection_rejection (STUBBED) called, applet_resource_user_id={aruid}"
        );
        RESULT_SUCCESS
    }

    fn cancel_audio_device_connection_rejection(&self, aruid: u64) -> ResultCode {
        log::warn!(
            "IBtmSystemCore::cancel_audio_device_connection_rejection (STUBBED) called, applet_resource_user_id={aruid}"
        );
        RESULT_SUCCESS
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    fn push_result(ctx: &mut HLERequestContext, result: ResultCode) {
        let mut response = ResponseBuilder::new(ctx, 2, 0, 0);
        response.push_result(result);
    }

    fn start_gamepad_pairing_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        Self::push_result(ctx, Self::as_self(this).start_gamepad_pairing());
    }

    fn cancel_gamepad_pairing_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        Self::push_result(ctx, Self::as_self(this).cancel_gamepad_pairing());
    }

    fn enable_radio_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        Self::push_result(ctx, Self::as_self(this).enable_radio());
    }

    fn disable_radio_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        Self::push_result(ctx, Self::as_self(this).disable_radio());
    }

    fn is_radio_enabled_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let (result, enabled) = Self::as_self(this).is_radio_enabled();
        let mut response = ResponseBuilder::new(ctx, 3, 0, 0);
        response.push_result(result);
        response.push_bool(enabled);
    }

    fn acquire_radio_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("IBtmSystemCore::acquire_radio_event (STUBBED) called");
        let object_id = Self::as_self(this)
            .radio_event
            .copy_object_id(ctx)
            .unwrap_or(0);
        let mut response = ResponseBuilder::new(ctx, 3, 1, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_bool(true);
        response.push_copy_object_id(object_id);
    }

    fn get_discovered_audio_device_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let mut request = RequestParser::new(ctx);
        let count = request.pop_i32();
        let total = Self::as_self(this).get_discovered_audio_device(count);
        let mut response = ResponseBuilder::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_i32(total);
    }

    fn acquire_audio_device_connection_event_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("IBtmSystemCore::acquire_audio_device_connection_event (STUBBED) called");
        let object_id = Self::as_self(this)
            .audio_device_connection_event
            .copy_object_id(ctx)
            .unwrap_or(0);
        let mut response = ResponseBuilder::new(ctx, 2, 1, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_copy_object_id(object_id);
    }

    fn get_connected_audio_devices_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let count = Self::as_self(this).get_connected_audio_devices();
        let mut response = ResponseBuilder::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_i32(count);
    }

    fn get_paired_audio_devices_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let count = Self::as_self(this).get_paired_audio_devices();
        let mut response = ResponseBuilder::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_i32(count);
    }

    fn request_audio_device_connection_rejection_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let aruid = ctx.get_pid();
        Self::push_result(
            ctx,
            Self::as_self(this).request_audio_device_connection_rejection(aruid),
        );
    }

    fn cancel_audio_device_connection_rejection_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let aruid = ctx.get_pid();
        Self::push_result(
            ctx,
            Self::as_self(this).cancel_audio_device_connection_rejection(aruid),
        );
    }
}

impl Drop for IBtmSystemCore {
    fn drop(&mut self) {
        self.service_context.close_event(self.radio_event_handle);
        self.service_context
            .close_event(self.audio_device_connection_event_handle);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn implemented_commands_share_settings_and_release_both_events() {
        std::thread::Builder::new()
            .name("IBtmSystemCore parity test".to_string())
            .stack_size(32 * 1024 * 1024)
            .spawn(|| {
                let set_sys = Arc::new(SystemSettingsService::new_for_test());
                let handler: SessionRequestHandlerPtr = set_sys.clone();
                let resolved = IBtmSystemCore::get_set_sys_service_from_handler(handler);
                assert!(Arc::ptr_eq(&set_sys, &resolved));

                let service = IBtmSystemCore::new_with_set_sys_provider(|| resolved);
                for command_id in [0, 1, 4, 5, 6, 7, 13, 14, 17, 20, 22, 23] {
                    assert!(
                        service.handlers[&command_id].handler_callback.is_some(),
                        "command {command_id} must have Eden's implemented handler"
                    );
                }
                for command_id in [2, 3, 8, 9, 10, 11, 12, 15, 16, 18, 19, 21] {
                    assert!(
                        service.handlers[&command_id].handler_callback.is_none(),
                        "command {command_id} must remain unimplemented like Eden"
                    );
                }

                assert_eq!(service.disable_radio(), RESULT_SUCCESS);
                assert_eq!(service.is_radio_enabled(), (RESULT_SUCCESS, false));
                assert_eq!(service.enable_radio(), RESULT_SUCCESS);
                assert_eq!(service.is_radio_enabled(), (RESULT_SUCCESS, true));
                assert_eq!(service.get_discovered_audio_device(4), 0);
                assert_eq!(service.get_connected_audio_devices(), 0);
                assert_eq!(service.get_paired_audio_devices(), 0);
                assert_eq!(
                    service.request_audio_device_connection_rejection(0x1234),
                    RESULT_SUCCESS
                );
                assert_eq!(
                    service.cancel_audio_device_connection_rejection(0x1234),
                    RESULT_SUCCESS
                );

                let radio_event = Arc::clone(&service.radio_event);
                let audio_event = Arc::clone(&service.audio_device_connection_event);
                assert!(Arc::ptr_eq(
                    &radio_event,
                    &service
                        .service_context
                        .get_event(service.radio_event_handle)
                        .unwrap()
                ));
                assert!(Arc::ptr_eq(
                    &audio_event,
                    &service
                        .service_context
                        .get_event(service.audio_device_connection_event_handle)
                        .unwrap()
                ));
                assert_eq!(Arc::strong_count(&radio_event), 3);
                assert_eq!(Arc::strong_count(&audio_event), 3);

                drop(service);

                assert_eq!(Arc::strong_count(&radio_event), 1);
                assert_eq!(Arc::strong_count(&audio_event), 1);
            })
            .unwrap()
            .join()
            .unwrap();
    }
}
