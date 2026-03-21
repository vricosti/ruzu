//! Port of zuyu/src/core/hle/service/audio/audio_device.h and audio_device.cpp
//!
//! IAudioDevice service.

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IAudioDevice:
///
/// | Cmd | Name                            |
/// |-----|---------------------------------|
/// | 0   | ListAudioDeviceName             |
/// | 1   | SetAudioDeviceOutputVolume      |
/// | 2   | GetAudioDeviceOutputVolume      |
/// | 3   | GetActiveAudioDeviceName        |
/// | 4   | QueryAudioDeviceSystemEvent     |
/// | 5   | GetActiveChannelCount           |
/// | 6   | ListAudioDeviceNameAuto         |
/// | 7   | SetAudioDeviceOutputVolumeAuto  |
/// | 8   | GetAudioDeviceOutputVolumeAuto  |
/// | 10  | GetActiveAudioDeviceNameAuto    |
/// | 11  | QueryAudioDeviceInputEvent      |
/// | 12  | QueryAudioDeviceOutputEvent     |
/// | 13  | GetActiveAudioOutputDeviceName  |
/// | 14  | ListAudioOutputDeviceName       |
pub struct IAudioDevice {
    _applet_resource_user_id: u64,
    _revision: u32,
    _device_num: u32,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IAudioDevice {
    pub fn new(applet_resource_user_id: u64, revision: u32, device_num: u32) -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::list_audio_device_name_handler), "ListAudioDeviceName"),
            (1, Some(Self::set_audio_device_output_volume_handler), "SetAudioDeviceOutputVolume"),
            (2, Some(Self::get_audio_device_output_volume_handler), "GetAudioDeviceOutputVolume"),
            (3, Some(Self::get_active_audio_device_name_handler), "GetActiveAudioDeviceName"),
            (4, Some(Self::query_audio_device_system_event_handler), "QueryAudioDeviceSystemEvent"),
            (5, Some(Self::get_active_channel_count_handler), "GetActiveChannelCount"),
            (6, Some(Self::list_audio_device_name_auto_handler), "ListAudioDeviceNameAuto"),
            (7, Some(Self::set_audio_device_output_volume_auto_handler), "SetAudioDeviceOutputVolumeAuto"),
            (8, Some(Self::get_audio_device_output_volume_auto_handler), "GetAudioDeviceOutputVolumeAuto"),
            (10, Some(Self::get_active_audio_device_name_auto_handler), "GetActiveAudioDeviceNameAuto"),
            (11, Some(Self::query_audio_device_input_event_handler), "QueryAudioDeviceInputEvent"),
            (12, Some(Self::query_audio_device_output_event_handler), "QueryAudioDeviceOutputEvent"),
            (13, Some(Self::get_active_audio_output_device_name_handler), "GetActiveAudioOutputDeviceName"),
            (14, Some(Self::list_audio_output_device_name_handler), "ListAudioOutputDeviceName"),
        ]);
        Self {
            _applet_resource_user_id: applet_resource_user_id,
            _revision: revision,
            _device_num: device_num,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Write the default audio device name to the output buffer.
    fn write_device_name(ctx: &mut HLERequestContext) {
        let name = b"AudioTvOutput\0";
        ctx.write_buffer(name, 0);
    }

    fn list_audio_device_name_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::ListAudioDeviceName (STUBBED)");
        Self::write_device_name(ctx);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(1); // one device
    }

    fn set_audio_device_output_volume_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::SetAudioDeviceOutputVolume (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_audio_device_output_volume_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::GetAudioDeviceOutputVolume (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_f32(1.0);
    }

    fn get_active_audio_device_name_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::GetActiveAudioDeviceName (STUBBED)");
        Self::write_device_name(ctx);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn query_audio_device_system_event_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::QueryAudioDeviceSystemEvent (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_active_channel_count_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::GetActiveChannelCount (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(2); // stereo
    }

    fn list_audio_device_name_auto_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::ListAudioDeviceNameAuto (STUBBED)");
        Self::write_device_name(ctx);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(1); // one device
    }

    fn set_audio_device_output_volume_auto_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::SetAudioDeviceOutputVolumeAuto (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_audio_device_output_volume_auto_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::GetAudioDeviceOutputVolumeAuto (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_f32(1.0);
    }

    fn get_active_audio_device_name_auto_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::GetActiveAudioDeviceNameAuto (STUBBED)");
        Self::write_device_name(ctx);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn query_audio_device_input_event_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::QueryAudioDeviceInputEvent (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn query_audio_device_output_event_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::QueryAudioDeviceOutputEvent (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_active_audio_output_device_name_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::GetActiveAudioOutputDeviceName (STUBBED)");
        Self::write_device_name(ctx);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn list_audio_output_device_name_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::ListAudioOutputDeviceName (STUBBED)");
        Self::write_device_name(ctx);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(1); // one device
    }
}

impl SessionRequestHandler for IAudioDevice {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IAudioDevice {
    fn get_service_name(&self) -> &str {
        "IAudioDevice"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
