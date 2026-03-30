//! Port of zuyu/src/core/hle/service/audio/audio_device.h and audio_device.cpp
//!
//! IAudioDevice service.

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::kernel::k_readable_event::KReadableEvent;
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
    /// The audio device event — upstream creates this in the constructor via
    /// `service_context.CreateEvent(...)` and signals it immediately.
    /// Created lazily on first Query*Event call.
    event: Mutex<Option<Arc<Mutex<KReadableEvent>>>>,
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
            event: Mutex::new(None),
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    /// Ensure the shared event exists, creating it lazily. Returns a handle for the caller.
    /// Upstream creates the event in the constructor and signals it immediately.
    fn get_or_create_event(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) -> Option<u32> {
        let svc = Self::as_self(this);
        let mut event_guard = svc.event.lock().unwrap();

        if let Some(ref readable) = *event_guard {
            // Already created — return a copy handle.
            return ctx.copy_handle_for_readable_event(Arc::clone(readable));
        }

        // Create the event (signaled=true, matching upstream constructor `event->Signal()`).
        let (handle, readable_event) = ctx.create_readable_event(true)?;
        *event_guard = Some(readable_event);
        Some(handle)
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
        log::debug!("IAudioDevice::ListAudioDeviceName");
        Self::write_device_name(ctx);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(1); // one device
    }

    fn set_audio_device_output_volume_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::SetAudioDeviceOutputVolume");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_audio_device_output_volume_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::GetAudioDeviceOutputVolume");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_f32(1.0);
    }

    fn get_active_audio_device_name_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::GetActiveAudioDeviceName");
        Self::write_device_name(ctx);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Port of upstream `IAudioDevice::QueryAudioDeviceSystemEvent`.
    /// Upstream: signals the event then returns `&event->GetReadableEvent()`.
    fn query_audio_device_system_event_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::info!("IAudioDevice::QueryAudioDeviceSystemEvent");
        if let Some(handle) = Self::get_or_create_event(this, ctx) {
            log::info!("IAudioDevice::QueryAudioDeviceSystemEvent handle={:#x}", handle);
            let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
            rb.push_result(RESULT_SUCCESS);
            rb.push_copy_objects(handle);
        } else {
            log::error!("IAudioDevice::QueryAudioDeviceSystemEvent failed to create event");
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(RESULT_SUCCESS);
        }
    }

    fn get_active_channel_count_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::GetActiveChannelCount");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(2); // stereo
    }

    fn list_audio_device_name_auto_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::ListAudioDeviceNameAuto");
        Self::write_device_name(ctx);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(1); // one device
    }

    fn set_audio_device_output_volume_auto_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::SetAudioDeviceOutputVolumeAuto");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_audio_device_output_volume_auto_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::GetAudioDeviceOutputVolumeAuto");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_f32(1.0);
    }

    fn get_active_audio_device_name_auto_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::GetActiveAudioDeviceNameAuto");
        Self::write_device_name(ctx);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Port of upstream `IAudioDevice::QueryAudioDeviceInputEvent`.
    /// Upstream returns `&event->GetReadableEvent()`.
    fn query_audio_device_input_event_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::info!("IAudioDevice::QueryAudioDeviceInputEvent");
        if let Some(handle) = Self::get_or_create_event(this, ctx) {
            let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
            rb.push_result(RESULT_SUCCESS);
            rb.push_copy_objects(handle);
        } else {
            log::error!("IAudioDevice::QueryAudioDeviceInputEvent failed to create event");
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(RESULT_SUCCESS);
        }
    }

    /// Port of upstream `IAudioDevice::QueryAudioDeviceOutputEvent`.
    /// Upstream returns `&event->GetReadableEvent()`.
    fn query_audio_device_output_event_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::info!("IAudioDevice::QueryAudioDeviceOutputEvent");
        if let Some(handle) = Self::get_or_create_event(this, ctx) {
            let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
            rb.push_result(RESULT_SUCCESS);
            rb.push_copy_objects(handle);
        } else {
            log::error!("IAudioDevice::QueryAudioDeviceOutputEvent failed to create event");
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(RESULT_SUCCESS);
        }
    }

    fn get_active_audio_output_device_name_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::GetActiveAudioOutputDeviceName");
        Self::write_device_name(ctx);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn list_audio_output_device_name_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::ListAudioOutputDeviceName");
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
