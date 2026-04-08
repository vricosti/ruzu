//! Port of zuyu/src/core/hle/service/audio/audio_device.h and audio_device.cpp
//!
//! IAudioDevice service.

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::core::SystemRef;
use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
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
    system: SystemRef,
    applet_resource_user_id: u64,
    revision: u32,
    _device_num: u32,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    /// The audio device event — upstream creates this in the constructor via
    /// `service_context.CreateEvent(...)` and signals it immediately.
    /// Created lazily on first Query*Event call.
    event: Mutex<Option<Arc<Mutex<KReadableEvent>>>>,
}

impl IAudioDevice {
    pub fn new(
        system: SystemRef,
        applet_resource_user_id: u64,
        revision: u32,
        device_num: u32,
    ) -> Self {
        let handlers = build_handler_map(&[
            (
                0,
                Some(Self::list_audio_device_name_handler),
                "ListAudioDeviceName",
            ),
            (
                1,
                Some(Self::set_audio_device_output_volume_handler),
                "SetAudioDeviceOutputVolume",
            ),
            (
                2,
                Some(Self::get_audio_device_output_volume_handler),
                "GetAudioDeviceOutputVolume",
            ),
            (
                3,
                Some(Self::get_active_audio_device_name_handler),
                "GetActiveAudioDeviceName",
            ),
            (
                4,
                Some(Self::query_audio_device_system_event_handler),
                "QueryAudioDeviceSystemEvent",
            ),
            (
                5,
                Some(Self::get_active_channel_count_handler),
                "GetActiveChannelCount",
            ),
            (
                6,
                Some(Self::list_audio_device_name_auto_handler),
                "ListAudioDeviceNameAuto",
            ),
            (
                7,
                Some(Self::set_audio_device_output_volume_auto_handler),
                "SetAudioDeviceOutputVolumeAuto",
            ),
            (
                8,
                Some(Self::get_audio_device_output_volume_auto_handler),
                "GetAudioDeviceOutputVolumeAuto",
            ),
            (
                10,
                Some(Self::get_active_audio_device_name_auto_handler),
                "GetActiveAudioDeviceNameAuto",
            ),
            (
                11,
                Some(Self::query_audio_device_input_event_handler),
                "QueryAudioDeviceInputEvent",
            ),
            (
                12,
                Some(Self::query_audio_device_output_event_handler),
                "QueryAudioDeviceOutputEvent",
            ),
            (
                13,
                Some(Self::get_active_audio_output_device_name_handler),
                "GetActiveAudioOutputDeviceName",
            ),
            (
                14,
                Some(Self::list_audio_output_device_name_handler),
                "ListAudioOutputDeviceName",
            ),
        ]);
        Self {
            system,
            applet_resource_user_id,
            revision,
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
    fn get_or_create_event(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) -> Option<u32> {
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

    fn write_name_array(ctx: &mut HLERequestContext, names: &[[u8; 0x100]]) {
        let mut bytes = Vec::with_capacity(names.len() * 0x100);
        for name in names {
            bytes.extend_from_slice(name);
        }
        ctx.write_buffer(&bytes, 0);
    }

    fn write_single_name(ctx: &mut HLERequestContext, name: &[u8; 0x100]) {
        ctx.write_buffer(name, 0);
    }

    fn read_first_name(ctx: &HLERequestContext) -> String {
        let bytes = ctx.read_buffer(0);
        let raw_name = bytes.get(..0x100).unwrap_or(&[]);
        let nul = raw_name
            .iter()
            .position(|&byte| byte == 0)
            .unwrap_or(raw_name.len());
        String::from_utf8_lossy(&raw_name[..nul]).into_owned()
    }

    fn make_audio_device_name(name: &str) -> [u8; 0x100] {
        let mut out = [0u8; 0x100];
        let bytes = name.as_bytes();
        let len = bytes.len().min(out.len().saturating_sub(1));
        out[..len].copy_from_slice(&bytes[..len]);
        out
    }

    fn default_active_device_name() -> [u8; 0x100] {
        Self::make_audio_device_name("AudioTvOutput")
    }

    fn list_audio_device_names(&self) -> Vec<[u8; 0x100]> {
        let mut names = vec![[0u8; 0x100]; 4];
        let count = (if self.system.is_null() {
            None
        } else {
            self.system.get().audio_core()
        })
            .map(|audio_core| {
                audio_core.list_audio_device_name(
                    self.applet_resource_user_id,
                    self.revision,
                    &mut names,
                ) as usize
            })
            .unwrap_or_else(|| {
                names[0] = Self::default_active_device_name();
                1
            });
        names.truncate(count);
        names
    }

    fn list_audio_output_device_names(&self) -> Vec<[u8; 0x100]> {
        let mut names = vec![[0u8; 0x100]; 3];
        let count = (if self.system.is_null() {
            None
        } else {
            self.system.get().audio_core()
        })
            .map(|audio_core| {
                audio_core.list_audio_output_device_name(
                    self.applet_resource_user_id,
                    self.revision,
                    &mut names,
                ) as usize
            })
            .unwrap_or_else(|| {
                names[0] = Self::default_active_device_name();
                1
            });
        names.truncate(count);
        names
    }

    fn list_audio_device_name_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioDevice::ListAudioDeviceName");
        let svc = Self::as_self(this);
        let names = svc.list_audio_device_names();
        Self::write_name_array(ctx, &names);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(names.len() as u32);
    }

    fn set_audio_device_output_volume_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::SetAudioDeviceOutputVolume");
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let volume = rp.pop_f32();
        let device_name = Self::read_first_name(ctx);
        if device_name == "AudioTvOutput" {
            if let Some(audio_core) = (!svc.system.is_null())
                .then(|| svc.system.get())
                .and_then(|system| system.audio_core())
            {
                audio_core.set_audio_device_volume(
                    svc.applet_resource_user_id,
                    svc.revision,
                    volume,
                );
            }
        }
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_audio_device_output_volume_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::GetAudioDeviceOutputVolume");
        let svc = Self::as_self(this);
        let device_name = Self::read_first_name(ctx);
        let volume = if device_name == "AudioTvOutput" {
            (!svc.system.is_null())
                .then(|| svc.system.get())
                .and_then(|system| system.audio_core())
                .map(|audio_core| {
                    audio_core.get_audio_device_volume(
                        svc.applet_resource_user_id,
                        svc.revision,
                        &device_name,
                    )
                })
                .unwrap_or(1.0)
        } else {
            1.0
        };
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_f32(volume);
    }

    fn get_active_audio_device_name_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::GetActiveAudioDeviceName");
        Self::write_single_name(ctx, &Self::default_active_device_name());
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
            log::info!(
                "IAudioDevice::QueryAudioDeviceSystemEvent handle={:#x}",
                handle
            );
            let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
            rb.push_result(RESULT_SUCCESS);
            rb.push_copy_objects(handle);
        } else {
            log::error!("IAudioDevice::QueryAudioDeviceSystemEvent failed to create event");
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(RESULT_SUCCESS);
        }
    }

    fn get_active_channel_count_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioDevice::GetActiveChannelCount");
        let svc = Self::as_self(_this);
        let active_channel_count = if let Some(audio_core) = (!svc.system.is_null())
            .then(|| svc.system.get())
            .and_then(|system| system.audio_core())
        {
            audio_core.get_audio_output_system_channels()
        } else {
            2
        };
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(active_channel_count);
    }

    fn list_audio_device_name_auto_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioDevice::ListAudioDeviceNameAuto");
        Self::list_audio_device_name_handler(this, ctx);
    }

    fn set_audio_device_output_volume_auto_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::SetAudioDeviceOutputVolumeAuto");
        Self::set_audio_device_output_volume_handler(this, ctx);
    }

    fn get_audio_device_output_volume_auto_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::GetAudioDeviceOutputVolumeAuto");
        Self::get_audio_device_output_volume_handler(this, ctx);
    }

    fn get_active_audio_device_name_auto_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::GetActiveAudioDeviceNameAuto");
        Self::write_single_name(ctx, &Self::default_active_device_name());
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
        Self::write_single_name(ctx, &Self::default_active_device_name());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn list_audio_output_device_name_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioDevice::ListAudioOutputDeviceName");
        let svc = Self::as_self(this);
        let names = svc.list_audio_output_device_names();
        Self::write_name_array(ctx, &names);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(names.len() as u32);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_keeps_applet_resource_user_id_and_revision() {
        let service = IAudioDevice::new(SystemRef::null(), 0x51, 0x5245_5631, 3);
        assert_eq!(service.applet_resource_user_id, 0x51);
        assert_eq!(service.revision, 0x5245_5631);
    }

    #[test]
    fn fallback_lists_keep_upstream_default_tv_name() {
        let service = IAudioDevice::new(SystemRef::null(), 0, 0x5245_5631, 0);
        let names = service.list_audio_device_names();
        assert_eq!(names.len(), 1);
        assert_eq!(
            &names[0][..14],
            &IAudioDevice::make_audio_device_name("AudioTvOutput")[..14]
        );
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
