//! Port of zuyu/src/core/hle/service/audio/audio_renderer_manager.h and audio_renderer_manager.cpp
//!
//! IAudioRendererManager service ("audren:u").

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IAudioRendererManager ("audren:u"):
///
/// | Cmd | Name                                       |
/// |-----|--------------------------------------------|
/// | 0   | OpenAudioRenderer                          |
/// | 1   | GetWorkBufferSize                          |
/// | 2   | GetAudioDeviceService                      |
/// | 3   | OpenAudioRendererForManualExecution         |
/// | 4   | GetAudioDeviceServiceWithRevisionInfo       |
pub struct IAudioRendererManager {
    num_audio_devices: std::sync::atomic::AtomicU32,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IAudioRendererManager {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::open_audio_renderer_handler), "OpenAudioRenderer"),
            (1, Some(Self::get_work_buffer_size_handler), "GetWorkBufferSize"),
            (2, Some(Self::get_audio_device_service_handler), "GetAudioDeviceService"),
            (3, None, "OpenAudioRendererForManualExecution"),
            (4, Some(Self::get_audio_device_service_with_revision_info_handler), "GetAudioDeviceServiceWithRevisionInfo"),
        ]);
        Self {
            num_audio_devices: std::sync::atomic::AtomicU32::new(0),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Port of upstream `IAudioRendererManager::OpenAudioRenderer`.
    ///
    /// Upstream creates a new `IAudioRenderer` with a real renderer backend.
    /// We create a stub IAudioRenderer that provides event handles so the game
    /// doesn't block on missing kernel events.
    fn open_audio_renderer_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::info!("IAudioRendererManager::OpenAudioRenderer");
        let renderer = Arc::new(super::audio_renderer::IAudioRenderer::new());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(renderer);
    }

    fn get_work_buffer_size_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioRendererManager::GetWorkBufferSize");
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(0x4000); // reasonable default work buffer size
    }

    /// Port of upstream `IAudioRendererManager::GetAudioDeviceService`.
    /// Upstream creates `IAudioDevice(system, aruid, REV1_magic, num_audio_devices++)`.
    fn get_audio_device_service_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::info!("IAudioRendererManager::GetAudioDeviceService");
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let device_num = svc.num_audio_devices.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let device = Arc::new(super::audio_device::IAudioDevice::new(0, 0x52455631, device_num)); // 'REV1'
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(device);
    }

    /// Port of upstream `IAudioRendererManager::GetAudioDeviceServiceWithRevisionInfo`.
    /// Upstream creates `IAudioDevice(system, aruid, revision, num_audio_devices++)`.
    fn get_audio_device_service_with_revision_info_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::info!("IAudioRendererManager::GetAudioDeviceServiceWithRevisionInfo");
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let device_num = svc.num_audio_devices.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let device = Arc::new(super::audio_device::IAudioDevice::new(0, 0, device_num));
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(device);
    }
}

impl SessionRequestHandler for IAudioRendererManager {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IAudioRendererManager {
    fn get_service_name(&self) -> &str {
        "audren:u"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
