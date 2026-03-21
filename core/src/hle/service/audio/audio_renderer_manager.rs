//! Port of zuyu/src/core/hle/service/audio/audio_renderer_manager.h and audio_renderer_manager.cpp
//!
//! IAudioRendererManager service ("audren:u").

use std::collections::BTreeMap;

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
    _num_audio_devices: u32,
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
            _num_audio_devices: 0,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn open_audio_renderer_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioRendererManager::OpenAudioRenderer (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_work_buffer_size_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioRendererManager::GetWorkBufferSize (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(0x4000); // reasonable default work buffer size
    }

    fn get_audio_device_service_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioRendererManager::GetAudioDeviceService (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_audio_device_service_with_revision_info_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioRendererManager::GetAudioDeviceServiceWithRevisionInfo (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
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
