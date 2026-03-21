//! Port of zuyu/src/core/hle/service/audio/audio_renderer.h and audio_renderer.cpp
//!
//! IAudioRenderer service.

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IAudioRenderer:
///
/// | Cmd | Name                              |
/// |-----|-----------------------------------|
/// | 0   | GetSampleRate                     |
/// | 1   | GetSampleCount                    |
/// | 2   | GetMixBufferCount                 |
/// | 3   | GetState                          |
/// | 4   | RequestUpdate                     |
/// | 5   | Start                             |
/// | 6   | Stop                              |
/// | 7   | QuerySystemEvent                  |
/// | 8   | SetRenderingTimeLimit             |
/// | 9   | GetRenderingTimeLimit             |
/// | 10  | RequestUpdateAuto                 |
/// | 11  | ExecuteAudioRendererRendering     |
/// | 12  | SetVoiceDropParameter             |
/// | 13  | GetVoiceDropParameter             |
pub struct IAudioRenderer {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IAudioRenderer {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::get_sample_rate_handler), "GetSampleRate"),
            (1, Some(Self::get_sample_count_handler), "GetSampleCount"),
            (2, Some(Self::get_mix_buffer_count_handler), "GetMixBufferCount"),
            (3, Some(Self::get_state_handler), "GetState"),
            (4, Some(Self::request_update_handler), "RequestUpdate"),
            (5, Some(Self::start_handler), "Start"),
            (6, Some(Self::stop_handler), "Stop"),
            (7, Some(Self::query_system_event_handler), "QuerySystemEvent"),
            (8, Some(Self::set_rendering_time_limit_handler), "SetRenderingTimeLimit"),
            (9, Some(Self::get_rendering_time_limit_handler), "GetRenderingTimeLimit"),
            (10, Some(Self::request_update_auto_handler), "RequestUpdateAuto"),
            (11, None, "ExecuteAudioRendererRendering"),
            (12, Some(Self::set_voice_drop_parameter_handler), "SetVoiceDropParameter"),
            (13, Some(Self::get_voice_drop_parameter_handler), "GetVoiceDropParameter"),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn get_sample_rate_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioRenderer::GetSampleRate (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(48000);
    }

    fn get_sample_count_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioRenderer::GetSampleCount (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(240);
    }

    fn get_mix_buffer_count_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioRenderer::GetMixBufferCount (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0);
    }

    fn get_state_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioRenderer::GetState (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0); // Stopped state
    }

    fn request_update_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioRenderer::RequestUpdate (STUBBED)");
        ctx.write_buffer(&[], 0);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn start_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioRenderer::Start (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn stop_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioRenderer::Stop (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn query_system_event_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioRenderer::QuerySystemEvent (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_rendering_time_limit_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioRenderer::SetRenderingTimeLimit (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_rendering_time_limit_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioRenderer::GetRenderingTimeLimit (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(100); // 100% rendering time limit
    }

    fn request_update_auto_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioRenderer::RequestUpdateAuto (STUBBED)");
        ctx.write_buffer(&[], 0);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_voice_drop_parameter_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioRenderer::SetVoiceDropParameter (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_voice_drop_parameter_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioRenderer::GetVoiceDropParameter (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_f32(0.0);
    }
}

impl SessionRequestHandler for IAudioRenderer {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IAudioRenderer {
    fn get_service_name(&self) -> &str {
        "IAudioRenderer"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
