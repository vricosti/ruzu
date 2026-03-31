//! Port of zuyu/src/core/hle/service/audio/audio_in.h and audio_in.cpp
//!
//! IAudioIn service.

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IAudioIn:
///
/// | Cmd | Name                          |
/// |-----|-------------------------------|
/// | 0   | GetAudioInState               |
/// | 1   | Start                         |
/// | 2   | Stop                          |
/// | 3   | AppendAudioInBuffer           |
/// | 4   | RegisterBufferEvent           |
/// | 5   | GetReleasedAudioInBuffers     |
/// | 6   | ContainsAudioInBuffer         |
/// | 7   | AppendUacInBuffer             |
/// | 8   | AppendAudioInBufferAuto       |
/// | 9   | GetReleasedAudioInBuffersAuto |
/// | 10  | AppendUacInBufferAuto         |
/// | 11  | GetAudioInBufferCount         |
/// | 12  | SetDeviceGain                 |
/// | 13  | GetDeviceGain                 |
/// | 14  | FlushAudioInBuffers           |
pub struct IAudioIn {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IAudioIn {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::get_audio_in_state_handler), "GetAudioInState"),
            (1, Some(Self::start_handler), "Start"),
            (2, Some(Self::stop_handler), "Stop"),
            (
                3,
                Some(Self::append_audio_in_buffer_handler),
                "AppendAudioInBuffer",
            ),
            (
                4,
                Some(Self::register_buffer_event_handler),
                "RegisterBufferEvent",
            ),
            (
                5,
                Some(Self::get_released_audio_in_buffers_handler),
                "GetReleasedAudioInBuffers",
            ),
            (
                6,
                Some(Self::contains_audio_in_buffer_handler),
                "ContainsAudioInBuffer",
            ),
            (
                7,
                Some(Self::append_uac_in_buffer_handler),
                "AppendUacInBuffer",
            ),
            (
                8,
                Some(Self::append_audio_in_buffer_auto_handler),
                "AppendAudioInBufferAuto",
            ),
            (
                9,
                Some(Self::get_released_audio_in_buffers_auto_handler),
                "GetReleasedAudioInBuffersAuto",
            ),
            (
                10,
                Some(Self::append_uac_in_buffer_auto_handler),
                "AppendUacInBufferAuto",
            ),
            (
                11,
                Some(Self::get_audio_in_buffer_count_handler),
                "GetAudioInBufferCount",
            ),
            (12, Some(Self::set_device_gain_handler), "SetDeviceGain"),
            (13, Some(Self::get_device_gain_handler), "GetDeviceGain"),
            (
                14,
                Some(Self::flush_audio_in_buffers_handler),
                "FlushAudioInBuffers",
            ),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn get_audio_in_state_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioIn::GetAudioInState (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0); // Stopped state
    }

    fn start_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioIn::Start (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn stop_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioIn::Stop (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn append_audio_in_buffer_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioIn::AppendAudioInBuffer (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn register_buffer_event_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioIn::RegisterBufferEvent (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_released_audio_in_buffers_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioIn::GetReleasedAudioInBuffers (STUBBED)");
        ctx.write_buffer(&[], 0);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0); // count
    }

    fn contains_audio_in_buffer_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioIn::ContainsAudioInBuffer (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(false);
    }

    fn append_uac_in_buffer_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioIn::AppendUacInBuffer (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn append_audio_in_buffer_auto_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioIn::AppendAudioInBufferAuto (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_released_audio_in_buffers_auto_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioIn::GetReleasedAudioInBuffersAuto (STUBBED)");
        ctx.write_buffer(&[], 0);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0); // count
    }

    fn append_uac_in_buffer_auto_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioIn::AppendUacInBufferAuto (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_audio_in_buffer_count_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioIn::GetAudioInBufferCount (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0);
    }

    fn set_device_gain_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioIn::SetDeviceGain (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_device_gain_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioIn::GetDeviceGain (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_f32(0.0);
    }

    fn flush_audio_in_buffers_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioIn::FlushAudioInBuffers (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(true);
    }
}

impl SessionRequestHandler for IAudioIn {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IAudioIn {
    fn get_service_name(&self) -> &str {
        "IAudioIn"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
