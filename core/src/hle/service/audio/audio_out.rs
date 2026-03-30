//! Port of zuyu/src/core/hle/service/audio/audio_out.h and audio_out.cpp
//!
//! IAudioOut service.

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IAudioOut:
///
/// | Cmd | Name                            |
/// |-----|---------------------------------|
/// | 0   | GetAudioOutState                |
/// | 1   | Start                           |
/// | 2   | Stop                            |
/// | 3   | AppendAudioOutBuffer            |
/// | 4   | RegisterBufferEvent             |
/// | 5   | GetReleasedAudioOutBuffers      |
/// | 6   | ContainsAudioOutBuffer          |
/// | 7   | AppendAudioOutBufferAuto        |
/// | 8   | GetReleasedAudioOutBuffersAuto  |
/// | 9   | GetAudioOutBufferCount          |
/// | 10  | GetAudioOutPlayedSampleCount    |
/// | 11  | FlushAudioOutBuffers            |
/// | 12  | SetAudioOutVolume               |
/// | 13  | GetAudioOutVolume               |
pub struct IAudioOut {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    /// Buffer event — upstream creates in constructor, signals when buffers are released.
    /// Created lazily on first RegisterBufferEvent call.
    buffer_event: Mutex<Option<Arc<Mutex<KReadableEvent>>>>,
}

impl IAudioOut {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::get_audio_out_state_handler), "GetAudioOutState"),
            (1, Some(Self::start_handler), "Start"),
            (2, Some(Self::stop_handler), "Stop"),
            (3, Some(Self::append_audio_out_buffer_handler), "AppendAudioOutBuffer"),
            (4, Some(Self::register_buffer_event_handler), "RegisterBufferEvent"),
            (5, Some(Self::get_released_audio_out_buffers_handler), "GetReleasedAudioOutBuffers"),
            (6, Some(Self::contains_audio_out_buffer_handler), "ContainsAudioOutBuffer"),
            (7, Some(Self::append_audio_out_buffer_auto_handler), "AppendAudioOutBufferAuto"),
            (8, Some(Self::get_released_audio_out_buffers_auto_handler), "GetReleasedAudioOutBuffersAuto"),
            (9, Some(Self::get_audio_out_buffer_count_handler), "GetAudioOutBufferCount"),
            (10, Some(Self::get_audio_out_played_sample_count_handler), "GetAudioOutPlayedSampleCount"),
            (11, Some(Self::flush_audio_out_buffers_handler), "FlushAudioOutBuffers"),
            (12, Some(Self::set_audio_out_volume_handler), "SetAudioOutVolume"),
            (13, Some(Self::get_audio_out_volume_handler), "GetAudioOutVolume"),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
            buffer_event: Mutex::new(None),
        }
    }

    fn get_audio_out_state_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioOut::GetAudioOutState (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0); // Stopped state
    }

    fn start_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioOut::Start (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn stop_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioOut::Stop (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn append_audio_out_buffer_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioOut::AppendAudioOutBuffer (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    /// Port of upstream `IAudioOut::RegisterBufferEvent`.
    /// Upstream returns `&buffer_event->GetReadableEvent()`.
    fn register_buffer_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::info!("IAudioOut::RegisterBufferEvent");
        let svc = Self::as_self(this);
        let mut event_guard = svc.buffer_event.lock().unwrap();

        if let Some(ref readable) = *event_guard {
            if let Some(handle) = ctx.copy_handle_for_readable_event(Arc::clone(readable)) {
                let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_copy_objects(handle);
                return;
            }
        }

        // Create pre-signaled event (so first wait returns immediately).
        if let Some((handle, readable_event)) = ctx.create_readable_event(true) {
            log::info!("IAudioOut::RegisterBufferEvent created event handle={:#x}", handle);
            *event_guard = Some(readable_event);
            drop(event_guard);
            let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
            rb.push_result(RESULT_SUCCESS);
            rb.push_copy_objects(handle);
        } else {
            log::error!("IAudioOut::RegisterBufferEvent failed to create event");
            drop(event_guard);
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(RESULT_SUCCESS);
        }
    }

    fn get_released_audio_out_buffers_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioOut::GetReleasedAudioOutBuffers (STUBBED)");
        // Write empty buffer (no released buffers)
        ctx.write_buffer(&[], 0);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0); // count
    }

    fn contains_audio_out_buffer_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioOut::ContainsAudioOutBuffer (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(false);
    }

    fn append_audio_out_buffer_auto_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioOut::AppendAudioOutBufferAuto (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_released_audio_out_buffers_auto_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioOut::GetReleasedAudioOutBuffersAuto (STUBBED)");
        ctx.write_buffer(&[], 0);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0); // count
    }

    fn get_audio_out_buffer_count_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioOut::GetAudioOutBufferCount (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0);
    }

    fn get_audio_out_played_sample_count_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioOut::GetAudioOutPlayedSampleCount (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(0);
    }

    fn flush_audio_out_buffers_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioOut::FlushAudioOutBuffers (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(true);
    }

    fn set_audio_out_volume_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioOut::SetAudioOutVolume (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_audio_out_volume_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioOut::GetAudioOutVolume (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_f32(1.0);
    }
}

impl SessionRequestHandler for IAudioOut {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IAudioOut {
    fn get_service_name(&self) -> &str {
        "IAudioOut"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
