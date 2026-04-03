//! Port of zuyu/src/core/hle/service/audio/audio_renderer.h and audio_renderer.cpp
//!
//! IAudioRenderer service.

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::audio_core::{AudioRendererSessionHandle, ExecutionMode};
use crate::hle::kernel::k_readable_event::KReadableEvent;
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
    renderer_impl: Arc<dyn AudioRendererSessionHandle>,
    /// The rendered event — signals the game that audio rendering is complete.
    /// Upstream: `Kernel::KEvent* rendered_event` created in constructor via
    /// `service_context.CreateEvent("IAudioRendererEvent")`.
    /// Created lazily on first `QuerySystemEvent` call since we need the
    /// HLERequestContext to create the kernel event.
    rendered_event: Mutex<Option<Arc<Mutex<KReadableEvent>>>>,
}

impl IAudioRenderer {
    pub fn new(renderer_impl: Arc<dyn AudioRendererSessionHandle>) -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::get_sample_rate_handler), "GetSampleRate"),
            (1, Some(Self::get_sample_count_handler), "GetSampleCount"),
            (
                2,
                Some(Self::get_mix_buffer_count_handler),
                "GetMixBufferCount",
            ),
            (3, Some(Self::get_state_handler), "GetState"),
            (4, Some(Self::request_update_handler), "RequestUpdate"),
            (5, Some(Self::start_handler), "Start"),
            (6, Some(Self::stop_handler), "Stop"),
            (
                7,
                Some(Self::query_system_event_handler),
                "QuerySystemEvent",
            ),
            (
                8,
                Some(Self::set_rendering_time_limit_handler),
                "SetRenderingTimeLimit",
            ),
            (
                9,
                Some(Self::get_rendering_time_limit_handler),
                "GetRenderingTimeLimit",
            ),
            (
                10,
                Some(Self::request_update_auto_handler),
                "RequestUpdateAuto",
            ),
            (11, None, "ExecuteAudioRendererRendering"),
            (
                12,
                Some(Self::set_voice_drop_parameter_handler),
                "SetVoiceDropParameter",
            ),
            (
                13,
                Some(Self::get_voice_drop_parameter_handler),
                "GetVoiceDropParameter",
            ),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
            renderer_impl,
            rendered_event: Mutex::new(None),
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    fn get_sample_rate_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(_this);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(svc.renderer_impl.get_sample_rate());
    }

    fn get_sample_count_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(_this);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(svc.renderer_impl.get_sample_count());
    }

    fn get_mix_buffer_count_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(_this);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(svc.renderer_impl.get_mix_buffer_count());
    }

    fn get_state_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(_this);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(svc.renderer_impl.get_state());
    }

    /// Port of upstream `IAudioRenderer::RequestUpdate` → delegates to RequestUpdateAuto.
    /// Since we don't have a real audio renderer impl, we stub the response and signal
    /// the rendered event so the game's WaitSynchronization returns.
    fn request_update_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioRenderer::RequestUpdate");
        Self::request_update_impl(this, ctx);
    }

    fn request_update_auto_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioRenderer::RequestUpdateAuto");
        Self::request_update_impl(this, ctx);
    }

    /// Common implementation for RequestUpdate and RequestUpdateAuto.
    /// Without a real audio renderer, we return an empty output buffer.
    /// The rendered event was created as pre-signaled and stays signaled
    /// (KReadableEvent does not auto-clear), so WaitSynchronization always
    /// returns immediately. This prevents the game from blocking on audio.
    fn request_update_impl(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let input = if ctx.can_read_buffer(0) {
            ctx.read_buffer(0)
        } else {
            Vec::new()
        };
        let mut performance = if ctx.can_write_buffer(1) {
            vec![0u8; ctx.get_write_buffer_size(1)]
        } else {
            Vec::new()
        };
        let mut output = if ctx.can_write_buffer(0) {
            vec![0u8; ctx.get_write_buffer_size(0)]
        } else {
            Vec::new()
        };
        let result = svc
            .renderer_impl
            .request_update(&input, &mut performance, &mut output);
        log::info!(
            "IAudioRenderer::RequestUpdate result={:#x} input_len={} output_len={} perf_len={}",
            result,
            input.len(),
            output.len(),
            performance.len()
        );
        if !output.is_empty() {
            ctx.write_buffer(&output, 0);
        }
        if !performance.is_empty() {
            ctx.write_buffer(&performance, 1);
        }
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(crate::hle::result::ResultCode::new(result));
    }

    fn start_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        Self::as_self(_this).renderer_impl.start();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn stop_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        Self::as_self(_this).renderer_impl.stop();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Port of upstream `IAudioRenderer::QuerySystemEvent`.
    ///
    /// Upstream returns `&rendered_event->GetReadableEvent()` as a copy handle.
    /// We create the KReadableEvent lazily on first call and return its handle.
    /// The event starts signaled so the first WaitSynchronization returns immediately.
    fn query_system_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::info!("IAudioRenderer::QuerySystemEvent");
        let svc = Self::as_self(this);
        if svc.renderer_impl.execution_mode() == ExecutionMode::Manual {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(crate::hle::result::ResultCode::from_module_description(
                crate::hle::result::ErrorModule::Audio,
                513,
            ));
            return;
        }

        let mut event_guard = svc.rendered_event.lock().unwrap();

        if let Some(ref readable) = *event_guard {
            // Event already created — return an additional handle to it.
            if let Some(handle) = ctx.copy_handle_for_readable_event(Arc::clone(readable)) {
                log::info!(
                    "IAudioRenderer::QuerySystemEvent returning existing event handle={:#x}",
                    handle
                );
                let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_copy_objects(handle);
            } else {
                log::error!("IAudioRenderer::QuerySystemEvent failed to copy handle");
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(RESULT_SUCCESS);
            }
            return;
        }

        let Some((handle, readable_event)) = ctx.create_readable_event(true) else {
            log::error!("IAudioRenderer::QuerySystemEvent failed to create KReadableEvent");
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(RESULT_SUCCESS);
            return;
        };

        log::info!(
            "IAudioRenderer::QuerySystemEvent created event handle={:#x}",
            handle
        );

        *event_guard = Some(readable_event);
        drop(event_guard);

        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(handle);
    }

    fn set_rendering_time_limit_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let limit = crate::hle::service::ipc_helpers::RequestParser::new(ctx).pop_u32();
        Self::as_self(_this)
            .renderer_impl
            .set_rendering_time_limit(limit);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_rendering_time_limit_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(_this);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(svc.renderer_impl.get_rendering_time_limit());
    }

    fn set_voice_drop_parameter_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let value = crate::hle::service::ipc_helpers::RequestParser::new(ctx).pop_f32();
        Self::as_self(_this)
            .renderer_impl
            .set_voice_drop_parameter(value);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_voice_drop_parameter_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(_this);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_f32(svc.renderer_impl.get_voice_drop_parameter());
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
