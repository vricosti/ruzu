//! Port of zuyu/src/core/hle/service/audio/audio_renderer.h and audio_renderer.cpp
//!
//! IAudioRenderer service.

use crate::core::AudioRendererSessionInterface;
use std::collections::BTreeMap;
use std::sync::{Arc, Mutex, Weak};

use crate::hle::kernel::k_event::KEvent;
use crate::hle::kernel::kernel::KernelCore;
use crate::hle::kernel::k_process::KProcess;
use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
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
    renderer: Mutex<Box<dyn AudioRendererSessionInterface>>,
    owner_process: Weak<Mutex<KProcess>>,
    rendered_event_object_id: u64,
    rendered_readable_event_object_id: u64,
    rendered_event: Arc<Mutex<KEvent>>,
    rendered_readable_event: Arc<Mutex<KReadableEvent>>,
}

impl IAudioRenderer {
    pub fn new(
        renderer: Box<dyn AudioRendererSessionInterface>,
        owner_process: Arc<Mutex<KProcess>>,
        rendered_event_object_id: u64,
        rendered_readable_event_object_id: u64,
        rendered_event: Arc<Mutex<KEvent>>,
        rendered_readable_event: Arc<Mutex<KReadableEvent>>,
    ) -> Self {
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
            renderer: Mutex::new(renderer),
            owner_process: Arc::downgrade(&owner_process),
            rendered_event_object_id,
            rendered_readable_event_object_id,
            rendered_event,
            rendered_readable_event,
        }
    }

    pub(crate) fn create_rendered_event(
        kernel: &KernelCore,
        owner_process: &Arc<Mutex<KProcess>>,
    ) -> (
        u64,
        u64,
        Arc<Mutex<KEvent>>,
        Arc<Mutex<KReadableEvent>>,
    ) {
        let rendered_event_object_id = kernel.create_new_object_id() as u64;
        let rendered_readable_event_object_id = kernel.create_new_object_id() as u64;

        let mut event = KEvent::new();
        let mut readable_event = KReadableEvent::new();

        let owner_process_id = owner_process.lock().unwrap().get_process_id();
        event.initialize(owner_process_id, rendered_readable_event_object_id);
        readable_event.initialize(rendered_event_object_id, rendered_readable_event_object_id);

        let event = Arc::new(Mutex::new(event));
        let readable_event = Arc::new(Mutex::new(readable_event));

        {
            let mut owner = owner_process.lock().unwrap();
            owner.register_event_object(rendered_event_object_id, Arc::clone(&event));
            owner.register_readable_event_object(
                rendered_readable_event_object_id,
                Arc::clone(&readable_event),
            );
        }

        (
            rendered_event_object_id,
            rendered_readable_event_object_id,
            event,
            readable_event,
        )
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    fn get_sample_rate_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(_this);
        log::debug!("IAudioRenderer::GetSampleRate");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(svc.renderer.lock().unwrap().get_sample_rate());
    }

    fn get_sample_count_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(_this);
        log::debug!("IAudioRenderer::GetSampleCount");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(svc.renderer.lock().unwrap().get_sample_count());
    }

    fn get_mix_buffer_count_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(_this);
        log::debug!("IAudioRenderer::GetMixBufferCount");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(svc.renderer.lock().unwrap().get_mix_buffer_count());
    }

    fn get_state_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(_this);
        log::debug!("IAudioRenderer::GetState");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(svc.renderer.lock().unwrap().get_state());
    }

    /// Port of upstream `IAudioRenderer::RequestUpdate` → delegates to RequestUpdateAuto.
    fn request_update_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::info!("IAudioRenderer::RequestUpdate");
        Self::request_update_impl(this, ctx);
    }

    fn request_update_auto_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::info!("IAudioRenderer::RequestUpdateAuto");
        Self::request_update_impl(this, ctx);
    }

    /// Common implementation for RequestUpdate and RequestUpdateAuto.
    fn request_update_impl(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let input = if ctx.can_read_buffer(0) {
            ctx.read_buffer(0)
        } else {
            Vec::new()
        };
        let mut output = vec![0u8; ctx.get_write_buffer_size(0)];
        let mut performance = vec![0u8; ctx.get_write_buffer_size(1)];
        let result = svc
            .renderer
            .lock()
            .unwrap()
            .request_update(&input, &mut performance, &mut output);
        log::info!(
            "IAudioRenderer::RequestUpdate buffers input={} output={} performance={} result=0x{:08X}",
            input.len(),
            output.len(),
            performance.len(),
            result.get_inner_value()
        );
        if std::env::var_os("RUZU_LOG_AUDIO_UPDATE_BYTES").is_some() && !output.is_empty() {
            let preview_words: Vec<u32> = output
                .chunks_exact(4)
                .take(8)
                .map(|chunk| u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]))
                .collect();
            log::info!(
                "IAudioRenderer::RequestUpdate output_preview={:08X?}",
                preview_words
            );
        }
        if result.is_success() {
            if !output.is_empty() {
                ctx.write_buffer(&output, 0);
            }
            if !performance.is_empty() {
                ctx.write_buffer(&performance, 1);
            }
        }
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    fn start_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        log::info!("IAudioRenderer::Start");
        svc.renderer.lock().unwrap().start();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn stop_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        log::debug!("IAudioRenderer::Stop");
        svc.renderer.lock().unwrap().stop();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Port of upstream `IAudioRenderer::QuerySystemEvent`.
    ///
    /// Upstream returns `&rendered_event->GetReadableEvent()` as a copy handle.
    /// We create the KReadableEvent lazily on first call and return its handle.
    /// Upstream creates an unsignaled event owned by the service context.
    fn query_system_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::info!("IAudioRenderer::QuerySystemEvent");
        let svc = Self::as_self(this);
        if !svc.renderer.lock().unwrap().supports_system_event() {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(ResultCode::from_module_description(
                crate::hle::result::ErrorModule::Audio,
                crate::hle::service::audio::errors::RESULT_NOT_SUPPORTED.1,
            ));
            return;
        }

        let _ = svc.rendered_event.lock().unwrap().is_initialized();
        let Some(handle) =
            ctx.copy_handle_for_readable_event(Arc::clone(&svc.rendered_readable_event))
        else {
            log::error!("IAudioRenderer::QuerySystemEvent failed to copy handle");
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(RESULT_SUCCESS);
            return;
        };

        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(handle);
    }

    fn set_rendering_time_limit_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        log::debug!("IAudioRenderer::SetRenderingTimeLimit");
        let mut rp = RequestParser::new(ctx);
        let rendering_time_limit = rp.pop_u32();
        svc.renderer
            .lock()
            .unwrap()
            .set_rendering_time_limit(rendering_time_limit);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_rendering_time_limit_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        log::debug!("IAudioRenderer::GetRenderingTimeLimit");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(svc.renderer.lock().unwrap().get_rendering_time_limit());
    }

    fn set_voice_drop_parameter_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        log::debug!("IAudioRenderer::SetVoiceDropParameter");
        let mut rp = RequestParser::new(ctx);
        let value = rp.pop_f32();
        svc.renderer.lock().unwrap().set_voice_drop_parameter(value);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_voice_drop_parameter_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        log::debug!("IAudioRenderer::GetVoiceDropParameter");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_f32(svc.renderer.lock().unwrap().get_voice_drop_parameter());
    }
}

impl Drop for IAudioRenderer {
    fn drop(&mut self) {
        if let Some(owner_process) = self.owner_process.upgrade() {
            let mut owner = owner_process.lock().unwrap();
            owner.unregister_readable_event_object_by_object_id(
                self.rendered_readable_event_object_id,
            );
            owner.unregister_event_object_by_object_id(self.rendered_event_object_id);
        }
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
