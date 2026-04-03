use crate::adsp::ADSP;
use crate::audio_render_manager::Manager as AudioRendererManager;
use crate::audio_manager::AudioManager;
use crate::common::audio_renderer_parameter::{
    AudioRendererParameterInternal as BackendAudioRendererParameterInternal,
    ExecutionMode as BackendExecutionMode,
};
use crate::renderer::{Renderer as BackendRenderer, System as BackendRendererSystem};
use crate::sink::sink::{new_sink_handle, SinkHandle};
use crate::sink::sink_details::create_sink_from_id;
use crate::SharedSystem;
use parking_lot::Mutex;
use ruzu_core::hle::kernel::k_process::KProcess;
use std::sync::Arc;

pub struct AudioCore {
    system: SharedSystem,
    audio_manager: Arc<AudioManager>,
    output_sink: SinkHandle,
    input_sink: SinkHandle,
    adsp: ADSP,
}

impl AudioCore {
    pub fn new(system: SharedSystem) -> Self {
        let (output_sink, input_sink) = Self::create_sinks();
        let adsp = ADSP::new(system.clone(), output_sink.clone());
        Self {
            system,
            audio_manager: Arc::new(AudioManager::new()),
            output_sink,
            input_sink,
            adsp,
        }
    }

    pub fn shutdown(&mut self) {
        if let Some(manager) = Arc::get_mut(&mut self.audio_manager) {
            manager.shutdown();
        }
    }

    pub fn get_audio_manager(&self) -> Arc<AudioManager> {
        self.audio_manager.clone()
    }

    pub fn get_output_sink(&self) -> SinkHandle {
        self.output_sink.clone()
    }

    pub fn get_input_sink(&self) -> SinkHandle {
        self.input_sink.clone()
    }

    pub fn adsp(&self) -> &ADSP {
        &self.adsp
    }

    pub fn create_audio_renderer_manager(&self) -> Arc<Mutex<AudioRendererManager>> {
        Arc::new(Mutex::new(AudioRendererManager::new(
            self.system.clone(),
            self.adsp.audio_renderer(),
        )))
    }

    fn create_sinks() -> (SinkHandle, SinkHandle) {
        let settings = common::settings::values();
        let sink_id = *settings.sink_id.get_value();
        let output_id = settings.audio_output_device_id.get_value().clone();
        let input_id = settings.audio_input_device_id.get_value().clone();
        (
            new_sink_handle(create_sink_from_id(sink_id, &output_id)),
            new_sink_handle(create_sink_from_id(sink_id, &input_id)),
        )
    }
}

impl Drop for AudioCore {
    fn drop(&mut self) {
        self.shutdown();
    }
}

struct AudioRendererManagerHandleImpl {
    manager: Arc<Mutex<AudioRendererManager>>,
    system: SharedSystem,
    audio_renderer: crate::adsp::adsp::AudioRendererHandle,
}

struct AudioRendererSessionHandleImpl {
    renderer: Mutex<BackendRenderer>,
}

fn convert_execution_mode(
    mode: ruzu_core::audio_core::ExecutionMode,
) -> BackendExecutionMode {
    match mode {
        ruzu_core::audio_core::ExecutionMode::Auto => BackendExecutionMode::Auto,
        ruzu_core::audio_core::ExecutionMode::Manual => BackendExecutionMode::Manual,
    }
}

fn convert_params(
    params: &ruzu_core::audio_core::AudioRendererParameterInternal,
) -> BackendAudioRendererParameterInternal {
    BackendAudioRendererParameterInternal {
        sample_rate: params.sample_rate,
        sample_count: params.sample_count,
        mixes: params.mixes,
        sub_mixes: params.sub_mixes,
        voices: params.voices,
        sinks: params.sinks,
        effects: params.effects,
        perf_frames: params.perf_frames,
        voice_drop_enabled: params.voice_drop_enabled,
        unk_21: params.unk_21,
        rendering_device: params.rendering_device,
        execution_mode: convert_execution_mode(params.execution_mode),
        splitter_infos: params.splitter_infos,
        splitter_destinations: params.splitter_destinations,
        external_context_size: params.external_context_size,
        revision: params.revision,
    }
}

impl ruzu_core::audio_core::AudioRendererSessionHandle for AudioRendererSessionHandleImpl {
    fn get_sample_rate(&self) -> u32 {
        self.renderer.lock().get_system().lock().get_sample_rate()
    }

    fn get_sample_count(&self) -> u32 {
        self.renderer.lock().get_system().lock().get_sample_count()
    }

    fn get_mix_buffer_count(&self) -> u32 {
        self.renderer.lock().get_system().lock().get_mix_buffer_count()
    }

    fn get_state(&self) -> u32 {
        u32::from(!self.renderer.lock().get_system().lock().is_active())
    }

    fn request_update(&self, input: &[u8], performance: &mut [u8], output: &mut [u8]) -> u32 {
        self.renderer.lock().request_update(input, performance, output).raw()
    }

    fn start(&self) {
        self.renderer.lock().start();
    }

    fn stop(&self) {
        self.renderer.lock().stop();
    }

    fn get_rendering_time_limit(&self) -> u32 {
        self.renderer.lock().get_system().lock().get_rendering_time_limit()
    }

    fn set_rendering_time_limit(&self, limit: u32) {
        self.renderer
            .lock()
            .get_system()
            .lock()
            .set_rendering_time_limit(limit);
    }

    fn get_voice_drop_parameter(&self) -> f32 {
        self.renderer.lock().get_system().lock().get_voice_drop_parameter()
    }

    fn set_voice_drop_parameter(&self, voice_drop_parameter: f32) {
        self.renderer
            .lock()
            .get_system()
            .lock()
            .set_voice_drop_parameter(voice_drop_parameter);
    }

    fn execution_mode(&self) -> ruzu_core::audio_core::ExecutionMode {
        match self.renderer.lock().get_system().lock().get_execution_mode() {
            BackendExecutionMode::Auto => ruzu_core::audio_core::ExecutionMode::Auto,
            BackendExecutionMode::Manual => ruzu_core::audio_core::ExecutionMode::Manual,
        }
    }
}

impl ruzu_core::audio_core::AudioRendererManagerHandle for AudioRendererManagerHandleImpl {
    fn get_work_buffer_size(
        &self,
        params: &ruzu_core::audio_core::AudioRendererParameterInternal,
    ) -> Result<u64, u32> {
        let backend = convert_params(params);
        let mut out_size = 0u64;
        let result = self.manager.lock().get_work_buffer_size(&backend, &mut out_size);
        if result.is_success() {
            Ok(out_size)
        } else {
            Err(result.raw())
        }
    }

    fn open_audio_renderer(
        &self,
        params: &ruzu_core::audio_core::AudioRendererParameterInternal,
        transfer_memory_size: u64,
        process_handle: *mut KProcess,
        applet_resource_user_id: u64,
    ) -> Result<Arc<dyn ruzu_core::audio_core::AudioRendererSessionHandle>, u32> {
        let backend = convert_params(params);
        let session_id = self.manager.lock().get_session_id();
        if session_id == -1 {
            return Err(crate::errors::RESULT_OUT_OF_SESSIONS.raw());
        }

        let system = Arc::new(Mutex::new(BackendRendererSystem::new(
            self.system.clone(),
            self.audio_renderer.clone(),
        )));
        let mut renderer = BackendRenderer::new(self.manager.clone(), system);
        let result = renderer.initialize(
            &backend,
            transfer_memory_size,
            process_handle,
            applet_resource_user_id,
            session_id,
        );
        if result.is_error() {
            self.manager.lock().release_session_id(session_id);
            return Err(result.raw());
        }

        Ok(Arc::new(AudioRendererSessionHandleImpl {
            renderer: Mutex::new(renderer),
        }))
    }
}

impl ruzu_core::audio_core::AudioCoreInterface for AudioCore {
    fn as_any(&self) -> &(dyn std::any::Any + Send) {
        self
    }

    fn create_audio_renderer_manager_handle(
        &self,
    ) -> Arc<dyn ruzu_core::audio_core::AudioRendererManagerHandle> {
        Arc::new(AudioRendererManagerHandleImpl {
            manager: self.create_audio_renderer_manager(),
            system: self.system.clone(),
            audio_renderer: self.adsp.audio_renderer(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::feature_support::CURRENT_REVISION;
    use ruzu_core::audio_core::AudioCoreInterface;

    fn make_params() -> ruzu_core::audio_core::AudioRendererParameterInternal {
        ruzu_core::audio_core::AudioRendererParameterInternal {
            sample_rate: 48_000,
            sample_count: 240,
            mixes: 1,
            sub_mixes: 0,
            voices: 0,
            sinks: 0,
            effects: 0,
            perf_frames: 0,
            voice_drop_enabled: 0,
            unk_21: 0,
            rendering_device: 0,
            execution_mode: ruzu_core::audio_core::ExecutionMode::Auto,
            splitter_infos: 0,
            splitter_destinations: 0,
            external_context_size: 0,
            revision: CURRENT_REVISION,
        }
    }

    #[test]
    fn manager_handle_open_audio_renderer_returns_error_without_deadlock() {
        let system = Arc::new(Mutex::new(ruzu_core::core::System::new()));
        let audio_core = AudioCore::new(system);
        let handle = audio_core.create_audio_renderer_manager_handle();
        let result = handle.open_audio_renderer(&make_params(), 0, std::ptr::dangling_mut(), 0x51);
        assert!(result.is_err());
    }
}
