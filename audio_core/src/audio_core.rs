use crate::adsp::ADSP;
use crate::audio_render_manager::Manager as AudioRenderManager;
use crate::common::audio_renderer_parameter::AudioRendererParameterInternal;
use crate::audio_manager::AudioManager;
use crate::renderer::audio_device::AudioDevice;
use crate::renderer::{Renderer, System as RendererSystem};
use crate::sink::sink::{new_sink_handle, SinkHandle};
use crate::sink::sink_details::create_sink_from_id;
use crate::SharedSystem;
use parking_lot::Mutex;
use std::sync::Mutex as StdMutex;
use std::sync::Arc;

pub struct AudioCore {
    system: SharedSystem,
    audio_manager: Arc<AudioManager>,
    audio_render_manager: Arc<Mutex<AudioRenderManager>>,
    output_sink: SinkHandle,
    input_sink: SinkHandle,
    adsp: ADSP,
}

impl AudioCore {
    pub fn new(system: SharedSystem) -> Self {
        let (output_sink, input_sink) = Self::create_sinks();
        let adsp = ADSP::new(system.clone(), output_sink.clone());
        let audio_render_manager = Arc::new(Mutex::new(AudioRenderManager::new(
            system.clone(),
            adsp.audio_renderer(),
        )));
        Self {
            system,
            audio_manager: Arc::new(AudioManager::new()),
            audio_render_manager,
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

struct AudioRendererSession {
    renderer: Mutex<Renderer>,
}

impl AudioRendererSession {
    fn new(renderer: Renderer) -> Self {
        Self {
            renderer: Mutex::new(renderer),
        }
    }
}

impl ruzu_core::core::AudioRendererSessionInterface for AudioRendererSession {
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

    fn request_update(
        &self,
        input: &[u8],
        performance: &mut [u8],
        output: &mut [u8],
    ) -> ruzu_core::hle::result::ResultCode {
        let result = self.renderer.lock().request_update(input, performance, output);
        ruzu_core::hle::result::ResultCode::new(result.raw())
    }

    fn start(&self) {
        self.renderer.lock().start();
    }

    fn stop(&self) {
        self.renderer.lock().stop();
    }

    fn supports_system_event(&self) -> bool {
        self.renderer.lock().get_system().lock().get_execution_mode()
            != crate::common::audio_renderer_parameter::ExecutionMode::Manual
    }

    fn set_process(&self, process: *mut ruzu_core::hle::kernel::k_process::KProcess) {
        self.renderer.lock().set_process(process);
    }

    fn set_rendering_time_limit(&self, rendering_time_limit: u32) {
        self.renderer
            .lock()
            .get_system()
            .lock()
            .set_rendering_time_limit(rendering_time_limit);
    }

    fn get_rendering_time_limit(&self) -> u32 {
        self.renderer
            .lock()
            .get_system()
            .lock()
            .get_rendering_time_limit()
    }

    fn set_voice_drop_parameter(&self, voice_drop_parameter: f32) {
        self.renderer
            .lock()
            .get_system()
            .lock()
            .set_voice_drop_parameter(voice_drop_parameter);
    }

    fn get_voice_drop_parameter(&self) -> f32 {
        self.renderer
            .lock()
            .get_system()
            .lock()
            .get_voice_drop_parameter()
    }
}

impl ruzu_core::core::AudioCoreInterface for AudioCore {
    fn get_audio_renderer_work_buffer_size(&self, params: &[u8; 0x34]) -> Option<u64> {
        let parsed = unsafe {
            core::ptr::read_unaligned(params.as_ptr() as *const AudioRendererParameterInternal)
        };
        Some(RendererSystem::get_work_buffer_size(&parsed))
    }

    fn list_audio_device_name(
        &self,
        applet_resource_user_id: u64,
        revision: u32,
        out_names: &mut [[u8; 0x100]],
    ) -> u32 {
        let device = AudioDevice::new(self.get_output_sink(), applet_resource_user_id, revision);
        let mut names = vec![crate::renderer::audio_device::AudioDeviceName::new(); out_names.len()];
        let count = device.list_audio_device_name(&mut names) as usize;
        for (dst, src) in out_names.iter_mut().zip(names.iter()).take(count) {
            *dst = src.name;
        }
        count as u32
    }

    fn list_audio_output_device_name(
        &self,
        applet_resource_user_id: u64,
        revision: u32,
        out_names: &mut [[u8; 0x100]],
    ) -> u32 {
        let device = AudioDevice::new(self.get_output_sink(), applet_resource_user_id, revision);
        let mut names = vec![crate::renderer::audio_device::AudioDeviceName::new(); out_names.len()];
        let count = device.list_audio_output_device_name(&mut names) as usize;
        for (dst, src) in out_names.iter_mut().zip(names.iter()).take(count) {
            *dst = src.name;
        }
        count as u32
    }

    fn set_audio_device_volume(
        &self,
        applet_resource_user_id: u64,
        revision: u32,
        volume: f32,
    ) {
        let device = AudioDevice::new(self.get_output_sink(), applet_resource_user_id, revision);
        device.set_device_volumes(volume);
    }

    fn get_audio_device_volume(
        &self,
        applet_resource_user_id: u64,
        revision: u32,
        name: &str,
    ) -> f32 {
        let device = AudioDevice::new(self.get_output_sink(), applet_resource_user_id, revision);
        device.get_device_volume(name)
    }

    fn get_audio_output_system_channels(&self) -> u32 {
        self.get_output_sink().lock().get_system_channels()
    }

    fn open_audio_renderer(
        &self,
        params: &[u8; 0x34],
        transfer_memory: *mut ruzu_core::hle::kernel::k_transfer_memory::KTransferMemory,
        transfer_memory_size: u64,
        process: *mut ruzu_core::hle::kernel::k_process::KProcess,
        applet_resource_user_id: u64,
        rendered_event: Arc<StdMutex<ruzu_core::hle::kernel::k_event::KEvent>>,
    ) -> std::result::Result<
        Box<dyn ruzu_core::core::AudioRendererSessionInterface>,
        ruzu_core::hle::result::ResultCode,
    >
    {
        let parsed = unsafe {
            core::ptr::read_unaligned(params.as_ptr() as *const AudioRendererParameterInternal)
        };
        let session_id = {
            let mut manager = self.audio_render_manager.lock();
            manager.get_session_id()
        };
        if session_id < 0 {
            return Err(ruzu_core::hle::result::ResultCode::new(
                crate::errors::RESULT_OUT_OF_SESSIONS.raw(),
            ));
        }

        let renderer_system = Arc::new(Mutex::new(RendererSystem::new(
            self.system.clone(),
            self.adsp.audio_renderer(),
            rendered_event,
        )));
        let mut renderer = Renderer::new(self.audio_render_manager.clone(), renderer_system);
        let result = renderer.initialize(
            &parsed,
            transfer_memory,
            transfer_memory_size,
            process,
            applet_resource_user_id,
            session_id,
        );
        if result.is_error() {
            self.audio_render_manager.lock().release_session_id(session_id);
            return Err(ruzu_core::hle::result::ResultCode::new(result.raw()));
        }

        Ok(Box::new(AudioRendererSession::new(renderer)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::audio_renderer_parameter::ExecutionMode;
    use crate::common::feature_support::CURRENT_REVISION;
    use ruzu_core::core::AudioCoreInterface;
    use std::sync::Arc;

    fn make_audio_core() -> AudioCore {
        let system: SharedSystem = Arc::new(parking_lot::Mutex::new(ruzu_core::core::System::new()));
        AudioCore::new(system)
    }

    #[test]
    fn audio_core_bridge_reports_renderer_work_buffer_size() {
        let audio_core = make_audio_core();
        let params = AudioRendererParameterInternal {
            sample_rate: 48_000,
            sample_count: 240,
            mixes: 1,
            sub_mixes: 0,
            voices: 24,
            sinks: 1,
            effects: 0,
            perf_frames: 0,
            voice_drop_enabled: 0,
            unk_21: 0,
            rendering_device: 0,
            execution_mode: ExecutionMode::Auto,
            splitter_infos: 0,
            splitter_destinations: 0,
            external_context_size: 0,
            revision: CURRENT_REVISION,
        };

        let raw = unsafe {
            *(core::ptr::addr_of!(params) as *const [u8; core::mem::size_of::<AudioRendererParameterInternal>()])
        };

        assert_eq!(
            audio_core.get_audio_renderer_work_buffer_size(&raw),
            Some(RendererSystem::get_work_buffer_size(&params))
        );
    }

    #[test]
    fn audio_core_bridge_opens_renderer_session_with_real_backend() {
        let audio_core = make_audio_core();
        let params = AudioRendererParameterInternal {
            sample_rate: 48_000,
            sample_count: 240,
            mixes: 1,
            sub_mixes: 0,
            voices: 24,
            sinks: 1,
            effects: 0,
            perf_frames: 0,
            voice_drop_enabled: 0,
            unk_21: 0,
            rendering_device: 0,
            execution_mode: ExecutionMode::Auto,
            splitter_infos: 0,
            splitter_destinations: 0,
            external_context_size: 0,
            revision: CURRENT_REVISION,
        };
        let raw = unsafe {
            *(core::ptr::addr_of!(params)
                as *const [u8; core::mem::size_of::<AudioRendererParameterInternal>()])
        };

        let process = Box::into_raw(Box::new(ruzu_core::hle::kernel::k_process::KProcess::new()));
        let session = audio_core
            .open_audio_renderer(
                &raw,
                RendererSystem::get_work_buffer_size(&params),
                process,
                1,
                Arc::new(StdMutex::new(
                    ruzu_core::hle::kernel::k_event::KEvent::new(),
                )),
            )
            .expect("audio renderer session should initialize");

        assert_eq!(session.get_sample_rate(), 48_000);
        assert_eq!(session.get_sample_count(), 240);
        assert_eq!(session.get_mix_buffer_count(), 1);
        assert_eq!(session.get_state(), 1);
        assert!(session.supports_system_event());
        drop(session);
        unsafe {
            drop(Box::from_raw(process));
        }
    }
}
