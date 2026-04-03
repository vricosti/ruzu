use crate::common::audio_renderer_parameter::{AudioRendererParameterInternal, ExecutionMode};
use crate::errors::RESULT_OUT_OF_SESSIONS;
use crate::renderer::{Manager, System};
use crate::Result;
use common::ResultCode;
use parking_lot::Mutex;
use ruzu_core::hle::kernel::k_process::KProcess;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;

pub struct Renderer {
    manager: Arc<Mutex<Manager>>,
    initialized: bool,
    system_registered: bool,
    system: Arc<Mutex<System>>,
}

impl Renderer {
    pub fn new(manager: Arc<Mutex<Manager>>, system: Arc<Mutex<System>>) -> Self {
        Self {
            manager,
            initialized: false,
            system_registered: false,
            system,
        }
    }

    pub fn initialize(
        &mut self,
        params: &AudioRendererParameterInternal,
        transfer_memory_size: u64,
        process_handle: *mut KProcess,
        applet_resource_user_id: u64,
        session_id: i32,
    ) -> Result {
        if params.execution_mode == ExecutionMode::Auto {
            if !self.manager.lock().add_system(self.system.clone()) {
                return RESULT_OUT_OF_SESSIONS;
            }
            self.system_registered = true;
        }

        let result = self.system.lock().initialize(
            params,
            transfer_memory_size,
            process_handle,
            applet_resource_user_id,
            session_id,
        );
        if result.is_success() {
            self.initialized = true;
            return result;
        }

        if self.system_registered {
            self.manager.lock().remove_system(&self.system);
            self.system_registered = false;
        }
        result
    }

    pub fn finalize(&mut self) {
        if !self.initialized {
            return;
        }

        let session_id = self.system.lock().get_session_id() as i32;
        self.system.lock().finalize();
        if self.system_registered {
            self.manager.lock().remove_system(&self.system);
            self.system_registered = false;
        }
        self.manager.lock().release_session_id(session_id);
        self.initialized = false;
    }

    pub fn get_system(&self) -> Arc<Mutex<System>> {
        self.system.clone()
    }

    pub fn get_rendered_event(&self) -> Arc<AtomicBool> {
        self.system.lock().get_rendered_event()
    }

    pub fn set_process(&self, process: *mut KProcess) {
        self.system.lock().set_process(process);
    }

    pub fn get_process(&self) -> *mut KProcess {
        self.system.lock().get_process()
    }

    pub fn start(&self) {
        self.system.lock().start();
    }
    pub fn stop(&self) {
        self.system.lock().stop();
    }

    pub fn request_update(
        &self,
        input: &[u8],
        performance: &mut [u8],
        output: &mut [u8],
    ) -> Result {
        self.system.lock().update(input, performance, output)
    }

    pub fn is_initialized(&self) -> bool {
        self.initialized
    }
}

impl Drop for Renderer {
    fn drop(&mut self) {
        if self.initialized {
            self.finalize();
        }
        let _ = ResultCode::SUCCESS;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::adsp::apps::audio_renderer::AudioRenderer;
    use crate::common::feature_support::CURRENT_REVISION;
    use crate::sink::null_sink::NullSink;
    use crate::sink::sink::new_sink_handle;
    use parking_lot::Mutex;
    use std::sync::atomic::Ordering;
    use std::sync::Arc;

    fn make_shared_system() -> crate::SharedSystem {
        Arc::new(Mutex::new(ruzu_core::core::System::new()))
    }

    fn make_renderer_handle(core: crate::SharedSystem) -> crate::adsp::adsp::AudioRendererHandle {
        Arc::new(Mutex::new(AudioRenderer::new(
            core,
            new_sink_handle(Box::new(NullSink::new("test"))),
        )))
    }

    fn make_params() -> AudioRendererParameterInternal {
        AudioRendererParameterInternal {
            sample_rate: 48_000,
            sample_count: 160,
            mixes: 1,
            sub_mixes: 0,
            voices: 0,
            sinks: 0,
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
        }
    }

    #[test]
    fn failed_initialize_does_not_leave_renderer_initialized() {
        let core = make_shared_system();
        let audio_renderer = make_renderer_handle(core.clone());
        let manager = Arc::new(Mutex::new(Manager::new(
            core.clone(),
            audio_renderer.clone(),
        )));
        let system = Arc::new(Mutex::new(System::new(core, audio_renderer)));
        let mut renderer = Renderer::new(manager.clone(), system);
        let params = make_params();

        let result = renderer.initialize(&params, 0, std::ptr::dangling_mut(), 1, 0);

        assert!(result.is_error());
        assert!(!renderer.is_initialized());
        assert_eq!(manager.lock().get_session_count(), 0);
    }

    #[test]
    fn stop_uses_system_auto_mode_wait_path() {
        use std::sync::atomic::AtomicBool;
        use std::time::Duration;

        let core = make_shared_system();
        let audio_renderer = make_renderer_handle(core.clone());
        let manager = Arc::new(Mutex::new(Manager::new(
            core.clone(),
            audio_renderer.clone(),
        )));
        let system = Arc::new(Mutex::new(System::new(core, audio_renderer)));
        let mut renderer = Renderer::new(manager, system.clone());
        let params = make_params();
        let transfer_size = System::get_work_buffer_size(&params);

        assert_eq!(
            renderer.initialize(&params, transfer_size, std::ptr::dangling_mut(), 1, 0),
            ResultCode::SUCCESS
        );
        renderer.start();

        let terminate_event = system.lock().get_terminate_event();
        let stop_finished = Arc::new(AtomicBool::new(false));
        let stop_finished_thread = stop_finished.clone();
        let stop_thread = std::thread::spawn(move || {
            renderer.stop();
            stop_finished_thread.store(true, Ordering::SeqCst);
        });

        std::thread::sleep(Duration::from_millis(20));
        assert!(!stop_finished.load(Ordering::SeqCst));

        terminate_event.signal();
        stop_thread.join().unwrap();

        assert!(stop_finished.load(Ordering::SeqCst));
    }
}
