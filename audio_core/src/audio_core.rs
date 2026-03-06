use crate::adsp::ADSP;
use crate::audio_manager::AudioManager;
use crate::sink::sink::{new_sink_handle, SinkHandle};
use crate::sink::sink_details::create_sink_from_id;
use crate::SharedSystem;
use common::settings::Values;
use std::sync::Arc;

pub struct AudioCore {
    audio_manager: Arc<AudioManager>,
    output_sink: SinkHandle,
    input_sink: SinkHandle,
    adsp: ADSP,
}

impl AudioCore {
    pub fn new(system: SharedSystem, settings: Arc<Values>) -> Self {
        let (output_sink, input_sink) = Self::create_sinks(&settings);
        let adsp = ADSP::new(system, output_sink.clone());
        Self {
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

    fn create_sinks(settings: &Values) -> (SinkHandle, SinkHandle) {
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
