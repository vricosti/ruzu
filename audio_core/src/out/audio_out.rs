use crate::device::SharedAudioEvent;
use crate::errors::RESULT_BUFFER_COUNT_REACHED;
use crate::out::audio_out_system::{AudioOutBuffer, State, System};
use crate::Result;
use common::ResultCode;
use parking_lot::Mutex;
use std::sync::Arc;

pub struct Out {
    system: Mutex<System>,
    buffer_event: SharedAudioEvent,
    release_session: Arc<dyn Fn(usize) + Send + Sync>,
}

impl Out {
    pub fn new(
        system: System,
        buffer_event: SharedAudioEvent,
        release_session: Arc<dyn Fn(usize) + Send + Sync>,
    ) -> Self {
        Self {
            system: Mutex::new(system),
            buffer_event,
            release_session,
        }
    }

    pub fn free(&self) {
        let session_id = self.system.lock().get_session_id();
        (self.release_session)(session_id);
    }

    pub fn get_system(&self) -> parking_lot::MutexGuard<'_, System> {
        self.system.lock()
    }

    pub fn get_state(&self) -> State {
        self.system.lock().get_state()
    }
    pub fn start_system(&self) -> Result {
        self.system.lock().start()
    }
    pub fn start_session(&self) {
        self.system.lock().start_session();
    }
    pub fn stop_system(&self) -> Result {
        self.system.lock().stop()
    }

    pub fn append_buffer(&self, buffer: AudioOutBuffer, tag: u64) -> Result {
        if self.system.lock().append_buffer(buffer, tag) {
            ResultCode::SUCCESS
        } else {
            RESULT_BUFFER_COUNT_REACHED
        }
    }

    pub fn release_and_register_buffers(&self) {
        let system = self.system.lock();
        if system.get_state() == State::Started {
            system.release_buffers();
            system.register_buffers();
        }
    }

    pub fn flush_audio_out_buffers(&self) -> bool {
        self.system.lock().flush_audio_out_buffers()
    }
    pub fn get_released_buffers(&self, tags: &mut [u64]) -> u32 {
        self.system.lock().get_released_buffers(tags)
    }
    pub fn get_buffer_event(&self) -> SharedAudioEvent {
        self.buffer_event.clone()
    }
    pub fn get_volume(&self) -> f32 {
        self.system.lock().get_volume()
    }
    pub fn set_volume(&self, volume: f32) {
        self.system.lock().set_volume(volume);
    }
    pub fn contains_audio_buffer(&self, tag: u64) -> bool {
        self.system.lock().contains_audio_buffer(tag)
    }
    pub fn get_buffer_count(&self) -> u32 {
        self.system.lock().get_buffer_count()
    }
    pub fn get_played_sample_count(&self) -> u64 {
        self.system.lock().get_played_sample_count()
    }
}
