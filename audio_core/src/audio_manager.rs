use crate::audio_event::{Event, Type};
use crate::errors::RESULT_OPERATION_FAILED;
use crate::Result;
use common::ResultCode;
use std::array;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};
use std::time::Duration;

pub type BufferEventFunc = Arc<dyn Fn() + Send + Sync + 'static>;

pub struct AudioManager {
    running: Arc<AtomicBool>,
    needs_update: Arc<AtomicBool>,
    events: Arc<Event>,
    buffer_events: Arc<Mutex<[Option<BufferEventFunc>; 3]>>,
    thread: Option<JoinHandle<()>>,
}

impl AudioManager {
    pub fn new() -> Self {
        let running = Arc::new(AtomicBool::new(true));
        let needs_update = Arc::new(AtomicBool::new(false));
        let events = Arc::new(Event::new());
        let buffer_events = Arc::new(Mutex::new(array::from_fn(|_| None::<BufferEventFunc>)));

        let thread_running = Arc::clone(&running);
        let thread_events = Arc::clone(&events);
        let thread_callbacks = Arc::clone(&buffer_events);
        let thread = thread::Builder::new()
            .name("AudioManager".to_string())
            .spawn(move || {
                thread_events.clear_events();
                while thread_running.load(Ordering::SeqCst) {
                    let timed_out = thread_events.wait(Duration::from_secs(2));
                    if thread_events.check_audio_event_set(Type::Max) {
                        break;
                    }
                    Self::dispatch_ready_callbacks(&thread_events, &thread_callbacks, timed_out);
                }
            })
            .expect("failed to spawn audio manager thread");

        Self {
            running,
            needs_update,
            events,
            buffer_events,
            thread: Some(thread),
        }
    }

    pub fn shutdown(&mut self) {
        self.running.store(false, Ordering::SeqCst);
        self.events.set_audio_event(Type::Max, true);
        if let Some(thread) = self.thread.take() {
            let _ = thread.join();
        }
    }

    pub fn set_out_manager(&self, buffer_func: BufferEventFunc) -> Result {
        self.set_manager_callback(Type::AudioOutManager, buffer_func)
    }

    pub fn set_in_manager(&self, buffer_func: BufferEventFunc) -> Result {
        self.set_manager_callback(Type::AudioInManager, buffer_func)
    }

    pub fn set_event(&self, event_type: Type, signalled: bool) {
        self.events.set_audio_event(event_type, signalled);
    }

    #[cfg(test)]
    pub(crate) fn dispatch_events_for_test(&self, timed_out: bool) {
        Self::dispatch_ready_callbacks(&self.events, &self.buffer_events, timed_out);
    }

    #[cfg(test)]
    #[allow(dead_code)]
    pub(crate) fn is_event_set(&self, event_type: Type) -> bool {
        self.events.check_audio_event_set(event_type)
    }

    fn set_manager_callback(&self, event_type: Type, buffer_func: BufferEventFunc) -> Result {
        if !self.running.load(Ordering::SeqCst) {
            return RESULT_OPERATION_FAILED;
        }

        let mut callbacks = self
            .buffer_events
            .lock()
            .expect("audio manager callbacks poisoned");
        let index = self.events.get_manager_index(event_type);
        if callbacks[index].is_none() {
            callbacks[index] = Some(buffer_func);
            self.needs_update.store(true, Ordering::SeqCst);
            self.events.set_audio_event(event_type, true);
        }
        ResultCode::SUCCESS
    }

    fn dispatch_ready_callbacks(
        events: &Event,
        buffer_events: &Mutex<[Option<BufferEventFunc>; 3]>,
        timed_out: bool,
    ) {
        let callbacks = buffer_events
            .lock()
            .expect("audio manager callbacks poisoned");
        for index in 0..callbacks.len() {
            let event_type = match index {
                0 => Type::AudioInManager,
                1 => Type::AudioOutManager,
                _ => Type::FinalOutputRecorderManager,
            };

            if (events.check_audio_event_set(event_type) || timed_out) && callbacks[index].is_some()
            {
                callbacks[index].as_ref().expect("callback checked above")();
            }
            events.set_audio_event(event_type, false);
        }
    }
}

impl Default for AudioManager {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for AudioManager {
    fn drop(&mut self) {
        self.shutdown();
    }
}
