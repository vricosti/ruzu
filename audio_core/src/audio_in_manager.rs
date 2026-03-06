use crate::audio_manager::{AudioManager, BufferEventFunc};
use crate::errors::RESULT_OUT_OF_SESSIONS;
use crate::r#in::audio_in::In;
use crate::renderer::audio_device::AudioDeviceName;
use crate::sink::get_device_list_for_sink;
use crate::{Result, SharedSystem};
use common::settings_enums::AudioEngine;
use common::ResultCode;
use log::{debug, error};
use parking_lot::Mutex;
use std::array;
use std::sync::Arc;

pub const MAX_IN_SESSIONS: usize = 4;

pub struct Manager {
    pub system: SharedSystem,
    pub session_ids: [usize; MAX_IN_SESSIONS],
    pub applet_resource_user_ids: [usize; MAX_IN_SESSIONS],
    pub sessions: Arc<Mutex<[Option<Arc<In>>; MAX_IN_SESSIONS]>>,
    pub num_free_sessions: usize,
    pub next_session_id: usize,
    pub free_session_id: usize,
    pub linked_to_manager: bool,
    pub sessions_started: bool,
    pub audio_manager: Arc<AudioManager>,
    pub mutex: Mutex<()>,
}

impl Manager {
    pub fn new(system: SharedSystem, audio_manager: Arc<AudioManager>) -> Self {
        Self {
            system,
            session_ids: array::from_fn(|i| i),
            applet_resource_user_ids: [0; MAX_IN_SESSIONS],
            sessions: Arc::new(Mutex::new(array::from_fn(|_| None))),
            num_free_sessions: MAX_IN_SESSIONS,
            next_session_id: 0,
            free_session_id: 0,
            linked_to_manager: false,
            sessions_started: false,
            audio_manager,
            mutex: Mutex::new(()),
        }
    }

    pub fn acquire_session_id(&mut self, session_id: &mut usize) -> Result {
        if self.num_free_sessions == 0 {
            error!("audio_core: all 4 AudioIn sessions are in use, cannot create any more");
            return RESULT_OUT_OF_SESSIONS;
        }
        *session_id = self.session_ids[self.next_session_id];
        self.next_session_id = (self.next_session_id + 1) % MAX_IN_SESSIONS;
        self.num_free_sessions -= 1;
        ResultCode::SUCCESS
    }

    pub fn release_session_id(&mut self, session_id: usize) {
        let _lock = self.mutex.lock();
        debug!("audio_core: freeing AudioIn session {session_id}");
        self.session_ids[self.free_session_id] = session_id;
        self.num_free_sessions += 1;
        self.free_session_id = (self.free_session_id + 1) % MAX_IN_SESSIONS;
        self.set_session(session_id, None);
        self.applet_resource_user_ids[session_id] = 0;
    }

    pub fn set_session(&self, session_id: usize, session: Option<Arc<In>>) {
        self.sessions.lock()[session_id] = session;
    }

    pub fn link_to_manager(&mut self) -> Result {
        let _lock = self.mutex.lock();
        if !self.linked_to_manager {
            let sessions = Arc::clone(&self.sessions);
            let callback: BufferEventFunc = Arc::new(move || {
                let sessions = sessions.lock().clone();
                for session in sessions.iter().flatten() {
                    session.release_and_register_buffers();
                }
            });
            let _ = self.audio_manager.set_in_manager(callback);
            self.linked_to_manager = true;
        }
        ResultCode::SUCCESS
    }

    pub fn start(&mut self) {
        let _ = self.link_to_manager();
        if self.sessions_started {
            return;
        }
        let _lock = self.mutex.lock();
        let sessions = self.sessions.lock().clone();
        for session in sessions.iter().flatten() {
            session.start_session();
        }
        self.sessions_started = true;
    }

    pub fn buffer_release_and_register(&self) {
        let _lock = self.mutex.lock();
        let sessions = self.sessions.lock().clone();
        for session in sessions.iter().flatten() {
            session.release_and_register_buffers();
        }
    }

    pub fn get_device_names(&mut self, names: &mut [AudioDeviceName], _filter: bool) -> u32 {
        let _ = self.link_to_manager();
        let input_devices = get_device_list_for_sink(AudioEngine::Null, true);
        if !input_devices.is_empty() && !names.is_empty() {
            names[0] = AudioDeviceName::from_str("Uac");
            return 1;
        }
        0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::audio_event::Type as AudioEventType;
    use crate::common::common::TARGET_SAMPLE_RATE;
    use crate::r#in::audio_in::In;
    use crate::r#in::audio_in_system::{AudioInBuffer, AudioInParameter, System as AudioInSystem};
    use crate::sink::sink::new_sink_handle;
    use crate::sink::NullSink;
    use parking_lot::Mutex;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::thread;
    use std::time::Duration;

    fn make_system() -> SharedSystem {
        Arc::new(Mutex::new(ruzu_core::core::System::new()))
    }

    fn wait_for_event(flag: &AtomicBool) -> bool {
        for _ in 0..100 {
            if flag.load(Ordering::SeqCst) {
                return true;
            }
            thread::sleep(Duration::from_millis(5));
        }
        flag.load(Ordering::SeqCst)
    }

    #[test]
    fn manager_callback_uses_live_session_table() {
        let system = make_system();
        let audio_manager = Arc::new(AudioManager::new());
        let mut manager = Manager::new(system.clone(), audio_manager.clone());
        let buffer_event = Arc::new(AtomicBool::new(false));
        let sink = new_sink_handle(Box::new(NullSink::new("null")));

        assert!(manager.link_to_manager().is_success());

        let mut in_system = AudioInSystem::new(system, sink, buffer_event.clone(), 0);
        in_system.set_audio_manager(Some(audio_manager.clone()));
        assert!(in_system
            .initialize(
                String::new(),
                &AudioInParameter {
                    sample_rate: TARGET_SAMPLE_RATE as i32,
                    channel_count: 2,
                    reserved: 0,
                },
                0,
            )
            .is_success());
        assert!(in_system.start().is_success());
        assert!(in_system.append_buffer(
            AudioInBuffer {
                next: 0,
                samples: 0,
                capacity: 8,
                size: 8,
                offset: 0,
            },
            1,
        ));

        manager.set_session(
            0,
            Some(Arc::new(In::new(
                in_system,
                buffer_event.clone(),
                Arc::new(|_| {}),
            ))),
        );

        audio_manager.set_event(AudioEventType::AudioInManager, true);
        audio_manager.dispatch_events_for_test(false);

        assert!(wait_for_event(&buffer_event));
    }

    #[test]
    fn start_links_manager_automatically() {
        let system = make_system();
        let audio_manager = Arc::new(AudioManager::new());
        let mut manager = Manager::new(system.clone(), audio_manager.clone());
        let buffer_event = Arc::new(AtomicBool::new(false));
        let sink = new_sink_handle(Box::new(NullSink::new("null")));

        let mut in_system = AudioInSystem::new(system, sink, buffer_event.clone(), 0);
        in_system.set_audio_manager(Some(audio_manager.clone()));
        assert!(in_system
            .initialize(
                String::new(),
                &AudioInParameter {
                    sample_rate: TARGET_SAMPLE_RATE as i32,
                    channel_count: 2,
                    reserved: 0,
                },
                0,
            )
            .is_success());

        let session = Arc::new(In::new(in_system, buffer_event.clone(), Arc::new(|_| {})));
        manager.set_session(0, Some(session.clone()));
        assert!(session.start_system().is_success());
        assert!(session
            .append_buffer(
                AudioInBuffer {
                    next: 0,
                    samples: 0,
                    capacity: 8,
                    size: 8,
                    offset: 0,
                },
                1,
            )
            .is_success());

        manager.start();

        assert!(manager.linked_to_manager);
        assert!(manager.sessions_started);
        audio_manager.set_event(AudioEventType::AudioInManager, true);
        audio_manager.dispatch_events_for_test(false);
    }
}
