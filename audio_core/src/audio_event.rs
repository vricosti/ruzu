use std::array;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Condvar, Mutex};
use std::time::Duration;

pub struct Event {
    event_lock: Mutex<()>,
    events_signalled: [AtomicBool; 4],
    manager_event: Condvar,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    AudioInManager,
    AudioOutManager,
    FinalOutputRecorderManager,
    Max,
}

impl Event {
    pub fn new() -> Self {
        Self {
            event_lock: Mutex::new(()),
            events_signalled: array::from_fn(|_| AtomicBool::new(false)),
            manager_event: Condvar::new(),
        }
    }

    pub const fn get_manager_index(&self, event_type: Type) -> usize {
        match event_type {
            Type::AudioInManager => 0,
            Type::AudioOutManager => 1,
            Type::FinalOutputRecorderManager => 2,
            Type::Max => 3,
        }
    }

    pub fn set_audio_event(&self, event_type: Type, signalled: bool) {
        self.events_signalled[self.get_manager_index(event_type)]
            .store(signalled, Ordering::SeqCst);
        if signalled {
            self.manager_event.notify_one();
        }
    }

    pub fn check_audio_event_set(&self, event_type: Type) -> bool {
        self.events_signalled[self.get_manager_index(event_type)].load(Ordering::SeqCst)
    }

    pub fn wait(&self, timeout: Duration) -> bool {
        let guard = self.event_lock.lock().expect("audio event mutex poisoned");
        let (_guard, result) = self
            .manager_event
            .wait_timeout_while(guard, timeout, |_| !self.any_events_set())
            .expect("audio event condvar poisoned");
        result.timed_out()
    }

    pub fn clear_events(&self) {
        for signalled in &self.events_signalled {
            signalled.store(false, Ordering::SeqCst);
        }
    }

    fn any_events_set(&self) -> bool {
        self.events_signalled
            .iter()
            .any(|event| event.load(Ordering::SeqCst))
    }
}

impl Default for Event {
    fn default() -> Self {
        Self::new()
    }
}
