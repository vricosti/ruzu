//! Port of zuyu/src/core/hle/kernel/k_event.h / k_event.cpp
//! Status: Partial (owner/process wiring ported)
//! Derniere synchro: 2026-03-14
//!
//! KEvent: a kernel event object containing a readable event.

use std::sync::{Arc, Mutex};

use super::k_process::KProcess;
use super::k_scheduler::KScheduler;
use crate::hle::result::RESULT_SUCCESS;

/// The kernel event object.
/// Matches upstream `KEvent` class (k_event.h).
pub struct KEvent {
    /// Embedded readable event object id registered in the owner process.
    pub readable_event_id: u64,
    /// Owner process id.
    pub owner_process_id: Option<u64>,
    pub initialized: bool,
    pub readable_event_destroyed: bool,
}

impl KEvent {
    pub fn new() -> Self {
        Self {
            readable_event_id: 0,
            owner_process_id: None,
            initialized: false,
            readable_event_destroyed: false,
        }
    }

    /// Initialize the event with an owner process.
    pub fn initialize(&mut self, owner_process_id: u64, readable_event_id: u64) {
        self.owner_process_id = Some(owner_process_id);
        self.readable_event_id = readable_event_id;
        self.initialized = true;
    }

    pub fn is_initialized(&self) -> bool {
        self.initialized
    }

    pub fn get_owner_process_id(&self) -> Option<u64> {
        self.owner_process_id
    }

    /// Signal the event.
    /// Matches upstream `KEvent::Signal`.
    pub fn signal(&mut self, process: &mut KProcess, scheduler: &Arc<Mutex<KScheduler>>) -> u32 {
        if self.readable_event_destroyed {
            return RESULT_SUCCESS.get_inner_value();
        }

        let Some(readable_event) = process.get_readable_event_by_object_id(self.readable_event_id) else {
            return RESULT_SUCCESS.get_inner_value();
        };
        let result = readable_event.lock().unwrap().signal(process, scheduler);
        result
    }

    /// Clear the event.
    /// Matches upstream `KEvent::Clear`.
    pub fn clear(&mut self, process: &KProcess) -> u32 {
        if self.readable_event_destroyed {
            return RESULT_SUCCESS.get_inner_value();
        }

        let Some(readable_event) = process.get_readable_event_by_object_id(self.readable_event_id) else {
            return RESULT_SUCCESS.get_inner_value();
        };
        let result = readable_event.lock().unwrap().clear();
        result
    }

    /// Called when the readable event is destroyed.
    pub fn on_readable_event_destroyed(&mut self) {
        self.readable_event_destroyed = true;
    }

    /// Finalize the event.
    pub fn finalize(&mut self) {
        self.initialized = false;
    }
}

impl Default for KEvent {
    fn default() -> Self {
        Self::new()
    }
}
