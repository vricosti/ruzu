use crate::adsp::adsp::AudioRendererHandle;
use crate::common::audio_renderer_parameter::AudioRendererParameterInternal;
use crate::common::common::MAX_RENDERER_SESSIONS;
use crate::common::feature_support::check_valid_revision;
use crate::errors::RESULT_INVALID_REVISION;
use crate::renderer::{System, SystemManager};
use crate::{Result, SharedSystem};
use common::ResultCode;
use parking_lot::Mutex;
use std::sync::Arc;

pub struct Manager {
    system: SharedSystem,
    session_ids: [i32; MAX_RENDERER_SESSIONS],
    session_count: u32,
    session_lock: Mutex<()>,
    system_manager: SystemManager,
}

impl Manager {
    pub fn new(system: SharedSystem, audio_renderer: AudioRendererHandle) -> Self {
        Self {
            system: system.clone(),
            session_ids: [0, 1],
            session_count: 0,
            session_lock: Mutex::new(()),
            system_manager: SystemManager::new(system, audio_renderer),
        }
    }

    pub fn stop(&mut self) {
        self.system_manager.stop();
    }

    pub fn get_system_manager(&mut self) -> &mut SystemManager {
        &mut self.system_manager
    }

    pub fn get_work_buffer_size(
        &self,
        params: &AudioRendererParameterInternal,
        out_count: &mut u64,
    ) -> Result {
        if !check_valid_revision(params.revision) {
            return RESULT_INVALID_REVISION;
        }
        *out_count = System::get_work_buffer_size(params);
        ResultCode::SUCCESS
    }

    pub fn get_session_id(&mut self) -> i32 {
        let _lock = self.session_lock.lock();
        let index = self.session_count as usize;
        if index >= self.session_ids.len() {
            return -1;
        }
        let session_id = self.session_ids[index];
        self.session_ids[index] = -1;
        self.session_count += 1;
        session_id
    }

    pub fn release_session_id(&mut self, session_id: i32) {
        let _lock = self.session_lock.lock();
        self.session_count = self.session_count.saturating_sub(1);
        self.session_ids[self.session_count as usize] = session_id;
    }

    pub fn get_session_count(&self) -> u32 {
        let _lock = self.session_lock.lock();
        self.session_count
    }

    pub fn add_system(&mut self, system: Arc<Mutex<System>>) -> bool {
        self.system_manager.add(system)
    }

    pub fn remove_system(&mut self, system: &Arc<Mutex<System>>) -> bool {
        self.system_manager.remove(system)
    }

    pub fn system(&self) -> SharedSystem {
        self.system.clone()
    }
}

impl Drop for Manager {
    fn drop(&mut self) {
        self.stop();
    }
}
