use crate::adsp::adsp::AudioRendererHandle;
use crate::common::common::MAX_RENDERER_SESSIONS;
use crate::renderer::system::System;
use crate::SharedSystem;
use log::error;
use parking_lot::Mutex;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread::{self, JoinHandle};

pub struct SystemManager {
    core: SharedSystem,
    systems: Arc<Mutex<Vec<Arc<Mutex<System>>>>>,
    thread: Option<JoinHandle<()>>,
    active: Arc<AtomicBool>,
    audio_renderer: AudioRendererHandle,
}

impl SystemManager {
    pub fn new(core: SharedSystem, audio_renderer: AudioRendererHandle) -> Self {
        Self {
            core,
            systems: Arc::new(Mutex::new(Vec::new())),
            thread: None,
            active: Arc::new(AtomicBool::new(false)),
            audio_renderer,
        }
    }

    pub fn initialize_unsafe(&mut self) {
        if self.active.swap(true, Ordering::SeqCst) {
            return;
        }

        log::info!("AudioRenderSystemManager::initialize_unsafe start");
        self.audio_renderer.lock().start();
        let systems = Arc::clone(&self.systems);
        let active = Arc::clone(&self.active);
        let audio_renderer = self.audio_renderer.clone();
        self.thread = Some(
            thread::Builder::new()
                .name("AudioRenderSystemManager".to_string())
                .spawn(move || {
                    log::info!("AudioRenderSystemManager thread started");
                    while active.load(Ordering::SeqCst) {
                        {
                            let systems = systems.lock();
                            log::info!(
                                "AudioRenderSystemManager loop systems={}",
                                systems.len()
                            );
                            for system in systems.iter() {
                                system.lock().send_command_to_dsp();
                            }
                        }

                        {
                            let mut renderer = audio_renderer.lock();
                            renderer.signal();
                            renderer.wait();
                        }
                        std::thread::yield_now();
                    }
                })
                .expect("failed to spawn audio render system manager thread"),
        );
    }

    pub fn stop(&mut self) {
        if !self.active.swap(false, Ordering::SeqCst) {
            return;
        }
        log::info!("AudioRenderSystemManager::stop");
        if let Some(thread) = self.thread.take() {
            let _ = thread.join();
        }
        self.audio_renderer.lock().stop();
    }

    pub fn add(&mut self, system: Arc<Mutex<System>>) -> bool {
        let mut systems = self.systems.lock();
        if systems.len() + 1 > MAX_RENDERER_SESSIONS {
            error!("audio_core: maximum AudioRenderer systems active, cannot add more");
            return false;
        }

        if systems.is_empty() {
            drop(systems);
            self.initialize_unsafe();
            systems = self.systems.lock();
        }

        systems.push(system);
        log::info!("AudioRenderSystemManager::add systems={}", systems.len());
        true
    }

    pub fn remove(&mut self, system: &Arc<Mutex<System>>) -> bool {
        let mut systems = self.systems.lock();
        let before = systems.len();
        systems.retain(|current| !Arc::ptr_eq(current, system));
        if systems.len() == before {
            error!("audio_core: failed to remove a render system, it was not found in the list");
            return false;
        }
        let should_stop = systems.is_empty();
        drop(systems);
        if should_stop {
            self.stop();
        }
        true
    }

    pub fn core(&self) -> SharedSystem {
        self.core.clone()
    }
}

impl Drop for SystemManager {
    fn drop(&mut self) {
        self.stop();
    }
}
