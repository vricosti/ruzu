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
                    // Target one audio frame per iteration (20 ms ≈ 50 Hz).
                    // Upstream achieves this implicitly because cubeb consumes
                    // buffers at the audio device rate and the DSP main loop
                    // blocks inside `wait_free_space_with_stop` until space
                    // frees up. In Rust, when the sink isn't fully started
                    // (sink paused or max_queue_size==0), that wait returns
                    // instantly and the loop spins thousands of times per
                    // second, which starves the game's WaitSynchronization on
                    // the rendered event. Pace the whole loop here instead.
                    const AUDIO_FRAME: std::time::Duration =
                        std::time::Duration::from_millis(20);
                    while active.load(Ordering::SeqCst) {
                        let loop_start = std::time::Instant::now();
                        {
                            let systems = systems.lock();
                            log::info!("AudioRenderSystemManager loop systems={}", systems.len());
                            for system in systems.iter() {
                                system.lock().send_command_to_dsp();
                            }
                        }

                        // Signal ADSP to render, but don't block waiting for
                        // a response. The ADSP thread may be stuck on sink
                        // free-space wait when the audio output isn't consuming.
                        // Skipping the blocking wait keeps the 20ms signal loop
                        // alive so signal_rendered_event fires continuously,
                        // matching the upstream behavior where the game gets
                        // audio-ready events at the frame rate regardless of
                        // actual audio throughput.
                        {
                            let mut renderer = audio_renderer.lock();
                            renderer.signal();
                        }

                        let elapsed = loop_start.elapsed();
                        if elapsed < AUDIO_FRAME {
                            std::thread::sleep(AUDIO_FRAME - elapsed);
                        }
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
