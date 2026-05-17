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
                    let profile = std::env::var_os("RUZU_PROFILE_SYSMGR").is_some();
                    // `RUZU_PROFILE_SYSMGR_RATE=1` — unfiltered version that
                    // periodically dumps the cumulative iter count + summed
                    // time breakdown. Catches fast iters that the per-iter
                    // filtered log skips. Use to confirm the manager loop's
                    // actual frequency (target: 200 Hz for 5ms audio frames).
                    let profile_rate = std::env::var_os("RUZU_PROFILE_SYSMGR_RATE").is_some();
                    let rate_start = std::time::Instant::now();
                    let mut rate_iter_count: u64 = 0;
                    let mut rate_send_us: u64 = 0;
                    let mut rate_lock_us: u64 = 0;
                    let mut rate_signal_us: u64 = 0;
                    let mut rate_wait_us: u64 = 0;
                    let mut rate_last_dump = std::time::Instant::now();
                    while active.load(Ordering::SeqCst) {
                        let t_iter = std::time::Instant::now();
                        let t = std::time::Instant::now();
                        {
                            let systems = systems.lock();
                            for system in systems.iter() {
                                system.lock().send_command_to_dsp();
                            }
                        }
                        let t_sendcmds = t.elapsed().as_micros();

                        let t = std::time::Instant::now();
                        let t_renderlock;
                        let t_signal;
                        let t_wait;
                        {
                            let mut renderer = audio_renderer.lock();
                            t_renderlock = t.elapsed().as_micros();
                            let t_s = std::time::Instant::now();
                            renderer.signal();
                            t_signal = t_s.elapsed().as_micros();
                            let t_w = std::time::Instant::now();
                            renderer.wait();
                            t_wait = t_w.elapsed().as_micros();
                        }
                        if profile {
                            let total = t_iter.elapsed().as_micros();
                            // Filter out empty iterations (wait returns instantly) so slow
                            // rendering cycles stand out in the log.
                            if total >= 500 {
                                log::info!(
                                    "PROFILE_SYSMGR total_us={} send_cmds_us={} render_lock_us={} signal_us={} wait_us={}",
                                    total, t_sendcmds, t_renderlock, t_signal, t_wait
                                );
                            }
                        }
                        if profile_rate {
                            rate_iter_count += 1;
                            rate_send_us += t_sendcmds as u64;
                            rate_lock_us += t_renderlock as u64;
                            rate_signal_us += t_signal as u64;
                            rate_wait_us += t_wait as u64;
                            // Dump every ~500ms so we see the rate evolve.
                            if rate_last_dump.elapsed().as_millis() >= 500 {
                                let elapsed_s = rate_start.elapsed().as_secs_f64();
                                log::info!(
                                    "PROFILE_SYSMGR_RATE t={:.2}s iters={} hz_total={:.1} \
                                     sum_send_ms={} sum_lock_ms={} sum_signal_ms={} sum_wait_ms={}",
                                    elapsed_s,
                                    rate_iter_count,
                                    rate_iter_count as f64 / elapsed_s.max(0.001),
                                    rate_send_us / 1000,
                                    rate_lock_us / 1000,
                                    rate_signal_us / 1000,
                                    rate_wait_us / 1000,
                                );
                                rate_last_dump = std::time::Instant::now();
                            }
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
