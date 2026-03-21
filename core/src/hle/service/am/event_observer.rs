// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/event_observer.h
//! Port of zuyu/src/core/hle/service/am/event_observer.cpp

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Condvar, Mutex};

use super::applet::Applet;
use super::window_system::WindowSystem;

/// Tag for multi-wait user data.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UserDataTag {
    WakeupEvent = 0,
    AppletProcess = 1,
}

/// Port of EventObserver
///
/// Observes applet process lifecycle events and triggers WindowSystem updates.
/// Upstream uses kernel multi-wait and a dedicated thread. Here we use a
/// condvar-based wakeup mechanism and a background thread that calls
/// WindowSystem::update() when signaled.
pub struct EventObserver {
    /// Shared state for the wakeup mechanism.
    wakeup: Arc<WakeupState>,

    /// Stop flag for the processing thread.
    stop_requested: Arc<AtomicBool>,

    /// Processing thread — upstream: std::thread m_thread
    thread: Option<std::thread::JoinHandle<()>>,
}

struct WakeupState {
    mutex: Mutex<bool>,
    condvar: Condvar,
}

impl EventObserver {
    /// Upstream: EventObserver(Core::System& system, WindowSystem& window_system)
    ///
    /// Creates the event observer and starts the background processing thread.
    /// The WindowSystem pointer must remain valid for the lifetime of this
    /// EventObserver (matching upstream lifetime contract).
    pub fn new(window_system: *const WindowSystem) -> Self {
        let wakeup = Arc::new(WakeupState {
            mutex: Mutex::new(false),
            condvar: Condvar::new(),
        });
        let stop_requested = Arc::new(AtomicBool::new(false));

        let wakeup_clone = wakeup.clone();
        let stop_clone = stop_requested.clone();
        let ws = window_system as usize; // transfer as usize for Send

        let thread = std::thread::Builder::new()
            .name("am:EventObserver".into())
            .spawn(move || {
                // SAFETY: The WindowSystem pointer is valid for the lifetime of EventObserver,
                // matching upstream's raw pointer usage pattern.
                let window_system = unsafe { &*(ws as *const WindowSystem) };
                Self::thread_func(window_system, &wakeup_clone, &stop_clone);
            })
            .expect("Failed to spawn EventObserver thread");

        Self {
            wakeup,
            stop_requested,
            thread: Some(thread),
        }
    }

    /// Upstream: void TrackAppletProcess(Applet& applet)
    ///
    /// In upstream, this creates a ProcessHolder and links it to the multi-wait.
    /// Here, since we use a simplified condvar wakeup, we just signal the
    /// observer to start watching. The process state is checked during update().
    pub fn track_applet_process(&self, _applet: &Arc<Mutex<Applet>>) {
        // Signal wakeup so the observer thread performs an update cycle.
        self.signal_wakeup();
    }

    /// Upstream: void RequestUpdate()
    pub fn request_update(&self) {
        self.signal_wakeup();
    }

    fn signal_wakeup(&self) {
        let mut signaled = self.wakeup.mutex.lock().unwrap();
        *signaled = true;
        self.wakeup.condvar.notify_one();
    }

    fn thread_func(
        window_system: &WindowSystem,
        wakeup: &WakeupState,
        stop_requested: &AtomicBool,
    ) {
        loop {
            // Wait for a signal or stop request.
            {
                let mut signaled = wakeup.mutex.lock().unwrap();
                while !*signaled && !stop_requested.load(Ordering::Acquire) {
                    signaled = wakeup.condvar.wait(signaled).unwrap();
                }

                if stop_requested.load(Ordering::Acquire) {
                    break;
                }

                *signaled = false;
            }

            // Perform recalculation — upstream: m_window_system.Update()
            window_system.update();
        }
    }
}

impl Drop for EventObserver {
    fn drop(&mut self) {
        // Signal thread and wait for processing to finish.
        self.stop_requested.store(true, Ordering::Release);
        self.signal_wakeup();
        if let Some(thread) = self.thread.take() {
            let _ = thread.join();
        }
    }
}
