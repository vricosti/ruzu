// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/conductor.h
//! Port of zuyu/src/core/hle/service/vi/conductor.cpp
//!
//! The Conductor manages vsync timing and display composition.
//! It maintains a VsyncManager per display and drives the composition loop.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use crate::core::SystemRef;
use crate::core_timing;
use crate::hle::service::nvnflinger::surface_flinger::SurfaceFlinger;
use crate::hle::service::os::event::Event;

use super::vsync_manager::VsyncManager;

/// Frame interval in nanoseconds (60 FPS).
pub const FRAME_NS: i64 = 1_000_000_000 / 60;

/// Conductor drives display composition and vsync signaling.
///
/// Upstream creates a CoreTiming event "ScreenComposition" that fires every
/// 16.67ms. In multicore mode, the callback signals a thread event and a
/// dedicated VsyncThread calls ProcessVsync(). In single-core mode, the
/// callback directly calls ProcessVsync().
pub struct Conductor {
    vsync_managers: HashMap<u64, VsyncManager>,
    swap_interval: i32,
    compose_speed_scale: f32,
    system: SystemRef,
    /// Surface flinger reference for composition.
    /// Upstream: Conductor holds Container& and calls Container::ComposeOnDisplay
    /// which delegates to SurfaceFlinger::ComposeDisplay. We skip the Container
    /// indirection to avoid circular references.
    surface_flinger: Arc<SurfaceFlinger>,
    event: Option<Arc<parking_lot::Mutex<core_timing::EventType>>>,
}

impl Conductor {
    pub fn new(system: SystemRef, display_ids: &[u64], surface_flinger: Arc<SurfaceFlinger>) -> Self {
        let mut vsync_managers = HashMap::new();
        for &id in display_ids {
            vsync_managers.insert(id, VsyncManager::new());
        }

        Self {
            vsync_managers,
            swap_interval: 1,
            compose_speed_scale: 1.0,
            system,
            surface_flinger,
            event: None,
        }
    }

    /// Start the vsync timer. Must be called after the Conductor is placed
    /// inside an `Arc<Mutex<>>` so the CoreTiming callback can reference it.
    ///
    /// Upstream does this in the constructor, but we split it because the
    /// CoreTiming callback needs a weak reference back to the Conductor.
    pub fn start(conductor: &Arc<Mutex<Self>>) {
        let weak = Arc::downgrade(conductor);
        let event = core_timing::create_event(
            "ScreenComposition".to_string(),
            Box::new(move |_time, _ns_late| {
                if let Some(conductor) = weak.upgrade() {
                    conductor.lock().unwrap().process_vsync();
                }
                // Reschedule at the next tick interval.
                // Upstream returns GetNextTicks() but we use a fixed 60fps for now.
                Some(Duration::from_nanos(FRAME_NS as u64))
            }),
        );

        let system = conductor.lock().unwrap().system;
        let core_timing = system.get().core_timing();
        let frame_dur = Duration::from_nanos(FRAME_NS as u64);
        core_timing
            .lock()
            .unwrap()
            .schedule_looping_event(frame_dur, frame_dur, &event, false);

        conductor.lock().unwrap().event = Some(event);
        log::info!("Conductor: vsync timer started at {}ns interval", FRAME_NS);
    }

    pub fn link_vsync_event(&mut self, display_id: u64, event: Arc<Event>) {
        if let Some(manager) = self.vsync_managers.get_mut(&display_id) {
            manager.link_vsync_event(event);
        }
    }

    pub fn unlink_vsync_event(&mut self, display_id: u64, event: &Arc<Event>) {
        if let Some(manager) = self.vsync_managers.get_mut(&display_id) {
            manager.unlink_vsync_event(event);
        }
    }

    /// Process a vsync tick: compose each display, then signal vsync events.
    /// Port of upstream `Conductor::ProcessVsync`.
    fn process_vsync(&mut self) {
        for (&display_id, manager) in self.vsync_managers.iter() {
            // Upstream: m_container.ComposeOnDisplay(&m_swap_interval, &m_compose_speed_scale, display_id);
            self.surface_flinger.compose_display(
                &mut self.swap_interval,
                &mut self.compose_speed_scale,
                display_id,
            );
            manager.signal_vsync();
        }
    }

    /// Calculate the next tick interval in nanoseconds.
    /// Port of upstream `Conductor::GetNextTicks`.
    pub fn get_next_ticks(&self) -> i64 {
        let speed_scale = 1.0f32 / self.compose_speed_scale;
        let effective_fps = 60.0f32 / self.swap_interval as f32;
        (speed_scale * (1_000_000_000.0f32 / effective_fps)) as i64
    }
}

impl Drop for Conductor {
    fn drop(&mut self) {
        // Unschedule the CoreTiming event.
        if let Some(ref event) = self.event {
            let core_timing = self.system.get().core_timing();
            core_timing
                .lock()
                .unwrap()
                .unschedule_event(event, core_timing::UnscheduleEventType::NoWait);
            log::info!("Conductor: vsync timer stopped");
        }
    }
}
