// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/alarm_worker.h
//! Port of zuyu/src/core/hle/service/glue/time/alarm_worker.cpp
//!
//! AlarmWorker: manages timer alarm events for the time service.

use std::sync::{Arc, Mutex};

use crate::hle::service::os::event::Event;
use crate::hle::service::psc::time::common::AlarmInfo;
use crate::hle::service::psc::time::manager::TimeManager;

/// AlarmWorker manages alarm events and timer scheduling.
///
/// Corresponds to `AlarmWorker` in upstream alarm_worker.h.
/// Upstream stores `Core::System& m_system` and `ServiceManager* m_time_m`.
/// We store `Arc<Mutex<TimeManager>>` for alarm access.
pub struct AlarmWorker {
    initialized: bool,
    /// Timer event for scheduling alarm checks.
    /// Upstream: `Kernel::KEvent* m_timer_event`.
    timer_event: Arc<Event>,
    /// Event signaled when the closest alarm changes.
    closest_alarm_event: Arc<Event>,
    /// Reference to TimeManager for alarm operations.
    /// Upstream: `ServiceManager* m_time_m`.
    time_manager: Option<Arc<Mutex<TimeManager>>>,
}

impl AlarmWorker {
    pub fn new() -> Self {
        Self {
            initialized: false,
            timer_event: Arc::new(Event::new()),
            closest_alarm_event: Arc::new(Event::new()),
            time_manager: None,
        }
    }

    /// Create with a TimeManager reference.
    pub fn with_time_manager(time_manager: Arc<Mutex<TimeManager>>) -> Self {
        Self {
            initialized: false,
            timer_event: Arc::new(Event::new()),
            closest_alarm_event: Arc::new(Event::new()),
            time_manager: Some(time_manager),
        }
    }

    /// Set the time manager reference.
    pub fn set_time_manager(&mut self, time_manager: Arc<Mutex<TimeManager>>) {
        self.time_manager = Some(time_manager);
    }

    /// Initialize the alarm worker.
    /// Upstream: creates timer event, timing event, attaches to closest alarm event.
    pub fn initialize(&mut self) {
        log::debug!("AlarmWorker::Initialize called");
        self.initialized = true;
    }

    /// Handle a power state change by checking alarms.
    /// Port of upstream `AlarmWorker::OnPowerStateChanged`.
    pub fn on_power_state_changed(&self) {
        log::debug!("AlarmWorker::OnPowerStateChanged called");

        let mut closest_alarm_info = AlarmInfo::default();
        let mut closest_time: i64 = 0;

        if !self.get_closest_alarm_info(&mut closest_alarm_info, &mut closest_time) {
            // No valid alarm — unschedule timer and clear event.
            // Upstream: m_system.CoreTiming().UnscheduleEvent(m_timer_timing_event)
            self.timer_event.clear();
            return;
        }

        if closest_alarm_info.alert_time <= closest_time {
            // Alarm has already fired — check and signal all triggered alarms.
            if let Some(ref tm) = self.time_manager {
                let tm_guard = tm.lock().unwrap();
                tm_guard
                    .alarms
                    .check_and_signal(&tm_guard.power_state_request_manager);
            }
        } else {
            let _next_time = closest_alarm_info.alert_time - closest_time;
            // Upstream: unschedule old timer, clear event, schedule new
            // CoreTiming event at nanoseconds(next_time).
            self.timer_event.clear();
            // CoreTiming scheduling would go here when wired.
        }
    }

    /// Get the closest pending alarm.
    /// Port of upstream `AlarmWorker::GetClosestAlarmInfo`.
    fn get_closest_alarm_info(
        &self,
        out_alarm_info: &mut AlarmInfo,
        out_time: &mut i64,
    ) -> bool {
        let Some(ref tm) = self.time_manager else {
            return false;
        };
        let tm_guard = tm.lock().unwrap();

        // Get the closest alarm via public API.
        let Some((alert_time, priority)) = tm_guard.alarms.get_closest_alarm() else {
            return false;
        };

        out_alarm_info.alert_time = alert_time;
        out_alarm_info.priority = priority;

        // Get current steady clock time.
        *out_time = tm_guard.alarms.get_raw_time();

        true
    }
}
