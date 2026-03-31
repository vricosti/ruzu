// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/alarm_worker.h
//! Port of zuyu/src/core/hle/service/glue/time/alarm_worker.cpp
//!
//! AlarmWorker: manages timer alarm events for the time service.

use std::sync::{Arc, Mutex};
use std::time::Duration;

use crate::core_timing::{CoreTiming, EventType, UnscheduleEventType};

/// Alias for parking_lot::Mutex used by CoreTiming.
type ParkingMutex<T> = parking_lot::Mutex<T>;
use crate::hle::service::os::event::Event;
use crate::hle::service::psc::time::common::AlarmInfo;
use crate::hle::service::psc::time::manager::TimeManager;

/// AlarmWorker manages alarm events and timer scheduling.
///
/// Corresponds to `AlarmWorker` in upstream alarm_worker.h.
/// Upstream stores `Core::System& m_system` (for CoreTiming) and
/// `ServiceManager* m_time_m` (for alarm operations).
pub struct AlarmWorker {
    initialized: bool,
    /// Timer event for scheduling alarm checks.
    /// Upstream: `Kernel::KEvent* m_timer_event`.
    timer_event: Arc<Event>,
    /// CoreTiming event type for the timer callback.
    /// Upstream: `std::shared_ptr<Core::Timing::EventType> m_timer_timing_event`.
    timer_timing_event: Option<Arc<ParkingMutex<EventType>>>,
    /// Event signaled when the closest alarm changes.
    closest_alarm_event: Arc<Event>,
    /// Reference to TimeManager for alarm operations.
    /// Upstream: `ServiceManager* m_time_m`.
    time_manager: Option<Arc<Mutex<TimeManager>>>,
    /// Reference to CoreTiming for scheduling.
    /// Upstream: `m_system.CoreTiming()`.
    core_timing: Option<Arc<std::sync::Mutex<CoreTiming>>>,
}

impl AlarmWorker {
    pub fn new() -> Self {
        Self {
            initialized: false,
            timer_event: Arc::new(Event::new()),
            timer_timing_event: None,
            closest_alarm_event: Arc::new(Event::new()),
            time_manager: None,
            core_timing: None,
        }
    }

    /// Create with all references.
    pub fn with_refs(
        time_manager: Arc<Mutex<TimeManager>>,
        core_timing: Arc<Mutex<CoreTiming>>,
    ) -> Self {
        Self {
            initialized: false,
            timer_event: Arc::new(Event::new()),
            timer_timing_event: None,
            closest_alarm_event: Arc::new(Event::new()),
            time_manager: Some(time_manager),
            core_timing: Some(core_timing),
        }
    }

    /// Set the time manager reference.
    pub fn set_time_manager(&mut self, time_manager: Arc<Mutex<TimeManager>>) {
        self.time_manager = Some(time_manager);
    }

    /// Set the core timing reference.
    pub fn set_core_timing(&mut self, core_timing: Arc<Mutex<CoreTiming>>) {
        self.core_timing = Some(core_timing);
    }

    /// Initialize the alarm worker.
    /// Port of upstream `AlarmWorker::Initialize`.
    /// Creates the CoreTiming event and attaches to the closest alarm event.
    pub fn initialize(&mut self) {
        log::debug!("AlarmWorker::Initialize called");

        // Create the timer timing event for CoreTiming scheduling.
        let timer_event_clone = self.timer_event.clone();
        let timer_timing_event = Arc::new(ParkingMutex::new(EventType {
            callback: Box::new(move |_late_ns, _time| {
                timer_event_clone.signal();
                None // no auto-reschedule
            }),
            name: "Glue:AlarmWorker:TimerEvent".to_string(),
            sequence_number: 0,
        }));
        self.timer_timing_event = Some(timer_timing_event);

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
            self.unschedule_timer();
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
            let next_time_ns = closest_alarm_info.alert_time - closest_time;

            // Unschedule old timer, clear event, schedule new timer.
            self.unschedule_timer();
            self.timer_event.clear();
            self.schedule_timer(Duration::from_nanos(next_time_ns as u64));
        }
    }

    /// Unschedule the CoreTiming timer event.
    fn unschedule_timer(&self) {
        if let (Some(ref ct), Some(ref evt)) = (&self.core_timing, &self.timer_timing_event) {
            ct.lock()
                .unwrap()
                .unschedule_event(evt, UnscheduleEventType::NoWait);
        }
    }

    /// Schedule the CoreTiming timer event at the given duration from now.
    fn schedule_timer(&self, duration: Duration) {
        if let (Some(ref ct), Some(ref evt)) = (&self.core_timing, &self.timer_timing_event) {
            ct.lock().unwrap().schedule_event(duration, evt, false);
        }
    }

    /// Get the closest pending alarm.
    /// Port of upstream `AlarmWorker::GetClosestAlarmInfo`.
    fn get_closest_alarm_info(&self, out_alarm_info: &mut AlarmInfo, out_time: &mut i64) -> bool {
        let Some(ref tm) = self.time_manager else {
            return false;
        };
        let tm_guard = tm.lock().unwrap();

        let Some((alert_time, priority)) = tm_guard.alarms.get_closest_alarm() else {
            return false;
        };

        out_alarm_info.alert_time = alert_time;
        out_alarm_info.priority = priority;

        *out_time = tm_guard.alarms.get_raw_time();

        true
    }
}
