// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/alarm_worker.h
//! Port of zuyu/src/core/hle/service/glue/time/alarm_worker.cpp
//!
//! AlarmWorker: manages timer alarm events for the time service.

use std::sync::Arc;
use std::time::Duration;

use crate::core_timing::{CoreTiming, EventType, UnscheduleEventType};
use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
use crate::hle::service::kernel_helpers::ServiceContext;

/// Alias for parking_lot::Mutex used by CoreTiming.
type ParkingMutex<T> = parking_lot::Mutex<T>;
use crate::hle::service::os::event::Event;
use crate::hle::service::psc::time::common::AlarmInfo;
use crate::hle::service::psc::time::service_manager::TimeServiceManager;

/// AlarmWorker manages alarm events and timer scheduling.
///
/// Corresponds to `AlarmWorker` in upstream alarm_worker.h.
/// Upstream stores `Core::System& m_system` (for CoreTiming) and
/// `ServiceManager* m_time_m` (for alarm operations).
pub struct AlarmWorker {
    service_context: ServiceContext,
    /// Timer event for scheduling alarm checks.
    /// Upstream: `Kernel::KEvent* m_timer_event`.
    timer_event_handle: Option<u32>,
    timer_event: Option<Arc<Event>>,
    /// CoreTiming event type for the timer callback.
    /// Upstream: `std::shared_ptr<Core::Timing::EventType> m_timer_timing_event`.
    timer_timing_event: Option<Arc<ParkingMutex<EventType>>>,
    /// Event signaled when the closest alarm changes.
    closest_alarm_event: Option<Arc<Event>>,
    /// Reference to TimeManager for alarm operations.
    /// Upstream: `ServiceManager* m_time_m`.
    time_manager: SessionRequestHandlerPtr,
    /// Reference to CoreTiming for scheduling.
    /// Upstream: `m_system.CoreTiming()`.
    core_timing: Arc<CoreTiming>,
}

impl AlarmWorker {
    pub fn new(time_manager: SessionRequestHandlerPtr, core_timing: Arc<CoreTiming>) -> Self {
        Self {
            service_context: ServiceContext::new("Glue:AlarmWorker".to_string()),
            timer_event_handle: None,
            timer_event: None,
            timer_timing_event: None,
            closest_alarm_event: None,
            time_manager,
            core_timing,
        }
    }

    pub fn get_event(&self) -> Arc<Event> {
        Arc::clone(
            self.closest_alarm_event
                .as_ref()
                .expect("AlarmWorker must be initialized before GetEvent"),
        )
    }

    pub fn get_timer_event(&self) -> Arc<Event> {
        Arc::clone(
            self.timer_event
                .as_ref()
                .expect("AlarmWorker must be initialized before GetTimerEvent"),
        )
    }

    /// Initialize the alarm worker.
    /// Port of upstream `AlarmWorker::Initialize`.
    /// Creates the CoreTiming event and attaches to the closest alarm event.
    pub fn initialize(&mut self) {
        log::debug!("AlarmWorker::Initialize called");

        let timer_event_handle = self
            .service_context
            .create_event("Glue:AlarmWorker:TimerEvent".to_string());
        let timer_event = self
            .service_context
            .get_event(timer_event_handle)
            .expect("AlarmWorker timer event must exist");

        // Create the timer timing event for CoreTiming scheduling.
        let timer_event_clone = Arc::clone(&timer_event);
        let timer_timing_event = Arc::new(ParkingMutex::new(EventType::new(
            Box::new(move |_late_ns, _time| {
                timer_event_clone.signal();
                None // no auto-reschedule
            }),
            "Glue:AlarmWorker::AlarmTimer".to_string(),
        )));
        self.timer_event_handle = Some(timer_event_handle);
        self.timer_event = Some(timer_event);
        self.timer_timing_event = Some(timer_timing_event);
        self.attach_to_closest_alarm_event();
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
            self.get_timer_event().clear();
            return;
        }

        if closest_alarm_info.alert_time <= closest_time {
            // Alarm has already fired — check and signal all triggered alarms.
            let _ = self.time_manager().check_and_signal_alarms();
        } else {
            let next_time_ns = closest_alarm_info.alert_time - closest_time;

            // Unschedule old timer, clear event, schedule new timer.
            self.unschedule_timer();
            self.get_timer_event().clear();
            self.schedule_timer(Duration::from_nanos(next_time_ns as u64));
        }
    }

    /// Unschedule the CoreTiming timer event.
    fn unschedule_timer(&self) {
        if let Some(ref event) = self.timer_timing_event {
            self.core_timing
                .unschedule_event(event, UnscheduleEventType::NoWait);
        }
    }

    /// Schedule the CoreTiming timer event at the given duration from now.
    fn schedule_timer(&self, duration: Duration) {
        if let Some(ref event) = self.timer_timing_event {
            self.core_timing.schedule_event(duration, event, false);
        }
    }

    /// Get the closest pending alarm.
    /// Port of upstream `AlarmWorker::GetClosestAlarmInfo`.
    fn get_closest_alarm_info(&self, out_alarm_info: &mut AlarmInfo, out_time: &mut i64) -> bool {
        let mut is_valid = false;
        let result =
            self.time_manager()
                .get_closest_alarm_info(&mut is_valid, out_alarm_info, out_time);
        assert_eq!(result, crate::hle::result::RESULT_SUCCESS);
        is_valid
    }

    /// Port of upstream `AlarmWorker::AttachToClosestAlarmEvent`.
    fn attach_to_closest_alarm_event(&mut self) {
        let mut event = None;
        let result = self
            .time_manager()
            .get_closest_alarm_updated_event(&mut event);
        assert_eq!(result, crate::hle::result::RESULT_SUCCESS);
        self.closest_alarm_event = event;
    }

    fn time_manager(&self) -> &TimeServiceManager {
        self.time_manager
            .as_any()
            .downcast_ref::<TimeServiceManager>()
            .expect("time:m is not a PSC::Time::ServiceManager")
    }
}

impl Drop for AlarmWorker {
    fn drop(&mut self) {
        self.unschedule_timer();
        if let Some(timer_event_handle) = self.timer_event_handle.take() {
            self.service_context.close_event(timer_event_handle);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn initialize_attaches_real_alarm_event_and_owns_timer_until_drop() {
        let time_manager = Arc::new(TimeServiceManager::new(
            crate::core::SystemRef::null(),
            std::ptr::null(),
            std::ptr::null_mut(),
        ));
        let mut closest_alarm_event = None;
        assert_eq!(
            time_manager.get_closest_alarm_updated_event(&mut closest_alarm_event),
            crate::hle::result::RESULT_SUCCESS
        );
        let closest_alarm_event = closest_alarm_event.unwrap();
        let core_timing = Arc::new(CoreTiming::new());
        let time_manager: SessionRequestHandlerPtr = time_manager;
        let mut worker = AlarmWorker::new(time_manager, core_timing);

        worker.initialize();
        let attached_event = worker.get_event();
        let timer_event = worker.get_timer_event();

        assert!(Arc::ptr_eq(&closest_alarm_event, &attached_event));
        assert!(!attached_event.is_signaled());
        closest_alarm_event.signal();
        assert!(attached_event.is_signaled());
        drop(attached_event);

        assert_eq!(Arc::strong_count(&timer_event), 4);
        drop(worker);
        assert_eq!(Arc::strong_count(&timer_event), 1);
    }
}
