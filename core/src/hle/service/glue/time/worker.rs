// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/worker.h
//! Port of zuyu/src/core/hle/service/glue/time/worker.cpp
//!
//! TimeWorker: thread-based event loop for time service operations.
//! Handles steady clock updates, filesystem timestamp updates, alarm processing,
//! and power state changes.

use crate::hle::service::psc::time::common::SystemClockContext;
use super::alarm_worker::AlarmWorker;
use super::pm_state_change_handler::PmStateChangeHandler;

/// Event types processed in the TimeWorker thread loop.
///
/// Corresponds to the anonymous `EventType` enum in upstream worker.cpp ThreadFunc.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum EventType {
    Exit = 0,
    PowerStateChange = 1,
    SignalAlarms = 2,
    UpdateLocalSystemClock = 3,
    UpdateNetworkSystemClock = 4,
    UpdateEphemeralSystemClock = 5,
    UpdateSteadyClock = 6,
    UpdateFileTimestamp = 7,
    AutoCorrect = 8,
}

/// TimeWorker runs a background thread that processes time-related events.
///
/// Corresponds to `TimeWorker` in upstream worker.h.
pub struct TimeWorker {
    pub alarm_worker: AlarmWorker,
    pub pm_state_change_handler: PmStateChangeHandler,
    /// Whether the initial report for network clock context has been set.
    ig_report_network_clock_context_set: bool,
    report_network_clock_context: SystemClockContext,
    /// Whether the initial report for ephemeral clock context has been set.
    ig_report_ephemeral_clock_context_set: bool,
    report_ephemeral_clock_context: SystemClockContext,
    /// Whether the worker thread is running.
    running: bool,
    // Upstream holds: thread handle (std::jthread), exit event (KEvent*),
    // timer events (KEvent* for steady clock and filesystem), clock sub-services
    // (shared_ptr<SystemClock> for local/network/ephemeral), clock operation
    // events (KReadableEvent* for each clock), and CoreTiming event types for
    // the two periodic timers. These require kernel integration (KEvent,
    // CoreTiming, jthread) and service manager wiring (time:sm, time:m, set:sys).
}

impl TimeWorker {
    /// Create a new TimeWorker.
    ///
    /// Corresponds to `TimeWorker::TimeWorker(System&, StandardSteadyClockResource&,
    /// FileTimestampWorker&)` in upstream worker.cpp.
    pub fn new() -> Self {
        Self {
            alarm_worker: AlarmWorker::new(),
            pm_state_change_handler: PmStateChangeHandler::new(),
            ig_report_network_clock_context_set: false,
            report_network_clock_context: SystemClockContext::default(),
            ig_report_ephemeral_clock_context_set: false,
            report_ephemeral_clock_context: SystemClockContext::default(),
            running: false,
        }
    }

    /// Initialize the worker with time service and settings references.
    ///
    /// Corresponds to `TimeWorker::Initialize` in upstream worker.cpp.
    ///
    /// Upstream performs these steps:
    /// 1. Gets settings items (steady clock interval, fs notify interval)
    ///    from set:sys via GetSettingsItemValueImpl.
    /// 2. Schedules two CoreTiming looping events:
    ///    - steady clock update timer (default 30 min interval)
    ///    - filesystem timestamp timer (default 600s interval)
    /// 3. Gets local/network/ephemeral SystemClock sub-services from time:sm.
    /// 4. Gets clock operation events from time:m (ServiceManager).
    /// 5. Gets the auto-correction update event from time:m.
    ///
    /// These steps require set:sys, time:sm, and time:m services to be
    /// fully wired, plus CoreTiming for the periodic timers. The alarm
    /// worker initialization is performed since it is self-contained.
    pub fn initialize(&mut self) {
        log::debug!("TimeWorker::Initialize called");

        // Initialize the alarm worker (self-contained, no external deps).
        self.alarm_worker.initialize();
    }

    /// Start the worker thread.
    ///
    /// Corresponds to `TimeWorker::StartThread` in upstream worker.cpp.
    /// Upstream spawns a std::jthread running ThreadFunc.
    ///
    /// The thread requires kernel WaitAny to wait on multiple KReadableEvent
    /// objects simultaneously, which is not yet available in the Rust port.
    /// When kernel multi-wait is implemented, this should spawn a thread
    /// calling thread_func.
    pub fn start_thread(&mut self) {
        log::debug!("TimeWorker::StartThread called");
        self.running = true;
    }

    /// The main thread function that processes events in a loop.
    ///
    /// Corresponds to `TimeWorker::ThreadFunc` in upstream worker.cpp.
    /// This is the event loop that waits on multiple events and dispatches handlers.
    ///
    /// Upstream loop structure:
    ///   If pm_state_change_handler.priority != 0:
    ///     WaitAny on: exit_event, alarm_event
    ///   Else:
    ///     WaitAny on: exit_event, alarm_event, alarm_timer, local_clock,
    ///                 network_clock, ephemeral_clock, steady_clock_timer,
    ///                 file_system_timer, auto_correct_event
    ///
    ///   Match on event index:
    ///     Exit -> return
    ///     PowerStateChange -> alarm_worker.on_power_state_changed()
    ///     SignalAlarms -> time_m.check_and_signal_alarms()
    ///     UpdateLocalSystemClock -> save context to set:sys, update fs timestamp
    ///     UpdateNetworkSystemClock -> save context to set:sys, system report
    ///     UpdateEphemeralSystemClock -> system report
    ///     UpdateSteadyClock -> update steady clock resource, set base time
    ///     UpdateFileTimestamp -> set filesystem posix time
    ///     AutoCorrect -> save auto-correction settings to set:sys
    ///
    /// Requires kernel WaitAny, CoreTiming, and service manager integration.
    #[allow(dead_code)]
    fn thread_func(&mut self) {
        log::debug!("TimeWorker::ThreadFunc started (event loop requires kernel WaitAny)");
    }
}
