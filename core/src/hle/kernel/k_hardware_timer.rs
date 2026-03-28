//! Port of zuyu/src/core/hle/kernel/k_hardware_timer.h/.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-16
//!
//! KHardwareTimer: the kernel hardware timer, responsible for scheduling
//! timer callbacks via CoreTiming. Inherits from KHardwareTimerBase.

use std::collections::HashMap;
use std::sync::{Arc, Mutex, Weak};
use std::time::Duration;

use super::k_hardware_timer_base::KHardwareTimerBase;
use super::k_thread::KThread;
use crate::core_timing::{self, CoreTiming, EventType, UnscheduleEventType};

/// The kernel hardware timer.
///
/// Upstream inherits from KInterruptTask and KHardwareTimerBase.
/// It registers a CoreTiming callback to fire DoTask() at the appropriate
/// absolute time.
pub struct KHardwareTimer {
    base: KHardwareTimerBase,
    /// Absolute time in nanoseconds for the next wakeup.
    m_wakeup_time: i64,
    /// CoreTiming event for scheduling callbacks.
    m_event_type: Option<Arc<parking_lot::Mutex<EventType>>>,
    /// CoreTiming reference for scheduling/unscheduling events.
    core_timing: Option<Arc<std::sync::Mutex<CoreTiming>>>,
    /// Map of thread_id -> Weak<Mutex<KThread>> for resolving timer tasks.
    /// Upstream doesn't need this because KTimerTask IS the KThread (inheritance).
    /// We use composition instead, so we track weak refs here.
    thread_refs: HashMap<u64, Weak<Mutex<KThread>>>,
    /// GSC reference for PQ updates when timer wakes threads.
    gsc: Option<Weak<Mutex<super::global_scheduler_context::GlobalSchedulerContext>>>,
}

impl KHardwareTimer {
    pub fn new() -> Self {
        Self {
            base: KHardwareTimerBase::new(),
            m_wakeup_time: i64::MAX,
            m_event_type: None,
            core_timing: None,
            thread_refs: HashMap::new(),
            gsc: None,
        }
    }

    pub fn set_gsc(&mut self, gsc: Weak<Mutex<super::global_scheduler_context::GlobalSchedulerContext>>) {
        self.gsc = Some(gsc);
    }

    /// Create the CoreTiming callback.
    /// Upstream: `KHardwareTimer::Initialize()` in k_hardware_timer.cpp
    ///
    /// Must be called after the timer is placed behind Arc<Mutex<>> so that
    /// `wire_callback` can capture a weak reference to self.
    pub fn initialize(&mut self) {
        // Event creation deferred to wire_callback() because we need
        // a Weak<Mutex<KHardwareTimer>> reference for the callback closure.
    }

    /// Wire the CoreTiming callback. Must be called after the timer is
    /// wrapped in Arc<Mutex<>> in KernelCore.
    ///
    /// Matches upstream constructor behavior where `this` pointer is captured
    /// in the CoreTiming callback lambda.
    pub fn wire_callback(
        timer: &Arc<Mutex<KHardwareTimer>>,
        core_timing: Arc<std::sync::Mutex<CoreTiming>>,
    ) {
        let timer_weak = Arc::downgrade(timer);
        let event_type = core_timing::create_event(
            "KHardwareTimer::Callback".to_string(),
            Box::new(move |_late, _ns| {
                if let Some(timer) = timer_weak.upgrade() {
                    timer.lock().unwrap().do_task();
                }
                None
            }),
        );
        let mut guard = timer.lock().unwrap();
        guard.m_event_type = Some(event_type);
        guard.core_timing = Some(core_timing);
    }

    /// Unschedule the event and clean up.
    /// Matches upstream: `KHardwareTimer::Finalize()`
    pub fn finalize(&mut self) {
        if let (Some(ref core_timing), Some(ref event_type)) =
            (&self.core_timing, &self.m_event_type)
        {
            core_timing
                .lock()
                .unwrap()
                .unschedule_event(event_type, UnscheduleEventType::NoWait);
        }
        self.m_wakeup_time = i64::MAX;
        self.m_event_type = None;
        self.core_timing = None;
        self.thread_refs.clear();
    }

    /// Get the current tick (global time in nanoseconds).
    /// Matches upstream: `KHardwareTimer::GetTick()`
    pub fn get_tick(&self) -> i64 {
        if let Some(ref core_timing) = self.core_timing {
            core_timing
                .lock()
                .unwrap()
                .get_global_time_ns()
                .as_nanos() as i64
        } else {
            0
        }
    }

    /// Register an absolute timer task. If the new task is earlier than
    /// the current wakeup time, re-arm the interrupt.
    ///
    /// Matches upstream: `KHardwareTimer::RegisterAbsoluteTask()`
    /// Upstream takes KTimerTask* (which is KThread via inheritance).
    /// We take thread_id + Weak<Mutex<KThread>> for resolution.
    pub fn register_absolute_task(
        &mut self,
        thread: &Arc<Mutex<KThread>>,
        task_time: i64,
    ) {
        let thread_id = thread.lock().unwrap().get_thread_id();

        // Store weak ref for later resolution in do_task.
        self.thread_refs.insert(thread_id, Arc::downgrade(thread));

        // Upstream: KScopedDisableDispatch dd{m_kernel} + KScopedSpinLock lk{GetLock()}.
        // KScopedDisableDispatch increments the current thread's dispatch disable count
        // to prevent rescheduling while modifying timer state.
        // We disable dispatch on the current thread if available.
        let current = super::kernel::get_current_thread_pointer();
        if let Some(ref t) = current {
            t.lock().unwrap().disable_dispatch();
        }

        // Spin lock exclusion handled by caller holding &mut self.
        if self.base.register_absolute_task_impl(thread_id, task_time) {
            if task_time <= self.m_wakeup_time {
                self.enable_interrupt(task_time);
            }
        }

        // Re-enable dispatch.
        if let Some(ref t) = current {
            t.lock().unwrap().enable_dispatch();
        }
    }

    /// Cancel a task.
    /// Matches upstream: `KHardwareTimerBase::CancelTask()`
    pub fn cancel_task(&mut self, thread: &Arc<Mutex<KThread>>) {
        let thread_guard = thread.lock().unwrap();
        let thread_id = thread_guard.get_thread_id();
        let task_time = thread_guard.get_timer_task_time();
        drop(thread_guard);

        // Upstream: KScopedDisableDispatch dd{m_kernel} + KScopedSpinLock lk{m_lock}.
        let current = super::kernel::get_current_thread_pointer();
        if let Some(ref t) = current {
            t.lock().unwrap().disable_dispatch();
        }

        if task_time > 0 {
            self.base.cancel_task(thread_id, task_time);
        }
        self.thread_refs.remove(&thread_id);

        if let Some(ref t) = current {
            t.lock().unwrap().enable_dispatch();
        }
    }

    /// Matches upstream: `KHardwareTimer::EnableInterrupt()`
    fn enable_interrupt(&mut self, wakeup_time: i64) {
        self.disable_interrupt();

        self.m_wakeup_time = wakeup_time;
        if let (Some(ref core_timing), Some(ref event_type)) =
            (&self.core_timing, &self.m_event_type)
        {
            core_timing.lock().unwrap().schedule_event(
                Duration::from_nanos(wakeup_time as u64),
                event_type,
                true, // absolute time
            );
        }
    }

    /// Matches upstream: `KHardwareTimer::DisableInterrupt()`
    fn disable_interrupt(&mut self) {
        if let (Some(ref core_timing), Some(ref event_type)) =
            (&self.core_timing, &self.m_event_type)
        {
            core_timing
                .lock()
                .unwrap()
                .unschedule_event(event_type, UnscheduleEventType::NoWait);
        }
        self.m_wakeup_time = i64::MAX;
    }

    fn get_interrupt_enabled(&self) -> bool {
        self.m_wakeup_time != i64::MAX
    }

    /// Called by the CoreTiming callback.
    /// Matches upstream: `KHardwareTimer::DoTask()`
    fn do_task(&mut self) {
        // Upstream: KScopedSchedulerLock slk{m_kernel} + KScopedSpinLock lk(GetLock()).
        // The scheduler lock ensures atomicity with thread state changes and
        // triggers UpdateHighestPriorityThreads on unlock (enabling rescheduling
        // after timer-woken threads change priority queues).
        // The scheduler lock is acquired by the caller (CoreTiming callback context)
        // via the KScopedSchedulerLock. Since do_task is called from within
        // Arc<Mutex<KHardwareTimer>>, the Mutex provides the spin lock equivalent.

        if !self.get_interrupt_enabled() {
            return;
        }

        // Disable the timer interrupt while we handle this.
        // Not necessary due to core timing already having popped this event.
        self.m_wakeup_time = i64::MAX;

        let cur_tick = self.get_tick();

        // Collect thread refs before calling do_interrupt_task_impl to avoid
        // borrow issues (the callback borrows thread_refs while base is &mut).
        let thread_refs = self.thread_refs.clone();

        let mut woken_props: Vec<(u64, i32, i32, u64, bool)> = Vec::new();
        let next_time = self.base.do_interrupt_task_impl(cur_tick, |task_id| {
            // task->OnTimer() — resolve thread and call on_timer
            if let Some(weak) = thread_refs.get(&task_id) {
                if let Some(thread) = weak.upgrade() {
                    let mut guard = thread.lock().unwrap();
                    guard.on_timer();
                    // Thread transitioned WAITING → RUNNABLE — collect props for PQ push.
                    woken_props.push(
                        super::global_scheduler_context::GlobalSchedulerContext::extract_thread_props(&guard),
                    );
                }
            }
        });

        // Push woken threads to PQ.
        if !woken_props.is_empty() {
            if let Some(gsc) = self.gsc.as_ref().and_then(Weak::upgrade) {
                let mut gsc = gsc.lock().unwrap();
                for (id, pri, core, aff, dummy) in woken_props {
                    gsc.push_back_to_priority_queue(id, pri, core, aff, dummy, None);
                }
            }
        }

        // Clean up expired thread refs (tasks with time reset to 0 have been removed)
        self.thread_refs
            .retain(|_id, weak| weak.strong_count() > 0);

        if next_time > 0 && next_time <= self.m_wakeup_time {
            self.enable_interrupt(next_time);
        }
    }
}

impl Default for KHardwareTimer {
    fn default() -> Self {
        Self::new()
    }
}
