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
    /// Map of thread_id -> raw KThread pointer for resolving timer tasks.
    /// Upstream stores the `KTimerTask*` directly because `KThread` inherits it.
    /// Rust uses a raw pointer here so timer delivery can run under the
    /// scheduler lock without relocking the sleeping thread's mutex.
    thread_ptrs: HashMap<u64, usize>,
    /// GSC reference for PQ updates when timer wakes threads.
    gsc: Option<Weak<Mutex<super::global_scheduler_context::GlobalSchedulerContext>>>,
}

enum TimerTaskTarget {
    ThreadArc(Arc<Mutex<KThread>>),
    RawPtr(usize),
}

impl KHardwareTimer {
    pub fn new() -> Self {
        Self {
            base: KHardwareTimerBase::new(),
            m_wakeup_time: i64::MAX,
            m_event_type: None,
            core_timing: None,
            thread_ptrs: HashMap::new(),
            gsc: None,
        }
    }

    pub fn set_gsc(
        &mut self,
        gsc: Weak<Mutex<super::global_scheduler_context::GlobalSchedulerContext>>,
    ) {
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
        self.thread_ptrs.clear();
    }

    /// Get the current tick (global time in nanoseconds).
    /// Matches upstream: `KHardwareTimer::GetTick()`
    pub fn get_tick(&self) -> i64 {
        if let Some(ref core_timing) = self.core_timing {
            core_timing.lock().unwrap().get_global_time_ns().as_nanos() as i64
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
    pub fn register_absolute_task(&mut self, thread: &Arc<Mutex<KThread>>, task_time: i64) {
        let thread_id = {
            let mut thread_guard = thread.lock().unwrap();
            thread_guard.set_timer_task_time(task_time);
            thread_guard.get_thread_id()
        };
        let thread_ptr = {
            let mut thread_guard = thread.lock().unwrap();
            (&mut *thread_guard) as *mut KThread as usize
        };
        self.thread_ptrs.insert(thread_id, thread_ptr);

        let had_current = super::kernel::with_current_thread_fast_mut(|t| {
            t.disable_dispatch();
        })
        .is_some();

        if self.base.register_absolute_task_impl(thread_id, task_time) {
            if task_time <= self.m_wakeup_time {
                self.enable_interrupt(task_time);
            }
        }
        if had_current {
            super::kernel::with_current_thread_fast_mut(|t| {
                t.enable_dispatch();
            });
        }
    }

    pub fn register_absolute_task_by_id(
        &mut self,
        thread_id: u64,
        thread_ptr: usize,
        task_time: i64,
    ) {
        self.thread_ptrs.insert(thread_id, thread_ptr);

        let had_current = super::kernel::with_current_thread_fast_mut(|t| {
            t.disable_dispatch();
        })
        .is_some();

        if self.base.register_absolute_task_impl(thread_id, task_time) {
            if task_time <= self.m_wakeup_time {
                self.enable_interrupt(task_time);
            }
        }

        if had_current {
            super::kernel::with_current_thread_fast_mut(|t| {
                t.enable_dispatch();
            });
        }
    }

    /// Cancel a task.
    /// Matches upstream: `KHardwareTimerBase::CancelTask()`
    pub fn cancel_task(&mut self, thread: &Arc<Mutex<KThread>>) {
        let (thread_id, task_time) = {
            let thread_guard = thread.lock().unwrap();
            (
                thread_guard.get_thread_id(),
                thread_guard.get_timer_task_time(),
            )
        };
        let had_current = super::kernel::with_current_thread_fast_mut(|t| {
            t.disable_dispatch();
        })
        .is_some();

        if task_time > 0 {
            self.base.cancel_task(thread_id, task_time);
        }
        thread.lock().unwrap().set_timer_task_time(0);
        self.thread_ptrs.remove(&thread_id);
        if had_current {
            super::kernel::with_current_thread_fast_mut(|t| {
                t.enable_dispatch();
            });
        }
    }

    pub fn cancel_task_by_id(&mut self, thread_id: u64, task_time: i64) {
        let had_current = super::kernel::with_current_thread_fast_mut(|t| {
            t.disable_dispatch();
        })
        .is_some();

        if task_time > 0 {
            self.base.cancel_task(thread_id, task_time);
        }
        self.thread_ptrs.remove(&thread_id);

        if had_current {
            super::kernel::with_current_thread_fast_mut(|t| {
                t.enable_dispatch();
            });
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

    fn resolve_task_target(&self, task_id: u64) -> Option<TimerTaskTarget> {
        if let Some(gsc) = self.gsc.as_ref().and_then(Weak::upgrade) {
            if let Some(thread) = gsc.lock().unwrap().get_thread_by_thread_id(task_id) {
                return Some(TimerTaskTarget::ThreadArc(thread));
            }
        }

        self.thread_ptrs
            .get(&task_id)
            .copied()
            .filter(|ptr| *ptr != 0)
            .map(TimerTaskTarget::RawPtr)
    }

    /// Called by the CoreTiming callback.
    /// Matches upstream: `KHardwareTimer::DoTask()`
    fn do_task(&mut self) {
        if !self.get_interrupt_enabled() {
            return;
        }

        // Disable the timer interrupt while we handle this.
        // Not necessary due to core timing already having popped this event.
        self.m_wakeup_time = i64::MAX;

        let cur_tick = self.get_tick();

        let (fired_task_ids, next_time) = self.base.collect_expired_tasks(cur_tick);
        let gsc = self.gsc.as_ref().and_then(Weak::upgrade);

        for task_id in fired_task_ids {
            let target = if let Some(gsc) = gsc.as_ref() {
                gsc.lock()
                    .unwrap()
                    .get_thread_by_thread_id(task_id)
                    .map(TimerTaskTarget::ThreadArc)
            } else {
                None
            }
            .or_else(|| {
                self.thread_ptrs
                    .get(&task_id)
                    .copied()
                    .filter(|ptr| *ptr != 0)
                    .map(TimerTaskTarget::RawPtr)
            });

            match target {
                Some(TimerTaskTarget::ThreadArc(thread)) => {
                    let mut thread = thread.lock().unwrap();
                    thread.set_timer_task_time(0);
                    thread.on_timer();
                }
                Some(TimerTaskTarget::RawPtr(thread_ptr)) => {
                    let thread = unsafe { &mut *(thread_ptr as *mut KThread) };
                    thread.set_timer_task_time(0);
                    thread.on_timer();
                }
                None => {}
            }

            self.thread_ptrs.remove(&task_id);
        }

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hle::kernel::global_scheduler_context::GlobalSchedulerContext;

    #[test]
    fn resolve_task_target_falls_back_to_gsc_thread_lookup() {
        let mut timer = KHardwareTimer::new();
        let gsc = Arc::new(Mutex::new(GlobalSchedulerContext::new()));
        let thread = Arc::new(Mutex::new(KThread::new()));
        thread.lock().unwrap().thread_id = 17;
        gsc.lock().unwrap().add_thread(Arc::clone(&thread));
        timer.set_gsc(Arc::downgrade(&gsc));

        match timer.resolve_task_target(17) {
            Some(TimerTaskTarget::ThreadArc(found)) => {
                assert!(Arc::ptr_eq(&found, &thread));
            }
            _ => panic!("expected GSC-backed timer target"),
        }
    }
}
