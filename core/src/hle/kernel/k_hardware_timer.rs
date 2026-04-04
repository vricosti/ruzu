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
use super::k_scheduler_lock::KScopedSchedulerLock;
use super::k_thread::KThread;
use crate::core_timing::{self, CoreTiming, EventType, UnscheduleEventType};

/// The kernel hardware timer.
///
/// Upstream inherits from KInterruptTask and KHardwareTimerBase.
/// It registers a CoreTiming callback to fire DoTask() at the appropriate
/// absolute time.
struct KHardwareTimerState {
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

pub struct KHardwareTimer {
    state: Mutex<KHardwareTimerState>,
}

enum TimerTaskTarget {
    ThreadArc(Arc<Mutex<KThread>>),
    RawPtr(usize),
}

impl KHardwareTimer {
    pub fn new() -> Self {
        Self {
            state: Mutex::new(KHardwareTimerState {
                base: KHardwareTimerBase::new(),
                m_wakeup_time: i64::MAX,
                m_event_type: None,
                core_timing: None,
                thread_ptrs: HashMap::new(),
                gsc: None,
            }),
        }
    }

    pub fn set_gsc(
        &mut self,
        gsc: Weak<Mutex<super::global_scheduler_context::GlobalSchedulerContext>>,
    ) {
        self.state.lock().unwrap().gsc = Some(gsc);
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
    pub fn wire_callback(timer: &Arc<KHardwareTimer>, core_timing: Arc<std::sync::Mutex<CoreTiming>>) {
        let timer_weak = Arc::downgrade(timer);
        let event_type = core_timing::create_event(
            "KHardwareTimer::Callback".to_string(),
            Box::new(move |_late, _ns| {
                if let Some(timer) = timer_weak.upgrade() {
                    timer.do_task();
                }
                None
            }),
        );
        let mut state = timer.state.lock().unwrap();
        state.m_event_type = Some(event_type);
        state.core_timing = Some(core_timing);
    }

    /// Unschedule the event and clean up.
    /// Matches upstream: `KHardwareTimer::Finalize()`
    pub fn finalize(&mut self) {
        let state = self.state.get_mut().unwrap();
        if let (Some(ref core_timing), Some(ref event_type)) =
            (&state.core_timing, &state.m_event_type)
        {
            core_timing
                .lock()
                .unwrap()
                .unschedule_event(event_type, UnscheduleEventType::NoWait);
        }
        state.m_wakeup_time = i64::MAX;
        state.m_event_type = None;
        state.core_timing = None;
        state.thread_ptrs.clear();
    }

    /// Get the current tick (global time in nanoseconds).
    /// Matches upstream: `KHardwareTimer::GetTick()`
    pub fn get_tick(&self) -> i64 {
        let state = self.state.lock().unwrap();
        if let Some(ref core_timing) = state.core_timing {
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
    pub fn register_absolute_task(&self, thread: &Arc<Mutex<KThread>>, task_time: i64) {
        let thread_id = {
            let mut thread_guard = thread.lock().unwrap();
            thread_guard.set_timer_task_time(task_time);
            thread_guard.get_thread_id()
        };
        let thread_ptr = {
            let mut thread_guard = thread.lock().unwrap();
            (&mut *thread_guard) as *mut KThread as usize
        };
        let had_current = super::kernel::with_current_thread_fast_mut(|t| {
            t.disable_dispatch();
        })
        .is_some();

        let mut state = self.state.lock().unwrap();
        state.thread_ptrs.insert(thread_id, thread_ptr);

        if state.base.register_absolute_task_impl(thread_id, task_time) {
            if task_time <= state.m_wakeup_time {
                self.enable_interrupt_locked(&mut state, task_time);
            }
        }
        if had_current {
            super::kernel::with_current_thread_fast_mut(|t| {
                t.enable_dispatch();
            });
        }
    }

    pub fn register_absolute_task_by_id(
        &self,
        thread_id: u64,
        thread_ptr: usize,
        task_time: i64,
    ) {
        log::trace!(
            "KHardwareTimer::register_absolute_task_by_id tid={} task_time={} ptr=0x{:x}",
            thread_id,
            task_time,
            thread_ptr
        );
        let had_current = super::kernel::with_current_thread_fast_mut(|t| {
            t.disable_dispatch();
        })
        .is_some();

        let mut state = self.state.lock().unwrap();
        state.thread_ptrs.insert(thread_id, thread_ptr);

        if state.base.register_absolute_task_impl(thread_id, task_time) {
            if task_time <= state.m_wakeup_time {
                self.enable_interrupt_locked(&mut state, task_time);
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
    pub fn cancel_task(&self, thread: &Arc<Mutex<KThread>>) {
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

        let mut state = self.state.lock().unwrap();
        if task_time > 0 {
            state.base.cancel_task(thread_id, task_time);
        }
        thread.lock().unwrap().set_timer_task_time(0);
        state.thread_ptrs.remove(&thread_id);
        if had_current {
            super::kernel::with_current_thread_fast_mut(|t| {
                t.enable_dispatch();
            });
        }
    }

    pub fn cancel_task_by_id(&self, thread_id: u64, task_time: i64) {
        let had_current = super::kernel::with_current_thread_fast_mut(|t| {
            t.disable_dispatch();
        })
        .is_some();

        let mut state = self.state.lock().unwrap();
        if task_time > 0 {
            state.base.cancel_task(thread_id, task_time);
        }
        state.thread_ptrs.remove(&thread_id);

        if had_current {
            super::kernel::with_current_thread_fast_mut(|t| {
                t.enable_dispatch();
            });
        }
    }

    /// Matches upstream: `KHardwareTimer::EnableInterrupt()`
    fn enable_interrupt_locked(&self, state: &mut KHardwareTimerState, wakeup_time: i64) {
        self.disable_interrupt_locked(state);

        state.m_wakeup_time = wakeup_time;
        if let (Some(ref core_timing), Some(ref event_type)) =
            (&state.core_timing, &state.m_event_type)
        {
            core_timing.lock().unwrap().schedule_event(
                Duration::from_nanos(wakeup_time as u64),
                event_type,
                true, // absolute time
            );
        }
    }

    /// Matches upstream: `KHardwareTimer::DisableInterrupt()`
    fn disable_interrupt_locked(&self, state: &mut KHardwareTimerState) {
        if let (Some(ref core_timing), Some(ref event_type)) =
            (&state.core_timing, &state.m_event_type)
        {
            core_timing
                .lock()
                .unwrap()
                .unschedule_event(event_type, UnscheduleEventType::NoWait);
        }
        state.m_wakeup_time = i64::MAX;
    }

    fn get_interrupt_enabled_locked(state: &KHardwareTimerState) -> bool {
        state.m_wakeup_time != i64::MAX
    }

    fn resolve_task_target(&self, task_id: u64) -> Option<TimerTaskTarget> {
        let state = self.state.lock().unwrap();
        if let Some(gsc) = state.gsc.as_ref().and_then(Weak::upgrade) {
            if let Some(thread) = gsc.lock().unwrap().get_thread_by_thread_id(task_id) {
                return Some(TimerTaskTarget::ThreadArc(thread));
            }
        }

        state.thread_ptrs
            .get(&task_id)
            .copied()
            .filter(|ptr| *ptr != 0)
            .map(TimerTaskTarget::RawPtr)
    }

    /// Called by the CoreTiming callback.
    /// Matches upstream: `KHardwareTimer::DoTask()`
    fn do_task(&self) {
        let gsc = self
            .state
            .lock()
            .unwrap()
            .gsc
            .as_ref()
            .and_then(Weak::upgrade);
        let scheduler_lock = gsc
            .as_ref()
            .map(|gsc| {
                let guard = gsc.lock().unwrap();
                std::ptr::addr_of!(guard.m_scheduler_lock)
            })
            .unwrap_or(std::ptr::null());

        let _scheduler_guard = if scheduler_lock.is_null() {
            None
        } else {
            Some(KScopedSchedulerLock::new(unsafe { &*scheduler_lock }))
        };
        let (deliveries, next_time) = {
            let mut state = self.state.lock().unwrap();

            if !Self::get_interrupt_enabled_locked(&state) {
                return;
            }

            // Disable the timer interrupt while we handle this.
            state.m_wakeup_time = i64::MAX;

            let cur_tick = if let Some(ref core_timing) = state.core_timing {
                core_timing.lock().unwrap().get_global_time_ns().as_nanos() as i64
            } else {
                0
            };
            let (fired_task_ids, next_time) = state.base.collect_expired_tasks(cur_tick);
            log::trace!(
                "KHardwareTimer::do_task cur_tick={} fired={:?} next_time={}",
                cur_tick,
                fired_task_ids,
                next_time
            );

            let mut deliveries = Vec::with_capacity(fired_task_ids.len());
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
                    state.thread_ptrs
                        .get(&task_id)
                        .copied()
                        .filter(|ptr| *ptr != 0)
                        .map(TimerTaskTarget::RawPtr)
                });

                if let Some(target) = target {
                    deliveries.push(target);
                }
                state.thread_ptrs.remove(&task_id);
            }

            (deliveries, next_time)
        };

        for target in deliveries {
            match target {
                TimerTaskTarget::ThreadArc(thread) => {
                    let mut thread = thread.lock().unwrap();
                    log::trace!(
                        "KHardwareTimer::do_task delivering tid={} state={:?} active_core={} current_core={}",
                        thread.get_thread_id(),
                        thread.get_state(),
                        thread.get_active_core(),
                        thread.get_current_core()
                    );
                    thread.set_timer_task_time(0);
                    thread.on_timer();
                }
                TimerTaskTarget::RawPtr(thread_ptr) => {
                    let thread = unsafe { &mut *(thread_ptr as *mut KThread) };
                    log::trace!(
                        "KHardwareTimer::do_task delivering raw tid={} state={:?} active_core={} current_core={}",
                        thread.get_thread_id(),
                        thread.get_state(),
                        thread.get_active_core(),
                        thread.get_current_core()
                    );
                    thread.set_timer_task_time(0);
                    thread.on_timer();
                }
            }
        }

        if next_time > 0 {
            let mut state = self.state.lock().unwrap();
            if next_time <= state.m_wakeup_time {
                self.enable_interrupt_locked(&mut state, next_time);
            }
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
    use crate::core_timing::CoreTiming;
    use crate::hle::kernel::global_scheduler_context::GlobalSchedulerContext;
    use crate::hle::kernel::k_thread::ThreadState;

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

    #[test]
    fn do_task_wakes_waiting_thread_via_gsc_owner() {
        let mut timer = KHardwareTimer::new();
        let gsc = Arc::new(Mutex::new(GlobalSchedulerContext::new()));
        let mut core_timing = CoreTiming::new();
        core_timing.set_multicore(true);
        let thread = Arc::new(Mutex::new(KThread::new()));
        {
            let mut guard = thread.lock().unwrap();
            guard.thread_id = 17;
            guard.begin_wait();
            guard.set_timer_task_time(10);
            guard.bind_self_reference(&thread);
        }
        gsc.lock().unwrap().add_thread(Arc::clone(&thread));
        timer.set_gsc(Arc::downgrade(&gsc));
        {
            let mut state = timer.state.lock().unwrap();
            state.core_timing = Some(Arc::new(Mutex::new(core_timing)));
            state.m_wakeup_time = 10;
            state.base.register_absolute_task_impl(17, 10);
        }

        timer.do_task();

        let guard = thread.lock().unwrap();
        assert_eq!(guard.get_state(), ThreadState::RUNNABLE);
        assert_eq!(
            guard.wait_result,
            crate::hle::kernel::svc::svc_results::RESULT_TIMED_OUT.get_inner_value()
        );
        assert_eq!(guard.get_timer_task_time(), 0);
    }
}
