// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: Copyright 2014 The Android Open Source Project
// SPDX-License-Identifier: GPL-3.0-or-later
// Parts of this implementation were based on:
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/include/gui/BufferQueueCore.h
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/libs/gui/BufferQueueCore.cpp

//! Port of zuyu/src/core/hle/service/nvnflinger/buffer_queue_core.h
//! Port of zuyu/src/core/hle/service/nvnflinger/buffer_queue_core.cpp

use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Condvar, Mutex, OnceLock, Weak};
use std::time::Instant;

use crate::hle::kernel::k_scheduler::KScheduler;
use crate::hle::kernel::k_scheduler_lock::KScopedSchedulerLock;
use crate::hle::kernel::k_scoped_scheduler_lock_and_sleep::KScopedSchedulerLockAndSleep;
use crate::hle::kernel::k_thread::{
    KThread, KThreadLock, ThreadState, ThreadWaitReasonForDebugging,
};
use crate::hle::kernel::k_thread_queue::KThreadQueue;
use crate::hle::kernel::kernel as kernel_globals;
use crate::hle::result::RESULT_SUCCESS;

use super::buffer_item::BufferItem;
use super::buffer_queue_defs::{self, SlotsType, NUM_BUFFER_SLOTS};
use super::buffer_slot::BufferState;
use super::consumer_listener::IConsumerListener;
use super::pixel_format::PixelFormat;
use super::producer_listener::IProducerListener;
use super::status::Status;
use super::ui::fence::Fence;
use super::window::NativeWindowApi;

pub struct BufferQueueCore {
    pub mutex: Mutex<BufferQueueCoreInner>,
    pub dequeue_condition: Condvar,
    pub dequeue_possible: AtomicBool,
    pub is_allocating_condition: Condvar,
    /// Guest threads parked in `wait_for_dequeue_condition`.
    ///
    /// Ruzu divergence (documented): upstream runs `DequeueBuffer` on a
    /// dedicated nvnflinger service host thread, so blocking on
    /// `dequeue_condition` only blocks that host thread. Ruzu's default IPC
    /// path runs the handler inline on the calling guest core's fiber;
    /// blocking the host condvar there freezes every other guest fiber
    /// multiplexed on that core (measured: MK8D pins its loading workers to
    /// core 1 alongside the graphics producer, and the frozen core stretched
    /// the first logo from ~3-4s to ~80s — see TODO.md 2026-07-05 ROOT
    /// CAUSE). Guest callers therefore park in the kernel (`begin_wait` +
    /// fiber reschedule) instead, and `signal_dequeue_condition` ends their
    /// wait here in addition to notifying the host condvar.
    dequeue_parked_threads: Mutex<Vec<Weak<KThreadLock>>>,
}

/// Safety-net timeout for a parked dequeue waiter. A missed wake merely
/// costs one re-check of the slot loop after this delay; the normal path is
/// woken by `signal_dequeue_condition` well before it (one frame ≈ 16ms).
const DEQUEUE_PARK_TIMEOUT_NS: i64 = 10_000_000;

/// `RUZU_BQ_DEQUEUE_HOST_WAIT=1` — force the legacy host-condvar wait even
/// for guest-core callers (diagnostic kill-switch for the fiber-park path).
fn host_wait_forced() -> bool {
    static FORCED: OnceLock<bool> = OnceLock::new();
    *FORCED.get_or_init(|| std::env::var_os("RUZU_BQ_DEQUEUE_HOST_WAIT").is_some())
}

pub struct BufferQueueCoreInner {
    pub is_abandoned: bool,
    pub consumer_controlled_by_app: bool,
    pub consumer_listener: Option<Arc<dyn IConsumerListener>>,
    pub consumer_usage_bit: u32,
    pub connected_api: NativeWindowApi,
    pub connected_producer_listener: Option<Arc<dyn IProducerListener>>,
    pub slots: Box<SlotsType>,
    pub queue: Vec<BufferItem>,
    pub override_max_buffer_count: i32,
    /// This is always disabled on HOS
    pub use_async_buffer: bool,
    pub dequeue_buffer_cannot_block: bool,
    pub default_buffer_format: PixelFormat,
    pub default_width: u32,
    pub default_height: u32,
    pub default_max_buffer_count: i32,
    /// This is always zero on HOS
    pub max_acquired_buffer_count: i32,
    pub buffer_has_been_queued: bool,
    pub frame_counter: u64,
    pub transform_hint: u32,
    pub is_allocating: bool,
}

static BQP_WAIT_SIGNAL_COUNT: AtomicU64 = AtomicU64::new(0);
static BQP_WAIT_ENTER_COUNT: AtomicU64 = AtomicU64::new(0);
static BQP_WAIT_RETURN_COUNT: AtomicU64 = AtomicU64::new(0);
static BQP_WAIT_PRE_TRUE_COUNT: AtomicU64 = AtomicU64::new(0);
static BQP_WAIT_TOTAL_US: AtomicU64 = AtomicU64::new(0);
static BQP_WAIT_MAX_US: AtomicU64 = AtomicU64::new(0);

fn bqp_wait_profile_enabled() -> bool {
    std::env::var_os("RUZU_PROFILE_BQP_WAIT").is_some()
}

fn update_max(target: &AtomicU64, value: u64) {
    let mut current = target.load(Ordering::Relaxed);
    while value > current {
        match target.compare_exchange_weak(current, value, Ordering::Relaxed, Ordering::Relaxed) {
            Ok(_) => break,
            Err(next) => current = next,
        }
    }
}

impl BufferQueueCore {
    pub const INVALID_BUFFER_SLOT: i32 = BufferItem::INVALID_BUFFER_SLOT;

    pub fn new() -> Arc<Self> {
        Arc::new(Self {
            mutex: Mutex::new(BufferQueueCoreInner {
                is_abandoned: false,
                consumer_controlled_by_app: false,
                consumer_listener: None,
                consumer_usage_bit: 0,
                connected_api: NativeWindowApi::NoConnectedApi,
                connected_producer_listener: None,
                slots: buffer_queue_defs::new_slots(),
                queue: Vec::new(),
                override_max_buffer_count: 0,
                use_async_buffer: false,
                dequeue_buffer_cannot_block: false,
                default_buffer_format: PixelFormat::Rgba8888,
                default_width: 1,
                default_height: 1,
                default_max_buffer_count: 2,
                max_acquired_buffer_count: 0,
                buffer_has_been_queued: false,
                frame_counter: 0,
                transform_hint: 0,
                is_allocating: false,
            }),
            dequeue_condition: Condvar::new(),
            dequeue_possible: AtomicBool::new(false),
            is_allocating_condition: Condvar::new(),
            dequeue_parked_threads: Mutex::new(Vec::new()),
        })
    }

    pub fn signal_dequeue_condition(&self) {
        if bqp_wait_profile_enabled() {
            BQP_WAIT_SIGNAL_COUNT.fetch_add(1, Ordering::Relaxed);
        }
        // Order matters for the lost-wakeup guard in the guest park path:
        // `dequeue_possible` must be visible before the parked list is
        // drained, so a parker that misses the drain still observes the flag
        // under the scheduler lock and cancels its sleep.
        self.dequeue_possible.store(true, Ordering::Release);
        self.dequeue_condition.notify_all();
        self.wake_parked_dequeue_waiters();
    }

    /// End the kernel wait of every guest thread parked by
    /// `wait_for_dequeue_condition` (ruzu divergence, see the field doc on
    /// `dequeue_parked_threads`).
    fn wake_parked_dequeue_waiters(&self) {
        let waiters: Vec<Weak<KThreadLock>> = {
            let mut list = self.dequeue_parked_threads.lock().unwrap();
            if list.is_empty() {
                return;
            }
            list.drain(..).collect()
        };
        let Some(scheduler_lock) = kernel_globals::scheduler_lock() else {
            return;
        };

        // Callers of signal_dequeue_condition hold `self.mutex`. When the
        // signaling context is itself a guest core fiber, the scheduler-lock
        // release below would otherwise preempt inline to the woken waiter,
        // which immediately re-locks `self.mutex` — deadlocking the host core
        // thread on a Mutex owned by the fiber it just switched away from.
        // Raising the dispatch-disable count defers only the current-core
        // switch to the caller's post-SVC reschedule; cross-core wakeups
        // (`reschedule_other_cores`) still fire inside `enable_scheduling`.
        let defer_local_switch = kernel_globals::get_kernel_ref()
            .is_some_and(|kernel| kernel.is_current_thread_guest_core());
        if defer_local_switch {
            kernel_globals::with_current_thread_fast_mut(|t| t.disable_dispatch());
        }
        {
            // Same lock order as `KReadableEvent::signal`: scheduler lock
            // first, then each thread.
            let _sl = KScopedSchedulerLock::new(scheduler_lock);
            for weak in waiters {
                if let Some(thread) = weak.upgrade() {
                    // `end_wait` no-ops when the thread is not (or no longer)
                    // waiting, so stale entries are harmless.
                    thread
                        .lock()
                        .unwrap()
                        .end_wait(RESULT_SUCCESS.get_inner_value());
                }
            }
        }
        if defer_local_switch {
            kernel_globals::with_current_thread_fast_mut(|t| {
                if t.get_disable_dispatch_count() > 0 {
                    t.enable_dispatch();
                }
            });
        }
    }

    pub fn wait_for_dequeue_condition<'a>(
        &'a self,
        guard: std::sync::MutexGuard<'a, BufferQueueCoreInner>,
    ) -> std::sync::MutexGuard<'a, BufferQueueCoreInner> {
        let profile = bqp_wait_profile_enabled();
        let start = if profile {
            BQP_WAIT_ENTER_COUNT.fetch_add(1, Ordering::Relaxed);
            if self.dequeue_possible.load(Ordering::Acquire) {
                BQP_WAIT_PRE_TRUE_COUNT.fetch_add(1, Ordering::Relaxed);
            }
            Some(Instant::now())
        } else {
            None
        };
        // Guest core fibers park in the kernel so the other fibers on the
        // core keep running (ruzu divergence, see `dequeue_parked_threads`).
        // Host threads (env-gated host-thread IPC routing, unit tests) keep
        // the upstream host-condvar wait. Both paths may return spuriously
        // (safety-net timeout on the park path); the caller's slot loop in
        // `wait_for_free_slot_then_relock` re-derives `try_again` either way,
        // matching condvar spurious-wakeup semantics.
        let guard = match self.try_wait_as_parked_guest_thread(guard) {
            Ok(guard) => guard,
            Err(guard) => self
                .dequeue_condition
                .wait_while(guard, |_| !self.dequeue_possible.load(Ordering::Acquire))
                .unwrap(),
        };
        self.dequeue_possible.store(false, Ordering::Release);
        if let Some(start) = start {
            let elapsed_us = start.elapsed().as_micros().min(u128::from(u64::MAX)) as u64;
            BQP_WAIT_RETURN_COUNT.fetch_add(1, Ordering::Relaxed);
            BQP_WAIT_TOTAL_US.fetch_add(elapsed_us, Ordering::Relaxed);
            update_max(&BQP_WAIT_MAX_US, elapsed_us);
        }
        guard
    }

    /// Park the calling guest thread in the kernel until
    /// `signal_dequeue_condition` (or the safety-net timer) wakes it, yielding
    /// the fiber so the other guest threads on this core keep running.
    ///
    /// Returns `Err(guard)` untouched when the caller is not a schedulable
    /// guest core thread; the caller then falls back to the host condvar.
    /// The park sequence mirrors the proven host-thread IPC park in
    /// `svc_ipc.rs` (`begin_wait` under `KScopedSchedulerLockAndSleep`, then
    /// a current-core fiber reschedule).
    fn try_wait_as_parked_guest_thread<'a>(
        &'a self,
        guard: std::sync::MutexGuard<'a, BufferQueueCoreInner>,
    ) -> Result<
        std::sync::MutexGuard<'a, BufferQueueCoreInner>,
        std::sync::MutexGuard<'a, BufferQueueCoreInner>,
    > {
        if host_wait_forced() {
            return Err(guard);
        }
        let Some(kernel) = kernel_globals::get_kernel_ref() else {
            return Err(guard);
        };
        if !kernel.is_current_thread_guest_core() {
            return Err(guard);
        }
        let Some(thread) = kernel_globals::get_current_emu_thread() else {
            return Err(guard);
        };
        if thread.lock().unwrap().is_dummy_thread() {
            return Err(guard);
        }
        let Some(scheduler_lock) = kernel_globals::scheduler_lock() else {
            return Err(guard);
        };
        let Some(scheduler) = kernel.current_scheduler().cloned() else {
            return Err(guard);
        };

        // Drop the queue mutex before parking: the wake path (consumer
        // release / vsync composition) needs it, and holding a host
        // MutexGuard across a fiber switch would deadlock the host core
        // thread when the next fiber re-locks it.
        drop(guard);

        // Register as a waiter BEFORE re-checking the flag. The waker sets
        // `dequeue_possible` before draining the list and ends waits under
        // the scheduler lock, so either the flag check below (under that same
        // lock) observes the signal, or the drain observes this entry.
        self.dequeue_parked_threads
            .lock()
            .unwrap()
            .push(Arc::downgrade(&thread));

        let (thread_id, thread_ptr) = {
            let thread = thread.lock().unwrap();
            (thread.thread_id, &*thread as *const KThread as usize)
        };
        let timeout_tick = kernel_globals::get_current_hardware_tick()
            .map(|tick| tick.saturating_add(DEQUEUE_PARK_TIMEOUT_NS))
            .unwrap_or(-1);
        let hardware_timer = kernel_globals::get_hardware_timer_arc();

        let parked = {
            let (mut sleep_guard, timer) = KScopedSchedulerLockAndSleep::new(
                scheduler_lock,
                hardware_timer.as_ref(),
                thread_id,
                thread_ptr,
                timeout_tick,
            );
            if self.dequeue_possible.load(Ordering::Acquire) {
                sleep_guard.cancel_sleep();
                false
            } else {
                let mut thread = thread.lock().unwrap();
                if thread.is_termination_requested() {
                    sleep_guard.cancel_sleep();
                    false
                } else {
                    let mut wait_queue = KThreadQueue::new();
                    if let Some(timer) = timer {
                        wait_queue.set_hardware_timer(timer);
                    }
                    thread.begin_wait_with_queue(wait_queue);
                    thread.set_wait_reason_for_debugging(
                        ThreadWaitReasonForDebugging::Synchronization,
                    );
                    true
                }
            }
            // `sleep_guard` drops here: registers the timer task and releases
            // the scheduler lock, which may already perform the fiber switch
            // (dispatch count permitting).
        };

        if parked {
            // If the scheduler-lock release above did not switch (nested
            // dispatch-disable scope), force the current-core reschedule now,
            // exactly like the host-thread IPC park in svc_ipc.rs. When the
            // release did switch, we resume here already RUNNABLE and must
            // not re-yield through a stale scheduler pointer (the thread may
            // in principle resume on a different core).
            let still_waiting = thread.lock().unwrap().get_state() == ThreadState::WAITING;
            if still_waiting {
                let sched_ptr = {
                    let mut scheduler = scheduler.lock().unwrap();
                    &mut *scheduler as *mut KScheduler
                };
                unsafe {
                    KScheduler::reschedule_current_core_raw(sched_ptr);
                }
            }
        }

        // Drop our stale entry (the timeout wake leaves it registered).
        self.dequeue_parked_threads
            .lock()
            .unwrap()
            .retain(|weak| !std::ptr::eq(weak.as_ptr(), Arc::as_ptr(&thread)));

        Ok(self.mutex.lock().unwrap())
    }

    pub fn wait_while_allocating_locked<'a>(
        &self,
        guard: std::sync::MutexGuard<'a, BufferQueueCoreInner>,
    ) -> std::sync::MutexGuard<'a, BufferQueueCoreInner> {
        self.is_allocating_condition
            .wait_while(guard, |inner| inner.is_allocating)
            .unwrap()
    }
}

pub fn dump_bqp_wait_profile() {
    if !bqp_wait_profile_enabled() {
        return;
    }
    let signals = BQP_WAIT_SIGNAL_COUNT.load(Ordering::Relaxed);
    let enters = BQP_WAIT_ENTER_COUNT.load(Ordering::Relaxed);
    let returns = BQP_WAIT_RETURN_COUNT.load(Ordering::Relaxed);
    let pre_true = BQP_WAIT_PRE_TRUE_COUNT.load(Ordering::Relaxed);
    let total_us = BQP_WAIT_TOTAL_US.load(Ordering::Relaxed);
    let max_us = BQP_WAIT_MAX_US.load(Ordering::Relaxed);
    let avg_us = if returns != 0 { total_us / returns } else { 0 };
    eprintln!(
        "[BQP_WAIT_PROFILE] signals={} wait_enters={} wait_returns={} pre_true={} total_us={} avg_us={} max_us={}",
        signals, enters, returns, pre_true, total_us, avg_us, max_us
    );
}

impl BufferQueueCoreInner {
    pub fn get_min_undequeued_buffer_count_locked(&self, async_flag: bool) -> i32 {
        // If DequeueBuffer is allowed to error out, we don't have to add an extra buffer.
        if !self.use_async_buffer {
            return 0;
        }
        if self.dequeue_buffer_cannot_block || async_flag {
            return self.max_acquired_buffer_count + 1;
        }
        self.max_acquired_buffer_count
    }

    pub fn get_min_max_buffer_count_locked(&self, async_flag: bool) -> i32 {
        self.get_min_undequeued_buffer_count_locked(async_flag)
    }

    pub fn get_max_buffer_count_locked(&self, async_flag: bool) -> i32 {
        let min_buffer_count = self.get_min_max_buffer_count_locked(async_flag);
        let mut max_buffer_count = self.default_max_buffer_count.max(min_buffer_count);

        if self.override_max_buffer_count != 0 {
            assert!(self.override_max_buffer_count >= min_buffer_count);
            return self.override_max_buffer_count;
        }

        // Any buffers that are dequeued by the producer or sitting in the queue waiting to be
        // consumed need to have their slots preserved.
        for slot in (max_buffer_count as usize)..NUM_BUFFER_SLOTS {
            let state = self.slots[slot].buffer_state;
            if state == BufferState::Queued || state == BufferState::Dequeued {
                max_buffer_count = slot as i32 + 1;
            }
        }

        max_buffer_count
    }

    pub fn get_preallocated_buffer_count_locked(&self) -> i32 {
        self.slots.iter().filter(|s| s.is_preallocated).count() as i32
    }

    pub fn free_buffer_locked(&mut self, slot: i32) {
        log::debug!("BufferQueueCore: free_buffer_locked slot {}", slot);
        let s = slot as usize;
        self.slots[s].graphic_buffer = None;

        if self.slots[s].buffer_state == BufferState::Acquired {
            self.slots[s].needs_cleanup_on_release = true;
        }

        self.slots[s].buffer_state = BufferState::Free;
        self.slots[s].frame_number = u32::MAX as u64;
        self.slots[s].acquire_called = false;
        self.slots[s].fence = Fence::no_fence();
    }

    pub fn free_all_buffers_locked(&mut self) {
        self.buffer_has_been_queued = false;
        for slot in 0..NUM_BUFFER_SLOTS as i32 {
            self.free_buffer_locked(slot);
        }
    }

    pub fn still_tracking(&self, item: &BufferItem) -> bool {
        let slot = &self.slots[item.slot as usize];
        match (&slot.graphic_buffer, &item.graphic_buffer) {
            (Some(slot_buf), Some(item_buf)) => {
                // Compare by pointer identity (same Arc)
                Arc::ptr_eq(slot_buf, item_buf)
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::mpsc;
    use std::time::Duration;

    #[test]
    fn wait_for_dequeue_condition_falls_back_to_host_condvar_without_kernel() {
        // Host threads (no kernel / no guest core context) must keep the
        // upstream host-condvar wait; `signal_dequeue_condition` (which also
        // walks the — empty — parked-thread list) must wake them.
        let core = BufferQueueCore::new();

        let (waiting_tx, waiting_rx) = mpsc::channel();
        let waiter_core = Arc::clone(&core);
        let waiter = std::thread::spawn(move || {
            let guard = waiter_core.mutex.lock().unwrap();
            waiting_tx.send(()).unwrap();
            let _guard = waiter_core.wait_for_dequeue_condition(guard);
            assert!(!waiter_core.dequeue_possible.load(Ordering::Acquire));
        });

        waiting_rx.recv_timeout(Duration::from_secs(1)).unwrap();
        std::thread::sleep(Duration::from_millis(10));
        core.signal_dequeue_condition();

        waiter.join().unwrap();
    }

    #[test]
    fn wait_while_allocating_locked_blocks_until_condition_is_signaled() {
        let core = BufferQueueCore::new();
        core.mutex.lock().unwrap().is_allocating = true;

        let (waiting_tx, waiting_rx) = mpsc::channel();
        let waiter_core = Arc::clone(&core);
        let waiter = std::thread::spawn(move || {
            let guard = waiter_core.mutex.lock().unwrap();
            waiting_tx.send(()).unwrap();
            let guard = waiter_core.wait_while_allocating_locked(guard);
            assert!(!guard.is_allocating);
        });

        waiting_rx.recv_timeout(Duration::from_secs(1)).unwrap();
        std::thread::sleep(Duration::from_millis(10));
        {
            let mut guard = core.mutex.lock().unwrap();
            guard.is_allocating = false;
        }
        core.is_allocating_condition.notify_all();

        waiter.join().unwrap();
    }
}
