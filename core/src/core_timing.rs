//! Port of zuyu/src/core/core_timing.h and zuyu/src/core/core_timing.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! The timing system. Manages timed events by scheduling callbacks at specific
//! CPU tick counts. Uses a priority queue (BinaryHeap) of events, and can
//! schedule/unschedule events and advance time. This is CRITICAL for emulation.

use common::thread::Event;
use common::wall_clock::{self, WallClock};
use parking_lot::Mutex;
use std::cmp::Ordering as CmpOrdering;
use std::collections::BinaryHeap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Weak};
use std::time::Duration;

/// Maximum slice length for single-core timing.
const MAX_SLICE_LENGTH: i64 = 10000;

/// A callback that may be scheduled for a particular core timing event.
/// Returns an optional reschedule time in nanoseconds.
pub type TimedCallback = Box<dyn Fn(i64, Duration) -> Option<Duration> + Send + Sync>;

/// Contains the characteristics of a particular event.
pub struct EventType {
    /// The event's callback function.
    pub callback: TimedCallback,
    /// The name of the event (for debugging).
    pub name: String,
    /// A monotonic sequence number, incremented when this event is changed externally.
    pub sequence_number: usize,
}

impl EventType {
    pub fn new(callback: TimedCallback, name: String) -> Self {
        Self {
            callback,
            name,
            sequence_number: 0,
        }
    }
}

/// Creates a core timing event with the given name and callback.
pub fn create_event(name: String, callback: TimedCallback) -> Arc<Mutex<EventType>> {
    Arc::new(Mutex::new(EventType::new(callback, name)))
}

/// Whether to wait for in-progress events when unscheduling.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnscheduleEventType {
    Wait,
    NoWait,
}

/// A scheduled event in the timing queue.
struct TimingEvent {
    /// Time in nanoseconds when this event fires.
    time: i64,
    /// FIFO ordering for events at the same time.
    fifo_order: u64,
    /// Weak reference to the event type.
    event_type: Weak<Mutex<EventType>>,
    /// If non-zero, this event auto-reschedules with this interval.
    reschedule_time: i64,
}

impl PartialEq for TimingEvent {
    fn eq(&self, other: &Self) -> bool {
        self.time == other.time && self.fifo_order == other.fifo_order
    }
}

impl Eq for TimingEvent {}

// BinaryHeap is a max-heap; we want the earliest event first,
// so we reverse the ordering (smallest time = highest priority).
impl PartialOrd for TimingEvent {
    fn partial_cmp(&self, other: &Self) -> Option<CmpOrdering> {
        Some(self.cmp(other))
    }
}

impl Ord for TimingEvent {
    fn cmp(&self, other: &Self) -> CmpOrdering {
        // Reverse ordering: earlier time = higher priority
        other
            .time
            .cmp(&self.time)
            .then_with(|| other.fifo_order.cmp(&self.fifo_order))
    }
}

/// The core timing system.
///
/// This is the system to schedule events into the emulated machine's future.
/// Time is measured in nanoseconds (globally) and CPU ticks (for single-core mode).
struct CoreTimingState {
    global_timer: i64,
    event_queue: BinaryHeap<TimingEvent>,
    event_fifo_id: u64,
    pause_end_time: i64,
    cpu_ticks: u64,
    downcount: i64,
}

pub struct CoreTiming {
    clock: Box<dyn WallClock>,

    state: Mutex<CoreTimingState>,

    event: Event,
    pause_event: Event,
    basic_lock: Mutex<()>,
    advance_lock: Mutex<()>,
    timer_thread: Mutex<Option<std::thread::JoinHandle<()>>>,
    paused: AtomicBool,
    paused_set: AtomicBool,
    wait_set: AtomicBool,
    shutting_down: AtomicBool,
    has_started: AtomicBool,
    on_thread_init: Mutex<Option<Box<dyn Fn() + Send>>>,

    is_multicore: AtomicBool,
}

impl CoreTiming {
    pub fn new() -> Self {
        Self {
            clock: wall_clock::create_optimal_clock(),
            state: Mutex::new(CoreTimingState {
                global_timer: 0,
                event_queue: BinaryHeap::new(),
                event_fifo_id: 0,
                pause_end_time: 0,
                cpu_ticks: 0,
                downcount: MAX_SLICE_LENGTH,
            }),
            event: Event::new(),
            pause_event: Event::new(),
            basic_lock: Mutex::new(()),
            advance_lock: Mutex::new(()),
            timer_thread: Mutex::new(None),
            paused: AtomicBool::new(false),
            paused_set: AtomicBool::new(false),
            wait_set: AtomicBool::new(false),
            shutting_down: AtomicBool::new(false),
            has_started: AtomicBool::new(false),
            on_thread_init: Mutex::new(None),
            is_multicore: AtomicBool::new(false),
        }
    }

    /// Sets if emulation is multicore or single core. Must be set before Initialize.
    pub fn set_multicore(&self, is_multicore: bool) {
        self.is_multicore.store(is_multicore, Ordering::SeqCst);
    }

    /// CoreTiming begins at the boundary of timing slice -1. An initial call to Advance() is
    /// required to end slice -1 and start slice 0 before the first cycle of code is executed.
    pub fn initialize<F: Fn() + Send + 'static>(self: &Arc<Self>, on_thread_init: F) {
        self.reset();
        *self.on_thread_init.lock() = Some(Box::new(on_thread_init));
        {
            let mut state = self.state.lock();
            state.event_fifo_id = 0;
            state.cpu_ticks = 0;
        }
        self.shutting_down.store(false, Ordering::SeqCst);

        if !self.is_multicore.load(Ordering::SeqCst) {
            return;
        }

        let ct = self.clone();

        let handle = std::thread::Builder::new()
            .name("CoreTiming".to_string())
            .spawn(move || {
                // Upstream: ThreadEntry calls on_thread_init() then ThreadLoop().
                if let Some(ref init_fn) = *ct.on_thread_init.lock() {
                    init_fn();
                }

                // ThreadLoop — matches upstream CoreTiming::ThreadLoop().
                // Rust still uses interior locks for mutable state, but the
                // timing thread is now owned and spawned by initialize().
                ct.has_started.store(true, Ordering::SeqCst);
                // Ensure the timer thread starts unpaused.
                // sync_pause(false) from System::run() may not have been called yet.
                ct.paused.store(false, Ordering::SeqCst);
                while !ct.shutting_down.load(Ordering::SeqCst) {
                    while !ct.paused.load(Ordering::SeqCst) {
                        ct.paused_set.store(false, Ordering::SeqCst);

                        let next_time = ct.advance();
                        if let Some(next_ns) = next_time {
                            let now_ns = ct.get_global_time_ns().as_nanos() as i64;
                            let wait_time = next_ns - now_ns;
                            if std::env::var_os("RUZU_TRACE_CT_FIRE").is_some() {
                                log::info!(
                                    "CT_LOOP next_ns={} now_ns={} wait_ns={}",
                                    next_ns,
                                    now_ns,
                                    wait_time
                                );
                            }
                            if wait_time > 0 {
                                // Match upstream ThreadLoop: wait interruptibly so newly
                                // scheduled earlier events wake the timer thread
                                // immediately.
                                ct.event
                                    .wait_for(std::time::Duration::from_nanos(wait_time as u64));
                            }
                        } else {
                            if std::env::var_os("RUZU_TRACE_CT_FIRE").is_some() {
                                log::info!("CT_LOOP queue_empty, parking");
                            }
                            ct.wait_set.store(true, Ordering::SeqCst);
                            ct.event.wait();
                        }
                        ct.wait_set.store(false, Ordering::SeqCst);
                    }

                    ct.paused_set.store(true, Ordering::SeqCst);
                    ct.pause_event.wait();
                }
            })
            .expect("Failed to spawn CoreTiming timer thread");

        *self.timer_thread.lock() = Some(handle);
        log::info!("CoreTiming: timer thread started");
    }

    /// Clear all pending events. This should ONLY be done on exit.
    pub fn clear_pending_events(&self) {
        let _adv = self.advance_lock.lock();
        let _basic = self.basic_lock.lock();
        self.state.lock().event_queue.clear();
        self.event.set();
    }

    /// Pauses/Unpauses the execution of the timer thread.
    pub fn pause(&self, is_paused: bool) {
        self.paused.store(is_paused, Ordering::SeqCst);
        self.pause_event.set();

        if !is_paused {
            self.state.lock().pause_end_time = self.get_global_time_ns().as_nanos() as i64;
        }
    }

    /// Pauses/Unpauses the execution of the timer thread and waits until paused.
    pub fn sync_pause(&self, is_paused: bool) {
        if is_paused == self.paused.load(Ordering::SeqCst)
            && self.paused_set.load(Ordering::SeqCst) == is_paused
        {
            return;
        }

        self.pause(is_paused);

        if self.timer_thread.lock().is_some() {
            if !is_paused {
                self.pause_event.set();
            }
            self.event.set();
            // Spin-wait for the timer thread to acknowledge the pause state
            while self.paused_set.load(Ordering::SeqCst) != is_paused {
                std::hint::spin_loop();
            }
        }

        if !is_paused {
            self.state.lock().pause_end_time = self.get_global_time_ns().as_nanos() as i64;
        }
    }

    /// Checks if core timing is running.
    pub fn is_running(&self) -> bool {
        !self.paused_set.load(Ordering::SeqCst)
    }

    /// Checks if the timer thread has started.
    pub fn has_started(&self) -> bool {
        self.has_started.load(Ordering::SeqCst)
    }

    /// Checks if there are any pending time events.
    pub fn has_pending_events(&self) -> bool {
        let _lock = self.basic_lock.lock();
        !(self.wait_set.load(Ordering::SeqCst) && self.state.lock().event_queue.is_empty())
    }

    /// Schedules an event in core timing.
    pub fn schedule_event(
        &self,
        ns_into_future: Duration,
        event_type: &Arc<Mutex<EventType>>,
        absolute_time: bool,
    ) {
        let _lock = self.basic_lock.lock();
        let next_time = if absolute_time {
            ns_into_future
        } else {
            self.get_global_time_ns() + ns_into_future
        };

        let mut state = self.state.lock();
        let fifo_order = state.event_fifo_id;
        state.event_queue.push(TimingEvent {
            time: next_time.as_nanos() as i64,
            fifo_order,
            event_type: Arc::downgrade(event_type),
            reschedule_time: 0,
        });
        state.event_fifo_id += 1;
        drop(_lock);
        self.event.set();
    }

    /// Schedules an event which will automatically re-schedule itself with the given time,
    /// until unscheduled.
    pub fn schedule_looping_event(
        &self,
        start_time: Duration,
        resched_time: Duration,
        event_type: &Arc<Mutex<EventType>>,
        absolute_time: bool,
    ) {
        let _lock = self.basic_lock.lock();
        let next_time = if absolute_time {
            start_time
        } else {
            self.get_global_time_ns() + start_time
        };

        let mut state = self.state.lock();
        let fifo_order = state.event_fifo_id;
        state.event_queue.push(TimingEvent {
            time: next_time.as_nanos() as i64,
            fifo_order,
            event_type: Arc::downgrade(event_type),
            reschedule_time: resched_time.as_nanos() as i64,
        });
        state.event_fifo_id += 1;
        drop(_lock);
        self.event.set();
    }

    /// Unschedules all instances of the given event type.
    pub fn unschedule_event(
        &self,
        event_type: &Arc<Mutex<EventType>>,
        unsched_type: UnscheduleEventType,
    ) {
        {
            let _lock = self.basic_lock.lock();

            let event_type_ptr = Arc::as_ptr(event_type);
            // Rebuild the queue without the matching events
            let mut state = self.state.lock();
            let old_queue = std::mem::take(&mut state.event_queue);
            for evt in old_queue.into_iter() {
                if let Some(strong) = evt.event_type.upgrade() {
                    if Arc::as_ptr(&strong) != event_type_ptr {
                        state.event_queue.push(evt);
                    }
                }
                // Drop events with dead weak refs too
            }

            event_type.lock().sequence_number += 1;
        }

        // Force any in-progress events to finish
        if unsched_type == UnscheduleEventType::Wait {
            let _lock = self.advance_lock.lock();
        }
    }

    /// Adds ticks to the CPU tick counter and decrements downcount.
    pub fn add_ticks(&self, ticks_to_add: u64) {
        let mut state = self.state.lock();
        state.cpu_ticks += ticks_to_add;
        state.downcount -= ticks_to_add as i64;
    }

    /// Resets the tick downcount to the max slice length.
    pub fn reset_ticks(&self) {
        self.state.lock().downcount = MAX_SLICE_LENGTH;
    }

    /// Adds idle ticks.
    pub fn idle(&self) {
        self.state.lock().cpu_ticks += 1000;
    }

    /// Returns the current downcount.
    pub fn get_downcount(&self) -> i64 {
        self.state.lock().downcount
    }

    /// Returns the current CNTPCT tick value.
    pub fn get_clock_ticks(&self) -> u64 {
        if self.is_multicore.load(Ordering::SeqCst) {
            self.clock.get_cntpct() as u64
        } else {
            wall_clock::cpu_tick_to_cntpct(self.state.lock().cpu_ticks)
        }
    }

    /// Returns the current GPU tick value.
    pub fn get_gpu_ticks(&self) -> u64 {
        if self.is_multicore.load(Ordering::SeqCst) {
            self.clock.get_gpu_tick() as u64
        } else {
            wall_clock::cpu_tick_to_gpu_tick(self.state.lock().cpu_ticks)
        }
    }

    /// Returns current time in nanoseconds.
    pub fn get_global_time_ns(&self) -> Duration {
        if self.is_multicore.load(Ordering::SeqCst) {
            self.clock.get_time_ns()
        } else {
            Duration::from_nanos(wall_clock::cpu_tick_to_ns(self.state.lock().cpu_ticks))
        }
    }

    /// Returns current time in microseconds.
    pub fn get_global_time_us(&self) -> Duration {
        if self.is_multicore.load(Ordering::SeqCst) {
            self.clock.get_time_us()
        } else {
            Duration::from_micros(wall_clock::cpu_tick_to_us(self.state.lock().cpu_ticks))
        }
    }

    /// Checks for events manually and returns time in nanoseconds for next event.
    /// This is the core advance function that fires all due events.
    ///
    /// The C++ version holds both advance_lock and basic_lock, unlocking basic_lock
    /// around callbacks. In Rust, since we have &mut self (exclusive access),
    /// we use the advance_lock for external synchronization and handle the
    /// basic_lock at a coarser granularity.
    pub fn advance(&self) -> Option<i64> {
        let _adv_lock = self.advance_lock.lock();
        self.state.lock().global_timer = self.get_global_time_ns().as_nanos() as i64;

        loop {
            // Check under basic_lock if there's a due event
            let maybe_evt = {
                let _basic = self.basic_lock.lock();
                let mut state = self.state.lock();
                match state.event_queue.peek() {
                    Some(evt) if evt.time <= state.global_timer => state.event_queue.pop(),
                    _ => None,
                }
            };

            let evt = match maybe_evt {
                Some(e) => e,
                None => break,
            };

            let upgraded = evt.event_type.upgrade();
            if upgraded.is_none() && std::env::var_os("RUZU_TRACE_CT_FIRE").is_some() {
                log::info!(
                    "CT_DROP_WEAK time={} reschedule={}",
                    evt.time,
                    evt.reschedule_time
                );
            }
            if let Some(event_type_arc) = upgraded {
                let evt_time = evt.time;
                let evt_sequence_num = event_type_arc.lock().sequence_number;

                // Fire the callback without holding basic_lock
                let ns_late = self.get_global_time_ns().as_nanos() as i64 - evt_time;
                if std::env::var_os("RUZU_TRACE_CT_FIRE").is_some() {
                    let name = event_type_arc.lock().name.clone();
                    log::info!(
                        "CT_FIRE name={} time={} reschedule={}",
                        name,
                        evt_time,
                        evt.reschedule_time
                    );
                }
                let callback_result = {
                    let et = event_type_arc.lock();
                    (et.callback)(evt_time, Duration::from_nanos(ns_late.max(0) as u64))
                };

                if evt.reschedule_time != 0 {
                    // Looping event: reschedule under basic_lock
                    let _basic = self.basic_lock.lock();
                    let mut state = self.state.lock();

                    // Check if the event was externally modified
                    if evt_sequence_num != event_type_arc.lock().sequence_number {
                        state.global_timer = self.get_global_time_ns().as_nanos() as i64;
                        continue;
                    }

                    let next_schedule_time = match callback_result {
                        Some(dur) => dur.as_nanos() as i64,
                        None => evt.reschedule_time,
                    };

                    // Clamp the reschedule base to max(evt_time, now, pause_end_time)
                    // so a lagging looping event catches up to now exactly once and
                    // then advances forward at the real interval. Without this, a
                    // looping event whose scheduled time is far behind wall time
                    // gets popped every iteration and each pass adds next_time to
                    // the stale evt_time, so evt_time advances faster than wall
                    // time and the event ends up scheduled far into the future
                    // and effectively stops firing. Matches upstream yuzu's timer
                    // thread, which uses wall time as the reschedule base.
                    let now_ns = self.get_global_time_ns().as_nanos() as i64;
                    let base = evt_time.max(now_ns).max(state.pause_end_time);
                    let next_time = base + next_schedule_time;

                    let fifo_order = state.event_fifo_id;
                    state.event_queue.push(TimingEvent {
                        time: next_time,
                        fifo_order,
                        event_type: evt.event_type.clone(),
                        reschedule_time: next_schedule_time,
                    });
                    state.event_fifo_id += 1;
                }
            }

            self.state.lock().global_timer = self.get_global_time_ns().as_nanos() as i64;
        }

        let _basic = self.basic_lock.lock();
        self.state.lock().event_queue.peek().map(|evt| evt.time)
    }

    /// Resets the timing system. Called on shutdown.
    fn reset(&self) {
        self.paused.store(true, Ordering::SeqCst);
        self.shutting_down.store(true, Ordering::SeqCst);
        self.pause_event.set();
        self.event.set();
        if let Some(thread) = self.timer_thread.lock().take() {
            let _ = thread.join();
        }
        self.has_started.store(false, Ordering::SeqCst);
    }
}

impl Default for CoreTiming {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for CoreTiming {
    fn drop(&mut self) {
        self.reset();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::sync::Arc;
    use std::time::{Duration, Instant};

    #[test]
    fn advance_looping_event_reschedules_from_event_time() {
        let mut core_timing = CoreTiming::new();
        let count = Arc::new(AtomicU64::new(0));
        let count_for_cb = count.clone();
        let event = create_event(
            "looping".to_string(),
            Box::new(move |_time, _late| {
                count_for_cb.fetch_add(1, Ordering::SeqCst);
                None
            }),
        );

        core_timing.schedule_looping_event(
            Duration::from_nanos(0),
            Duration::from_nanos(5),
            &event,
            false,
        );

        let first_time = core_timing.event_queue.peek().unwrap().time;
        core_timing.advance();
        let second_time = core_timing.event_queue.peek().unwrap().time;

        assert_eq!(count.load(Ordering::SeqCst), 1);
        assert_eq!(second_time, first_time + 5);
    }

    #[test]
    fn timer_thread_wakes_for_earlier_new_event() {
        let mut core_timing = CoreTiming::new();
        core_timing.set_multicore(true);
        core_timing.initialize(|| {});
        let core_timing = Arc::new(std::sync::Mutex::new(core_timing));
        CoreTiming::start_timer_thread(core_timing.clone());

        let late_fired = Arc::new(AtomicBool::new(false));
        let late_fired_cb = late_fired.clone();
        let late_event = create_event(
            "late".to_string(),
            Box::new(move |_time, _late| {
                late_fired_cb.store(true, Ordering::SeqCst);
                None
            }),
        );

        let early_fired_at = Arc::new(AtomicU64::new(0));
        let early_fired_at_cb = early_fired_at.clone();
        let start = Instant::now();
        let early_event = create_event(
            "early".to_string(),
            Box::new(move |_time, _late| {
                early_fired_at_cb.store(start.elapsed().as_millis() as u64, Ordering::SeqCst);
                None
            }),
        );

        core_timing
            .lock()
            .unwrap()
            .schedule_event(Duration::from_millis(250), &late_event, false);
        std::thread::sleep(Duration::from_millis(20));
        core_timing
            .lock()
            .unwrap()
            .schedule_event(Duration::from_millis(20), &early_event, false);

        let deadline = Instant::now() + Duration::from_millis(200);
        while early_fired_at.load(Ordering::SeqCst) == 0 && Instant::now() < deadline {
            std::thread::sleep(Duration::from_millis(5));
        }

        let early_ms = early_fired_at.load(Ordering::SeqCst);
        assert!(early_ms > 0, "earlier event never fired");
        assert!(
            early_ms < 150,
            "earlier event fired too late: {}ms",
            early_ms
        );
        assert!(
            !late_fired.load(Ordering::SeqCst),
            "later event should not have fired first"
        );
    }
}
