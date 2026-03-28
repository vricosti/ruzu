//! Port of zuyu/src/core/core_timing.h and zuyu/src/core/core_timing.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! The timing system. Manages timed events by scheduling callbacks at specific
//! CPU tick counts. Uses a priority queue (BinaryHeap) of events, and can
//! schedule/unschedule events and advance time. This is CRITICAL for emulation.

use parking_lot::Mutex;
use common::thread::Event;
use common::wall_clock::{self, WallClock};
use std::cmp::Ordering as CmpOrdering;
use std::collections::BinaryHeap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Weak};
use std::time::Duration;

/// Maximum slice length for single-core timing.
const MAX_SLICE_LENGTH: i64 = 10000;

/// A callback that may be scheduled for a particular core timing event.
/// Returns an optional reschedule time in nanoseconds.
pub type TimedCallback =
    Box<dyn Fn(i64, Duration) -> Option<Duration> + Send + Sync>;

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
pub struct CoreTiming {
    clock: Box<dyn WallClock>,

    global_timer: i64,

    event_queue: BinaryHeap<TimingEvent>,
    event_fifo_id: u64,

    event: Arc<Event>,
    pause_event: Arc<Event>,
    basic_lock: Mutex<()>,
    advance_lock: Mutex<()>,
    timer_thread: Option<std::thread::JoinHandle<()>>,
    paused: Arc<AtomicBool>,
    paused_set: Arc<AtomicBool>,
    wait_set: Arc<AtomicBool>,
    shutting_down: Arc<AtomicBool>,
    has_started: Arc<AtomicBool>,
    on_thread_init: Option<Box<dyn Fn() + Send>>,

    is_multicore: bool,
    pause_end_time: i64,

    /// Cycle timing
    cpu_ticks: u64,
    downcount: i64,
}

// CoreTiming needs to be Send for thread spawning.
// The internal Event types use Mutex+Condvar which are Send+Sync.
unsafe impl Send for CoreTiming {}

impl CoreTiming {
    pub fn new() -> Self {
        Self {
            clock: wall_clock::create_optimal_clock(),
            global_timer: 0,
            event_queue: BinaryHeap::new(),
            event_fifo_id: 0,
            event: Arc::new(Event::new()),
            pause_event: Arc::new(Event::new()),
            basic_lock: Mutex::new(()),
            advance_lock: Mutex::new(()),
            timer_thread: None,
            paused: Arc::new(AtomicBool::new(false)),
            paused_set: Arc::new(AtomicBool::new(false)),
            wait_set: Arc::new(AtomicBool::new(false)),
            shutting_down: Arc::new(AtomicBool::new(false)),
            has_started: Arc::new(AtomicBool::new(false)),
            on_thread_init: None,
            is_multicore: false,
            pause_end_time: 0,
            cpu_ticks: 0,
            downcount: MAX_SLICE_LENGTH,
        }
    }

    /// Sets if emulation is multicore or single core. Must be set before Initialize.
    pub fn set_multicore(&mut self, is_multicore: bool) {
        self.is_multicore = is_multicore;
    }

    /// CoreTiming begins at the boundary of timing slice -1. An initial call to Advance() is
    /// required to end slice -1 and start slice 0 before the first cycle of code is executed.
    pub fn initialize<F: Fn() + Send + 'static>(&mut self, on_thread_init: F) {
        self.reset();
        self.on_thread_init = Some(Box::new(on_thread_init));
        self.event_fifo_id = 0;
        self.shutting_down.store(false, Ordering::SeqCst);
        self.cpu_ticks = 0;

        // Note: In multicore mode, the timer thread must be started separately
        // via start_timer_thread() after CoreTiming is wrapped in Arc<Mutex<>>.
        // Upstream spawns it here because C++ has shared_ptr.
    }

    /// Start the timer thread for multicore mode.
    /// Must be called after CoreTiming is wrapped in Arc<Mutex<>> and events are scheduled.
    /// Matches upstream `CoreTiming::Initialize()` which spawns ThreadLoop.
    pub fn start_timer_thread(ct: Arc<std::sync::Mutex<CoreTiming>>) {
        let is_multicore = ct.lock().unwrap().is_multicore;
        if !is_multicore {
            return;
        }

        // Extract shared state before moving into thread closure.
        // Matches upstream ThreadLoop accessing CoreTiming fields directly.
        let shutting_down = ct.lock().unwrap().shutting_down.clone();
        let paused = ct.lock().unwrap().paused.clone();
        let paused_set = ct.lock().unwrap().paused_set.clone();
        let wait_set = ct.lock().unwrap().wait_set.clone();
        let event = ct.lock().unwrap().event.clone();
        let pause_event = ct.lock().unwrap().pause_event.clone();
        let ct_clone = ct.clone();

        let handle = std::thread::Builder::new()
            .name("CoreTiming".to_string())
            .spawn(move || {
                // Upstream: ThreadEntry calls on_thread_init() then ThreadLoop().
                {
                    let ct = ct_clone.lock().unwrap();
                    if let Some(ref init_fn) = ct.on_thread_init {
                        init_fn();
                    }
                }

                // ThreadLoop — matches upstream CoreTiming::ThreadLoop().
                // Note: upstream accesses CoreTiming fields directly (no outer mutex).
                // We must minimize lock holding to avoid deadlocking with CPU threads
                // that call get_clock_ticks() under the same mutex.
                ct_clone.lock().unwrap().has_started.store(true, Ordering::SeqCst);
                // Ensure the timer thread starts unpaused.
                // sync_pause(false) from System::run() may not have been called yet.
                paused.store(false, Ordering::SeqCst);
                while !shutting_down.load(Ordering::SeqCst) {
                    while !paused.load(Ordering::SeqCst) {
                        paused_set.store(false, Ordering::SeqCst);

                        // Pop due events under lock, fire callbacks outside lock.
                        let due_events = ct_clone.lock().unwrap().collect_due_events();
                        for (evt_type, evt_time, reschedule_time) in &due_events {
                            let et = evt_type.lock();
                            let ns_late = {
                                let ct = ct_clone.lock().unwrap();
                                ct.get_global_time_ns().as_nanos() as i64 - evt_time
                            };
                            let callback_result = (et.callback)(
                                *evt_time,
                                std::time::Duration::from_nanos(ns_late.max(0) as u64),
                            );

                            if *reschedule_time != 0 {
                                let mut ct = ct_clone.lock().unwrap();
                                let next_time = match callback_result {
                                    Some(dur) => dur.as_nanos() as i64,
                                    None => *reschedule_time,
                                };
                                let fire_time = if *evt_time < ct.pause_end_time {
                                    ct.pause_end_time + next_time
                                } else {
                                    evt_time + next_time
                                };
                                let fifo_id = ct.event_fifo_id;
                                ct.event_fifo_id += 1;
                                ct.event_queue.push(TimingEvent {
                                    time: fire_time,
                                    fifo_order: fifo_id,
                                    event_type: Arc::downgrade(evt_type),
                                    reschedule_time: next_time,
                                });
                            }
                        }

                        if due_events.is_empty() {
                            // Check if there's a future event to wait for.
                            let (next_time, queue_len) = {
                                let ct = ct_clone.lock().unwrap();
                                (ct.event_queue.peek().map(|e| e.time), ct.event_queue.len())
                            };
                            if let Some(next_ns) = next_time {
                                let now_ns = ct_clone.lock().unwrap()
                                    .get_global_time_ns().as_nanos() as i64;
                                let wait_time = next_ns - now_ns;
                                if wait_time > 0 {
                                        // Sleep directly instead of using event.wait_for(),
                                    // which can return immediately if the event flag is
                                    // spuriously set by other CoreTiming operations.
                                    std::thread::sleep(std::time::Duration::from_nanos(
                                        wait_time as u64,
                                    ));
                                }
                            } else {
                                wait_set.store(true, Ordering::SeqCst);
                                event.wait();
                            }
                            wait_set.store(false, Ordering::SeqCst);
                        }
                    }

                    paused_set.store(true, Ordering::SeqCst);
                    pause_event.wait();
                }
            })
            .expect("Failed to spawn CoreTiming timer thread");

        ct.lock().unwrap().timer_thread = Some(handle);
        log::info!("CoreTiming: timer thread started");
    }

    /// Clear all pending events. This should ONLY be done on exit.
    pub fn clear_pending_events(&mut self) {
        let _adv = self.advance_lock.lock();
        let _basic = self.basic_lock.lock();
        self.event_queue.clear();
        self.event.set();
    }

    /// Pauses/Unpauses the execution of the timer thread.
    pub fn pause(&mut self, is_paused: bool) {
        self.paused.store(is_paused, Ordering::SeqCst);
        self.pause_event.set();

        if !is_paused {
            self.pause_end_time = self.get_global_time_ns().as_nanos() as i64;
        }
    }

    /// Pauses/Unpauses the execution of the timer thread and waits until paused.
    pub fn sync_pause(&mut self, is_paused: bool) {
        if is_paused == self.paused.load(Ordering::SeqCst)
            && self.paused_set.load(Ordering::SeqCst) == is_paused
        {
            return;
        }

        self.pause(is_paused);

        if self.timer_thread.is_some() {
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
            self.pause_end_time = self.get_global_time_ns().as_nanos() as i64;
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
        !(self.wait_set.load(Ordering::SeqCst) && self.event_queue.is_empty())
    }

    /// Schedules an event in core timing.
    pub fn schedule_event(
        &mut self,
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

        self.event_queue.push(TimingEvent {
            time: next_time.as_nanos() as i64,
            fifo_order: self.event_fifo_id,
            event_type: Arc::downgrade(event_type),
            reschedule_time: 0,
        });
        self.event_fifo_id += 1;
        drop(_lock);
        self.event.set();
    }

    /// Schedules an event which will automatically re-schedule itself with the given time,
    /// until unscheduled.
    pub fn schedule_looping_event(
        &mut self,
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

        self.event_queue.push(TimingEvent {
            time: next_time.as_nanos() as i64,
            fifo_order: self.event_fifo_id,
            event_type: Arc::downgrade(event_type),
            reschedule_time: resched_time.as_nanos() as i64,
        });
        self.event_fifo_id += 1;
        drop(_lock);
        self.event.set();
    }

    /// Unschedules all instances of the given event type.
    pub fn unschedule_event(
        &mut self,
        event_type: &Arc<Mutex<EventType>>,
        unsched_type: UnscheduleEventType,
    ) {
        {
            let _lock = self.basic_lock.lock();

            let event_type_ptr = Arc::as_ptr(event_type);
            // Rebuild the queue without the matching events
            let old_queue = std::mem::take(&mut self.event_queue);
            for evt in old_queue.into_iter() {
                if let Some(strong) = evt.event_type.upgrade() {
                    if Arc::as_ptr(&strong) != event_type_ptr {
                        self.event_queue.push(evt);
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
    pub fn add_ticks(&mut self, ticks_to_add: u64) {
        self.cpu_ticks += ticks_to_add;
        self.downcount -= ticks_to_add as i64;
    }

    /// Resets the tick downcount to the max slice length.
    pub fn reset_ticks(&mut self) {
        self.downcount = MAX_SLICE_LENGTH;
    }

    /// Adds idle ticks.
    pub fn idle(&mut self) {
        self.cpu_ticks += 1000;
    }

    /// Collect all due events from the queue without firing callbacks.
    /// Returns Vec of (event_type_arc, fire_time, reschedule_time).
    /// Used by the timer thread to fire callbacks outside the CoreTiming lock.
    pub fn collect_due_events(
        &mut self,
    ) -> Vec<(Arc<parking_lot::Mutex<EventType>>, i64, i64)> {
        self.global_timer = self.get_global_time_ns().as_nanos() as i64;
        let mut result = Vec::new();
        loop {
            let _basic = self.basic_lock.lock();
            match self.event_queue.peek() {
                Some(evt) if evt.time <= self.global_timer => {
                    let evt = self.event_queue.pop().unwrap();
                    if let Some(arc) = evt.event_type.upgrade() {
                        result.push((arc, evt.time, evt.reschedule_time));
                    }
                }
                _ => break,
            }
        }
        result
    }

    /// Returns the current downcount.
    pub fn get_downcount(&self) -> i64 {
        self.downcount
    }

    /// Returns the current CNTPCT tick value.
    pub fn get_clock_ticks(&self) -> u64 {
        if self.is_multicore {
            self.clock.get_cntpct() as u64
        } else {
            wall_clock::cpu_tick_to_cntpct(self.cpu_ticks)
        }
    }

    /// Returns the current GPU tick value.
    pub fn get_gpu_ticks(&self) -> u64 {
        if self.is_multicore {
            self.clock.get_gpu_tick() as u64
        } else {
            wall_clock::cpu_tick_to_gpu_tick(self.cpu_ticks)
        }
    }

    /// Returns current time in nanoseconds.
    pub fn get_global_time_ns(&self) -> Duration {
        if self.is_multicore {
            self.clock.get_time_ns()
        } else {
            Duration::from_nanos(wall_clock::cpu_tick_to_ns(self.cpu_ticks))
        }
    }

    /// Returns current time in microseconds.
    pub fn get_global_time_us(&self) -> Duration {
        if self.is_multicore {
            self.clock.get_time_us()
        } else {
            Duration::from_micros(wall_clock::cpu_tick_to_us(self.cpu_ticks))
        }
    }

    /// Checks for events manually and returns time in nanoseconds for next event.
    /// This is the core advance function that fires all due events.
    ///
    /// The C++ version holds both advance_lock and basic_lock, unlocking basic_lock
    /// around callbacks. In Rust, since we have &mut self (exclusive access),
    /// we use the advance_lock for external synchronization and handle the
    /// basic_lock at a coarser granularity.
    pub fn advance(&mut self) -> Option<i64> {
        let _adv_lock = self.advance_lock.lock();
        self.global_timer = self.get_global_time_ns().as_nanos() as i64;

        loop {
            // Check under basic_lock if there's a due event
            let maybe_evt = {
                let _basic = self.basic_lock.lock();
                match self.event_queue.peek() {
                    Some(evt) if evt.time <= self.global_timer => self.event_queue.pop(),
                    _ => None,
                }
            };

            let evt = match maybe_evt {
                Some(e) => e,
                None => break,
            };

            if let Some(event_type_arc) = evt.event_type.upgrade() {
                let evt_time = evt.time;
                let evt_sequence_num = event_type_arc.lock().sequence_number;

                // Fire the callback without holding basic_lock
                let ns_late = self.get_global_time_ns().as_nanos() as i64 - evt_time;
                let callback_result = {
                    let et = event_type_arc.lock();
                    (et.callback)(evt_time, Duration::from_nanos(ns_late.max(0) as u64))
                };

                if evt.reschedule_time != 0 {
                    // Looping event: reschedule under basic_lock
                    let _basic = self.basic_lock.lock();

                    // Check if the event was externally modified
                    if evt_sequence_num != event_type_arc.lock().sequence_number {
                        self.global_timer = self.get_global_time_ns().as_nanos() as i64;
                        continue;
                    }

                    let next_schedule_time = match callback_result {
                        Some(dur) => dur.as_nanos() as i64,
                        None => evt.reschedule_time,
                    };

                    // If this event was scheduled into a pause, re-set from pause end
                    let next_time = if evt_time < self.pause_end_time {
                        self.pause_end_time + next_schedule_time
                    } else {
                        evt_time + next_schedule_time
                    };

                    self.event_queue.push(TimingEvent {
                        time: next_time,
                        fifo_order: self.event_fifo_id,
                        event_type: evt.event_type.clone(),
                        reschedule_time: next_schedule_time,
                    });
                    self.event_fifo_id += 1;
                }
            }

            self.global_timer = self.get_global_time_ns().as_nanos() as i64;
        }

        let _basic = self.basic_lock.lock();
        self.event_queue.peek().map(|evt| evt.time)
    }

    /// Resets the timing system. Called on shutdown.
    fn reset(&mut self) {
        self.paused.store(true, Ordering::SeqCst);
        self.shutting_down.store(true, Ordering::SeqCst);
        self.pause_event.set();
        self.event.set();
        if let Some(thread) = self.timer_thread.take() {
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
