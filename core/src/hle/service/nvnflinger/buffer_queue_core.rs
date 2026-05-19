// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: Copyright 2014 The Android Open Source Project
// SPDX-License-Identifier: GPL-3.0-or-later
// Parts of this implementation were based on:
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/include/gui/BufferQueueCore.h
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/libs/gui/BufferQueueCore.cpp

//! Port of zuyu/src/core/hle/service/nvnflinger/buffer_queue_core.h
//! Port of zuyu/src/core/hle/service/nvnflinger/buffer_queue_core.cpp

use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::time::Instant;

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
        })
    }

    pub fn signal_dequeue_condition(&self) {
        if bqp_wait_profile_enabled() {
            BQP_WAIT_SIGNAL_COUNT.fetch_add(1, Ordering::Relaxed);
        }
        self.dequeue_possible.store(true, Ordering::Release);
        self.dequeue_condition.notify_all();
    }

    pub fn wait_for_dequeue_condition<'a>(
        &self,
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
        let guard = self
            .dequeue_condition
            .wait_while(guard, |_| !self.dequeue_possible.load(Ordering::Acquire))
            .unwrap();
        self.dequeue_possible.store(false, Ordering::Release);
        if let Some(start) = start {
            let elapsed_us = start.elapsed().as_micros().min(u128::from(u64::MAX)) as u64;
            BQP_WAIT_RETURN_COUNT.fetch_add(1, Ordering::Relaxed);
            BQP_WAIT_TOTAL_US.fetch_add(elapsed_us, Ordering::Relaxed);
            update_max(&BQP_WAIT_MAX_US, elapsed_us);
        }
        guard
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

    pub fn wait_while_allocating_locked(&self) {
        // In upstream this waits on is_allocating_condition while is_allocating is true.
        // Since we hold the mutex via MutexGuard, this would need to use the outer
        // Condvar. For now we just assert we are not allocating.
        assert!(
            !self.is_allocating,
            "BufferQueueCore: unexpected allocation in progress"
        );
    }
}
