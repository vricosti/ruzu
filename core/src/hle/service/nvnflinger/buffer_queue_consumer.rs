// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: Copyright 2014 The Android Open Source Project
// SPDX-License-Identifier: GPL-3.0-or-later
// Parts of this implementation were based on:
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/include/gui/BufferQueueConsumer.h
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/libs/gui/BufferQueueConsumer.cpp

//! Port of zuyu/src/core/hle/service/nvnflinger/buffer_queue_consumer.h
//! Port of zuyu/src/core/hle/service/nvnflinger/buffer_queue_consumer.cpp

use std::sync::Arc;

use super::buffer_item::BufferItem;
use super::buffer_queue_core::BufferQueueCore;
use super::buffer_queue_defs::NUM_BUFFER_SLOTS;
use super::buffer_slot::BufferState;
use super::status::Status;
use super::ui::fence::Fence;

pub struct BufferQueueConsumer {
    core: Arc<BufferQueueCore>,
}

impl BufferQueueConsumer {
    pub fn new(core: Arc<BufferQueueCore>) -> Self {
        Self { core }
    }

    pub fn acquire_buffer(&self, out_buffer: &mut BufferItem, expected_present_ns: i64) -> Status {
        let mut inner = self.core.mutex.lock().unwrap();

        // Check that the consumer doesn't currently have the maximum number of buffers acquired.
        let num_acquired: i32 = inner
            .slots
            .iter()
            .filter(|s| s.buffer_state == BufferState::Acquired)
            .count() as i32;

        if num_acquired >= inner.max_acquired_buffer_count + 1 {
            log::error!(
                "BufferQueueConsumer: max acquired buffer count reached: {} (max {})",
                num_acquired,
                inner.max_acquired_buffer_count
            );
            return Status::InvalidOperation;
        }

        // Check if the queue is empty.
        if inner.queue.is_empty() {
            return Status::NoBufferAvailable;
        }

        // If expected_present is specified, we may not want to return a buffer yet.
        if expected_present_ns != 0 {
            const MAX_REASONABLE_NSEC: i64 = 1_000_000_000; // 1 second

            // Drop old buffers that are past their time
            while inner.queue.len() > 1 && !inner.queue[0].is_auto_timestamp {
                let desired_present = inner.queue[1].timestamp;
                if desired_present < expected_present_ns - MAX_REASONABLE_NSEC
                    || desired_present > expected_present_ns
                {
                    log::debug!(
                        "BufferQueueConsumer: nodrop desire={} expect={}",
                        desired_present,
                        expected_present_ns
                    );
                    break;
                }

                log::debug!(
                    "BufferQueueConsumer: drop desire={} expect={} size={}",
                    desired_present,
                    expected_present_ns,
                    inner.queue.len()
                );

                if inner.still_tracking(&inner.queue[0]) {
                    let slot = inner.queue[0].slot as usize;
                    inner.slots[slot].buffer_state = BufferState::Free;
                }
                inner.queue.remove(0);
            }

            // See if the front buffer is ready to be acquired.
            let desired_present = inner.queue[0].timestamp;
            if desired_present > expected_present_ns
                && desired_present < expected_present_ns + MAX_REASONABLE_NSEC
            {
                log::debug!(
                    "BufferQueueConsumer: defer desire={} expect={}",
                    desired_present,
                    expected_present_ns
                );
                return Status::PresentLater;
            }

            log::debug!(
                "BufferQueueConsumer: accept desire={} expect={}",
                desired_present,
                expected_present_ns
            );
        }

        let front = inner.queue.remove(0);
        let slot = front.slot as usize;

        *out_buffer = BufferItem {
            graphic_buffer: front.graphic_buffer.clone(),
            fence: front.fence,
            crop: front.crop,
            transform: front.transform,
            scaling_mode: front.scaling_mode,
            timestamp: front.timestamp,
            is_auto_timestamp: front.is_auto_timestamp,
            frame_number: front.frame_number,
            slot: front.slot,
            is_droppable: front.is_droppable,
            acquire_called: front.acquire_called,
            transform_to_display_inverse: front.transform_to_display_inverse,
            swap_interval: front.swap_interval,
        };

        log::debug!("BufferQueueConsumer: acquiring slot={}", slot);

        // If the front buffer is still being tracked, update its slot state
        if inner.still_tracking(&front) {
            inner.slots[slot].acquire_called = true;
            inner.slots[slot].needs_cleanup_on_release = false;
            inner.slots[slot].buffer_state = BufferState::Acquired;
        }

        // If the buffer has previously been acquired by the consumer, set graphic_buffer to None
        // to avoid unnecessarily remapping this buffer on the consumer side.
        if out_buffer.acquire_called {
            out_buffer.graphic_buffer = None;
        }

        // Signal that a slot might be free
        drop(inner);
        self.core.signal_dequeue_condition();

        Status::NoError
    }

    pub fn release_buffer(&self, slot: i32, frame_number: u64, release_fence: &Fence) -> Status {
        if slot < 0 || slot >= NUM_BUFFER_SLOTS as i32 {
            log::error!("BufferQueueConsumer: slot {} out of range", slot);
            return Status::BadValue;
        }

        let listener;
        {
            let mut inner = self.core.mutex.lock().unwrap();

            // If the frame number has changed because the buffer has been reallocated,
            // we can ignore this ReleaseBuffer for the old buffer.
            if frame_number != inner.slots[slot as usize].frame_number {
                return Status::StaleBufferSlot;
            }

            // Make sure this buffer hasn't been queued while acquired by the consumer.
            for item in &inner.queue {
                if item.slot == slot {
                    log::error!(
                        "BufferQueueConsumer: buffer slot {} pending release is currently queued",
                        slot
                    );
                    return Status::BadValue;
                }
            }

            if inner.slots[slot as usize].buffer_state == BufferState::Acquired {
                inner.slots[slot as usize].buffer_state = BufferState::Free;
                listener = inner.connected_producer_listener.clone();
                log::debug!("BufferQueueConsumer: releasing slot {}", slot);
            } else if inner.slots[slot as usize].needs_cleanup_on_release {
                log::debug!(
                    "BufferQueueConsumer: releasing a stale buffer slot {} (state = {:?})",
                    slot,
                    inner.slots[slot as usize].buffer_state
                );
                inner.slots[slot as usize].needs_cleanup_on_release = false;
                return Status::StaleBufferSlot;
            } else {
                log::error!(
                    "BufferQueueConsumer: attempted to release buffer slot {} but its state was {:?}",
                    slot,
                    inner.slots[slot as usize].buffer_state
                );
                return Status::BadValue;
            }

            self.core.signal_dequeue_condition();
        }

        // Call back without lock held
        if let Some(ref l) = listener {
            l.on_buffer_released();
        }

        Status::NoError
    }

    pub fn connect(&self, controlled_by_app: bool) {
        let mut inner = self.core.mutex.lock().unwrap();
        inner.consumer_controlled_by_app = controlled_by_app;
    }

    pub fn disconnect(&self) {
        log::debug!("BufferQueueConsumer::disconnect called");
        let mut inner = self.core.mutex.lock().unwrap();

        inner.is_abandoned = true;
        inner.consumer_listener = None;
        inner.queue.clear();
        inner.free_all_buffers_locked();
        drop(inner);
        self.core.signal_dequeue_condition();
    }

    pub fn get_released_buffers(&self, out_slot_mask: &mut u64) {
        let inner = self.core.mutex.lock().unwrap();

        if inner.is_abandoned {
            log::error!("BufferQueueConsumer: BufferQueue has been abandoned");
            *out_slot_mask = 0;
            return;
        }

        let mut mask: u64 = 0;
        for s in 0..NUM_BUFFER_SLOTS {
            if !inner.slots[s].acquire_called {
                mask |= 1u64 << s;
            }
        }

        // Remove from the mask queued buffers for which acquire has been called
        for item in &inner.queue {
            if item.acquire_called {
                mask &= !(1u64 << item.slot);
            }
        }

        log::debug!("BufferQueueConsumer: returning mask {}", mask);
        *out_slot_mask = mask;
    }
}
