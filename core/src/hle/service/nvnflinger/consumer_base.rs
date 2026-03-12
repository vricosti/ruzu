// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: Copyright 2010 The Android Open Source Project
// SPDX-License-Identifier: GPL-3.0-or-later
// Parts of this implementation were based on:
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/include/gui/ConsumerBase.h
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/libs/gui/ConsumerBase.cpp

//! Port of zuyu/src/core/hle/service/nvnflinger/consumer_base.h
//! Port of zuyu/src/core/hle/service/nvnflinger/consumer_base.cpp

use std::sync::{Arc, Mutex};

use super::buffer_item::BufferItem;
use super::buffer_queue_consumer::BufferQueueConsumer;
use super::buffer_queue_defs::NUM_BUFFER_SLOTS;
use super::status::Status;
use super::ui::fence::Fence;
use super::ui::graphic_buffer::GraphicBuffer;

pub struct ConsumerBaseSlot {
    pub graphic_buffer: Option<Arc<GraphicBuffer>>,
    pub fence: Fence,
    pub frame_number: u64,
}

impl Default for ConsumerBaseSlot {
    fn default() -> Self {
        Self {
            graphic_buffer: None,
            fence: Fence::default(),
            frame_number: 0,
        }
    }
}

/// ConsumerBase provides the base implementation for buffer consumers.
///
/// In upstream C++, ConsumerBase inherits from IConsumerListener and
/// enable_shared_from_this. Here we use interior mutability via Mutex
/// to match the upstream locking pattern.
pub struct ConsumerBase {
    inner: Mutex<ConsumerBaseInner>,
}

struct ConsumerBaseInner {
    slots: Box<[ConsumerBaseSlot; NUM_BUFFER_SLOTS]>,
    is_abandoned: bool,
    consumer: Option<Arc<BufferQueueConsumer>>,
}

impl ConsumerBase {
    pub fn new(consumer: Arc<BufferQueueConsumer>) -> Self {
        Self {
            inner: Mutex::new(ConsumerBaseInner {
                slots: Box::new(std::array::from_fn(|_| ConsumerBaseSlot::default())),
                is_abandoned: false,
                consumer: Some(consumer),
            }),
        }
    }

    pub fn connect(&self, controlled_by_app: bool) {
        // In upstream, this calls consumer->Connect(shared_from_this(), controlled_by_app).
        // The actual consumer connection depends on the BufferQueueConsumer infrastructure.
        if let Ok(inner) = self.inner.lock() {
            if let Some(ref consumer) = inner.consumer {
                consumer.connect(controlled_by_app);
            }
        }
    }

    pub fn abandon(&self) {
        log::debug!("ConsumerBase::abandon called");
        let mut inner = self.inner.lock().unwrap();
        if !inner.is_abandoned {
            Self::abandon_locked(&mut inner);
            inner.is_abandoned = true;
        }
    }

    fn abandon_locked(inner: &mut ConsumerBaseInner) {
        for i in 0..NUM_BUFFER_SLOTS {
            Self::free_buffer_locked_inner(inner, i as i32);
        }
        // Disconnect from the BufferQueue
        if let Some(ref consumer) = inner.consumer {
            consumer.disconnect();
        }
        inner.consumer = None;
    }

    fn free_buffer_locked_inner(inner: &mut ConsumerBaseInner, slot_index: i32) {
        log::debug!("ConsumerBase: free_buffer_locked slot_index={}", slot_index);
        let s = slot_index as usize;
        inner.slots[s].graphic_buffer = None;
        inner.slots[s].fence = Fence::no_fence();
        inner.slots[s].frame_number = 0;
    }

    // IConsumerListener implementations
    pub fn on_frame_available(&self, _item: &BufferItem) {
        log::debug!("ConsumerBase::on_frame_available called");
    }

    pub fn on_frame_replaced(&self, _item: &BufferItem) {
        log::debug!("ConsumerBase::on_frame_replaced called");
    }

    pub fn on_buffers_released(&self) {
        let mut inner = self.inner.lock().unwrap();
        log::debug!("ConsumerBase::on_buffers_released called");

        if inner.is_abandoned {
            return;
        }

        let mut mask: u64 = 0;
        if let Some(ref consumer) = inner.consumer {
            consumer.get_released_buffers(&mut mask);
        }
        for i in 0..NUM_BUFFER_SLOTS {
            if mask & (1u64 << i) != 0 {
                Self::free_buffer_locked_inner(&mut inner, i as i32);
            }
        }
    }

    pub fn on_sideband_stream_changed(&self) {}

    /// Acquire a buffer. Returns the buffer item and status.
    pub fn acquire_buffer_locked(
        &self,
        item: &mut BufferItem,
        _present_when_ns: i64,
    ) -> Status {
        let mut inner = self.inner.lock().unwrap();
        let consumer = match &inner.consumer {
            Some(c) => Arc::clone(c),
            None => return Status::NoInit,
        };

        let status = consumer.acquire_buffer(item, _present_when_ns);
        if status != Status::NoError {
            return status;
        }

        if item.graphic_buffer.is_some() {
            inner.slots[item.slot as usize].graphic_buffer = item.graphic_buffer.clone();
        }

        inner.slots[item.slot as usize].frame_number = item.frame_number;
        inner.slots[item.slot as usize].fence = item.fence;

        log::debug!("ConsumerBase: acquire_buffer_locked slot={}", item.slot);
        Status::NoError
    }

    pub fn add_release_fence_locked(
        &self,
        slot: i32,
        graphic_buffer: &Option<Arc<GraphicBuffer>>,
        fence: &Fence,
    ) -> Status {
        log::debug!("ConsumerBase: add_release_fence_locked slot={}", slot);

        let mut inner = self.inner.lock().unwrap();
        if !Self::still_tracking_inner(&inner, slot, graphic_buffer) {
            return Status::NoError;
        }

        inner.slots[slot as usize].fence = *fence;
        Status::NoError
    }

    pub fn release_buffer_locked(
        &self,
        slot: i32,
        graphic_buffer: &Option<Arc<GraphicBuffer>>,
    ) -> Status {
        let mut inner = self.inner.lock().unwrap();
        if !Self::still_tracking_inner(&inner, slot, graphic_buffer) {
            return Status::NoError;
        }

        log::debug!("ConsumerBase: release_buffer_locked slot={}", slot);
        let consumer = match &inner.consumer {
            Some(c) => Arc::clone(c),
            None => return Status::NoInit,
        };

        let frame_number = inner.slots[slot as usize].frame_number;
        let fence = inner.slots[slot as usize].fence;
        let err = consumer.release_buffer(slot, frame_number, &fence);
        if err == Status::StaleBufferSlot {
            Self::free_buffer_locked_inner(&mut inner, slot);
        }

        inner.slots[slot as usize].fence = Fence::no_fence();
        err
    }

    fn still_tracking_inner(
        inner: &ConsumerBaseInner,
        slot: i32,
        graphic_buffer: &Option<Arc<GraphicBuffer>>,
    ) -> bool {
        if slot < 0 || slot >= NUM_BUFFER_SLOTS as i32 {
            return false;
        }
        match (&inner.slots[slot as usize].graphic_buffer, graphic_buffer) {
            (Some(a), Some(b)) => a.get_handle() == b.get_handle(),
            _ => false,
        }
    }
}
