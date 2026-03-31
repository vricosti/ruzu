// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: Copyright 2012 The Android Open Source Project
// SPDX-License-Identifier: GPL-3.0-or-later
// Parts of this implementation were based on:
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/include/gui/BufferItemConsumer.h
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/libs/gui/BufferItemConsumer.cpp

//! Port of zuyu/src/core/hle/service/nvnflinger/buffer_item_consumer.h
//! Port of zuyu/src/core/hle/service/nvnflinger/buffer_item_consumer.cpp

use std::sync::Arc;

use super::buffer_item::BufferItem;
use super::buffer_queue_consumer::BufferQueueConsumer;
use super::consumer_base::ConsumerBase;
use super::consumer_listener::IConsumerListener;
use super::status::Status;
use super::ui::fence::Fence;

/// BufferItemConsumer wraps a ConsumerBase to provide buffer acquire/release.
pub struct BufferItemConsumer {
    base: ConsumerBase,
}

impl BufferItemConsumer {
    pub fn new(consumer: Arc<BufferQueueConsumer>) -> Self {
        Self {
            base: ConsumerBase::new(consumer),
        }
    }

    pub fn connect(self: &Arc<Self>, controlled_by_app: bool) -> Status {
        self.base.connect(
            Arc::clone(self) as Arc<dyn IConsumerListener>,
            controlled_by_app,
        )
    }

    pub fn abandon(&self) {
        self.base.abandon();
    }

    pub fn acquire_buffer(
        &self,
        item: &mut BufferItem,
        present_when_ns: i64,
        _wait_for_fence: bool,
    ) -> Status {
        let status = self.base.acquire_buffer_locked(item, present_when_ns);
        if status != Status::NoError {
            if status != Status::NoBufferAvailable {
                log::error!("BufferItemConsumer: Failed to acquire buffer: {:?}", status);
            }
            return status;
        }

        // In upstream, if wait_for_fence is true, it would wait on the fence.
        // This is UNIMPLEMENTED in upstream as well.

        // Note: The slot's graphic_buffer is assigned by acquire_buffer_locked.
        // We need to copy it to the item. In upstream this reads from slots[item->slot].
        // The ConsumerBase already handles this internally.

        Status::NoError
    }

    pub fn release_buffer(&self, item: &BufferItem, release_fence: &Fence) -> Status {
        if let Status::NoError =
            self.base
                .add_release_fence_locked(item.slot, &item.graphic_buffer, release_fence)
        {
            // ok
        } else {
            log::error!("BufferItemConsumer: Failed to add fence");
        }

        let status = self
            .base
            .release_buffer_locked(item.slot, &item.graphic_buffer);
        if status != Status::NoError {
            log::warn!("BufferItemConsumer: Failed to release buffer: {:?}", status);
            return status;
        }

        Status::NoError
    }
}

impl IConsumerListener for BufferItemConsumer {
    fn on_frame_available(&self, item: &BufferItem) {
        self.base.on_frame_available(item);
    }

    fn on_frame_replaced(&self, item: &BufferItem) {
        self.base.on_frame_replaced(item);
    }

    fn on_buffers_released(&self) {
        self.base.on_buffers_released();
    }

    fn on_sideband_stream_changed(&self) {
        self.base.on_sideband_stream_changed();
    }
}
