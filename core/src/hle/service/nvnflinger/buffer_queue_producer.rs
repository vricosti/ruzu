// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: Copyright 2014 The Android Open Source Project
// SPDX-License-Identifier: GPL-3.0-or-later
// Parts of this implementation were based on:
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/include/gui/BufferQueueProducer.h

//! Port of zuyu/src/core/hle/service/nvnflinger/buffer_queue_producer.h
//!
//! The BufferQueueProducer is the producer-side interface for buffer queues.
//! It implements IBinder for binder transactions from the application.
//!
//! Full method implementations (DequeueBuffer, QueueBuffer, etc.) depend on
//! the complete BufferQueueCore + NvMap + kernel event infrastructure.
//! The struct and method signatures are fully ported; method bodies that
//! require complex interactions with other subsystems use todo!().

use std::sync::Arc;

use super::buffer_queue_core::BufferQueueCore;
use super::graphic_buffer_producer::{QueueBufferInput, QueueBufferOutput};
use super::pixel_format::PixelFormat;
use super::status::Status;
use super::ui::fence::Fence;
use super::ui::graphic_buffer::{GraphicBuffer, NvGraphicBuffer};
use super::window::{NativeWindow, NativeWindowApi};
use super::producer_listener::IProducerListener;

pub struct BufferQueueProducer {
    core: Arc<BufferQueueCore>,
    sticky_transform: u32,
    next_callback_ticket: i32,
    current_callback_ticket: i32,
}

impl BufferQueueProducer {
    pub fn new(core: Arc<BufferQueueCore>) -> Self {
        Self {
            core,
            sticky_transform: 0,
            next_callback_ticket: 0,
            current_callback_ticket: 0,
        }
    }

    pub fn request_buffer(&self, slot: i32) -> (Status, Option<Arc<GraphicBuffer>>) {
        let mut inner = self.core.mutex.lock().unwrap();
        if inner.is_abandoned {
            log::error!("BufferQueueProducer: BufferQueue has been abandoned");
            return (Status::NoInit, None);
        }
        if slot < 0 || slot as usize >= super::buffer_queue_defs::NUM_BUFFER_SLOTS {
            log::error!("BufferQueueProducer: slot {} out of range", slot);
            return (Status::BadValue, None);
        }
        inner.slots[slot as usize].request_buffer_called = true;
        let buf = inner.slots[slot as usize].graphic_buffer.clone();
        (Status::NoError, buf)
    }

    pub fn set_buffer_count(&self, buffer_count: i32) -> Status {
        log::debug!("BufferQueueProducer::set_buffer_count count={}", buffer_count);
        let mut inner = self.core.mutex.lock().unwrap();

        if inner.is_abandoned {
            log::error!("BufferQueueProducer: BufferQueue has been abandoned");
            return Status::NoInit;
        }

        if buffer_count > super::buffer_queue_defs::NUM_BUFFER_SLOTS as i32 {
            log::error!("BufferQueueProducer: buffer_count {} too large", buffer_count);
            return Status::BadValue;
        }

        inner.override_max_buffer_count = buffer_count;
        inner.free_all_buffers_locked();
        drop(inner);
        self.core.signal_dequeue_condition();

        Status::NoError
    }

    pub fn dequeue_buffer(
        &self,
        _async_flag: bool,
        _width: u32,
        _height: u32,
        _format: PixelFormat,
        _usage: u32,
    ) -> (Status, i32, Fence) {
        // Upstream dequeue_buffer calls WaitForFreeSlotThenRelock to find a free buffer slot,
        // then configures the slot's GraphicBuffer via NvMap allocation and returns the slot
        // index with an acquire fence. Requires:
        //   - WaitForFreeSlotThenRelock (condition variable wait loop over mSlots)
        //   - NvMap buffer allocation for GraphicBuffer backing
        //   - Kernel event signaling for buffer availability
        // Blocked on NvMap and kernel event infrastructure.
        log::warn!("BufferQueueProducer::dequeue_buffer: slot management infrastructure not yet ported");
        (Status::WouldBlock, -1, Fence::default())
    }

    pub fn queue_buffer(
        &self,
        _slot: i32,
        _input: &QueueBufferInput,
    ) -> (Status, QueueBufferOutput) {
        // Upstream queue_buffer validates the slot, updates frame number, sets the buffer's
        // acquire fence, notifies the consumer listener (onFrameAvailable/onFrameReplaced),
        // and returns QueueBufferOutput with
        // (
        //   width, height, transformHint, numPendingBuffers from the consumer.
        // Requires:
        //   - Complete mSlots buffer state management
        //   - ConsumerListener notification (IConsumerListener::onFrameAvailable)
        //   - Fence synchronization
        // Blocked on buffer queue and consumer listener infrastructure.
        log::warn!("BufferQueueProducer::queue_buffer: buffer queue infrastructure not yet ported");
        (Status::WouldBlock, QueueBufferOutput::new())
    }

    pub fn cancel_buffer(&self, slot: i32, fence: &Fence) {
        let mut inner = self.core.mutex.lock().unwrap();
        if inner.is_abandoned {
            return;
        }
        if slot < 0 || slot as usize >= super::buffer_queue_defs::NUM_BUFFER_SLOTS {
            return;
        }
        inner.slots[slot as usize].buffer_state = super::buffer_slot::BufferState::Free;
        inner.slots[slot as usize].frame_number = 0;
        inner.slots[slot as usize].fence = *fence;
        drop(inner);
        self.core.signal_dequeue_condition();
    }

    pub fn query(&self, what: NativeWindow) -> (Status, i32) {
        let inner = self.core.mutex.lock().unwrap();

        if inner.is_abandoned {
            return (Status::NoInit, 0);
        }

        let value = match what {
            NativeWindow::Width => inner.default_width as i32,
            NativeWindow::Height => inner.default_height as i32,
            NativeWindow::Format => inner.default_buffer_format as i32,
            NativeWindow::MinUndequeedBuffers => {
                inner.get_min_undequeued_buffer_count_locked(false)
            }
            NativeWindow::ConsumerUsageBits => inner.consumer_usage_bit as i32,
            NativeWindow::DefaultWidth => inner.default_width as i32,
            NativeWindow::DefaultHeight => inner.default_height as i32,
            NativeWindow::TransformHint => inner.transform_hint as i32,
            _ => {
                log::warn!("BufferQueueProducer::query unhandled: {:?}", what);
                0
            }
        };

        (Status::NoError, value)
    }

    pub fn connect(
        &self,
        listener: Option<Arc<dyn IProducerListener>>,
        api: NativeWindowApi,
        producer_controlled_by_app: bool,
    ) -> (Status, QueueBufferOutput) {
        let mut inner = self.core.mutex.lock().unwrap();

        if inner.is_abandoned {
            return (Status::NoInit, QueueBufferOutput::new());
        }

        if inner.connected_api != NativeWindowApi::NoConnectedApi {
            log::error!("BufferQueueProducer: already connected (api={:?})", inner.connected_api);
            return (Status::BadValue, QueueBufferOutput::new());
        }

        inner.connected_api = api;
        inner.connected_producer_listener = listener;

        let mut output = QueueBufferOutput::new();
        output.inflate(
            inner.default_width,
            inner.default_height,
            inner.transform_hint,
            inner.queue.len() as u32,
        );

        (Status::NoError, output)
    }

    pub fn disconnect(&self, api: NativeWindowApi) -> Status {
        let mut inner = self.core.mutex.lock().unwrap();

        if inner.is_abandoned {
            return Status::NoInit;
        }

        if inner.connected_api != api {
            log::error!(
                "BufferQueueProducer: disconnect wrong API {:?} (connected={:?})",
                api,
                inner.connected_api
            );
            return Status::BadValue;
        }

        inner.connected_api = NativeWindowApi::NoConnectedApi;
        inner.connected_producer_listener = None;
        inner.free_all_buffers_locked();
        drop(inner);
        self.core.signal_dequeue_condition();

        Status::NoError
    }

    pub fn set_preallocated_buffer(
        &self,
        slot: i32,
        buffer: Option<Arc<NvGraphicBuffer>>,
    ) -> Status {
        let mut inner = self.core.mutex.lock().unwrap();

        if slot < 0 || slot as usize >= super::buffer_queue_defs::NUM_BUFFER_SLOTS {
            return Status::BadValue;
        }

        let s = slot as usize;
        inner.free_buffer_locked(slot);

        if let Some(buf) = buffer {
            inner.slots[s].graphic_buffer = Some(Arc::new(GraphicBuffer::from_nv_buffer(*buf, false)));
            inner.slots[s].is_preallocated = true;

            if inner.default_width != buf.get_width() || inner.default_height != buf.get_height() {
                inner.default_width = buf.get_width();
                inner.default_height = buf.get_height();
            }
        }

        drop(inner);
        self.core.signal_dequeue_condition();

        Status::NoError
    }
}
