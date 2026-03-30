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

use super::binder::IBinder;
use super::buffer_queue_core::BufferQueueCore;
use super::graphic_buffer_producer::{QueueBufferInput, QueueBufferOutput};
use super::parcel::{InputParcel, OutputParcel};
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
        async_flag: bool,
        width: u32,
        height: u32,
        _format: PixelFormat,
        _usage: u32,
    ) -> (Status, i32, Fence) {
        let mut inner = self.core.mutex.lock().unwrap();

        if inner.is_abandoned {
            return (Status::NoInit, -1, Fence::default());
        }

        let max_buffer_count = inner.get_max_buffer_count_locked(async_flag);

        // Find a free slot with a graphic buffer (preallocated).
        // Upstream: WaitForFreeSlotThenRelock picks the oldest free buffer.
        let mut found: i32 = -1;
        for s in 0..(max_buffer_count as usize) {
            if inner.slots[s].buffer_state == super::buffer_slot::BufferState::Free
                && inner.slots[s].graphic_buffer.is_some()
            {
                if found == -1
                    || inner.slots[s].frame_number < inner.slots[found as usize].frame_number
                {
                    found = s as i32;
                }
            }
        }

        if found == -1 {
            // No free buffers — upstream would block via condition variable.
            // Return WouldBlock so the caller can retry.
            return (Status::WouldBlock, -1, Fence::default());
        }

        let s = found as usize;
        inner.slots[s].buffer_state = super::buffer_slot::BufferState::Dequeued;

        // Use default size if width/height are 0.
        let _w = if width == 0 { inner.default_width } else { width };
        let _h = if height == 0 { inner.default_height } else { height };

        let fence = inner.slots[s].fence;
        inner.slots[s].fence = Fence::no_fence();

        log::debug!("BufferQueueProducer::dequeue_buffer -> slot={}", found);
        (Status::NoError, found, fence)
    }

    pub fn queue_buffer(
        &self,
        slot: i32,
        input: &QueueBufferInput,
    ) -> (Status, QueueBufferOutput) {
        let mut inner = self.core.mutex.lock().unwrap();

        if inner.is_abandoned {
            return (Status::NoInit, QueueBufferOutput::new());
        }

        if slot < 0 || slot as usize >= super::buffer_queue_defs::NUM_BUFFER_SLOTS {
            return (Status::BadValue, QueueBufferOutput::new());
        }

        let s = slot as usize;
        if inner.slots[s].buffer_state != super::buffer_slot::BufferState::Dequeued {
            log::error!(
                "BufferQueueProducer::queue_buffer: slot {} not dequeued (state={:?})",
                slot, inner.slots[s].buffer_state
            );
            return (Status::BadValue, QueueBufferOutput::new());
        }

        // Transition to Queued.
        inner.slots[s].buffer_state = super::buffer_slot::BufferState::Queued;
        inner.slots[s].fence = input.fence;
        inner.frame_counter += 1;
        inner.slots[s].frame_number = inner.frame_counter;

        // Push to the FIFO queue for the consumer.
        let gb = inner.slots[s].graphic_buffer.clone();
        let frame_num = inner.frame_counter;
        let item = super::buffer_item::BufferItem {
            slot,
            graphic_buffer: gb,
            fence: input.fence,
            crop: input.crop,
            transform: input.transform,
            scaling_mode: input.scaling_mode as u32,
            timestamp: input.timestamp,
            is_auto_timestamp: input.is_auto_timestamp != 0,
            frame_number: frame_num,
            swap_interval: input.swap_interval,
            is_droppable: false,
            acquire_called: false,
            transform_to_display_inverse: false,
        };
        inner.queue.push(item);

        let mut output = QueueBufferOutput::new();
        output.inflate(
            inner.default_width,
            inner.default_height,
            inner.transform_hint,
            inner.queue.len() as u32,
        );

        // Upstream: notify consumer (onFrameAvailable).
        // Signal that a buffer is available for the consumer to acquire.
        drop(inner);
        self.core.signal_dequeue_condition();

        log::debug!("BufferQueueProducer::queue_buffer slot={} frame={}", slot, self.core.mutex.lock().unwrap().frame_counter);
        (Status::NoError, output)
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

impl IBinder for BufferQueueProducer {
    fn transact(&self, code: u32, parcel_data: &[u8], parcel_reply: &mut [u8], _flags: u32) {
        #[repr(u32)]
        enum TransactionId {
            RequestBuffer = 1,
            SetBufferCount = 2,
            DequeueBuffer = 3,
            DetachBuffer = 4,
            DetachNextBuffer = 5,
            AttachBuffer = 6,
            QueueBuffer = 7,
            CancelBuffer = 8,
            Query = 9,
            Connect = 10,
            Disconnect = 11,
            AllocateBuffers = 13,
            SetPreallocatedBuffer = 14,
            GetBufferHistory = 17,
        }

        let mut status = Status::NoError;
        let mut parcel_in = InputParcel::new(parcel_data);
        let mut parcel_out = OutputParcel::new();

        match code {
            x if x == TransactionId::Connect as u32 => {
                let enable_listener = parcel_in.read::<u8>() != 0;
                let api = match parcel_in.read::<i32>() {
                    0 => NativeWindowApi::NoConnectedApi,
                    1 => NativeWindowApi::Egl,
                    2 => NativeWindowApi::Cpu,
                    3 => NativeWindowApi::Media,
                    4 => NativeWindowApi::Camera,
                    _ => NativeWindowApi::NoConnectedApi,
                };
                let producer_controlled_by_app = parcel_in.read::<u8>() != 0;

                if enable_listener {
                    log::warn!("BufferQueueProducer::transact Connect listener is unimplemented");
                }

                let (new_status, output) = self.connect(None, api, producer_controlled_by_app);
                status = new_status;
                parcel_out.write(&output);
            }
            x if x == TransactionId::SetPreallocatedBuffer as u32 => {
                let slot = parcel_in.read::<i32>();
                let buffer = parcel_in.read_object::<NvGraphicBuffer>().map(Arc::new);
                status = self.set_preallocated_buffer(slot, buffer);
            }
            x if x == TransactionId::DequeueBuffer as u32 => {
                let is_async = parcel_in.read::<u8>() != 0;
                let width = parcel_in.read::<u32>();
                let height = parcel_in.read::<u32>();
                let pixel_format = parcel_in.read::<PixelFormat>();
                let usage = parcel_in.read::<u32>();

                let (new_status, slot, fence) =
                    self.dequeue_buffer(is_async, width, height, pixel_format, usage);
                status = new_status;
                parcel_out.write(&slot);
                parcel_out.write_flattened_object(Some(&fence));
            }
            x if x == TransactionId::RequestBuffer as u32 => {
                let slot = parcel_in.read::<i32>();
                let (new_status, buf) = self.request_buffer(slot);
                status = new_status;
                parcel_out.write_flattened_object(buf.as_ref().map(|g| &g.buffer));
            }
            x if x == TransactionId::QueueBuffer as u32 => {
                let slot = parcel_in.read::<i32>();
                let input = parcel_in.read_flattened::<QueueBufferInput>();
                let (new_status, output) = self.queue_buffer(slot, &input);
                status = new_status;
                parcel_out.write(&output);
            }
            x if x == TransactionId::Query as u32 => {
                let what_raw = parcel_in.read::<i32>();
                match what_raw {
                    0 => {
                        let (new_status, value) = self.query(NativeWindow::Width);
                        status = new_status;
                        parcel_out.write(&value);
                    }
                    1 => {
                        let (new_status, value) = self.query(NativeWindow::Height);
                        status = new_status;
                        parcel_out.write(&value);
                    }
                    2 => {
                        let (new_status, value) = self.query(NativeWindow::Format);
                        status = new_status;
                        parcel_out.write(&value);
                    }
                    3 => {
                        let (new_status, value) = self.query(NativeWindow::MinUndequeedBuffers);
                        status = new_status;
                        parcel_out.write(&value);
                    }
                    9 => {
                        let (new_status, value) = self.query(NativeWindow::ConsumerRunningBehind);
                        status = new_status;
                        parcel_out.write(&value);
                    }
                    10 => {
                        let (new_status, value) = self.query(NativeWindow::ConsumerUsageBits);
                        status = new_status;
                        parcel_out.write(&value);
                    }
                    11 => {
                        let (new_status, value) = self.query(NativeWindow::StickyTransform);
                        status = new_status;
                        parcel_out.write(&value);
                    }
                    _ => {
                        log::error!("BufferQueueProducer::transact Query unknown what={}", what_raw);
                        status = Status::BadValue;
                    }
                }
            }
            x if x == TransactionId::CancelBuffer as u32 => {
                let slot = parcel_in.read::<i32>();
                let fence = parcel_in.read_flattened::<Fence>();
                self.cancel_buffer(slot, &fence);
            }
            x if x == TransactionId::Disconnect as u32 => {
                let api = match parcel_in.read::<i32>() {
                    0 => NativeWindowApi::NoConnectedApi,
                    1 => NativeWindowApi::Egl,
                    2 => NativeWindowApi::Cpu,
                    3 => NativeWindowApi::Media,
                    4 => NativeWindowApi::Camera,
                    _ => {
                        log::error!("BufferQueueProducer::transact Disconnect unknown api");
                        NativeWindowApi::NoConnectedApi
                    }
                };
                status = self.disconnect(api);
            }
            x if x == TransactionId::DetachBuffer as u32 => {
                let slot = parcel_in.read::<i32>();
                status = Status::BadValue;
                log::warn!("BufferQueueProducer::transact DetachBuffer(slot={}) unimplemented", slot);
            }
            x if x == TransactionId::SetBufferCount as u32 => {
                let buffer_count = parcel_in.read::<i32>();
                status = self.set_buffer_count(buffer_count);
            }
            x if x == TransactionId::GetBufferHistory as u32 => {
                log::warn!("BufferQueueProducer::transact GetBufferHistory (STUBBED)");
            }
            x if x == TransactionId::DetachNextBuffer as u32 => {
                status = Status::BadValue;
                log::warn!("BufferQueueProducer::transact DetachNextBuffer unimplemented");
            }
            x if x == TransactionId::AttachBuffer as u32 => {
                status = Status::BadValue;
                log::warn!("BufferQueueProducer::transact AttachBuffer unimplemented");
            }
            x if x == TransactionId::AllocateBuffers as u32 => {
                status = Status::BadValue;
                log::warn!("BufferQueueProducer::transact AllocateBuffers unimplemented");
            }
            _ => {
                status = Status::BadValue;
                log::error!("BufferQueueProducer::transact unknown code={}", code);
            }
        }

        parcel_out.write(&status);
        let serialized = parcel_out.serialize();
        let copy_len = std::cmp::min(parcel_reply.len(), serialized.len());
        parcel_reply[..copy_len].copy_from_slice(&serialized[..copy_len]);
    }

    fn get_native_handle(&self, type_id: u32) -> Option<u32> {
        log::warn!("BufferQueueProducer::get_native_handle type_id={} (STUBBED)", type_id);
        None
    }
}
