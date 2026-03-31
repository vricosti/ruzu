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

use std::sync::{Arc, Condvar, Mutex, Weak};

use crate::hle::kernel::k_event::KEvent;
use crate::hle::kernel::k_process::KProcess;
use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::kernel::k_scheduler::KScheduler;

use super::binder::IBinder;
use super::buffer_queue_core::BufferQueueCore;
use super::graphic_buffer_producer::{QueueBufferInput, QueueBufferOutput};
use super::parcel::{InputParcel, OutputParcel};
use super::pixel_format::PixelFormat;
use super::producer_listener::IProducerListener;
use super::status::Status;
use super::ui::fence::Fence;
use super::ui::graphic_buffer::{GraphicBuffer, NvGraphicBuffer};
use super::window::{NativeWindow, NativeWindowApi, NativeWindowScalingMode};

pub struct BufferQueueProducer {
    core: Arc<BufferQueueCore>,
    buffer_wait_event: Arc<Mutex<KEvent>>,
    buffer_wait_readable_event: Arc<Mutex<KReadableEvent>>,
    buffer_wait_event_owner: Mutex<Option<BufferWaitEventOwner>>,
    sticky_transform: Mutex<u32>,
    next_callback_ticket: Mutex<i32>,
    current_callback_ticket: Mutex<i32>,
    callback_condition: Condvar,
}

struct BufferWaitEventOwner {
    process: Weak<Mutex<KProcess>>,
    scheduler: Weak<Mutex<KScheduler>>,
}

impl BufferQueueProducer {
    pub fn new(core: Arc<BufferQueueCore>) -> Self {
        let (buffer_wait_event, buffer_wait_readable_event) = Self::new_buffer_wait_event();
        Self {
            core,
            buffer_wait_event,
            buffer_wait_readable_event,
            buffer_wait_event_owner: Mutex::new(None),
            sticky_transform: Mutex::new(0),
            next_callback_ticket: Mutex::new(0),
            current_callback_ticket: Mutex::new(0),
            callback_condition: Condvar::new(),
        }
    }

    fn new_buffer_wait_event() -> (Arc<Mutex<KEvent>>, Arc<Mutex<KReadableEvent>>) {
        static NEXT_EVENT_ID: std::sync::atomic::AtomicU64 =
            std::sync::atomic::AtomicU64::new(0x2100_0000);

        let event_object_id = NEXT_EVENT_ID.fetch_add(2, std::sync::atomic::Ordering::Relaxed);
        let readable_object_id = event_object_id + 1;
        let event = Arc::new(Mutex::new(KEvent::new()));
        let readable_event = Arc::new(Mutex::new(KReadableEvent::new()));
        readable_event
            .lock()
            .unwrap()
            .initialize(event_object_id, readable_object_id);
        event.lock().unwrap().initialize(0, readable_object_id);
        (event, readable_event)
    }

    fn signal_buffer_wait_event(&self) {
        let owner = self.buffer_wait_event_owner.lock().unwrap();
        let maybe_process = owner.as_ref().and_then(|owner| owner.process.upgrade());
        let maybe_scheduler = owner.as_ref().and_then(|owner| owner.scheduler.upgrade());
        drop(owner);

        if let (Some(process), Some(scheduler)) = (maybe_process, maybe_scheduler) {
            let mut process = process.lock().unwrap();
            self.buffer_wait_event
                .lock()
                .unwrap()
                .signal(&mut process, &scheduler);
        } else {
            self.buffer_wait_readable_event.lock().unwrap().is_signaled = true;
        }
    }

    fn wait_for_free_slot_then_relock<'a>(
        &self,
        async_flag: bool,
        found: &mut i32,
        return_flags: &mut i32,
        mut inner: std::sync::MutexGuard<'a, super::buffer_queue_core::BufferQueueCoreInner>,
    ) -> (
        Status,
        std::sync::MutexGuard<'a, super::buffer_queue_core::BufferQueueCoreInner>,
    ) {
        let mut try_again = true;

        while try_again {
            if inner.is_abandoned {
                log::error!("BufferQueueProducer: BufferQueue has been abandoned");
                return (Status::NoInit, inner);
            }

            let max_buffer_count = inner.get_max_buffer_count_locked(async_flag);
            if async_flag
                && inner.override_max_buffer_count != 0
                && inner.override_max_buffer_count < max_buffer_count
            {
                *found = BufferQueueCore::INVALID_BUFFER_SLOT;
                return (Status::BadValue, inner);
            }

            for slot in max_buffer_count as usize..super::buffer_queue_defs::NUM_BUFFER_SLOTS {
                debug_assert!(
                    inner.slots[slot].buffer_state == super::buffer_slot::BufferState::Free
                );
                if inner.slots[slot].graphic_buffer.is_some()
                    && inner.slots[slot].buffer_state == super::buffer_slot::BufferState::Free
                    && !inner.slots[slot].is_preallocated
                {
                    inner.free_buffer_locked(slot as i32);
                    *return_flags |= Status::RELEASE_ALL_BUFFERS as i32;
                }
            }

            *found = BufferQueueCore::INVALID_BUFFER_SLOT;
            let mut dequeued_count = 0;
            let mut acquired_count = 0;
            for slot in 0..max_buffer_count as usize {
                match inner.slots[slot].buffer_state {
                    super::buffer_slot::BufferState::Dequeued => {
                        dequeued_count += 1;
                    }
                    super::buffer_slot::BufferState::Acquired => {
                        acquired_count += 1;
                    }
                    super::buffer_slot::BufferState::Free => {
                        if *found == BufferQueueCore::INVALID_BUFFER_SLOT
                            || inner.slots[slot].frame_number
                                < inner.slots[*found as usize].frame_number
                        {
                            *found = slot as i32;
                        }
                    }
                    _ => {}
                }
            }

            if inner.override_max_buffer_count == 0 && dequeued_count != 0 {
                log::error!(
                    "BufferQueueProducer: can't dequeue multiple buffers without setting the buffer count"
                );
                return (Status::InvalidOperation, inner);
            }

            if inner.buffer_has_been_queued {
                let new_undequeued_count = max_buffer_count - (dequeued_count + 1);
                let min_undequeued_count = inner.get_min_undequeued_buffer_count_locked(async_flag);
                if new_undequeued_count < min_undequeued_count {
                    log::error!(
                        "BufferQueueProducer: min undequeued buffer count({}) exceeded (dequeued={} undequeued={})",
                        min_undequeued_count,
                        dequeued_count,
                        new_undequeued_count
                    );
                    return (Status::InvalidOperation, inner);
                }
            }

            let too_many_buffers = inner.queue.len() > max_buffer_count as usize;
            try_again = (*found == BufferQueueCore::INVALID_BUFFER_SLOT) || too_many_buffers;
            if try_again {
                if inner.dequeue_buffer_cannot_block
                    && acquired_count <= inner.max_acquired_buffer_count
                {
                    return (Status::WouldBlock, inner);
                }
                inner = self.core.wait_for_dequeue_condition(inner);
            }
        }

        (Status::NoError, inner)
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
        } else if inner.slots[slot as usize].buffer_state
            != super::buffer_slot::BufferState::Dequeued
        {
            log::error!(
                "BufferQueueProducer: slot {} is not owned by producer (state={:?})",
                slot,
                inner.slots[slot as usize].buffer_state
            );
            return (Status::BadValue, None);
        }
        inner.slots[slot as usize].request_buffer_called = true;
        let buf = inner.slots[slot as usize].graphic_buffer.clone();
        (Status::NoError, buf)
    }

    pub fn set_buffer_count(&self, buffer_count: i32) -> Status {
        log::debug!(
            "BufferQueueProducer::set_buffer_count count={}",
            buffer_count
        );
        let mut inner = self.core.mutex.lock().unwrap();
        inner.wait_while_allocating_locked();

        if inner.is_abandoned {
            log::error!("BufferQueueProducer: BufferQueue has been abandoned");
            return Status::NoInit;
        }

        if buffer_count > super::buffer_queue_defs::NUM_BUFFER_SLOTS as i32 {
            log::error!(
                "BufferQueueProducer: buffer_count {} too large",
                buffer_count
            );
            return Status::BadValue;
        }

        for slot in 0..super::buffer_queue_defs::NUM_BUFFER_SLOTS {
            if inner.slots[slot].buffer_state == super::buffer_slot::BufferState::Dequeued {
                log::error!("BufferQueueProducer: buffer owned by producer");
                return Status::BadValue;
            }
        }

        if buffer_count == 0 {
            inner.override_max_buffer_count = 0;
            drop(inner);
            self.core.signal_dequeue_condition();
            return Status::NoError;
        }

        let min_buffer_slots = inner.get_min_max_buffer_count_locked(false);
        if buffer_count < min_buffer_slots {
            log::error!(
                "BufferQueueProducer: requested buffer count {} is less than minimum {}",
                buffer_count,
                min_buffer_slots
            );
            return Status::BadValue;
        }

        if inner.get_preallocated_buffer_count_locked() <= 0 {
            inner.free_all_buffers_locked();
        }

        inner.override_max_buffer_count = buffer_count;
        let listener = inner.consumer_listener.clone();
        drop(inner);
        self.core.signal_dequeue_condition();
        self.signal_buffer_wait_event();
        if let Some(listener) = listener {
            listener.on_buffers_released();
        }

        Status::NoError
    }

    pub fn dequeue_buffer(
        &self,
        async_flag: bool,
        mut width: u32,
        mut height: u32,
        mut format: PixelFormat,
        mut usage: u32,
    ) -> (i32, i32, Fence) {
        if (width != 0 && height == 0) || (width == 0 && height != 0) {
            log::error!(
                "BufferQueueProducer: invalid size: w={} h={}",
                width,
                height
            );
            return (Status::BadValue as i32, -1, Fence::default());
        }

        let mut return_flags = Status::NoError as i32;
        let attached_by_consumer;
        let out_slot;
        let out_fence;
        {
            let mut inner = self.core.mutex.lock().unwrap();
            inner.wait_while_allocating_locked();

            if format == PixelFormat::NoFormat {
                format = inner.default_buffer_format;
            }
            usage |= inner.consumer_usage_bit;

            let mut found = 0;
            let (status, mut inner) = self.wait_for_free_slot_then_relock(
                async_flag,
                &mut found,
                &mut return_flags,
                inner,
            );
            if status != Status::NoError {
                return (status as i32, -1, Fence::default());
            }

            if found == BufferQueueCore::INVALID_BUFFER_SLOT {
                log::error!("BufferQueueProducer: no available buffer slots");
                return (Status::Busy as i32, -1, Fence::default());
            }

            out_slot = found;
            attached_by_consumer = inner.slots[found as usize].attached_by_consumer;

            if width == 0 && height == 0 {
                width = inner.default_width;
                height = inner.default_height;
            }

            inner.slots[found as usize].buffer_state = super::buffer_slot::BufferState::Dequeued;

            let needs_reallocation = match inner.slots[found as usize].graphic_buffer.as_ref() {
                Some(buffer) => {
                    buffer.get_width() != width
                        || buffer.get_height() != height
                        || buffer.get_format() != format
                        || (buffer.get_usage() & usage) != usage
                }
                None => true,
            };

            if needs_reallocation {
                inner.slots[found as usize].acquire_called = false;
                inner.slots[found as usize].graphic_buffer = None;
                inner.slots[found as usize].request_buffer_called = false;
                inner.slots[found as usize].fence = Fence::no_fence();
                return_flags |= Status::BUFFER_NEEDS_REALLOCATION as i32;
            }

            out_fence = inner.slots[found as usize].fence;
            inner.slots[found as usize].fence = Fence::no_fence();
        }

        if (return_flags & Status::BUFFER_NEEDS_REALLOCATION as i32) != 0 {
            log::debug!(
                "BufferQueueProducer::dequeue_buffer allocating a new buffer for slot {}",
                out_slot
            );
            let graphic_buffer = Arc::new(GraphicBuffer::new(width, height, format, usage));
            {
                let mut inner = self.core.mutex.lock().unwrap();
                if inner.is_abandoned {
                    log::error!("BufferQueueProducer: BufferQueue has been abandoned");
                    return (Status::NoInit as i32, -1, Fence::default());
                }
                inner.slots[out_slot as usize].frame_number = u32::MAX as u64;
                inner.slots[out_slot as usize].graphic_buffer = Some(graphic_buffer);
            }
        }

        if attached_by_consumer {
            return_flags |= Status::BUFFER_NEEDS_REALLOCATION as i32;
        }

        log::debug!(
            "BufferQueueProducer::dequeue_buffer returning slot={} flags={}",
            out_slot,
            return_flags
        );
        (return_flags, out_slot, out_fence)
    }

    pub fn queue_buffer(&self, slot: i32, input: &QueueBufferInput) -> (Status, QueueBufferOutput) {
        match input.scaling_mode {
            NativeWindowScalingMode::Freeze
            | NativeWindowScalingMode::ScaleToWindow
            | NativeWindowScalingMode::ScaleCrop
            | NativeWindowScalingMode::NoScaleCrop
            | NativeWindowScalingMode::PreserveAspectRatio => {}
        }

        let mut inner = self.core.mutex.lock().unwrap();

        if inner.is_abandoned {
            return (Status::NoInit, QueueBufferOutput::new());
        }

        let max_buffer_count = inner.get_max_buffer_count_locked(input.async_flag != 0);
        if input.async_flag != 0
            && inner.override_max_buffer_count != 0
            && inner.override_max_buffer_count < max_buffer_count
        {
            return (Status::BadValue, QueueBufferOutput::new());
        }

        if slot < 0 || slot >= max_buffer_count {
            return (Status::BadValue, QueueBufferOutput::new());
        }

        let s = slot as usize;
        if inner.slots[s].buffer_state != super::buffer_slot::BufferState::Dequeued {
            log::error!(
                "BufferQueueProducer::queue_buffer: slot {} not dequeued (state={:?})",
                slot,
                inner.slots[s].buffer_state
            );
            return (Status::BadValue, QueueBufferOutput::new());
        }

        inner.slots[s].buffer_state = super::buffer_slot::BufferState::Queued;
        inner.slots[s].fence = input.fence;
        inner.frame_counter += 1;
        inner.slots[s].frame_number = inner.frame_counter;

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
            is_droppable: inner.dequeue_buffer_cannot_block || input.async_flag != 0,
            acquire_called: false,
            transform_to_display_inverse: false,
        };
        let mut callback_item = super::buffer_item::BufferItem {
            graphic_buffer: item.graphic_buffer.clone(),
            fence: item.fence,
            crop: item.crop,
            transform: item.transform,
            scaling_mode: item.scaling_mode,
            timestamp: item.timestamp,
            is_auto_timestamp: item.is_auto_timestamp,
            frame_number: item.frame_number,
            slot: item.slot,
            is_droppable: item.is_droppable,
            acquire_called: item.acquire_called,
            transform_to_display_inverse: item.transform_to_display_inverse,
            swap_interval: item.swap_interval,
        };
        let mut frame_available_listener = None;
        let mut frame_replaced_listener = None;

        *self.sticky_transform.lock().unwrap() = input.sticky_transform;

        if inner.queue.is_empty() {
            inner.queue.push(item);
            frame_available_listener = inner.consumer_listener.clone();
        } else {
            let front_is_droppable = inner.queue[0].is_droppable;
            if front_is_droppable {
                let (front_slot, front_still_tracking) = {
                    let front = &inner.queue[0];
                    (front.slot, inner.still_tracking(front))
                };
                if front_still_tracking {
                    inner.slots[front_slot as usize].buffer_state =
                        super::buffer_slot::BufferState::Free;
                    inner.slots[front_slot as usize].frame_number = 0;
                }
                inner.queue[0] = item;
                frame_replaced_listener = inner.consumer_listener.clone();
            } else {
                inner.queue.push(item);
                frame_available_listener = inner.consumer_listener.clone();
            }
        }
        inner.buffer_has_been_queued = true;

        let mut output = QueueBufferOutput::new();
        output.inflate(
            inner.default_width,
            inner.default_height,
            inner.transform_hint,
            inner.queue.len() as u32,
        );

        let callback_ticket = {
            let mut next_callback_ticket = self.next_callback_ticket.lock().unwrap();
            let ticket = *next_callback_ticket;
            *next_callback_ticket += 1;
            ticket
        };

        drop(inner);
        self.core.signal_dequeue_condition();
        callback_item.graphic_buffer = None;
        callback_item.slot = super::buffer_item::BufferItem::INVALID_BUFFER_SLOT;

        let mut current_callback_ticket = self.current_callback_ticket.lock().unwrap();
        while callback_ticket != *current_callback_ticket {
            current_callback_ticket = self
                .callback_condition
                .wait(current_callback_ticket)
                .unwrap();
        }

        if let Some(listener) = frame_available_listener {
            listener.on_frame_available(&callback_item);
        } else if let Some(listener) = frame_replaced_listener {
            listener.on_frame_replaced(&callback_item);
        }

        *current_callback_ticket += 1;
        self.callback_condition.notify_all();
        drop(current_callback_ticket);

        log::debug!(
            "BufferQueueProducer::queue_buffer slot={} frame={}",
            slot,
            self.core.mutex.lock().unwrap().frame_counter
        );
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
        self.signal_buffer_wait_event();
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
            log::error!(
                "BufferQueueProducer: already connected (api={:?})",
                inner.connected_api
            );
            return (Status::BadValue, QueueBufferOutput::new());
        }

        inner.connected_api = api;
        inner.connected_producer_listener = listener;
        inner.buffer_has_been_queued = false;
        inner.dequeue_buffer_cannot_block =
            inner.consumer_controlled_by_app && producer_controlled_by_app;

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
        inner.queue.clear();
        inner.free_all_buffers_locked();
        drop(inner);
        self.core.signal_dequeue_condition();
        self.signal_buffer_wait_event();

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
            inner.slots[s].graphic_buffer =
                Some(Arc::new(GraphicBuffer::from_nv_buffer(*buf, false)));
            inner.slots[s].is_preallocated = true;
            inner.override_max_buffer_count = inner.get_preallocated_buffer_count_locked();

            if inner.default_width != buf.get_width() || inner.default_height != buf.get_height() {
                inner.default_width = buf.get_width();
                inner.default_height = buf.get_height();
            }
            inner.default_buffer_format = buf.get_format();
        }

        drop(inner);
        self.core.signal_dequeue_condition();
        self.signal_buffer_wait_event();

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
        let mut raw_status: Option<i32> = None;
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
                raw_status = Some(new_status);
                status = match new_status {
                    0 => Status::NoError,
                    1 => Status::BUFFER_NEEDS_REALLOCATION,
                    2 => Status::RELEASE_ALL_BUFFERS,
                    3 => Status::BUFFER_NEEDS_REALLOCATION,
                    -11 => Status::WouldBlock,
                    -12 => Status::NoMemory,
                    -16 => Status::Busy,
                    -19 => Status::NoInit,
                    -22 => Status::BadValue,
                    -38 => Status::InvalidOperation,
                    _ => Status::NoError,
                };
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
                        log::error!(
                            "BufferQueueProducer::transact Query unknown what={}",
                            what_raw
                        );
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
                log::warn!(
                    "BufferQueueProducer::transact DetachBuffer(slot={}) unimplemented",
                    slot
                );
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

        let status_to_write = raw_status.unwrap_or(status as i32);
        parcel_out.write(&status_to_write);
        let serialized = parcel_out.serialize();
        let copy_len = std::cmp::min(parcel_reply.len(), serialized.len());
        parcel_reply[..copy_len].copy_from_slice(&serialized[..copy_len]);
    }

    fn get_native_handle(&self, _type_id: u32) -> Option<Arc<Mutex<KReadableEvent>>> {
        Some(Arc::clone(&self.buffer_wait_readable_event))
    }

    fn register_native_handle_owner(
        &self,
        process: Arc<Mutex<KProcess>>,
        scheduler: Arc<Mutex<KScheduler>>,
    ) {
        {
            let mut process_guard = process.lock().unwrap();
            let event_object_id = self
                .buffer_wait_readable_event
                .lock()
                .unwrap()
                .get_parent_id()
                .unwrap_or(0);
            let readable_object_id = self.buffer_wait_readable_event.lock().unwrap().object_id;

            self.buffer_wait_event.lock().unwrap().owner_process_id =
                Some(process_guard.process_id);
            process_guard
                .register_event_object(event_object_id, Arc::clone(&self.buffer_wait_event));
            process_guard.register_readable_event_object(
                readable_object_id,
                Arc::clone(&self.buffer_wait_readable_event),
            );
        }

        *self.buffer_wait_event_owner.lock().unwrap() = Some(BufferWaitEventOwner {
            process: Arc::downgrade(&process),
            scheduler: Arc::downgrade(&scheduler),
        });
    }
}

#[cfg(test)]
mod tests {
    use super::super::graphic_buffer_producer::QueueBufferInput;
    use super::super::pixel_format::PixelFormat;
    use super::*;

    #[test]
    fn get_native_handle_returns_persistent_buffer_wait_event() {
        let core = BufferQueueCore::new();
        let producer = BufferQueueProducer::new(core);

        let first = producer.get_native_handle(0).unwrap();
        let second = producer.get_native_handle(15).unwrap();

        assert!(Arc::ptr_eq(&first, &second));
    }

    #[test]
    fn connect_sets_nonblocking_flag_from_core_and_producer_control() {
        let core = BufferQueueCore::new();
        core.mutex.lock().unwrap().consumer_controlled_by_app = true;
        let producer = BufferQueueProducer::new(core.clone());

        let (status, _) = producer.connect(None, NativeWindowApi::Egl, true);
        assert_eq!(status, Status::NoError);
        assert!(core.mutex.lock().unwrap().dequeue_buffer_cannot_block);
    }

    #[test]
    fn disconnect_signals_buffer_wait_event() {
        let core = BufferQueueCore::new();
        let producer = BufferQueueProducer::new(core);
        let event = producer.get_native_handle(0).unwrap();
        assert!(!event.lock().unwrap().is_signaled());

        let (status, _) = producer.connect(None, NativeWindowApi::Egl, false);
        assert_eq!(status, Status::NoError);
        assert_eq!(producer.disconnect(NativeWindowApi::Egl), Status::NoError);

        assert!(event.lock().unwrap().is_signaled());
    }

    #[test]
    fn set_preallocated_buffer_signals_wait_event_and_updates_defaults() {
        let core = BufferQueueCore::new();
        let producer = BufferQueueProducer::new(core.clone());
        let event = producer.get_native_handle(0).unwrap();
        let buffer = Arc::new(NvGraphicBuffer::new(1280, 720, PixelFormat::Rgba8888, 0));

        assert_eq!(
            producer.set_preallocated_buffer(0, Some(buffer)),
            Status::NoError
        );
        assert!(event.lock().unwrap().is_signaled());

        let inner = core.mutex.lock().unwrap();
        assert_eq!(inner.default_width, 1280);
        assert_eq!(inner.default_height, 720);
        assert_eq!(inner.override_max_buffer_count, 1);
    }

    #[test]
    fn queue_buffer_marks_core_as_having_queued_buffers() {
        let core = BufferQueueCore::new();
        let producer = BufferQueueProducer::new(core.clone());
        let buffer = Arc::new(NvGraphicBuffer::new(16, 16, PixelFormat::Rgba8888, 0));
        assert_eq!(
            producer.set_preallocated_buffer(0, Some(buffer)),
            Status::NoError
        );

        let (status, slot, _fence) =
            producer.dequeue_buffer(false, 16, 16, PixelFormat::Rgba8888, 0);
        assert_eq!(status, Status::NoError as i32);
        assert_eq!(slot, 0);

        let (status, _) = producer.queue_buffer(slot, &QueueBufferInput::default());
        assert_eq!(status, Status::NoError);
        assert!(core.mutex.lock().unwrap().buffer_has_been_queued);
    }

    #[test]
    fn request_buffer_rejects_slot_not_owned_by_producer() {
        let core = BufferQueueCore::new();
        let producer = BufferQueueProducer::new(core.clone());
        core.mutex.lock().unwrap().slots[0].graphic_buffer = Some(Arc::new(GraphicBuffer::new(
            16,
            16,
            PixelFormat::Rgba8888,
            0,
        )));

        let (status, buffer) = producer.request_buffer(0);
        assert_eq!(status, Status::BadValue);
        assert!(buffer.is_none());
    }

    #[test]
    fn dequeue_buffer_requires_explicit_buffer_count_for_second_dequeue() {
        let core = BufferQueueCore::new();
        let producer = BufferQueueProducer::new(core.clone());
        assert_eq!(
            producer.set_preallocated_buffer(
                0,
                Some(Arc::new(NvGraphicBuffer::new(
                    16,
                    16,
                    PixelFormat::Rgba8888,
                    0
                )))
            ),
            Status::NoError
        );
        assert_eq!(
            producer.set_preallocated_buffer(
                1,
                Some(Arc::new(NvGraphicBuffer::new(
                    16,
                    16,
                    PixelFormat::Rgba8888,
                    0
                )))
            ),
            Status::NoError
        );

        let (status, slot, _) = producer.dequeue_buffer(false, 16, 16, PixelFormat::Rgba8888, 0);
        assert_eq!(status, Status::NoError as i32);
        assert_eq!(slot, 0);

        let (status, slot, _) = producer.dequeue_buffer(false, 16, 16, PixelFormat::Rgba8888, 0);
        assert_eq!(status, Status::InvalidOperation as i32);
        assert_eq!(slot, -1);
    }

    #[test]
    fn dequeue_buffer_sets_reallocation_flag_for_empty_slot() {
        let core = BufferQueueCore::new();
        let producer = BufferQueueProducer::new(core.clone());
        core.mutex.lock().unwrap().override_max_buffer_count = 1;

        let (status, slot, _) = producer.dequeue_buffer(false, 32, 32, PixelFormat::Rgba8888, 0);
        assert_eq!(status, Status::BUFFER_NEEDS_REALLOCATION as i32);
        assert_eq!(slot, 0);

        let inner = core.mutex.lock().unwrap();
        assert!(inner.slots[0].graphic_buffer.is_some());
        assert_eq!(
            inner.slots[0].buffer_state,
            super::super::buffer_slot::BufferState::Dequeued
        );
        assert!(!inner.slots[0].request_buffer_called);
    }
}
