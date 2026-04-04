// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/shared_buffer_manager.cpp/.h

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex, Weak};

use common::math_util::Rectangle;

use crate::core::SystemRef;
use crate::hle::kernel::k_memory_block::{KMemoryPermission, KMemoryState, PAGE_SIZE};
use crate::hle::kernel::k_page_group::KPageGroup;
use crate::hle::kernel::k_process::KProcess;
use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::kernel::k_scheduler::KScheduler;
use crate::hle::kernel::k_typed_address::KProcessAddress;
use crate::hle::service::nvdrv::core::container::SessionId;
use crate::hle::service::nvdrv::devices::nvmap::{IocAllocParams, IocCreateParams, IocFreeParams};
use crate::hle::service::nvdrv::devices::nvmap::NvMapDevice;
use crate::hle::service::nvdrv::nvdrv::Module;
use crate::hle::service::nvdrv::nvdata::DeviceFD;
use crate::hle::service::nvnflinger::binder::IBinder;
use crate::hle::service::nvnflinger::buffer_queue_producer::BufferQueueProducer;
use crate::hle::service::nvnflinger::graphic_buffer_producer::{QueueBufferInput, QueueBufferOutput};
use crate::hle::service::nvnflinger::pixel_format::PixelFormat;
use crate::hle::service::nvnflinger::status::Status;
use crate::hle::service::nvnflinger::ui::fence::Fence;
use crate::hle::service::nvnflinger::ui::graphic_buffer::NvGraphicBuffer;
use crate::hle::service::nvnflinger::window::NativeWindowTransform;
use crate::hle::service::vi::container::Container;
use crate::hle::service::vi::vi_results;

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct SharedMemorySlot {
    pub buffer_offset: u64,
    pub size: u64,
    pub width: i32,
    pub height: i32,
}
const _: () = assert!(core::mem::size_of::<SharedMemorySlot>() == 0x18);

#[derive(Clone, Copy)]
#[repr(C)]
pub struct SharedMemoryPoolLayout {
    pub num_slots: i32,
    pub _padding: [u8; 4],
    pub slots: [SharedMemorySlot; 0x10],
}
const _: () = assert!(core::mem::size_of::<SharedMemoryPoolLayout>() == 0x188);

#[derive(Debug, Clone, Copy, Default)]
pub struct SharedBufferSession {
    pub nvmap_fd: DeviceFD,
    pub session_id: SessionId,
    pub layer_id: u64,
    pub buffer_nvmap_handle: u32,
    pub map_address: u64,
}

const SHARED_BUFFER_BLOCK_LINEAR_FORMAT: PixelFormat = PixelFormat::Rgba8888;
const SHARED_BUFFER_BLOCK_LINEAR_BPP: u32 = 4;
const SHARED_BUFFER_BLOCK_LINEAR_WIDTH: u32 = 1280;
const SHARED_BUFFER_BLOCK_LINEAR_HEIGHT: u32 = 768;
const SHARED_BUFFER_BLOCK_LINEAR_STRIDE: u32 =
    SHARED_BUFFER_BLOCK_LINEAR_WIDTH * SHARED_BUFFER_BLOCK_LINEAR_BPP;
const SHARED_BUFFER_NUM_SLOTS: u32 = 7;
const SHARED_BUFFER_WIDTH: u32 = 1280;
const SHARED_BUFFER_HEIGHT: u32 = 720;
const SHARED_BUFFER_ASYNC: bool = false;
const SHARED_BUFFER_SLOT_SIZE: u32 = SHARED_BUFFER_BLOCK_LINEAR_WIDTH
    * SHARED_BUFFER_BLOCK_LINEAR_HEIGHT
    * SHARED_BUFFER_BLOCK_LINEAR_BPP;
const SHARED_BUFFER_SIZE: u32 = SHARED_BUFFER_SLOT_SIZE * SHARED_BUFFER_NUM_SLOTS;

const SHARED_BUFFER_POOL_LAYOUT: SharedMemoryPoolLayout = {
    let mut layout = SharedMemoryPoolLayout {
        num_slots: SHARED_BUFFER_NUM_SLOTS as i32,
        _padding: [0; 4],
        slots: [SharedMemorySlot {
            buffer_offset: 0,
            size: 0,
            width: 0,
            height: 0,
        }; 0x10],
    };

    let mut i = 0;
    while i < SHARED_BUFFER_NUM_SLOTS as usize {
        layout.slots[i] = SharedMemorySlot {
            buffer_offset: (i as u64) * SHARED_BUFFER_SLOT_SIZE as u64,
            size: SHARED_BUFFER_SLOT_SIZE as u64,
            width: SHARED_BUFFER_WIDTH as i32,
            height: SHARED_BUFFER_HEIGHT as i32,
        };
        i += 1;
    }
    layout
};

struct SharedBufferManagerInner {
    next_buffer_id: u64,
    display_id: u64,
    buffer_id: u64,
    sessions: BTreeMap<u64, SharedBufferSession>,
    buffer_page_group: Option<KPageGroup>,
}

pub struct SharedBufferManager {
    inner: Mutex<SharedBufferManagerInner>,
    system: SystemRef,
    container: Weak<Container>,
    nvdrv: Arc<Module>,
}

impl SharedBufferManager {
    pub fn new(system: SystemRef, container: Weak<Container>, nvdrv: Arc<Module>) -> Self {
        Self {
            inner: Mutex::new(SharedBufferManagerInner {
                next_buffer_id: 1,
                display_id: 0,
                buffer_id: 0,
                sessions: BTreeMap::new(),
                buffer_page_group: None,
            }),
            system,
            container,
            nvdrv,
        }
    }

    fn container(&self) -> Result<Arc<Container>, crate::hle::result::ResultCode> {
        self.container
            .upgrade()
            .ok_or(vi_results::RESULT_OPERATION_FAILED)
    }

    fn allocate_shared_buffer_memory(&self) -> Result<KPageGroup, crate::hle::result::ResultCode> {
        let system_ptr = self.system.get() as *const crate::core::System as *mut crate::core::System;
        let device_memory: *const crate::device_memory::DeviceMemory =
            unsafe { (*system_ptr).device_memory() as *const crate::device_memory::DeviceMemory };
        let kernel = unsafe {
            (*system_ptr)
                .kernel_mut()
                .ok_or(vi_results::RESULT_OPERATION_FAILED)?
        };

        use crate::hle::kernel::k_memory_manager::{Direction, KMemoryManager, Pool};
        let num_pages = (SHARED_BUFFER_SIZE as usize) / PAGE_SIZE;
        let option = KMemoryManager::encode_option(Pool::SECURE, Direction::FromBack);
        let phys = kernel
            .memory_manager_mut()
            .allocate_and_open_continuous(num_pages, 1, option);
        if phys == 0 {
            return Err(vi_results::RESULT_OPERATION_FAILED);
        }

        let mut pg = KPageGroup::new();
        pg.add_block(phys, num_pages)
            .map_err(|_| vi_results::RESULT_OPERATION_FAILED)?;

        let start = unsafe { (*device_memory).get_pointer(phys) as *mut u32 };
        let end =
            unsafe { (*device_memory).get_pointer(phys + SHARED_BUFFER_SIZE as u64) as *mut u32 };
        let mut cur = start;
        while cur < end {
            unsafe {
                *cur = 0xFF0000FF;
                cur = cur.add(1);
            }
        }

        Ok(pg)
    }

    fn map_shared_buffer_into_process_address_space(
        &self,
        pg: &KPageGroup,
        process: &mut KProcess,
    ) -> Result<u64, crate::hle::result::ResultCode> {
        let alias_code_begin = process.page_table.get_alias_code_region_start().get() as usize;
        let alias_code_pages = process.page_table.get_alias_code_region_size() / PAGE_SIZE;
        if alias_code_pages == 0 {
            return Err(vi_results::RESULT_OPERATION_FAILED);
        }

        let seed = process.get_random_entropy(0);
        for i in 0..64usize {
            let page_index = ((seed as usize).wrapping_add(i * 0x9E37)) % alias_code_pages;
            let address = alias_code_begin + page_index * PAGE_SIZE;
            let result = process.page_table.map_page_group(
                KProcessAddress::new(address as u64),
                pg,
                KMemoryState::IO_MEMORY,
                KMemoryPermission::USER_READ | KMemoryPermission::USER_WRITE,
            );
            if result == 0 {
                return Ok(address as u64);
            }
        }

        Err(vi_results::RESULT_OPERATION_FAILED)
    }

    fn make_graphic_buffer(&self, producer: &BufferQueueProducer, slot: u32, handle: u32) {
        let mut buffer = NvGraphicBuffer::new(
            SHARED_BUFFER_WIDTH,
            SHARED_BUFFER_HEIGHT,
            SHARED_BUFFER_BLOCK_LINEAR_FORMAT,
            0,
        );
        buffer.stride = SHARED_BUFFER_BLOCK_LINEAR_STRIDE as i32;
        buffer.external_format = SHARED_BUFFER_BLOCK_LINEAR_FORMAT;
        buffer.buffer_id = handle;
        buffer.offset = slot * SHARED_BUFFER_SLOT_SIZE;
        let buffer = Arc::new(buffer);
        debug_assert_eq!(
            producer.set_preallocated_buffer(slot as i32, Some(buffer)),
            Status::NoError
        );
    }

    pub fn create_session(
        &self,
        owner_process: &Arc<Mutex<KProcess>>,
        display_id: u64,
        enable_blending: bool,
    ) -> Result<(u64, u64), crate::hle::result::ResultCode> {
        let aruid = owner_process.lock().unwrap().get_process_id();
        let mut inner = self.inner.lock().unwrap();
        if inner.sessions.contains_key(&aruid) {
            return Err(vi_results::RESULT_PERMISSION_DENIED);
        }

        if inner.buffer_page_group.is_none() {
            inner.buffer_page_group = Some(self.allocate_shared_buffer_memory()?);
            inner.buffer_id = inner.next_buffer_id;
            inner.next_buffer_id += 1;
            inner.display_id = display_id;
        }

        let map_address = {
            let pg = inner.buffer_page_group.as_ref().unwrap();
            self.map_shared_buffer_into_process_address_space(pg, &mut owner_process.lock().unwrap())?
        };

        let session_id = self.nvdrv.get_container().open_session(owner_process);
        let nvmap_fd = self.nvdrv.open("/dev/nvmap", session_id);
        let nvmap = self
            .nvdrv
            .get_nvmap_device(nvmap_fd)
            .ok_or(vi_results::RESULT_OPERATION_FAILED)?;

        let mut create_params = IocCreateParams {
            size: SHARED_BUFFER_SIZE,
            handle: 0,
        };
        if nvmap.ioc_create(&mut create_params) != crate::hle::service::nvdrv::nvdata::NvResult::Success
        {
            return Err(vi_results::RESULT_OPERATION_FAILED);
        }

        let mut alloc_params = IocAllocParams {
            handle: create_params.handle,
            heap_mask: 0,
            flags: Default::default(),
            align: 0,
            kind: 0,
            _padding: [0; 7],
            address: map_address,
        };
        if nvmap.ioc_alloc(&mut alloc_params, nvmap_fd)
            != crate::hle::service::nvdrv::nvdata::NvResult::Success
        {
            return Err(vi_results::RESULT_OPERATION_FAILED);
        }

        let container = self.container()?;
        let (_producer_binder_id, layer_id) = container.create_stray_layer(display_id)?;
        container.set_layer_blending(layer_id, enable_blending)?;

        let producer = container.get_layer_producer_handle(layer_id)?;
        self.make_graphic_buffer(&producer, 0, create_params.handle);
        self.make_graphic_buffer(&producer, 1, create_params.handle);

        inner.sessions.insert(
            aruid,
            SharedBufferSession {
                nvmap_fd,
                session_id,
                layer_id,
                buffer_nvmap_handle: create_params.handle,
                map_address,
            },
        );

        Ok((inner.buffer_id, layer_id))
    }

    pub fn destroy_session(&self, owner_process: &Arc<Mutex<KProcess>>) {
        let aruid = owner_process.lock().unwrap().get_process_id();
        let mut inner = self.inner.lock().unwrap();
        let Some(session) = inner.sessions.remove(&aruid) else {
            return;
        };
        drop(inner);

        if let Ok(container) = self.container() {
            let _ = container.destroy_stray_layer(session.layer_id);
        }

        if let Some(nvmap) = self.nvdrv.get_nvmap_device(session.nvmap_fd) {
            let mut free = IocFreeParams {
                handle: session.buffer_nvmap_handle,
                ..Default::default()
            };
            let _ = nvmap.ioc_free(&mut free, session.nvmap_fd);
        }

        self.nvdrv.close(session.nvmap_fd);
        self.nvdrv.get_container().close_session(session.session_id);

        if let Some(pg) = self.inner.lock().unwrap().buffer_page_group.as_ref() {
            let _ = owner_process.lock().unwrap().page_table.unmap_page_group(
                KProcessAddress::new(session.map_address),
                pg,
                KMemoryState::IO_MEMORY,
            );
        }
    }

    pub fn get_shared_buffer_memory_handle_id(
        &self,
        buffer_id: u64,
        applet_resource_user_id: u64,
    ) -> Result<(u64, i32, SharedMemoryPoolLayout), crate::hle::result::ResultCode> {
        let inner = self.inner.lock().unwrap();
        if inner.buffer_id == 0 || buffer_id != inner.buffer_id {
            return Err(vi_results::RESULT_NOT_FOUND);
        }
        let session = inner
            .sessions
            .get(&applet_resource_user_id)
            .ok_or(vi_results::RESULT_NOT_FOUND)?;
        Ok((
            SHARED_BUFFER_SIZE as u64,
            session.buffer_nvmap_handle as i32,
            SHARED_BUFFER_POOL_LAYOUT,
        ))
    }

    pub fn acquire_shared_frame_buffer(
        &self,
        layer_id: u64,
    ) -> Result<(Fence, [i32; 4], i64), crate::hle::result::ResultCode> {
        let container = self.container()?;
        let producer = container.get_layer_producer_handle(layer_id)?;
        let (status, slot, fence) = producer.dequeue_buffer(
            SHARED_BUFFER_ASYNC,
            SHARED_BUFFER_WIDTH,
            SHARED_BUFFER_HEIGHT,
            SHARED_BUFFER_BLOCK_LINEAR_FORMAT,
            0,
        );
        if status != Status::NoError as i32 {
            return Err(vi_results::RESULT_OPERATION_FAILED);
        }
        Ok((fence, [0, 1, -1, -1], slot as i64))
    }

    pub fn present_shared_frame_buffer(
        &self,
        fence: Fence,
        crop_region: Rectangle<i32>,
        transform: u32,
        swap_interval: i32,
        layer_id: u64,
        slot: i64,
    ) -> Result<(), crate::hle::result::ResultCode> {
        let container = self.container()?;
        let producer = container.get_layer_producer_handle(layer_id)?;
        let (status, _buffer) = producer.request_buffer(slot as i32);
        if status != Status::NoError {
            return Err(vi_results::RESULT_OPERATION_FAILED);
        }

        let mut input = QueueBufferInput::default();
        input.crop = crop_region;
        input.fence = fence;
        input.transform = NativeWindowTransform::from_bits_retain(transform);
        input.swap_interval = swap_interval;

        let (status, _output): (Status, QueueBufferOutput) =
            producer.queue_buffer(slot as i32, &input);
        if status != Status::NoError {
            return Err(vi_results::RESULT_OPERATION_FAILED);
        }
        Ok(())
    }

    pub fn cancel_shared_frame_buffer(
        &self,
        layer_id: u64,
        slot: i64,
    ) -> Result<(), crate::hle::result::ResultCode> {
        let container = self.container()?;
        let producer = container.get_layer_producer_handle(layer_id)?;
        producer.cancel_buffer(slot as i32, &Fence::no_fence());
        Ok(())
    }

    pub fn get_shared_frame_buffer_acquirable_event(
        &self,
        layer_id: u64,
    ) -> Result<Arc<Mutex<KReadableEvent>>, crate::hle::result::ResultCode> {
        let container = self.container()?;
        let producer = container.get_layer_producer_handle(layer_id)?;
        IBinder::get_native_handle(&*producer, 0).ok_or(vi_results::RESULT_OPERATION_FAILED)
    }
}
