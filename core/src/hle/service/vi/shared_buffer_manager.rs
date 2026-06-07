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
use crate::hle::kernel::k_process::ProcessLock;
use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::kernel::k_typed_address::KProcessAddress;
use crate::hle::service::nvdrv::core::container::SessionId;
use crate::hle::service::nvdrv::devices::nvmap::{IocAllocParams, IocCreateParams, IocFreeParams};
use crate::hle::service::nvdrv::nvdata::DeviceFD;
use crate::hle::service::nvdrv::nvdrv::Module;
use crate::hle::service::nvnflinger::binder::IBinder;
use crate::hle::service::nvnflinger::buffer_queue_producer::BufferQueueProducer;
use crate::hle::service::nvnflinger::graphic_buffer_producer::{
    QueueBufferInput, QueueBufferOutput,
};
use crate::hle::service::nvnflinger::pixel_format::PixelFormat;
use crate::hle::service::nvnflinger::status::{Status, StatusCode};
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

struct Mt19937_64 {
    state: [u64; 312],
    index: usize,
}

impl Mt19937_64 {
    const N: usize = 312;
    const M: usize = 156;
    const MATRIX_A: u64 = 0xB502_6F5A_A966_19E9;
    const UPPER_MASK: u64 = 0xFFFF_FFFF_8000_0000;
    const LOWER_MASK: u64 = 0x0000_0000_7FFF_FFFF;

    fn new(seed: u64) -> Self {
        let mut state = [0; Self::N];
        state[0] = seed;
        for i in 1..Self::N {
            state[i] = 6_364_136_223_846_793_005u64
                .wrapping_mul(state[i - 1] ^ (state[i - 1] >> 62))
                .wrapping_add(i as u64);
        }
        Self {
            state,
            index: Self::N,
        }
    }

    fn next_u64(&mut self) -> u64 {
        if self.index >= Self::N {
            self.twist();
        }

        let mut value = self.state[self.index];
        self.index += 1;

        value ^= (value >> 29) & 0x5555_5555_5555_5555;
        value ^= (value << 17) & 0x71D6_7FFF_EDA6_0000;
        value ^= (value << 37) & 0xFFF7_EEE0_0000_0000;
        value ^= value >> 43;
        value
    }

    fn twist(&mut self) {
        for i in 0..Self::N {
            let x = (self.state[i] & Self::UPPER_MASK)
                | (self.state[(i + 1) % Self::N] & Self::LOWER_MASK);
            let mut xa = x >> 1;
            if (x & 1) != 0 {
                xa ^= Self::MATRIX_A;
            }
            self.state[i] = self.state[(i + Self::M) % Self::N] ^ xa;
        }
        self.index = 0;
    }
}

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
        let system_ptr =
            self.system.get() as *const crate::core::System as *mut crate::core::System;
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

        let mut rng = Mt19937_64::new(process.get_random_entropy(0));
        for _ in 0..64usize {
            let page_index = (rng.next_u64() as usize) % alias_code_pages;
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
        owner_process: &Arc<ProcessLock>,
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
            self.map_shared_buffer_into_process_address_space(
                pg,
                &mut owner_process.lock().unwrap(),
            )?
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
        if nvmap.ioc_create(&mut create_params)
            != crate::hle::service::nvdrv::nvdata::NvResult::Success
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

    pub fn destroy_session(&self, owner_process: &Arc<ProcessLock>) {
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
        if status != StatusCode::NO_ERROR {
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

    pub fn write_applet_capture_buffer(
        &self,
    ) -> Result<(bool, i32), crate::hle::result::ResultCode> {
        let Some(gpu) = self.system.get().gpu_core() else {
            return Err(vi_results::RESULT_OPERATION_FAILED);
        };
        let Some(host1x) = self.system.get().host1x_core() else {
            return Err(vi_results::RESULT_OPERATION_FAILED);
        };
        let capture_buffer = gpu.get_applet_capture_buffer();
        let system_ptr =
            self.system.get() as *const crate::core::System as *mut crate::core::System;
        let device_memory: *const crate::device_memory::DeviceMemory =
            unsafe { (*system_ptr).device_memory() as *const crate::device_memory::DeviceMemory };

        let mut e = -((SHARED_BUFFER_BLOCK_LINEAR_WIDTH
            * SHARED_BUFFER_BLOCK_LINEAR_HEIGHT
            * SHARED_BUFFER_BLOCK_LINEAR_BPP) as i64);
        let inner = self.inner.lock().unwrap();
        let Some(buffer_page_group) = inner.buffer_page_group.as_ref() else {
            return Err(vi_results::RESULT_OPERATION_FAILED);
        };
        for block in buffer_page_group.iter() {
            let start = unsafe { (*device_memory).get_pointer(block.get_address()) as *mut u8 };
            let mut current = start;
            let size = block.get_size();
            for offset in 0..size {
                let value = if e >= 0 && (e as usize) < capture_buffer.len() {
                    capture_buffer[e as usize]
                } else {
                    0
                };
                unsafe {
                    *start.add(offset) = value;
                }
                current = unsafe { start.add(offset + 1) };
                e += 1;
            }

            let end = unsafe { start.add(size) };
            let invalidate_size = end as usize - current as usize;
            host1x.smmu_apply_op_on_host_pointer(current as usize, &mut |addr| {
                gpu.invalidate_region(addr, invalidate_size as u64);
            });
        }

        Ok((true, 1))
    }
}

#[cfg(test)]
mod tests {
    use super::Mt19937_64;

    #[test]
    fn mt19937_64_matches_std_reference_sequence() {
        let mut rng = Mt19937_64::new(5489);
        assert_eq!(rng.next_u64(), 14_514_284_786_278_117_030);
        assert_eq!(rng.next_u64(), 4_620_546_740_167_642_908);
        assert_eq!(rng.next_u64(), 13_109_570_281_517_897_720);
        assert_eq!(rng.next_u64(), 17_462_938_647_148_434_322);
        assert_eq!(rng.next_u64(), 355_488_278_567_739_596);
    }
}
