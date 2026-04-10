// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvmap.h
//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvmap.cpp

use std::collections::HashMap;
use std::sync::Mutex;

use crate::hle::kernel::k_memory_block::KMemoryPermission;
use crate::hle::service::nvdrv::core::container::{Container, SessionId};
use crate::hle::service::nvdrv::core::nvmap as nvmap_core;
use crate::hle::service::nvdrv::devices::nvdevice::NvDevice;
use crate::hle::service::nvdrv::nvdata::{DeviceFD, Ioctl, NvResult};

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HandleParameterType {
    Size = 1,
    Alignment = 2,
    Base = 3,
    Heap = 4,
    Kind = 5,
    IsSharedMemMapped = 6,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IocCreateParams {
    pub size: u32,
    pub handle: u32,
}
const _: () = assert!(std::mem::size_of::<IocCreateParams>() == 8);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IocFromIdParams {
    pub id: u32,
    pub handle: u32,
}
const _: () = assert!(std::mem::size_of::<IocFromIdParams>() == 8);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IocAllocParams {
    pub handle: u32,
    pub heap_mask: u32,
    pub flags: nvmap_core::HandleFlags,
    pub align: u32,
    pub kind: u8,
    pub _padding: [u8; 7],
    pub address: u64,
}
const _: () = assert!(std::mem::size_of::<IocAllocParams>() == 32);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IocFreeParams {
    pub handle: u32,
    pub _padding: [u8; 4],
    pub address: u64,
    pub size: u32,
    pub flags: nvmap_core::HandleFlags,
}
const _: () = assert!(std::mem::size_of::<IocFreeParams>() == 24);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IocParamParams {
    pub handle: u32,
    pub param: u32,
    pub result: u32,
}
const _: () = assert!(std::mem::size_of::<IocParamParams>() == 12);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IocGetIdParams {
    pub id: u32,
    pub handle: u32,
}
const _: () = assert!(std::mem::size_of::<IocGetIdParams>() == 8);

/// The nvmap device file.
pub struct NvMapDevice {
    file: *const nvmap_core::NvMap,
    container: *const Container,
    sessions: Mutex<HashMap<DeviceFD, SessionId>>,
}

// Safety: NvMapDevice is only accessed from the service thread.
unsafe impl Send for NvMapDevice {}
unsafe impl Sync for NvMapDevice {}

impl NvMapDevice {
    pub fn new(file: &nvmap_core::NvMap, container: &Container) -> Self {
        Self {
            file: file as *const _,
            container: container as *const _,
            sessions: Mutex::new(HashMap::new()),
        }
    }

    fn file(&self) -> &nvmap_core::NvMap {
        unsafe { &*self.file }
    }

    fn container(&self) -> &Container {
        unsafe { &*self.container }
    }

    pub fn ioc_create(&self, params: &mut IocCreateParams) -> NvResult {
        log::debug!("nvmap::IocCreate called, size=0x{:08X}", params.size);

        if params.size == 0 {
            log::error!("Failed to create Object");
            return NvResult::BadValue;
        }

        let page_size: u64 = 0x1000;
        let aligned_size = (params.size as u64 + page_size - 1) & !(page_size - 1);
        match self.file().create_handle(aligned_size) {
            Ok(handle) => {
                handle.set_orig_size(params.size as u64);
                params.handle = handle.id;
                log::debug!("handle: {}, size: 0x{:X}", handle.id, params.size);
                NvResult::Success
            }
            Err(result) => {
                log::error!("Failed to create Object");
                result
            }
        }
    }

    pub fn ioc_alloc(&self, params: &mut IocAllocParams, fd: DeviceFD) -> NvResult {
        log::debug!("nvmap::IocAlloc called, addr={:X}", params.address);

        if params.handle == 0 {
            log::error!("Handle is 0");
            return NvResult::BadValue;
        }

        if (params.align.wrapping_sub(1)) & params.align != 0 {
            log::error!("Incorrect alignment used, alignment={:08X}", params.align);
            return NvResult::BadValue;
        }

        let page_size: u32 = 0x1000;
        if params.align < page_size {
            params.align = page_size;
        }

        let handle = self.file().get_handle(params.handle);
        if handle.is_none() {
            log::error!("Object does not exist, handle={:08X}", params.handle);
            return NvResult::BadValue;
        }

        let handle = handle.unwrap();
        {
            let inner = handle.lock_inner();
            if inner.allocated {
                log::error!("Object is already allocated, handle={:08X}", params.handle);
                return NvResult::InsufficientMemory;
            }
        }

        let session_id = {
            let sessions = self.sessions.lock().unwrap();
            sessions.get(&fd).copied().unwrap_or_default()
        };

        let result = handle.alloc(
            params.flags,
            params.align,
            params.kind,
            params.address,
            session_id,
        );
        if result != NvResult::Success {
            log::error!("Object failed to allocate, handle={:08X}", params.handle);
            return result;
        }

        let process = self
            .container()
            .get_session_process(session_id)
            .expect("nvmap::IocAlloc active session must own a process like upstream");

        let handle_size = {
            let inner = handle.lock_inner();
            inner.size as usize
        };
        let lock_result = {
            let mut process_guard = process.lock().unwrap();
            process_guard
                .page_table
                .get_base_mut()
                .lock_for_map_device_address_space(
                    params.address as usize,
                    handle_size,
                    KMemoryPermission::NONE,
                    true,
                )
        };
        debug_assert_eq!(lock_result, 0);

        NvResult::Success
    }

    pub fn ioc_get_id(&self, params: &mut IocGetIdParams) -> NvResult {
        log::debug!("nvmap::IocGetId called");

        if params.handle == 0 {
            log::error!("Error!");
            return NvResult::BadValue;
        }

        let handle = self.file().get_handle(params.handle);
        if handle.is_none() {
            log::error!("Error!");
            return NvResult::AccessDenied;
        }

        params.id = handle.unwrap().id;
        NvResult::Success
    }

    pub fn ioc_from_id(&self, params: &mut IocFromIdParams) -> NvResult {
        log::debug!("nvmap::IocFromId called, id:{}", params.id);

        if params.id == 0 {
            log::error!("Zero Id is invalid!");
            return NvResult::BadValue;
        }

        let handle = self.file().get_handle(params.id);
        if handle.is_none() {
            log::error!("Unregistered handle!");
            return NvResult::BadValue;
        }

        let handle = handle.unwrap();
        let result = handle.duplicate(false);
        if result != NvResult::Success {
            log::error!("Could not duplicate handle!");
            return result;
        }
        params.handle = handle.id;
        NvResult::Success
    }

    pub fn ioc_param(&self, params: &mut IocParamParams) -> NvResult {
        log::debug!("nvmap::IocParam called type={}", params.param);

        if params.handle == 0 {
            log::error!("Invalid handle!");
            return NvResult::BadValue;
        }

        let handle = self.file().get_handle(params.handle);
        if handle.is_none() {
            log::error!("Not registered handle!");
            return NvResult::BadValue;
        }

        let handle = handle.unwrap();
        let inner = handle.lock_inner();
        match params.param {
            1 => {
                // Size
                params.result = handle.orig_size() as u32;
            }
            2 => {
                // Alignment
                params.result = inner.align as u32;
            }
            3 => {
                // Base
                params.result = (-22_i32) as u32; // posix EINVAL
            }
            4 => {
                // Heap
                if inner.allocated {
                    params.result = 0x40000000;
                } else {
                    params.result = 0;
                }
            }
            5 => {
                // Kind
                params.result = inner.kind as u32;
            }
            6 => {
                // IsSharedMemMapped
                params.result = inner.is_shared_mem_mapped as u32;
            }
            _ => {
                return NvResult::BadValue;
            }
        }

        NvResult::Success
    }

    pub fn ioc_free(&self, params: &mut IocFreeParams, fd: DeviceFD) -> NvResult {
        log::debug!("nvmap::IocFree called");

        if params.handle == 0 {
            log::error!("Handle null freed?");
            return NvResult::Success;
        }

        if let Some(free_info) = self.file().free_handle(params.handle, false) {
            if free_info.can_unlock {
                let sessions = self.sessions.lock().unwrap();
                let session_id = sessions.get(&fd).copied().unwrap_or_default();
                drop(sessions);

                if let Some(process) = self.container().get_session_process(session_id) {
                    let unlock_result = process
                        .lock()
                        .unwrap()
                        .page_table
                        .get_base_mut()
                        .unlock_for_device_address_space(
                            free_info.address as usize,
                            free_info.size as usize,
                        );
                    debug_assert_eq!(unlock_result, 0);
                }
            }
            params.address = free_info.address;
            params.size = free_info.size as u32;
            params.flags.raw = 0;
            if free_info.was_uncached {
                params.flags.set_map_uncached(true);
            }
        }

        NvResult::Success
    }
}

/// Helper to read a fixed-size struct from a byte slice.
pub fn read_struct<T: Copy + Default>(input: &[u8]) -> T {
    let mut val = T::default();
    let size = std::mem::size_of::<T>().min(input.len());
    if size > 0 {
        unsafe {
            std::ptr::copy_nonoverlapping(input.as_ptr(), &mut val as *mut T as *mut u8, size);
        }
    }
    val
}

/// Helper to write a fixed-size struct to a byte slice.
pub fn write_struct<T: Copy>(output: &mut [u8], val: &T) {
    let size = std::mem::size_of::<T>().min(output.len());
    if size > 0 {
        unsafe {
            std::ptr::copy_nonoverlapping(val as *const T as *const u8, output.as_mut_ptr(), size);
        }
    }
}

impl NvDevice for NvMapDevice {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn ioctl1(&self, fd: DeviceFD, command: Ioctl, input: &[u8], output: &mut [u8]) -> NvResult {
        match command.group() {
            0x1 => match command.cmd() {
                0x1 => {
                    let mut params: IocCreateParams = read_struct(input);
                    let result = self.ioc_create(&mut params);
                    write_struct(output, &params);
                    result
                }
                0x3 => {
                    let mut params: IocFromIdParams = read_struct(input);
                    let result = self.ioc_from_id(&mut params);
                    write_struct(output, &params);
                    result
                }
                0x4 => {
                    let mut params: IocAllocParams = read_struct(input);
                    let result = self.ioc_alloc(&mut params, fd);
                    write_struct(output, &params);
                    result
                }
                0x5 => {
                    let mut params: IocFreeParams = read_struct(input);
                    let result = self.ioc_free(&mut params, fd);
                    write_struct(output, &params);
                    result
                }
                0x9 => {
                    let mut params: IocParamParams = read_struct(input);
                    let result = self.ioc_param(&mut params);
                    write_struct(output, &params);
                    result
                }
                0xe => {
                    let mut params: IocGetIdParams = read_struct(input);
                    let result = self.ioc_get_id(&mut params);
                    write_struct(output, &params);
                    result
                }
                _ => {
                    log::error!("Unimplemented ioctl={:08X}", command.raw);
                    NvResult::NotImplemented
                }
            },
            _ => {
                log::error!("Unimplemented ioctl={:08X}", command.raw);
                NvResult::NotImplemented
            }
        }
    }

    fn ioctl2(
        &self,
        _fd: DeviceFD,
        command: Ioctl,
        _input: &[u8],
        _inline_input: &[u8],
        _output: &mut [u8],
    ) -> NvResult {
        log::error!("Unimplemented ioctl={:08X}", command.raw);
        NvResult::NotImplemented
    }

    fn ioctl3(
        &self,
        _fd: DeviceFD,
        command: Ioctl,
        _input: &[u8],
        _output: &mut [u8],
        _inline_output: &mut [u8],
    ) -> NvResult {
        log::error!("Unimplemented ioctl={:08X}", command.raw);
        NvResult::NotImplemented
    }

    fn on_open(&self, session_id: SessionId, fd: DeviceFD) {
        let mut sessions = self.sessions.lock().unwrap();
        sessions.insert(fd, session_id);
    }

    fn on_close(&self, fd: DeviceFD) {
        let mut sessions = self.sessions.lock().unwrap();
        sessions.remove(&fd);
    }
}

#[cfg(test)]
mod tests {
    use super::{IocCreateParams, IocFromIdParams, NvMapDevice};
    use crate::hle::service::nvdrv::core::container::Container;
    use crate::hle::service::nvdrv::nvdata::NvResult;

    #[test]
    fn ioc_create_preserves_unaligned_orig_size() {
        let container = Container::new();
        let device = NvMapDevice::new(container.get_nv_map_file(), &container);
        let mut params = IocCreateParams {
            size: 0x1234,
            handle: 0,
        };

        let result = device.ioc_create(&mut params);

        assert_eq!(result, NvResult::Success);
        let handle = container.get_nv_map_file().get_handle(params.handle).unwrap();
        assert_eq!(handle.orig_size(), 0x1234);
        assert_eq!(handle.lock_inner().size, 0x2000);
    }

    #[test]
    fn ioc_from_id_propagates_duplicate_failure_for_unallocated_handle() {
        let container = Container::new();
        let device = NvMapDevice::new(container.get_nv_map_file(), &container);
        let handle = container.get_nv_map_file().create_handle(0x1000).unwrap();
        let mut params = IocFromIdParams {
            id: handle.id,
            handle: 0,
        };

        let result = device.ioc_from_id(&mut params);

        assert_eq!(result, NvResult::BadValue);
        assert_eq!(params.handle, 0);
    }
}
