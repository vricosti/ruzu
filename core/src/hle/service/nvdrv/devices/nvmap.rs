// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvmap.h
//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvmap.cpp

use std::collections::{HashMap, VecDeque};
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

#[derive(Clone, Copy)]
struct NvmapHistoryEvent {
    sequence: u64,
    kind: &'static str,
    fd: DeviceFD,
    handle: u32,
    size: u64,
    address: u64,
    flags: u32,
    align: u32,
    heap_mask: u32,
}

static NVMAP_HISTORY: std::sync::OnceLock<Mutex<VecDeque<NvmapHistoryEvent>>> =
    std::sync::OnceLock::new();
static NVMAP_HISTORY_SEQUENCE: std::sync::atomic::AtomicU64 =
    std::sync::atomic::AtomicU64::new(0);

fn record_nvmap_history(event: NvmapHistoryEvent) {
    let mut history = NVMAP_HISTORY
        .get_or_init(|| Mutex::new(VecDeque::with_capacity(128)))
        .lock()
        .unwrap();
    if history.len() == 128 {
        history.pop_front();
    }
    history.push_back(NvmapHistoryEvent {
        sequence: NVMAP_HISTORY_SEQUENCE.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1,
        ..event
    });
}

fn dump_nvmap_history(reason: &str) {
    let Some(history) = NVMAP_HISTORY.get() else {
        return;
    };
    let history = history.lock().unwrap();
    eprintln!(
        "[NVMAP_HISTORY] reason={} events={}",
        reason,
        history.len()
    );
    for event in history.iter() {
        eprintln!(
            "[NVMAP_HISTORY] #{:05} {} fd={} handle=0x{:X} size=0x{:X} addr=0x{:X} flags=0x{:X} align=0x{:X} heap_mask=0x{:X}",
            event.sequence,
            event.kind,
            event.fd,
            event.handle,
            event.size,
            event.address,
            event.flags,
            event.align,
            event.heap_mask,
        );
    }
}

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
    fn should_trace_alloc_loop() -> bool {
        std::env::var_os("RUZU_TRACE_NVMAP_LOOP")
            .is_some_and(|value| value != std::ffi::OsStr::new("0"))
    }

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
        log::info!("nvmap::IocCreate called, size=0x{:08X}", params.size);

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
                record_nvmap_history(NvmapHistoryEvent {
                    sequence: 0,
                    kind: "create",
                    fd: 0,
                    handle: params.handle,
                    size: aligned_size,
                    address: 0,
                    flags: 0,
                    align: 0,
                    heap_mask: 0,
                });
                if Self::should_trace_alloc_loop() {
                    log::info!(
                        "nvmap::IocCreate size=0x{:X} aligned=0x{:X} handle=0x{:X}",
                        params.size,
                        aligned_size,
                        params.handle
                    );
                }
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
        log::info!(
            "nvmap::IocAlloc called, handle=0x{:X} addr=0x{:X}",
            params.handle,
            params.address
        );
        if Self::should_trace_alloc_loop() {
            log::info!(
                "nvmap::IocAlloc begin fd={} handle=0x{:X} heap_mask=0x{:X} flags=0x{:X} align=0x{:X} kind=0x{:X} addr=0x{:X}",
                fd,
                params.handle,
                params.heap_mask,
                params.flags.raw,
                params.align,
                params.kind,
                params.address
            );
        }

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

        let (handle_address, handle_size) = {
            let inner = handle.lock_inner();
            (inner.address as usize, inner.size as usize)
        };
        record_nvmap_history(NvmapHistoryEvent {
            sequence: 0,
            kind: "alloc",
            fd,
            handle: params.handle,
            size: handle_size as u64,
            address: handle_address as u64,
            flags: params.flags.raw,
            align: params.align,
            heap_mask: params.heap_mask,
        });
        let (lock_result, lock_start_info, lock_end_info) = {
            let mut process_guard = process.lock().unwrap();
            let lock_result = process_guard
                .page_table
                .get_base_mut()
                .lock_for_map_device_address_space(
                    handle_address,
                    handle_size,
                    KMemoryPermission::NONE,
                    true,
                );
            let start_info = process_guard.page_table.query_info(handle_address);
            let end_info = handle_size
                .checked_sub(1)
                .and_then(|last_offset| handle_address.checked_add(last_offset))
                .and_then(|last_addr| process_guard.page_table.query_info(last_addr));
            (lock_result, start_info, end_info)
        };
        if lock_result != 0 {
            dump_nvmap_history("lock_for_map_device_address_space_failed");
            crate::hle::service::nvdrv::nvdrv_interface::dump_nvdrv_ioctl_history(
                "nvmap_lock_for_map_device_address_space_failed",
            );
            crate::hle::kernel::svc::svc_memory_history::dump(
                "nvmap_lock_for_map_device_address_space_failed",
            );
            crate::hle::service::nvnflinger::diagnostics::dump(
                "nvmap_lock_for_map_device_address_space_failed",
            );
            log::error!(
                "nvmap::IocAlloc failed to lock CPU range for device mapping: handle=0x{:X} addr=0x{:X} size=0x{:X} result=0x{:08X}",
                params.handle,
                handle_address,
                handle_size,
                lock_result
            );
            if let Some(info) = lock_start_info {
                log::error!(
                    "nvmap::IocAlloc lock start state: base=0x{:X} size=0x{:X} state={:?} perm={:?} attr={:?} orig_perm={:?} ipc_locks={} device_use={}",
                    info.m_address,
                    info.m_size,
                    info.m_state,
                    info.m_permission,
                    info.m_attribute,
                    info.m_original_permission,
                    info.m_ipc_lock_count,
                    info.m_device_use_count
                );
            } else {
                log::error!("nvmap::IocAlloc lock start state: <missing>");
            }
            if let Some(info) = lock_end_info {
                log::error!(
                    "nvmap::IocAlloc lock end state: base=0x{:X} size=0x{:X} state={:?} perm={:?} attr={:?} orig_perm={:?} ipc_locks={} device_use={}",
                    info.m_address,
                    info.m_size,
                    info.m_state,
                    info.m_permission,
                    info.m_attribute,
                    info.m_original_permission,
                    info.m_ipc_lock_count,
                    info.m_device_use_count
                );
            } else {
                log::error!("nvmap::IocAlloc lock end state: <missing>");
            }
            // Upstream uses ASSERT(...IsSuccess()) here and still returns the
            // original nvmap result to the guest if asserts are ignored. Do
            // not turn this diagnostic into a guest-visible nvmap failure:
            // MK8D's caller treats that path as fatal and jumps through a null
            // callback. The page-table mismatch remains logged above.
        }

        {
            let mut inner = handle.lock_inner();
            if inner.d_address == 0 {
                let map_size = inner.aligned_size as usize;
                if let Some(d_address) = self.container().map_preallocated_area(
                    session_id,
                    inner.address,
                    map_size,
                ) {
                    inner.d_address = d_address;
                    inner.in_heap = true;
                    if std::env::var_os("RUZU_TRACE_NVMAP_PIN").is_some() {
                        eprintln!(
                            "[NVMAP_PIN] handle=0x{:X} vaddr=0x{:X} size=0x{:X} -> prealloc d_address=0x{:X}",
                            params.handle, inner.address, map_size, d_address
                        );
                    }
                }
            }
        }

        if Self::should_trace_alloc_loop() {
            let inner = handle.lock_inner();
            let host_ptr = process.lock().unwrap().get_memory().and_then(|memory| {
                let memory = memory.lock().unwrap();
                let ptr = memory.get_pointer_silent(inner.address);
                if ptr.is_null() {
                    None
                } else {
                    Some(ptr as usize)
                }
            });
            log::info!(
                "nvmap::IocAlloc end fd={} handle=0x{:X} result={} alloc_addr=0x{:X} size=0x{:X} host_ptr={:?}",
                fd,
                params.handle,
                result as u32,
                inner.address,
                inner.size,
                host_ptr.map(|ptr| format!("0x{ptr:X}"))
            );
        }

        if let Ok(raw_handle) = std::env::var("RUZU_STOP_NVMAP_ALLOC_HANDLE") {
            let digits = raw_handle
                .trim()
                .strip_prefix("0x")
                .or_else(|| raw_handle.trim().strip_prefix("0X"))
                .unwrap_or(raw_handle.trim());
            let parsed = u32::from_str_radix(digits, 16)
                .ok()
                .or_else(|| raw_handle.trim().parse::<u32>().ok());
            if parsed == Some(params.handle) {
                eprintln!(
                    "[NVMAP_ALLOC_STOP] handle=0x{:X} addr=0x{:X} size=0x{:X}",
                    params.handle, params.address, handle_size
                );
                unsafe {
                    libc::raise(libc::SIGSTOP);
                }
            }
        }

        NvResult::Success
    }

    pub fn ioc_get_id(&self, params: &mut IocGetIdParams) -> NvResult {
        log::info!("nvmap::IocGetId called, handle=0x{:X}", params.handle);

        if params.handle == 0 {
            log::error!("nvmap::IocGetId handle=0 invalid");
            return NvResult::BadValue;
        }

        let handle = self.file().get_handle(params.handle);
        if handle.is_none() {
            log::error!(
                "nvmap::IocGetId handle 0x{:X} not registered",
                params.handle
            );
            return NvResult::AccessDenied;
        }

        let id = handle.unwrap().id;
        params.id = id;
        log::info!("nvmap::IocGetId handle=0x{:X} -> id={}", params.handle, id);
        NvResult::Success
    }

    pub fn ioc_from_id(&self, params: &mut IocFromIdParams) -> NvResult {
        log::info!("nvmap::IocFromId called, id:{}", params.id);

        if params.id == 0 {
            log::error!("nvmap::IocFromId Zero Id is invalid!");
            return NvResult::BadValue;
        }

        let handle = self.file().get_handle(params.id);
        if handle.is_none() {
            log::error!("nvmap::IocFromId Unregistered handle id={}", params.id);
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
                    if std::env::var_os("RUZU_TRACE_IOCALLOC").is_some() {
                        let mut ih = String::new();
                        for (i, b) in input.iter().enumerate().take(48) {
                            if i > 0 && (i & 3) == 0 {
                                ih.push(' ');
                            }
                            use std::fmt::Write;
                            let _ = write!(ih, "{:02x}", b);
                        }
                        eprintln!("[IOCALLOC_IN] len=0x{:x} bytes={}", input.len(), ih);
                    }
                    let mut params: IocAllocParams = read_struct(input);
                    if std::env::var_os("RUZU_TRACE_IOCALLOC").is_some() {
                        eprintln!(
                            "[IOCALLOC_PARSED] handle=0x{:x} heap_mask=0x{:x} flags=0x{:x} align=0x{:x} kind=0x{:x} address=0x{:x}",
                            params.handle, params.heap_mask, params.flags.raw,
                            params.align, params.kind, params.address
                        );
                    }
                    let result = self.ioc_alloc(&mut params, fd);
                    if std::env::var_os("RUZU_TRACE_IOCALLOC").is_some() {
                        eprintln!(
                            "[IOCALLOC_POST] handle=0x{:x} heap_mask=0x{:x} flags=0x{:x} align=0x{:x} kind=0x{:x} address=0x{:x} result={}",
                            params.handle, params.heap_mask, params.flags.raw,
                            params.align, params.kind, params.address, result as u32
                        );
                    }
                    write_struct(output, &params);
                    if std::env::var_os("RUZU_TRACE_IOCALLOC").is_some() {
                        let mut oh = String::new();
                        for (i, b) in output.iter().enumerate().take(48) {
                            if i > 0 && (i & 3) == 0 {
                                oh.push(' ');
                            }
                            use std::fmt::Write;
                            let _ = write!(oh, "{:02x}", b);
                        }
                        eprintln!("[IOCALLOC_OUT] len=0x{:x} bytes={}", output.len(), oh);
                    }
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
        let handle = container
            .get_nv_map_file()
            .get_handle(params.handle)
            .unwrap();
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
