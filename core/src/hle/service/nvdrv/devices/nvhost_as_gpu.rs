// SPDX-FileCopyrightText: 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: 2021 Skyline Team and Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvhost_as_gpu.h
//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvhost_as_gpu.cpp

use std::collections::BTreeMap;
use std::sync::Mutex;

use common::address_space::FlatAllocator;

use crate::hle::service::nvdrv::core::container::SessionId;
use crate::hle::service::nvdrv::devices::nvdevice::NvDevice;
use crate::hle::service::nvdrv::devices::nvmap::{read_struct, write_struct};
use crate::hle::service::nvdrv::nvdata::{DeviceFD, Ioctl, NvResult};

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
    pub struct MappingFlags: u32 {
        const NONE = 0;
        const FIXED = 1 << 0;
        const SPARSE = 1 << 1;
        const REMAP = 1 << 8;
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct VaRegion {
    pub offset: u64,
    pub page_size: u32,
    pub _pad0: u32,
    pub pages: u64,
}
const _: () = assert!(std::mem::size_of::<VaRegion>() == 0x18);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlAllocAsEx {
    pub flags: u32,
    pub as_fd: i32,
    pub big_page_size: u32,
    pub reserved: u32,
    pub va_range_start: u64,
    pub va_range_end: u64,
    pub va_range_split: u64,
}
const _: () = assert!(std::mem::size_of::<IoctlAllocAsEx>() == 40);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlAllocSpace {
    pub pages: u32,
    pub page_size: u32,
    pub flags: u32,
    pub _pad: u32,
    pub offset: u64,
}
const _: () = assert!(std::mem::size_of::<IoctlAllocSpace>() == 24);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlFreeSpace {
    pub offset: u64,
    pub pages: u32,
    pub page_size: u32,
}
const _: () = assert!(std::mem::size_of::<IoctlFreeSpace>() == 16);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlRemapEntry {
    pub flags: u16,
    pub kind: u16,
    pub handle: u32,
    pub handle_offset_big_pages: u32,
    pub as_offset_big_pages: u32,
    pub big_pages: u32,
}
const _: () = assert!(std::mem::size_of::<IoctlRemapEntry>() == 20);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlMapBufferEx {
    pub flags: u32,
    pub kind: u32,
    pub handle: u32,
    pub page_size: u32,
    pub buffer_offset: i64,
    pub mapping_size: u64,
    pub offset: i64,
}
const _: () = assert!(std::mem::size_of::<IoctlMapBufferEx>() == 40);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlUnmapBuffer {
    pub offset: i64,
}
const _: () = assert!(std::mem::size_of::<IoctlUnmapBuffer>() == 8);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlBindChannel {
    pub fd: i32,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlGetVaRegions {
    pub buf_addr: u64,
    pub buf_size: u32,
    pub reserved: u32,
    pub regions: [VaRegion; 2],
}
const _: () = assert!(
    std::mem::size_of::<IoctlGetVaRegions>() == 16 + std::mem::size_of::<VaRegion>() * 2
);

struct Mapping {
    handle: u32,
    ptr: u64,
    offset: u64,
    size: u64,
    fixed: bool,
    big_page: bool,
    sparse_alloc: bool,
}

struct Allocation {
    size: u64,
    page_size: u32,
    sparse: bool,
    big_pages: bool,
}

struct VM {
    big_page_size: u32,
    big_page_size_bits: u32,
    va_range_start: u64,
    va_range_split: u64,
    va_range_end: u64,
    small_page_allocator: Option<FlatAllocator>,
    big_page_allocator: Option<FlatAllocator>,
    initialised: bool,
}

impl Default for VM {
    fn default() -> Self {
        const DEFAULT_BIG_PAGE_SIZE: u32 = 0x20000;
        Self {
            big_page_size: DEFAULT_BIG_PAGE_SIZE,
            big_page_size_bits: DEFAULT_BIG_PAGE_SIZE.trailing_zeros(),
            va_range_start: (DEFAULT_BIG_PAGE_SIZE as u64) << 10,
            va_range_split: 1u64 << 34,
            va_range_end: 1u64 << 37,
            small_page_allocator: None,
            big_page_allocator: None,
            initialised: false,
        }
    }
}

impl VM {
    const YUZU_PAGESIZE: u32 = 0x1000;
    const PAGE_SIZE_BITS: u32 = 12; // countr_zero(0x1000)
    const SUPPORTED_BIG_PAGE_SIZES: u32 = 0x30000;
    const VA_START_SHIFT: u32 = 10;
}

/// nvhost_as_gpu device.
pub struct NvHostAsGpu {
    vm: Mutex<VM>,
    mapping_map: Mutex<BTreeMap<u64, Mapping>>,
    allocation_map: Mutex<BTreeMap<u64, Allocation>>,
}

impl NvHostAsGpu {
    pub fn new() -> Self {
        Self {
            vm: Mutex::new(VM::default()),
            mapping_map: Mutex::new(BTreeMap::new()),
            allocation_map: Mutex::new(BTreeMap::new()),
        }
    }

    pub fn alloc_as_ex(&self, params: &mut IoctlAllocAsEx) -> NvResult {
        log::debug!("nvhost_as_gpu::AllocAsEx called, big_page_size=0x{:X}", params.big_page_size);

        let mut vm = self.vm.lock().unwrap();

        if vm.initialised {
            log::error!("Cannot initialise an address space twice!");
            return NvResult::InvalidState;
        }

        if params.big_page_size != 0 {
            if !params.big_page_size.is_power_of_two() {
                log::error!("Non power-of-2 big page size: 0x{:X}!", params.big_page_size);
                return NvResult::BadValue;
            }

            if (params.big_page_size & VM::SUPPORTED_BIG_PAGE_SIZES) == 0 {
                log::error!("Unsupported big page size: 0x{:X}!", params.big_page_size);
                return NvResult::BadValue;
            }

            vm.big_page_size = params.big_page_size;
            vm.big_page_size_bits = params.big_page_size.trailing_zeros();
            vm.va_range_start = (params.big_page_size as u64) << VM::VA_START_SHIFT;
        }

        if params.va_range_start != 0 {
            vm.va_range_start = params.va_range_start;
            vm.va_range_split = params.va_range_split;
            vm.va_range_end = params.va_range_end;
        }

        let start_pages = (vm.va_range_start >> VM::PAGE_SIZE_BITS) as u32;
        let end_pages = (vm.va_range_split >> VM::PAGE_SIZE_BITS) as u32;
        vm.small_page_allocator = Some(FlatAllocator::new(start_pages, end_pages));

        let start_big_pages = (vm.va_range_split >> vm.big_page_size_bits) as u32;
        let end_big_pages = ((vm.va_range_end - vm.va_range_split) >> vm.big_page_size_bits) as u32;
        vm.big_page_allocator = Some(FlatAllocator::new(start_big_pages, end_big_pages));

        vm.initialised = true;

        NvResult::Success
    }

    pub fn allocate_space(&self, params: &mut IoctlAllocSpace) -> NvResult {
        log::debug!(
            "nvhost_as_gpu::AllocateSpace called, pages={:X}, page_size={:X}, flags={:X}",
            params.pages,
            params.page_size,
            params.flags
        );

        let mut vm = self.vm.lock().unwrap();
        if !vm.initialised {
            return NvResult::BadValue;
        }

        if params.page_size != VM::YUZU_PAGESIZE && params.page_size != vm.big_page_size {
            return NvResult::BadValue;
        }

        let flags = MappingFlags::from_bits_truncate(params.flags);

        if params.page_size != vm.big_page_size && flags.contains(MappingFlags::SPARSE) {
            log::error!("Sparse small pages are not implemented!");
            return NvResult::NotImplemented;
        }

        let page_size_bits = if params.page_size == VM::YUZU_PAGESIZE {
            VM::PAGE_SIZE_BITS
        } else {
            vm.big_page_size_bits
        };

        let allocator = if params.page_size == VM::YUZU_PAGESIZE {
            vm.small_page_allocator.as_mut()
        } else {
            vm.big_page_allocator.as_mut()
        };
        let Some(allocator) = allocator else {
            return NvResult::InvalidState;
        };

        if flags.contains(MappingFlags::FIXED) {
            allocator.allocate_fixed((params.offset >> page_size_bits) as u32, params.pages);
        } else {
            let Some(offset_pages) = allocator.allocate(params.pages) else {
                return NvResult::InsufficientMemory;
            };
            params.offset = (offset_pages as u64) << page_size_bits;
        }

        let size = (params.pages as u64) * (params.page_size as u64);
        let mut alloc_map = self.allocation_map.lock().unwrap();
        alloc_map.insert(
            params.offset,
            Allocation {
                size,
                page_size: params.page_size,
                sparse: flags.contains(MappingFlags::SPARSE),
                big_pages: params.page_size != VM::YUZU_PAGESIZE,
            },
        );
        NvResult::Success
    }

    pub fn free_space(&self, params: &mut IoctlFreeSpace) -> NvResult {
        log::debug!(
            "nvhost_as_gpu::FreeSpace called, offset={:X}, pages={:X}, page_size={:X}",
            params.offset,
            params.pages,
            params.page_size
        );

        let mut vm = self.vm.lock().unwrap();
        if !vm.initialised {
            return NvResult::BadValue;
        }

        let mut alloc_map = self.allocation_map.lock().unwrap();
        if let Some(alloc) = alloc_map.get(&params.offset) {
            if alloc.page_size != params.page_size
                || alloc.size != (params.pages as u64) * (params.page_size as u64)
            {
                return NvResult::BadValue;
            }
            let page_size_bits = if alloc.big_pages {
                vm.big_page_size_bits
            } else {
                VM::PAGE_SIZE_BITS
            };
            let allocator = if alloc.big_pages {
                vm.big_page_allocator.as_mut()
            } else {
                vm.small_page_allocator.as_mut()
            };
            if let Some(allocator) = allocator {
                allocator.free(
                    (params.offset >> page_size_bits) as u32,
                    (alloc.size >> page_size_bits) as u32,
                );
            }
            alloc_map.remove(&params.offset);
            NvResult::Success
        } else {
            NvResult::BadValue
        }
    }

    pub fn remap(&self, _entries: &[IoctlRemapEntry]) -> NvResult {
        log::debug!("nvhost_as_gpu::Remap called (stubbed)");
        // Stubbed: Full implementation requires GPU memory manager.
        NvResult::Success
    }

    pub fn map_buffer_ex(&self, params: &mut IoctlMapBufferEx) -> NvResult {
        log::debug!(
            "nvhost_as_gpu::MapBufferEx called, flags={:X}, handle={:X}, offset={}",
            params.flags,
            params.handle,
            params.offset
        );

        let mut vm = self.vm.lock().unwrap();
        if !vm.initialised {
            return NvResult::BadValue;
        }

        let flags = MappingFlags::from_bits_truncate(params.flags);
        let size = if params.mapping_size != 0 {
            params.mapping_size
        } else {
            params.page_size.max(VM::YUZU_PAGESIZE) as u64
        };

        if flags.contains(MappingFlags::REMAP) {
            let mapping_map = self.mapping_map.lock().unwrap();
            let Some(mapping) = mapping_map.get(&(params.offset as u64)) else {
                return NvResult::BadValue;
            };
            if mapping.size < size {
                return NvResult::BadValue;
            }
            return NvResult::Success;
        }

        let big_page = if params.page_size == vm.big_page_size {
            true
        } else if params.page_size == 0 || params.page_size == VM::YUZU_PAGESIZE {
            false
        } else {
            return NvResult::BadValue;
        };

        if flags.contains(MappingFlags::FIXED) {
            let alloc_map = self.allocation_map.lock().unwrap();
            let Some((alloc_start, alloc)) = alloc_map.range(..=(params.offset as u64)).next_back() else {
                return NvResult::BadValue;
            };
            if (params.offset as u64) < *alloc_start
                || ((params.offset as u64) - *alloc_start) + size > alloc.size
            {
                return NvResult::BadValue;
            }
            let use_big_pages = alloc.big_pages && big_page;
            drop(alloc_map);
            self.mapping_map.lock().unwrap().insert(
                params.offset as u64,
                Mapping {
                    handle: params.handle,
                    ptr: 0,
                    offset: params.offset as u64,
                    size,
                    fixed: true,
                    big_page: use_big_pages,
                    sparse_alloc: false,
                },
            );
            return NvResult::Success;
        }

        let page_size = if big_page {
            vm.big_page_size
        } else {
            VM::YUZU_PAGESIZE
        };
        let page_size_bits = if big_page {
            vm.big_page_size_bits
        } else {
            VM::PAGE_SIZE_BITS
        };
        let allocator = if big_page {
            vm.big_page_allocator.as_mut()
        } else {
            vm.small_page_allocator.as_mut()
        };
        let Some(allocator) = allocator else {
            return NvResult::InvalidState;
        };
        let aligned_size = (size + page_size as u64 - 1) & !((page_size as u64) - 1);
        let Some(offset_pages) = allocator.allocate((aligned_size >> page_size_bits) as u32) else {
            return NvResult::InsufficientMemory;
        };
        params.offset = ((offset_pages as u64) << page_size_bits) as i64;
        self.mapping_map.lock().unwrap().insert(
            params.offset as u64,
            Mapping {
                handle: params.handle,
                ptr: 0,
                offset: params.offset as u64,
                size,
                fixed: false,
                big_page,
                sparse_alloc: false,
            },
        );
        NvResult::Success
    }

    pub fn unmap_buffer(&self, params: &mut IoctlUnmapBuffer) -> NvResult {
        log::debug!("nvhost_as_gpu::UnmapBuffer called, offset=0x{:X}", params.offset);

        let vm = self.vm.lock().unwrap();
        if !vm.initialised {
            return NvResult::BadValue;
        }
        drop(vm);

        let mut mapping_map = self.mapping_map.lock().unwrap();
        mapping_map.remove(&(params.offset as u64));

        NvResult::Success
    }

    pub fn bind_channel(&self, params: &mut IoctlBindChannel) -> NvResult {
        log::debug!("nvhost_as_gpu::BindChannel called, fd={:X}", params.fd);
        // Stubbed: Full implementation requires GPU channel binding.
        NvResult::Success
    }

    pub fn get_va_regions(&self, params: &mut IoctlGetVaRegions) -> NvResult {
        log::debug!(
            "nvhost_as_gpu::GetVARegions called, buf_addr={:X}, buf_size={:X}",
            params.buf_addr,
            params.buf_size
        );

        let vm = self.vm.lock().unwrap();
        if !vm.initialised {
            return NvResult::BadValue;
        }

        params.buf_size = 2 * std::mem::size_of::<VaRegion>() as u32;
        // Stubbed: would compute from allocators
        params.regions[0] = VaRegion {
            offset: vm.va_range_start,
            page_size: VM::YUZU_PAGESIZE,
            _pad0: 0,
            pages: (vm.va_range_split - vm.va_range_start) >> VM::PAGE_SIZE_BITS,
        };
        params.regions[1] = VaRegion {
            offset: vm.va_range_split,
            page_size: vm.big_page_size,
            _pad0: 0,
            pages: (vm.va_range_end - vm.va_range_split) >> vm.big_page_size_bits,
        };

        NvResult::Success
    }
}

impl NvDevice for NvHostAsGpu {
    fn ioctl1(
        &self,
        _fd: DeviceFD,
        command: Ioctl,
        input: &[u8],
        output: &mut [u8],
    ) -> NvResult {
        match command.group() {
            b'A' => match command.cmd() {
                0x1 => {
                    let mut params: IoctlBindChannel = read_struct(input);
                    let r = self.bind_channel(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x2 => {
                    let mut params: IoctlAllocSpace = read_struct(input);
                    let r = self.allocate_space(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x3 => {
                    let mut params: IoctlFreeSpace = read_struct(input);
                    let r = self.free_space(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x5 => {
                    let mut params: IoctlUnmapBuffer = read_struct(input);
                    let r = self.unmap_buffer(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x6 => {
                    let mut params: IoctlMapBufferEx = read_struct(input);
                    let r = self.map_buffer_ex(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x8 => {
                    let mut params: IoctlGetVaRegions = read_struct(input);
                    let r = self.get_va_regions(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x9 => {
                    let mut params: IoctlAllocAsEx = read_struct(input);
                    let r = self.alloc_as_ex(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x14 => {
                    // Remap: variable-length entries
                    let entry_size = std::mem::size_of::<IoctlRemapEntry>();
                    let num_entries = input.len() / entry_size;
                    let mut entries = Vec::with_capacity(num_entries);
                    for i in 0..num_entries {
                        let start = i * entry_size;
                        let end = start + entry_size;
                        if end <= input.len() {
                            entries.push(read_struct::<IoctlRemapEntry>(&input[start..end]));
                        }
                    }
                    self.remap(&entries)
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
        input: &[u8],
        output: &mut [u8],
        inline_output: &mut [u8],
    ) -> NvResult {
        match command.group() {
            b'A' => match command.cmd() {
                0x8 => {
                    let mut params: IoctlGetVaRegions = read_struct(input);
                    let r = self.get_va_regions(&mut params);
                    write_struct(output, &params);
                    // Write inline output
                    let region_size = std::mem::size_of::<VaRegion>();
                    for (i, region) in params.regions.iter().enumerate() {
                        let start = i * region_size;
                        if start + region_size <= inline_output.len() {
                            write_struct(&mut inline_output[start..], region);
                        }
                    }
                    r
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

    fn on_open(&self, _session_id: SessionId, _fd: DeviceFD) {}
    fn on_close(&self, _fd: DeviceFD) {}

    fn query_event(&self, event_id: u32) -> Option<u32> {
        log::error!("Unknown AS GPU Event {}", event_id);
        None
    }
}

#[cfg(test)]
mod tests {
    use super::{IoctlAllocAsEx, IoctlAllocSpace, IoctlMapBufferEx, NvHostAsGpu};
    use crate::hle::service::nvdrv::nvdata::NvResult;

    #[test]
    fn allocate_space_returns_non_zero_offset_for_dynamic_allocations() {
        let gpu_as = NvHostAsGpu::new();
        let mut alloc_as = IoctlAllocAsEx::default();
        assert_eq!(gpu_as.alloc_as_ex(&mut alloc_as), NvResult::Success);

        let mut params = IoctlAllocSpace {
            pages: 1,
            page_size: 0x1000,
            ..Default::default()
        };
        assert_eq!(gpu_as.allocate_space(&mut params), NvResult::Success);
        assert_ne!(params.offset, 0);
    }

    #[test]
    fn map_buffer_ex_returns_non_zero_offset_for_dynamic_mappings() {
        let gpu_as = NvHostAsGpu::new();
        let mut alloc_as = IoctlAllocAsEx::default();
        assert_eq!(gpu_as.alloc_as_ex(&mut alloc_as), NvResult::Success);

        let mut params = IoctlMapBufferEx {
            mapping_size: 0x1000,
            page_size: 0x1000,
            handle: 4,
            ..Default::default()
        };
        assert_eq!(gpu_as.map_buffer_ex(&mut params), NvResult::Success);
        assert_ne!(params.offset, 0);
    }
}
