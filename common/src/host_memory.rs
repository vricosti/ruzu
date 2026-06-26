//! Port of zuyu/src/common/host_memory.h and zuyu/src/common/host_memory.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

use crate::alignment::align_up;
use crate::free_region_manager::FreeRegionManager;
use crate::virtual_buffer::VirtualBuffer;
use log::error;
#[cfg(target_os = "macos")]
use std::ffi::CString;
use std::ptr;
#[cfg(target_os = "macos")]
use std::sync::atomic::{AtomicU64, Ordering};

const PAGE_ALIGNMENT: usize = 0x1000;
const HUGE_PAGE_SIZE: usize = 0x200000;

bitflags::bitflags! {
    /// Memory permission flags, matching C++ Common::MemoryPermission.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct MemoryPermission: u32 {
        const READ = 1 << 0;
        const WRITE = 1 << 1;
        const READ_WRITE = Self::READ.bits() | Self::WRITE.bits();
        const EXECUTE = 1 << 2;
    }
}

/// Platform-specific implementation of host memory management.
#[cfg(any(target_os = "linux", target_os = "macos"))]
struct HostMemoryImpl {
    backing_size: usize,
    virtual_size: usize,
    backing_base: *mut u8,
    virtual_base: *mut u8,
    virtual_map_base: *mut u8,
    fd: i32,
    free_manager: FreeRegionManager,
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
impl HostMemoryImpl {
    fn new(backing_size: usize, virtual_size: usize) -> Result<Self, String> {
        unsafe {
            // Verify page size
            let page_size = libc::sysconf(libc::_SC_PAGESIZE);
            if page_size != 0x1000 {
                return Err(format!(
                    "page size {:#x} is incompatible with 4K paging",
                    page_size
                ));
            }

            let fd = create_backing_fd()?;
            if fd < 0 {
                return Err(format!(
                    "create backing fd failed: {}",
                    std::io::Error::last_os_error()
                ));
            }

            // Extend the file to backing_size
            let ret = libc::ftruncate(fd, backing_size as libc::off_t);
            if ret != 0 {
                libc::close(fd);
                return Err(format!(
                    "ftruncate failed: {}",
                    std::io::Error::last_os_error()
                ));
            }

            // Map backing memory
            let backing_base = libc::mmap(
                ptr::null_mut(),
                backing_size,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_SHARED,
                fd,
                0,
            );
            if backing_base == libc::MAP_FAILED {
                libc::close(fd);
                return Err(format!(
                    "mmap backing failed: {}",
                    std::io::Error::last_os_error()
                ));
            }

            // Map virtual address space
            let virtual_map_base = libc::mmap(
                ptr::null_mut(),
                virtual_size,
                libc::PROT_NONE,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS | map_noreserve(),
                -1,
                0,
            );
            if virtual_map_base == libc::MAP_FAILED {
                libc::munmap(backing_base, backing_size);
                libc::close(fd);
                return Err(format!(
                    "mmap virtual failed: {}",
                    std::io::Error::last_os_error()
                ));
            }

            // Enable huge pages (skip when RUZU_NO_HUGEPAGE=1; diagnostic for
            // multi-thread mmap THP interactions).
            #[cfg(target_os = "linux")]
            if std::env::var_os("RUZU_NO_HUGEPAGE").is_none() {
                libc::madvise(virtual_map_base, virtual_size, libc::MADV_HUGEPAGE);
            }

            let free_manager = FreeRegionManager::new();
            free_manager.set_address_space(virtual_map_base as *mut u8, virtual_size);

            Ok(Self {
                backing_size,
                virtual_size,
                backing_base: backing_base as *mut u8,
                virtual_base: virtual_map_base as *mut u8,
                virtual_map_base: virtual_map_base as *mut u8,
                fd,
                free_manager,
            })
        }
    }

    fn map(
        &self,
        virtual_offset: usize,
        host_offset: usize,
        length: usize,
        perms: MemoryPermission,
    ) {
        let (mut vo, mut len) = (virtual_offset, length);
        self.adjust_map(&mut vo, &mut len);
        if len == 0 {
            return;
        }

        // Remove from free regions
        unsafe {
            self.free_manager
                .allocate_block(self.virtual_base.add(vo), len);
        }

        // Deduce protection flags
        let mut flags = libc::PROT_NONE;
        if perms.contains(MemoryPermission::READ) {
            flags |= libc::PROT_READ;
        }
        if perms.contains(MemoryPermission::WRITE) {
            flags |= libc::PROT_WRITE;
        }

        unsafe {
            let ret = libc::mmap(
                self.virtual_base.add(vo) as *mut libc::c_void,
                len,
                flags,
                libc::MAP_SHARED | libc::MAP_FIXED,
                self.fd,
                host_offset as libc::off_t,
            );
            assert!(ret != libc::MAP_FAILED, "mmap failed during Map");
            // RUZU_TRACE_HOST_MMAP=0xVADDR — log HostMemoryImpl::map calls
            // whose `vo` (= passed virtual_offset) covers the GUEST vaddr.
            // Used to verify fastmem arena mapping actually happens for
            // the mstate region.
            if let Ok(spec) = std::env::var("RUZU_TRACE_HOST_MMAP") {
                if let Ok(target_vaddr) =
                    u64::from_str_radix(spec.trim().trim_start_matches("0x"), 16)
                {
                    // `vo` is the offset within virtual_base; for our setup,
                    // it equals the guest vaddr (the JIT uses R13 + vaddr).
                    if (vo as u64) <= target_vaddr && target_vaddr < (vo as u64) + len as u64 {
                        let mmap_target_va = self.virtual_base.add(vo) as u64;
                        eprintln!(
                            "[HOST_MMAP] virtual_base={:p} vo=0x{:X} mmap_target_host_va=0x{:X} len=0x{:X} fd={} host_offset=0x{:X} flags=0x{:X} perms={:?} ret={:p}",
                            self.virtual_base, vo, mmap_target_va, len, self.fd, host_offset, flags, perms, ret
                        );
                    }
                }
            }
        }
    }

    fn unmap(&self, virtual_offset: usize, length: usize) {
        let (mut vo, mut len) = (virtual_offset, length);
        self.adjust_map(&mut vo, &mut len);
        if len == 0 {
            return;
        }

        // Merge with adjacent placeholder mappings
        let (merged_pointer, merged_size) =
            unsafe { self.free_manager.free_block(self.virtual_base.add(vo), len) };

        unsafe {
            let ret = libc::mmap(
                merged_pointer as *mut libc::c_void,
                merged_size,
                libc::PROT_NONE,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS | libc::MAP_FIXED,
                -1,
                0,
            );
            assert!(ret != libc::MAP_FAILED, "mmap failed during Unmap");
        }
    }

    fn protect(
        &self,
        virtual_offset: usize,
        length: usize,
        read: bool,
        write: bool,
        _execute: bool,
    ) {
        // RUZU_TRACE_HOST_MMAP=0xVADDR — also log protect calls covering it.
        if let Ok(spec) = std::env::var("RUZU_TRACE_HOST_MMAP") {
            if let Ok(target_vaddr) = u64::from_str_radix(spec.trim().trim_start_matches("0x"), 16)
            {
                if (virtual_offset as u64) <= target_vaddr
                    && target_vaddr < (virtual_offset as u64) + length as u64
                {
                    eprintln!(
                        "[HOST_PROTECT] vo=0x{:X} len=0x{:X} read={} write={}",
                        virtual_offset, length, read, write
                    );
                }
            }
        }
        let (mut vo, mut len) = (virtual_offset, length);
        self.adjust_map(&mut vo, &mut len);
        if len == 0 {
            return;
        }

        let mut flags = libc::PROT_NONE;
        if read {
            flags |= libc::PROT_READ;
        }
        if write {
            flags |= libc::PROT_WRITE;
        }

        unsafe {
            let ret = libc::mprotect(self.virtual_base.add(vo) as *mut libc::c_void, len, flags);
            assert!(ret == 0, "mprotect failed");
        }
    }

    fn clear_backing_region(&self, physical_offset: usize, length: usize) -> bool {
        #[cfg(target_os = "linux")]
        unsafe {
            let ret = libc::madvise(
                self.backing_base.add(physical_offset) as *mut libc::c_void,
                length,
                libc::MADV_REMOVE,
            );
            assert!(ret == 0, "madvise MADV_REMOVE failed");
        }
        #[cfg(target_os = "linux")]
        {
            true
        }
        #[cfg(target_os = "macos")]
        {
            let _ = physical_offset;
            let _ = length;
            false
        }
    }

    fn enable_direct_mapped_address(&mut self) {
        self.virtual_base = ptr::null_mut();
    }

    fn adjust_map(&self, virtual_offset: &mut usize, length: &mut usize) {
        if !self.virtual_base.is_null() {
            return;
        }

        let intended_start = *virtual_offset;
        let intended_end = intended_start + *length;
        let address_space_start = self.virtual_map_base as usize;
        let address_space_end = address_space_start + self.virtual_size;

        if address_space_start > intended_end || intended_start > address_space_end {
            *virtual_offset = 0;
            *length = 0;
        } else {
            *virtual_offset = std::cmp::max(intended_start, address_space_start);
            *length = std::cmp::min(intended_end, address_space_end) - *virtual_offset;
        }
    }
}

#[cfg(target_os = "linux")]
fn create_backing_fd() -> Result<i32, String> {
    unsafe {
        let name = b"HostMemory\0";
        let fd = libc::syscall(libc::SYS_memfd_create, name.as_ptr(), 0) as i32;
        if fd < 0 {
            Err(format!(
                "memfd_create failed: {}",
                std::io::Error::last_os_error()
            ))
        } else {
            Ok(fd)
        }
    }
}

#[cfg(target_os = "macos")]
fn create_backing_fd() -> Result<i32, String> {
    static NEXT_ID: AtomicU64 = AtomicU64::new(0);
    let id = NEXT_ID.fetch_add(1, Ordering::Relaxed);
    let name = CString::new(format!("/ruzu-host-memory-{}-{}", std::process::id(), id))
        .map_err(|e| e.to_string())?;
    unsafe {
        let fd = libc::shm_open(
            name.as_ptr(),
            libc::O_RDWR | libc::O_CREAT | libc::O_EXCL,
            0o600,
        );
        if fd < 0 {
            return Err(format!(
                "shm_open failed: {}",
                std::io::Error::last_os_error()
            ));
        }
        libc::shm_unlink(name.as_ptr());
        Ok(fd)
    }
}

#[cfg(target_os = "linux")]
fn map_noreserve() -> i32 {
    libc::MAP_NORESERVE
}

#[cfg(target_os = "macos")]
fn map_noreserve() -> i32 {
    0
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
impl Drop for HostMemoryImpl {
    fn drop(&mut self) {
        unsafe {
            if self.virtual_map_base != libc::MAP_FAILED as *mut u8
                && !self.virtual_map_base.is_null()
            {
                libc::munmap(
                    self.virtual_map_base as *mut libc::c_void,
                    self.virtual_size,
                );
            }
            if self.backing_base != libc::MAP_FAILED as *mut u8 && !self.backing_base.is_null() {
                libc::munmap(self.backing_base as *mut libc::c_void, self.backing_size);
            }
            if self.fd != -1 {
                libc::close(self.fd);
            }
        }
    }
}

/// A low level linear memory buffer, which supports multiple mappings.
/// Its purpose is to rebuild a given sparse memory layout, including mirrors.
pub struct HostMemory {
    backing_size: usize,
    virtual_size: usize,
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    imp: Option<HostMemoryImpl>,
    backing_base: *mut u8,
    virtual_base: *mut u8,
    virtual_base_offset: usize,
    /// Fallback if fastmem is not supported.
    /// Kept alive to ensure the backing memory is not freed.
    #[allow(dead_code)]
    fallback_buffer: Option<VirtualBuffer<u8>>,
}

impl HostMemory {
    pub fn new(backing_size: usize, virtual_size: usize) -> Self {
        let aligned_backing = align_up(backing_size as u64, PAGE_ALIGNMENT as u64) as usize;
        let aligned_virtual =
            align_up(virtual_size as u64, PAGE_ALIGNMENT as u64) as usize + HUGE_PAGE_SIZE;

        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            match HostMemoryImpl::new(aligned_backing, aligned_virtual) {
                Ok(imp) => {
                    let backing_base = imp.backing_base;
                    let mut virtual_base = imp.virtual_base;
                    let mut virtual_base_offset = 0;

                    if !virtual_base.is_null() {
                        // Ensure virtual base is aligned to HUGE_PAGE_SIZE
                        let aligned =
                            align_up(virtual_base as u64, HUGE_PAGE_SIZE as u64) as *mut u8;
                        virtual_base_offset = aligned as usize - virtual_base as usize;
                        virtual_base = aligned;
                    }

                    return Self {
                        backing_size,
                        virtual_size,
                        imp: Some(imp),
                        backing_base,
                        virtual_base,
                        virtual_base_offset,
                        fallback_buffer: None,
                    };
                }
                Err(e) => {
                    error!("Fastmem unavailable ({}), falling back to VirtualBuffer", e);
                }
            }
        }

        // Fallback path
        let mut fallback = VirtualBuffer::<u8>::with_count(backing_size);
        let backing_base = fallback.data_mut();
        Self {
            backing_size,
            virtual_size,
            #[cfg(any(target_os = "linux", target_os = "macos"))]
            imp: None,
            backing_base,
            virtual_base: ptr::null_mut(),
            virtual_base_offset: 0,
            fallback_buffer: Some(fallback),
        }
    }

    pub fn map(
        &self,
        virtual_offset: usize,
        host_offset: usize,
        length: usize,
        perms: MemoryPermission,
        _separate_heap: bool,
    ) {
        assert!(virtual_offset % PAGE_ALIGNMENT == 0);
        assert!(host_offset % PAGE_ALIGNMENT == 0);
        assert!(length % PAGE_ALIGNMENT == 0);
        assert!(virtual_offset + length <= self.virtual_size);
        assert!(
            host_offset + length <= self.backing_size,
            "host_memory::map: host_offset=0x{:X} + length=0x{:X} > backing_size=0x{:X} (virtual_offset=0x{:X}, virtual_size=0x{:X})",
            host_offset, length, self.backing_size, virtual_offset, self.virtual_size
        );

        if length == 0 || self.virtual_base.is_null() {
            return;
        }

        #[cfg(any(target_os = "linux", target_os = "macos"))]
        if let Some(ref imp) = self.imp {
            imp.map(
                virtual_offset + self.virtual_base_offset,
                host_offset,
                length,
                perms,
            );
        }
    }

    pub fn unmap(&self, virtual_offset: usize, length: usize, _separate_heap: bool) {
        assert!(virtual_offset % PAGE_ALIGNMENT == 0);
        assert!(length % PAGE_ALIGNMENT == 0);
        assert!(virtual_offset + length <= self.virtual_size);

        if length == 0 || self.virtual_base.is_null() {
            return;
        }

        #[cfg(any(target_os = "linux", target_os = "macos"))]
        if let Some(ref imp) = self.imp {
            imp.unmap(virtual_offset + self.virtual_base_offset, length);
        }
    }

    pub fn protect(&self, virtual_offset: usize, length: usize, perm: MemoryPermission) {
        assert!(virtual_offset % PAGE_ALIGNMENT == 0);
        assert!(length % PAGE_ALIGNMENT == 0);
        assert!(virtual_offset + length <= self.virtual_size);

        if length == 0 || self.virtual_base.is_null() {
            return;
        }

        let read = perm.contains(MemoryPermission::READ);
        let write = perm.contains(MemoryPermission::WRITE);
        let execute = perm.contains(MemoryPermission::EXECUTE);

        #[cfg(any(target_os = "linux", target_os = "macos"))]
        if let Some(ref imp) = self.imp {
            imp.protect(
                virtual_offset + self.virtual_base_offset,
                length,
                read,
                write,
                execute,
            );
        }
    }

    pub fn clear_backing_region(&self, physical_offset: usize, length: usize, fill_value: u32) {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            if fill_value == 0 {
                if let Some(ref imp) = self.imp {
                    if imp.clear_backing_region(physical_offset, length) {
                        return;
                    }
                }
            }
        }

        // Fallback: memset
        unsafe {
            ptr::write_bytes(
                self.backing_base.add(physical_offset),
                fill_value as u8,
                length,
            );
        }
    }

    pub fn enable_direct_mapped_address(&mut self) {
        #[cfg(target_os = "linux")]
        if let Some(ref mut imp) = self.imp {
            imp.enable_direct_mapped_address();
            self.virtual_size += self.virtual_base as usize;
        }
    }

    pub fn backing_base_pointer(&self) -> *mut u8 {
        self.backing_base
    }

    pub fn backing_size(&self) -> usize {
        self.backing_size
    }

    pub fn virtual_base_pointer(&self) -> *mut u8 {
        self.virtual_base
    }

    pub fn is_in_virtual_range(&self, address: *const u8) -> bool {
        let addr = address as usize;
        let base = self.virtual_base as usize;
        addr >= base && addr < base + self.virtual_size
    }
}

// Safety: HostMemory manages raw pointers to mmap'd memory regions.
// The memory is allocated and freed in a controlled manner.
unsafe impl Send for HostMemory {}
unsafe impl Sync for HostMemory {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_host_memory() {
        // Create a small host memory (backing + virtual)
        let hm = HostMemory::new(0x100000, 0x200000);
        assert!(!hm.backing_base_pointer().is_null());
    }
}
