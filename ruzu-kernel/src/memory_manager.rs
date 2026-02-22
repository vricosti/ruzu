// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Guest virtual memory manager for the Nintendo Switch HLE kernel.
//!
//! Implements a 39-bit guest address space (512 GiB) backed by a 4 GiB sparse
//! anonymous mmap. A flat page table maps guest virtual pages to offsets in the
//! backing store. Region metadata is kept in a `BTreeMap` to support the
//! `QueryMemory` SVC.

use std::collections::BTreeMap;

use bitflags::bitflags;
use memmap2::MmapMut;
use thiserror::Error;

use ruzu_common::{
    VAddr, ADDRESS_SPACE_PAGES, ADDRESS_SPACE_SIZE, PAGE_MASK, PAGE_SHIFT, PAGE_SIZE,
    is_page_aligned,
};

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Size of the sparse anonymous backing store (4 GiB).
const BACKING_SIZE: usize = 4 * 1024 * 1024 * 1024;

// ---------------------------------------------------------------------------
// Error types
// ---------------------------------------------------------------------------

/// Errors returned by [`MemoryManager`] operations.
#[derive(Debug, Error)]
pub enum MemoryError {
    #[error("address 0x{0:016X} is out of the 39-bit address space")]
    OutOfRange(VAddr),

    #[error("address 0x{0:016X} is not page-aligned")]
    MisalignedAddress(VAddr),

    #[error("size 0x{0:X} is not page-aligned")]
    MisalignedSize(u64),

    #[error("size must be non-zero")]
    ZeroSize,

    #[error("region [0x{0:016X}..0x{1:016X}) overlaps an existing mapping")]
    RegionOverlap(VAddr, VAddr),

    #[error("address 0x{0:016X} is not mapped")]
    NotMapped(VAddr),

    #[error("region [0x{0:016X}..0x{1:016X}) is not fully mapped")]
    NotFullyMapped(VAddr, VAddr),

    #[error("backing store exhausted (requested {requested:#X}, available {available:#X})")]
    BackingExhausted { requested: usize, available: usize },

    #[error("permission denied at 0x{addr:016X}: required {required}, have {actual}")]
    PermissionDenied {
        addr: VAddr,
        required: MemoryPermission,
        actual: MemoryPermission,
    },

    #[error("failed to create backing mmap: {0}")]
    MmapFailed(#[from] std::io::Error),
}

/// Result alias for memory operations.
pub type MemoryResult<T> = Result<T, MemoryError>;

// ---------------------------------------------------------------------------
// Permission flags
// ---------------------------------------------------------------------------

bitflags! {
    /// Memory permission flags matching the Switch kernel.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct MemoryPermission: u32 {
        const READ    = 1;
        const WRITE   = 2;
        const EXECUTE = 4;

        const READ_WRITE          = Self::READ.bits() | Self::WRITE.bits();
        const READ_EXECUTE        = Self::READ.bits() | Self::EXECUTE.bits();
        const READ_WRITE_EXECUTE  = Self::READ.bits() | Self::WRITE.bits() | Self::EXECUTE.bits();
        const NONE                = 0;
    }
}

impl std::fmt::Display for MemoryPermission {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let r = if self.contains(Self::READ) { 'R' } else { '-' };
        let w = if self.contains(Self::WRITE) { 'W' } else { '-' };
        let x = if self.contains(Self::EXECUTE) { 'X' } else { '-' };
        write!(f, "{r}{w}{x}")
    }
}

// ---------------------------------------------------------------------------
// Memory state
// ---------------------------------------------------------------------------

/// Memory region state, matching the Switch kernel's `MemoryState` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum MemoryState {
    Unmapped = 0x00,
    Code = 0x01,
    CodeData = 0x02,
    Normal = 0x03,
    Heap = 0x04,
    SharedMemory = 0x05,
    Stack = 0x06,
    Ipc = 0x07,
    ThreadLocal = 0x08,
    TransferMemory = 0x09,
}

impl std::fmt::Display for MemoryState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unmapped => write!(f, "Unmapped"),
            Self::Code => write!(f, "Code"),
            Self::CodeData => write!(f, "CodeData"),
            Self::Normal => write!(f, "Normal"),
            Self::Heap => write!(f, "Heap"),
            Self::SharedMemory => write!(f, "SharedMemory"),
            Self::Stack => write!(f, "Stack"),
            Self::Ipc => write!(f, "Ipc"),
            Self::ThreadLocal => write!(f, "ThreadLocal"),
            Self::TransferMemory => write!(f, "TransferMemory"),
        }
    }
}

// ---------------------------------------------------------------------------
// Page table entry
// ---------------------------------------------------------------------------

/// A single entry in the flat page table.
///
/// Tracks the byte offset into the backing mmap (or `None` for unmapped pages)
/// and the effective permissions for the page.
#[derive(Debug, Clone, Copy)]
pub struct PageEntry {
    /// Byte offset into the backing `MmapMut`, or `None` if the page is unmapped.
    pub offset: Option<usize>,
    /// Effective permissions for this page.
    pub permission: MemoryPermission,
}

impl Default for PageEntry {
    fn default() -> Self {
        Self {
            offset: None,
            permission: MemoryPermission::NONE,
        }
    }
}

impl PageEntry {
    /// Returns `true` if this page is mapped.
    #[inline]
    pub fn is_mapped(&self) -> bool {
        self.offset.is_some()
    }
}

// ---------------------------------------------------------------------------
// Memory region (for QueryMemory / region tracking)
// ---------------------------------------------------------------------------

/// Describes a contiguous region of guest virtual memory with uniform
/// permissions and state.
#[derive(Debug, Clone)]
pub struct MemoryRegion {
    /// Base virtual address (page-aligned).
    pub base_addr: VAddr,
    /// Size in bytes (page-aligned).
    pub size: u64,
    /// Access permissions.
    pub permission: MemoryPermission,
    /// Memory state.
    pub state: MemoryState,
    /// Optional human-readable name (for debugging).
    pub name: Option<String>,
}

// ---------------------------------------------------------------------------
// MemoryInfo (SVC QueryMemory return value)
// ---------------------------------------------------------------------------

/// Information about a memory region, returned by `query_memory`.
///
/// Mirrors the Switch `MemoryInfo` structure used by SVC QueryMemory.
#[derive(Debug, Clone)]
pub struct MemoryInfo {
    /// Base address of the region.
    pub base_addr: VAddr,
    /// Size of the region in bytes.
    pub size: u64,
    /// Memory state of the region.
    pub state: MemoryState,
    /// Memory permissions of the region.
    pub permission: MemoryPermission,
}

// ---------------------------------------------------------------------------
// MemoryManager
// ---------------------------------------------------------------------------

/// Guest virtual memory manager.
///
/// Manages the full 39-bit (512 GiB) guest address space via a flat page table
/// and allocates guest-visible pages out of a 4 GiB sparse anonymous mmap.
pub struct MemoryManager {
    /// Sparse 4 GiB anonymous mmap used as the backing store for all guest
    /// memory. Pages are allocated linearly from offset 0.
    backing: MmapMut,

    /// Flat page table indexed by `VAddr >> PAGE_SHIFT`. Each entry records the
    /// offset into `backing` (or `None` if the page is not mapped).
    page_table: Vec<PageEntry>,

    /// Region metadata keyed by base virtual address. Used to answer
    /// `QueryMemory` SVCs and to detect overlapping mappings.
    regions: BTreeMap<VAddr, MemoryRegion>,

    /// Next free byte offset in the backing store. Monotonically increases as
    /// new regions are mapped; freed pages are *not* reclaimed (the backing
    /// store is sparse, so the OS will not commit pages that are never touched).
    next_backing_offset: usize,
}

impl MemoryManager {
    /// Create a new memory manager with a 4 GiB sparse anonymous backing
    /// store and a page table covering the 39-bit address space.
    pub fn new() -> MemoryResult<Self> {
        Self::with_capacity(BACKING_SIZE, ADDRESS_SPACE_PAGES)
    }

    /// Create a memory manager with a custom backing size and page table size.
    /// Useful for tests that don't need the full 4 GiB + 128M-entry table.
    pub fn with_capacity(backing_size: usize, page_count: usize) -> MemoryResult<Self> {
        let backing = MmapMut::map_anon(backing_size)?;
        let page_table = vec![PageEntry::default(); page_count];

        Ok(Self {
            backing,
            page_table,
            regions: BTreeMap::new(),
            next_backing_offset: 0,
        })
    }

    // -- Mapping operations -------------------------------------------------

    /// Map a region of guest virtual memory.
    ///
    /// Allocates backing pages from the sparse mmap and populates the page
    /// table. The region is recorded in the `regions` map for `QueryMemory`.
    pub fn map(
        &mut self,
        vaddr: VAddr,
        size: u64,
        permission: MemoryPermission,
        state: MemoryState,
    ) -> MemoryResult<()> {
        self.map_named(vaddr, size, permission, state, None)
    }

    /// Map a region with an optional debug name.
    pub fn map_named(
        &mut self,
        vaddr: VAddr,
        size: u64,
        permission: MemoryPermission,
        state: MemoryState,
        name: Option<String>,
    ) -> MemoryResult<()> {
        self.validate_range(vaddr, size)?;

        let size_usize = size as usize;
        let end = vaddr + size;

        // Check for overlapping regions.
        if self.overlaps(vaddr, end) {
            return Err(MemoryError::RegionOverlap(vaddr, end));
        }

        // Ensure there is enough backing space.
        let available = self.backing.len() - self.next_backing_offset;
        if size_usize > available {
            return Err(MemoryError::BackingExhausted {
                requested: size_usize,
                available,
            });
        }

        // Allocate backing and fill the page table.
        let backing_base = self.next_backing_offset;
        let num_pages = size_usize / PAGE_SIZE;

        for i in 0..num_pages {
            let page_idx = ((vaddr >> PAGE_SHIFT) as usize) + i;
            self.page_table[page_idx] = PageEntry {
                offset: Some(backing_base + i * PAGE_SIZE),
                permission,
            };
        }

        self.next_backing_offset += size_usize;

        // Record the region.
        self.regions.insert(
            vaddr,
            MemoryRegion {
                base_addr: vaddr,
                size,
                permission,
                state,
                name,
            },
        );

        log::debug!(
            "MemoryManager::map [{:#010X}..{:#010X}) perm={} state={} backing=[{:#X}..{:#X})",
            vaddr,
            end,
            permission,
            state,
            backing_base,
            backing_base + size_usize,
        );

        Ok(())
    }

    /// Unmap a previously mapped region.
    ///
    /// The region must exactly match a previously mapped region (same base
    /// address and size). Backing memory is not reclaimed.
    pub fn unmap(&mut self, vaddr: VAddr, size: u64) -> MemoryResult<()> {
        self.validate_range(vaddr, size)?;

        // Verify that the region exists and covers the requested range.
        let region = self
            .regions
            .get(&vaddr)
            .ok_or(MemoryError::NotMapped(vaddr))?;

        if region.size != size {
            return Err(MemoryError::NotFullyMapped(vaddr, vaddr + size));
        }

        // Clear page table entries.
        let num_pages = (size as usize) / PAGE_SIZE;
        let base_page = (vaddr >> PAGE_SHIFT) as usize;

        for i in 0..num_pages {
            self.page_table[base_page + i] = PageEntry::default();
        }

        self.regions.remove(&vaddr);

        log::debug!(
            "MemoryManager::unmap [{:#010X}..{:#010X})",
            vaddr,
            vaddr + size,
        );

        Ok(())
    }

    // -- Read operations ----------------------------------------------------

    /// Read a `u8` from guest virtual memory.
    #[inline]
    pub fn read_u8(&self, vaddr: VAddr) -> MemoryResult<u8> {
        let off = self.resolve_read(vaddr, 1)?;
        Ok(self.backing[off])
    }

    /// Read a little-endian `u16` from guest virtual memory.
    #[inline]
    pub fn read_u16(&self, vaddr: VAddr) -> MemoryResult<u16> {
        let off = self.resolve_read(vaddr, 2)?;
        let bytes: [u8; 2] = self.backing[off..off + 2]
            .try_into()
            .expect("slice length mismatch");
        Ok(u16::from_le_bytes(bytes))
    }

    /// Read a little-endian `u32` from guest virtual memory.
    #[inline]
    pub fn read_u32(&self, vaddr: VAddr) -> MemoryResult<u32> {
        let off = self.resolve_read(vaddr, 4)?;
        let bytes: [u8; 4] = self.backing[off..off + 4]
            .try_into()
            .expect("slice length mismatch");
        Ok(u32::from_le_bytes(bytes))
    }

    /// Read a little-endian `u64` from guest virtual memory.
    #[inline]
    pub fn read_u64(&self, vaddr: VAddr) -> MemoryResult<u64> {
        let off = self.resolve_read(vaddr, 8)?;
        let bytes: [u8; 8] = self.backing[off..off + 8]
            .try_into()
            .expect("slice length mismatch");
        Ok(u64::from_le_bytes(bytes))
    }

    /// Read `size` bytes starting at `vaddr`.
    ///
    /// The read may span multiple pages. Each page must be mapped with at
    /// least `READ` permission.
    pub fn read_bytes(&self, vaddr: VAddr, size: usize) -> MemoryResult<Vec<u8>> {
        if size == 0 {
            return Ok(Vec::new());
        }

        let mut result = Vec::with_capacity(size);
        let mut remaining = size;
        let mut current_addr = vaddr;

        while remaining > 0 {
            let page_offset = (current_addr & PAGE_MASK) as usize;
            let chunk = remaining.min(PAGE_SIZE - page_offset);

            let off = self.resolve_read(current_addr, chunk)?;
            result.extend_from_slice(&self.backing[off..off + chunk]);

            current_addr += chunk as u64;
            remaining -= chunk;
        }

        Ok(result)
    }

    // -- Write operations ---------------------------------------------------

    /// Write a `u8` to guest virtual memory.
    #[inline]
    pub fn write_u8(&mut self, vaddr: VAddr, value: u8) -> MemoryResult<()> {
        let off = self.resolve_write(vaddr, 1)?;
        self.backing[off] = value;
        Ok(())
    }

    /// Write a little-endian `u16` to guest virtual memory.
    #[inline]
    pub fn write_u16(&mut self, vaddr: VAddr, value: u16) -> MemoryResult<()> {
        let off = self.resolve_write(vaddr, 2)?;
        self.backing[off..off + 2].copy_from_slice(&value.to_le_bytes());
        Ok(())
    }

    /// Write a little-endian `u32` to guest virtual memory.
    #[inline]
    pub fn write_u32(&mut self, vaddr: VAddr, value: u32) -> MemoryResult<()> {
        let off = self.resolve_write(vaddr, 4)?;
        self.backing[off..off + 4].copy_from_slice(&value.to_le_bytes());
        Ok(())
    }

    /// Write a little-endian `u64` to guest virtual memory.
    #[inline]
    pub fn write_u64(&mut self, vaddr: VAddr, value: u64) -> MemoryResult<()> {
        let off = self.resolve_write(vaddr, 8)?;
        self.backing[off..off + 8].copy_from_slice(&value.to_le_bytes());
        Ok(())
    }

    /// Write a byte slice starting at `vaddr`.
    ///
    /// The write may span multiple pages. Each page must be mapped with at
    /// least `READ | WRITE` permission.
    pub fn write_bytes(&mut self, vaddr: VAddr, data: &[u8]) -> MemoryResult<()> {
        if data.is_empty() {
            return Ok(());
        }

        let mut remaining = data.len();
        let mut src_offset = 0;
        let mut current_addr = vaddr;

        while remaining > 0 {
            let page_offset = (current_addr & PAGE_MASK) as usize;
            let chunk = remaining.min(PAGE_SIZE - page_offset);

            let off = self.resolve_write(current_addr, chunk)?;
            self.backing[off..off + chunk]
                .copy_from_slice(&data[src_offset..src_offset + chunk]);

            current_addr += chunk as u64;
            src_offset += chunk;
            remaining -= chunk;
        }

        Ok(())
    }

    // -- Query operations ---------------------------------------------------

    /// Query information about the memory region containing `vaddr`.
    ///
    /// This corresponds to the Switch SVC `QueryMemory`. Returns a
    /// [`MemoryInfo`] describing the region that contains (or immediately
    /// follows) the given address.
    pub fn query_memory(&self, vaddr: VAddr) -> MemoryInfo {
        // Find the region whose base address is <= vaddr.
        if let Some((&base, region)) = self.regions.range(..=vaddr).next_back() {
            let region_end = base + region.size;
            if vaddr < region_end {
                // vaddr falls within this mapped region.
                return MemoryInfo {
                    base_addr: region.base_addr,
                    size: region.size,
                    state: region.state,
                    permission: region.permission,
                };
            }
        }

        // vaddr is in an unmapped gap. Compute the extent of the gap.
        let gap_start = self.gap_start_before(vaddr);
        let gap_end = self.gap_end_after(vaddr);

        MemoryInfo {
            base_addr: gap_start,
            size: gap_end - gap_start,
            state: MemoryState::Unmapped,
            permission: MemoryPermission::NONE,
        }
    }

    // -- Permission management ----------------------------------------------

    /// Update the permissions for an existing mapped region.
    pub fn set_permissions(
        &mut self,
        vaddr: VAddr,
        size: u64,
        new_perm: MemoryPermission,
    ) -> MemoryResult<()> {
        self.validate_range(vaddr, size)?;

        let region = self
            .regions
            .get_mut(&vaddr)
            .ok_or(MemoryError::NotMapped(vaddr))?;

        if region.size != size {
            return Err(MemoryError::NotFullyMapped(vaddr, vaddr + size));
        }

        region.permission = new_perm;

        // Update page table entries.
        let num_pages = (size as usize) / PAGE_SIZE;
        let base_page = (vaddr >> PAGE_SHIFT) as usize;

        for i in 0..num_pages {
            self.page_table[base_page + i].permission = new_perm;
        }

        Ok(())
    }

    // -- Accessors ----------------------------------------------------------

    /// Return a reference to the raw backing store.
    ///
    /// This is intended for subsystems (GPU, DMA) that need direct access to
    /// guest memory without going through the per-page permission checks.
    pub fn backing(&self) -> &[u8] {
        &self.backing
    }

    /// Return a mutable reference to the raw backing store.
    pub fn backing_mut(&mut self) -> &mut [u8] {
        &mut self.backing
    }

    /// Return the byte offset in the backing store for a guest virtual
    /// address, or `None` if the page is unmapped.
    pub fn backing_offset(&self, vaddr: VAddr) -> Option<usize> {
        let page_idx = (vaddr >> PAGE_SHIFT) as usize;
        if page_idx >= self.page_table.len() {
            return None;
        }
        let entry = &self.page_table[page_idx];
        entry.offset.map(|base| base + (vaddr & PAGE_MASK) as usize)
    }

    /// Return an iterator over all mapped regions.
    pub fn regions(&self) -> impl Iterator<Item = (&VAddr, &MemoryRegion)> {
        self.regions.iter()
    }

    /// Total bytes allocated from the backing store so far.
    pub fn backing_used(&self) -> usize {
        self.next_backing_offset
    }

    /// Total backing store capacity.
    pub fn backing_capacity(&self) -> usize {
        self.backing.len()
    }

    // -- Internal helpers ---------------------------------------------------

    /// Validate that `(vaddr, size)` is a legal range within the address space.
    fn validate_range(&self, vaddr: VAddr, size: u64) -> MemoryResult<()> {
        if size == 0 {
            return Err(MemoryError::ZeroSize);
        }
        if !is_page_aligned(vaddr) {
            return Err(MemoryError::MisalignedAddress(vaddr));
        }
        if !is_page_aligned(size) {
            return Err(MemoryError::MisalignedSize(size));
        }
        let end = vaddr.checked_add(size).ok_or(MemoryError::OutOfRange(vaddr))?;
        if end > ADDRESS_SPACE_SIZE {
            return Err(MemoryError::OutOfRange(vaddr));
        }
        Ok(())
    }

    /// Check whether `[start, end)` overlaps any existing region.
    fn overlaps(&self, start: VAddr, end: VAddr) -> bool {
        // Check the region whose base is immediately at or before `start`.
        if let Some((&base, region)) = self.regions.range(..end).next_back() {
            if base + region.size > start {
                return true;
            }
        }
        // Also check if any region starts strictly inside [start, end).
        if let Some((&base, _)) = self.regions.range(start..end).next() {
            if base < end {
                return true;
            }
        }
        false
    }

    /// Resolve a guest virtual address for reading. Returns the byte offset
    /// into the backing store. Checks that all pages touched by
    /// `[vaddr, vaddr + len)` are mapped with `READ` permission.
    fn resolve_read(&self, vaddr: VAddr, len: usize) -> MemoryResult<usize> {
        if vaddr >= ADDRESS_SPACE_SIZE {
            return Err(MemoryError::OutOfRange(vaddr));
        }

        let page_idx = (vaddr >> PAGE_SHIFT) as usize;
        let entry = &self.page_table[page_idx];

        let offset = entry.offset.ok_or(MemoryError::NotMapped(vaddr))?;

        if !entry.permission.contains(MemoryPermission::READ) {
            return Err(MemoryError::PermissionDenied {
                addr: vaddr,
                required: MemoryPermission::READ,
                actual: entry.permission,
            });
        }

        // For accesses that stay within one page, we can return immediately.
        let page_offset = (vaddr & PAGE_MASK) as usize;
        if page_offset + len <= PAGE_SIZE {
            return Ok(offset + page_offset);
        }

        // Multi-page access: verify all subsequent pages are readable.
        // We still return the offset of the first byte -- but only if the
        // backing is contiguous. For the general case, callers should use
        // `read_bytes` which iterates page by page.
        let end_addr = vaddr + len as u64 - 1;
        let end_page = (end_addr >> PAGE_SHIFT) as usize;

        for pidx in (page_idx + 1)..=end_page {
            let e = &self.page_table[pidx];
            if e.offset.is_none() {
                let unmapped_addr = (pidx as u64) << PAGE_SHIFT;
                return Err(MemoryError::NotMapped(unmapped_addr));
            }
            if !e.permission.contains(MemoryPermission::READ) {
                return Err(MemoryError::PermissionDenied {
                    addr: (pidx as u64) << PAGE_SHIFT,
                    required: MemoryPermission::READ,
                    actual: e.permission,
                });
            }
        }

        Ok(offset + page_offset)
    }

    /// Resolve a guest virtual address for writing. Returns the byte offset
    /// into the backing store. Checks that all pages touched by
    /// `[vaddr, vaddr + len)` are mapped with `READ | WRITE` permission.
    fn resolve_write(&self, vaddr: VAddr, len: usize) -> MemoryResult<usize> {
        if vaddr >= ADDRESS_SPACE_SIZE {
            return Err(MemoryError::OutOfRange(vaddr));
        }

        let page_idx = (vaddr >> PAGE_SHIFT) as usize;
        let entry = &self.page_table[page_idx];

        let offset = entry.offset.ok_or(MemoryError::NotMapped(vaddr))?;

        let required = MemoryPermission::READ | MemoryPermission::WRITE;
        if !entry.permission.contains(required) {
            return Err(MemoryError::PermissionDenied {
                addr: vaddr,
                required,
                actual: entry.permission,
            });
        }

        let page_offset = (vaddr & PAGE_MASK) as usize;
        if page_offset + len <= PAGE_SIZE {
            return Ok(offset + page_offset);
        }

        let end_addr = vaddr + len as u64 - 1;
        let end_page = (end_addr >> PAGE_SHIFT) as usize;

        for pidx in (page_idx + 1)..=end_page {
            let e = &self.page_table[pidx];
            if e.offset.is_none() {
                let unmapped_addr = (pidx as u64) << PAGE_SHIFT;
                return Err(MemoryError::NotMapped(unmapped_addr));
            }
            if !e.permission.contains(required) {
                return Err(MemoryError::PermissionDenied {
                    addr: (pidx as u64) << PAGE_SHIFT,
                    required,
                    actual: e.permission,
                });
            }
        }

        Ok(offset + page_offset)
    }

    /// Find the start of the unmapped gap that contains or precedes `vaddr`.
    fn gap_start_before(&self, vaddr: VAddr) -> VAddr {
        if let Some((&base, region)) = self.regions.range(..=vaddr).next_back() {
            let region_end = base + region.size;
            if vaddr >= region_end {
                return region_end;
            }
        }
        0
    }

    /// Find the end of the unmapped gap that contains or follows `vaddr`.
    fn gap_end_after(&self, vaddr: VAddr) -> VAddr {
        if let Some((&base, _)) = self.regions.range((vaddr + 1)..).next() {
            return base;
        }
        ADDRESS_SPACE_SIZE
    }
}

// ---------------------------------------------------------------------------
// MemoryAccess trait implementation (bridge to ruzu-cpu interpreter)
// ---------------------------------------------------------------------------

impl ruzu_cpu::memory::MemoryAccess for MemoryManager {
    fn read_u8(&self, addr: u64) -> Result<u8, ruzu_cpu::memory::MemoryFault> {
        MemoryManager::read_u8(self, addr).map_err(|_| ruzu_cpu::memory::MemoryFault::Unmapped(addr))
    }

    fn read_u16(&self, addr: u64) -> Result<u16, ruzu_cpu::memory::MemoryFault> {
        MemoryManager::read_u16(self, addr).map_err(|_| ruzu_cpu::memory::MemoryFault::Unmapped(addr))
    }

    fn read_u32(&self, addr: u64) -> Result<u32, ruzu_cpu::memory::MemoryFault> {
        MemoryManager::read_u32(self, addr).map_err(|_| ruzu_cpu::memory::MemoryFault::Unmapped(addr))
    }

    fn read_u64(&self, addr: u64) -> Result<u64, ruzu_cpu::memory::MemoryFault> {
        MemoryManager::read_u64(self, addr).map_err(|_| ruzu_cpu::memory::MemoryFault::Unmapped(addr))
    }

    fn read_u128(&self, addr: u64) -> Result<u128, ruzu_cpu::memory::MemoryFault> {
        // Read as two u64s (little-endian).
        let lo = MemoryManager::read_u64(self, addr)
            .map_err(|_| ruzu_cpu::memory::MemoryFault::Unmapped(addr))?;
        let hi = MemoryManager::read_u64(self, addr + 8)
            .map_err(|_| ruzu_cpu::memory::MemoryFault::Unmapped(addr + 8))?;
        Ok((lo as u128) | ((hi as u128) << 64))
    }

    fn write_u8(&mut self, addr: u64, val: u8) -> Result<(), ruzu_cpu::memory::MemoryFault> {
        MemoryManager::write_u8(self, addr, val)
            .map_err(|_| ruzu_cpu::memory::MemoryFault::Unmapped(addr))
    }

    fn write_u16(&mut self, addr: u64, val: u16) -> Result<(), ruzu_cpu::memory::MemoryFault> {
        MemoryManager::write_u16(self, addr, val)
            .map_err(|_| ruzu_cpu::memory::MemoryFault::Unmapped(addr))
    }

    fn write_u32(&mut self, addr: u64, val: u32) -> Result<(), ruzu_cpu::memory::MemoryFault> {
        MemoryManager::write_u32(self, addr, val)
            .map_err(|_| ruzu_cpu::memory::MemoryFault::Unmapped(addr))
    }

    fn write_u64(&mut self, addr: u64, val: u64) -> Result<(), ruzu_cpu::memory::MemoryFault> {
        MemoryManager::write_u64(self, addr, val)
            .map_err(|_| ruzu_cpu::memory::MemoryFault::Unmapped(addr))
    }

    fn write_u128(&mut self, addr: u64, val: u128) -> Result<(), ruzu_cpu::memory::MemoryFault> {
        // Write as two u64s (little-endian).
        MemoryManager::write_u64(self, addr, val as u64)
            .map_err(|_| ruzu_cpu::memory::MemoryFault::Unmapped(addr))?;
        MemoryManager::write_u64(self, addr + 8, (val >> 64) as u64)
            .map_err(|_| ruzu_cpu::memory::MemoryFault::Unmapped(addr + 8))
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use ruzu_common::PAGE_SIZE_U64;

    /// Test backing size: 16 MiB (enough for tests, small for parallel execution).
    const TEST_BACKING_SIZE: usize = 16 * 1024 * 1024;
    /// Test page count: covers addresses 0..64 MiB (16384 pages).
    const TEST_PAGE_COUNT: usize = 16384;

    /// Helper: create a memory manager for testing with a small backing.
    fn make_mm() -> MemoryManager {
        MemoryManager::with_capacity(TEST_BACKING_SIZE, TEST_PAGE_COUNT)
            .expect("failed to create MemoryManager")
    }

    // -- Map / Unmap --------------------------------------------------------

    #[test]
    fn test_map_basic() {
        let mut mm = make_mm();
        let base: VAddr = 0x8_0000;
        let size: u64 = PAGE_SIZE_U64 * 4;
        mm.map(base, size, MemoryPermission::READ_WRITE, MemoryState::Normal)
            .expect("map should succeed");

        // Verify all four pages are mapped.
        for i in 0..4 {
            let addr = base + (i as u64) * PAGE_SIZE_U64;
            let page_idx = (addr >> PAGE_SHIFT) as usize;
            assert!(mm.page_table[page_idx].is_mapped());
            assert_eq!(mm.page_table[page_idx].permission, MemoryPermission::READ_WRITE);
        }
    }

    #[test]
    fn test_map_overlap_rejected() {
        let mut mm = make_mm();
        let base: VAddr = 0x10_0000;
        let size: u64 = PAGE_SIZE_U64 * 4;
        mm.map(base, size, MemoryPermission::READ_WRITE, MemoryState::Normal)
            .unwrap();

        // Overlapping at the end.
        let result = mm.map(
            base + PAGE_SIZE_U64 * 2,
            PAGE_SIZE_U64 * 4,
            MemoryPermission::READ_WRITE,
            MemoryState::Normal,
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_unmap() {
        let mut mm = make_mm();
        let base: VAddr = 0x20_0000;
        let size: u64 = PAGE_SIZE_U64 * 2;
        mm.map(base, size, MemoryPermission::READ_WRITE, MemoryState::Normal)
            .unwrap();
        mm.unmap(base, size).expect("unmap should succeed");

        // Pages should no longer be mapped.
        for i in 0..2 {
            let page_idx = ((base >> PAGE_SHIFT) as usize) + i;
            assert!(!mm.page_table[page_idx].is_mapped());
        }
    }

    #[test]
    fn test_unmap_not_mapped() {
        let mut mm = make_mm();
        let result = mm.unmap(0x30_0000, PAGE_SIZE_U64);
        assert!(result.is_err());
    }

    // -- Alignment & validation ---------------------------------------------

    #[test]
    fn test_misaligned_address() {
        let mut mm = make_mm();
        let result = mm.map(0x1001, PAGE_SIZE_U64, MemoryPermission::READ, MemoryState::Code);
        assert!(matches!(result, Err(MemoryError::MisalignedAddress(_))));
    }

    #[test]
    fn test_misaligned_size() {
        let mut mm = make_mm();
        let result = mm.map(0x1000, 0x1001, MemoryPermission::READ, MemoryState::Code);
        assert!(matches!(result, Err(MemoryError::MisalignedSize(_))));
    }

    #[test]
    fn test_zero_size() {
        let mut mm = make_mm();
        let result = mm.map(0x1000, 0, MemoryPermission::READ, MemoryState::Code);
        assert!(matches!(result, Err(MemoryError::ZeroSize)));
    }

    #[test]
    fn test_out_of_range() {
        let mut mm = make_mm();
        // Beyond 39-bit address space.
        let result = mm.map(
            ADDRESS_SPACE_SIZE,
            PAGE_SIZE_U64,
            MemoryPermission::READ,
            MemoryState::Code,
        );
        assert!(matches!(result, Err(MemoryError::OutOfRange(_))));
    }

    // -- Read / Write scalar ------------------------------------------------

    #[test]
    fn test_read_write_u8() {
        let mut mm = make_mm();
        let base: VAddr = 0x4_0000;
        mm.map(base, PAGE_SIZE_U64, MemoryPermission::READ_WRITE, MemoryState::Normal)
            .unwrap();

        mm.write_u8(base, 0xAB).unwrap();
        assert_eq!(mm.read_u8(base).unwrap(), 0xAB);
    }

    #[test]
    fn test_read_write_u16() {
        let mut mm = make_mm();
        let base: VAddr = 0x5_0000;
        mm.map(base, PAGE_SIZE_U64, MemoryPermission::READ_WRITE, MemoryState::Normal)
            .unwrap();

        mm.write_u16(base, 0xBEEF).unwrap();
        assert_eq!(mm.read_u16(base).unwrap(), 0xBEEF);
    }

    #[test]
    fn test_read_write_u32() {
        let mut mm = make_mm();
        let base: VAddr = 0x6_0000;
        mm.map(base, PAGE_SIZE_U64, MemoryPermission::READ_WRITE, MemoryState::Normal)
            .unwrap();

        mm.write_u32(base + 4, 0xDEAD_BEEF).unwrap();
        assert_eq!(mm.read_u32(base + 4).unwrap(), 0xDEAD_BEEF);
    }

    #[test]
    fn test_read_write_u64() {
        let mut mm = make_mm();
        let base: VAddr = 0x7_0000;
        mm.map(base, PAGE_SIZE_U64, MemoryPermission::READ_WRITE, MemoryState::Normal)
            .unwrap();

        let val: u64 = 0x0123_4567_89AB_CDEF;
        mm.write_u64(base, val).unwrap();
        assert_eq!(mm.read_u64(base).unwrap(), val);
    }

    // -- Read / Write bytes -------------------------------------------------

    #[test]
    fn test_read_write_bytes() {
        let mut mm = make_mm();
        let base: VAddr = 0xA_0000;
        mm.map(base, PAGE_SIZE_U64, MemoryPermission::READ_WRITE, MemoryState::Normal)
            .unwrap();

        let data = b"Hello, Switch!";
        mm.write_bytes(base, data).unwrap();
        let read_back = mm.read_bytes(base, data.len()).unwrap();
        assert_eq!(&read_back, data);
    }

    #[test]
    fn test_read_write_bytes_cross_page() {
        let mut mm = make_mm();
        let base: VAddr = 0xB_0000;
        // Map two contiguous pages.
        mm.map(
            base,
            PAGE_SIZE_U64 * 2,
            MemoryPermission::READ_WRITE,
            MemoryState::Normal,
        )
        .unwrap();

        // Write across the page boundary.
        let offset = PAGE_SIZE - 8;
        let start_addr = base + offset as u64;
        let data: Vec<u8> = (0..16u8).collect();

        mm.write_bytes(start_addr, &data).unwrap();
        let read_back = mm.read_bytes(start_addr, data.len()).unwrap();
        assert_eq!(read_back, data);
    }

    // -- Permission enforcement ---------------------------------------------

    #[test]
    fn test_read_without_permission() {
        let mut mm = make_mm();
        let base: VAddr = 0xC_0000;
        // Map with write-only (no read).
        mm.map(base, PAGE_SIZE_U64, MemoryPermission::WRITE, MemoryState::Normal)
            .unwrap();

        let result = mm.read_u8(base);
        assert!(matches!(result, Err(MemoryError::PermissionDenied { .. })));
    }

    #[test]
    fn test_write_without_permission() {
        let mut mm = make_mm();
        let base: VAddr = 0xD_0000;
        // Map with read-only.
        mm.map(base, PAGE_SIZE_U64, MemoryPermission::READ, MemoryState::Normal)
            .unwrap();

        let result = mm.write_u8(base, 0xFF);
        assert!(matches!(result, Err(MemoryError::PermissionDenied { .. })));
    }

    #[test]
    fn test_read_unmapped() {
        let mm = make_mm();
        let result = mm.read_u8(0x10_0000);
        assert!(matches!(result, Err(MemoryError::NotMapped(_))));
    }

    #[test]
    fn test_write_unmapped() {
        let mut mm = make_mm();
        let result = mm.write_u8(0x10_0000, 0xFF);
        assert!(matches!(result, Err(MemoryError::NotMapped(_))));
    }

    // -- QueryMemory --------------------------------------------------------

    #[test]
    fn test_query_memory_mapped() {
        let mut mm = make_mm();
        let base: VAddr = 0xE_0000;
        let size = PAGE_SIZE_U64 * 4;
        mm.map(base, size, MemoryPermission::READ_EXECUTE, MemoryState::Code)
            .unwrap();

        let info = mm.query_memory(base + PAGE_SIZE_U64);
        assert_eq!(info.base_addr, base);
        assert_eq!(info.size, size);
        assert_eq!(info.state, MemoryState::Code);
        assert_eq!(info.permission, MemoryPermission::READ_EXECUTE);
    }

    #[test]
    fn test_query_memory_unmapped_gap() {
        let mut mm = make_mm();
        // Map two regions with a gap between them.
        let region1_base: VAddr = 0x10_0000;
        let region1_size = PAGE_SIZE_U64 * 2;
        let region2_base: VAddr = 0x20_0000;
        let region2_size = PAGE_SIZE_U64 * 2;

        mm.map(
            region1_base,
            region1_size,
            MemoryPermission::READ_WRITE,
            MemoryState::Normal,
        )
        .unwrap();
        mm.map(
            region2_base,
            region2_size,
            MemoryPermission::READ,
            MemoryState::Code,
        )
        .unwrap();

        // Query an address in the gap.
        let info = mm.query_memory(0x15_0000);
        assert_eq!(info.state, MemoryState::Unmapped);
        assert_eq!(info.base_addr, region1_base + region1_size);
        assert_eq!(info.size, region2_base - (region1_base + region1_size));
    }

    #[test]
    fn test_query_memory_before_first_region() {
        let mut mm = make_mm();
        let base: VAddr = 0x10_0000;
        mm.map(base, PAGE_SIZE_U64, MemoryPermission::READ, MemoryState::Code)
            .unwrap();

        let info = mm.query_memory(0x1000);
        assert_eq!(info.state, MemoryState::Unmapped);
        assert_eq!(info.base_addr, 0);
        assert_eq!(info.size, base);
    }

    // -- set_permissions ----------------------------------------------------

    #[test]
    fn test_set_permissions() {
        let mut mm = make_mm();
        let base: VAddr = 0xF_0000;
        let size = PAGE_SIZE_U64 * 2;
        mm.map(base, size, MemoryPermission::READ, MemoryState::Code)
            .unwrap();

        // Initially read-only, write should fail.
        assert!(mm.write_u8(base, 0x42).is_err());

        // Upgrade to read-write.
        mm.set_permissions(base, size, MemoryPermission::READ_WRITE)
            .unwrap();

        mm.write_u8(base, 0x42).unwrap();
        assert_eq!(mm.read_u8(base).unwrap(), 0x42);
    }

    // -- Backing offset access ----------------------------------------------

    #[test]
    fn test_backing_offset() {
        let mut mm = make_mm();
        let base: VAddr = 0x8_0000;
        mm.map(base, PAGE_SIZE_U64, MemoryPermission::READ_WRITE, MemoryState::Normal)
            .unwrap();

        let off = mm.backing_offset(base).expect("should have offset");
        // First mapping should start at offset 0 in the backing.
        assert_eq!(off, 0);

        let off_mid = mm
            .backing_offset(base + 0x100)
            .expect("should have offset");
        assert_eq!(off_mid, 0x100);
    }

    #[test]
    fn test_backing_offset_unmapped() {
        let mm = make_mm();
        assert!(mm.backing_offset(0x1_0000).is_none());
    }

    // -- Multiple regions ---------------------------------------------------

    #[test]
    fn test_multiple_regions() {
        let mut mm = make_mm();

        // Map code region.
        mm.map(
            0x8_0000,
            PAGE_SIZE_U64 * 4,
            MemoryPermission::READ_EXECUTE,
            MemoryState::Code,
        )
        .unwrap();

        // Map stack region (non-adjacent).
        mm.map(
            0x10_0000,
            PAGE_SIZE_U64 * 16,
            MemoryPermission::READ_WRITE,
            MemoryState::Stack,
        )
        .unwrap();

        // Map heap region.
        mm.map(
            0x20_0000,
            PAGE_SIZE_U64 * 64,
            MemoryPermission::READ_WRITE,
            MemoryState::Heap,
        )
        .unwrap();

        // Verify code region.
        let info = mm.query_memory(0x8_0000);
        assert_eq!(info.state, MemoryState::Code);

        // Verify stack region.
        let info = mm.query_memory(0x10_0000);
        assert_eq!(info.state, MemoryState::Stack);

        // Verify heap region.
        let info = mm.query_memory(0x20_0000);
        assert_eq!(info.state, MemoryState::Heap);

        // Read and write across regions.
        mm.write_u32(0x20_0000, 0x1234_5678).unwrap();
        assert_eq!(mm.read_u32(0x20_0000).unwrap(), 0x1234_5678);
    }

    // -- Unmap then remap ---------------------------------------------------

    #[test]
    fn test_unmap_then_remap() {
        let mut mm = make_mm();
        let base: VAddr = 0x10_0000;
        let size = PAGE_SIZE_U64 * 2;

        mm.map(base, size, MemoryPermission::READ_WRITE, MemoryState::Normal)
            .unwrap();
        mm.write_u32(base, 0xAAAA_BBBB).unwrap();
        mm.unmap(base, size).unwrap();

        // Reading after unmap should fail.
        assert!(mm.read_u32(base).is_err());

        // Remap the same address (gets fresh backing).
        mm.map(base, size, MemoryPermission::READ_WRITE, MemoryState::Heap)
            .unwrap();

        // Should read zero (fresh backing from sparse mmap).
        assert_eq!(mm.read_u32(base).unwrap(), 0);
    }
}
