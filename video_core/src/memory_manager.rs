// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GPU virtual address space manager.
//!
//! Maps GPU virtual addresses (40-bit, 1 TiB) to guest physical (CPU) addresses
//! using a 2-level page table with 4 KB pages.
//!
//! Layout: `[L0: 14 bits][L1: 14 bits][Offset: 12 bits]` = 40 bits total.

const PAGE_BITS: u32 = 12;
const PAGE_SIZE: u64 = 1 << PAGE_BITS;
const L1_BITS: u32 = 14;
const L0_BITS: u32 = 14;
const L1_SIZE: usize = 1 << L1_BITS; // 16384 entries per L1 table

/// Sentinel for unmapped pages.
const INVALID_ENTRY: u64 = u64::MAX;

/// Total GPU address space: 40 bits = 1 TiB.
pub const GPU_VA_BITS: u32 = L0_BITS + L1_BITS + PAGE_BITS;
pub const GPU_VA_SIZE: u64 = 1 << GPU_VA_BITS;

/// GPU virtual memory manager with a 2-level page table.
pub struct GpuMemoryManager {
    /// L0 table: each entry is an optional L1 page table.
    page_table: Vec<Option<Box<[u64; L1_SIZE]>>>,
    /// Next free GPU VA for bump allocation.
    next_alloc: u64,
}

impl GpuMemoryManager {
    pub fn new() -> Self {
        let l0_size = 1 << L0_BITS;
        let mut page_table = Vec::with_capacity(l0_size);
        for _ in 0..l0_size {
            page_table.push(None);
        }
        Self {
            page_table,
            // Start allocations at 64 MB to avoid the zero page region.
            next_alloc: 0x0400_0000,
        }
    }

    /// Map a contiguous range of GPU VA to CPU addresses.
    pub fn map(&mut self, gpu_va: u64, cpu_addr: u64, size: u64) {
        let mut offset = 0u64;
        while offset < size {
            let va = gpu_va + offset;
            let ca = cpu_addr + offset;
            self.set_entry(va, ca);
            offset += PAGE_SIZE;
        }
        log::trace!(
            "gpu_mm: map GPU 0x{:X}..0x{:X} -> CPU 0x{:X}",
            gpu_va,
            gpu_va + size,
            cpu_addr
        );
    }

    /// Unmap a contiguous GPU VA range.
    pub fn unmap(&mut self, gpu_va: u64, size: u64) {
        let mut offset = 0u64;
        while offset < size {
            let va = gpu_va + offset;
            self.set_entry(va, INVALID_ENTRY);
            offset += PAGE_SIZE;
        }
        log::trace!("gpu_mm: unmap GPU 0x{:X}..0x{:X}", gpu_va, gpu_va + size);
    }

    /// Translate a GPU VA to a CPU/guest physical address.
    /// Returns `None` if the page is not mapped.
    pub fn translate(&self, gpu_va: u64) -> Option<u64> {
        let page_offset = gpu_va & (PAGE_SIZE - 1);
        let entry = self.get_entry(gpu_va);
        if entry == INVALID_ENTRY {
            None
        } else {
            Some(entry + page_offset)
        }
    }

    /// Map at a specific GPU VA (alias for `map`).
    pub fn alloc_fixed(&mut self, gpu_va: u64, cpu_addr: u64, size: u64) {
        self.map(gpu_va, cpu_addr, size);
    }

    /// Allocate GPU VA from the bump allocator and map to a CPU address.
    /// Returns the allocated GPU VA.
    pub fn alloc_any(&mut self, cpu_addr: u64, size: u64) -> u64 {
        let aligned_size = (size + PAGE_SIZE - 1) & !(PAGE_SIZE - 1);
        let gpu_va = self.next_alloc;
        self.next_alloc += aligned_size;
        self.map(gpu_va, cpu_addr, aligned_size);
        gpu_va
    }

    /// Read bytes from GPU VA space using a CPU memory reader.
    /// `read_cpu_mem` reads from a guest physical address.
    pub fn read(
        &self,
        gpu_va: u64,
        dst: &mut [u8],
        read_cpu_mem: &dyn Fn(u64, &mut [u8]),
    ) {
        let mut offset = 0usize;
        while offset < dst.len() {
            let va = gpu_va + offset as u64;
            let page_off = (va & (PAGE_SIZE - 1)) as usize;
            let chunk_size = std::cmp::min(dst.len() - offset, PAGE_SIZE as usize - page_off);

            if let Some(cpu_addr) = self.translate(va) {
                read_cpu_mem(cpu_addr, &mut dst[offset..offset + chunk_size]);
            } else {
                // Unmapped — fill with zeros.
                dst[offset..offset + chunk_size].fill(0);
            }
            offset += chunk_size;
        }
    }

    /// Write bytes to GPU VA space using a CPU memory writer.
    /// `write_cpu_mem` writes to a guest physical address.
    pub fn write(
        &self,
        gpu_va: u64,
        src: &[u8],
        write_cpu_mem: &mut dyn FnMut(u64, &[u8]),
    ) {
        let mut offset = 0usize;
        while offset < src.len() {
            let va = gpu_va + offset as u64;
            let page_off = (va & (PAGE_SIZE - 1)) as usize;
            let chunk_size = std::cmp::min(src.len() - offset, PAGE_SIZE as usize - page_off);
            if let Some(cpu_addr) = self.translate(va) {
                write_cpu_mem(cpu_addr, &src[offset..offset + chunk_size]);
            }
            offset += chunk_size;
        }
    }

    /// Translate a GPU VA to a CPU/guest physical address.
    /// Upstream: `MemoryManager::GpuToCpuAddress(gpu_addr)`.
    /// This is the primary API used by buffer_cache, texture_cache, and engines.
    pub fn gpu_to_cpu_address(&self, gpu_addr: u64) -> Option<u64> {
        self.translate(gpu_addr)
    }

    /// Check if a GPU VA range is fully mapped and contiguous in CPU space.
    /// Upstream: `MemoryManager::IsContinuousRange(gpu_addr, size)`.
    pub fn is_continuous_range(&self, gpu_addr: u64, size: u64) -> bool {
        if size == 0 {
            return true;
        }
        let first_cpu = match self.translate(gpu_addr) {
            Some(addr) => addr,
            None => return false,
        };
        let mut offset = PAGE_SIZE;
        while offset < size {
            let expected = first_cpu + offset;
            match self.translate(gpu_addr + offset) {
                Some(addr) if addr == expected => {}
                _ => return false,
            }
            offset += PAGE_SIZE;
        }
        true
    }

    /// Check if a GPU VA range is fully mapped (each page has a valid entry).
    /// Upstream: `MemoryManager::IsFullyMappedRange(gpu_addr, size)`.
    pub fn is_fully_mapped_range(&self, gpu_addr: u64, size: u64) -> bool {
        let mut offset = 0u64;
        while offset < size {
            if self.translate(gpu_addr + offset).is_none() {
                return false;
            }
            offset += PAGE_SIZE;
        }
        true
    }

    /// Check if a GPU VA range fits within a single page (granular access).
    /// Upstream: `MemoryManager::IsGranularRange(gpu_addr, size)`.
    pub fn is_granular_range(&self, gpu_addr: u64, size: u64) -> bool {
        let start_page = gpu_addr >> PAGE_BITS;
        let end_page = (gpu_addr + size - 1) >> PAGE_BITS;
        start_page == end_page
    }

    /// Read a block of data from GPU VA space into a buffer.
    /// Upstream: `MemoryManager::ReadBlock(gpu_src, output, size)`.
    pub fn read_block(&self, gpu_src: u64, output: &mut [u8], read_cpu: &dyn Fn(u64, &mut [u8])) {
        self.read(gpu_src, output, read_cpu);
    }

    /// Read a block (unsafe variant — no cache flush).
    /// Upstream: `MemoryManager::ReadBlockUnsafe(gpu_src, output, size)`.
    pub fn read_block_unsafe(&self, gpu_src: u64, output: &mut [u8], read_cpu: &dyn Fn(u64, &mut [u8])) {
        self.read(gpu_src, output, read_cpu);
    }

    /// Write a block of data from a buffer to GPU VA space.
    /// Upstream: `MemoryManager::WriteBlock(gpu_dest, input, size)`.
    pub fn write_block(&self, gpu_dest: u64, input: &[u8], write_cpu: &mut dyn FnMut(u64, &[u8])) {
        self.write(gpu_dest, input, write_cpu);
    }

    /// Write a block (unsafe variant — no cache invalidation).
    /// Upstream: `MemoryManager::WriteBlockUnsafe(gpu_dest, input, size)`.
    pub fn write_block_unsafe(&self, gpu_dest: u64, input: &[u8], write_cpu: &mut dyn FnMut(u64, &[u8])) {
        self.write(gpu_dest, input, write_cpu);
    }

    /// Flush a region (notify rasterizer to write back).
    /// Upstream: `MemoryManager::FlushRegion(gpu_addr, size)`.
    /// Requires rasterizer binding — no-op until bound.
    pub fn flush_region(&self, _gpu_addr: u64, _size: u64) {
        // Upstream: calls rasterizer->FlushRegion() if bound.
    }

    /// Invalidate a region (notify rasterizer to discard cache).
    /// Upstream: `MemoryManager::InvalidateRegion(gpu_addr, size)`.
    pub fn invalidate_region(&self, _gpu_addr: u64, _size: u64) {
        // Upstream: calls rasterizer->InvalidateRegion() if bound.
    }

    /// Check if a GPU address is within the valid address range.
    ///
    /// Upstream: `MemoryManager::IsWithinGPUAddressRange(gpu_addr)`
    pub fn is_within_gpu_address_range(&self, gpu_addr: u64) -> bool {
        gpu_addr < GPU_VA_SIZE
    }

    /// Return the maximum continuous range of mapped GPU memory starting at `gpu_addr`.
    ///
    /// Upstream: `MemoryManager::MaxContinuousRange(gpu_addr, size)`
    /// Walks pages forward from `gpu_addr` until an unmapped page is found or `size` is reached.
    pub fn max_continuous_range(&self, gpu_addr: u64, size: u64) -> u64 {
        let mut remaining = size;
        let mut current = gpu_addr;
        while remaining > 0 {
            if self.translate(current).is_none() {
                break;
            }
            let page_offset = current & (PAGE_SIZE - 1);
            let chunk = std::cmp::min(remaining, PAGE_SIZE - page_offset);
            current += chunk;
            remaining -= chunk;
        }
        size - remaining
    }

    /// Return the total mapped size starting from a GPU address.
    ///
    /// Upstream: `MemoryManager::GetMemoryLayoutSize(gpu_addr)`
    /// Returns the continuous mapped range size from `gpu_addr`.
    pub fn get_memory_layout_size(&self, gpu_addr: u64) -> u64 {
        // Walk pages until we hit an unmapped page or end of address space.
        let mut total = 0u64;
        let mut current = gpu_addr;
        loop {
            if current >= GPU_VA_SIZE {
                break;
            }
            if self.translate(current).is_none() {
                break;
            }
            let page_offset = current & (PAGE_SIZE - 1);
            let chunk = PAGE_SIZE - page_offset;
            total += chunk;
            current += chunk;
        }
        total
    }

    // ── Internal helpers ─────────────────────────────────────────────────

    fn l0_index(gpu_va: u64) -> usize {
        ((gpu_va >> (L1_BITS + PAGE_BITS)) & ((1 << L0_BITS) - 1)) as usize
    }

    fn l1_index(gpu_va: u64) -> usize {
        ((gpu_va >> PAGE_BITS) & ((1 << L1_BITS) - 1)) as usize
    }

    fn set_entry(&mut self, gpu_va: u64, cpu_page_addr: u64) {
        let l0 = Self::l0_index(gpu_va);
        let l1 = Self::l1_index(gpu_va);

        if self.page_table[l0].is_none() {
            if cpu_page_addr == INVALID_ENTRY {
                return; // No L1 table and we're unmapping — nothing to do.
            }
            self.page_table[l0] = Some(Box::new([INVALID_ENTRY; L1_SIZE]));
        }

        // Store the entry: either INVALID_ENTRY for unmapped, or aligned CPU address.
        let entry = if cpu_page_addr == INVALID_ENTRY {
            INVALID_ENTRY
        } else {
            cpu_page_addr & !(PAGE_SIZE - 1)
        };
        self.page_table[l0].as_mut().unwrap()[l1] = entry;
    }

    fn get_entry(&self, gpu_va: u64) -> u64 {
        let l0 = Self::l0_index(gpu_va);
        let l1 = Self::l1_index(gpu_va);

        match &self.page_table[l0] {
            Some(table) => table[l1],
            None => INVALID_ENTRY,
        }
    }

    /// Read a value of type `T` from GPU virtual address space.
    ///
    /// Upstream: `template <typename T> T MemoryManager::Read(GPUVAddr addr) const`
    ///
    /// Reads `size_of::<T>()` bytes starting at `gpu_addr`, translating through the
    /// page table page-by-page and using `read_cpu_mem` to access guest physical memory.
    /// Returns `None` if the first page is unmapped.
    ///
    /// The type `T` must be `Copy` and have no padding (caller's responsibility).
    pub fn read_value<T: Copy>(
        &self,
        gpu_addr: u64,
        read_cpu_mem: &dyn Fn(u64, &mut [u8]),
    ) -> Option<T> {
        let size = std::mem::size_of::<T>();
        // Safety: we're reading into a zeroed byte buffer then transmuting.
        // This matches upstream's memcpy-based Read<T>.
        let mut bytes = vec![0u8; size];
        // Check that the first page is mapped (upstream asserts on failure).
        if self.translate(gpu_addr).is_none() {
            return None;
        }
        self.read(gpu_addr, &mut bytes, read_cpu_mem);
        // Safety: T is Copy and we have exactly size_of::<T>() bytes.
        let value = unsafe { std::ptr::read(bytes.as_ptr() as *const T) };
        Some(value)
    }

    /// Write a value of type `T` to GPU virtual address space.
    ///
    /// Upstream: `template <typename T> void MemoryManager::Write(GPUVAddr addr, T data)`
    pub fn write_value<T: Copy>(
        &self,
        gpu_addr: u64,
        data: T,
        write_cpu_mem: &mut dyn FnMut(u64, &[u8]),
    ) {
        let size = std::mem::size_of::<T>();
        let bytes =
            unsafe { std::slice::from_raw_parts(&data as *const T as *const u8, size) };
        self.write(gpu_addr, bytes, write_cpu_mem);
    }
}

impl Default for GpuMemoryManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_map_and_translate() {
        let mut mm = GpuMemoryManager::new();
        mm.map(0x1000, 0xDEAD_0000, 0x2000);

        assert_eq!(mm.translate(0x1000), Some(0xDEAD_0000));
        assert_eq!(mm.translate(0x1500), Some(0xDEAD_0500));
        assert_eq!(mm.translate(0x2000), Some(0xDEAD_1000));
        assert_eq!(mm.translate(0x2FFF), Some(0xDEAD_1FFF));
    }

    #[test]
    fn test_unmapped_returns_none() {
        let mm = GpuMemoryManager::new();
        assert_eq!(mm.translate(0x1000), None);
        assert_eq!(mm.translate(0), None);
    }

    #[test]
    fn test_unmap() {
        let mut mm = GpuMemoryManager::new();
        mm.map(0x1000, 0xBEEF_0000, 0x1000);
        assert_eq!(mm.translate(0x1000), Some(0xBEEF_0000));

        mm.unmap(0x1000, 0x1000);
        assert_eq!(mm.translate(0x1000), None);
    }

    #[test]
    fn test_alloc_any() {
        let mut mm = GpuMemoryManager::new();
        let va1 = mm.alloc_any(0xAAAA_0000, 0x3000);
        let va2 = mm.alloc_any(0xBBBB_0000, 0x1000);

        // va2 should be after va1's allocation (3 pages).
        assert!(va2 > va1);
        assert_eq!(va2, va1 + 0x3000);

        assert_eq!(mm.translate(va1), Some(0xAAAA_0000));
        assert_eq!(mm.translate(va2), Some(0xBBBB_0000));
    }

    #[test]
    fn test_page_boundary_offset() {
        let mut mm = GpuMemoryManager::new();
        mm.map(0x5000, 0xCAFE_0000, 0x1000);

        // Address within the page should add the offset.
        assert_eq!(mm.translate(0x5ABC), Some(0xCAFE_0ABC));
    }

    #[test]
    fn test_read_via_callback() {
        let mut mm = GpuMemoryManager::new();
        mm.map(0x1000, 0x8000_0000, 0x1000);

        let mut buf = [0u8; 4];
        mm.read(0x1000, &mut buf, &|addr, dst| {
            // Simulate guest memory: return address bytes.
            let bytes = (addr as u32).to_le_bytes();
            let len = dst.len().min(bytes.len());
            dst[..len].copy_from_slice(&bytes[..len]);
        });

        let val = u32::from_le_bytes(buf);
        assert_eq!(val, 0x8000_0000);
    }

    #[test]
    fn test_write_via_callback() {
        let mut mm = GpuMemoryManager::new();
        mm.map(0x1000, 0x8000_0000, 0x1000);

        let mut written: Vec<(u64, Vec<u8>)> = Vec::new();
        let data = [0xDE, 0xAD, 0xBE, 0xEF];
        mm.write(0x1000, &data, &mut |addr, src| {
            written.push((addr, src.to_vec()));
        });

        assert_eq!(written.len(), 1);
        assert_eq!(written[0].0, 0x8000_0000);
        assert_eq!(written[0].1, vec![0xDE, 0xAD, 0xBE, 0xEF]);
    }

    #[test]
    fn test_write_cross_page() {
        let mut mm = GpuMemoryManager::new();
        // Map two consecutive pages to different CPU addresses.
        mm.map(0x1000, 0xA000_0000, 0x1000);
        mm.map(0x2000, 0xB000_0000, 0x1000);

        let mut written: Vec<(u64, Vec<u8>)> = Vec::new();
        // Write 8 bytes starting at 0x1FFC (4 bytes in page 1, 4 bytes in page 2).
        let data = [1, 2, 3, 4, 5, 6, 7, 8];
        mm.write(0x1FFC, &data, &mut |addr, src| {
            written.push((addr, src.to_vec()));
        });

        assert_eq!(written.len(), 2);
        // First chunk: last 4 bytes of page 1.
        assert_eq!(written[0].0, 0xA000_0FFC);
        assert_eq!(written[0].1, vec![1, 2, 3, 4]);
        // Second chunk: first 4 bytes of page 2.
        assert_eq!(written[1].0, 0xB000_0000);
        assert_eq!(written[1].1, vec![5, 6, 7, 8]);
    }
}
