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
