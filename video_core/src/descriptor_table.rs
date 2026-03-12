// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Generic descriptor table for caching fixed-size descriptors read from GPU
//! memory pools.
//!
//! Used by Maxwell3D for TIC (Texture Image Control) and TSC (Texture Sampler
//! Control) pools. Each descriptor is `ENTRY_SIZE` bytes. The pool base address
//! and limit come from Maxwell3D registers. On [`DescriptorTable::read`], the
//! descriptor is fetched from GPU memory via a reader closure and cached.
//! Change detection tracks whether the value differs from the last read.

/// Generic descriptor table that caches descriptors from a GPU memory pool.
///
/// Each descriptor is `ENTRY_SIZE` bytes. The pool base address and limit
/// come from Maxwell3D registers. On `read()`, the descriptor is fetched
/// from GPU memory (via a reader closure) and cached. Change detection
/// tracks whether the value differs from the last read.
pub struct DescriptorTable<const ENTRY_SIZE: usize> {
    /// Current pool GPU virtual address.
    gpu_addr: u64,
    /// Current pool limit (max valid index).
    limit: u32,
    /// Bitset tracking which indices have been read (64 indices per u64).
    read_bitset: Vec<u64>,
    /// Cached raw descriptor data: `descriptors[index]` = [u8; ENTRY_SIZE].
    descriptors: Vec<[u8; ENTRY_SIZE]>,
}

impl<const ENTRY_SIZE: usize> DescriptorTable<ENTRY_SIZE> {
    /// Create a new empty descriptor table.
    pub fn new() -> Self {
        Self {
            gpu_addr: 0,
            limit: 0,
            read_bitset: Vec::new(),
            descriptors: Vec::new(),
        }
    }

    /// Update the pool address and limit. Returns `true` if either changed
    /// (which means all cached descriptors are now stale and the table is
    /// reset).
    pub fn synchronize(&mut self, gpu_addr: u64, limit: u32) -> bool {
        if gpu_addr == self.gpu_addr && limit == self.limit {
            return false;
        }
        self.gpu_addr = gpu_addr;
        self.limit = limit;
        let count = (limit + 1) as usize;
        self.descriptors = vec![[0u8; ENTRY_SIZE]; count];
        self.read_bitset = vec![0u64; (count + 63) / 64];
        true
    }

    /// Read descriptor at `index` from the pool via `gpu_read`.
    ///
    /// Returns `(raw_bytes, changed)` where `changed` is true if this is the
    /// first read at this index or if the descriptor data differs from the
    /// previously cached value.
    ///
    /// `gpu_read`: closure `|gpu_va, &mut [u8]|` that reads from GPU VA space.
    pub fn read(
        &mut self,
        index: u32,
        gpu_read: &dyn Fn(u64, &mut [u8]),
    ) -> ([u8; ENTRY_SIZE], bool) {
        assert!(index <= self.limit, "descriptor index {} exceeds limit {}", index, self.limit);

        let gpu_va = self.gpu_addr + (index as u64) * (ENTRY_SIZE as u64);
        let mut buffer = [0u8; ENTRY_SIZE];
        gpu_read(gpu_va, &mut buffer);

        let word = (index / 64) as usize;
        let bit = index % 64;
        let mask = 1u64 << bit;

        if self.read_bitset[word] & mask == 0 {
            // First read at this index.
            self.read_bitset[word] |= mask;
            self.descriptors[index as usize] = buffer;
            (buffer, true)
        } else if buffer != self.descriptors[index as usize] {
            // Data changed since last read.
            self.descriptors[index as usize] = buffer;
            (buffer, true)
        } else {
            // Same data as last read.
            (buffer, false)
        }
    }

    /// Invalidate all cached entries (marks everything as unread).
    ///
    /// Does NOT clear descriptor data, allowing change detection on next read.
    pub fn invalidate(&mut self) {
        for word in &mut self.read_bitset {
            *word = 0;
        }
    }

    /// Current pool GPU virtual address.
    pub fn gpu_addr(&self) -> u64 {
        self.gpu_addr
    }

    /// Current pool limit (max valid index).
    pub fn limit(&self) -> u32 {
        self.limit
    }

    /// Whether the table has a valid (non-zero) address.
    pub fn is_valid(&self) -> bool {
        self.gpu_addr != 0
    }
}

/// TIC descriptor table (32 bytes per entry = 8 x u32).
pub type TicTable = DescriptorTable<32>;

/// TSC descriptor table (32 bytes per entry = 8 x u32).
pub type TscTable = DescriptorTable<32>;

#[cfg(test)]
mod tests {
    use super::*;

    // ── Basic operations ─────────────────────────────────────────────────

    #[test]
    fn test_new_empty() {
        let table = DescriptorTable::<32>::new();
        assert_eq!(table.gpu_addr(), 0);
        assert_eq!(table.limit(), 0);
        assert!(!table.is_valid());
    }

    #[test]
    fn test_synchronize_sets_address() {
        let mut table = DescriptorTable::<32>::new();
        let changed = table.synchronize(0x1000_0000, 64);
        assert!(changed);
        assert_eq!(table.gpu_addr(), 0x1000_0000);
        assert_eq!(table.limit(), 64);
        assert!(table.is_valid());
    }

    #[test]
    fn test_synchronize_no_change() {
        let mut table = DescriptorTable::<32>::new();
        table.synchronize(0x1000_0000, 64);
        let changed = table.synchronize(0x1000_0000, 64);
        assert!(!changed);
    }

    #[test]
    fn test_synchronize_reset_on_change() {
        let mut table = DescriptorTable::<32>::new();
        table.synchronize(0x1000_0000, 64);

        // Read index 0 to populate cache.
        let data = [0xAA; 32];
        let reader = |_addr: u64, buf: &mut [u8]| buf.copy_from_slice(&data);
        let (_, changed) = table.read(0, &reader);
        assert!(changed);

        // Re-read same data → not changed.
        let (_, changed) = table.read(0, &reader);
        assert!(!changed);

        // Change address → table resets, next read is "changed".
        table.synchronize(0x2000_0000, 64);
        let (_, changed) = table.read(0, &reader);
        assert!(changed);
    }

    #[test]
    fn test_invalidate_clears_read_state() {
        let mut table = DescriptorTable::<32>::new();
        table.synchronize(0x1000_0000, 10);

        let data = [0x42; 32];
        let reader = |_: u64, buf: &mut [u8]| buf.copy_from_slice(&data);

        // First read.
        let (_, changed) = table.read(0, &reader);
        assert!(changed);

        // Second read → same data, not changed.
        let (_, changed) = table.read(0, &reader);
        assert!(!changed);

        // Invalidate, then read → changed again (even though data is the same).
        table.invalidate();
        let (_, changed) = table.read(0, &reader);
        assert!(changed);
    }

    #[test]
    fn test_is_valid() {
        let mut table = DescriptorTable::<32>::new();
        assert!(!table.is_valid());
        table.synchronize(0x1000, 10);
        assert!(table.is_valid());
        table.synchronize(0, 0);
        assert!(!table.is_valid());
    }

    // ── Read and change detection ────────────────────────────────────────

    #[test]
    fn test_read_first_time() {
        let mut table = DescriptorTable::<32>::new();
        table.synchronize(0x5000, 5);

        let data = [0x11; 32];
        let reader = |_: u64, buf: &mut [u8]| buf.copy_from_slice(&data);
        let (raw, changed) = table.read(3, &reader);
        assert!(changed);
        assert_eq!(raw, data);
    }

    #[test]
    fn test_read_same_data() {
        let mut table = DescriptorTable::<32>::new();
        table.synchronize(0x5000, 5);

        let data = [0x22; 32];
        let reader = |_: u64, buf: &mut [u8]| buf.copy_from_slice(&data);
        table.read(2, &reader);
        let (_, changed) = table.read(2, &reader);
        assert!(!changed);
    }

    #[test]
    fn test_read_changed_data() {
        let mut table = DescriptorTable::<32>::new();
        table.synchronize(0x5000, 5);

        let data1 = [0x33; 32];
        let reader1 = |_: u64, buf: &mut [u8]| buf.copy_from_slice(&data1);
        table.read(1, &reader1);

        let data2 = [0x44; 32];
        let reader2 = |_: u64, buf: &mut [u8]| buf.copy_from_slice(&data2);
        let (raw, changed) = table.read(1, &reader2);
        assert!(changed);
        assert_eq!(raw, data2);
    }

    #[test]
    fn test_read_correct_gpu_address() {
        let mut table = DescriptorTable::<32>::new();
        table.synchronize(0x1_0000, 10);

        use std::cell::Cell;
        let captured_addr = Cell::new(0u64);
        let reader = |addr: u64, buf: &mut [u8]| {
            captured_addr.set(addr);
            buf.fill(0);
        };

        table.read(5, &reader);
        // Expected: base + index * entry_size = 0x1_0000 + 5 * 32 = 0x1_00A0
        assert_eq!(captured_addr.get(), 0x1_0000 + 5 * 32);
    }

    #[test]
    fn test_read_index_zero() {
        let mut table = DescriptorTable::<32>::new();
        table.synchronize(0x8000, 5);

        use std::cell::Cell;
        let captured_addr = Cell::new(0u64);
        let reader = |addr: u64, buf: &mut [u8]| {
            captured_addr.set(addr);
            buf.fill(0);
        };

        table.read(0, &reader);
        assert_eq!(captured_addr.get(), 0x8000);
    }

    #[test]
    fn test_read_max_index() {
        let mut table = DescriptorTable::<32>::new();
        let limit = 99;
        table.synchronize(0x2000, limit);

        let data = [0xFF; 32];
        let reader = |_: u64, buf: &mut [u8]| buf.copy_from_slice(&data);
        let (raw, changed) = table.read(limit, &reader);
        assert!(changed);
        assert_eq!(raw, data);
    }

    #[test]
    fn test_read_multiple_indices() {
        let mut table = DescriptorTable::<32>::new();
        table.synchronize(0x3000, 10);

        let data_a = [0xAA; 32];
        let data_b = [0xBB; 32];
        let reader_a = |_: u64, buf: &mut [u8]| buf.copy_from_slice(&data_a);
        let reader_b = |_: u64, buf: &mut [u8]| buf.copy_from_slice(&data_b);

        // First read at both indices → changed.
        let (_, c1) = table.read(0, &reader_a);
        let (_, c2) = table.read(1, &reader_b);
        assert!(c1);
        assert!(c2);

        // Re-read same data → not changed, independently.
        let (_, c1) = table.read(0, &reader_a);
        let (_, c2) = table.read(1, &reader_b);
        assert!(!c1);
        assert!(!c2);
    }

    #[test]
    fn test_read_after_invalidate() {
        let mut table = DescriptorTable::<32>::new();
        table.synchronize(0x4000, 5);

        let data = [0x55; 32];
        let reader = |_: u64, buf: &mut [u8]| buf.copy_from_slice(&data);

        table.read(3, &reader);
        table.invalidate();

        // Same data but after invalidate → changed=true.
        let (_, changed) = table.read(3, &reader);
        assert!(changed);
    }

    // ── Bitset and capacity ──────────────────────────────────────────────

    #[test]
    fn test_large_pool_limit() {
        let mut table = DescriptorTable::<32>::new();
        table.synchronize(0x1_0000_0000, 1000);
        assert_eq!(table.limit(), 1000);
        // 1001 entries → ceil(1001/64) = 16 bitset words.
        assert_eq!(table.read_bitset.len(), 16);
        assert_eq!(table.descriptors.len(), 1001);
    }

    #[test]
    fn test_bitset_word_boundary() {
        let mut table = DescriptorTable::<32>::new();
        table.synchronize(0x1000, 128);

        let data = [0xCC; 32];
        let reader = |_: u64, buf: &mut [u8]| buf.copy_from_slice(&data);

        // Index 63 is in word 0, index 64 is in word 1.
        let (_, c63) = table.read(63, &reader);
        let (_, c64) = table.read(64, &reader);
        assert!(c63);
        assert!(c64);

        // Re-read → both unchanged, independently tracked.
        let (_, c63) = table.read(63, &reader);
        let (_, c64) = table.read(64, &reader);
        assert!(!c63);
        assert!(!c64);
    }

    #[test]
    fn test_synchronize_grows_pool() {
        let mut table = DescriptorTable::<32>::new();
        table.synchronize(0x1000, 10);
        assert_eq!(table.descriptors.len(), 11);

        let changed = table.synchronize(0x1000, 100);
        assert!(changed);
        assert_eq!(table.descriptors.len(), 101);
    }

    #[test]
    fn test_synchronize_shrinks_pool() {
        let mut table = DescriptorTable::<32>::new();
        table.synchronize(0x1000, 100);
        assert_eq!(table.descriptors.len(), 101);

        let changed = table.synchronize(0x1000, 10);
        assert!(changed);
        assert_eq!(table.descriptors.len(), 11);
    }

    // ── Type alias tests ─────────────────────────────────────────────────

    #[test]
    fn test_tic_table_32_bytes() {
        let mut tic: TicTable = TicTable::new();
        tic.synchronize(0xA000, 2);

        let mut data = [0u8; 32];
        data[0] = 0x42; // Recognizable pattern.
        let reader = |_: u64, buf: &mut [u8]| buf.copy_from_slice(&data);

        let (raw, changed) = tic.read(0, &reader);
        assert!(changed);
        assert_eq!(raw.len(), 32);
        assert_eq!(raw[0], 0x42);
    }

    #[test]
    fn test_tsc_table_32_bytes() {
        let mut tsc: TscTable = TscTable::new();
        tsc.synchronize(0xB000, 2);

        let mut data = [0u8; 32];
        data[31] = 0x99; // Recognizable pattern at end.
        let reader = |_: u64, buf: &mut [u8]| buf.copy_from_slice(&data);

        let (raw, changed) = tsc.read(1, &reader);
        assert!(changed);
        assert_eq!(raw.len(), 32);
        assert_eq!(raw[31], 0x99);
    }
}
