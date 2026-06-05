// SPDX-FileCopyrightText: Copyright 2024 ruzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/host1x/gpu_device_memory_manager.h` and
//! `gpu_device_memory_manager.cpp`.
//!
//! Defines the Maxwell device memory traits and the device memory manager type
//! alias. In C++ this instantiates `Core::DeviceMemoryManager<MaxwellDeviceTraits>`;
//! in Rust we define the trait constants and a placeholder manager struct until
//! the core device memory manager crate is fully ported.

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Mutex;

/// Number of virtual address bits for the Maxwell device address space.
///
/// Port of `MaxwellDeviceTraits::device_virtual_bits`.
pub const DEVICE_VIRTUAL_BITS: usize = 34;

/// Page size constants matching `core/memory.h:YUZU_PAGEBITS` /
/// `YUZU_PAGESIZE`. ruzu's `common::types::PAGE_SIZE` is 0x1000.
pub const PAGE_BITS: u32 = 12;
pub const PAGE_SIZE: u64 = 1 << PAGE_BITS;

/// Device address type (matches upstream `DAddr`).
pub type DAddr = u64;

/// Callback signature for `MaxwellDeviceMethods::MarkRegionCaching`.
/// `(address, size, caching)`. Upstream calls
/// `Core::Memory::Memory::RasterizerMarkRegionCached(address, size, caching)`
/// which routes to the active rasterizer. The Rust port stores it as a Box
/// so callers can swap a no-op or a live rasterizer-bound closure.
pub type MarkRegionCachingFn = Box<dyn Fn(u64, usize, bool) + Send + Sync>;
pub type InvalidateRegionFn = Box<dyn Fn(DAddr, usize) + Send + Sync>;

/// Port of `Core::DeviceMemoryManager<MaxwellDeviceTraits>`.
///
/// This port keeps only the `UpdatePagesCachedCount` slice that the shader /
/// buffer / texture caches call into. The Read/Write/GetPointer entry points
/// remain stubs pending the full `Core::DeviceMemoryManager` infrastructure.
pub struct MaxwellDeviceMemoryManager {
    /// Per-page reference count, matching upstream's
    /// `cached_pages` table (`u8` atomic per page). Stored in a `HashMap`
    /// keyed by page-index so the working set stays proportional to the
    /// number of cached pages rather than the full 39-bit address space.
    /// Upstream uses a `SubentriesArray` of `std::atomic_uint8_t` for
    /// O(1) access; the HashMap matches semantics under one `Mutex`.
    cached_pages: Mutex<std::collections::HashMap<u64, u8>>,

    /// Optional callback invoked when a contiguous range transitions
    /// to / from cached. Mirrors upstream's
    /// `Tegra::MaxwellDeviceMethods::MarkRegionCaching` indirection
    /// through `Core::Memory::Memory::RasterizerMarkRegionCached`.
    /// Initially `None` until the rasterizer wires itself in;
    /// `update_pages_cached_count` then still maintains the ref counts
    /// (matches upstream when `memory_device_inter == nullptr`).
    mark_region_caching: Mutex<Option<MarkRegionCachingFn>>,

    /// Optional callback invoked after device writes. Mirrors upstream
    /// `DeviceMemoryManager<Traits>::WriteBlock`, which writes to backing
    /// memory and then calls `device_inter->InvalidateRegion(address, size)`.
    invalidate_region: Mutex<Option<InvalidateRegionFn>>,

    /// SMMU bump allocator — next free device address. Matches upstream's
    /// `Core::DeviceMemoryManager<MaxwellDeviceTraits>::Allocate` returning
    /// fresh SMMU virtual addresses. Starts at `SMMU_BASE` and increments
    /// by aligned size. Allocation is bump-only — no free-list — which
    /// matches upstream's MK8D startup behavior closely enough for the
    /// games tested.
    smmu_next: AtomicU64,

    /// SMMU page table: page-aligned device address → host pointer (as
    /// `usize` since raw pointers don't impl `Send`/`Sync` automatically).
    /// Upstream's equivalent is the `entries` array in
    /// `DeviceMemoryManager<MaxwellDeviceTraits>`, indexed by page number.
    /// Populated by `smmu_map`, walked by `smmu_get_host_ptr`.
    smmu_page_table: Mutex<std::collections::HashMap<u64, usize>>,
}

/// Base device address for the SMMU bump allocator. Mirrors upstream's
/// `host1x.MemoryManager().Allocate()` returning small DRAM offsets like
/// `0x41000` (observed in zuyu logs for MK8D handle 0x8).
pub const SMMU_BASE: u64 = 0x40000;

/// Page size used by the SMMU page table — matches CPU page size so each
/// host page maps to one SMMU page.
pub const SMMU_PAGE_BITS: u32 = 12;
pub const SMMU_PAGE_SIZE: u64 = 1 << SMMU_PAGE_BITS;

static UPDATE_CACHED_LAST_STAGE: AtomicU64 = AtomicU64::new(0);
static UPDATE_CACHED_COUNTS: [AtomicU64; 10] = [
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
];

fn record_update_cached_stage(stage: usize) {
    if std::env::var_os("RUZU_PROFILE_UPDATE_CACHED_STALL").is_none() {
        return;
    }
    UPDATE_CACHED_LAST_STAGE.store(stage as u64, Ordering::Relaxed);
    if let Some(counter) = UPDATE_CACHED_COUNTS.get(stage) {
        counter.fetch_add(1, Ordering::Relaxed);
    }
}

pub fn dump_update_cached_stall_profile() {
    if UPDATE_CACHED_COUNTS[0].load(Ordering::Relaxed) == 0 {
        return;
    }
    const NAMES: [&str; 10] = [
        "enter",
        "after_range",
        "before_cached_pages_lock",
        "after_cached_pages_lock",
        "after_page_loop",
        "after_counts_drop",
        "before_callback_lock",
        "after_callback_lock",
        "after_callbacks",
        "exit",
    ];
    let last_stage = UPDATE_CACHED_LAST_STAGE.load(Ordering::Relaxed) as usize;
    let last_stage_name = NAMES.get(last_stage).copied().unwrap_or("unknown");
    eprintln!(
        "[UPDATE_CACHED_STALL_PROFILE] last_stage={} ({})",
        last_stage, last_stage_name
    );
    for (index, name) in NAMES.iter().enumerate() {
        eprintln!(
            "[UPDATE_CACHED_STALL_PROFILE]   {:02} {:<28} {}",
            index,
            name,
            UPDATE_CACHED_COUNTS[index].load(Ordering::Relaxed)
        );
    }
}

/// Implement the texture-cache `GpuMemoryReader` adapter so descriptor
/// tables can read TIC/TSC entries directly via `smmu_read_block`. The
/// texture cache holds an `Arc<MaxwellDeviceMemoryManager>`, so passing
/// `&*device_memory` already yields a `&dyn GpuMemoryReader`.
impl crate::texture_cache::descriptor_table::GpuMemoryReader for MaxwellDeviceMemoryManager {
    fn read_block(&self, d_address: u64, output: &mut [u8]) -> bool {
        self.smmu_read_block(d_address, output)
    }

    fn addr_valid(&self, d_address: u64) -> bool {
        self.smmu_get_host_ptr(d_address).is_some()
    }
}

impl Default for MaxwellDeviceMemoryManager {
    fn default() -> Self {
        Self {
            cached_pages: Mutex::new(std::collections::HashMap::new()),
            mark_region_caching: Mutex::new(None),
            invalidate_region: Mutex::new(None),
            smmu_next: AtomicU64::new(SMMU_BASE),
            smmu_page_table: Mutex::new(std::collections::HashMap::new()),
        }
    }
}

impl MaxwellDeviceMemoryManager {
    /// Read a value of type T from a device address.
    ///
    /// Stubbed — requires the full `Core::DeviceMemoryManager<MaxwellDeviceTraits>` port.
    pub fn read_u8(&self, _addr: DAddr) -> u8 {
        log::warn!("MaxwellDeviceMemoryManager::read_u8: not yet implemented");
        0
    }

    pub fn read_u16(&self, _addr: DAddr) -> u16 {
        log::warn!("MaxwellDeviceMemoryManager::read_u16: not yet implemented");
        0
    }

    pub fn read_u32(&self, _addr: DAddr) -> u32 {
        log::warn!("MaxwellDeviceMemoryManager::read_u32: not yet implemented");
        0
    }

    pub fn read_u64(&self, _addr: DAddr) -> u64 {
        log::warn!("MaxwellDeviceMemoryManager::read_u64: not yet implemented");
        0
    }

    /// Write a value of type T to a device address.
    ///
    /// Stubbed — requires the full `Core::DeviceMemoryManager<MaxwellDeviceTraits>` port.
    pub fn write_u8(&self, _addr: DAddr, _data: u8) {
        log::warn!("MaxwellDeviceMemoryManager::write_u8: not yet implemented");
    }

    pub fn write_u16(&self, _addr: DAddr, _data: u16) {
        log::warn!("MaxwellDeviceMemoryManager::write_u16: not yet implemented");
    }

    pub fn write_u32(&self, _addr: DAddr, _data: u32) {
        log::warn!("MaxwellDeviceMemoryManager::write_u32: not yet implemented");
    }

    pub fn write_u64(&self, _addr: DAddr, _data: u64) {
        log::warn!("MaxwellDeviceMemoryManager::write_u64: not yet implemented");
    }

    /// Get a pointer to the given device address.
    ///
    /// Stubbed — requires the full `Core::DeviceMemoryManager<MaxwellDeviceTraits>` port.
    pub fn get_pointer(&self, _addr: DAddr) -> *const u8 {
        log::warn!("MaxwellDeviceMemoryManager::get_pointer: not yet implemented");
        std::ptr::null()
    }

    /// Get a mutable pointer to the given device address.
    ///
    /// Stubbed — requires the full `Core::DeviceMemoryManager<MaxwellDeviceTraits>` port.
    pub fn get_pointer_mut(&self, _addr: DAddr) -> *mut u8 {
        log::warn!("MaxwellDeviceMemoryManager::get_pointer_mut: not yet implemented");
        std::ptr::null_mut()
    }

    /// Install or replace the `MarkRegionCaching` callback. The rasterizer
    /// passes a closure that bridges to its own
    /// `update_pages_cached_count` implementation, mirroring upstream's
    /// `Tegra::MaxwellDeviceMethods::MarkRegionCaching` →
    /// `Core::Memory::Memory::RasterizerMarkRegionCached` path.
    pub fn set_mark_region_caching(&self, callback: MarkRegionCachingFn) {
        *self.mark_region_caching.lock().unwrap() = Some(callback);
    }

    pub fn set_invalidate_region(&self, callback: InvalidateRegionFn) {
        *self.invalidate_region.lock().unwrap() = Some(callback);
    }

    /// Allocate `size` bytes of device-virtual address space. Returns a
    /// device address. Aligned up to `SMMU_PAGE_SIZE` and reserved via a
    /// bump allocator.
    ///
    /// Port of `Core::DeviceMemoryManager<MaxwellDeviceTraits>::Allocate`
    /// (the bump-pointer fast path; upstream also has a small-page
    /// allocator backed by free lists that we omit until needed).
    pub fn smmu_allocate(&self, size: usize) -> DAddr {
        let aligned_size = ((size as u64) + SMMU_PAGE_SIZE - 1) & !(SMMU_PAGE_SIZE - 1);
        self.smmu_next.fetch_add(aligned_size, Ordering::Relaxed)
    }

    /// Install an SMMU page-table mapping: for each page in
    /// `[d_address, d_address + size)`, record the corresponding host
    /// pointer so later `smmu_get_host_ptr` calls can translate device
    /// addresses back to host memory.
    ///
    /// Port of `Core::DeviceMemoryManager<Traits>::Map`. `host_ptr` is the
    /// start of the host backing for the `size`-byte region — typically
    /// obtained from `core::Memory::get_pointer(guest_vaddr)`.
    ///
    /// # Safety
    /// `host_ptr` must remain valid for the lifetime of this mapping, and
    /// must point to at least `size` bytes of accessible memory. Caller
    /// (typically `nvmap::pin_handle`) must ensure unmapping clears the
    /// page table entries before the host backing is freed.
    pub fn smmu_map(&self, d_address: DAddr, host_ptr: *const u8, size: usize) {
        if host_ptr.is_null() || size == 0 {
            return;
        }
        let start_page = d_address >> SMMU_PAGE_BITS;
        let end_page = (d_address + size as u64 + SMMU_PAGE_SIZE - 1) >> SMMU_PAGE_BITS;
        let host_base = host_ptr as usize;
        let d_base = start_page << SMMU_PAGE_BITS;
        let mut table = self.smmu_page_table.lock().unwrap();
        for page in start_page..end_page {
            let page_offset = (page << SMMU_PAGE_BITS) - d_base;
            table.insert(page, host_base + page_offset as usize);
        }
    }

    /// Look up the host pointer for a device address via the SMMU page
    /// table. Returns `None` if no mapping exists.
    ///
    /// Port of the page-walk inside upstream
    /// `Core::DeviceMemoryManager<Traits>::ReadBlockUnsafe`.
    pub fn smmu_get_host_ptr(&self, d_address: DAddr) -> Option<*const u8> {
        let page = d_address >> SMMU_PAGE_BITS;
        let page_offset = (d_address & (SMMU_PAGE_SIZE - 1)) as usize;
        let table = self.smmu_page_table.lock().unwrap();
        table
            .get(&page)
            .map(|&base| (base + page_offset) as *const u8)
    }

    /// Read `output.len()` bytes from a device address into `output`,
    /// walking the SMMU page table page-by-page so multi-page reads
    /// resolve correctly. Returns `true` if every page was mapped.
    ///
    /// Port of upstream `Core::DeviceMemoryManager<Traits>::ReadBlockUnsafe`.
    pub fn smmu_read_block(&self, d_address: DAddr, output: &mut [u8]) -> bool {
        if output.is_empty() {
            return true;
        }
        let table = self.smmu_page_table.lock().unwrap();
        let mut remaining = output.len();
        let mut out_off = 0usize;
        let mut current = d_address;
        while remaining > 0 {
            let page = current >> SMMU_PAGE_BITS;
            let page_off = (current & (SMMU_PAGE_SIZE - 1)) as usize;
            let bytes_in_page = (SMMU_PAGE_SIZE as usize - page_off).min(remaining);
            match table.get(&page) {
                Some(&base) => unsafe {
                    let src = (base + page_off) as *const u8;
                    std::ptr::copy_nonoverlapping(
                        src,
                        output.as_mut_ptr().add(out_off),
                        bytes_in_page,
                    );
                },
                None => return false,
            }
            out_off += bytes_in_page;
            remaining -= bytes_in_page;
            current += bytes_in_page as u64;
        }
        true
    }

    /// Write `data.len()` bytes from `data` to a device address, walking
    /// the SMMU page table page-by-page. Returns `true` if every page was
    /// mapped.
    ///
    /// Port of upstream `Core::DeviceMemoryManager<Traits>::WriteBlock` for the
    /// current SMMU-backed Host1x write users: copy through mapped pages, then
    /// invalidate the written device range through the installed GPU callback.
    pub fn smmu_write_block(&self, d_address: DAddr, data: &[u8]) -> bool {
        if data.is_empty() {
            return true;
        }
        let table = self.smmu_page_table.lock().unwrap();
        let mut remaining = data.len();
        let mut in_off = 0usize;
        let mut current = d_address;
        while remaining > 0 {
            let page = current >> SMMU_PAGE_BITS;
            let page_off = (current & (SMMU_PAGE_SIZE - 1)) as usize;
            let bytes_in_page = (SMMU_PAGE_SIZE as usize - page_off).min(remaining);
            match table.get(&page) {
                Some(&base) => unsafe {
                    let dst = (base + page_off) as *mut u8;
                    std::ptr::copy_nonoverlapping(data.as_ptr().add(in_off), dst, bytes_in_page);
                },
                None => return false,
            }
            in_off += bytes_in_page;
            remaining -= bytes_in_page;
            current += bytes_in_page as u64;
        }
        drop(table);
        if let Some(callback) = self.invalidate_region.lock().unwrap().as_ref() {
            callback(d_address, data.len());
        }
        true
    }

    /// Port of upstream
    /// `Core::DeviceMemoryManager<Traits>::UpdatePagesCachedCount`
    /// (`core/device_memory_manager.inc:510-587`).
    ///
    /// Per-page reference counting: each page's `count.fetch_add(delta)`
    /// runs once, with batched `MarkRegionCaching(begin, bytes, caching)`
    /// calls when contiguous runs of pages transition from `0`↔non-zero.
    ///
    /// **Port simplifications vs upstream**:
    /// - No `ScopedRangeLock`: we take a single `Mutex` over the whole
    ///   ref-count table for the duration of the call. Upstream uses a
    ///   fine-grained range lock; ruzu has no equivalent yet. This is
    ///   correctness-preserving (still atomic), just lower throughput.
    /// - No `ExtractCPUBacking`: ruzu doesn't yet have an ASID /
    ///   per-process CPU-backing map. Passing the device address
    ///   directly to `MarkRegionCaching` is enough for ruzu's current
    ///   single-process callers (shader_cache, buffer_cache,
    ///   texture_cache).
    /// - 8-bit saturating-on-overflow counter: matches upstream's
    ///   `CounterType = u8` semantics but uses `wrapping_add(delta as u8)`.
    ///   Upstream comment: "Assume delta is either -1 or 1" — same here.
    pub fn update_pages_cached_count(&self, addr: DAddr, size: usize, delta: i32) {
        record_update_cached_stage(0);
        if size == 0 {
            return;
        }

        let page_begin = addr >> PAGE_BITS;
        let page_end = (addr + size as u64 + PAGE_SIZE - 1) >> PAGE_BITS;
        record_update_cached_stage(1);

        // Pending-batch tracking for grouped MarkRegionCaching calls.
        // `uncache_*` accumulates pages that just transitioned to count==0.
        // `cache_*` accumulates pages that just transitioned to count==1 (and delta>0).
        let mut uncache_begin: u64 = 0;
        let mut uncache_bytes: u64 = 0;
        let mut cache_begin: u64 = 0;
        let mut cache_bytes: u64 = 0;

        let mut callbacks: Vec<(u64, usize, bool)> = Vec::new();
        record_update_cached_stage(2);
        let mut counts = self.cached_pages.lock().unwrap();
        record_update_cached_stage(3);

        // Helper to flush pending batches. Closes over locals via
        // explicit references so we can call it from inside the loop.
        fn flush_callbacks(
            callbacks: &mut Vec<(u64, usize, bool)>,
            uncache_begin: &mut u64,
            uncache_bytes: &mut u64,
            cache_begin: &mut u64,
            cache_bytes: &mut u64,
        ) {
            if *uncache_bytes > 0 {
                callbacks.push((*uncache_begin << PAGE_BITS, *uncache_bytes as usize, false));
                *uncache_bytes = 0;
            }
            if *cache_bytes > 0 {
                callbacks.push((*cache_begin << PAGE_BITS, *cache_bytes as usize, true));
                *cache_bytes = 0;
            }
        }

        // Upstream tracks `old_vpage` to detect non-contiguous CPU vaddr
        // ranges (which force a batch flush). Without ASID translation
        // we use the device page directly — by construction the loop is
        // already contiguous in device space, so the discontinuity check
        // never fires. The variable is kept so the structure mirrors
        // upstream and future ASID-aware code drops in cleanly.
        let mut old_page: u64 = page_begin.wrapping_sub(1);

        for page in page_begin..page_end {
            // Discontinuity detection (no-op for now; see comment above).
            if page != old_page.wrapping_add(1) {
                flush_callbacks(
                    &mut callbacks,
                    &mut uncache_begin,
                    &mut uncache_bytes,
                    &mut cache_begin,
                    &mut cache_bytes,
                );
            }
            old_page = page;

            let entry = counts.entry(page).or_insert(0);
            // delta is typically ±1; `wrapping_add` matches upstream's
            // `count.fetch_add(static_cast<CounterType>(delta))` on `u8`.
            let new_count = entry.wrapping_add(delta as u8);
            *entry = new_count;

            // Transition to 0 → schedule uncache.
            if new_count == 0 {
                if uncache_bytes == 0 {
                    uncache_begin = page;
                }
                uncache_bytes += PAGE_SIZE;
            } else if uncache_bytes > 0 {
                // Non-zero count interrupts an in-progress uncache batch.
                callbacks.push((uncache_begin << PAGE_BITS, uncache_bytes as usize, false));
                uncache_bytes = 0;
            }

            // First-time cache (0→1 with delta>0) → schedule cache.
            if new_count == 1 && delta > 0 {
                if cache_bytes == 0 {
                    cache_begin = page;
                }
                cache_bytes += PAGE_SIZE;
            } else if cache_bytes > 0 {
                // Anything else interrupts the cache batch.
                callbacks.push((cache_begin << PAGE_BITS, cache_bytes as usize, true));
                cache_bytes = 0;
            }
        }

        flush_callbacks(
            &mut callbacks,
            &mut uncache_begin,
            &mut uncache_bytes,
            &mut cache_begin,
            &mut cache_bytes,
        );
        record_update_cached_stage(4);
        drop(counts);
        record_update_cached_stage(5);

        if callbacks.is_empty() {
            record_update_cached_stage(9);
            return;
        }
        record_update_cached_stage(6);
        let callback_guard = self.mark_region_caching.lock().unwrap();
        record_update_cached_stage(7);
        if let Some(callback) = callback_guard.as_ref() {
            for (address, size, caching) in callbacks {
                callback(address, size, caching);
            }
        }
        record_update_cached_stage(8);
        record_update_cached_stage(9);
    }
}

/// Port of `Tegra::MaxwellDeviceMethods`.
///
/// Provides the `mark_region_caching` callback used by the device memory manager.
pub struct MaxwellDeviceMethods;

impl MaxwellDeviceMethods {
    /// Mark a region of device memory as cacheable or non-cacheable.
    ///
    /// Stubbed — requires platform-specific cache invalidation (e.g., CacheInvalidate on
    /// Tegra). Upstream: Tegra::MaxwellDeviceMethods::MarkRegionCaching().
    pub fn mark_region_caching(_address: u64, _size: usize, _caching: bool) {
        log::warn!("MaxwellDeviceMethods::mark_region_caching: not yet implemented");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    /// Helper: a `MarkRegionCaching` callback that records every call so
    /// tests can assert the upstream batching semantics.
    fn recorder() -> (MarkRegionCachingFn, Arc<Mutex<Vec<(u64, usize, bool)>>>) {
        let log = Arc::new(Mutex::new(Vec::new()));
        let log_clone = Arc::clone(&log);
        let cb: MarkRegionCachingFn = Box::new(move |addr, size, caching| {
            log_clone.lock().unwrap().push((addr, size, caching))
        });
        (cb, log)
    }

    #[test]
    fn single_page_cache_then_uncache() {
        let mgr = MaxwellDeviceMemoryManager::default();
        let (cb, log) = recorder();
        mgr.set_mark_region_caching(cb);

        // 0→1 within one page → one cache call.
        mgr.update_pages_cached_count(0x1000, 0x100, 1);
        // 1→0 same page → one uncache call.
        mgr.update_pages_cached_count(0x1000, 0x100, -1);

        let calls = log.lock().unwrap();
        assert_eq!(
            *calls,
            vec![(0x1000, 0x1000, true), (0x1000, 0x1000, false)]
        );
    }

    #[test]
    fn contiguous_pages_batched_into_one_call() {
        let mgr = MaxwellDeviceMemoryManager::default();
        let (cb, log) = recorder();
        mgr.set_mark_region_caching(cb);

        // 4 pages, 0→1 each → single batched cache call covering all 4.
        mgr.update_pages_cached_count(0x4000, 4 * 0x1000, 1);

        let calls = log.lock().unwrap();
        assert_eq!(*calls, vec![(0x4000, 4 * 0x1000, true)]);
    }

    #[test]
    fn refcount_stays_cached_until_zero() {
        let mgr = MaxwellDeviceMemoryManager::default();
        let (cb, log) = recorder();
        mgr.set_mark_region_caching(cb);

        mgr.update_pages_cached_count(0x2000, 0x1000, 1); // 0→1: cache
        mgr.update_pages_cached_count(0x2000, 0x1000, 1); // 1→2: no callback
        mgr.update_pages_cached_count(0x2000, 0x1000, -1); // 2→1: no callback
        mgr.update_pages_cached_count(0x2000, 0x1000, -1); // 1→0: uncache

        let calls = log.lock().unwrap();
        assert_eq!(
            *calls,
            vec![(0x2000, 0x1000, true), (0x2000, 0x1000, false)]
        );
    }

    #[test]
    fn no_callback_still_tracks_counts() {
        // When no `MarkRegionCaching` callback is installed, the ref
        // counts are still maintained — matches upstream's
        // `if (memory_device_inter != nullptr)` guard.
        let mgr = MaxwellDeviceMemoryManager::default();
        mgr.update_pages_cached_count(0x3000, 0x2000, 1);
        let counts = mgr.cached_pages.lock().unwrap();
        assert_eq!(counts.get(&3), Some(&1));
        assert_eq!(counts.get(&4), Some(&1));
    }

    #[test]
    fn zero_size_is_noop() {
        let mgr = MaxwellDeviceMemoryManager::default();
        let (cb, log) = recorder();
        mgr.set_mark_region_caching(cb);
        mgr.update_pages_cached_count(0x1000, 0, 1);
        assert!(log.lock().unwrap().is_empty());
    }
}
