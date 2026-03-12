// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `video_core/buffer_cache/buffer_cache.h` and `buffer_cache.cpp`
//!
//! Concrete buffer cache implementation. This file contains the method bodies
//! for the `BufferCache<P>` template class.
//!
//! The C++ version splits the template definition across `buffer_cache_base.h`
//! (class declaration) and `buffer_cache.h` (template method implementations),
//! plus `buffer_cache.cpp` (explicit template instantiation and profiling macros).
//! In Rust, the struct definition lives in `buffer_cache_base.rs` and the
//! method implementations live here.

use std::collections::VecDeque;

use common::div_ceil::div_ceil;
use common::slot_vector::{SlotId, SlotVector};
use common::types::VAddr;
use parking_lot::Mutex;

use super::buffer_base::BufferBase;
use super::buffer_cache_base::*;
use super::memory_tracker_base::MemoryTrackerBase;
use super::word_manager::DeviceTracker;

// ---------------------------------------------------------------------------
// Cache-level constants (from BufferCache<P> private section)
// ---------------------------------------------------------------------------

/// Page size for caching purposes (unrelated to CPU page size).
const CACHING_PAGEBITS: u32 = 16;

/// Caching page size in bytes.
const CACHING_PAGESIZE: u64 = 1u64 << CACHING_PAGEBITS;

/// Default expected device memory threshold (512 MiB).
const DEFAULT_EXPECTED_MEMORY: u64 = 512 * 1024 * 1024;

/// Default critical device memory threshold (1 GiB).
const DEFAULT_CRITICAL_MEMORY: u64 = 1024 * 1024 * 1024;

/// Target memory threshold (4 GiB).
const TARGET_THRESHOLD: u64 = 4 * 1024 * 1024 * 1024;

/// Debug flag: when true, GPU->CPU downloads are disabled.
const DISABLE_DOWNLOADS: bool = true;

/// Number of page-table entries: covers 2^34 bytes / CACHING_PAGESIZE.
const PAGE_TABLE_SIZE: usize = (1u64 << 34) as usize >> CACHING_PAGEBITS;

// ---------------------------------------------------------------------------
// BufferCache<P>
// ---------------------------------------------------------------------------

/// The main buffer cache.
///
/// Corresponds to the C++ `BufferCache<P>` template. The generic parameter
/// `P` is the backend policy (see `BufferCacheParams` trait).
///
/// All complex method bodies use `todo!()` as this is a structural port.
/// Behavioral completion will follow per CLAUDE.md.
pub struct BufferCache<P: BufferCacheParams, DT: DeviceTracker> {
    /// Recursive mutex for external synchronization.
    pub mutex: Mutex<()>,

    // -- Channel state (upstream inherits from ChannelSetupCaches) --
    pub channel_state: Option<Box<BufferCacheChannelInfo>>,

    // -- Slot storage --
    slot_buffers: SlotVector<BufferBase>,

    // -- Page table: maps device page -> BufferId --
    page_table: Vec<BufferId>,

    // -- Draw indirect state --
    last_index_count: u32,

    // -- Memory tracker --
    memory_tracker: MemoryTrackerBase<DT>,

    // -- GPU-modified range tracking --
    // TODO: use common::range_sets::RangeSet once ported
    uncommitted_gpu_modified_ranges: Vec<(VAddr, VAddr)>,
    gpu_modified_ranges: Vec<(VAddr, VAddr)>,
    committed_gpu_modified_ranges: VecDeque<Vec<(VAddr, VAddr)>>,

    // -- Async buffer downloads --
    pending_downloads: VecDeque<Vec<BufferCopy>>,

    // -- Immediate buffer --
    immediate_buffer_capacity: usize,
    immediate_buffer_alloc: Vec<u8>,

    // -- LRU / GC state --
    frame_tick: u64,
    total_used_memory: u64,
    minimum_memory: u64,
    critical_memory: u64,
    inline_buffer_id: BufferId,

    // -- Scratch buffer --
    tmp_buffer: Vec<u8>,

    /// Marker for the params type.
    _params: std::marker::PhantomData<P>,
}

impl<P: BufferCacheParams, DT: DeviceTracker> BufferCache<P, DT> {
    /// Create a new buffer cache.
    pub fn new(device_tracker: &DT) -> Self {
        let mut slot_buffers = SlotVector::new();
        // Ensure the first slot is used for the null buffer
        let _null_id = slot_buffers.insert(BufferBase::null(super::buffer_base::NullBufferParams));

        Self {
            mutex: Mutex::new(()),
            channel_state: None,
            slot_buffers,
            page_table: vec![SlotId::invalid(); PAGE_TABLE_SIZE],
            last_index_count: 0,
            memory_tracker: MemoryTrackerBase::new(device_tracker),
            uncommitted_gpu_modified_ranges: Vec::new(),
            gpu_modified_ranges: Vec::new(),
            committed_gpu_modified_ranges: VecDeque::new(),
            pending_downloads: VecDeque::new(),
            immediate_buffer_capacity: 0,
            immediate_buffer_alloc: Vec::new(),
            frame_tick: 0,
            total_used_memory: 0,
            minimum_memory: DEFAULT_EXPECTED_MEMORY,
            critical_memory: DEFAULT_CRITICAL_MEMORY,
            inline_buffer_id: NULL_BUFFER_ID,
            tmp_buffer: Vec::new(),
            _params: std::marker::PhantomData,
        }
    }

    // -----------------------------------------------------------------------
    // Public API — frame lifecycle
    // -----------------------------------------------------------------------

    /// Advance one frame: run GC, update cache statistics, tick delayed destruction.
    pub fn tick_frame(&mut self) {
        todo!()
    }

    // -----------------------------------------------------------------------
    // Public API — memory writes
    // -----------------------------------------------------------------------

    /// Notify the cache that a CPU write happened at `[device_addr, device_addr+size)`.
    pub fn write_memory(&mut self, device_addr: VAddr, size: u64) {
        todo!()
    }

    /// Notify the cache about a cached (deferred) CPU write.
    pub fn cached_write_memory(&mut self, device_addr: VAddr, size: u64) {
        todo!()
    }

    /// Called when a CPU write is detected. Returns true if the caller must
    /// flush GPU-modified data first.
    pub fn on_cpu_write(&mut self, device_addr: VAddr, size: u64) -> bool {
        todo!()
    }

    /// Download GPU-modified memory back to the CPU for the given range.
    pub fn download_memory(&mut self, device_addr: VAddr, size: u64) {
        todo!()
    }

    /// Get the flush area for a device address range.
    pub fn get_flush_area(
        &mut self,
        _device_addr: VAddr,
        _size: u64,
    ) -> Option<RasterizerDownloadArea> {
        todo!()
    }

    /// Inline a small memory write directly into the buffer.
    pub fn inline_memory(
        &mut self,
        _dest_address: VAddr,
        _copy_size: usize,
        _inlined_buffer: &[u8],
    ) -> bool {
        todo!()
    }

    // -----------------------------------------------------------------------
    // Public API — buffer binding (graphics)
    // -----------------------------------------------------------------------

    /// Bind a graphics uniform buffer.
    pub fn bind_graphics_uniform_buffer(
        &mut self,
        _stage: usize,
        _index: u32,
        _gpu_addr: u64,
        _size: u32,
    ) {
        todo!()
    }

    /// Disable a graphics uniform buffer.
    pub fn disable_graphics_uniform_buffer(&mut self, _stage: usize, _index: u32) {
        todo!()
    }

    /// Update all graphics buffer bindings.
    pub fn update_graphics_buffers(&mut self, _is_indexed: bool) {
        todo!()
    }

    /// Update all compute buffer bindings.
    pub fn update_compute_buffers(&mut self) {
        todo!()
    }

    /// Bind host geometry buffers (index + vertex).
    pub fn bind_host_geometry_buffers(&mut self, _is_indexed: bool) {
        todo!()
    }

    /// Bind host stage buffers.
    pub fn bind_host_stage_buffers(&mut self, _stage: usize) {
        todo!()
    }

    /// Bind host compute buffers.
    pub fn bind_host_compute_buffers(&mut self) {
        todo!()
    }

    /// Set the uniform buffer state for graphics stages.
    pub fn set_uniform_buffers_state(
        &mut self,
        _mask: &[u32; NUM_STAGES as usize],
        _sizes: &UniformBufferSizes,
    ) {
        todo!()
    }

    /// Set the uniform buffer state for compute.
    pub fn set_compute_uniform_buffer_state(
        &mut self,
        _mask: u32,
        _sizes: &ComputeUniformBufferSizes,
    ) {
        todo!()
    }

    // -----------------------------------------------------------------------
    // Public API — storage buffers
    // -----------------------------------------------------------------------

    /// Unbind all graphics storage buffers for a stage.
    pub fn unbind_graphics_storage_buffers(&mut self, _stage: usize) {
        todo!()
    }

    /// Bind a graphics storage buffer.
    pub fn bind_graphics_storage_buffer(
        &mut self,
        _stage: usize,
        _ssbo_index: usize,
        _cbuf_index: u32,
        _cbuf_offset: u32,
        _is_written: bool,
    ) {
        todo!()
    }

    /// Unbind all compute storage buffers.
    pub fn unbind_compute_storage_buffers(&mut self) {
        todo!()
    }

    /// Bind a compute storage buffer.
    pub fn bind_compute_storage_buffer(
        &mut self,
        _ssbo_index: usize,
        _cbuf_index: u32,
        _cbuf_offset: u32,
        _is_written: bool,
    ) {
        todo!()
    }

    // -----------------------------------------------------------------------
    // Public API — texture buffers
    // -----------------------------------------------------------------------

    /// Unbind all graphics texture buffers for a stage.
    pub fn unbind_graphics_texture_buffers(&mut self, _stage: usize) {
        todo!()
    }

    /// Bind a graphics texture buffer.
    pub fn bind_graphics_texture_buffer(
        &mut self,
        _stage: usize,
        _tbo_index: usize,
        _gpu_addr: u64,
        _size: u32,
        _format: u32,
        _is_written: bool,
        _is_image: bool,
    ) {
        todo!()
    }

    /// Unbind all compute texture buffers.
    pub fn unbind_compute_texture_buffers(&mut self) {
        todo!()
    }

    /// Bind a compute texture buffer.
    pub fn bind_compute_texture_buffer(
        &mut self,
        _tbo_index: usize,
        _gpu_addr: u64,
        _size: u32,
        _format: u32,
        _is_written: bool,
        _is_image: bool,
    ) {
        todo!()
    }

    // -----------------------------------------------------------------------
    // Public API — obtain buffers
    // -----------------------------------------------------------------------

    /// Obtain a buffer by GPU virtual address.
    ///
    /// Returns `(buffer_id, offset)` within the buffer.
    pub fn obtain_buffer(
        &mut self,
        _gpu_addr: u64,
        _size: u32,
        _sync_info: ObtainBufferSynchronize,
        _post_op: ObtainBufferOperation,
    ) -> (BufferId, u32) {
        todo!()
    }

    /// Obtain a buffer by CPU/device address.
    pub fn obtain_cpu_buffer(
        &mut self,
        _device_addr: VAddr,
        _size: u32,
        _sync_info: ObtainBufferSynchronize,
        _post_op: ObtainBufferOperation,
    ) -> (BufferId, u32) {
        todo!()
    }

    // -----------------------------------------------------------------------
    // Public API — flush / commit
    // -----------------------------------------------------------------------

    /// Flush all cached CPU writes.
    pub fn flush_cached_writes(&mut self) {
        todo!()
    }

    /// Return true when there are uncommitted buffers to be downloaded.
    pub fn has_uncommitted_flushes(&self) -> bool {
        !self.uncommitted_gpu_modified_ranges.is_empty()
    }

    /// Accumulate current uncommitted ranges into committed.
    pub fn accumulate_flushes(&mut self) {
        todo!()
    }

    /// Return true when the caller should wait for async flushes.
    pub fn should_wait_async_flushes(&self) -> bool {
        !self.committed_gpu_modified_ranges.is_empty()
    }

    /// Commit asynchronous downloads.
    pub fn commit_async_flushes(&mut self) {
        todo!()
    }

    /// Commit asynchronous downloads (high priority).
    pub fn commit_async_flushes_high(&mut self) {
        todo!()
    }

    /// Pop completed asynchronous downloads.
    pub fn pop_async_flushes(&mut self) {
        todo!()
    }

    /// Pop completed asynchronous buffers.
    pub fn pop_async_buffers(&mut self) {
        todo!()
    }

    // -----------------------------------------------------------------------
    // Public API — DMA
    // -----------------------------------------------------------------------

    /// Perform a DMA copy between two GPU virtual addresses.
    pub fn dma_copy(&mut self, _src_address: u64, _dest_address: u64, _amount: u64) -> bool {
        todo!()
    }

    /// Perform a DMA clear.
    pub fn dma_clear(&mut self, _src_address: u64, _amount: u64, _value: u32) -> bool {
        todo!()
    }

    // -----------------------------------------------------------------------
    // Public API — region queries
    // -----------------------------------------------------------------------

    /// Return true when a device region is GPU-modified.
    pub fn is_region_gpu_modified(&self, _addr: VAddr, _size: usize) -> bool {
        todo!()
    }

    /// Return true when a region is registered in the cache.
    pub fn is_region_registered(&self, _addr: VAddr, _size: usize) -> bool {
        todo!()
    }

    /// Return true when a device region is CPU-modified.
    pub fn is_region_cpu_modified(&self, _addr: VAddr, _size: usize) -> bool {
        todo!()
    }

    // -----------------------------------------------------------------------
    // Public API — draw indirect
    // -----------------------------------------------------------------------

    /// Get the draw indirect count buffer.
    pub fn get_draw_indirect_count(&mut self) -> (BufferId, u32) {
        todo!()
    }

    /// Get the draw indirect buffer.
    pub fn get_draw_indirect_buffer(&mut self) -> (BufferId, u32) {
        todo!()
    }

    // -----------------------------------------------------------------------
    // Public API — buffer operations retry loop
    // -----------------------------------------------------------------------

    /// Execute `func` in a retry loop: if any buffers are deleted during the
    /// operation, re-run it.
    pub fn buffer_operations<F>(&mut self, mut func: F)
    where
        F: FnMut(&mut Self),
    {
        loop {
            if let Some(ref mut cs) = self.channel_state {
                cs.has_deleted_buffers = false;
            }
            func(self);
            if let Some(ref cs) = self.channel_state {
                if !cs.has_deleted_buffers {
                    break;
                }
            } else {
                break;
            }
        }
    }

    // -----------------------------------------------------------------------
    // Private helpers — static
    // -----------------------------------------------------------------------

    /// Call `func` for each set bit in `enabled_mask`.
    fn for_each_enabled_bit<F>(mut enabled_mask: u32, mut func: F)
    where
        F: FnMut(u32),
    {
        let mut index: u32 = 0;
        while enabled_mask != 0 {
            let disabled_bits = enabled_mask.trailing_zeros();
            index += disabled_bits;
            enabled_mask >>= disabled_bits;
            func(index);
            index += 1;
            enabled_mask >>= 1;
        }
    }

    /// Iterate over all buffers overlapping `[device_addr, device_addr+size)`.
    fn for_each_buffer_in_range<F>(&mut self, device_addr: VAddr, size: u64, mut func: F)
    where
        F: FnMut(BufferId, &mut BufferBase),
    {
        let page_end = div_ceil(device_addr + size, CACHING_PAGESIZE);
        let mut page = device_addr >> CACHING_PAGEBITS;
        while page < page_end {
            let buffer_id = self.page_table[page as usize];
            if !buffer_id.is_valid() {
                page += 1;
                continue;
            }
            let buffer = &mut self.slot_buffers[buffer_id];
            func(buffer_id, buffer);
            let end_addr = buffer.cpu_addr() + buffer.size_bytes() as u64;
            page = div_ceil(end_addr, CACHING_PAGESIZE);
        }
    }

    /// Check if a range fits within a single device page.
    fn is_range_granular(device_addr: VAddr, size: usize) -> bool {
        let device_pagemask = 4096u64 - 1; // Core::DEVICE_PAGEMASK
        (device_addr & !device_pagemask) == ((device_addr + size as u64) & !device_pagemask)
    }

    // -----------------------------------------------------------------------
    // Private helpers — operations (all todo!() for structural port)
    // -----------------------------------------------------------------------

    fn run_garbage_collector(&mut self) {
        todo!()
    }

    fn bind_host_index_buffer(&mut self) {
        todo!()
    }

    fn bind_host_vertex_buffers(&mut self) {
        todo!()
    }

    fn bind_host_draw_indirect_buffers(&mut self) {
        todo!()
    }

    fn bind_host_graphics_uniform_buffers(&mut self, _stage: usize) {
        todo!()
    }

    fn bind_host_graphics_uniform_buffer(
        &mut self,
        _stage: usize,
        _index: u32,
        _binding_index: u32,
        _needs_bind: bool,
    ) {
        todo!()
    }

    fn bind_host_graphics_storage_buffers(&mut self, _stage: usize) {
        todo!()
    }

    fn bind_host_graphics_texture_buffers(&mut self, _stage: usize) {
        todo!()
    }

    fn bind_host_transform_feedback_buffers(&mut self) {
        todo!()
    }

    fn bind_host_compute_uniform_buffers(&mut self) {
        todo!()
    }

    fn bind_host_compute_storage_buffers(&mut self) {
        todo!()
    }

    fn bind_host_compute_texture_buffers(&mut self) {
        todo!()
    }

    fn do_update_graphics_buffers(&mut self, _is_indexed: bool) {
        todo!()
    }

    fn do_update_compute_buffers(&mut self) {
        todo!()
    }

    fn update_index_buffer(&mut self) {
        todo!()
    }

    fn update_vertex_buffers(&mut self) {
        todo!()
    }

    fn update_vertex_buffer(&mut self, _index: u32) {
        todo!()
    }

    fn update_draw_indirect(&mut self) {
        todo!()
    }

    fn update_uniform_buffers(&mut self, _stage: usize) {
        todo!()
    }

    fn update_storage_buffers(&mut self, _stage: usize) {
        todo!()
    }

    fn update_texture_buffers(&mut self, _stage: usize) {
        todo!()
    }

    fn update_transform_feedback_buffers(&mut self) {
        todo!()
    }

    fn update_transform_feedback_buffer(&mut self, _index: u32) {
        todo!()
    }

    fn update_compute_uniform_buffers(&mut self) {
        todo!()
    }

    fn update_compute_storage_buffers(&mut self) {
        todo!()
    }

    fn update_compute_texture_buffers(&mut self) {
        todo!()
    }

    fn mark_written_buffer(&mut self, _buffer_id: BufferId, _device_addr: VAddr, _size: u32) {
        todo!()
    }

    fn find_buffer(&mut self, _device_addr: VAddr, _size: u32) -> BufferId {
        todo!()
    }

    fn resolve_overlaps(&mut self, _device_addr: VAddr, _wanted_size: u32) -> OverlapResult {
        todo!()
    }

    fn join_overlap(
        &mut self,
        _new_buffer_id: BufferId,
        _overlap_id: BufferId,
        _accumulate_stream_score: bool,
    ) {
        todo!()
    }

    fn create_buffer(&mut self, _device_addr: VAddr, _wanted_size: u32) -> BufferId {
        todo!()
    }

    fn register(&mut self, _buffer_id: BufferId) {
        todo!()
    }

    fn unregister(&mut self, _buffer_id: BufferId) {
        todo!()
    }

    fn change_register(&mut self, _buffer_id: BufferId, _insert: bool) {
        todo!()
    }

    fn touch_buffer(&mut self, _buffer_id: BufferId) {
        // Touches the buffer in the LRU cache
        todo!()
    }

    fn synchronize_buffer(
        &mut self,
        _buffer_id: BufferId,
        _device_addr: VAddr,
        _size: u32,
    ) -> bool {
        todo!()
    }

    fn upload_memory(
        &mut self,
        _buffer_id: BufferId,
        _total_size_bytes: u64,
        _largest_copy: u64,
        _copies: &mut [BufferCopy],
    ) {
        todo!()
    }

    fn immediate_upload_memory(
        &mut self,
        _buffer_id: BufferId,
        _largest_copy: u64,
        _copies: &[BufferCopy],
    ) {
        todo!()
    }

    fn mapped_upload_memory(
        &mut self,
        _buffer_id: BufferId,
        _total_size_bytes: u64,
        _copies: &mut [BufferCopy],
    ) {
        todo!()
    }

    fn download_buffer_memory(&mut self, _buffer_id: BufferId) {
        todo!()
    }

    fn download_buffer_memory_range(
        &mut self,
        _buffer_id: BufferId,
        _device_addr: VAddr,
        _size: u64,
    ) {
        todo!()
    }

    fn delete_buffer(&mut self, _buffer_id: BufferId, _do_not_mark: bool) {
        todo!()
    }

    fn storage_buffer_binding(
        &self,
        _ssbo_addr: u64,
        _cbuf_index: u32,
        _is_written: bool,
    ) -> Binding {
        todo!()
    }

    fn get_texture_buffer_binding(
        &mut self,
        _gpu_addr: u64,
        _size: u32,
        _format: u32,
    ) -> TextureBufferBinding {
        todo!()
    }

    fn immediate_buffer_with_data(&mut self, _device_addr: VAddr, _size: usize) -> &[u8] {
        todo!()
    }

    fn immediate_buffer(&mut self, _wanted_capacity: usize) -> &mut [u8] {
        todo!()
    }

    fn has_fast_uniform_buffer_bound(&self, _stage: usize, _binding_index: u32) -> bool {
        todo!()
    }

    fn clear_download(&mut self, _base_addr: VAddr, _size: u64) {
        todo!()
    }

    fn inline_memory_implementation(
        &mut self,
        _dest_address: VAddr,
        _copy_size: usize,
        _inlined_buffer: &[u8],
    ) {
        todo!()
    }
}

// ---------------------------------------------------------------------------
// RasterizerDownloadArea — return type for GetFlushArea
// ---------------------------------------------------------------------------

/// Describes an area that must be flushed from GPU to CPU.
#[derive(Debug, Clone, Copy)]
pub struct RasterizerDownloadArea {
    pub start_address: VAddr,
    pub end_address: VAddr,
    pub preemtive: bool,
}

#[cfg(test)]
mod tests {
    use super::super::word_manager::DeviceTracker;
    use super::*;

    struct DummyTracker;
    impl DeviceTracker for DummyTracker {
        fn update_pages_cached_count(&self, _addr: VAddr, _size: u64, _delta: i32) {}
    }

    struct TestParams;
    impl BufferCacheParams for TestParams {
        const IS_OPENGL: bool = false;
        const HAS_PERSISTENT_UNIFORM_BUFFER_BINDINGS: bool = false;
        const HAS_FULL_INDEX_AND_PRIMITIVE_SUPPORT: bool = true;
        const NEEDS_BIND_UNIFORM_INDEX: bool = false;
        const NEEDS_BIND_STORAGE_INDEX: bool = false;
        const USE_MEMORY_MAPS: bool = false;
        const SEPARATE_IMAGE_BUFFER_BINDINGS: bool = false;
        const USE_MEMORY_MAPS_FOR_UPLOADS: bool = false;
    }

    #[test]
    fn test_buffer_cache_construction() {
        let tracker = DummyTracker;
        let cache = BufferCache::<TestParams, DummyTracker>::new(&tracker);
        assert!(!cache.has_uncommitted_flushes());
        assert!(!cache.should_wait_async_flushes());
    }

    #[test]
    fn test_for_each_enabled_bit() {
        let mut bits = Vec::new();
        BufferCache::<TestParams, DummyTracker>::for_each_enabled_bit(0b1010_0101, |idx| {
            bits.push(idx);
        });
        assert_eq!(bits, vec![0, 2, 5, 7]);
    }

    #[test]
    fn test_is_range_granular() {
        // Same page
        assert!(BufferCache::<TestParams, DummyTracker>::is_range_granular(
            0x1000, 0x100
        ));
        // Cross page
        assert!(!BufferCache::<TestParams, DummyTracker>::is_range_granular(
            0x1F00, 0x200
        ));
    }
}
