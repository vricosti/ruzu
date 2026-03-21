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
use common::lru_cache::LeastRecentlyUsedCache;
use common::range_sets::RangeSet;
use common::slot_vector::{SlotId, SlotVector};
use common::types::VAddr;
use parking_lot::Mutex;

use crate::delayed_destruction_ring::DelayedDestructionRing;

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

/// Stream score threshold above which a buffer region is treated as a stream buffer.
const STREAM_LEAP_THRESHOLD: i32 = 16;

/// Device page size (4 KiB). Matches `Core::DEVICE_PAGESIZE` upstream.
const DEVICE_PAGESIZE: u64 = 4096;

/// Address space bits used by the Maxwell device memory manager.
///
/// Upstream: `Tegra::MaxwellDeviceMemoryManager::AS_BITS = 34`.
const AS_BITS: u32 = 34;

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
    uncommitted_gpu_modified_ranges: RangeSet,
    gpu_modified_ranges: RangeSet,
    committed_gpu_modified_ranges: VecDeque<RangeSet>,

    // -- Async buffer downloads --
    pending_downloads: VecDeque<Vec<BufferCopy>>,

    // -- Immediate buffer --
    immediate_buffer_capacity: usize,
    immediate_buffer_alloc: Vec<u8>,

    // -- LRU / GC state --
    /// LRU cache tracking buffer access order for garbage collection.
    ///
    /// Upstream: `Common::LeastRecentlyUsedCache<LRUItemParams> lru_cache`
    /// where `LRUItemParams::ObjectType = BufferId` and `LRUItemParams::TickType = u64`.
    /// We use `i64` here because the Rust LRU cache requires `K: Into<i64>` for
    /// its `for_each_item_below` comparison. frame_tick values fit in i64.
    lru_cache: LeastRecentlyUsedCache<BufferId, i64>,

    /// Deferred destruction ring for buffers removed from the cache.
    ///
    /// Upstream: `DelayedDestructionRing<Buffer, 8> delayed_destruction_ring`
    delayed_destruction_ring: DelayedDestructionRing<BufferBase, 8>,

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
            uncommitted_gpu_modified_ranges: RangeSet::new(),
            gpu_modified_ranges: RangeSet::new(),
            committed_gpu_modified_ranges: VecDeque::new(),
            lru_cache: LeastRecentlyUsedCache::new(),
            delayed_destruction_ring: DelayedDestructionRing::new(),
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
    ///
    /// Upstream: `BufferCache<P>::TickFrame`
    ///
    /// NOTE: Parts of this method that require `runtime` (TickFrame, CanReportMemoryUsage,
    /// GetDeviceMemoryUsage) and `delayed_destruction_ring` are stubbed out — those types
    /// are not yet available in this port.
    pub fn tick_frame(&mut self) {
        // Homebrew console apps don't create or bind any channels, so this will be None.
        if self.channel_state.is_none() {
            return;
        }

        // Calculate hits and shots and move hit bits to the right (shift history window).
        // Upstream: std::reduce + std::copy_n to shift history arrays left by one.
        if let Some(ref mut cs) = self.channel_state {
            let hits: u32 = cs.uniform_cache_hits.iter().copied().sum();
            let shots: u32 = cs.uniform_cache_shots.iter().copied().sum();

            // Shift history: copy [0..N-1] into [1..N], then zero slot 0.
            for i in (1..cs.uniform_cache_hits.len()).rev() {
                cs.uniform_cache_hits[i] = cs.uniform_cache_hits[i - 1];
            }
            for i in (1..cs.uniform_cache_shots.len()).rev() {
                cs.uniform_cache_shots[i] = cs.uniform_cache_shots[i - 1];
            }
            cs.uniform_cache_hits[0] = 0;
            cs.uniform_cache_shots[0] = 0;

            // Determine whether to skip the cache for small uniform buffers.
            // Upstream: skip_preferred = hits * 256 < shots * 251
            let skip_preferred = hits.saturating_mul(256) < shots.saturating_mul(251);
            cs.uniform_buffer_skip_cache_size = if skip_preferred {
                DEFAULT_SKIP_CACHE_SIZE
            } else {
                0
            };
        }

        // TODO: runtime.TickFrame(slot_buffers) — runtime not yet available.
        // TODO: total_used_memory = runtime.GetDeviceMemoryUsage() if CanReportMemoryUsage.

        if self.total_used_memory >= self.minimum_memory {
            self.run_garbage_collector();
        }

        self.frame_tick += 1;

        self.delayed_destruction_ring.tick();
        // TODO: async_buffers_death_ring cleanup — requires runtime staging buffer free.
    }

    // -----------------------------------------------------------------------
    // Public API — memory writes
    // -----------------------------------------------------------------------

    /// Notify the cache that a CPU write happened at `[device_addr, device_addr+size)`.
    ///
    /// Upstream: `BufferCache<P>::WriteMemory`
    pub fn write_memory(&mut self, device_addr: VAddr, size: u64) {
        if self.memory_tracker.is_region_gpu_modified(device_addr, size) {
            self.clear_download(device_addr, size);
            self.gpu_modified_ranges.subtract(device_addr, size as usize);
        }
        self.memory_tracker.mark_region_as_cpu_modified(device_addr, size);
    }

    /// Notify the cache about a cached (deferred) CPU write.
    ///
    /// Upstream: `BufferCache<P>::CachedWriteMemory`
    ///
    /// NOTE: `device_memory.ReadBlockUnsafe` is not available; the inline path falls back to
    /// `write_memory` for non-GPU-modified regions and logs a warning for the inline path.
    pub fn cached_write_memory(&mut self, device_addr: VAddr, size: u64) {
        let is_dirty = self.is_region_registered(device_addr, size as usize);
        if !is_dirty {
            return;
        }
        let aligned_start = device_addr & !(DEVICE_PAGESIZE - 1);
        let aligned_end = (device_addr + size + DEVICE_PAGESIZE - 1) & !(DEVICE_PAGESIZE - 1);
        if !self.is_region_gpu_modified(aligned_start, (aligned_end - aligned_start) as usize) {
            self.write_memory(device_addr, size);
            return;
        }
        // TODO: device_memory.ReadBlockUnsafe — not yet available in this port.
        // Upstream reads back current memory content and calls InlineMemoryImplementation.
        // We fall through to write_memory as a safe approximation.
        log::warn!(
            "cached_write_memory: GPU-modified region at {:#x}+{} — inline path requires \
             device_memory which is not yet ported; falling back to write_memory",
            device_addr,
            size
        );
        self.write_memory(device_addr, size);
    }

    /// Called when a CPU write is detected. Returns true if the caller must
    /// flush GPU-modified data first.
    ///
    /// Upstream: `BufferCache<P>::OnCPUWrite`
    pub fn on_cpu_write(&mut self, device_addr: VAddr, size: u64) -> bool {
        let is_dirty = self.is_region_registered(device_addr, size as usize);
        if !is_dirty {
            return false;
        }
        if self.memory_tracker.is_region_gpu_modified(device_addr, size) {
            return true;
        }
        self.write_memory(device_addr, size);
        false
    }

    /// Download GPU-modified memory back to the CPU for the given range.
    ///
    /// Upstream: `BufferCache<P>::DownloadMemory`
    pub fn download_memory(&mut self, device_addr: VAddr, size: u64) {
        // Collect overlapping buffer IDs first to avoid borrow issues.
        let mut buffer_ids = Vec::new();
        {
            let page_end = div_ceil(device_addr + size, CACHING_PAGESIZE);
            let mut page = device_addr >> CACHING_PAGEBITS;
            while page < page_end {
                let buffer_id = self.page_table[page as usize];
                if !buffer_id.is_valid() {
                    page += 1;
                    continue;
                }
                if !buffer_ids.contains(&buffer_id) {
                    buffer_ids.push(buffer_id);
                }
                let buffer = &self.slot_buffers[buffer_id];
                let end_addr = buffer.cpu_addr() + buffer.size_bytes() as u64;
                page = div_ceil(end_addr, CACHING_PAGESIZE);
            }
        }
        for buffer_id in buffer_ids {
            self.download_buffer_memory_range(buffer_id, device_addr, size);
        }
    }

    /// Get the flush area for a device address range.
    ///
    /// Upstream: `BufferCache<P>::GetFlushArea`
    pub fn get_flush_area(
        &mut self,
        device_addr: VAddr,
        size: u64,
    ) -> Option<RasterizerDownloadArea> {
        let device_addr_start_aligned = device_addr & !(DEVICE_PAGESIZE - 1);
        let device_addr_end_aligned =
            (device_addr + size + DEVICE_PAGESIZE - 1) & !(DEVICE_PAGESIZE - 1);

        if self.memory_tracker.is_region_preflushable(device_addr, size) {
            return Some(RasterizerDownloadArea {
                start_address: device_addr_start_aligned,
                end_address: device_addr_end_aligned,
                preemtive: true,
            });
        }

        let preemtive = !self.is_region_gpu_modified(
            device_addr_start_aligned,
            (device_addr_end_aligned - device_addr_start_aligned) as usize,
        );
        self.memory_tracker.mark_region_as_preflushable(
            device_addr_start_aligned,
            device_addr_end_aligned - device_addr_start_aligned,
        );

        Some(RasterizerDownloadArea {
            start_address: device_addr_start_aligned,
            end_address: device_addr_end_aligned,
            preemtive,
        })
    }

    /// Inline a small memory write directly into the buffer.
    ///
    /// Upstream: `BufferCache<P>::InlineMemory`
    pub fn inline_memory(
        &mut self,
        dest_address: VAddr,
        copy_size: usize,
        inlined_buffer: &[u8],
    ) -> bool {
        let is_dirty = self.is_region_registered(dest_address, copy_size);
        if !is_dirty {
            return false;
        }
        let aligned_start = dest_address & !(DEVICE_PAGESIZE - 1);
        let aligned_end = (dest_address + copy_size as u64 + DEVICE_PAGESIZE - 1) & !(DEVICE_PAGESIZE - 1);
        if !self.is_region_gpu_modified(aligned_start, (aligned_end - aligned_start) as usize) {
            return false;
        }
        self.inline_memory_implementation(dest_address, copy_size, inlined_buffer);
        true
    }

    // -----------------------------------------------------------------------
    // Public API — buffer binding (graphics)
    // -----------------------------------------------------------------------

    /// Bind a graphics uniform buffer.
    ///
    /// Upstream: `BufferCache<P>::BindGraphicsUniformBuffer`
    ///
    /// NOTE: `gpu_memory->GpuToCpuAddress` is not yet available. We store `gpu_addr`
    /// directly in the `device_addr` field as a placeholder.
    pub fn bind_graphics_uniform_buffer(
        &mut self,
        stage: usize,
        index: u32,
        gpu_addr: u64,
        size: u32,
    ) {
        let Some(ref mut cs) = self.channel_state else {
            return;
        };
        // TODO: translate gpu_addr via gpu_memory->GpuToCpuAddress once available.
        let binding = Binding {
            device_addr: gpu_addr,
            size,
            buffer_id: NULL_BUFFER_ID,
        };
        if stage < NUM_STAGES as usize && (index as usize) < NUM_GRAPHICS_UNIFORM_BUFFERS as usize {
            cs.uniform_buffers[stage][index as usize] = binding;
        }
    }

    /// Disable a graphics uniform buffer.
    ///
    /// Upstream: `BufferCache<P>::DisableGraphicsUniformBuffer`
    pub fn disable_graphics_uniform_buffer(&mut self, stage: usize, index: u32) {
        let Some(ref mut cs) = self.channel_state else {
            return;
        };
        if stage < NUM_STAGES as usize && (index as usize) < NUM_GRAPHICS_UNIFORM_BUFFERS as usize {
            cs.uniform_buffers[stage][index as usize] = NULL_BINDING;
        }
    }

    /// Update all graphics buffer bindings.
    ///
    /// Upstream: `BufferCache<P>::UpdateGraphicsBuffers`
    pub fn update_graphics_buffers(&mut self, is_indexed: bool) {
        if self.channel_state.is_none() {
            return;
        }
        loop {
            if let Some(ref mut cs) = self.channel_state {
                cs.has_deleted_buffers = false;
            }
            self.do_update_graphics_buffers(is_indexed);
            if let Some(ref cs) = self.channel_state {
                if !cs.has_deleted_buffers {
                    break;
                }
            } else {
                break;
            }
        }
    }

    /// Update all compute buffer bindings.
    ///
    /// Upstream: `BufferCache<P>::UpdateComputeBuffers`
    pub fn update_compute_buffers(&mut self) {
        if self.channel_state.is_none() {
            return;
        }
        loop {
            if let Some(ref mut cs) = self.channel_state {
                cs.has_deleted_buffers = false;
            }
            self.do_update_compute_buffers();
            if let Some(ref cs) = self.channel_state {
                if !cs.has_deleted_buffers {
                    break;
                }
            } else {
                break;
            }
        }
    }

    /// Bind host geometry buffers (index + vertex).
    ///
    /// Upstream: `BufferCache<P>::BindHostGeometryBuffers`
    ///
    /// NOTE: draw state (maxwell3d, current_draw_indirect) not yet available.
    pub fn bind_host_geometry_buffers(&mut self, is_indexed: bool) {
        if is_indexed {
            self.bind_host_index_buffer();
        }
        // Non-indexed quad topology path requires maxwell3d draw state — stubbed.
        self.bind_host_vertex_buffers();
        self.bind_host_transform_feedback_buffers();
        // TODO: if current_draw_indirect — requires engine state not yet ported.
    }

    /// Bind host stage buffers.
    ///
    /// Upstream: `BufferCache<P>::BindHostStageBuffers`
    pub fn bind_host_stage_buffers(&mut self, stage: usize) {
        self.bind_host_graphics_uniform_buffers(stage);
        self.bind_host_graphics_storage_buffers(stage);
        self.bind_host_graphics_texture_buffers(stage);
    }

    /// Bind host compute buffers.
    ///
    /// Upstream: `BufferCache<P>::BindHostComputeBuffers`
    pub fn bind_host_compute_buffers(&mut self) {
        self.bind_host_compute_uniform_buffers();
        self.bind_host_compute_storage_buffers();
        self.bind_host_compute_texture_buffers();
    }

    /// Set the uniform buffer state for graphics stages.
    ///
    /// Upstream: `BufferCache<P>::SetUniformBuffersState`
    pub fn set_uniform_buffers_state(
        &mut self,
        mask: &[u32; NUM_STAGES as usize],
        sizes: &UniformBufferSizes,
    ) {
        let Some(ref mut cs) = self.channel_state else {
            return;
        };
        if P::HAS_PERSISTENT_UNIFORM_BUFFER_BINDINGS && cs.enabled_uniform_buffer_masks != *mask {
            if P::IS_OPENGL {
                cs.fast_bound_uniform_buffers.fill(0);
            }
            cs.dirty_uniform_buffers.fill(!0u32);
            cs.uniform_buffer_binding_sizes = [[0u32; NUM_GRAPHICS_UNIFORM_BUFFERS as usize];
                NUM_STAGES as usize];
        }
        cs.enabled_uniform_buffer_masks = *mask;
        cs.uniform_buffer_sizes = Some(Box::new(*sizes));
    }

    /// Set the uniform buffer state for compute.
    ///
    /// Upstream: `BufferCache<P>::SetComputeUniformBufferState`
    pub fn set_compute_uniform_buffer_state(
        &mut self,
        mask: u32,
        sizes: &ComputeUniformBufferSizes,
    ) {
        let Some(ref mut cs) = self.channel_state else {
            return;
        };
        cs.enabled_compute_uniform_buffer_mask = mask;
        cs.compute_uniform_buffer_sizes = Some(Box::new(*sizes));
    }

    // -----------------------------------------------------------------------
    // Public API — storage buffers
    // -----------------------------------------------------------------------

    /// Unbind all graphics storage buffers for a stage.
    ///
    /// Upstream: `BufferCache<P>::UnbindGraphicsStorageBuffers`
    pub fn unbind_graphics_storage_buffers(&mut self, stage: usize) {
        let Some(ref mut cs) = self.channel_state else {
            return;
        };
        if stage < NUM_STAGES as usize {
            cs.enabled_storage_buffers[stage] = 0;
            cs.written_storage_buffers[stage] = 0;
        }
    }

    /// Bind a graphics storage buffer.
    ///
    /// Upstream: `BufferCache<P>::BindGraphicsStorageBuffer`
    ///
    /// NOTE: Resolving the storage buffer address requires maxwell3d shader stage state
    /// (`cbufs.const_buffers[cbuf_index].address + cbuf_offset`) which is not yet available.
    /// We store the placeholder NULL_BINDING and set the enabled/written masks correctly.
    pub fn bind_graphics_storage_buffer(
        &mut self,
        stage: usize,
        ssbo_index: usize,
        cbuf_index: u32,
        cbuf_offset: u32,
        is_written: bool,
    ) {
        let Some(ref mut cs) = self.channel_state else {
            return;
        };
        if stage >= NUM_STAGES as usize || ssbo_index >= NUM_STORAGE_BUFFERS as usize {
            return;
        }
        cs.enabled_storage_buffers[stage] |= 1u32 << ssbo_index;
        cs.written_storage_buffers[stage] |= if is_written { 1u32 } else { 0u32 } << ssbo_index;

        // TODO: resolve ssbo_addr = cbufs[cbuf_index].address + cbuf_offset via maxwell3d state.
        // Upstream calls StorageBufferBinding(ssbo_addr, cbuf_index, is_written).
        // For now store NULL_BINDING — the buffer_id will be resolved in UpdateStorageBuffers.
        let _ = (cbuf_index, cbuf_offset);
        cs.storage_buffers[stage][ssbo_index] = NULL_BINDING;
    }

    /// Unbind all compute storage buffers.
    ///
    /// Upstream: `BufferCache<P>::UnbindComputeStorageBuffers`
    pub fn unbind_compute_storage_buffers(&mut self) {
        let Some(ref mut cs) = self.channel_state else {
            return;
        };
        cs.enabled_compute_storage_buffers = 0;
        cs.written_compute_storage_buffers = 0;
        cs.image_compute_texture_buffers = 0;
    }

    /// Bind a compute storage buffer.
    ///
    /// Upstream: `BufferCache<P>::BindComputeStorageBuffer`
    ///
    /// NOTE: Resolving the SSBO address requires kepler_compute launch description which is
    /// not yet available. We set masks and store NULL_BINDING.
    pub fn bind_compute_storage_buffer(
        &mut self,
        ssbo_index: usize,
        cbuf_index: u32,
        cbuf_offset: u32,
        is_written: bool,
    ) {
        let Some(ref mut cs) = self.channel_state else {
            return;
        };
        if ssbo_index >= cs.compute_storage_buffers.len() {
            log::error!(
                "bind_compute_storage_buffer: index {} exceeds maximum storage buffer count",
                ssbo_index
            );
            return;
        }
        cs.enabled_compute_storage_buffers |= 1u32 << ssbo_index;
        cs.written_compute_storage_buffers |= if is_written { 1u32 } else { 0u32 } << ssbo_index;

        // TODO: resolve ssbo_addr via kepler_compute->launch_description.const_buffer_config.
        let _ = (cbuf_index, cbuf_offset);
        cs.compute_storage_buffers[ssbo_index] = NULL_BINDING;
    }

    // -----------------------------------------------------------------------
    // Public API — texture buffers
    // -----------------------------------------------------------------------

    /// Unbind all graphics texture buffers for a stage.
    ///
    /// Upstream: `BufferCache<P>::UnbindGraphicsTextureBuffers`
    pub fn unbind_graphics_texture_buffers(&mut self, stage: usize) {
        let Some(ref mut cs) = self.channel_state else {
            return;
        };
        if stage < NUM_STAGES as usize {
            cs.enabled_texture_buffers[stage] = 0;
            cs.written_texture_buffers[stage] = 0;
            cs.image_texture_buffers[stage] = 0;
        }
    }

    /// Bind a graphics texture buffer.
    ///
    /// Upstream: `BufferCache<P>::BindGraphicsTextureBuffer`
    pub fn bind_graphics_texture_buffer(
        &mut self,
        stage: usize,
        tbo_index: usize,
        gpu_addr: u64,
        size: u32,
        format: u32,
        is_written: bool,
        is_image: bool,
    ) {
        let Some(ref mut cs) = self.channel_state else {
            return;
        };
        if stage >= NUM_STAGES as usize || tbo_index >= NUM_TEXTURE_BUFFERS as usize {
            return;
        }
        cs.enabled_texture_buffers[stage] |= 1u32 << tbo_index;
        cs.written_texture_buffers[stage] |= if is_written { 1u32 } else { 0u32 } << tbo_index;
        if P::SEPARATE_IMAGE_BUFFER_BINDINGS {
            cs.image_texture_buffers[stage] |= if is_image { 1u32 } else { 0u32 } << tbo_index;
        }
        // TODO: translate gpu_addr via gpu_memory->GpuToCpuAddress.
        // Call GetTextureBufferBinding(gpu_addr, size, format) once GPU memory is available.
        cs.texture_buffers[stage][tbo_index] = TextureBufferBinding {
            device_addr: gpu_addr,
            size,
            buffer_id: NULL_BUFFER_ID,
            format,
        };
    }

    /// Unbind all compute texture buffers.
    ///
    /// Upstream: `BufferCache<P>::UnbindComputeTextureBuffers`
    pub fn unbind_compute_texture_buffers(&mut self) {
        let Some(ref mut cs) = self.channel_state else {
            return;
        };
        cs.enabled_compute_texture_buffers = 0;
        cs.written_compute_texture_buffers = 0;
        cs.image_compute_texture_buffers = 0;
    }

    /// Bind a compute texture buffer.
    ///
    /// Upstream: `BufferCache<P>::BindComputeTextureBuffer`
    pub fn bind_compute_texture_buffer(
        &mut self,
        tbo_index: usize,
        gpu_addr: u64,
        size: u32,
        format: u32,
        is_written: bool,
        is_image: bool,
    ) {
        let Some(ref mut cs) = self.channel_state else {
            return;
        };
        if tbo_index >= cs.compute_texture_buffers.len() {
            log::error!(
                "bind_compute_texture_buffer: index {} exceeds maximum texture buffer count",
                tbo_index
            );
            return;
        }
        cs.enabled_compute_texture_buffers |= 1u32 << tbo_index;
        cs.written_compute_texture_buffers |=
            if is_written { 1u32 } else { 0u32 } << tbo_index;
        if P::SEPARATE_IMAGE_BUFFER_BINDINGS {
            cs.image_compute_texture_buffers |= if is_image { 1u32 } else { 0u32 } << tbo_index;
        }
        // TODO: translate gpu_addr via gpu_memory->GpuToCpuAddress.
        cs.compute_texture_buffers[tbo_index] = TextureBufferBinding {
            device_addr: gpu_addr,
            size,
            buffer_id: NULL_BUFFER_ID,
            format,
        };
    }

    // -----------------------------------------------------------------------
    // Public API — obtain buffers
    // -----------------------------------------------------------------------

    /// Obtain a buffer by GPU virtual address.
    ///
    /// Returns `(buffer_id, offset)` within the buffer.
    ///
    /// Upstream: `BufferCache<P>::ObtainBuffer`
    ///
    /// NOTE: gpu_memory->GpuToCpuAddress translation is not yet available.
    /// Returns the null buffer; callers should use obtain_cpu_buffer when device_addr is known.
    pub fn obtain_buffer(
        &mut self,
        _gpu_addr: u64,
        _size: u32,
        _sync_info: ObtainBufferSynchronize,
        _post_op: ObtainBufferOperation,
    ) -> (BufferId, u32) {
        // TODO: translate gpu_addr via gpu_memory->GpuToCpuAddress, then call obtain_cpu_buffer.
        log::warn!(
            "obtain_buffer: GPU->CPU address translation not yet available; returning null buffer"
        );
        (NULL_BUFFER_ID, 0)
    }

    /// Obtain a buffer by CPU/device address.
    ///
    /// Upstream: `BufferCache<P>::ObtainCPUBuffer`
    pub fn obtain_cpu_buffer(
        &mut self,
        device_addr: VAddr,
        size: u32,
        sync_info: ObtainBufferSynchronize,
        post_op: ObtainBufferOperation,
    ) -> (BufferId, u32) {
        let buffer_id = self.find_buffer(device_addr, size);

        match sync_info {
            ObtainBufferSynchronize::FullSynchronize => {
                self.synchronize_buffer(buffer_id, device_addr, size);
            }
            _ => {}
        }

        match post_op {
            ObtainBufferOperation::MarkAsWritten => {
                self.mark_written_buffer(buffer_id, device_addr, size);
            }
            ObtainBufferOperation::DiscardWrite => {
                let device_addr_start = device_addr & !63u64; // AlignDown(device_addr, 64)
                let device_addr_end = (device_addr + size as u64 + 63) & !63u64;
                let new_size = device_addr_end - device_addr_start;
                self.clear_download(device_addr_start, new_size);
                self.gpu_modified_ranges.subtract(device_addr_start, new_size as usize);
            }
            _ => {}
        }

        let offset = self.slot_buffers[buffer_id].offset(device_addr);
        (buffer_id, offset)
    }

    // -----------------------------------------------------------------------
    // Public API — flush / commit
    // -----------------------------------------------------------------------

    /// Flush all cached CPU writes.
    ///
    /// Upstream: `BufferCache<P>::FlushCachedWrites`
    pub fn flush_cached_writes(&mut self) {
        self.memory_tracker.flush_cached_writes();
    }

    /// Return true when there are uncommitted buffers to be downloaded.
    pub fn has_uncommitted_flushes(&self) -> bool {
        !self.uncommitted_gpu_modified_ranges.empty()
    }

    /// Accumulate current uncommitted ranges into committed.
    ///
    /// Upstream: `BufferCache<P>::AccumulateFlushes`
    pub fn accumulate_flushes(&mut self) {
        if self.uncommitted_gpu_modified_ranges.empty() {
            return;
        }
        // Move uncommitted ranges into a new committed slot.
        let ranges = std::mem::replace(&mut self.uncommitted_gpu_modified_ranges, RangeSet::new());
        self.committed_gpu_modified_ranges.push_back(ranges);
    }

    /// Return true when the caller should wait for async flushes.
    pub fn should_wait_async_flushes(&self) -> bool {
        !self.committed_gpu_modified_ranges.is_empty()
    }

    /// Commit asynchronous downloads.
    ///
    /// Upstream: `BufferCache<P>::CommitAsyncFlushes` delegates to CommitAsyncFlushesHigh.
    pub fn commit_async_flushes(&mut self) {
        self.commit_async_flushes_high();
    }

    /// Commit asynchronous downloads (high priority).
    ///
    /// Upstream: `BufferCache<P>::CommitAsyncFlushesHigh`
    ///
    /// NOTE: This method depends on runtime staging buffer allocation and GPU copy operations
    /// which are not yet available in this port. We accumulate flushes and log the intent.
    pub fn commit_async_flushes_high(&mut self) {
        self.accumulate_flushes();

        if self.committed_gpu_modified_ranges.is_empty() {
            // Upstream pushes empty optional async buffer.
            return;
        }

        // TODO: full implementation requires:
        //   - runtime.DownloadStagingBuffer
        //   - runtime.PreCopyBarrier / PostCopyBarrier
        //   - runtime.CopyBuffer (GPU→staging)
        //   - memory_tracker.ForEachDownloadRange (already available but needs range_set walk)
        //   - gpu_modified_ranges.ForEachInRange (range_set not yet ported)
        // For now, clear committed ranges so the cache does not stall indefinitely.
        log::warn!(
            "commit_async_flushes_high: GPU download staging not yet implemented; \
             clearing {} committed range sets",
            self.committed_gpu_modified_ranges.len()
        );
        self.committed_gpu_modified_ranges.clear();
    }

    /// Pop completed asynchronous downloads.
    ///
    /// Upstream: `BufferCache<P>::PopAsyncFlushes` delegates to PopAsyncBuffers.
    pub fn pop_async_flushes(&mut self) {
        self.pop_async_buffers();
    }

    /// Pop completed asynchronous buffers.
    ///
    /// Upstream: `BufferCache<P>::PopAsyncBuffers`
    ///
    /// NOTE: Requires async_buffers (staging buffer queue) and device_memory.WriteBlockUnsafe
    /// which are not yet ported.
    pub fn pop_async_buffers(&mut self) {
        if self.pending_downloads.is_empty() {
            return;
        }
        // TODO: full implementation reads back from staging memory and writes to device_memory.
        // For now simply drain pending downloads to avoid stale state.
        self.pending_downloads.pop_front();
    }

    // -----------------------------------------------------------------------
    // Public API — DMA
    // -----------------------------------------------------------------------

    /// Perform a DMA copy between two GPU virtual addresses.
    ///
    /// Upstream: `BufferCache<P>::DMACopy`
    ///
    /// NOTE: GPU->CPU address translation and runtime.CopyBuffer are not yet available.
    pub fn dma_copy(&mut self, _src_address: u64, _dest_address: u64, _amount: u64) -> bool {
        // TODO: requires gpu_memory->GpuToCpuAddress, runtime.CopyBuffer, and range_set ops.
        log::warn!("dma_copy: not yet implemented (requires GPU memory + runtime)");
        false
    }

    /// Perform a DMA clear.
    ///
    /// Upstream: `BufferCache<P>::DMAClear`
    ///
    /// NOTE: GPU->CPU address translation and runtime.ClearBuffer are not yet available.
    pub fn dma_clear(&mut self, _src_address: u64, _amount: u64, _value: u32) -> bool {
        // TODO: requires gpu_memory->GpuToCpuAddress and runtime.ClearBuffer.
        log::warn!("dma_clear: not yet implemented (requires GPU memory + runtime)");
        false
    }

    // -----------------------------------------------------------------------
    // Public API — region queries
    // -----------------------------------------------------------------------

    /// Return true when a device region is GPU-modified.
    ///
    /// Upstream: `BufferCache<P>::IsRegionGpuModified`
    pub fn is_region_gpu_modified(&self, addr: VAddr, size: usize) -> bool {
        let mut found = false;
        self.gpu_modified_ranges.for_each_in_range(addr, size, |_start, _end| {
            found = true;
        });
        found
    }

    /// Return true when a region is registered in the cache.
    ///
    /// Upstream: `BufferCache<P>::IsRegionRegistered`
    pub fn is_region_registered(&self, addr: VAddr, size: usize) -> bool {
        let end_addr = addr + size as u64;
        let page_end = div_ceil(end_addr, CACHING_PAGESIZE);
        let mut page = addr >> CACHING_PAGEBITS;
        while page < page_end {
            let buffer_id = self.page_table[page as usize];
            if !buffer_id.is_valid() {
                page += 1;
                continue;
            }
            let buffer = &self.slot_buffers[buffer_id];
            let buf_start = buffer.cpu_addr();
            let buf_end = buf_start + buffer.size_bytes() as u64;
            if buf_start < end_addr && addr < buf_end {
                return true;
            }
            page = div_ceil(end_addr, CACHING_PAGESIZE);
        }
        false
    }

    /// Return true when a device region is CPU-modified.
    ///
    /// Upstream: `BufferCache<P>::IsRegionCpuModified`
    pub fn is_region_cpu_modified(&mut self, addr: VAddr, size: usize) -> bool {
        self.memory_tracker.is_region_cpu_modified(addr, size as u64)
    }

    // -----------------------------------------------------------------------
    // Public API — draw indirect
    // -----------------------------------------------------------------------

    /// Get the draw indirect count buffer.
    ///
    /// Upstream: `BufferCache<P>::GetDrawIndirectCount`
    pub fn get_draw_indirect_count(&mut self) -> (BufferId, u32) {
        let Some(ref cs) = self.channel_state else {
            return (NULL_BUFFER_ID, 0);
        };
        let binding = cs.count_buffer_binding;
        let offset = self.slot_buffers[binding.buffer_id].offset(binding.device_addr);
        (binding.buffer_id, offset)
    }

    /// Get the draw indirect buffer.
    ///
    /// Upstream: `BufferCache<P>::GetDrawIndirectBuffer`
    pub fn get_draw_indirect_buffer(&mut self) -> (BufferId, u32) {
        let Some(ref cs) = self.channel_state else {
            return (NULL_BUFFER_ID, 0);
        };
        let binding = cs.indirect_buffer_binding;
        let offset = self.slot_buffers[binding.buffer_id].offset(binding.device_addr);
        (binding.buffer_id, offset)
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
    // Range-set helpers — no longer needed; using common::range_sets::RangeSet directly.
    // -----------------------------------------------------------------------

    // -----------------------------------------------------------------------
    // Private helpers — operations
    // -----------------------------------------------------------------------

    /// Run the garbage collector: destroy LRU buffers until memory pressure is reduced.
    ///
    /// Upstream: `BufferCache<P>::RunGarbageCollector`
    ///
    /// NOTE: Upstream uses `lru_cache.ForEachItemBelow` which is not yet ported.
    /// We perform a simplified pass that destroys the oldest buffers heuristically.
    fn run_garbage_collector(&mut self) {
        let aggressive_gc = self.total_used_memory >= self.critical_memory;
        let ticks_to_destroy: u64 = if aggressive_gc { 60 } else { 120 };
        let num_iterations: usize = if aggressive_gc { 64 } else { 32 };

        // Upstream: lru_cache.ForEachItemBelow(frame_tick - ticks_to_destroy, clean_up)
        // The callback downloads buffer memory and then deletes the buffer,
        // stopping after num_iterations buffers.
        let tick_threshold = self.frame_tick.saturating_sub(ticks_to_destroy) as i64;
        let mut remaining = num_iterations;
        let mut to_delete: Vec<BufferId> = Vec::new();
        self.lru_cache.for_each_item_below(tick_threshold, |buffer_id| {
            if remaining == 0 {
                return true; // stop
            }
            remaining -= 1;
            to_delete.push(buffer_id);
            false // continue
        });
        for buffer_id in to_delete {
            self.download_buffer_memory(buffer_id);
            self.delete_buffer(buffer_id, false);
        }
    }

    fn bind_host_index_buffer(&mut self) {
        // Upstream: synchronizes + binds the index buffer to the GPU.
        // Requires runtime.BindIndexBuffer and draw_state — not yet available.
        let Some(ref cs) = self.channel_state else {
            return;
        };
        let binding = cs.index_buffer;
        let buffer_id = binding.buffer_id;
        let device_addr = binding.device_addr;
        let size = binding.size;
        drop(cs); // release borrow

        // Touch and synchronize.
        self.touch_buffer(buffer_id);
        self.synchronize_buffer(buffer_id, device_addr, size);
        // TODO: runtime.BindIndexBuffer — not yet available.
    }

    fn bind_host_vertex_buffers(&mut self) {
        // Upstream: iterates vertex buffers, synchronizes, collects host bindings, then
        // calls runtime.BindVertexBuffers. Runtime not yet available.
        let Some(ref cs) = self.channel_state else {
            return;
        };
        let bindings: Vec<Binding> = cs.vertex_buffers.to_vec();
        drop(cs);

        for binding in bindings {
            self.touch_buffer(binding.buffer_id);
            self.synchronize_buffer(binding.buffer_id, binding.device_addr, binding.size);
        }
        // TODO: runtime.BindVertexBuffers — not yet available.
    }

    fn bind_host_draw_indirect_buffers(&mut self) {
        // Upstream: synchronize count + indirect buffers.
        // current_draw_indirect is engine state not yet ported.
        let Some(ref cs) = self.channel_state else {
            return;
        };
        let count_binding = cs.count_buffer_binding;
        let indirect_binding = cs.indirect_buffer_binding;
        drop(cs);

        self.touch_buffer(count_binding.buffer_id);
        self.synchronize_buffer(
            count_binding.buffer_id,
            count_binding.device_addr,
            count_binding.size,
        );
        self.touch_buffer(indirect_binding.buffer_id);
        self.synchronize_buffer(
            indirect_binding.buffer_id,
            indirect_binding.device_addr,
            indirect_binding.size,
        );
    }

    /// Upstream: `BufferCache<P>::BindHostGraphicsUniformBuffers`
    fn bind_host_graphics_uniform_buffers(&mut self, stage: usize) {
        let Some(ref cs) = self.channel_state else {
            return;
        };
        let dirty = if P::HAS_PERSISTENT_UNIFORM_BUFFER_BINDINGS {
            let d = cs.dirty_uniform_buffers[stage];
            d
        } else {
            !0u32
        };
        let mask = cs.enabled_uniform_buffer_masks[stage];
        drop(cs);

        if P::HAS_PERSISTENT_UNIFORM_BUFFER_BINDINGS {
            if let Some(ref mut cs) = self.channel_state {
                cs.dirty_uniform_buffers[stage] = 0;
            }
        }

        let mut binding_index = 0u32;
        let mask_copy = mask;
        let dirty_copy = dirty;
        Self::for_each_enabled_bit(mask_copy, |index| {
            let needs_bind = ((dirty_copy >> index) & 1) != 0;
            // NOTE: We capture binding_index by value here because we can't borrow self
            // inside the closure. The method is called after the closure completes.
            let _ = (needs_bind, binding_index);
            if P::NEEDS_BIND_UNIFORM_INDEX {
                binding_index += 1;
            }
        });

        // Do the actual binding outside the for_each_enabled_bit call to avoid closure borrow issues.
        let mut binding_index = 0u32;
        let Some(ref cs) = self.channel_state else {
            return;
        };
        let mask = cs.enabled_uniform_buffer_masks[stage];
        let dirty = if P::HAS_PERSISTENT_UNIFORM_BUFFER_BINDINGS {
            cs.dirty_uniform_buffers[stage]
        } else {
            !0u32
        };
        drop(cs);

        let mut bits = mask;
        let mut idx: u32 = 0;
        while bits != 0 {
            let skip = bits.trailing_zeros();
            idx += skip;
            bits >>= skip;
            let needs_bind = ((dirty >> idx) & 1) != 0;
            self.bind_host_graphics_uniform_buffer(stage, idx, binding_index, needs_bind);
            if P::NEEDS_BIND_UNIFORM_INDEX {
                binding_index += 1;
            }
            idx += 1;
            bits >>= 1;
        }
    }

    /// Upstream: `BufferCache<P>::BindHostGraphicsUniformBuffer`
    ///
    /// NOTE: The fast-buffer path (runtime.BindMappedUniformBuffer,
    /// runtime.PushFastUniformBuffer) requires device_memory and runtime which are
    /// not yet available. We fall through to the synchronize path only.
    fn bind_host_graphics_uniform_buffer(
        &mut self,
        stage: usize,
        index: u32,
        binding_index: u32,
        needs_bind: bool,
    ) {
        let Some(ref cs) = self.channel_state else {
            return;
        };
        let binding = cs.uniform_buffers[stage][index as usize];
        let skip_cache_size = cs.uniform_buffer_skip_cache_size;
        let ub_sizes = cs.uniform_buffer_sizes.clone();
        drop(cs);

        let device_addr = binding.device_addr;
        let size = if let Some(ref sizes) = ub_sizes {
            binding.size.min(sizes[stage][index as usize])
        } else {
            binding.size
        };

        // Touch the buffer.
        self.touch_buffer(binding.buffer_id);

        let use_fast_buffer = binding.buffer_id != NULL_BUFFER_ID
            && size <= skip_cache_size
            && !self.memory_tracker.is_region_gpu_modified(device_addr, size as u64);

        if use_fast_buffer {
            // TODO: fast buffer path (BindMappedUniformBuffer / PushFastUniformBuffer)
            // requires device_memory and runtime — log and skip for now.
            if P::IS_OPENGL {
                if let Some(ref mut cs) = self.channel_state {
                    cs.fast_bound_uniform_buffers[stage] |= 1u32 << binding_index;
                    cs.uniform_buffer_binding_sizes[stage][binding_index as usize] = size;
                }
            }
            return;
        }

        // Classic cached path.
        let sync_cached = self.synchronize_buffer(binding.buffer_id, device_addr, size);
        if let Some(ref mut cs) = self.channel_state {
            if sync_cached {
                cs.uniform_cache_hits[0] = cs.uniform_cache_hits[0].wrapping_add(1);
            }
            cs.uniform_cache_shots[0] = cs.uniform_cache_shots[0].wrapping_add(1);
        }

        let has_fast_bound = self.has_fast_uniform_buffer_bound(stage, binding_index);
        let binding_size_differs = if P::HAS_PERSISTENT_UNIFORM_BUFFER_BINDINGS {
            self.channel_state.as_ref().map_or(false, |cs| {
                cs.uniform_buffer_binding_sizes[stage][binding_index as usize] != size
            })
        } else {
            false
        };
        let needs_bind = needs_bind | has_fast_bound | binding_size_differs;
        if !needs_bind {
            return;
        }

        let offset = self.slot_buffers[binding.buffer_id].offset(device_addr);
        if P::IS_OPENGL {
            if let Some(ref mut cs) = self.channel_state {
                cs.fast_bound_uniform_buffers[stage] &= !(1u32 << binding_index);
            }
        }
        if P::HAS_PERSISTENT_UNIFORM_BUFFER_BINDINGS {
            if let Some(ref mut cs) = self.channel_state {
                cs.uniform_buffer_binding_sizes[stage][binding_index as usize] = size;
            }
        }
        // TODO: runtime.BindUniformBuffer — not yet available.
        let _ = (offset, size, binding_index);
    }

    fn bind_host_graphics_storage_buffers(&mut self, stage: usize) {
        // Upstream: iterates enabled storage buffers, synchronizes, then calls
        // runtime.BindStorageBuffer. Runtime not yet available.
        let Some(ref cs) = self.channel_state else {
            return;
        };
        let mask = cs.enabled_storage_buffers[stage];
        let written_mask = cs.written_storage_buffers[stage];
        let bindings: Vec<Binding> = cs.storage_buffers[stage].to_vec();
        drop(cs);

        let mut binding_index = 0u32;
        let mut bits = mask;
        let mut idx: u32 = 0;
        while bits != 0 {
            let skip = bits.trailing_zeros();
            idx += skip;
            bits >>= skip;

            let binding = bindings[idx as usize];
            self.touch_buffer(binding.buffer_id);
            self.synchronize_buffer(binding.buffer_id, binding.device_addr, binding.size);

            let is_written = ((written_mask >> idx) & 1) != 0;
            if is_written {
                self.mark_written_buffer(binding.buffer_id, binding.device_addr, binding.size);
            }
            // TODO: runtime.BindStorageBuffer — not yet available.
            let _ = binding_index;
            if P::NEEDS_BIND_STORAGE_INDEX {
                binding_index += 1;
            }

            idx += 1;
            bits >>= 1;
        }
    }

    fn bind_host_graphics_texture_buffers(&mut self, stage: usize) {
        // Upstream: iterates enabled texture buffers, synchronizes, calls
        // runtime.BindTextureBuffer / BindImageBuffer. Runtime not yet available.
        let Some(ref cs) = self.channel_state else {
            return;
        };
        let mask = cs.enabled_texture_buffers[stage];
        let written_mask = cs.written_texture_buffers[stage];
        let image_mask = cs.image_texture_buffers[stage];
        let bindings: Vec<TextureBufferBinding> = cs.texture_buffers[stage].to_vec();
        drop(cs);

        let mut bits = mask;
        let mut idx: u32 = 0;
        while bits != 0 {
            let skip = bits.trailing_zeros();
            idx += skip;
            bits >>= skip;

            let binding = bindings[idx as usize];
            self.synchronize_buffer(binding.buffer_id, binding.device_addr, binding.size);

            let is_written = ((written_mask >> idx) & 1) != 0;
            if is_written {
                self.mark_written_buffer(binding.buffer_id, binding.device_addr, binding.size);
            }
            let _is_image = P::SEPARATE_IMAGE_BUFFER_BINDINGS && ((image_mask >> idx) & 1) != 0;
            // TODO: runtime.BindTextureBuffer / BindImageBuffer — not yet available.

            idx += 1;
            bits >>= 1;
        }
    }

    fn bind_host_transform_feedback_buffers(&mut self) {
        // Upstream: iterates transform feedback buffers if tfb is enabled.
        // Requires maxwell3d->regs.transform_feedback_enabled — not yet available.
        // We synchronize unconditionally for all non-null bindings.
        let Some(ref cs) = self.channel_state else {
            return;
        };
        let bindings: Vec<Binding> = cs.transform_feedback_buffers.to_vec();
        drop(cs);

        for binding in bindings {
            if !binding.buffer_id.is_valid() || binding.buffer_id == NULL_BUFFER_ID {
                continue;
            }
            self.touch_buffer(binding.buffer_id);
            self.synchronize_buffer(binding.buffer_id, binding.device_addr, binding.size);
            self.mark_written_buffer(binding.buffer_id, binding.device_addr, binding.size);
            // TODO: runtime.BindTransformFeedbackBuffers — not yet available.
        }
    }

    fn bind_host_compute_uniform_buffers(&mut self) {
        // Upstream: marks all uniform buffers dirty (persistent bindings), then
        // iterates and calls runtime.BindComputeUniformBuffer.
        if P::HAS_PERSISTENT_UNIFORM_BUFFER_BINDINGS {
            if let Some(ref mut cs) = self.channel_state {
                cs.dirty_uniform_buffers.fill(!0u32);
                cs.fast_bound_uniform_buffers.fill(0);
            }
        }

        let Some(ref cs) = self.channel_state else {
            return;
        };
        let mask = cs.enabled_compute_uniform_buffer_mask;
        let ub_sizes = cs.compute_uniform_buffer_sizes.clone();
        let bindings: Vec<Binding> = cs.compute_uniform_buffers.to_vec();
        drop(cs);

        let mut binding_index = 0u32;
        let mut bits = mask;
        let mut idx: u32 = 0;
        while bits != 0 {
            let skip = bits.trailing_zeros();
            idx += skip;
            bits >>= skip;

            let binding = bindings[idx as usize];
            self.touch_buffer(binding.buffer_id);
            let size = if let Some(ref sizes) = ub_sizes {
                binding.size.min(sizes[idx as usize])
            } else {
                binding.size
            };
            self.synchronize_buffer(binding.buffer_id, binding.device_addr, size);
            // TODO: runtime.BindComputeUniformBuffer — not yet available.
            if P::NEEDS_BIND_UNIFORM_INDEX {
                binding_index += 1;
            }
            let _ = binding_index;

            idx += 1;
            bits >>= 1;
        }
    }

    fn bind_host_compute_storage_buffers(&mut self) {
        let Some(ref cs) = self.channel_state else {
            return;
        };
        let mask = cs.enabled_compute_storage_buffers;
        let written_mask = cs.written_compute_storage_buffers;
        let bindings: Vec<Binding> = cs.compute_storage_buffers.to_vec();
        drop(cs);

        let mut binding_index = 0u32;
        let mut bits = mask;
        let mut idx: u32 = 0;
        while bits != 0 {
            let skip = bits.trailing_zeros();
            idx += skip;
            bits >>= skip;

            let binding = bindings[idx as usize];
            self.touch_buffer(binding.buffer_id);
            self.synchronize_buffer(binding.buffer_id, binding.device_addr, binding.size);

            let is_written = ((written_mask >> idx) & 1) != 0;
            if is_written {
                self.mark_written_buffer(binding.buffer_id, binding.device_addr, binding.size);
            }
            // TODO: runtime.BindComputeStorageBuffer — not yet available.
            if P::NEEDS_BIND_STORAGE_INDEX {
                binding_index += 1;
            }
            let _ = binding_index;

            idx += 1;
            bits >>= 1;
        }
    }

    fn bind_host_compute_texture_buffers(&mut self) {
        let Some(ref cs) = self.channel_state else {
            return;
        };
        let mask = cs.enabled_compute_texture_buffers;
        let written_mask = cs.written_compute_texture_buffers;
        let image_mask = cs.image_compute_texture_buffers;
        let bindings: Vec<TextureBufferBinding> = cs.compute_texture_buffers.to_vec();
        drop(cs);

        let mut bits = mask;
        let mut idx: u32 = 0;
        while bits != 0 {
            let skip = bits.trailing_zeros();
            idx += skip;
            bits >>= skip;

            let binding = bindings[idx as usize];
            self.synchronize_buffer(binding.buffer_id, binding.device_addr, binding.size);

            let is_written = ((written_mask >> idx) & 1) != 0;
            if is_written {
                self.mark_written_buffer(binding.buffer_id, binding.device_addr, binding.size);
            }
            let _is_image = P::SEPARATE_IMAGE_BUFFER_BINDINGS && ((image_mask >> idx) & 1) != 0;
            // TODO: runtime.BindTextureBuffer / BindImageBuffer — not yet available.

            idx += 1;
            bits >>= 1;
        }
    }

    /// Upstream: `BufferCache<P>::DoUpdateGraphicsBuffers`
    ///
    /// NOTE: UpdateIndexBuffer, UpdateVertexBuffer, UpdateDrawIndirect, and
    /// UpdateTransformFeedbackBuffer all depend on maxwell3d engine state which
    /// is not yet available. Those sub-methods are stubbed below.
    fn do_update_graphics_buffers(&mut self, is_indexed: bool) {
        self.buffer_operations(|cache| {
            if is_indexed {
                cache.update_index_buffer();
            }
            cache.update_vertex_buffers();
            cache.update_transform_feedback_buffers();
            for stage in 0..NUM_STAGES as usize {
                cache.update_uniform_buffers(stage);
                cache.update_storage_buffers(stage);
                cache.update_texture_buffers(stage);
            }
            // TODO: if current_draw_indirect → update_draw_indirect()
        });
    }

    /// Upstream: `BufferCache<P>::DoUpdateComputeBuffers`
    fn do_update_compute_buffers(&mut self) {
        self.buffer_operations(|cache| {
            cache.update_compute_uniform_buffers();
            cache.update_compute_storage_buffers();
            cache.update_compute_texture_buffers();
        });
    }

    /// Upstream: `BufferCache<P>::UpdateIndexBuffer`
    ///
    /// NOTE: Depends on maxwell3d draw_state and gpu_memory — not yet available.
    fn update_index_buffer(&mut self) {
        // TODO: full implementation reads maxwell3d draw state and resolves GPU address.
        // Stub: no-op until engine state is available.
    }

    /// Upstream: `BufferCache<P>::UpdateVertexBuffers`
    ///
    /// NOTE: Depends on maxwell3d dirty flags — not yet available.
    fn update_vertex_buffers(&mut self) {
        // TODO: check maxwell3d->dirty.flags[Dirty::VertexBuffers].
    }

    /// Upstream: `BufferCache<P>::UpdateVertexBuffer`
    fn update_vertex_buffer(&mut self, _index: u32) {
        // TODO: depends on maxwell3d vertex stream registers and gpu_memory.
    }

    /// Upstream: `BufferCache<P>::UpdateDrawIndirect`
    fn update_draw_indirect(&mut self) {
        // TODO: depends on current_draw_indirect engine state.
    }

    /// Upstream: `BufferCache<P>::UpdateUniformBuffers`
    fn update_uniform_buffers(&mut self, stage: usize) {
        let Some(ref cs) = self.channel_state else {
            return;
        };
        let mask = cs.enabled_uniform_buffer_masks[stage];
        drop(cs);

        let mut bits = mask;
        let mut idx: u32 = 0;
        while bits != 0 {
            let skip = bits.trailing_zeros();
            idx += skip;
            bits >>= skip;

            if let Some(ref cs) = self.channel_state {
                let binding = cs.uniform_buffers[stage][idx as usize];
                // If already resolved, skip.
                if binding.buffer_id.is_valid() && binding.buffer_id != NULL_BUFFER_ID {
                    idx += 1;
                    bits >>= 1;
                    continue;
                }
            }

            // Mark as dirty and resolve buffer_id.
            if P::HAS_PERSISTENT_UNIFORM_BUFFER_BINDINGS {
                if let Some(ref mut cs) = self.channel_state {
                    cs.dirty_uniform_buffers[stage] |= 1u32 << idx;
                }
            }

            let (device_addr, size) = if let Some(ref cs) = self.channel_state {
                let b = cs.uniform_buffers[stage][idx as usize];
                (b.device_addr, b.size)
            } else {
                break;
            };
            let buffer_id = self.find_buffer(device_addr, size);
            if let Some(ref mut cs) = self.channel_state {
                cs.uniform_buffers[stage][idx as usize].buffer_id = buffer_id;
            }

            idx += 1;
            bits >>= 1;
        }
    }

    /// Upstream: `BufferCache<P>::UpdateStorageBuffers`
    fn update_storage_buffers(&mut self, stage: usize) {
        let Some(ref cs) = self.channel_state else {
            return;
        };
        let mask = cs.enabled_storage_buffers[stage];
        drop(cs);

        let mut bits = mask;
        let mut idx: u32 = 0;
        while bits != 0 {
            let skip = bits.trailing_zeros();
            idx += skip;
            bits >>= skip;

            let (device_addr, size) = if let Some(ref cs) = self.channel_state {
                let b = cs.storage_buffers[stage][idx as usize];
                (b.device_addr, b.size)
            } else {
                break;
            };
            let buffer_id = self.find_buffer(device_addr, size);
            if let Some(ref mut cs) = self.channel_state {
                cs.storage_buffers[stage][idx as usize].buffer_id = buffer_id;
            }

            idx += 1;
            bits >>= 1;
        }
    }

    /// Upstream: `BufferCache<P>::UpdateTextureBuffers`
    fn update_texture_buffers(&mut self, stage: usize) {
        let Some(ref cs) = self.channel_state else {
            return;
        };
        let mask = cs.enabled_texture_buffers[stage];
        drop(cs);

        let mut bits = mask;
        let mut idx: u32 = 0;
        while bits != 0 {
            let skip = bits.trailing_zeros();
            idx += skip;
            bits >>= skip;

            let (device_addr, size) = if let Some(ref cs) = self.channel_state {
                let b = cs.texture_buffers[stage][idx as usize];
                (b.device_addr, b.size)
            } else {
                break;
            };
            let buffer_id = self.find_buffer(device_addr, size);
            if let Some(ref mut cs) = self.channel_state {
                cs.texture_buffers[stage][idx as usize].buffer_id = buffer_id;
            }

            idx += 1;
            bits >>= 1;
        }
    }

    /// Upstream: `BufferCache<P>::UpdateTransformFeedbackBuffers`
    fn update_transform_feedback_buffers(&mut self) {
        // TODO: check maxwell3d->regs.transform_feedback_enabled.
        for index in 0..NUM_TRANSFORM_FEEDBACK_BUFFERS {
            self.update_transform_feedback_buffer(index);
        }
    }

    /// Upstream: `BufferCache<P>::UpdateTransformFeedbackBuffer`
    fn update_transform_feedback_buffer(&mut self, _index: u32) {
        // TODO: depends on maxwell3d transform_feedback.buffers[index] registers and gpu_memory.
    }

    /// Upstream: `BufferCache<P>::UpdateComputeUniformBuffers`
    fn update_compute_uniform_buffers(&mut self) {
        // TODO: depends on kepler_compute->launch_description for GPU address resolution.
        let Some(ref cs) = self.channel_state else {
            return;
        };
        let mask = cs.enabled_compute_uniform_buffer_mask;
        drop(cs);

        let mut bits = mask;
        let mut idx: u32 = 0;
        while bits != 0 {
            let skip = bits.trailing_zeros();
            idx += skip;
            bits >>= skip;

            // Reset to NULL_BINDING first (upstream always resets).
            if let Some(ref mut cs) = self.channel_state {
                cs.compute_uniform_buffers[idx as usize] = NULL_BINDING;
            }
            // TODO: resolve GPU address from kepler_compute launch description.
            let (device_addr, size) = if let Some(ref cs) = self.channel_state {
                let b = cs.compute_uniform_buffers[idx as usize];
                (b.device_addr, b.size)
            } else {
                break;
            };
            let buffer_id = self.find_buffer(device_addr, size);
            if let Some(ref mut cs) = self.channel_state {
                cs.compute_uniform_buffers[idx as usize].buffer_id = buffer_id;
            }

            idx += 1;
            bits >>= 1;
        }
    }

    /// Upstream: `BufferCache<P>::UpdateComputeStorageBuffers`
    fn update_compute_storage_buffers(&mut self) {
        let Some(ref cs) = self.channel_state else {
            return;
        };
        let mask = cs.enabled_compute_storage_buffers;
        drop(cs);

        let mut bits = mask;
        let mut idx: u32 = 0;
        while bits != 0 {
            let skip = bits.trailing_zeros();
            idx += skip;
            bits >>= skip;

            let (device_addr, size) = if let Some(ref cs) = self.channel_state {
                let b = cs.compute_storage_buffers[idx as usize];
                (b.device_addr, b.size)
            } else {
                break;
            };
            let buffer_id = self.find_buffer(device_addr, size);
            if let Some(ref mut cs) = self.channel_state {
                cs.compute_storage_buffers[idx as usize].buffer_id = buffer_id;
            }

            idx += 1;
            bits >>= 1;
        }
    }

    /// Upstream: `BufferCache<P>::UpdateComputeTextureBuffers`
    fn update_compute_texture_buffers(&mut self) {
        let Some(ref cs) = self.channel_state else {
            return;
        };
        let mask = cs.enabled_compute_texture_buffers;
        drop(cs);

        let mut bits = mask;
        let mut idx: u32 = 0;
        while bits != 0 {
            let skip = bits.trailing_zeros();
            idx += skip;
            bits >>= skip;

            let (device_addr, size) = if let Some(ref cs) = self.channel_state {
                let b = cs.compute_texture_buffers[idx as usize];
                (b.device_addr, b.size)
            } else {
                break;
            };
            let buffer_id = self.find_buffer(device_addr, size);
            if let Some(ref mut cs) = self.channel_state {
                cs.compute_texture_buffers[idx as usize].buffer_id = buffer_id;
            }

            idx += 1;
            bits >>= 1;
        }
    }

    /// Mark a buffer region as GPU-written.
    ///
    /// Upstream: `BufferCache<P>::MarkWrittenBuffer`
    fn mark_written_buffer(&mut self, _buffer_id: BufferId, device_addr: VAddr, size: u32) {
        self.memory_tracker
            .mark_region_as_gpu_modified(device_addr, size as u64);
        self.gpu_modified_ranges.add(device_addr, size as usize);
        self.uncommitted_gpu_modified_ranges.add(device_addr, size as usize);
    }

    /// Find or create a buffer covering `[device_addr, device_addr+size)`.
    ///
    /// Upstream: `BufferCache<P>::FindBuffer`
    fn find_buffer(&mut self, device_addr: VAddr, size: u32) -> BufferId {
        if device_addr == 0 {
            return NULL_BUFFER_ID;
        }
        let page = device_addr >> CACHING_PAGEBITS;
        let buffer_id = self.page_table[page as usize];
        if !buffer_id.is_valid() {
            return self.create_buffer(device_addr, size);
        }
        if self.slot_buffers[buffer_id].is_in_bounds(device_addr, size as u64) {
            return buffer_id;
        }
        self.create_buffer(device_addr, size)
    }

    /// Collect all buffers that overlap `[device_addr, device_addr+wanted_size)`.
    ///
    /// Upstream: `BufferCache<P>::ResolveOverlaps`
    fn resolve_overlaps(&mut self, device_addr: VAddr, wanted_size: u32) -> OverlapResult {
        let mut overlap_ids: Vec<BufferId> = Vec::new();
        let mut begin = device_addr;
        let mut end = device_addr + wanted_size as u64;

        let max_page: u64 = 1u64 << AS_BITS;

        let expand_begin = |begin: &mut u64, addr: &mut u64, add_value: u64| {
            let min_page = CACHING_PAGESIZE + DEVICE_PAGESIZE;
            if add_value > *begin - min_page {
                *begin = min_page;
                *addr = DEVICE_PAGESIZE;
            } else {
                *begin -= add_value;
                *addr = *begin - CACHING_PAGESIZE;
            }
        };

        let expand_end = |end: &mut u64, add_value: u64| {
            if add_value > max_page - *end {
                *end = max_page;
            } else {
                *end += add_value;
            }
        };

        if begin == 0 {
            return OverlapResult {
                ids: overlap_ids,
                begin,
                end,
                has_stream_leap: false,
            };
        }

        let mut stream_score: i32 = 0;
        let mut has_stream_leap = false;
        let mut scan_addr = device_addr;

        loop {
            if scan_addr >> CACHING_PAGEBITS >= div_ceil(end, CACHING_PAGESIZE) {
                break;
            }

            let overlap_id = self.page_table[(scan_addr >> CACHING_PAGEBITS) as usize];
            if overlap_id.is_valid() && !self.slot_buffers[overlap_id].is_picked() {
                overlap_ids.push(overlap_id);
                self.slot_buffers[overlap_id].pick();

                let overlap_device_addr = self.slot_buffers[overlap_id].cpu_addr();
                let expands_left = overlap_device_addr < begin;
                if expands_left {
                    begin = overlap_device_addr;
                }
                let overlap_end =
                    overlap_device_addr + self.slot_buffers[overlap_id].size_bytes() as u64;
                let expands_right = overlap_end > end;
                if expands_right {
                    end = overlap_end;
                }

                stream_score += self.slot_buffers[overlap_id].stream_score();
                if stream_score > STREAM_LEAP_THRESHOLD && !has_stream_leap {
                    has_stream_leap = true;
                    let mut addr_copy = scan_addr;
                    if expands_right {
                        expand_begin(&mut begin, &mut addr_copy, CACHING_PAGESIZE * 128);
                    }
                    if expands_left {
                        expand_end(&mut end, CACHING_PAGESIZE * 128);
                    }
                }
            }

            let next = scan_addr.checked_add(CACHING_PAGESIZE).unwrap_or(u64::MAX);
            if next == u64::MAX {
                break;
            }
            scan_addr = next;
        }

        // Unmark picked buffers.
        for &id in &overlap_ids {
            self.slot_buffers[id].unpick();
        }

        OverlapResult {
            ids: overlap_ids,
            begin,
            end,
            has_stream_leap,
        }
    }

    /// Copy an overlapping buffer into `new_buffer_id` and delete the overlap.
    ///
    /// Upstream: `BufferCache<P>::JoinOverlap`
    ///
    /// NOTE: runtime.CopyBuffer is not yet available; we accumulate stream score only.
    fn join_overlap(
        &mut self,
        new_buffer_id: BufferId,
        overlap_id: BufferId,
        accumulate_stream_score: bool,
    ) {
        if accumulate_stream_score {
            let score = self.slot_buffers[overlap_id].stream_score() + 1;
            self.slot_buffers[new_buffer_id].increase_stream_score(score);
        }
        // TODO: runtime.CopyBuffer(new_buffer, overlap, copies, true) — not yet available.
        // Data transfer from overlap to new_buffer is skipped for now.
        self.delete_buffer(overlap_id, true);
    }

    /// Allocate a new buffer that covers `[device_addr, device_addr+wanted_size)`,
    /// merging any overlapping buffers.
    ///
    /// Upstream: `BufferCache<P>::CreateBuffer`
    ///
    /// NOTE: runtime.ClearBuffer is not yet available — the GPU-side clear is skipped.
    fn create_buffer(&mut self, device_addr: VAddr, wanted_size: u32) -> BufferId {
        // Align start and end to caching page boundaries.
        let device_addr_end =
            (device_addr + wanted_size as u64 + CACHING_PAGESIZE - 1) & !(CACHING_PAGESIZE - 1);
        let device_addr = device_addr & !(CACHING_PAGESIZE - 1);
        let wanted_size = (device_addr_end - device_addr) as u32;

        let overlap = self.resolve_overlaps(device_addr, wanted_size);
        let size = (overlap.end - overlap.begin) as u32;

        let new_buffer_id = self.slot_buffers.insert(BufferBase::new(overlap.begin, size as u64));
        // TODO: runtime.ClearBuffer(new_buffer, 0, size_bytes, 0) — not yet available.

        let overlap_ids: Vec<BufferId> = overlap.ids.clone();
        let has_stream_leap = overlap.has_stream_leap;
        for overlap_id in overlap_ids {
            self.join_overlap(new_buffer_id, overlap_id, !has_stream_leap);
        }

        self.register(new_buffer_id);
        self.touch_buffer(new_buffer_id);
        new_buffer_id
    }

    /// Register a buffer in the page table and update memory accounting.
    ///
    /// Upstream: `BufferCache<P>::Register`
    fn register(&mut self, buffer_id: BufferId) {
        self.change_register(buffer_id, true);
    }

    /// Unregister a buffer from the page table and update memory accounting.
    ///
    /// Upstream: `BufferCache<P>::Unregister`
    fn unregister(&mut self, buffer_id: BufferId) {
        self.change_register(buffer_id, false);
    }

    /// Insert or remove a buffer from the page table.
    ///
    /// Upstream: `BufferCache<P>::ChangeRegister<insert>`
    fn change_register(&mut self, buffer_id: BufferId, insert: bool) {
        let (device_addr_begin, size) = {
            let buffer = &self.slot_buffers[buffer_id];
            (buffer.cpu_addr(), buffer.size_bytes())
        };

        if insert {
            self.total_used_memory += (size + 1023) as u64 & !1023u64; // AlignUp(size, 1024)
            let lru_id = self.lru_cache.insert(buffer_id, self.frame_tick as i64);
            self.slot_buffers[buffer_id].set_lru_id(lru_id);
        } else {
            let aligned = (size + 1023) as u64 & !1023u64;
            self.total_used_memory = self.total_used_memory.saturating_sub(aligned);
            let lru_id = self.slot_buffers[buffer_id].get_lru_id();
            self.lru_cache.free(lru_id);
        }

        let device_addr_end = device_addr_begin + size as u64;
        let page_begin = device_addr_begin / CACHING_PAGESIZE;
        let page_end = div_ceil(device_addr_end, CACHING_PAGESIZE);

        for page in page_begin..page_end {
            if insert {
                self.page_table[page as usize] = buffer_id;
            } else {
                self.page_table[page as usize] = SlotId::invalid();
            }
        }
    }

    /// Update the LRU position of a buffer.
    ///
    /// Upstream: `BufferCache<P>::TouchBuffer`
    fn touch_buffer(&mut self, buffer_id: BufferId) {
        if buffer_id != NULL_BUFFER_ID && buffer_id.is_valid() {
            let lru_id = self.slot_buffers[buffer_id].get_lru_id();
            self.lru_cache.touch(lru_id, self.frame_tick as i64);
        }
    }

    /// Synchronize CPU-modified data to the GPU buffer.
    ///
    /// Upstream: `BufferCache<P>::SynchronizeBuffer`
    ///
    /// Returns `true` if no upload was needed (region was already clean).
    fn synchronize_buffer(&mut self, buffer_id: BufferId, device_addr: VAddr, size: u32) -> bool {
        let mut copies: Vec<BufferCopy> = Vec::new();
        let mut total_size_bytes: u64 = 0;
        let mut largest_copy: u64 = 0;

        let buffer_start = self.slot_buffers[buffer_id].cpu_addr();

        self.memory_tracker.for_each_upload_range(
            device_addr,
            size as u64,
            &mut |device_addr_out, range_size| {
                copies.push(BufferCopy {
                    src_offset: total_size_bytes,
                    dst_offset: device_addr_out - buffer_start,
                    size: range_size,
                });
                total_size_bytes += range_size;
                largest_copy = largest_copy.max(range_size);
            },
        );

        if total_size_bytes == 0 {
            return true;
        }

        self.upload_memory(buffer_id, total_size_bytes, largest_copy, &mut copies);
        false
    }

    /// Upload CPU data to a GPU buffer.
    ///
    /// Upstream: `BufferCache<P>::UploadMemory`
    fn upload_memory(
        &mut self,
        buffer_id: BufferId,
        total_size_bytes: u64,
        largest_copy: u64,
        copies: &mut [BufferCopy],
    ) {
        if P::USE_MEMORY_MAPS_FOR_UPLOADS {
            self.mapped_upload_memory(buffer_id, total_size_bytes, copies);
        } else {
            self.immediate_upload_memory(buffer_id, largest_copy, copies);
        }
    }

    /// Upload memory via direct buffer writes.
    ///
    /// Upstream: `BufferCache<P>::ImmediateUploadMemory`
    ///
    /// NOTE: `device_memory.GetPointer` and `buffer.ImmediateUpload` are not yet available.
    fn immediate_upload_memory(
        &mut self,
        _buffer_id: BufferId,
        _largest_copy: u64,
        _copies: &[BufferCopy],
    ) {
        if P::USE_MEMORY_MAPS_FOR_UPLOADS {
            return; // This path is only for the non-memory-map case.
        }
        // TODO: device_memory.GetPointer / ReadBlockUnsafe — not yet available.
        // Upstream reads CPU memory and calls buffer.ImmediateUpload(copy.dst_offset, span).
    }

    /// Upload memory via staging buffer.
    ///
    /// Upstream: `BufferCache<P>::MappedUploadMemory`
    ///
    /// NOTE: runtime.UploadStagingBuffer and runtime.CopyBuffer are not yet available.
    fn mapped_upload_memory(
        &mut self,
        _buffer_id: BufferId,
        _total_size_bytes: u64,
        _copies: &mut [BufferCopy],
    ) {
        if !P::USE_MEMORY_MAPS {
            return;
        }
        // TODO: runtime.UploadStagingBuffer, device_memory.ReadBlockUnsafe, runtime.CopyBuffer.
    }

    /// Download buffer memory back to the CPU (full buffer).
    ///
    /// Upstream: `BufferCache<P>::DownloadBufferMemory(Buffer&)`
    fn download_buffer_memory(&mut self, buffer_id: BufferId) {
        let (cpu_addr, size_bytes) = {
            let b = &self.slot_buffers[buffer_id];
            (b.cpu_addr(), b.size_bytes() as u64)
        };
        self.download_buffer_memory_range(buffer_id, cpu_addr, size_bytes);
    }

    /// Download a sub-range of buffer memory back to the CPU.
    ///
    /// Upstream: `BufferCache<P>::DownloadBufferMemory(Buffer&, DAddr, u64)`
    ///
    /// NOTE: runtime memory maps, staging buffer, and device_memory.WriteBlockUnsafe
    /// are not yet available.
    fn download_buffer_memory_range(
        &mut self,
        buffer_id: BufferId,
        device_addr: VAddr,
        size: u64,
    ) {
        // Collect the ranges that need to be downloaded via memory_tracker.
        // We split the logic into two phases to avoid the borrow conflict:
        // Phase 1: collect download ranges from memory_tracker (borrows memory_tracker).
        // Phase 2: apply range subtractions and build copy list (borrows gpu_modified_ranges).
        let buffer_addr = self.slot_buffers[buffer_id].cpu_addr();

        let mut download_ranges: Vec<(VAddr, u64)> = Vec::new();
        self.memory_tracker.for_each_download_range_and_clear(
            device_addr,
            size,
            &mut |device_addr_out: VAddr, range_size: u64| {
                download_ranges.push((device_addr_out, range_size));
            },
        );

        let mut copies: Vec<BufferCopy> = Vec::new();
        let mut total_size_bytes: u64 = 0;
        let mut largest_copy: u64 = 0;

        for (device_addr_out, range_size) in download_ranges {
            // Iterate GPU-modified sub-ranges using RangeSet::for_each_in_range.
            let mut sub_intervals: Vec<(VAddr, VAddr)> = Vec::new();
            self.gpu_modified_ranges.for_each_in_range(
                device_addr_out,
                range_size as usize,
                |new_start, new_end| {
                    sub_intervals.push((new_start, new_end));
                },
            );

            for (new_start, new_end) in sub_intervals {
                let new_offset = new_start - buffer_addr;
                let new_size = new_end - new_start;
                copies.push(BufferCopy {
                    src_offset: new_offset,
                    dst_offset: total_size_bytes,
                    size: new_size,
                });
                constexpr_align_up(&mut total_size_bytes, new_size, 64);
                largest_copy = largest_copy.max(new_size);
            }

            self.clear_download(device_addr_out, range_size);
            self.gpu_modified_ranges.subtract(device_addr_out, range_size as usize);
        }

        if total_size_bytes == 0 {
            return;
        }

        if DISABLE_DOWNLOADS {
            return;
        }

        // TODO: USE_MEMORY_MAPS path and non-mapped path both require runtime/device_memory.
        let _ = largest_copy;
    }

    /// Delete a buffer, cleaning up all state that references it.
    ///
    /// Upstream: `BufferCache<P>::DeleteBuffer`
    fn delete_buffer(&mut self, buffer_id: BufferId, do_not_mark: bool) {
        let Some(ref mut cs) = self.channel_state else {
            return;
        };

        // Clear any bindings that reference this buffer.
        if cs.index_buffer.buffer_id == buffer_id {
            cs.index_buffer.buffer_id = SlotId::invalid();
        }
        for binding in cs.vertex_buffers.iter_mut() {
            if binding.buffer_id == buffer_id {
                binding.buffer_id = SlotId::invalid();
            }
        }
        for stage_buffers in cs.uniform_buffers.iter_mut() {
            for binding in stage_buffers.iter_mut() {
                if binding.buffer_id == buffer_id {
                    binding.buffer_id = SlotId::invalid();
                }
            }
        }
        for stage_buffers in cs.storage_buffers.iter_mut() {
            for binding in stage_buffers.iter_mut() {
                if binding.buffer_id == buffer_id {
                    binding.buffer_id = SlotId::invalid();
                }
            }
        }
        for binding in cs.transform_feedback_buffers.iter_mut() {
            if binding.buffer_id == buffer_id {
                binding.buffer_id = SlotId::invalid();
            }
        }
        for binding in cs.compute_uniform_buffers.iter_mut() {
            if binding.buffer_id == buffer_id {
                binding.buffer_id = SlotId::invalid();
            }
        }
        for binding in cs.compute_storage_buffers.iter_mut() {
            if binding.buffer_id == buffer_id {
                binding.buffer_id = SlotId::invalid();
            }
        }

        if P::HAS_PERSISTENT_UNIFORM_BUFFER_BINDINGS {
            cs.dirty_uniform_buffers.fill(!0u32);
            cs.uniform_buffer_binding_sizes = [[0; NUM_GRAPHICS_UNIFORM_BUFFERS as usize];
                NUM_STAGES as usize];
        }

        cs.has_deleted_buffers = true;
        drop(cs); // release borrow before calling methods that need &mut self

        // Mark the whole buffer as CPU-modified to stop tracking.
        if !do_not_mark {
            let (cpu_addr, size_bytes) = {
                let b = &self.slot_buffers[buffer_id];
                (b.cpu_addr(), b.size_bytes() as u64)
            };
            self.memory_tracker.mark_region_as_cpu_modified(cpu_addr, size_bytes);
        }

        self.unregister(buffer_id);
        let buffer = self.slot_buffers.take(buffer_id);
        self.delayed_destruction_ring.push(buffer);
    }

    /// Build a storage buffer binding from a GPU virtual SSBO address.
    ///
    /// Upstream: `BufferCache<P>::StorageBufferBinding`
    ///
    /// NOTE: gpu_memory read operations are not yet available. Returns NULL_BINDING.
    fn storage_buffer_binding(
        &self,
        _ssbo_addr: u64,
        cbuf_index: u32,
        _is_written: bool,
    ) -> Binding {
        // TODO: gpu_memory->Read<u64>(ssbo_addr) and gpu_memory->GpuToCpuAddress — not yet ported.
        log::warn!(
            "storage_buffer_binding: GPU memory read not available for cbuf_index {}",
            cbuf_index
        );
        NULL_BINDING
    }

    /// Build a texture buffer binding from a GPU virtual address.
    ///
    /// Upstream: `BufferCache<P>::GetTextureBufferBinding`
    ///
    /// NOTE: gpu_memory->GpuToCpuAddress is not yet available. Stores gpu_addr directly.
    fn get_texture_buffer_binding(
        &mut self,
        gpu_addr: u64,
        size: u32,
        format: u32,
    ) -> TextureBufferBinding {
        // TODO: translate gpu_addr via gpu_memory->GpuToCpuAddress.
        if gpu_addr == 0 || size == 0 {
            TextureBufferBinding {
                device_addr: 0,
                size: 0,
                buffer_id: NULL_BUFFER_ID,
                format: 0,
            }
        } else {
            TextureBufferBinding {
                device_addr: gpu_addr, // placeholder until GPU memory is available
                size,
                buffer_id: NULL_BUFFER_ID,
                format,
            }
        }
    }

    /// Get an immediate buffer slice backed by device memory at `device_addr`.
    ///
    /// Upstream: `BufferCache<P>::ImmediateBufferWithData`
    ///
    /// NOTE: device_memory.GetPointer is not yet available. Returns empty slice.
    fn immediate_buffer_with_data(&mut self, _device_addr: VAddr, _size: usize) -> &[u8] {
        // TODO: device_memory.GetPointer — not yet available.
        &[]
    }

    /// Ensure `immediate_buffer_alloc` has at least `wanted_capacity` bytes and return a slice.
    ///
    /// Upstream: `BufferCache<P>::ImmediateBuffer`
    fn immediate_buffer(&mut self, wanted_capacity: usize) -> &mut [u8] {
        if self.immediate_buffer_alloc.len() < wanted_capacity {
            self.immediate_buffer_alloc.resize(wanted_capacity, 0u8);
        }
        &mut self.immediate_buffer_alloc[..wanted_capacity]
    }

    /// Return true if a fast uniform buffer is currently bound at `(stage, binding_index)`.
    ///
    /// Upstream: `BufferCache<P>::HasFastUniformBufferBound`
    fn has_fast_uniform_buffer_bound(&self, stage: usize, binding_index: u32) -> bool {
        if P::IS_OPENGL {
            self.channel_state.as_ref().map_or(false, |cs| {
                ((cs.fast_bound_uniform_buffers[stage] >> binding_index) & 1) != 0
            })
        } else {
            // Only OpenGL has fast uniform buffers.
            false
        }
    }

    /// Remove `[base_addr, base_addr+size)` from all download tracking structures.
    ///
    /// Upstream: `BufferCache<P>::ClearDownload`
    fn clear_download(&mut self, base_addr: VAddr, size: u64) {
        // TODO: async_downloads.DeleteAll — requires OverlapRangeSet field.
        self.uncommitted_gpu_modified_ranges.subtract(base_addr, size as usize);
        for range_set in self.committed_gpu_modified_ranges.iter_mut() {
            range_set.subtract(base_addr, size as usize);
        }
    }

    /// Perform the inline memory write into the buffer cache.
    ///
    /// Upstream: `BufferCache<P>::InlineMemoryImplementation`
    ///
    /// NOTE: runtime.UploadStagingBuffer and buffer.ImmediateUpload are not yet available.
    fn inline_memory_implementation(
        &mut self,
        dest_address: VAddr,
        copy_size: usize,
        _inlined_buffer: &[u8],
    ) {
        self.clear_download(dest_address, copy_size as u64);
        self.gpu_modified_ranges.subtract(dest_address, copy_size);

        let buffer_id = self.find_buffer(dest_address, copy_size as u32);
        self.synchronize_buffer(buffer_id, dest_address, copy_size as u32);

        // TODO: USE_MEMORY_MAPS_FOR_UPLOADS path: runtime.UploadStagingBuffer + CopyBuffer.
        // TODO: non-mapped path: buffer.ImmediateUpload(buffer.Offset(dest_address), inlined_buffer).
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Align `total` up by `new_size` rounded to `align`.
#[inline]
fn constexpr_align_up(total: &mut u64, new_size: u64, align: u64) {
    *total += (new_size + align - 1) & !(align - 1);
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

    #[test]
    fn test_tick_frame_no_channel() {
        let tracker = DummyTracker;
        let mut cache = BufferCache::<TestParams, DummyTracker>::new(&tracker);
        // Should return early without panicking when channel_state is None.
        cache.tick_frame();
        assert_eq!(cache.frame_tick, 0);
    }

    #[test]
    fn test_write_memory_marks_cpu_modified() {
        let tracker = DummyTracker;
        let mut cache = BufferCache::<TestParams, DummyTracker>::new(&tracker);
        // write_memory should not panic.
        cache.write_memory(0x10000, 0x1000);
    }

    #[test]
    fn test_on_cpu_write_unregistered() {
        let tracker = DummyTracker;
        let mut cache = BufferCache::<TestParams, DummyTracker>::new(&tracker);
        // An unregistered region should return false (no GPU data to flush).
        let result = cache.on_cpu_write(0x20000, 0x100);
        assert!(!result);
    }

    #[test]
    fn test_get_flush_area() {
        let tracker = DummyTracker;
        let mut cache = BufferCache::<TestParams, DummyTracker>::new(&tracker);
        let area = cache.get_flush_area(0x1234, 0x100);
        assert!(area.is_some());
        let a = area.unwrap();
        // Should be aligned to DEVICE_PAGESIZE (4096).
        assert_eq!(a.start_address % DEVICE_PAGESIZE, 0);
        assert_eq!(a.end_address % DEVICE_PAGESIZE, 0);
        assert!(a.start_address <= 0x1234);
        assert!(a.end_address >= 0x1234 + 0x100);
    }

    #[test]
    fn test_accumulate_flushes() {
        let tracker = DummyTracker;
        let mut cache = BufferCache::<TestParams, DummyTracker>::new(&tracker);
        // Add something to uncommitted ranges.
        cache.uncommitted_gpu_modified_ranges.add(0x1000, 0x1000);
        assert!(cache.has_uncommitted_flushes());
        cache.accumulate_flushes();
        assert!(!cache.has_uncommitted_flushes());
        assert!(cache.should_wait_async_flushes());
    }

    #[test]
    fn test_disable_graphics_uniform_buffer() {
        let tracker = DummyTracker;
        let mut cache = BufferCache::<TestParams, DummyTracker>::new(&tracker);
        cache.channel_state = Some(Box::new(BufferCacheChannelInfo::default()));
        cache.disable_graphics_uniform_buffer(0, 0);
        let cs = cache.channel_state.as_ref().unwrap();
        assert_eq!(cs.uniform_buffers[0][0].device_addr, 0);
        assert_eq!(cs.uniform_buffers[0][0].buffer_id, NULL_BUFFER_ID);
    }

    #[test]
    fn test_unbind_graphics_storage_buffers() {
        let tracker = DummyTracker;
        let mut cache = BufferCache::<TestParams, DummyTracker>::new(&tracker);
        cache.channel_state = Some(Box::new(BufferCacheChannelInfo::default()));
        if let Some(ref mut cs) = cache.channel_state {
            cs.enabled_storage_buffers[0] = 0xFF;
            cs.written_storage_buffers[0] = 0x0F;
        }
        cache.unbind_graphics_storage_buffers(0);
        let cs = cache.channel_state.as_ref().unwrap();
        assert_eq!(cs.enabled_storage_buffers[0], 0);
        assert_eq!(cs.written_storage_buffers[0], 0);
    }

    #[test]
    fn test_unbind_compute_storage_buffers() {
        let tracker = DummyTracker;
        let mut cache = BufferCache::<TestParams, DummyTracker>::new(&tracker);
        cache.channel_state = Some(Box::new(BufferCacheChannelInfo::default()));
        if let Some(ref mut cs) = cache.channel_state {
            cs.enabled_compute_storage_buffers = 0xFF;
            cs.written_compute_storage_buffers = 0x0F;
        }
        cache.unbind_compute_storage_buffers();
        let cs = cache.channel_state.as_ref().unwrap();
        assert_eq!(cs.enabled_compute_storage_buffers, 0);
        assert_eq!(cs.written_compute_storage_buffers, 0);
    }

    #[test]
    fn test_range_subtract() {
        let mut rs = RangeSet::new();
        rs.add(0, 100);
        rs.subtract(20, 30);
        // Should split into [0, 20) and [50, 100).
        let mut result = Vec::new();
        rs.for_each(|s, e| result.push((s, e)));
        assert_eq!(result, vec![(0, 20), (50, 100)]);
    }

    #[test]
    fn test_range_subtract_full_removal() {
        let mut rs = RangeSet::new();
        rs.add(10, 40);
        rs.subtract(0, 100);
        assert!(rs.empty());
    }

    #[test]
    fn test_is_region_registered_empty() {
        let tracker = DummyTracker;
        let cache = BufferCache::<TestParams, DummyTracker>::new(&tracker);
        assert!(!cache.is_region_registered(0x1000, 0x100));
    }

    #[test]
    fn test_find_buffer_null_addr() {
        let tracker = DummyTracker;
        let mut cache = BufferCache::<TestParams, DummyTracker>::new(&tracker);
        let id = cache.find_buffer(0, 0x100);
        assert_eq!(id, NULL_BUFFER_ID);
    }

    #[test]
    fn test_create_and_find_buffer() {
        let tracker = DummyTracker;
        let mut cache = BufferCache::<TestParams, DummyTracker>::new(&tracker);
        let addr = 0x0001_0000u64;
        let size = 0x1000u32;
        let id1 = cache.find_buffer(addr, size);
        // Finding again should return the same buffer.
        let id2 = cache.find_buffer(addr, size);
        assert_eq!(id1, id2);
        assert_ne!(id1, NULL_BUFFER_ID);
    }

    #[test]
    fn test_inline_memory_unregistered() {
        let tracker = DummyTracker;
        let mut cache = BufferCache::<TestParams, DummyTracker>::new(&tracker);
        // Unregistered region should return false.
        let result = cache.inline_memory(0x5000, 0x10, &[0u8; 16]);
        assert!(!result);
    }
}
