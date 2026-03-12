// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/texture_cache/texture_cache_base.h
//!
//! Defines the base data structures for the texture cache: channel info,
//! slot vectors, pending downloads, join-caching state, and the generic
//! `TextureCache<P>` skeleton.
//!
//! The upstream file is a ~510-line header that combines:
//!   - `TextureCacheChannelInfo` (per-channel descriptor tables)
//!   - `TextureCache<P>` template class definition
//!   - supporting inner types (`BlitImages`, `PendingDownload`, etc.)
//!
//! The template implementation lives in texture_cache.h (texture_cache.rs).

use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::{Arc, Mutex};

use common::slot_vector::SlotVector;

use super::descriptor_table::DescriptorTable;
use super::image_base::{GPUVAddr, ImageAllocBase, ImageBase, ImageMapView};
use super::image_view_base::ImageViewBase;
use super::render_targets::RenderTargets;
use super::types::*;

// ── Constants ──────────────────────────────────────────────────────────

/// Address shift for caching images into a hash table.
const YUZU_PAGEBITS: u64 = 20;

// ── ImageViewInOut ─────────────────────────────────────────────────────

/// In/out parameter for batch image-view resolution.
///
/// Port of `VideoCommon::ImageViewInOut`.
#[derive(Debug, Clone, Copy, Default)]
pub struct ImageViewInOut {
    pub index: u32,
    pub blacklist: bool,
    pub id: ImageViewId,
}

// ── AsyncDecodeContext ─────────────────────────────────────────────────

/// State for an in-flight asynchronous texture decode.
///
/// Port of `VideoCommon::AsyncDecodeContext`.
pub struct AsyncDecodeContext {
    pub image_id: ImageId,
    pub decoded_data: Vec<u8>,
    pub copies: Vec<BufferImageCopy>,
    pub mutex: Mutex<()>,
    pub complete: std::sync::atomic::AtomicBool,
}

// ── TextureCacheGPUMap ─────────────────────────────────────────────────

/// GPU page table: maps a 20-bit-shifted GPU address to a vec of image ids.
///
/// Port of `VideoCommon::TextureCacheGPUMap`.
pub type TextureCacheGPUMap = HashMap<u64, Vec<ImageId>>;

// ── TextureCacheChannelInfo ────────────────────────────────────────────

/// Per-channel state for the texture cache.
///
/// Port of `VideoCommon::TextureCacheChannelInfo`.
///
/// The upstream class inherits from `ChannelInfo` and holds descriptor
/// tables plus cached sampler/image-view mappings.
pub struct TextureCacheChannelInfo {
    // Descriptor tables — using a dummy `u64` descriptor type as a
    // placeholder until TICEntry / TSCEntry are ported.
    pub graphics_image_table: DescriptorTable<u64>,
    pub graphics_sampler_table: DescriptorTable<u64>,
    pub graphics_sampler_ids: Vec<SamplerId>,
    pub graphics_image_view_ids: Vec<ImageViewId>,

    pub compute_image_table: DescriptorTable<u64>,
    pub compute_sampler_table: DescriptorTable<u64>,
    pub compute_sampler_ids: Vec<SamplerId>,
    pub compute_image_view_ids: Vec<ImageViewId>,

    // Per-channel caches
    pub image_views: HashMap<u64, ImageViewId>, // keyed by TICEntry hash
    pub samplers: HashMap<u64, SamplerId>,      // keyed by TSCEntry hash

    pub gpu_page_table: Option<Box<TextureCacheGPUMap>>,
    pub sparse_page_table: Option<Box<TextureCacheGPUMap>>,
}

impl TextureCacheChannelInfo {
    pub fn new() -> Self {
        Self {
            graphics_image_table: DescriptorTable::new(),
            graphics_sampler_table: DescriptorTable::new(),
            graphics_sampler_ids: Vec::new(),
            graphics_image_view_ids: Vec::new(),
            compute_image_table: DescriptorTable::new(),
            compute_sampler_table: DescriptorTable::new(),
            compute_sampler_ids: Vec::new(),
            compute_image_view_ids: Vec::new(),
            image_views: HashMap::new(),
            samplers: HashMap::new(),
            gpu_page_table: None,
            sparse_page_table: None,
        }
    }
}

// ── BlitImages (private helper) ────────────────────────────────────────

/// Identifies a source/destination image pair for a blit.
///
/// Port of `TextureCache::BlitImages`.
#[derive(Debug, Clone, Copy)]
pub struct BlitImages {
    pub dst_id: ImageId,
    pub src_id: ImageId,
    pub dst_format: super::format_lookup_table::PixelFormat,
    pub src_format: super::format_lookup_table::PixelFormat,
}

// ── PendingDownload / BufferDownload ───────────────────────────────────

#[derive(Debug, Clone, Copy)]
pub struct BufferDownload {
    pub address: GPUVAddr,
    pub size: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct PendingDownload {
    pub is_swizzle: bool,
    pub async_buffer_id: usize,
    pub object_id: common::slot_vector::SlotId,
}

// ── JoinCopy ───────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy)]
pub struct JoinCopy {
    pub is_alias: bool,
    pub id: ImageId,
}

// ── TextureCache<P> ────────────────────────────────────────────────────

/// Memory thresholds (from upstream `TextureCache` template).
const TARGET_THRESHOLD: i64 = 4 * 1024 * 1024 * 1024; // 4 GiB
const DEFAULT_EXPECTED_MEMORY: i64 = 1024 * 1024 * 1024 + 125 * 1024 * 1024; // 1 GiB + 125 MiB
const DEFAULT_CRITICAL_MEMORY: i64 = 1024 * 1024 * 1024 + 625 * 1024 * 1024; // 1 GiB + 625 MiB
const GC_EMERGENCY_COUNTS: usize = 2;
const TICKS_TO_DESTROY: usize = 8;
const UNSET_CHANNEL: usize = usize::MAX;

/// The main texture cache.
///
/// Port of the `TextureCache<P>` template class from texture_cache_base.h.
/// `P` in upstream is a policy class providing associated types
/// (`Runtime`, `Image`, `ImageView`, `Sampler`, `Framebuffer`, etc.).
///
/// This Rust version uses concrete placeholder types for now;
/// the generic parameter approach will be refined as backend types are ported.
pub struct TextureCacheBase {
    // Slot storage
    pub slot_images: SlotVector<ImageBase>,
    pub slot_map_views: SlotVector<ImageMapView>,
    pub slot_image_views: SlotVector<ImageViewBase>,
    pub slot_image_allocs: SlotVector<ImageAllocBase>,
    // slot_samplers, slot_framebuffers: need backend types

    // Render state
    pub render_targets: RenderTargets,
    pub framebuffers: HashMap<u64, FramebufferId>, // keyed by RenderTargets hash

    // Page tables
    pub page_table: HashMap<u64, Vec<ImageMapId>>,
    pub sparse_views: HashMap<ImageId, Vec<ImageViewId>>,

    // Memory tracking
    pub has_deleted_images: bool,
    pub is_rescaling: bool,
    pub total_used_memory: u64,
    pub minimum_memory: u64,
    pub expected_memory: u64,
    pub critical_memory: u64,

    // Download tracking
    pub uncommitted_downloads: Vec<PendingDownload>,
    pub committed_downloads: VecDeque<Vec<PendingDownload>>,

    // Modification tick
    pub modification_tick: u64,
    pub frame_tick: u64,

    // Async decode
    pub async_decodes: Vec<Box<AsyncDecodeContext>>,

    // Join caching
    pub join_overlap_ids: Vec<ImageId>,
    pub join_overlaps_found: HashSet<ImageId>,
    pub join_left_aliased_ids: Vec<ImageId>,
    pub join_right_aliased_ids: Vec<ImageId>,
    pub join_ignore_textures: HashSet<ImageId>,
    pub join_bad_overlap_ids: Vec<ImageId>,
    pub join_copies_to_do: Vec<JoinCopy>,
    pub join_alias_indices: HashMap<ImageId, usize>,

    // Image alloc table
    pub image_allocs_table: HashMap<GPUVAddr, ImageAllocId>,

    // Scratch buffers
    pub swizzle_data_buffer: Vec<u8>,
    pub unswizzle_data_buffer: Vec<u8>,

    // Mutex
    pub mutex: std::sync::Mutex<()>,
}

impl TextureCacheBase {
    /// Create a new texture cache (placeholder constructor).
    ///
    /// Port of `TextureCache<P>::TextureCache(Runtime&, MaxwellDeviceMemoryManager&)`.
    pub fn new() -> Self {
        Self {
            slot_images: SlotVector::new(),
            slot_map_views: SlotVector::new(),
            slot_image_views: SlotVector::new(),
            slot_image_allocs: SlotVector::new(),
            render_targets: RenderTargets::default(),
            framebuffers: HashMap::new(),
            page_table: HashMap::new(),
            sparse_views: HashMap::new(),
            has_deleted_images: false,
            is_rescaling: false,
            total_used_memory: 0,
            minimum_memory: 0,
            expected_memory: DEFAULT_EXPECTED_MEMORY as u64 + 512 * 1024 * 1024,
            critical_memory: DEFAULT_CRITICAL_MEMORY as u64 + 1024 * 1024 * 1024,
            uncommitted_downloads: Vec::new(),
            committed_downloads: VecDeque::new(),
            modification_tick: 0,
            frame_tick: 0,
            async_decodes: Vec::new(),
            join_overlap_ids: Vec::new(),
            join_overlaps_found: HashSet::new(),
            join_left_aliased_ids: Vec::new(),
            join_right_aliased_ids: Vec::new(),
            join_ignore_textures: HashSet::new(),
            join_bad_overlap_ids: Vec::new(),
            join_copies_to_do: Vec::new(),
            join_alias_indices: HashMap::new(),
            image_allocs_table: HashMap::new(),
            swizzle_data_buffer: vec![0u8; 8 * 1024 * 1024], // 8 MiB
            unswizzle_data_buffer: vec![0u8; 1 * 1024 * 1024], // 1 MiB
            mutex: std::sync::Mutex::new(()),
        }
    }

    /// Notify the cache that a new frame has been queued.
    ///
    /// Port of `TextureCache<P>::TickFrame`.
    pub fn tick_frame(&mut self) {
        todo!("tick_frame")
    }

    /// Mark images in a range as modified from the CPU.
    ///
    /// Port of `TextureCache<P>::WriteMemory`.
    pub fn write_memory(&mut self, _cpu_addr: u64, _size: usize) {
        todo!("write_memory")
    }

    /// Download contents of host images to guest memory in a region.
    ///
    /// Port of `TextureCache<P>::DownloadMemory`.
    pub fn download_memory(&mut self, _cpu_addr: u64, _size: usize) {
        todo!("download_memory")
    }

    /// Remove images in a region.
    ///
    /// Port of `TextureCache<P>::UnmapMemory`.
    pub fn unmap_memory(&mut self, _cpu_addr: u64, _size: usize) {
        todo!("unmap_memory")
    }

    /// Return true when there are uncommitted images to be downloaded.
    pub fn has_uncommitted_flushes(&self) -> bool {
        !self.uncommitted_downloads.is_empty()
    }

    /// Return true when the caller should wait for async downloads.
    pub fn should_wait_async_flushes(&self) -> bool {
        !self.committed_downloads.is_empty()
    }

    /// Commit asynchronous downloads.
    pub fn commit_async_flushes(&mut self) {
        todo!("commit_async_flushes")
    }

    /// Pop asynchronous downloads.
    pub fn pop_async_flushes(&mut self) {
        todo!("pop_async_flushes")
    }

    // ── Page iteration helpers ─────────────────────────────────────────

    /// Iterate over all page indices in a CPU address range.
    pub fn for_each_cpu_page(addr: u64, size: usize, mut func: impl FnMut(u64)) {
        let page_end = (addr + size as u64 - 1) >> YUZU_PAGEBITS;
        let mut page = addr >> YUZU_PAGEBITS;
        while page <= page_end {
            func(page);
            page += 1;
        }
    }

    /// Iterate over all page indices in a GPU address range.
    pub fn for_each_gpu_page(addr: GPUVAddr, size: usize, mut func: impl FnMut(u64)) {
        let page_end = (addr + size as u64 - 1) >> YUZU_PAGEBITS;
        let mut page = addr >> YUZU_PAGEBITS;
        while page <= page_end {
            func(page);
            page += 1;
        }
    }
}
