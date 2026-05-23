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

use parking_lot::ReentrantMutex;

use common::slot_vector::SlotVector;

use crate::framebuffer_config::{BlendMode, FramebufferConfig};
use crate::renderer_base::GuestMemoryWriter;

use super::descriptor_table::DescriptorTable;
use super::format_lookup_table::PixelFormat;
use super::image_base::{
    GPUVAddr, ImageAllocBase, ImageBase, ImageFlagBits, ImageMapView, NullImageParams,
};
use super::image_view_base::{ImageViewBase, NullImageViewParams};
use super::image_view_info::{ImageViewInfo, SwizzleSource};
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

/// Backend-independent result of `TextureCache<P>::TryFindFramebufferImageView`.
///
/// Upstream returns `P::ImageView*` plus the rescale flag. The Rust cache still
/// stores only `ImageViewBase`, so the OpenGL backend maps `view_id` to its
/// backend image-view handle when that storage is available.
#[derive(Debug, Clone)]
pub struct FramebufferImageView {
    pub view_id: ImageViewId,
    pub view: ImageViewBase,
    pub scaled: bool,
}

pub type ImageDownloader =
    Arc<dyn Fn(ImageId, &ImageBase, &mut [u8]) -> bool + Send + Sync + 'static>;

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

// ── DescriptorSyncRegs ─────────────────────────────────────────────────

/// Snapshot of the Maxwell3D registers consumed by
/// `TextureCacheBase::synchronize_graphics_descriptors`.
///
/// Mirrors the fields upstream `KConditionVariable<P>::SynchronizeGraphicsDescriptors`
/// reads off `maxwell3d->regs`:
/// * `regs.sampler_binding == SamplerBinding::ViaHeaderBinding`
/// * `regs.tex_header.limit` / `regs.tex_header.Address()`
/// * `regs.tex_sampler.limit` / `regs.tex_sampler.Address()`
///
/// Passing a snapshot keeps the texture cache from needing a back-reference
/// to `Maxwell3D` (which is owned by the GPU side of the channel).
#[derive(Debug, Clone, Copy, Default)]
pub struct DescriptorSyncRegs {
    pub sampler_binding_via_header: bool,
    pub tex_header_addr: GPUVAddr,
    pub tex_header_limit: u32,
    pub tex_sampler_addr: GPUVAddr,
    pub tex_sampler_limit: u32,
}

// ── TextureCacheChannelInfo ────────────────────────────────────────────

/// Per-channel state for the texture cache.
///
/// Port of `VideoCommon::TextureCacheChannelInfo`.
///
/// The upstream class inherits from `ChannelInfo` and holds descriptor
/// tables plus cached sampler/image-view mappings.
pub struct TextureCacheChannelInfo {
    // Descriptor tables — typed against the real `TicEntry`/`TscEntry`
    // structs from `video_core::textures::texture`. `DescriptorTable::read`
    // is still stubbed (returns `T::default()` until the GPU memory reader
    // is wired in), so `VisitImageView` will see all-zero descriptors and
    // never produce a non-NULL image view yet.
    pub graphics_image_table: DescriptorTable<crate::textures::texture::TicEntry>,
    pub graphics_sampler_table: DescriptorTable<crate::textures::texture::TscEntry>,
    pub graphics_sampler_ids: Vec<SamplerId>,
    pub graphics_image_view_ids: Vec<ImageViewId>,

    pub compute_image_table: DescriptorTable<crate::textures::texture::TicEntry>,
    pub compute_sampler_table: DescriptorTable<crate::textures::texture::TscEntry>,
    pub compute_sampler_ids: Vec<SamplerId>,
    pub compute_image_view_ids: Vec<ImageViewId>,

    // Per-channel caches. Upstream uses
    //   std::unordered_map<TICEntry, ImageViewId> image_views;
    //   std::unordered_map<TSCEntry, SamplerId>   samplers;
    // keyed by descriptor identity (raw `[u64; 4]`). `TicEntry`/`TscEntry`
    // expose manual `Hash`+`PartialEq` impls over `raw`, so the Rust
    // `HashMap` keys them directly — same lookup semantics as upstream's
    // `try_emplace(descriptor)`.
    pub image_views: HashMap<crate::textures::texture::TicEntry, ImageViewId>,
    pub samplers: HashMap<crate::textures::texture::TscEntry, SamplerId>,

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
    /// Slot pool of TSC descriptors keyed by `SamplerId`. Upstream stores
    /// `P::Sampler` directly (the backend type); ruzu separates the
    /// abstract `TscEntry` here from the backend `Sampler` which lives in
    /// the GL wrapper's `HashMap<SamplerId, Sampler>`. Mirrors the
    /// `slot_images` / `slot_image_views` split.
    pub slot_samplers: SlotVector<crate::textures::texture::TscEntry>,
    // slot_framebuffers: needs backend types

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

    // Rust adaptation of upstream `Runtime::DownloadStagingBuffer` +
    // backend `Image::DownloadMemory` and `Tegra::MemoryManager`.
    pub image_downloader: Option<ImageDownloader>,
    pub guest_memory_writer: Option<GuestMemoryWriter>,

    /// Shared `MaxwellDeviceMemoryManager` reference. Mirrors upstream
    /// `MaxwellDeviceMemoryManager& device_memory` member used by
    /// `TrackImage` / `UntrackImage` to drive `UpdatePagesCachedCount`.
    /// Same `Arc` as `Host1x::memory_manager()` and `ShaderCache::device_memory()`.
    pub device_memory:
        std::sync::Arc<crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager>,

    /// Per-channel descriptor state (TIC/TSC tables + cached id arrays).
    /// Upstream: `TextureCache<P>` inherits from
    /// `VideoCommon::ChannelSetupCaches<TextureCacheChannelInfo>` which
    /// owns `channel_state` as a pointer to the currently-active channel.
    /// Ruzu currently models a single graphics channel, so the info is held
    /// inline; promote to `Option<Box<...>>` keyed by channel id when
    /// multi-channel support lands.
    pub channel_state: TextureCacheChannelInfo,

    // Mutex
    pub mutex: ReentrantMutex<()>,
}

impl TextureCacheBase {
    /// Create a new texture cache.
    ///
    /// Port of `TextureCache<P>::TextureCache(Runtime&, MaxwellDeviceMemoryManager&)`.
    /// `device_memory` is the shared `Arc` from `Host1x::memory_manager()`.
    pub fn new(
        device_memory: std::sync::Arc<
            crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager,
        >,
    ) -> Self {
        let mut cache = Self {
            slot_images: SlotVector::new(),
            slot_map_views: SlotVector::new(),
            slot_image_views: SlotVector::new(),
            slot_image_allocs: SlotVector::new(),
            slot_samplers: SlotVector::new(),
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
            image_downloader: None,
            guest_memory_writer: None,
            device_memory,
            channel_state: TextureCacheChannelInfo::new(),
            mutex: ReentrantMutex::new(()),
        };

        // Upstream reserves slot 0 for all null resources in
        // `TextureCache<P>::TextureCache`, making NULL_*_ID{0} compile-time
        // constants that are never returned for real resources.
        let null_image_id = cache.slot_images.insert(ImageBase::null(NullImageParams));
        debug_assert_eq!(null_image_id, crate::texture_cache::types::NULL_IMAGE_ID);
        let null_view_id = cache
            .slot_image_views
            .insert(ImageViewBase::null(NullImageViewParams));
        debug_assert_eq!(
            null_view_id,
            crate::texture_cache::types::NULL_IMAGE_VIEW_ID
        );

        // Ruzu's base stores raw `TSCEntry`s rather than backend `Sampler`s,
        // so reserve sampler id 0 with the upstream null sampler descriptor.
        let mut null_sampler = crate::textures::texture::TscEntry::default();
        let word1 = (crate::textures::texture::TextureFilter::Linear as u32)
            | ((crate::textures::texture::TextureFilter::Linear as u32) << 4)
            | ((crate::textures::texture::TextureMipmapFilter::Linear as u32) << 6)
            | (1 << 8);
        null_sampler.raw[0] = (word1 as u64) << 32;
        let null_id = cache.slot_samplers.insert(null_sampler);
        debug_assert_eq!(null_id, crate::texture_cache::types::NULL_SAMPLER_ID);

        cache
    }

    pub fn set_image_downloader(&mut self, downloader: ImageDownloader) {
        self.image_downloader = Some(downloader);
    }

    pub fn set_guest_memory_writer(&mut self, writer: GuestMemoryWriter) {
        self.guest_memory_writer = Some(writer);
    }

    /// Notify the cache that a new frame has been queued.
    ///
    /// Port of `TextureCache<P>::TickFrame`.
    ///
    /// In the full implementation, this:
    /// 1. Ticks the delayed destruction ring
    /// 2. Increments the frame tick counter
    /// 3. Rescales images if resolution settings changed
    pub fn tick_frame(&mut self) {
        self.frame_tick += 1;
        self.has_deleted_images = false;
        // In full implementation: delayed_destruction_ring.Tick()
        // In full implementation: check for resolution scaling changes
    }

    /// Mark images in a range as modified from the CPU.
    ///
    /// Port of `TextureCache<P>::WriteMemory`.
    ///
    /// In the full implementation, this iterates over all images overlapping
    /// the given CPU address range and marks them as CPU-modified, scheduling
    /// them for re-upload on next GPU access.
    pub fn write_memory(&mut self, cpu_addr: u64, size: usize) {
        Self::for_each_cpu_page(cpu_addr, size, |page| {
            if let Some(image_ids) = self.page_table.get(&page) {
                for &_image_id in image_ids {
                    // In full implementation: mark image as CPU-modified
                }
            }
        });
    }

    /// Download contents of host images to guest memory in a region.
    ///
    /// Port of `TextureCache<P>::DownloadMemory`.
    ///
    /// In the full implementation, this forces a download of all GPU-modified
    /// images in the given CPU address range back to guest memory.
    pub fn download_memory(&mut self, cpu_addr: u64, size: usize) {
        let Some(downloader) = self.image_downloader.as_ref().cloned() else {
            if std::env::var_os("RUZU_TRACE_TEXTURE_DOWNLOAD").is_some() {
                log::info!(
                    "[TEXTURE_DOWNLOAD] miss no_image_downloader cpu=0x{:X} size={}",
                    cpu_addr,
                    size
                );
            }
            return;
        };
        let Some(writer) = self.guest_memory_writer.as_ref().cloned() else {
            if std::env::var_os("RUZU_TRACE_TEXTURE_DOWNLOAD").is_some() {
                log::info!(
                    "[TEXTURE_DOWNLOAD] miss no_guest_memory_writer cpu=0x{:X} size={}",
                    cpu_addr,
                    size
                );
            }
            return;
        };

        let mut images = self.collect_images_in_region(cpu_addr, size);
        images.retain(|&image_id| self.slot_images[image_id].is_safe_download());
        if images.is_empty() {
            return;
        }

        for &image_id in &images {
            self.slot_images[image_id]
                .flags
                .remove(ImageFlagBits::GPU_MODIFIED);
        }
        images.sort_by_key(|&image_id| self.slot_images[image_id].modification_tick);

        for image_id in images {
            let image = self.slot_images[image_id].clone();
            let mut staging = vec![0u8; image.unswizzled_size_bytes as usize];
            if !downloader(image_id, &image, &mut staging) {
                continue;
            }
            let copies = super::util::full_download_copies(&image.info);
            super::util::swizzle_image(
                writer.as_ref(),
                image.cpu_addr,
                &image.info,
                &copies,
                &staging,
                &mut self.swizzle_data_buffer,
            );
        }
    }

    /// Port of `TextureCache<P>::TryFindFramebufferImageView`.
    pub fn try_find_framebuffer_image_view(
        &mut self,
        config: &FramebufferConfig,
        cpu_addr: u64,
    ) -> Option<FramebufferImageView> {
        if cpu_addr == 0 {
            return None;
        }

        let valid_image_ids: Vec<ImageId> = self
            .collect_images_in_region(cpu_addr, 1)
            .into_iter()
            .filter(|&image_id| {
                let image = &self.slot_images[image_id];
                image.cpu_addr == cpu_addr && !image.image_view_ids.is_empty()
            })
            .collect();

        let image_id = match valid_image_ids.as_slice() {
            [] => return None,
            [only] => *only,
            many => *many
                .iter()
                .max_by_key(|&&id| self.slot_images[id].modification_tick)
                .expect("non-empty image list"),
        };

        let view_format = match config.pixel_format.0 {
            4 => PixelFormat::R5G6B5Unorm,
            5 => PixelFormat::B8G8R8A8Unorm,
            _ => PixelFormat::A8B8G8R8Unorm,
        };
        let mut info = ImageViewInfo::for_render_target(
            ImageViewType::E2D,
            view_format,
            SubresourceRange::default(),
        );
        if config.blending == BlendMode::Opaque {
            info.x_source = SwizzleSource::R as u8;
            info.y_source = SwizzleSource::G as u8;
            info.z_source = SwizzleSource::B as u8;
            info.w_source = SwizzleSource::OneFloat as u8;
        }

        let existing_view_id = self.slot_images[image_id].find_view(&info);
        let view_id = if existing_view_id.is_valid() {
            existing_view_id
        } else {
            let image = &self.slot_images[image_id];
            let view = ImageViewBase::new(&info, &image.info, image_id, image.gpu_addr);
            let view_id = self.slot_image_views.insert(view);
            self.slot_images[image_id].insert_view(info, view_id);
            view_id
        };
        if std::env::var_os("RUZU_TRACE_PRESENT_IMG").is_some() {
            let image = &self.slot_images[image_id];
            log::warn!(
                "[PRESENT_IMG] cpu=0x{:X} candidates={} chosen_image={} view_id={} num_views_on_image={} all_image_ids={:?}",
                cpu_addr,
                valid_image_ids.len(),
                image_id.index,
                view_id.index,
                image.image_view_ids.len(),
                valid_image_ids.iter().map(|i| i.index).collect::<Vec<_>>(),
            );
        }
        let image = &self.slot_images[image_id];
        let view = self.slot_image_views[view_id].clone();
        Some(FramebufferImageView {
            view_id,
            view,
            scaled: image.flags.contains(ImageFlagBits::RESCALED),
        })
    }

    /// Collect every `ImageId` whose backing CPU pages overlap the given
    /// region. Public so backend wrappers (e.g. `renderer_opengl::TextureCache`)
    /// can implement their own `download_memory` that needs direct access to
    /// the backend-specific image table — the base callback-based path can't
    /// borrow that table without interior mutability, so the wrapper does the
    /// full loop in user code instead.
    pub fn collect_images_in_region(&self, cpu_addr: u64, size: usize) -> Vec<ImageId> {
        let mut image_ids = Vec::new();
        let mut seen = HashSet::new();
        Self::for_each_cpu_page(cpu_addr, size, |page| {
            if let Some(map_ids) = self.page_table.get(&page) {
                for &map_id in map_ids {
                    let map = &self.slot_map_views[map_id];
                    if !map.overlaps(cpu_addr, size) || !seen.insert(map.image_id) {
                        continue;
                    }
                    image_ids.push(map.image_id);
                }
            }
        });
        image_ids
    }

    /// Remove images in a region.
    ///
    /// Port of `TextureCache<P>::UnmapMemory`.
    ///
    /// In the full implementation, this removes all images whose CPU address
    /// falls within the given range, cleaning up both the page table and
    /// slot vector entries.
    pub fn unmap_memory(&mut self, cpu_addr: u64, size: usize) {
        Self::for_each_cpu_page(cpu_addr, size, |page| {
            if let Some(image_ids) = self.page_table.get(&page) {
                for &_image_id in image_ids {
                    // In full implementation: unregister and destroy image
                }
            }
        });
    }

    /// Return true when there are uncommitted images to be downloaded.
    pub fn has_uncommitted_flushes(&self) -> bool {
        !self.uncommitted_downloads.is_empty()
    }

    /// Return true when the caller should wait for async downloads.
    pub fn should_wait_async_flushes(&self) -> bool {
        self.committed_downloads
            .front()
            .is_some_and(|downloads| !downloads.is_empty())
    }

    /// Commit asynchronous downloads.
    ///
    /// Port of `TextureCache<P>::CommitAsyncFlushes`.
    ///
    /// Moves uncommitted downloads into the committed queue for later
    /// completion and readback.
    pub fn commit_async_flushes(&mut self) {
        self.committed_downloads
            .push_back(std::mem::take(&mut self.uncommitted_downloads));
    }

    /// Pop asynchronous downloads.
    ///
    /// Port of `TextureCache<P>::PopAsyncFlushes`.
    ///
    /// Completes the oldest committed download batch, reading back pixel data
    /// to guest memory.
    pub fn pop_async_flushes(&mut self) {
        if let Some(_batch) = self.committed_downloads.pop_front() {
            // In full implementation: for each download in batch,
            // read back the staging buffer and copy to guest memory.
        }
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

#[cfg(test)]
mod tests {
    use super::TextureCacheBase;
    use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
    use std::sync::Arc;

    #[test]
    fn texture_cache_mutex_is_reentrant() {
        let cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let _lock_a = cache.mutex.lock();
        let _lock_b = cache.mutex.lock();
    }

    #[test]
    fn empty_committed_download_batch_does_not_require_wait() {
        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));

        cache.commit_async_flushes();

        assert!(!cache.should_wait_async_flushes());
    }
}
