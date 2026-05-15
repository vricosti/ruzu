// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/texture_cache/texture_cache.h and texture_cache.cpp
//!
//! This file corresponds to the ~3 000-line template-method implementation
//! of `TextureCache<P>` from the upstream header.
//!
//! texture_cache.cpp itself is tiny (just explicit template instantiation
//! of `TextureCacheChannelInfo` and `ChannelSetupCaches`).  The real code
//! lives in the .h because it is template code in C++.
//!
//! In Rust the template is replaced by concrete methods on
//! `TextureCacheBase` (defined in texture_cache_base.rs).  Method
//! signatures are ported; bodies that depend on backend-specific types
//! (Runtime, Image slot vectors, Maxwell3D registers, etc.) log a warning
//! and return safe defaults until those types are ported.

use crate::engines::draw_manager::DrawState;
use crate::engines::maxwell_3d::RenderTargetInfo;
use crate::surface;

use super::descriptor_table::DescriptorTable;
use super::image_base::{GPUVAddr, ImageBase, ImageFlagBits, ImageMapView};
use super::image_info::{ImageInfo, TilingMode};
use super::image_view_base::ImageViewBase;
use super::image_view_info::ImageViewInfo;
use super::texture_cache_base::*;
use super::types::*;

// All method implementations live on TextureCacheBase.

impl TextureCacheBase {
    // ── Garbage collection ─────────────────────────────────────────────

    /// Port of `TextureCache<P>::RunGarbageCollector`.
    ///
    /// Upstream iterates the LRU cache and destroys images that have not been
    /// used recently.  Requires `slot_images` typed as concrete `Image<P>` with
    /// `ImageFlagBits` and the runtime `DownloadStagingBuffer` / `Finish` calls.
    /// Not implementable without backend types; logs a warning and returns.
    pub fn run_garbage_collector(&mut self) {
        log::warn!(
            "TextureCacheBase::run_garbage_collector: backend types not yet available — GC skipped"
        );
    }

    // ── Image view resolution ──────────────────────────────────────────

    /// Port of `TextureCache<P>::FillGraphicsImageViews`
    /// (texture_cache.h:192-197). Thin wrapper that forwards to
    /// `fill_image_views` over the channel's graphics image table.
    pub fn fill_graphics_image_views(&mut self, views: &mut [ImageViewInOut], has_blacklists: bool) {
        Self::fill_image_views(
            &mut self.channel_state.graphics_image_table,
            &mut self.channel_state.graphics_image_view_ids,
            &mut self.channel_state.image_views,
            views,
            has_blacklists,
            &mut self.has_deleted_images,
        );
    }

    /// Port of `TextureCache<P>::FillComputeImageViews`
    /// (texture_cache.h:199-203). Upstream passes `has_blacklists=true`
    /// unconditionally to the underlying `FillImageViews` template.
    pub fn fill_compute_image_views(&mut self, views: &mut [ImageViewInOut]) {
        Self::fill_image_views(
            &mut self.channel_state.compute_image_table,
            &mut self.channel_state.compute_image_view_ids,
            &mut self.channel_state.image_views,
            views,
            true,
            &mut self.has_deleted_images,
        );
    }

    /// Port of `TextureCache<P>::FillImageViews` (texture_cache.h:472-495).
    ///
    /// Retry loop body runs `VisitImageView` for each entry; if
    /// `has_deleted_images` flips during a visit (an image got evicted) the
    /// entire batch reruns. The `has_blacklists`/`ScaleDown` branch still
    /// needs backend image access and is left as a TODO until the slot pools
    /// are concrete.
    fn fill_image_views(
        table: &mut DescriptorTable<crate::textures::texture::TicEntry>,
        cached_ids: &mut [ImageViewId],
        image_views_cache: &mut std::collections::HashMap<
            crate::textures::texture::TicEntry,
            ImageViewId,
        >,
        views: &mut [ImageViewInOut],
        has_blacklists: bool,
        has_deleted_images: &mut bool,
    ) {
        let mut has_blacklisted;
        loop {
            *has_deleted_images = false;
            has_blacklisted = false;
            for view in views.iter_mut() {
                view.id = Self::visit_image_view(table, cached_ids, image_views_cache, view.index);
                if has_blacklists && view.blacklist && view.id != NULL_IMAGE_VIEW_ID {
                    // Upstream: ScaleDown(slot_images[image_view.image_id])
                    // sets has_blacklisted=true when a rescale-down fires.
                    // Needs backend slot_images access; TODO once Image<P> lands.
                }
            }
            if !*has_deleted_images && !(has_blacklists && has_blacklisted) {
                break;
            }
        }
    }

    /// Port of `TextureCache<P>::VisitImageView` (texture_cache.h:497-514).
    ///
    /// Reads the TIC descriptor at `index` from the GPU-resident table; on a
    /// fresh read, looks up (or creates) an `ImageView` via `find_image_view`.
    /// Then would call `PrepareImageView` to ensure the backing image's
    /// contents are uploaded. `PrepareImageView` is still a stub while the
    /// backend `P::ImageView`/`P::Image` slot pools are not wired.
    fn visit_image_view(
        table: &mut DescriptorTable<crate::textures::texture::TicEntry>,
        cached_ids: &mut [ImageViewId],
        image_views_cache: &mut std::collections::HashMap<
            crate::textures::texture::TicEntry,
            ImageViewId,
        >,
        index: u32,
    ) -> ImageViewId {
        if index > table.limit() {
            // Matches upstream LOG_DEBUG + NULL_IMAGE_VIEW_ID return.
            return NULL_IMAGE_VIEW_ID;
        }
        let (descriptor, is_new) = table.read(index);
        let slot = &mut cached_ids[index as usize];
        if is_new {
            *slot = Self::find_image_view(image_views_cache, &descriptor);
        }
        if *slot != NULL_IMAGE_VIEW_ID {
            // Upstream: PrepareImageView(image_view_id, false, false). Needs
            // backend slot_images/slot_image_views with concrete image types;
            // left as a no-op until those land.
        }
        *slot
    }

    /// Port of `TextureCache<P>::FindImageView` (texture_cache.h:1103-1113):
    /// ```cpp
    /// if (!IsValidEntry(*gpu_memory, config)) return NULL_IMAGE_VIEW_ID;
    /// const auto [pair, is_new] = channel_state->image_views.try_emplace(config);
    /// ImageViewId& image_view_id = pair->second;
    /// if (is_new) image_view_id = CreateImageView(config);
    /// return image_view_id;
    /// ```
    ///
    /// The `try_emplace` semantics map to Rust's `HashMap::entry(..).or_insert_with(..)`.
    /// The `IsValidEntry` GPU-memory check and `CreateImageView` (which builds
    /// a backend `P::ImageView` via `Runtime::UploadStagingBuffer` +
    /// `Image::UploadMemory`) are not yet implemented; until they are, the
    /// cache caches a `NULL_IMAGE_VIEW_ID` per unique descriptor — same
    /// observable behaviour as the previous stub, but with the cache shape
    /// in place so plugging in `create_image_view` is a one-line swap.
    fn find_image_view(
        image_views_cache: &mut std::collections::HashMap<
            crate::textures::texture::TicEntry,
            ImageViewId,
        >,
        descriptor: &crate::textures::texture::TicEntry,
    ) -> ImageViewId {
        // TODO(texture_cache): upstream first guards on
        //   `IsValidEntry(*gpu_memory, *descriptor)` — needs a GPU memory
        //   reader on the channel before the GPU virtual address can be
        //   validated. Until then we proceed to the cache lookup; misses
        //   still resolve to NULL via the create-image-view stub.
        *image_views_cache
            .entry(*descriptor)
            .or_insert_with(|| Self::create_image_view(descriptor))
    }

    /// Port of `TextureCache<P>::CreateImageView` (texture_cache.h:1115-1137).
    ///
    /// Upstream constructs an `ImageInfo` from the TIC, either:
    ///   * for `ImageType::Buffer`: inserts a fresh `slot_image_views` entry
    ///     bound to `descriptor.Address()`, OR
    ///   * for normal images: resolves the backing image (`FindOrInsertImage`),
    ///     reads `image.TryFindBase(descriptor.Address())`, and emplaces an
    ///     `ImageViewInfo` view via `FindOrEmplaceImageView`.
    ///
    /// Both branches require concrete backend `P::ImageView`/`P::Image` types
    /// plus `Runtime::UploadStagingBuffer` for the upload path — none of
    /// which exist yet in ruzu. The stub returns `NULL_IMAGE_VIEW_ID` so the
    /// outer scaffolding skips the slot.
    fn create_image_view(_descriptor: &crate::textures::texture::TicEntry) -> ImageViewId {
        NULL_IMAGE_VIEW_ID
    }

    /// Port of `TextureCache<P>::CheckFeedbackLoop`.
    ///
    /// Checks whether any sampled image view matches a current render target;
    /// if so, emits a barrier via `Runtime::BarrierFeedbackLoop`.
    pub fn check_feedback_loop(&self, _views: &[ImageViewInOut]) {
        log::warn!("TextureCacheBase::check_feedback_loop: backend types not yet available");
    }

    // ── Descriptor synchronisation ─────────────────────────────────────

    /// Port of `TextureCache<P>::SynchronizeGraphicsDescriptors`
    /// (texture_cache.h:294-307).
    ///
    /// Upstream reads `maxwell3d->regs` directly; ruzu can't borrow Maxwell3D
    /// from the texture cache so the caller hands in a `DescriptorSyncRegs`
    /// snapshot captured at draw-time. The body otherwise mirrors upstream
    /// step-for-step: pick TIC/TSC table limits (collapsing TSC limit onto
    /// TIC's when `sampler_binding == ViaHeaderBinding`), call
    /// `DescriptorTable::synchronize` on each, and grow the cached
    /// `*_ids` arrays when a table's limit changed.
    pub fn synchronize_graphics_descriptors(&mut self, regs: DescriptorSyncRegs) {
        let tic_limit = regs.tex_header_limit;
        let tsc_limit = if regs.sampler_binding_via_header {
            tic_limit
        } else {
            regs.tex_sampler_limit
        };

        let channel = &mut self.channel_state;
        if channel
            .graphics_sampler_table
            .synchronize(regs.tex_sampler_addr, tsc_limit)
        {
            channel
                .graphics_sampler_ids
                .resize(tsc_limit as usize + 1, CORRUPT_ID);
        }
        if channel
            .graphics_image_table
            .synchronize(regs.tex_header_addr, tic_limit)
        {
            channel
                .graphics_image_view_ids
                .resize(tic_limit as usize + 1, CORRUPT_ID);
        }
    }

    /// Port of `TextureCache<P>::SynchronizeComputeDescriptors`.
    pub fn synchronize_compute_descriptors(&mut self) {
        log::warn!(
            "TextureCacheBase::synchronize_compute_descriptors: Kepler Compute regs not yet ported"
        );
    }

    // ── Render targets ─────────────────────────────────────────────────

    /// Port of `TextureCache<P>::RescaleRenderTargets`.
    ///
    /// Iterates dirty render target slots, resolves images, and decides
    /// whether to scale up or down based on scale ratings.  Returns whether
    /// the final render targets are rescaled.
    pub fn rescale_render_targets(&mut self) -> bool {
        log::warn!("TextureCacheBase::rescale_render_targets: backend types not yet available");
        false
    }

    /// Port of `TextureCache<P>::UpdateRenderTargets`.
    ///
    /// Calls `RescaleRenderTargets` when dirty, prepares all color and depth
    /// image views, and updates `render_targets.size`.
    pub fn update_render_targets(&mut self, _is_clear: bool) {
        log::warn!("TextureCacheBase::update_render_targets: backend types not yet available");
    }

    /// Rust bridge for `TextureCache<P>::UpdateRenderTargets` while the cache
    /// does not own a `Maxwell3D*`.
    ///
    /// Upstream reads `maxwell3d->regs.rt_control` and `maxwell3d->regs.rt[]`
    /// directly from this owner. The Rust draw path snapshots those registers
    /// into `DrawState` and provides the channel GPU→CPU translator here.
    pub fn update_render_targets_from_draw_state(
        &mut self,
        draw_state: &DrawState,
        mut gpu_to_cpu: impl FnMut(GPUVAddr) -> Option<u64>,
    ) {
        let count = draw_state.rt_control.count.min(NUM_RT as u32) as usize;
        if count == 0 {
            return;
        }

        for color_index in 0..count {
            let target_index = draw_state.rt_control.map[color_index] as usize;
            if target_index >= NUM_RT {
                continue;
            }
            let rt = draw_state.render_targets[target_index];
            if rt.address == 0 || rt.width == 0 || rt.height == 0 || rt.format == 0 {
                continue;
            }
            let Some(cpu_addr) = gpu_to_cpu(rt.address) else {
                if std::env::var_os("RUZU_TRACE_RT").is_some() {
                    log::info!(
                        "[RT] miss translate color={} target={} gpu=0x{:X} {}x{} fmt=0x{:X}",
                        color_index,
                        target_index,
                        rt.address,
                        rt.width,
                        rt.height,
                        rt.format
                    );
                }
                continue;
            };

            let image_id = self.find_or_insert_render_target_image(&rt, cpu_addr);
            self.ensure_render_target_view(image_id);
            self.mark_modification_by_id(image_id);

            if std::env::var_os("RUZU_TRACE_RT").is_some() {
                let image = &self.slot_images[image_id];
                log::info!(
                    "[RT] color={} target={} gpu=0x{:X} cpu=0x{:X} {}x{} fmt=0x{:X} image={} views={}",
                    color_index,
                    target_index,
                    rt.address,
                    cpu_addr,
                    rt.width,
                    rt.height,
                    rt.format,
                    image_id.index,
                    image.image_view_ids.len()
                );
            }
        }
    }

    // ── Image lookup / insertion ───────────────────────────────────────

    /// Port of `TextureCache<P>::FindOrInsertImage`.
    ///
    /// Looks up an existing image by GPU address and descriptor; inserts a new
    /// one if not found.  Returns `NULL_IMAGE_ID` until backend types are ready.
    pub fn find_or_insert_image(
        &mut self,
        _info: &ImageInfo,
        _gpu_addr: GPUVAddr,
        _options: RelaxedOptions,
    ) -> ImageId {
        log::warn!("TextureCacheBase::find_or_insert_image: backend types not yet available");
        ImageId::default()
    }

    /// Port of `TextureCache<P>::FindImage`.
    ///
    /// Searches for an existing image matching the descriptor and GPU address.
    pub fn find_image(
        &mut self,
        _info: &ImageInfo,
        _gpu_addr: GPUVAddr,
        _options: RelaxedOptions,
    ) -> ImageId {
        log::warn!("TextureCacheBase::find_image: backend types not yet available");
        ImageId::default()
    }

    /// Port of `TextureCache<P>::InsertImage`.
    ///
    /// Allocates a new image slot, uploads guest memory, and registers it.
    pub fn insert_image(
        &mut self,
        _info: &ImageInfo,
        _gpu_addr: GPUVAddr,
        _options: RelaxedOptions,
    ) -> ImageId {
        log::warn!("TextureCacheBase::insert_image: backend types not yet available");
        ImageId::default()
    }

    fn find_or_insert_render_target_image(
        &mut self,
        rt: &RenderTargetInfo,
        cpu_addr: u64,
    ) -> ImageId {
        let pixel_format = surface::pixel_format_from_render_target_format(rt.format);
        let pitch = rt
            .width
            .saturating_mul(surface::bytes_per_block(pixel_format));
        let info = ImageInfo {
            format: pixel_format,
            image_type: ImageType::Linear,
            resources: SubresourceExtent {
                levels: 1,
                layers: 1,
            },
            size: Extent3D {
                width: rt.width,
                height: rt.height,
                depth: 1,
            },
            tiling: TilingMode::PitchLinear(pitch),
            layer_stride: 0,
            maybe_unaligned_layer_stride: 0,
            num_samples: 1,
            tile_width_spacing: 0,
            rescaleable: false,
            downscaleable: false,
            forced_flushed: false,
            dma_downloaded: false,
            is_sparse: false,
        };

        if let Some((image_id, _)) = self.slot_images.iter().find(|(_, image)| {
            image.gpu_addr == rt.address
                && image.cpu_addr == cpu_addr
                && image.info.size.width == info.size.width
                && image.info.size.height == info.size.height
                && image.info.format == info.format
        }) {
            return image_id;
        }

        let image_id = self
            .slot_images
            .insert(ImageBase::new(info, rt.address, cpu_addr));
        let image_size = self.slot_images[image_id].guest_size_bytes as usize;
        let map_id = self.slot_map_views.insert(ImageMapView::new(
            rt.address, cpu_addr, image_size, image_id,
        ));
        self.slot_images[image_id].map_view_id = map_id;
        self.register_image(image_id);
        image_id
    }

    fn ensure_render_target_view(&mut self, image_id: ImageId) -> ImageViewId {
        let image = &self.slot_images[image_id];
        let info = ImageViewInfo::for_render_target(
            ImageViewType::E2D,
            image.info.format,
            SubresourceRange::default(),
        );
        let existing = image.find_view(&info);
        if existing.is_valid() {
            return existing;
        }

        let gpu_addr = image.gpu_addr;
        let image_info = image.info.clone();
        let view = ImageViewBase::new(&info, &image_info, image_id, gpu_addr);
        let view_id = self.slot_image_views.insert(view);
        self.slot_images[image_id].insert_view(info, view_id);
        view_id
    }

    /// Port of `TextureCache<P>::JoinImages`.
    ///
    /// Resolves overlapping images by computing aliases and copies to do.
    pub fn join_images(
        &mut self,
        _info: &ImageInfo,
        _gpu_addr: GPUVAddr,
        _cpu_addr: u64,
    ) -> ImageId {
        log::warn!("TextureCacheBase::join_images: backend types not yet available");
        ImageId::default()
    }

    // ── Registration / tracking ────────────────────────────────────────

    /// Port of `TextureCache<P>::RegisterImage`.
    ///
    /// Inserts the image into page tables and marks it for CPU write-tracking.
    pub fn register_image(&mut self, image_id: ImageId) {
        let map_id = self.slot_images[image_id].map_view_id;
        if !map_id.is_valid() {
            log::warn!("TextureCacheBase::register_image: image has no map view");
            return;
        }
        let (cpu_addr, size) = {
            let map = &self.slot_map_views[map_id];
            (map.cpu_addr, map.size)
        };
        Self::for_each_cpu_page(cpu_addr, size, |page| {
            let entries = self.page_table.entry(page).or_default();
            if !entries.contains(&map_id) {
                entries.push(map_id);
            }
        });
        self.slot_images[image_id]
            .flags
            .insert(ImageFlagBits::REGISTERED);
    }

    /// Port of `TextureCache<P>::UnregisterImage`.
    ///
    /// Removes the image from page tables and clears any image-view references.
    pub fn unregister_image(&mut self, _image_id: ImageId) {
        log::warn!("TextureCacheBase::unregister_image: backend types not yet available");
    }

    /// Port of `TextureCache<P>::TrackImage` (texture_cache.h:2113).
    ///
    /// Marks the image as `Tracked` and bumps the per-page cached count
    /// on the shared `MaxwellDeviceMemoryManager` so guest CPU writes to
    /// the image's backing range trigger cache invalidation. Handles both
    /// dense images (single contiguous range) and sparse images (multiple
    /// map views), matching upstream's branch on `ImageFlagBits::Sparse`.
    pub fn track_image(&mut self, image_id: ImageId) {
        let image = &mut self.slot_images[image_id];
        debug_assert!(
            !image.flags.contains(ImageFlagBits::TRACKED),
            "TextureCache::track_image: image already tracked"
        );
        image.flags.insert(ImageFlagBits::TRACKED);
        let is_sparse = image.flags.contains(ImageFlagBits::SPARSE);
        let registered = image.flags.contains(ImageFlagBits::REGISTERED);
        let cpu_addr = image.cpu_addr;
        let guest_size_bytes = image.guest_size_bytes;
        if !is_sparse {
            // Upstream guard: skip the "kernel" sentinel range
            // (`cpu_addr >= ~(1ULL << 40)`).
            if cpu_addr < !(1u64 << 40) {
                self.device_memory.update_pages_cached_count(
                    cpu_addr,
                    guest_size_bytes as usize,
                    1,
                );
            }
            return;
        }
        debug_assert!(
            registered,
            "TextureCache::track_image: sparse image must be registered first"
        );
        let sparse_maps = self
            .sparse_views
            .get(&image_id)
            .expect("sparse image missing from sparse_views")
            .clone();
        for map_view_id in sparse_maps {
            let map = &self.slot_map_views[map_view_id];
            self.device_memory
                .update_pages_cached_count(map.cpu_addr, map.size, 1);
        }
    }

    /// Port of `TextureCache<P>::UntrackImage` (texture_cache.h:2141).
    ///
    /// Inverse of `track_image`: clears the `Tracked` flag and decrements
    /// the per-page cached count for the image's backing pages.
    pub fn untrack_image(&mut self, image_id: ImageId) {
        let image = &mut self.slot_images[image_id];
        debug_assert!(
            image.flags.contains(ImageFlagBits::TRACKED),
            "TextureCache::untrack_image: image not tracked"
        );
        image.flags.remove(ImageFlagBits::TRACKED);
        let is_sparse = image.flags.contains(ImageFlagBits::SPARSE);
        let registered = image.flags.contains(ImageFlagBits::REGISTERED);
        let cpu_addr = image.cpu_addr;
        let guest_size_bytes = image.guest_size_bytes;
        if !is_sparse {
            if cpu_addr < !(1u64 << 40) {
                self.device_memory.update_pages_cached_count(
                    cpu_addr,
                    guest_size_bytes as usize,
                    -1,
                );
            }
            return;
        }
        debug_assert!(
            registered,
            "TextureCache::untrack_image: sparse image must be registered first"
        );
        let sparse_maps = self
            .sparse_views
            .get(&image_id)
            .expect("sparse image missing from sparse_views")
            .clone();
        for map_view_id in sparse_maps {
            let map = &self.slot_map_views[map_view_id];
            self.device_memory
                .update_pages_cached_count(map.cpu_addr, map.size, -1);
        }
    }

    /// Port of `TextureCache<P>::DeleteImage`.
    ///
    /// Destroys the backend image and removes it from all data structures.
    /// `immediate_delete` corresponds to the upstream `bool immediate` parameter
    /// that determines whether the image is placed in the delayed-destruction
    /// ring or freed immediately.
    pub fn delete_image(&mut self, _image_id: ImageId, _immediate_delete: bool) {
        log::warn!("TextureCacheBase::delete_image: backend types not yet available");
    }

    // ── Blit ───────────────────────────────────────────────────────────

    /// Port of `TextureCache<P>::BlitImage`.
    ///
    /// Resolves source/destination image pairs, handles rescaling, and
    /// delegates to the backend blit/resolve operations.  Returns false until
    /// Fermi2D engine types and backend runtime are available.
    pub fn blit_image(&mut self, _dst: &(), _src: &(), _copy: &()) -> bool {
        log::warn!("TextureCacheBase::blit_image: Fermi2D / backend types not yet available");
        false
    }

    // ── Rescaling ──────────────────────────────────────────────────────

    /// Port of `TextureCache<P>::IsRescaling`.
    pub fn is_rescaling_active(&self) -> bool {
        self.is_rescaling
    }

    // ── Prepare / refresh ──────────────────────────────────────────────

    /// Port of `TextureCache<P>::PrepareImage`.
    ///
    /// Ensures an image is resident and up-to-date before GPU use.  If the
    /// image is CPU-modified it is re-uploaded; if it is a modification then
    /// the GPU-modified flag is set.
    pub fn prepare_image(&mut self, _image_id: ImageId, _is_modification: bool, _invalidate: bool) {
        log::warn!("TextureCacheBase::prepare_image: backend types not yet available");
    }

    /// Port of `TextureCache<P>::RefreshContents`.
    ///
    /// Re-uploads guest texture data for a CPU-modified image.
    fn refresh_contents(&mut self, _image_id: ImageId) {
        log::warn!("TextureCacheBase::refresh_contents: backend types not yet available");
    }

    // ── Modification marks ─────────────────────────────────────────────

    /// Port of `TextureCache<P>::MarkModification(ImageId)`.
    ///
    /// Sets the `GpuModified` flag on the image and updates
    /// `modification_tick`.
    pub fn mark_modification_by_id(&mut self, id: ImageId) {
        if !id.is_valid() {
            return;
        }
        self.modification_tick = self.modification_tick.saturating_add(1);
        let image = &mut self.slot_images[id];
        image.flags.insert(ImageFlagBits::GPU_MODIFIED);
        image.flags.remove(ImageFlagBits::CPU_MODIFIED);
        image.modification_tick = self.modification_tick;
    }

    // ── GPU memory queries ─────────────────────────────────────────────

    /// Port of `TextureCache<P>::IsRegionGpuModified`.
    ///
    /// Returns true if any image overlapping the given CPU address range has
    /// been modified from the GPU and not yet downloaded to guest memory.
    /// Returns false until backend image types are available.
    pub fn is_region_gpu_modified(&self, _addr: u64, _size: usize) -> bool {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engines::maxwell_3d::{RenderTargetInfo, RtControlInfo};
    use crate::framebuffer_config::FramebufferConfig;

    #[test]
    fn update_render_targets_from_draw_state_registers_presentable_view() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;
        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let mut draw_state = DrawState::default();
        draw_state.rt_control = RtControlInfo {
            count: 1,
            map: [0, 0, 0, 0, 0, 0, 0, 0],
        };
        draw_state.render_targets[0] = RenderTargetInfo {
            address: 0x4000_0000,
            width: 64,
            height: 32,
            format: 0xD5,
        };

        cache.update_render_targets_from_draw_state(&draw_state, |gpu_addr| {
            (gpu_addr == 0x4000_0000).then_some(0x535B_5000)
        });

        let config = FramebufferConfig {
            address: 0x535B_5000,
            width: 64,
            height: 32,
            stride: 64,
            ..Default::default()
        };
        let view = cache.try_find_framebuffer_image_view(&config, 0x535B_5000);
        assert!(view.is_some());
        assert_eq!(cache.slot_images.size(), 1);
        assert_eq!(cache.slot_image_views.size(), 1);
    }
}
