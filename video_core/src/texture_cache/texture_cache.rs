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

use crate::engines::draw_manager::Maxwell3DRenderTargets;
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
    /// (texture_cache.h:192-197). Method wrapper around `fill_image_views`
    /// targeting the channel's graphics descriptor table + caches.
    pub fn fill_graphics_image_views(
        &mut self,
        views: &mut [ImageViewInOut],
        has_blacklists: bool,
    ) {
        self.fill_image_views(true, views, has_blacklists);
    }

    /// Same as `fill_graphics_image_views`, but reads TIC descriptors and
    /// validates image addresses through a caller-provided channel GPU-VA
    /// reader.
    pub fn fill_graphics_image_views_with_gpu_reader(
        &mut self,
        views: &mut [ImageViewInOut],
        has_blacklists: bool,
        read_gpu: &mut dyn FnMut(GPUVAddr, &mut [u8]) -> bool,
        addr_valid: &mut dyn FnMut(GPUVAddr) -> bool,
    ) {
        let mut has_blacklisted;
        loop {
            self.has_deleted_images = false;
            has_blacklisted = false;
            for view in views.iter_mut() {
                view.id = self
                    .visit_graphics_image_view_with_gpu_reader(view.index, read_gpu, addr_valid);
                if has_blacklists && view.blacklist && view.id != NULL_IMAGE_VIEW_ID {
                    // TODO: ScaleDown(slot_images[image_view.image_id]).
                    has_blacklisted = false;
                }
            }
            if !self.has_deleted_images && !(has_blacklists && has_blacklisted) {
                break;
            }
        }
    }

    /// Port of `TextureCache<P>::FillComputeImageViews`
    /// (texture_cache.h:199-203). Upstream passes `has_blacklists=true`
    /// unconditionally to the underlying `FillImageViews` template.
    pub fn fill_compute_image_views(&mut self, views: &mut [ImageViewInOut]) {
        self.fill_image_views(false, views, true);
    }

    /// Port of `TextureCache<P>::FillImageViews` (texture_cache.h:472-495).
    ///
    /// Retry loop: for each view, resolves its TIC descriptor and looks up
    /// (or creates) an `ImageViewId`. Reruns the batch if any visit cleared
    /// `has_deleted_images` (an image got evicted mid-visit). The
    /// `has_blacklists`/`ScaleDown` branch still needs backend access and is
    /// noted as TODO.
    fn fill_image_views(
        &mut self,
        graphics: bool,
        views: &mut [ImageViewInOut],
        has_blacklists: bool,
    ) {
        let mut has_blacklisted;
        loop {
            self.has_deleted_images = false;
            has_blacklisted = false;
            for view in views.iter_mut() {
                view.id = self.visit_image_view(graphics, view.index);
                if has_blacklists && view.blacklist && view.id != NULL_IMAGE_VIEW_ID {
                    // Upstream: ScaleDown(slot_images[image_view.image_id])
                    // sets has_blacklisted=true when a rescale-down fires.
                    // Needs backend image scaling; TODO once the GL texture
                    // pool lands.
                }
            }
            if !self.has_deleted_images && !(has_blacklists && has_blacklisted) {
                break;
            }
        }
    }

    /// Port of `TextureCache<P>::VisitImageView` (texture_cache.h:497-514).
    ///
    /// Reads the TIC descriptor at `index` from the channel's graphics or
    /// compute table; on a fresh read, looks up (or creates) an `ImageView`
    /// via `find_image_view`. Splits the borrow across `channel_state`
    /// fields + `device_memory` so `find_image_view` can take `&mut self`.
    fn visit_image_view(&mut self, graphics: bool, index: u32) -> ImageViewId {
        if std::env::var_os("RUZU_TRACE_VISIT_TIC").is_some() {
            let table = if graphics {
                &self.channel_state.graphics_image_table
            } else {
                &self.channel_state.compute_image_table
            };
            // Use cached descriptor without reading guest memory
            let desc = table.cached(index);
            if let Some(d) = desc {
                log::warn!(
                    "[VISIT_TIC] graphics={} index={} cached_addr=0x{:X}",
                    graphics,
                    index,
                    d.address()
                );
            } else {
                log::warn!(
                    "[VISIT_TIC] graphics={} index={} cached=None",
                    graphics,
                    index
                );
            }
        }
        let gpu_memory = self.device_memory.clone();
        // Step 1: read the TIC descriptor with a local borrow on the table only.
        let (descriptor, is_new) = {
            let table = if graphics {
                &mut self.channel_state.graphics_image_table
            } else {
                &mut self.channel_state.compute_image_table
            };
            if index > table.limit() {
                return NULL_IMAGE_VIEW_ID;
            }
            table.read(&*gpu_memory, index)
        };
        // Step 2: on first read for this index, resolve via find_image_view
        // (now &mut self).
        if is_new {
            let new_id = self.find_image_view(&descriptor);
            let cached_ids = if graphics {
                &mut self.channel_state.graphics_image_view_ids
            } else {
                &mut self.channel_state.compute_image_view_ids
            };
            cached_ids[index as usize] = new_id;
        }
        // Step 3: return the (now stable) cached id.
        let cached_ids = if graphics {
            &self.channel_state.graphics_image_view_ids
        } else {
            &self.channel_state.compute_image_view_ids
        };
        cached_ids[index as usize]
    }

    fn visit_graphics_image_view_with_gpu_reader(
        &mut self,
        index: u32,
        read_gpu: &mut dyn FnMut(GPUVAddr, &mut [u8]) -> bool,
        addr_valid: &mut dyn FnMut(GPUVAddr) -> bool,
    ) -> ImageViewId {
        let (descriptor, is_new) = {
            let table = &mut self.channel_state.graphics_image_table;
            if index > table.limit() {
                return NULL_IMAGE_VIEW_ID;
            }
            table.read_with(index, |gpu_addr, out| read_gpu(gpu_addr, out))
        };
        if std::env::var_os("RUZU_TRACE_VISIT_TIC").is_some() {
            log::warn!(
                "[VISIT_TIC_GPU] index={} tic_gpu=0x{:X} fmt=0x{:X} width={} height={} is_new={}",
                index,
                descriptor.address(),
                {
                    let raw = descriptor.raw[0];
                    raw as u32 & 0xFFFFFF // approximation of format bits
                },
                {
                    // width is in word4 bits[16:0] for pitch, varies by layout
                    let w = (descriptor.raw[2] >> 0) as u32 & 0xFFFF;
                    w
                },
                {
                    let h = (descriptor.raw[2] >> 16) as u32 & 0xFFFF;
                    h
                },
                is_new,
            );
        }
        if is_new {
            let new_id = self.find_image_view_with_addr_valid(&descriptor, addr_valid);
            self.channel_state.graphics_image_view_ids[index as usize] = new_id;
        }
        self.channel_state.graphics_image_view_ids[index as usize]
    }

    /// Port of `TextureCache<P>::FindImageView` (texture_cache.h:1103-1113).
    /// Guards on `IsValidEntry`, then does a HashMap try_emplace against the
    /// descriptor; on cache miss, calls `create_image_view`.
    fn find_image_view(&mut self, descriptor: &crate::textures::texture::TicEntry) -> ImageViewId {
        let gpu_memory = self.device_memory.clone();
        if !super::util::is_valid_entry(&*gpu_memory, descriptor) {
            return NULL_IMAGE_VIEW_ID;
        }
        if let Some(&id) = self.channel_state.image_views.get(descriptor) {
            if std::env::var_os("RUZU_TRACE_TIC_LOOKUP").is_some() {
                log::warn!(
                    "[TIC_LOOKUP] cached gpu=0x{:X} view_id={} (cached)",
                    descriptor.address(),
                    id.index
                );
            }
            return id;
        }
        let addr = descriptor.address();
        let new_id = self.create_image_view(descriptor);
        if std::env::var_os("RUZU_TRACE_TIC_LOOKUP").is_some() {
            log::warn!(
                "[TIC_LOOKUP] new gpu=0x{:X} view_id={} (created)",
                addr,
                new_id.index
            );
        }
        self.channel_state.image_views.insert(*descriptor, new_id);
        new_id
    }

    fn find_image_view_with_addr_valid(
        &mut self,
        descriptor: &crate::textures::texture::TicEntry,
        addr_valid: &mut dyn FnMut(GPUVAddr) -> bool,
    ) -> ImageViewId {
        if !super::util::is_valid_entry_with_addr_valid(descriptor, |gpu_addr| addr_valid(gpu_addr))
        {
            return NULL_IMAGE_VIEW_ID;
        }
        if let Some(&id) = self.channel_state.image_views.get(descriptor) {
            return id;
        }
        let new_id = self.create_image_view(descriptor);
        self.channel_state.image_views.insert(*descriptor, new_id);
        new_id
    }

    /// Port of `TextureCache<P>::CreateImageView` (texture_cache.h:1115-1137).
    /// Now wired through to the real slot pools — returns the inserted view's
    /// `ImageViewId` (not a NULL stub). The created `ImageViewBase` carries
    /// the format, dimensions, range and parent `ImageId`, with the
    /// `Strong` flag set on both the view and its backing image. The backend
    /// GL texture handle is still 0 — that's the next slice's problem; the
    /// renderer needs to walk `slot_image_views[id]` and lazy-create the GL
    /// texture from there.
    fn create_image_view(
        &mut self,
        descriptor: &crate::textures::texture::TicEntry,
    ) -> ImageViewId {
        let info = super::image_info::ImageInfo::from_tic_entry(descriptor);
        if info.image_type == ImageType::Buffer {
            let view_info = super::image_view_info::ImageViewInfo::from_tic_entry(descriptor, 0);
            let view = ImageViewBase::new_buffer(&info, &view_info, descriptor.address());
            return self.slot_image_views.insert(view);
        }
        let layer_offset = descriptor.base_layer() as u64 * info.layer_stride as u64;
        let image_gpu_addr = descriptor.address().wrapping_sub(layer_offset);
        let image_id = self.find_or_insert_image(&info, image_gpu_addr);
        if image_id == NULL_IMAGE_ID {
            return NULL_IMAGE_VIEW_ID;
        }
        let base = match self
            .slot_images
            .get(image_id)
            .try_find_base(descriptor.address())
        {
            Some(base) => base,
            None => return NULL_IMAGE_VIEW_ID,
        };
        debug_assert_eq!(base.level, 0);
        let view_info =
            super::image_view_info::ImageViewInfo::from_tic_entry(descriptor, base.layer);
        let existing = self.slot_images.get(image_id).find_view(&view_info);
        if existing.is_valid() {
            return existing;
        }
        let parent_info = self.slot_images.get(image_id).info.clone();
        let view = ImageViewBase::new(&view_info, &parent_info, image_id, descriptor.address());
        let view_id = self.slot_image_views.insert(view);
        self.slot_images
            .get_mut(image_id)
            .insert_view(view_info, view_id);
        // Upstream tags both the view and its image as `Strong`. Bitflags
        // already supports `|=` on the existing `flags` fields.
        self.slot_image_views.get_mut(view_id).flags |=
            super::image_view_base::ImageViewFlagBits::STRONG;
        self.slot_images.get_mut(image_id).flags |= ImageFlagBits::STRONG;
        view_id
    }

    /// Port of `TextureCache<P>::FindOrInsertImage` (texture_cache.h:1140-1146).
    /// Looks up an existing image that can satisfy `info` at `gpu_addr`;
    /// on miss, inserts a fresh `ImageBase` keyed by that address.
    pub(crate) fn find_or_insert_image(
        &mut self,
        info: &super::image_info::ImageInfo,
        gpu_addr: GPUVAddr,
    ) -> ImageId {
        if let Some(id) = self.find_image(info, gpu_addr) {
            return id;
        }
        self.insert_image(info, gpu_addr)
    }

    /// Port of `TextureCache<P>::FindImage`'s compatibility predicate
    /// (texture_cache.h:1149-1202), using a direct slot scan instead of
    /// upstream's CPU page-table map. The important upstream contract is that
    /// an existing image is reusable only when `IsSubresource` proves it
    /// covers the requested image range; exact GPU address alone is
    /// insufficient for cube/cube-array views.
    pub(crate) fn find_image(
        &self,
        info: &super::image_info::ImageInfo,
        gpu_addr: GPUVAddr,
    ) -> Option<ImageId> {
        self.find_image_with_caps(info, gpu_addr, RelaxedOptions::empty(), false, false)
    }

    /// Same compatibility predicate as `find_image`, with backend runtime
    /// flags supplied by the caller. Upstream `TextureCache<P>::FindImage`
    /// derives these from `runtime.HasBrokenTextureViewFormats()` and
    /// `runtime.HasNativeBgr()` before calling `IsSubresource`.
    pub(crate) fn find_image_with_caps(
        &self,
        info: &super::image_info::ImageInfo,
        gpu_addr: GPUVAddr,
        options: RelaxedOptions,
        broken_views: bool,
        native_bgr: bool,
    ) -> Option<ImageId> {
        self.slot_images
            .iter()
            .filter_map(|(id, image)| {
                if image.flags.contains(ImageFlagBits::REMAPPED) {
                    return None;
                }
                super::util::is_subresource(
                    info,
                    image,
                    gpu_addr,
                    options,
                    broken_views,
                    native_bgr,
                )
                .then_some((id, image.modification_tick))
            })
            .max_by_key(|(_, modification_tick)| *modification_tick)
            .map(|(id, _)| id)
    }

    /// Port of `InsertImage` minus the backend `slot_images.insert(runtime,
    /// info, ...)` upload glue. Constructs an `ImageBase`, inserts into the
    /// slot pool, and returns its `ImageId`. The CPU address is set to the
    /// GPU address as a placeholder until `gpu_memory.GpuToCpuAddress`
    /// returns a real host VAddr.
    pub(crate) fn insert_image(
        &mut self,
        info: &super::image_info::ImageInfo,
        gpu_addr: GPUVAddr,
    ) -> ImageId {
        // Upstream: `cpu_addr = gpu_memory->GpuToCpuAddress(gpu_addr).value()`.
        // Ruzu currently stashes the same value because the SMMU host
        // pointer is what the page-walked CPU address would point at.
        let cpu_addr = gpu_addr;
        let image = ImageBase::new(info.clone(), gpu_addr, cpu_addr);
        let image_id = self.slot_images.insert(image);
        self.register_image(image_id);
        image_id
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

    // ── Sampler resolution ─────────────────────────────────────────────

    /// Resolve a graphics-stage sampler index to a `SamplerId`.
    ///
    /// Port of `TextureCache<P>::GetGraphicsSamplerId` (texture_cache.h:256-267).
    /// Reads the TSC table at `index`, dedupes via `channel_state.samplers`,
    /// and caches the result in `channel_state.graphics_sampler_ids[index]`
    /// so subsequent draws skip the lookup when the descriptor hasn't
    /// changed.
    ///
    /// Returns `NULL_SAMPLER_ID` for out-of-range indices — upstream logs
    /// `LOG_DEBUG("Invalid sampler index={}")` and does the same.
    pub fn get_graphics_sampler_id(&mut self, index: u32) -> SamplerId {
        use crate::texture_cache::types::NULL_SAMPLER_ID;
        if index > self.channel_state.graphics_sampler_table.limit() {
            log::debug!(
                "TextureCacheBase::get_graphics_sampler_id: invalid index={}",
                index
            );
            return NULL_SAMPLER_ID;
        }
        let gpu_memory_arc = self.device_memory.clone();
        let (descriptor, is_new) = self
            .channel_state
            .graphics_sampler_table
            .read(gpu_memory_arc.as_ref(), index);
        let cached = self.channel_state.graphics_sampler_ids[index as usize];
        if !is_new && cached.is_valid() && cached != CORRUPT_ID {
            return cached;
        }
        let id = self.find_sampler(&descriptor);
        self.channel_state.graphics_sampler_ids[index as usize] = id;
        id
    }

    /// Resolve a graphics-stage sampler index using a caller-provided GPU-VA
    /// reader for the TSC table.
    ///
    /// Same ownership as upstream `TextureCache<P>::GetGraphicsSamplerId`;
    /// this Rust bridge exists because the current cache base stores the
    /// Host1x SMMU device-memory manager, while graphics TSC table pointers
    /// in MK8D are GPU virtual addresses that must go through the channel
    /// `MemoryManager`.
    pub fn get_graphics_sampler_id_with_gpu_reader(
        &mut self,
        index: u32,
        table_addr: GPUVAddr,
        table_limit: u32,
        mut read_gpu: impl FnMut(GPUVAddr, &mut [u8]) -> bool,
    ) -> SamplerId {
        use crate::texture_cache::types::NULL_SAMPLER_ID;
        if index > table_limit {
            log::debug!(
                "TextureCacheBase::get_graphics_sampler_id_with_gpu_reader: invalid index={}",
                index
            );
            return NULL_SAMPLER_ID;
        }
        if index as usize >= self.channel_state.graphics_sampler_ids.len() {
            self.channel_state
                .graphics_sampler_ids
                .resize(index as usize + 1, CORRUPT_ID);
        }
        let cached = self.channel_state.graphics_sampler_ids[index as usize];
        if cached.is_valid() && cached != CORRUPT_ID {
            return cached;
        }

        let descriptor_addr = table_addr
            + index as u64 * std::mem::size_of::<crate::textures::texture::TscEntry>() as u64;
        let mut buf = [0u8; std::mem::size_of::<crate::textures::texture::TscEntry>()];
        if !read_gpu(descriptor_addr, &mut buf) {
            if std::env::var_os("RUZU_TRACE_TSC_READ").is_some() {
                log::warn!(
                    "[TSC_READ] miss index={} table=0x{:X} limit={} addr=0x{:X}",
                    index,
                    table_addr,
                    table_limit,
                    descriptor_addr,
                );
            }
            return NULL_SAMPLER_ID;
        }
        let descriptor = unsafe {
            std::ptr::read_unaligned(buf.as_ptr() as *const crate::textures::texture::TscEntry)
        };
        if std::env::var_os("RUZU_TRACE_TSC_READ").is_some() {
            log::warn!(
                "[TSC_READ] index={} table=0x{:X} limit={} addr=0x{:X} raw={:016X} {:016X} {:016X} {:016X}",
                index,
                table_addr,
                table_limit,
                descriptor_addr,
                descriptor.raw[0],
                descriptor.raw[1],
                descriptor.raw[2],
                descriptor.raw[3],
            );
        }
        let id = self.find_sampler(&descriptor);
        self.channel_state.graphics_sampler_ids[index as usize] = id;
        id
    }

    /// Look up or insert a sampler by its TSC descriptor.
    ///
    /// Port of `TextureCache<P>::FindSampler` (texture_cache.h:1735-1744):
    /// all-zero TSC → `NULL_SAMPLER_ID`; otherwise `try_emplace` into the
    /// `channel_state.samplers` HashMap and on first occurrence allocate
    /// a fresh slot in `slot_samplers`.
    pub fn find_sampler(&mut self, config: &crate::textures::texture::TscEntry) -> SamplerId {
        use crate::texture_cache::types::NULL_SAMPLER_ID;
        // Upstream `std::ranges::all_of(config.raw, [](u64 v){ return v == 0; })`.
        if config.raw.iter().all(|&w| w == 0) {
            return NULL_SAMPLER_ID;
        }
        if let Some(&id) = self.channel_state.samplers.get(config) {
            return id;
        }
        let id = self.slot_samplers.insert(*config);
        self.channel_state.samplers.insert(*config, id);
        id
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
    /// into `Maxwell3DRenderTargets` and provides the channel GPU->CPU translator here.
    pub fn update_render_targets_from_snapshot(
        &mut self,
        render_targets: &Maxwell3DRenderTargets,
        mut gpu_to_cpu: impl FnMut(GPUVAddr) -> Option<u64>,
    ) {
        for index in 0..NUM_RT {
            self.render_targets.draw_buffers[index] = render_targets.rt_control.map[index] as u8;
            self.render_targets.color_buffer_ids[index] = ImageViewId::default();
        }

        self.render_targets.is_rescaled = self.is_rescaling;

        for index in 0..NUM_RT {
            let rt = render_targets.render_targets[index];
            if rt.address == 0 || rt.width == 0 || rt.height == 0 || rt.format == 0 {
                continue;
            }
            let Some(cpu_addr) = gpu_to_cpu(rt.address) else {
                if std::env::var_os("RUZU_TRACE_RT").is_some() {
                    log::info!(
                        "[RT] miss translate color={} target={} gpu=0x{:X} {}x{} fmt=0x{:X}",
                        index,
                        index,
                        rt.address,
                        rt.width,
                        rt.height,
                        rt.format
                    );
                }
                continue;
            };

            let image_id = self.find_or_insert_render_target_image(
                &rt,
                render_targets.anti_alias_samples_mode,
                cpu_addr,
            );
            let view_id = self.find_render_target_view_from_image(
                image_id,
                &rt,
                render_targets.anti_alias_samples_mode,
                rt.address,
            );
            self.render_targets.color_buffer_ids[index] = view_id;

            if std::env::var_os("RUZU_TRACE_RT").is_some() {
                let image = &self.slot_images[image_id];
                log::info!(
                    "[RT] color={} target={} gpu=0x{:X} cpu=0x{:X} {}x{} fmt=0x{:X} image={} views={}",
                    index,
                    index,
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

        let zeta = render_targets.zeta;
        self.render_targets.depth_buffer_id = ImageViewId::default();
        if zeta.enabled && zeta.address != 0 && zeta.width != 0 && zeta.height != 0 {
            if let Some(cpu_addr) = gpu_to_cpu(zeta.address) {
                let info = ImageInfo::from_zeta_info(&zeta, render_targets.anti_alias_samples_mode);
                let image_id = self.find_or_insert_image_from_info(&info, zeta.address, cpu_addr);
                self.render_targets.depth_buffer_id =
                    self.find_image_view_from_image_info(image_id, &info, zeta.address);
            } else if std::env::var_os("RUZU_TRACE_RT").is_some() {
                log::info!(
                    "[RT] miss translate zeta gpu=0x{:X} {}x{} fmt=0x{:X}",
                    zeta.address,
                    zeta.width,
                    zeta.height,
                    zeta.format
                );
            }
        }
    }

    fn find_or_insert_render_target_image(
        &mut self,
        rt: &RenderTargetInfo,
        anti_alias_samples_mode: u32,
        cpu_addr: u64,
    ) -> ImageId {
        let info = ImageInfo::from_render_target_info(rt, anti_alias_samples_mode);
        self.find_or_insert_image_from_info(&info, rt.address, cpu_addr)
    }

    pub fn find_or_insert_image_from_info(
        &mut self,
        info: &ImageInfo,
        gpu_addr: GPUVAddr,
        cpu_addr: u64,
    ) -> ImageId {
        if let Some((image_id, _)) = self.slot_images.iter().find(|(_, image)| {
            image.gpu_addr == gpu_addr
                && image.cpu_addr == cpu_addr
                && image.info.size.width == info.size.width
                && image.info.size.height == info.size.height
                && image.info.format == info.format
        }) {
            return image_id;
        }

        let image_id = self
            .slot_images
            .insert(ImageBase::new(info.clone(), gpu_addr, cpu_addr));
        let image_size = self.slot_images[image_id].guest_size_bytes as usize;
        let map_id = self
            .slot_map_views
            .insert(ImageMapView::new(gpu_addr, cpu_addr, image_size, image_id));
        self.slot_images[image_id].map_view_id = map_id;
        self.register_image(image_id);
        image_id
    }

    /// Port of `TextureCache<P>::FindRenderTargetView` after the target image
    /// has been found or inserted.
    pub fn find_render_target_view_from_image(
        &mut self,
        image_id: ImageId,
        rt: &RenderTargetInfo,
        anti_alias_samples_mode: u32,
        gpu_addr: GPUVAddr,
    ) -> ImageViewId {
        let rt_info = ImageInfo::from_render_target_info(rt, anti_alias_samples_mode);
        self.find_image_view_from_image_info(image_id, &rt_info, gpu_addr)
    }

    pub fn find_image_view_from_image_info(
        &mut self,
        image_id: ImageId,
        rt_info: &ImageInfo,
        gpu_addr: GPUVAddr,
    ) -> ImageViewId {
        let image = &self.slot_images[image_id];
        let view_type = super::util::render_target_image_view_type(&rt_info);
        let base = if image.info.image_type == ImageType::Linear {
            SubresourceBase { level: 0, layer: 0 }
        } else {
            match image.try_find_base(gpu_addr) {
                Some(base) => base,
                None => return NULL_IMAGE_VIEW_ID,
            }
        };
        let layers = if image.info.image_type == ImageType::E3D {
            rt_info.size.depth as i32
        } else {
            rt_info.resources.layers
        };
        let info = ImageViewInfo::for_render_target(
            view_type,
            rt_info.format,
            SubresourceRange {
                base,
                extent: SubresourceExtent { levels: 1, layers },
            },
        );
        let existing = image.find_view(&info);
        if existing.is_valid() {
            return existing;
        }

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
        let mut map_id = self.slot_images[image_id].map_view_id;
        if !map_id.is_valid()
            && !self.slot_images[image_id]
                .flags
                .contains(ImageFlagBits::SPARSE)
        {
            let image = &self.slot_images[image_id];
            map_id = self.slot_map_views.insert(ImageMapView::new(
                image.gpu_addr,
                image.cpu_addr,
                image.guest_size_bytes as usize,
                image_id,
            ));
            self.slot_images[image_id].map_view_id = map_id;
        }
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
    /// Removes the image from CPU page tables and clears registration state.
    pub fn unregister_image(&mut self, image_id: ImageId) {
        let image = &self.slot_images[image_id];
        debug_assert!(
            image.flags.contains(ImageFlagBits::REGISTERED),
            "TextureCache::unregister_image: image not registered"
        );
        let is_sparse = image.flags.contains(ImageFlagBits::SPARSE);
        let map_ids = if is_sparse {
            self.sparse_views
                .remove(&image_id)
                .unwrap_or_default()
                .into_iter()
                .collect::<Vec<_>>()
        } else {
            let map_id = image.map_view_id;
            if map_id.is_valid() {
                vec![map_id]
            } else {
                Vec::new()
            }
        };

        for map_id in &map_ids {
            if !map_id.is_valid() {
                continue;
            }
            let map = &self.slot_map_views[*map_id];
            Self::for_each_cpu_page(map.cpu_addr, map.size, |page| {
                if let Some(image_map_ids) = self.page_table.get_mut(&page) {
                    image_map_ids.retain(|&id| id != *map_id);
                    if image_map_ids.is_empty() {
                        self.page_table.remove(&page);
                    }
                }
            });
        }
        for map_id in map_ids {
            if map_id.is_valid() {
                self.slot_map_views.erase(map_id);
            }
        }

        let image = &mut self.slot_images[image_id];
        image.flags.remove(ImageFlagBits::REGISTERED);
        image.flags.remove(ImageFlagBits::BAD_OVERLAP);
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
    pub fn delete_image(&mut self, image_id: ImageId, _immediate_delete: bool) {
        let image_view_ids = self.slot_images[image_id].image_view_ids.clone();
        let aliased_images = self.slot_images[image_id].aliased_images.clone();
        let overlapping_images = self.slot_images[image_id].overlapping_images.clone();
        let gpu_addr = self.slot_images[image_id].gpu_addr;

        debug_assert!(
            !self.slot_images[image_id]
                .flags
                .contains(ImageFlagBits::TRACKED),
            "TextureCache::delete_image: image was not untracked"
        );
        debug_assert!(
            !self.slot_images[image_id]
                .flags
                .contains(ImageFlagBits::REGISTERED),
            "TextureCache::delete_image: image was not unregistered"
        );

        self.has_deleted_images = true;

        for view_id in &image_view_ids {
            for color_buffer_id in &mut self.render_targets.color_buffer_ids {
                if *color_buffer_id == *view_id {
                    *color_buffer_id = ImageViewId::default();
                }
            }
            if self.render_targets.depth_buffer_id == *view_id {
                self.render_targets.depth_buffer_id = ImageViewId::default();
            }
        }
        self.framebuffers.clear();

        for alias in aliased_images {
            if alias.id == image_id {
                continue;
            }
            let other_image = &mut self.slot_images[alias.id];
            other_image
                .aliased_images
                .retain(|other_alias| other_alias.id != image_id);
            other_image.check_alias_state();
        }
        for overlap_id in overlapping_images {
            if overlap_id == image_id {
                continue;
            }
            let other_image = &mut self.slot_images[overlap_id];
            other_image
                .overlapping_images
                .retain(|&other_overlap_id| other_overlap_id != image_id);
            other_image.check_bad_overlap_state();
        }
        // Upstream `DeleteImage` purges every reference to the dying views
        // BEFORE the slots are reused: `RemoveImageViewReferences` drops the
        // per-channel descriptor→view caches, and the TIC tables are
        // invalidated so the next visit re-resolves every slot. Without this,
        // `find_image_view*` keeps serving the dead (or recycled!) view id —
        // MK8D's scene-RT image is evicted/recreated constantly by nvmap heap
        // unmaps, and the composite pass then samples a stale texture frozen
        // on the clear color (red splash / black demo).
        self.channel_state
            .image_views
            .retain(|_, id| !image_view_ids.contains(id));
        self.channel_state.graphics_image_table.invalidate();
        self.channel_state.compute_image_table.invalidate();

        for image_view_id in image_view_ids {
            if image_view_id != NULL_IMAGE_VIEW_ID && image_view_id.is_valid() {
                self.slot_image_views.erase(image_view_id);
            }
        }

        self.slot_images.erase(image_id);

        if let Some(alloc_id) = self.image_allocs_table.get(&gpu_addr).copied() {
            let alloc_images = &mut self.slot_image_allocs[alloc_id].images;
            alloc_images.retain(|&id| id != image_id);
            if alloc_images.is_empty() {
                self.slot_image_allocs.erase(alloc_id);
                self.image_allocs_table.remove(&gpu_addr);
            }
        }
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
    use crate::framebuffer_config::{AndroidPixelFormat, FramebufferConfig};
    use crate::textures::texture::{ComponentType, TextureFormat, TextureType, TicEntry};

    fn color_2d_tic(address: u64, base_layer: u32) -> TicEntry {
        let word0 = (TextureFormat::A8B8G8R8 as u32)
            | ((ComponentType::Unorm as u32) << 7)
            | ((ComponentType::Unorm as u32) << 10)
            | ((ComponentType::Unorm as u32) << 13)
            | ((ComponentType::Unorm as u32) << 16);
        let word1 = address as u32;
        let word2 = (((address >> 32) as u32) & 0xFFFF) | (3 << 21);
        let word3 = 0;
        let word4 = 63 | ((base_layer & 0x7) << 16) | ((TextureType::Texture2D as u32) << 23);
        let word5 = 31 | (1 << 31);

        TicEntry {
            raw: [
                word0 as u64 | ((word1 as u64) << 32),
                word2 as u64 | ((word3 as u64) << 32),
                word4 as u64 | ((word5 as u64) << 32),
                0,
            ],
        }
    }

    #[test]
    fn texture_cache_reserves_zero_for_null_resources() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;
        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let mut descriptor = crate::textures::texture::TscEntry::default();
        descriptor.raw[0] = 0x0000_03A2_0002_6080;

        let info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            size: crate::texture_cache::types::Extent3D {
                width: 16,
                height: 16,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        let image_id = cache
            .slot_images
            .insert(crate::texture_cache::image_base::ImageBase::new(
                info, 0x1000, 0x2000,
            ));

        let sampler_id = cache.find_sampler(&descriptor);

        assert_ne!(image_id, crate::texture_cache::types::NULL_IMAGE_ID);
        assert_eq!(image_id.index, 1);
        assert_ne!(sampler_id, crate::texture_cache::types::NULL_SAMPLER_ID);
        assert_eq!(sampler_id.index, 1);
    }

    #[test]
    fn update_render_targets_from_snapshot_registers_presentable_view() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;
        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let mut render_targets = Maxwell3DRenderTargets::default();
        render_targets.rt_control = RtControlInfo {
            count: 1,
            map: [0, 0, 0, 0, 0, 0, 0, 0],
        };
        render_targets.render_targets[0] = RenderTargetInfo {
            address: 0x4000_0000,
            width: 64,
            height: 32,
            format: 0xD5,
            tile_mode: 1 << 12,
            array_pitch: 32 * 4,
            depth: 1,
            base_layer: 0,
        };
        render_targets.zeta = crate::engines::maxwell_3d::ZetaInfo {
            enabled: true,
            address: 0x5000_0000,
            width: 64,
            height: 32,
            format: 0xA,
            tile_mode: 1 << 12,
            array_pitch: 32,
            depth: 1,
        };

        cache.update_render_targets_from_snapshot(&render_targets, |gpu_addr| match gpu_addr {
            0x4000_0000 => Some(0x535B_5000),
            0x5000_0000 => Some(0x535C_0000),
            _ => None,
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
        assert!(cache.render_targets.color_buffer_ids[0].is_valid());
        assert!(cache.render_targets.depth_buffer_id.is_valid());
        assert_eq!(cache.render_targets.draw_buffers[0], 0);
        assert_eq!(cache.slot_images.size(), 3);
        assert_eq!(cache.slot_image_views.size(), 4);
    }

    #[test]
    fn try_find_framebuffer_image_view_emplaces_display_specific_view() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use crate::texture_cache::image_view_info::SwizzleSource;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let mut render_targets = Maxwell3DRenderTargets::default();
        render_targets.rt_control = RtControlInfo {
            count: 1,
            map: [0, 0, 0, 0, 0, 0, 0, 0],
        };
        render_targets.render_targets[0] = RenderTargetInfo {
            address: 0x4000_0000,
            width: 64,
            height: 32,
            format: 0xD5,
            tile_mode: 1 << 12,
            array_pitch: 32 * 4,
            depth: 1,
            base_layer: 0,
        };

        cache.update_render_targets_from_snapshot(&render_targets, |gpu_addr| {
            (gpu_addr == 0x4000_0000).then_some(0x535B_5000)
        });
        let initial_view_count = cache.slot_image_views.size();

        let config = FramebufferConfig {
            address: 0x535B_5000,
            width: 64,
            height: 32,
            stride: 64,
            pixel_format: AndroidPixelFormat(5),
            ..Default::default()
        };
        let view = cache
            .try_find_framebuffer_image_view(&config, 0x535B_5000)
            .expect("framebuffer view");

        assert_eq!(cache.slot_image_views.size(), initial_view_count + 1);
        assert_eq!(
            cache.slot_image_views[view.view_id].format,
            surface::PixelFormat::B8G8R8A8Unorm
        );
        let image = &cache.slot_images[view.view.image_id];
        assert_eq!(
            image
                .image_view_infos
                .last()
                .expect("new view info")
                .w_source,
            SwizzleSource::OneFloat as u8
        );
    }

    #[test]
    fn write_memory_marks_registered_image_cpu_modified_and_untracks() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            size: crate::texture_cache::types::Extent3D {
                width: 16,
                height: 16,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        let image_id = cache.insert_image(&info, 0x4000);
        cache.slot_images[image_id]
            .flags
            .remove(ImageFlagBits::CPU_MODIFIED);
        cache.track_image(image_id);

        cache.write_memory(0x4000, 4);

        let image = &cache.slot_images[image_id];
        assert!(image.flags.contains(ImageFlagBits::CPU_MODIFIED));
        assert!(!image.flags.contains(ImageFlagBits::TRACKED));
    }

    #[test]
    fn unmap_memory_unregisters_untracks_and_deletes_image() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            size: crate::texture_cache::types::Extent3D {
                width: 16,
                height: 16,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        let image_id = cache.insert_image(&info, 0x6000);
        let map_id = cache.slot_images[image_id].map_view_id;
        assert!(map_id.is_valid());
        assert_eq!(cache.collect_images_in_region(0x6000, 4), vec![image_id]);

        cache.track_image(image_id);
        cache.unmap_memory(0x6000, 4);

        assert_eq!(cache.collect_images_in_region(0x6000, 4), Vec::new());
        assert_eq!(cache.slot_images.size(), 1);
        assert_eq!(cache.slot_map_views.size(), 0);
        assert!(cache.page_table.is_empty());
        assert!(cache.has_deleted_images);
    }

    #[test]
    fn create_image_view_uses_try_find_base_layer() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let mut descriptor = color_2d_tic(0, 2);
        let layer_stride = ImageInfo::from_tic_entry(&descriptor).layer_stride as u64;
        descriptor = color_2d_tic(0x5000_0000 + 2 * layer_stride, 2);

        let view_id = cache.create_image_view(&descriptor);

        assert!(view_id.is_valid());
        let view = &cache.slot_image_views[view_id];
        assert_eq!(view.range.base.layer, 2);
        assert_eq!(cache.slot_images[view.image_id].gpu_addr, 0x5000_0000);
    }
}
