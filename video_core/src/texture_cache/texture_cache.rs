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
use crate::engines::maxwell_dma::dma;
use crate::memory_manager::MemoryManager;
use crate::surface;
use parking_lot::Mutex as ParkingMutex;
use std::sync::Arc;
use std::sync::OnceLock;

use super::descriptor_table::DescriptorTable;
use super::image_base::{
    add_image_alias, GPUVAddr, ImageAllocBase, ImageBase, ImageFlagBits, ImageMapView,
};
use super::image_info::{ImageInfo, TilingMode};
use super::image_view_base::{ImageViewBase, ImageViewFlagBits};
use super::image_view_info::ImageViewInfo;
use super::texture_cache_base::*;
use super::types::*;

// All method implementations live on TextureCacheBase.

macro_rules! trace_texture_fill_stall {
    ($($arg:tt)*) => {
        if std::env::var_os("RUZU_TRACE_TEXTURE_FILL_STALL").is_some() {
            eprintln!($($arg)*);
        }
    };
}

fn parse_u64_env_list(name: &str) -> Option<Vec<u64>> {
    let spec = std::env::var(name).ok()?;
    let spec = spec.trim();
    if spec.is_empty() {
        return None;
    }
    if spec == "*" {
        return Some(Vec::new());
    }
    let values = spec
        .split(',')
        .filter_map(|raw| {
            let value = raw.trim();
            if value.is_empty() {
                return None;
            }
            if let Some(hex) = value
                .strip_prefix("0x")
                .or_else(|| value.strip_prefix("0X"))
            {
                u64::from_str_radix(hex, 16).ok()
            } else {
                value.parse::<u64>().ok()
            }
        })
        .collect::<Vec<_>>();
    (!values.is_empty()).then_some(values)
}

fn should_trace_texture_cache_addr(gpu_addr: GPUVAddr) -> bool {
    static TARGETS: OnceLock<Option<Vec<u64>>> = OnceLock::new();
    let Some(targets) = TARGETS
        .get_or_init(|| parse_u64_env_list("RUZU_TRACE_TEXTURE_CACHE_ADDRS"))
        .as_deref()
    else {
        return false;
    };
    targets.is_empty() || targets.contains(&gpu_addr)
}

pub(crate) struct FindOrInsertImageResult {
    pub image_id: ImageId,
    pub inserted: bool,
    pub needs_backend_completion: bool,
    pub queued_join_tail: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct DmaBufferImageCopyResult {
    pub image_id: ImageId,
    pub copy: BufferImageCopy,
}

impl TextureCacheBase {
    pub(crate) fn set_backend_completes_join_images(&mut self, enabled: bool) {
        self.backend_completes_join_images = enabled;
    }

    fn queue_backend_completion_for_inserted_result(&mut self, result: &FindOrInsertImageResult) {
        if result.needs_backend_completion && result.inserted && !result.queued_join_tail {
            self.pending_backend_insertions.push(result.image_id);
        }
    }

    pub fn set_channel_gpu_memory(&mut self, gpu_memory: Arc<ParkingMutex<MemoryManager>>) {
        self.channel_gpu_memory = Some(gpu_memory);
        self.rebase_virtual_invalid_images();
    }

    pub fn clear_channel_gpu_memory(&mut self) {
        self.channel_gpu_memory = None;
    }

    fn translated_cpu_addr(&self, gpu_addr: GPUVAddr, size: u64) -> Option<u64> {
        self.channel_gpu_memory
            .as_ref()
            .and_then(|gpu_memory| {
                let gpu_memory = gpu_memory.lock();
                gpu_memory
                    .gpu_to_cpu_address(gpu_addr)
                    .or_else(|| gpu_memory.gpu_to_cpu_address_range(gpu_addr, size))
            })
            .or_else(|| self.virtual_invalid_ranges.get(&(gpu_addr, size)).copied())
    }

    pub(crate) fn resolve_or_allocate_cpu_addr(&mut self, gpu_addr: GPUVAddr, size: u64) -> u64 {
        if let Some(cpu_addr) = self.channel_gpu_memory.as_ref().and_then(|gpu_memory| {
            let gpu_memory = gpu_memory.lock();
            gpu_memory
                .gpu_to_cpu_address(gpu_addr)
                .or_else(|| gpu_memory.gpu_to_cpu_address_range(gpu_addr, size))
        }) {
            return cpu_addr;
        }
        if let Some(&cpu_addr) = self.virtual_invalid_ranges.get(&(gpu_addr, size)) {
            return cpu_addr;
        }
        let fake_addr = !(1u64 << 40) + self.virtual_invalid_space;
        self.virtual_invalid_space += common::alignment::align_up(size, 32);
        self.virtual_invalid_ranges
            .insert((gpu_addr, size), fake_addr);
        fake_addr
    }

    fn sparse_segments_for_image(
        &self,
        image_id: ImageId,
        context: &'static str,
    ) -> Vec<(GPUVAddr, u64, usize)> {
        let image = &self.slot_images[image_id];
        let gpu_memory = self
            .channel_gpu_memory
            .as_ref()
            .unwrap_or_else(|| panic!("{context} sparse image requires channel GPU memory"))
            .lock();
        gpu_memory
            .get_submapped_range(image.gpu_addr, image.guest_size_bytes as u64)
            .into_iter()
            .map(|(segment_gpu_addr, segment_size)| {
                let cpu_addr = gpu_memory
                    .gpu_to_cpu_address(segment_gpu_addr)
                    .unwrap_or_else(|| panic!("{context} sparse segment must have CPU address"));
                (segment_gpu_addr, cpu_addr, segment_size as usize)
            })
            .collect()
    }

    pub(crate) fn rebase_virtual_invalid_images(&mut self) {
        let Some(gpu_memory) = self.channel_gpu_memory.as_ref().cloned() else {
            return;
        };
        let sentinel = !(1u64 << 40);
        let image_ids = self
            .slot_images
            .iter()
            .filter_map(|(id, image)| {
                (id != NULL_IMAGE_ID
                    && image.cpu_addr >= sentinel
                    && !image.flags.contains(ImageFlagBits::SPARSE))
                .then_some(id)
            })
            .collect::<Vec<_>>();

        for image_id in image_ids {
            let (gpu_addr, size, was_registered, was_tracked) = {
                let image = &self.slot_images[image_id];
                (
                    image.gpu_addr,
                    image.guest_size_bytes as u64,
                    image.flags.contains(ImageFlagBits::REGISTERED),
                    image.flags.contains(ImageFlagBits::TRACKED),
                )
            };
            let Some(cpu_addr) = ({
                let gpu_memory = gpu_memory.lock();
                gpu_memory
                    .gpu_to_cpu_address(gpu_addr)
                    .or_else(|| gpu_memory.gpu_to_cpu_address_range(gpu_addr, size))
            }) else {
                continue;
            };
            if was_tracked {
                self.untrack_image(image_id);
            }
            if was_registered {
                self.unregister_image(image_id);
            }
            {
                let image = &mut self.slot_images[image_id];
                image.cpu_addr = cpu_addr;
                image.cpu_addr_end = cpu_addr + image.guest_size_bytes as u64;
            }
            if was_registered {
                self.register_image(image_id);
            }
            if was_tracked {
                self.track_image(image_id);
            }
        }
    }

    fn can_add_image_alias(lhs: &ImageBase, rhs: &ImageBase) -> bool {
        if lhs.info.image_type != rhs.info.image_type {
            return false;
        }
        if lhs.info.image_type == ImageType::Linear {
            return true;
        }
        let options = RelaxedOptions::SIZE | RelaxedOptions::FORMAT;
        super::util::find_subresource(&rhs.info, lhs, rhs.gpu_addr, options, false, true).is_some()
    }

    // ── Garbage collection ─────────────────────────────────────────────

    /// Port of `TextureCache<P>::RunGarbageCollector`.
    pub fn run_garbage_collector(&mut self) {
        let downloader = self.image_downloader.as_ref().cloned();
        self.run_garbage_collector_with_downloader(|_image_id, image, staging| {
            let Some(downloader) = downloader.as_ref() else {
                return false;
            };
            downloader(_image_id, image, staging)
        });
    }

    /// Port of `TextureCache<P>::RunGarbageCollector`, with the backend
    /// `Runtime::DownloadStagingBuffer` + `Image::DownloadMemory` operation
    /// supplied by the concrete renderer wrapper.
    pub fn run_garbage_collector_with_downloader(
        &mut self,
        mut download_image: impl FnMut(ImageId, &ImageBase, &mut [u8]) -> bool,
    ) {
        let mut high_priority_mode = false;
        let mut aggressive_mode = false;
        let mut ticks_to_destroy = 0u64;
        let mut num_iterations = 0usize;

        let configure = |cache: &Self,
                         allow_aggressive: bool,
                         high_priority_mode: &mut bool,
                         aggressive_mode: &mut bool,
                         ticks_to_destroy: &mut u64,
                         num_iterations: &mut usize| {
            *high_priority_mode = cache.total_used_memory >= cache.expected_memory;
            *aggressive_mode = allow_aggressive && cache.total_used_memory >= cache.critical_memory;
            *ticks_to_destroy = if *aggressive_mode {
                10
            } else if *high_priority_mode {
                25
            } else {
                50
            };
            *num_iterations = if *aggressive_mode {
                40
            } else if *high_priority_mode {
                20
            } else {
                10
            };
        };

        configure(
            self,
            false,
            &mut high_priority_mode,
            &mut aggressive_mode,
            &mut ticks_to_destroy,
            &mut num_iterations,
        );
        self.cleanup_lru_images(
            self.frame_tick.saturating_sub(ticks_to_destroy),
            &mut num_iterations,
            &mut high_priority_mode,
            &mut aggressive_mode,
            &mut download_image,
        );

        if self.total_used_memory >= self.critical_memory {
            configure(
                self,
                true,
                &mut high_priority_mode,
                &mut aggressive_mode,
                &mut ticks_to_destroy,
                &mut num_iterations,
            );
            self.cleanup_lru_images(
                self.frame_tick.saturating_sub(ticks_to_destroy),
                &mut num_iterations,
                &mut high_priority_mode,
                &mut aggressive_mode,
                &mut download_image,
            );
        }
    }

    fn cleanup_lru_images(
        &mut self,
        tick_threshold: u64,
        num_iterations: &mut usize,
        high_priority_mode: &mut bool,
        aggressive_mode: &mut bool,
        download_image: &mut impl FnMut(ImageId, &ImageBase, &mut [u8]) -> bool,
    ) {
        let mut candidates = Vec::new();
        self.lru_cache
            .for_each_item_below(tick_threshold as i64, |image_id| {
                candidates.push(image_id);
                false
            });

        for image_id in candidates {
            if *num_iterations == 0 {
                break;
            }
            if !image_id.is_valid() || image_id == NULL_IMAGE_ID {
                continue;
            }

            *num_iterations -= 1;
            let image = &self.slot_images[image_id];
            if image.flags.contains(ImageFlagBits::IS_DECODING) {
                continue;
            }
            if !*aggressive_mode && image.flags.contains(ImageFlagBits::COSTLY_LOAD) {
                continue;
            }
            let must_download =
                image.is_safe_download() && !image.flags.contains(ImageFlagBits::BAD_OVERLAP);
            if !*high_priority_mode && must_download {
                continue;
            }
            if must_download && !self.download_image_for_gc(image_id, download_image) {
                continue;
            }
            if self.slot_images[image_id]
                .flags
                .contains(ImageFlagBits::TRACKED)
            {
                self.untrack_image(image_id);
            }
            self.unregister_image(image_id);
            let immediate_delete = self.slot_images[image_id].scale_tick > self.frame_tick + 5;
            self.delete_image(image_id, immediate_delete);

            if self.total_used_memory < self.critical_memory {
                if *aggressive_mode {
                    *num_iterations >>= 2;
                    *aggressive_mode = false;
                    break;
                }
                if *high_priority_mode && self.total_used_memory < self.expected_memory {
                    *num_iterations >>= 1;
                    *high_priority_mode = false;
                }
            }
        }
    }

    fn download_image_for_gc(
        &mut self,
        image_id: ImageId,
        download_image: &mut impl FnMut(ImageId, &ImageBase, &mut [u8]) -> bool,
    ) -> bool {
        let image = self.slot_images[image_id].clone();
        let mut staging = vec![0u8; image.unswizzled_size_bytes as usize];
        if !download_image(image_id, &image, &mut staging) {
            return false;
        }
        let copies = super::util::full_download_copies(&image.info);
        if !self.write_downloaded_image(&image, &copies, &staging) {
            return false;
        }
        self.slot_images[image_id]
            .flags
            .remove(ImageFlagBits::GPU_MODIFIED);
        true
    }

    /// Common writeback half of upstream `SwizzleImage(*gpu_memory, image.gpu_addr, ...)`.
    pub fn write_downloaded_image(
        &mut self,
        image: &ImageBase,
        copies: &[BufferImageCopy],
        staging: &[u8],
    ) -> bool {
        if let Some(gpu_memory) = self.channel_gpu_memory.as_ref().cloned() {
            let gpu_memory = gpu_memory.lock();
            super::util::swizzle_image(
                &|gpu_addr, data| {
                    let _ = gpu_memory.write_block_unsafe(gpu_addr, data);
                },
                image.gpu_addr,
                &image.info,
                copies,
                staging,
                &mut self.swizzle_data_buffer,
            );
            return true;
        }

        let Some(writer) = self.guest_memory_writer.as_ref().cloned() else {
            return false;
        };
        super::util::swizzle_image(
            writer.as_ref(),
            image.cpu_addr,
            &image.info,
            copies,
            staging,
            &mut self.swizzle_data_buffer,
        );
        true
    }

    fn registered_image_memory_size(image: &ImageBase) -> u64 {
        let mut tentative_size = u64::from(image.guest_size_bytes.max(image.unswizzled_size_bytes));
        if (surface::is_pixel_format_astc(image.info.format)
            && image.flags.contains(ImageFlagBits::ACCELERATED_UPLOAD))
            || image.flags.contains(ImageFlagBits::CONVERTED)
        {
            tentative_size = surface::transcoded_astc_size(tentative_size, image.info.format);
        }
        common::alignment::align_up(tentative_size, 1024)
    }

    fn scaled_image_memory_size(image: &ImageBase) -> u64 {
        let resolution = common::settings::values().resolution_info.clone();
        let scale_up = (resolution.up_scale * resolution.up_scale) as u64;
        let down_shift = (resolution.down_shift + resolution.down_shift) as u64;
        let image_size_bytes = u64::from(image.guest_size_bytes.max(image.unswizzled_size_bytes));
        let tentative_size = (image_size_bytes * scale_up) >> down_shift;
        common::alignment::align_up(tentative_size, 1024)
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
        let mut iteration = 0u64;
        loop {
            iteration = iteration.wrapping_add(1);
            trace_texture_fill_stall!(
                "[TEXTURE_FILL_STALL] fill_image_views iter={} graphics={} views={} has_blacklists={}",
                iteration,
                graphics,
                views.len(),
                has_blacklists
            );
            self.has_deleted_images = false;
            has_blacklisted = false;
            for (slot, view) in views.iter_mut().enumerate() {
                trace_texture_fill_stall!(
                    "[TEXTURE_FILL_STALL] before_visit iter={} slot={} index={} blacklist={}",
                    iteration,
                    slot,
                    view.index,
                    view.blacklist
                );
                view.id = self.visit_image_view(graphics, view.index);
                trace_texture_fill_stall!(
                    "[TEXTURE_FILL_STALL] after_visit iter={} slot={} index={} id={} has_deleted={}",
                    iteration,
                    slot,
                    view.index,
                    view.id.index,
                    self.has_deleted_images
                );
                if has_blacklists && view.blacklist && view.id != NULL_IMAGE_VIEW_ID {
                    // Upstream `ScaleDown(slot_images[image_view.image_id])`
                    // is backend-owned. Concrete texture-cache wrappers apply
                    // it after this base descriptor pass.
                }
            }
            trace_texture_fill_stall!(
                "[TEXTURE_FILL_STALL] iter_done iter={} has_deleted={} has_blacklisted={}",
                iteration,
                self.has_deleted_images,
                has_blacklisted
            );
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
    /// fields + the appropriate GPU-memory owner so `find_image_view` can
    /// take `&mut self`.
    fn visit_image_view(&mut self, graphics: bool, index: u32) -> ImageViewId {
        if std::env::var_os("RUZU_TRACE_VISIT_TIC").is_some() {
            let table = if graphics {
                &self.current_channel_state().graphics_image_table
            } else {
                &self.current_channel_state().compute_image_table
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
        // Step 1: read the TIC descriptor with a local borrow on the table only.
        let (descriptor, is_new) = {
            let channel_gpu_memory = self.channel_gpu_memory.as_ref().cloned();
            let device_memory = self.device_memory.clone();
            let table = if graphics {
                &mut self.current_channel_state_mut().graphics_image_table
            } else {
                &mut self.current_channel_state_mut().compute_image_table
            };
            if index > table.limit() {
                if std::env::var_os("RUZU_TRACE_VISIT_RESULT").is_some() {
                    log::warn!(
                        "[VISIT_RESULT] graphics={} index={} rejected limit={}",
                        graphics,
                        index,
                        table.limit(),
                    );
                }
                return NULL_IMAGE_VIEW_ID;
            }
            trace_texture_fill_stall!(
                "[TEXTURE_FILL_STALL] before_table_read graphics={} index={} limit={}",
                graphics,
                index,
                table.limit()
            );
            if let Some(gpu_memory) = channel_gpu_memory {
                table.read_with(index, |gpu_addr, out| {
                    trace_texture_fill_stall!(
                        "[TEXTURE_FILL_STALL] before_tic_read_block graphics={} index={} gpu=0x{:X} len={}",
                        graphics,
                        index,
                        gpu_addr,
                        out.len()
                    );
                    gpu_memory.lock().read_block(gpu_addr, out)
                })
            } else {
                table.read(&*device_memory, index)
            }
        };
        trace_texture_fill_stall!(
            "[TEXTURE_FILL_STALL] after_table_read graphics={} index={} desc_addr=0x{:X} fmt=0x{:X} is_new={}",
            graphics,
            index,
            descriptor.address(),
            descriptor.format(),
            is_new
        );
        if std::env::var_os("RUZU_TRACE_VISIT_RESULT").is_some() {
            log::warn!(
                "[VISIT_RESULT] graphics={} index={} desc_addr=0x{:X} desc_fmt=0x{:X} is_new={}",
                graphics,
                index,
                descriptor.address(),
                descriptor.format(),
                is_new,
            );
        }
        // Step 2: resolve on first descriptor read, and also recover if a
        // shader-environment descriptor probe read the TIC before the image
        // view id cache was populated.
        let cached = {
            let cached_ids = if graphics {
                &self.current_channel_state().graphics_image_view_ids
            } else {
                &self.current_channel_state().compute_image_view_ids
            };
            cached_ids[index as usize]
        };
        if is_new || !cached.is_valid() || cached == CORRUPT_ID {
            trace_texture_fill_stall!(
                "[TEXTURE_FILL_STALL] before_find_image_view graphics={} index={} cached={}",
                graphics,
                index,
                cached.index
            );
            let new_id = self.find_image_view(&descriptor);
            trace_texture_fill_stall!(
                "[TEXTURE_FILL_STALL] after_find_image_view graphics={} index={} new_id={}",
                graphics,
                index,
                new_id.index
            );
            if std::env::var_os("RUZU_TRACE_VISIT_RESULT").is_some() {
                log::warn!(
                    "[VISIT_RESULT] graphics={} index={} cached={} new_id={}",
                    graphics,
                    index,
                    cached.index,
                    new_id.index,
                );
            }
            let cached_ids = if graphics {
                &mut self.current_channel_state_mut().graphics_image_view_ids
            } else {
                &mut self.current_channel_state_mut().compute_image_view_ids
            };
            cached_ids[index as usize] = new_id;
            return new_id;
        }
        if std::env::var_os("RUZU_TRACE_VISIT_RESULT").is_some() {
            log::warn!(
                "[VISIT_RESULT] graphics={} index={} cached={} reused",
                graphics,
                index,
                cached.index,
            );
        }
        // Step 3: return the (now stable) cached id.
        cached
    }

    /// Diagnostic helper for [TIC_LOOKUP]: resolve the PixelFormat of the
    /// image backing `view_id` (or a marker when null/imageless).
    fn tic_lookup_image_format(&self, view_id: ImageViewId) -> String {
        if !view_id.is_valid() {
            return "null_view".to_string();
        }
        let view = self.slot_image_views.get(view_id);
        let image_id = view.image_id;
        if !image_id.is_valid() {
            return "no_image".to_string();
        }
        format!("{:?}", self.slot_images.get(image_id).info.format)
    }

    /// Port of `TextureCache<P>::FindImageView` (texture_cache.h:1103-1113).
    /// Guards on `IsValidEntry`, then does a HashMap try_emplace against the
    /// descriptor; on cache miss, calls `create_image_view`.
    fn find_image_view(&mut self, descriptor: &crate::textures::texture::TicEntry) -> ImageViewId {
        if let Some(gpu_memory) = self.channel_gpu_memory.as_ref().cloned() {
            return self.find_image_view_with_gpu_to_cpu(descriptor, &mut |gpu_addr, size| {
                let gpu_memory = gpu_memory.lock();
                gpu_memory
                    .gpu_to_cpu_address(gpu_addr)
                    .or_else(|| gpu_memory.gpu_to_cpu_address_range(gpu_addr, size))
            });
        }
        let gpu_memory = self.device_memory.clone();
        if !super::util::is_valid_entry(&*gpu_memory, descriptor) {
            return NULL_IMAGE_VIEW_ID;
        }
        if let Some(&id) = self.current_channel_state_mut().image_views.get(descriptor) {
            if {
                static TIC_TRACE: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
                *TIC_TRACE.get_or_init(|| std::env::var_os("RUZU_TRACE_TIC_LOOKUP").is_some())
            } {
                log::warn!(
                    "[TIC_LOOKUP] cached gpu=0x{:X} tic_format=0x{:X} view_id={} img_fmt={} (cached)",
                    descriptor.address(),
                    descriptor.format(),
                    id.index,
                    self.tic_lookup_image_format(id)
                );
            }
            return id;
        }
        let addr = descriptor.address();
        let new_id = self.create_image_view(descriptor);
        if {
            static TIC_TRACE: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
            *TIC_TRACE.get_or_init(|| std::env::var_os("RUZU_TRACE_TIC_LOOKUP").is_some())
        } {
            log::warn!(
                "[TIC_LOOKUP] new gpu=0x{:X} tic_format=0x{:X} view_id={} img_fmt={} (created)",
                addr,
                descriptor.format(),
                new_id.index,
                self.tic_lookup_image_format(new_id)
            );
        }
        self.current_channel_state_mut()
            .image_views
            .insert(*descriptor, new_id);
        new_id
    }

    fn find_image_view_with_gpu_to_cpu(
        &mut self,
        descriptor: &crate::textures::texture::TicEntry,
        gpu_to_cpu: &mut dyn FnMut(GPUVAddr, u64) -> Option<u64>,
    ) -> ImageViewId {
        if !super::util::is_valid_entry_with_range_valid(descriptor, |gpu_addr, size| {
            gpu_to_cpu(gpu_addr, size).is_some()
        }) {
            return NULL_IMAGE_VIEW_ID;
        }
        if let Some(&id) = self.current_channel_state_mut().image_views.get(descriptor) {
            if {
                static TIC_TRACE: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
                *TIC_TRACE.get_or_init(|| std::env::var_os("RUZU_TRACE_TIC_LOOKUP").is_some())
            } {
                log::warn!(
                    "[TIC_LOOKUP] cached gpu=0x{:X} tic_format=0x{:X} view_id={} img_fmt={} (cached/addr_valid)",
                    descriptor.address(),
                    descriptor.format(),
                    id.index,
                    self.tic_lookup_image_format(id)
                );
            }
            return id;
        }
        let new_id = self.create_image_view_with_gpu_to_cpu(descriptor, gpu_to_cpu);
        if {
            static TIC_TRACE: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
            *TIC_TRACE.get_or_init(|| std::env::var_os("RUZU_TRACE_TIC_LOOKUP").is_some())
        } {
            log::warn!(
                "[TIC_LOOKUP] new gpu=0x{:X} tic_format=0x{:X} view_id={} img_fmt={} (created/addr_valid)",
                descriptor.address(),
                descriptor.format(),
                new_id.index,
                self.tic_lookup_image_format(new_id)
            );
        }
        self.current_channel_state_mut()
            .image_views
            .insert(*descriptor, new_id);
        new_id
    }

    fn create_image_view_with_gpu_to_cpu(
        &mut self,
        descriptor: &crate::textures::texture::TicEntry,
        gpu_to_cpu: &mut dyn FnMut(GPUVAddr, u64) -> Option<u64>,
    ) -> ImageViewId {
        let info = super::image_info::ImageInfo::from_tic_entry(descriptor);
        if info.image_type == ImageType::Buffer {
            let view_info = super::image_view_info::ImageViewInfo::from_tic_entry(descriptor, 0);
            let view = ImageViewBase::new_buffer(&info, &view_info, descriptor.address());
            return self.slot_image_views.insert(view);
        }
        let layer_offset = descriptor.base_layer() as u64 * info.layer_stride as u64;
        let image_gpu_addr = descriptor.address().wrapping_sub(layer_offset);
        let image_size = super::util::calculate_guest_size_in_bytes(&info);
        let cpu_addr = gpu_to_cpu(image_gpu_addr, image_size as u64).unwrap_or_else(|| {
            self.resolve_or_allocate_cpu_addr(image_gpu_addr, image_size as u64)
        });
        let result = self.find_or_insert_image_from_info_with_options_result(
            &info,
            image_gpu_addr,
            cpu_addr,
            RelaxedOptions::empty(),
        );
        self.queue_backend_completion_for_inserted_result(&result);
        let image_id = result.image_id;
        if image_id == NULL_IMAGE_ID {
            return NULL_IMAGE_VIEW_ID;
        }
        let base = Self::create_image_view_base(self.slot_images.get(image_id), descriptor);
        let view_info =
            super::image_view_info::ImageViewInfo::from_tic_entry(descriptor, base.layer);
        let view_id = self.find_or_emplace_image_view(image_id, view_info, descriptor.address());
        self.slot_image_views.get_mut(view_id).flags |=
            super::image_view_base::ImageViewFlagBits::STRONG;
        self.slot_images.get_mut(image_id).flags |= ImageFlagBits::STRONG;
        view_id
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
        let result = if self.backend_completes_join_images {
            let image_size = super::util::calculate_guest_size_in_bytes(&info) as u64;
            let cpu_addr = self.resolve_or_allocate_cpu_addr(image_gpu_addr, image_size);
            self.find_or_insert_image_from_info_with_options_result(
                &info,
                image_gpu_addr,
                cpu_addr,
                RelaxedOptions::empty(),
            )
        } else {
            FindOrInsertImageResult {
                image_id: self.find_or_insert_image(&info, image_gpu_addr),
                inserted: false,
                needs_backend_completion: false,
                queued_join_tail: false,
            }
        };
        self.queue_backend_completion_for_inserted_result(&result);
        let image_id = result.image_id;
        if image_id == NULL_IMAGE_ID {
            return NULL_IMAGE_VIEW_ID;
        }
        let base = Self::create_image_view_base(self.slot_images.get(image_id), descriptor);
        let view_info =
            super::image_view_info::ImageViewInfo::from_tic_entry(descriptor, base.layer);
        let view_id = self.find_or_emplace_image_view(image_id, view_info, descriptor.address());
        // Upstream tags both the view and its image as `Strong`. Bitflags
        // already supports `|=` on the existing `flags` fields.
        self.slot_image_views.get_mut(view_id).flags |=
            super::image_view_base::ImageViewFlagBits::STRONG;
        self.slot_images.get_mut(image_id).flags |= ImageFlagBits::STRONG;
        view_id
    }

    fn create_image_view_base(
        image: &ImageBase,
        descriptor: &crate::textures::texture::TicEntry,
    ) -> SubresourceBase {
        let base = image
            .try_find_base(descriptor.address())
            .expect("TextureCache::CreateImageView TryFindBase failed");
        assert_eq!(
            base.level, 0,
            "TextureCache::CreateImageView base level must be zero"
        );
        base
    }

    /// Port of `TextureCache<P>::FindOrInsertImage` (texture_cache.h:1140-1146).
    /// Looks up an existing image that can satisfy `info` at `gpu_addr`;
    /// on miss, inserts a fresh `ImageBase` keyed by that address.
    pub(crate) fn find_or_insert_image(
        &mut self,
        info: &super::image_info::ImageInfo,
        gpu_addr: GPUVAddr,
    ) -> ImageId {
        assert!(
            !self.backend_completes_join_images,
            "TextureCacheBase::find_or_insert_image cannot return a fully-completed ImageId when backend completion is required"
        );
        if let Some(id) = self.find_image(info, gpu_addr) {
            return id;
        }
        self.insert_image(info, gpu_addr)
    }

    /// Port of `TextureCache<P>::FindImage` (texture_cache.h:1149-1202).
    ///
    /// Upstream first translates the requested GPU range to a CPU/device
    /// address and returns the null image on translation failure. Candidate
    /// reuse is then restricted to images registered in that CPU region.
    pub(crate) fn find_image(
        &self,
        info: &super::image_info::ImageInfo,
        gpu_addr: GPUVAddr,
    ) -> Option<ImageId> {
        self.find_image_with_caps(
            info,
            gpu_addr,
            RelaxedOptions::empty(),
            self.has_broken_texture_view_formats,
            self.has_native_bgr,
        )
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
        let size = super::util::calculate_guest_size_in_bytes(info) as u64;
        let cpu_addr = self.translated_cpu_addr(gpu_addr, size)?;
        self.find_image_in_cpu_region_with_caps(
            info,
            gpu_addr,
            cpu_addr,
            options,
            broken_views,
            native_bgr,
        )
    }

    /// CPU-region bounded variant of upstream `TextureCache<P>::FindImage`.
    ///
    /// Upstream translates the candidate GPU address to a CPU/device address
    /// and scans only images registered in that backing region. It accepts
    /// compatible texture views, not just exact format matches, and chooses
    /// the most recently modified candidate when multiple images overlap.
    pub(crate) fn find_image_in_cpu_region_with_caps(
        &self,
        info: &super::image_info::ImageInfo,
        gpu_addr: GPUVAddr,
        cpu_addr: u64,
        options: RelaxedOptions,
        broken_views: bool,
        native_bgr: bool,
    ) -> Option<ImageId> {
        let broken_views = broken_views || options.contains(RelaxedOptions::FORCE_BROKEN_VIEWS);
        let flexible_formats = options.contains(RelaxedOptions::FORMAT);
        let size_bytes = super::util::calculate_guest_size_in_bytes(info) as usize;
        let mut image_id = None;
        let mut image_ids = Vec::new();

        for existing_image_id in self.collect_images_in_region(cpu_addr, size_bytes) {
            let existing_image = &self.slot_images[existing_image_id];
            if existing_image.flags.contains(ImageFlagBits::REMAPPED) {
                continue;
            }

            let matched = if info.image_type == ImageType::Linear
                || existing_image.info.image_type == ImageType::Linear
            {
                let strict_size = !options.contains(RelaxedOptions::SIZE)
                    && existing_image.flags.contains(ImageFlagBits::STRONG);
                let existing = &existing_image.info;
                existing_image.gpu_addr == gpu_addr
                    && existing.image_type == info.image_type
                    && existing.pitch() == info.pitch()
                    && super::util::is_pitch_linear_same_size(existing, info, strict_size)
                    && surface::is_view_compatible(
                        existing.format,
                        info.format,
                        broken_views,
                        native_bgr,
                    )
            } else {
                super::util::is_subresource(
                    info,
                    existing_image,
                    gpu_addr,
                    options,
                    broken_views,
                    native_bgr,
                )
            };

            if !matched {
                continue;
            }
            image_id = Some(existing_image_id);
            image_ids.push(existing_image_id);
            if !flexible_formats && existing_image.info.format == info.format {
                break;
            }
        }

        if image_ids.len() <= 1 {
            return image_id;
        }
        image_ids
            .into_iter()
            .max_by_key(|&id| self.slot_images[id].modification_tick)
    }

    /// Port of `InsertImage` minus the backend `slot_images.insert(runtime,
    /// info, ...)` upload glue. Resolves the CPU/device address in the same
    /// order as upstream: direct GPU translation, range translation, then a
    /// virtual-invalid fake CPU range.
    pub(crate) fn insert_image(
        &mut self,
        info: &super::image_info::ImageInfo,
        gpu_addr: GPUVAddr,
    ) -> ImageId {
        assert!(
            !self.backend_completes_join_images,
            "TextureCacheBase::insert_image cannot complete JoinImages without the backend owner"
        );
        assert!(
            !info.is_sparse || self.channel_gpu_memory.is_some(),
            "TextureCache::insert_image sparse image requires channel GPU memory"
        );
        let size = super::util::calculate_guest_size_in_bytes(info) as u64;
        let cpu_addr = self.resolve_or_allocate_cpu_addr(gpu_addr, size);
        let image_id = self.join_images(info, gpu_addr, cpu_addr);
        self.register_image_alloc(image_id);
        image_id
    }

    /// Port of `TextureCache<P>::CheckFeedbackLoop`.
    ///
    /// Checks whether any sampled image view matches a current render target;
    /// if so, emits a barrier via `Runtime::BarrierFeedbackLoop`.
    pub fn check_feedback_loop(&self, views: &[ImageViewInOut]) -> bool {
        if !*common::settings::values()
            .barrier_feedback_loops
            .get_value()
        {
            return false;
        }

        for view in views {
            if !view.id.is_valid() || view.id == NULL_IMAGE_VIEW_ID {
                continue;
            }
            let image_view = &self.slot_image_views[view.id];
            for &ct_view_id in &self.render_targets.color_buffer_ids {
                if !ct_view_id.is_valid() || ct_view_id == NULL_IMAGE_VIEW_ID {
                    continue;
                }
                let ct_view = &self.slot_image_views[ct_view_id];
                if image_view.image_id == ct_view.image_id {
                    return true;
                }
            }

            let depth_buffer_id = self.render_targets.depth_buffer_id;
            if depth_buffer_id.is_valid() && depth_buffer_id != NULL_IMAGE_VIEW_ID {
                let zt_view = &self.slot_image_views[depth_buffer_id];
                if image_view.image_id == zt_view.image_id {
                    return true;
                }
            }
        }

        false
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
        if std::env::var_os("RUZU_TRACE_DESCRIPTOR_SYNC").is_some() {
            log::warn!(
                "[DESC_SYNC] graphics tic_addr=0x{:X} tic_limit={} tsc_addr=0x{:X} tsc_limit={} via_header={}",
                regs.tex_header_addr,
                tic_limit,
                regs.tex_sampler_addr,
                tsc_limit,
                regs.sampler_binding_via_header,
            );
        }

        let channel = self.current_channel_state_mut();
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

    /// Port of `TextureCache<P>::SynchronizeComputeDescriptors`
    /// (texture_cache.h:310-322).
    ///
    /// Upstream reads `kepler_compute->regs` and
    /// `kepler_compute->launch_description.linked_tsc` directly. Ruzu passes
    /// those values as a snapshot, matching the graphics descriptor sync
    /// pattern.
    pub fn synchronize_compute_descriptors(
        &mut self,
        regs: crate::texture_cache::texture_cache_base::ComputeDescriptorSyncRegs,
    ) {
        let tic_limit = regs.tic_limit;
        let tsc_limit = if regs.linked_tsc {
            tic_limit
        } else {
            regs.tsc_limit
        };

        let channel = self.current_channel_state_mut();
        if channel
            .compute_sampler_table
            .synchronize(regs.tsc_addr, tsc_limit)
        {
            channel
                .compute_sampler_ids
                .resize(tsc_limit as usize + 1, CORRUPT_ID);
        }
        if channel
            .compute_image_table
            .synchronize(regs.tic_addr, tic_limit)
        {
            channel
                .compute_image_view_ids
                .resize(tic_limit as usize + 1, CORRUPT_ID);
        }
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
        if index
            > self
                .current_channel_state_mut()
                .graphics_sampler_table
                .limit()
        {
            log::debug!(
                "TextureCacheBase::get_graphics_sampler_id: invalid index={}",
                index
            );
            return NULL_SAMPLER_ID;
        }
        let (descriptor, is_new) =
            if let Some(gpu_memory) = self.channel_gpu_memory.as_ref().cloned() {
                self.current_channel_state_mut()
                    .graphics_sampler_table
                    .read_with(index, |gpu_addr, out| {
                        gpu_memory.lock().read_block(gpu_addr, out)
                    })
            } else {
                let gpu_memory_arc = self.device_memory.clone();
                self.current_channel_state_mut()
                    .graphics_sampler_table
                    .read(gpu_memory_arc.as_ref(), index)
            };
        let cached = self.current_channel_state_mut().graphics_sampler_ids[index as usize];
        if !is_new && cached.is_valid() && cached != CORRUPT_ID {
            return cached;
        }
        let id = self.find_sampler(&descriptor);
        self.current_channel_state_mut().graphics_sampler_ids[index as usize] = id;
        id
    }

    /// Resolve a compute-stage sampler index to a `SamplerId`.
    ///
    /// Port of `TextureCache<P>::GetComputeSamplerId` (texture_cache.h:270-281).
    pub fn get_compute_sampler_id(&mut self, index: u32) -> SamplerId {
        use crate::texture_cache::types::NULL_SAMPLER_ID;
        if index
            > self
                .current_channel_state_mut()
                .compute_sampler_table
                .limit()
        {
            log::debug!(
                "TextureCacheBase::get_compute_sampler_id: invalid index={}",
                index
            );
            return NULL_SAMPLER_ID;
        }
        let (descriptor, is_new) =
            if let Some(gpu_memory) = self.channel_gpu_memory.as_ref().cloned() {
                self.current_channel_state_mut()
                    .compute_sampler_table
                    .read_with(index, |gpu_addr, out| {
                        gpu_memory.lock().read_block(gpu_addr, out)
                    })
            } else {
                let gpu_memory_arc = self.device_memory.clone();
                self.current_channel_state_mut()
                    .compute_sampler_table
                    .read(gpu_memory_arc.as_ref(), index)
            };
        let cached = self.current_channel_state_mut().compute_sampler_ids[index as usize];
        if !is_new && cached.is_valid() && cached != CORRUPT_ID {
            return cached;
        }
        let id = self.find_sampler(&descriptor);
        self.current_channel_state_mut().compute_sampler_ids[index as usize] = id;
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
        if let Some(&id) = self.current_channel_state_mut().samplers.get(config) {
            return id;
        }
        let id = self.slot_samplers.insert(*config);
        self.current_channel_state_mut()
            .samplers
            .insert(*config, id);
        id
    }

    // ── Render targets ─────────────────────────────────────────────────

    /// Port of `TextureCache<P>::RescaleRenderTargets`.
    ///
    /// Iterates dirty render target slots, resolves images, and decides
    /// whether to scale up or down based on scale ratings.  Returns whether
    /// the final render targets are rescaled.
    pub fn rescale_render_targets(&mut self) -> bool {
        panic!(
            "TextureCacheBase::rescale_render_targets cannot preserve upstream ScaleUp/ScaleDown ordering without a backend runtime; use the backend texture-cache wrapper"
        );
    }

    /// Port of `TextureCache<P>::UpdateRenderTargets`.
    ///
    /// Calls `RescaleRenderTargets` when dirty, prepares all color and depth
    /// image views, and updates `render_targets.size`.
    pub fn update_render_targets(&mut self, _is_clear: bool) {
        panic!(
            "TextureCacheBase::update_render_targets cannot prepare render-target image views without a backend runtime; use the backend texture-cache wrapper"
        );
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
        mut gpu_to_cpu: impl FnMut(GPUVAddr, u64) -> Option<u64>,
    ) {
        let mut dirty_flags = [false; 256];
        dirty_flags[crate::dirty_flags::flags::RENDER_TARGETS as usize] = true;
        dirty_flags[crate::dirty_flags::flags::RENDER_TARGET_CONTROL as usize] = true;
        self.update_render_targets_from_snapshot_with_dirty_flags(
            render_targets,
            &dirty_flags,
            gpu_to_cpu,
        );
    }

    pub fn update_render_targets_from_snapshot_with_dirty_flags(
        &mut self,
        render_targets: &Maxwell3DRenderTargets,
        dirty_flags: &[bool; 256],
        mut gpu_to_cpu: impl FnMut(GPUVAddr, u64) -> Option<u64>,
    ) {
        if !dirty_flags[crate::dirty_flags::flags::RENDER_TARGETS as usize] {
            return;
        }
        let force_rt_lookup =
            dirty_flags[crate::dirty_flags::flags::RENDER_TARGET_CONTROL as usize];
        for index in 0..NUM_RT {
            self.render_targets.draw_buffers[index] = render_targets.rt_control.map[index] as u8;
        }

        let resolution = common::settings::values().resolution_info.clone();
        let (up_scale, down_shift) = if self.is_rescaling {
            (resolution.up_scale, resolution.down_shift)
        } else {
            (1, 0)
        };
        self.render_targets.is_rescaled = self.is_rescaling;
        self.render_targets.size = Extent2D {
            width: (render_targets.surface_clip.width.wrapping_mul(up_scale)) >> down_shift,
            height: (render_targets.surface_clip.height.wrapping_mul(up_scale)) >> down_shift,
        };

        for index in 0..NUM_RT {
            let color_flag = crate::dirty_flags::flags::COLOR_BUFFER0 + index as u8;
            if !force_rt_lookup && !dirty_flags[color_flag as usize] {
                continue;
            }
            if index >= render_targets.rt_control.count as usize {
                self.bind_color_render_target(index, ImageViewId::default());
                continue;
            }
            let rt = render_targets.render_targets[index];
            if rt.address == 0 || rt.width == 0 || rt.height == 0 || rt.format == 0 {
                self.bind_color_render_target(index, ImageViewId::default());
                continue;
            }
            let info =
                ImageInfo::from_render_target_info(&rt, render_targets.anti_alias_samples_mode);
            let guest_size = super::util::calculate_guest_size_in_bytes(&info) as u64;
            let cpu_addr = gpu_to_cpu(rt.address, guest_size).unwrap_or_else(|| {
                if std::env::var_os("RUZU_TRACE_RT").is_some() {
                    log::info!(
                        "[RT] miss translate color={} target={} gpu=0x{:X} {}x{} fmt=0x{:X}; using virtual-invalid fallback",
                        index,
                        index,
                        rt.address,
                        rt.width,
                        rt.height,
                        rt.format
                    );
                }
                self.resolve_or_allocate_cpu_addr(rt.address, guest_size)
            });

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
            self.bind_color_render_target(index, view_id);

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

        if force_rt_lookup || dirty_flags[crate::dirty_flags::flags::ZETA_BUFFER as usize] {
            let zeta = render_targets.zeta;
            if zeta.enabled && zeta.address != 0 && zeta.width != 0 && zeta.height != 0 {
                let info = ImageInfo::from_zeta_info(&zeta, render_targets.anti_alias_samples_mode);
                let guest_size = super::util::calculate_guest_size_in_bytes(&info) as u64;
                let cpu_addr = gpu_to_cpu(zeta.address, guest_size).unwrap_or_else(|| {
                    if std::env::var_os("RUZU_TRACE_RT").is_some() {
                        log::info!(
                            "[RT] miss translate zeta gpu=0x{:X} {}x{} fmt=0x{:X}; using virtual-invalid fallback",
                            zeta.address,
                            zeta.width,
                            zeta.height,
                            zeta.format
                        );
                    }
                    self.resolve_or_allocate_cpu_addr(zeta.address, guest_size)
                });
                let image_id = self.find_or_insert_image_from_info(&info, zeta.address, cpu_addr);
                let view_id = self.find_image_view_from_image_info(image_id, &info, zeta.address);
                self.bind_depth_render_target(view_id);
            } else {
                self.bind_depth_render_target(ImageViewId::default());
            }
        }
    }

    fn queue_preemptive_render_target_download(&mut self, new_id: ImageViewId) {
        if !new_id.is_valid() || new_id == NULL_IMAGE_VIEW_ID {
            return;
        }
        let new_view = &self.slot_image_views[new_id];
        if new_view
            .flags
            .contains(ImageViewFlagBits::PREEMTIVE_DOWNLOAD)
        {
            self.uncommitted_downloads.push(PendingDownload {
                is_swizzle: true,
                async_buffer_id: 0,
                object_id: new_view.image_id,
            });
        }
    }

    fn bind_color_render_target(&mut self, index: usize, new_id: ImageViewId) {
        if self.render_targets.color_buffer_ids[index] == new_id {
            return;
        }
        self.queue_preemptive_render_target_download(new_id);
        self.render_targets.color_buffer_ids[index] = new_id;
    }

    fn bind_depth_render_target(&mut self, new_id: ImageViewId) {
        if self.render_targets.depth_buffer_id == new_id {
            return;
        }
        self.queue_preemptive_render_target_download(new_id);
        self.render_targets.depth_buffer_id = new_id;
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
        assert!(
            !self.backend_completes_join_images,
            "TextureCacheBase::find_or_insert_image_from_info cannot return a fully-completed ImageId when backend completion is required"
        );
        self.find_or_insert_image_from_info_with_options_result(
            info,
            gpu_addr,
            cpu_addr,
            RelaxedOptions::empty(),
        )
        .image_id
    }

    /// CPU-address-aware counterpart of upstream
    /// `TextureCache<P>::FindOrInsertImage(info, gpu_addr, options)`.
    pub fn find_or_insert_image_from_info_with_options(
        &mut self,
        info: &ImageInfo,
        gpu_addr: GPUVAddr,
        cpu_addr: u64,
        options: RelaxedOptions,
    ) -> ImageId {
        assert!(
            !self.backend_completes_join_images,
            "TextureCacheBase::find_or_insert_image_from_info_with_options cannot return a fully-completed ImageId when backend completion is required"
        );
        self.find_or_insert_image_from_info_with_options_result(info, gpu_addr, cpu_addr, options)
            .image_id
    }

    pub(crate) fn find_or_insert_image_from_info_with_options_result(
        &mut self,
        info: &ImageInfo,
        gpu_addr: GPUVAddr,
        cpu_addr: u64,
        options: RelaxedOptions,
    ) -> FindOrInsertImageResult {
        let trace_addr = should_trace_texture_cache_addr(gpu_addr);
        if let Some(image_id) = self.find_image_in_cpu_region_with_caps(
            info,
            gpu_addr,
            cpu_addr,
            options,
            self.has_broken_texture_view_formats,
            self.has_native_bgr,
        ) {
            if trace_addr {
                use std::sync::atomic::{AtomicU64, Ordering};
                static REGION_HITS: AtomicU64 = AtomicU64::new(0);
                let hit = REGION_HITS.fetch_add(1, Ordering::Relaxed);
                if hit < 32 || hit.is_power_of_two() {
                    let image = &self.slot_images[image_id];
                    log::warn!(
                        "[TEX_CACHE] region_hit #{} id={} gpu=0x{:X} cpu=0x{:X} req_fmt={:?} img_fmt={:?} req={}x{} img={}x{} guest=0x{:X} flags=0x{:X} tick={}",
                        hit,
                        image_id.index,
                        gpu_addr,
                        cpu_addr,
                        info.format,
                        image.info.format,
                        info.size.width,
                        info.size.height,
                        image.info.size.width,
                        image.info.size.height,
                        image.guest_size_bytes,
                        image.flags.bits(),
                        image.modification_tick,
                    );
                }
            }
            return FindOrInsertImageResult {
                image_id,
                inserted: false,
                needs_backend_completion: false,
                queued_join_tail: false,
            };
        }
        if trace_addr {
            log::warn!(
                "[TEX_CACHE] miss_join gpu=0x{:X} cpu=0x{:X} fmt={:?} {}x{}",
                gpu_addr,
                cpu_addr,
                info.format,
                info.size.width,
                info.size.height
            );
        }
        let pending_join_count = self.pending_join_copies.len();
        let image_id = self.join_images(info, gpu_addr, cpu_addr);
        let queued_join_tail = self.pending_join_copies.len() != pending_join_count;
        if !self.backend_completes_join_images {
            self.register_image_alloc(image_id);
        }
        FindOrInsertImageResult {
            image_id,
            inserted: true,
            needs_backend_completion: self.backend_completes_join_images,
            queued_join_tail,
        }
    }

    pub(crate) fn register_image_alloc(&mut self, image_id: ImageId) {
        if !image_id.is_valid() {
            return;
        }
        let gpu_addr = self.slot_images[image_id].gpu_addr;
        let alloc_id = if let Some(&alloc_id) = self.image_allocs_table.get(&gpu_addr) {
            alloc_id
        } else {
            let alloc_id = self.slot_image_allocs.insert(ImageAllocBase::default());
            self.image_allocs_table.insert(gpu_addr, alloc_id);
            alloc_id
        };
        let alloc_images = &mut self.slot_image_allocs[alloc_id].images;
        if !alloc_images.contains(&image_id) {
            alloc_images.push(image_id);
        }
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
            image
                .try_find_base(gpu_addr)
                .expect("TextureCache::FindRenderTargetView TryFindBase failed")
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
        self.find_or_emplace_image_view(image_id, info, gpu_addr)
    }

    /// Port of `TextureCache<P>::FindOrEmplaceImageView`.
    pub fn find_or_emplace_image_view(
        &mut self,
        image_id: ImageId,
        info: ImageViewInfo,
        gpu_addr: GPUVAddr,
    ) -> ImageViewId {
        let existing = self.slot_images[image_id].find_view(&info);
        if existing.is_valid() {
            return existing;
        }

        let image_info = self.slot_images[image_id].info.clone();
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
        info: &ImageInfo,
        mut gpu_addr: GPUVAddr,
        mut cpu_addr: u64,
    ) -> ImageId {
        let trace_addr = should_trace_texture_cache_addr(gpu_addr);
        let mut new_info = info.clone();
        let size_bytes = super::util::calculate_guest_size_in_bytes(&new_info) as usize;
        let broken_views = self.has_broken_texture_view_formats;
        let native_bgr = self.has_native_bgr;

        self.join_overlap_ids.clear();
        self.join_overlaps_found.clear();
        self.join_left_aliased_ids.clear();
        self.join_right_aliased_ids.clear();
        self.join_ignore_textures.clear();
        self.join_bad_overlap_ids.clear();
        self.join_copies_to_do.clear();
        self.join_alias_indices.clear();

        let this_is_linear = info.image_type == ImageType::Linear;
        let overlaps = self.collect_images_in_region(cpu_addr, size_bytes);
        if trace_addr {
            log::warn!(
                "[TEX_CACHE] join_begin gpu=0x{:X} cpu=0x{:X} size=0x{:X} fmt={:?} {}x{} overlaps={:?}",
                gpu_addr,
                cpu_addr,
                size_bytes,
                info.format,
                info.size.width,
                info.size.height,
                overlaps.iter().map(|id| id.index).collect::<Vec<_>>()
            );
        }
        for overlap_id in overlaps {
            if !overlap_id.is_valid() {
                continue;
            }
            let overlap_snapshot = self.slot_images[overlap_id].clone();
            if trace_addr || should_trace_texture_cache_addr(overlap_snapshot.gpu_addr) {
                log::warn!(
                    "[TEX_CACHE] join_overlap new_gpu=0x{:X} overlap={} gpu=0x{:X} cpu=0x{:X} fmt={:?} {}x{} flags=0x{:X}",
                    gpu_addr,
                    overlap_id.index,
                    overlap_snapshot.gpu_addr,
                    overlap_snapshot.cpu_addr,
                    overlap_snapshot.info.format,
                    overlap_snapshot.info.size.width,
                    overlap_snapshot.info.size.height,
                    overlap_snapshot.flags.bits()
                );
            }
            if overlap_snapshot.flags.contains(ImageFlagBits::REMAPPED) {
                self.join_ignore_textures.insert(overlap_id);
                continue;
            }
            let overlap_is_linear = overlap_snapshot.info.image_type == ImageType::Linear;
            if this_is_linear != overlap_is_linear {
                continue;
            }
            if this_is_linear && overlap_is_linear {
                if info.pitch() == overlap_snapshot.info.pitch()
                    && gpu_addr == overlap_snapshot.gpu_addr
                {
                    self.join_left_aliased_ids.push(overlap_id);
                }
                continue;
            }

            self.join_overlaps_found.insert(overlap_id);
            if let Some(solution) = super::util::resolve_overlap(
                &new_info,
                gpu_addr,
                cpu_addr,
                &overlap_snapshot,
                true,
                broken_views,
                native_bgr,
            ) {
                gpu_addr = solution.gpu_addr;
                cpu_addr = solution.cpu_addr;
                new_info.resources = solution.resources;
                self.join_overlap_ids.push(overlap_id);
                self.join_copies_to_do.push(JoinCopy {
                    is_alias: false,
                    id: overlap_id,
                    gpu_modified_at_join: overlap_snapshot
                        .flags
                        .contains(ImageFlagBits::GPU_MODIFIED),
                });
                continue;
            }

            let options = RelaxedOptions::SIZE | RelaxedOptions::FORMAT;
            let new_image_base = ImageBase::new(new_info.clone(), gpu_addr, cpu_addr);
            if super::util::is_subresource(
                &new_info,
                &overlap_snapshot,
                gpu_addr,
                options,
                broken_views,
                native_bgr,
            ) {
                self.join_left_aliased_ids.push(overlap_id);
                self.slot_images[overlap_id]
                    .flags
                    .insert(ImageFlagBits::ALIAS);
                self.join_copies_to_do.push(JoinCopy {
                    is_alias: true,
                    id: overlap_id,
                    gpu_modified_at_join: overlap_snapshot
                        .flags
                        .contains(ImageFlagBits::GPU_MODIFIED),
                });
            } else if super::util::is_subresource(
                &overlap_snapshot.info,
                &new_image_base,
                overlap_snapshot.gpu_addr,
                options,
                broken_views,
                native_bgr,
            ) {
                self.join_right_aliased_ids.push(overlap_id);
                self.slot_images[overlap_id]
                    .flags
                    .insert(ImageFlagBits::ALIAS);
                self.join_copies_to_do.push(JoinCopy {
                    is_alias: true,
                    id: overlap_id,
                    gpu_modified_at_join: overlap_snapshot
                        .flags
                        .contains(ImageFlagBits::GPU_MODIFIED),
                });
            } else {
                self.join_bad_overlap_ids.push(overlap_id);
            }
        }
        for overlap_id in self.collect_images_in_gpu_region(gpu_addr, size_bytes, true) {
            if self.join_overlaps_found.contains(&overlap_id) {
                continue;
            }
            let overlap = &self.slot_images[overlap_id];
            if overlap.flags.contains(ImageFlagBits::REMAPPED)
                || (overlap.gpu_addr == gpu_addr && overlap.guest_size_bytes as usize == size_bytes)
            {
                self.join_ignore_textures.insert(overlap_id);
            }
        }

        let new_image_id =
            self.slot_images
                .insert(ImageBase::new(new_info.clone(), gpu_addr, cpu_addr));
        if new_info.is_sparse {
            let gpu_memory = self
                .channel_gpu_memory
                .as_ref()
                .expect("TextureCache::join_images sparse image requires channel GPU memory")
                .lock();
            if !gpu_memory.is_continuous_range(gpu_addr, size_bytes as u64) {
                self.slot_images[new_image_id]
                    .flags
                    .insert(ImageFlagBits::SPARSE);
            }
        }

        for overlap_id in self.join_ignore_textures.clone() {
            if !overlap_id.is_valid() || overlap_id == NULL_IMAGE_ID {
                continue;
            }
            if self.slot_images[overlap_id]
                .flags
                .contains(ImageFlagBits::GPU_MODIFIED)
            {
                self.record_unimplemented_join_images_path(
                    "ignored overlap is GPU_MODIFIED",
                    info,
                    gpu_addr,
                    cpu_addr,
                    overlap_id,
                );
                panic!(
                    "TextureCache::JoinImages ignored GPU-modified overlap is unimplemented: overlap_id={} gpu=0x{:X}",
                    overlap_id.index, self.slot_images[overlap_id].gpu_addr
                );
            }
            if self.slot_images[overlap_id]
                .flags
                .contains(ImageFlagBits::TRACKED)
            {
                self.untrack_image(overlap_id);
            }
            if self.slot_images[overlap_id]
                .flags
                .contains(ImageFlagBits::REGISTERED)
            {
                self.unregister_image(overlap_id);
            }
            self.delete_image(overlap_id, false);
        }

        let has_pending_join_tail = !self.join_copies_to_do.is_empty();
        if has_pending_join_tail {
            self.join_copies_to_do
                .sort_by_key(|copy| self.slot_images[copy.id].modification_tick);
            self.pending_join_copies.push(PendingJoinCopies {
                new_image_id,
                copies: self.join_copies_to_do.clone(),
                left_aliased_ids: self.join_left_aliased_ids.clone(),
                right_aliased_ids: self.join_right_aliased_ids.clone(),
                bad_overlap_ids: self.join_bad_overlap_ids.clone(),
                alias_indices: std::collections::HashMap::new(),
                alias_relations_applied: false,
            });
        } else {
            self.join_alias_indices = self.apply_join_relations(
                new_image_id,
                &self.join_right_aliased_ids.clone(),
                &self.join_left_aliased_ids.clone(),
                &self.join_bad_overlap_ids.clone(),
            );
        }

        // Upstream now refreshes contents, creates backend copies from GPU-modified overlaps,
        // then unregisters and deletes superseded images. The base cache has no backend
        // `Runtime::CopyImage`, so it queues the copy/delete tail for the backend wrapper.
        if !has_pending_join_tail && !self.backend_completes_join_images {
            self.register_image(new_image_id);
        }
        if trace_addr {
            log::warn!(
                "[TEX_CACHE] join_end id={} gpu=0x{:X} cpu=0x{:X} fmt={:?} {}x{} copies={} left_alias={} right_alias={} bad={}",
                new_image_id.index,
                self.slot_images[new_image_id].gpu_addr,
                self.slot_images[new_image_id].cpu_addr,
                self.slot_images[new_image_id].info.format,
                self.slot_images[new_image_id].info.size.width,
                self.slot_images[new_image_id].info.size.height,
                self.join_copies_to_do.len(),
                self.join_left_aliased_ids.len(),
                self.join_right_aliased_ids.len(),
                self.join_bad_overlap_ids.len()
            );
        }
        new_image_id
    }

    fn record_unimplemented_join_images_path(
        &self,
        reason: &str,
        info: &ImageInfo,
        new_gpu_addr: GPUVAddr,
        new_cpu_addr: u64,
        overlap_id: ImageId,
    ) {
        #[cfg(not(test))]
        {
            let overlap = &self.slot_images[overlap_id];
            let path = std::path::Path::new(".agents/texture_cache_unimplemented_state.md");
            if let Some(parent) = path.parent() {
                let _ = std::fs::create_dir_all(parent);
            }
            let entry = format!(
                "\n## TextureCache::JoinImages unsupported path\n\
                 - reason: {}\n\
                 - upstream: video_core/texture_cache/texture_cache.h TextureCache<P>::JoinImages ignored-overlap loop calls UNIMPLEMENTED() when ImageFlagBits::GpuModified is set\n\
                 - new_gpu_addr: 0x{:X}\n\
                 - new_cpu_addr: 0x{:X}\n\
                 - new_format: {:?}\n\
                 - new_size: {}x{}x{}\n\
                 - new_guest_size: {}\n\
                 - overlap_id: {}\n\
                 - overlap_gpu_addr: 0x{:X}\n\
                 - overlap_cpu_addr: 0x{:X}\n\
                 - overlap_format: {:?}\n\
                 - overlap_size: {}x{}x{}\n\
                 - overlap_guest_size: {}\n\
                 - overlap_flags: 0x{:X}\n",
                reason,
                new_gpu_addr,
                new_cpu_addr,
                info.format,
                info.size.width,
                info.size.height,
                info.size.depth,
                super::util::calculate_guest_size_in_bytes(info),
                overlap_id.index,
                overlap.gpu_addr,
                overlap.cpu_addr,
                overlap.info.format,
                overlap.info.size.width,
                overlap.info.size.height,
                overlap.info.size.depth,
                overlap.guest_size_bytes,
                overlap.flags.bits(),
            );
            let _ = std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(path)
                .and_then(|mut file| {
                    use std::io::Write;
                    file.write_all(entry.as_bytes())
                });
        }
    }

    pub(crate) fn apply_join_relations(
        &mut self,
        new_image_id: ImageId,
        right_aliased_ids: &[ImageId],
        left_aliased_ids: &[ImageId],
        bad_overlap_ids: &[ImageId],
    ) -> std::collections::HashMap<ImageId, usize> {
        let mut alias_indices = std::collections::HashMap::new();
        let mut bad_overlap_ids = bad_overlap_ids.to_vec();
        let mut new_image = self.slot_images[new_image_id].clone();
        for &aliased_id in right_aliased_ids {
            let alias_index = new_image.aliased_images.len();
            let mut aliased = self.slot_images[aliased_id].clone();
            if !Self::can_add_image_alias(&new_image, &aliased) {
                if !bad_overlap_ids.contains(&aliased_id) {
                    bad_overlap_ids.push(aliased_id);
                }
                continue;
            }
            if !add_image_alias(&mut new_image, &mut aliased, new_image_id, aliased_id) {
                continue;
            }
            alias_indices.insert(aliased_id, alias_index);
            new_image.flags.insert(ImageFlagBits::ALIAS);
            self.slot_images[aliased_id] = aliased;
        }
        for &aliased_id in left_aliased_ids {
            let alias_index = new_image.aliased_images.len();
            let mut aliased = self.slot_images[aliased_id].clone();
            if !Self::can_add_image_alias(&aliased, &new_image) {
                if !bad_overlap_ids.contains(&aliased_id) {
                    bad_overlap_ids.push(aliased_id);
                }
                continue;
            }
            if !add_image_alias(&mut aliased, &mut new_image, aliased_id, new_image_id) {
                continue;
            }
            alias_indices.insert(aliased_id, alias_index);
            new_image.flags.insert(ImageFlagBits::ALIAS);
            self.slot_images[aliased_id] = aliased;
        }
        self.slot_images[new_image_id] = new_image;

        for aliased_id in bad_overlap_ids {
            self.slot_images[aliased_id]
                .overlapping_images
                .push(new_image_id);
            self.slot_images[new_image_id]
                .overlapping_images
                .push(aliased_id);
            let aliased_bad = {
                let aliased = &self.slot_images[aliased_id];
                aliased.info.resources.levels == 1
                    && aliased.info.block().depth == 0
                    && aliased.overlapping_images.len() > 1
            };
            if aliased_bad {
                self.slot_images[aliased_id]
                    .flags
                    .insert(ImageFlagBits::BAD_OVERLAP);
            }
            let new_bad = {
                let image = &self.slot_images[new_image_id];
                image.info.resources.levels == 1
                    && image.info.block().depth == 0
                    && image.overlapping_images.len() > 1
            };
            if new_bad {
                self.slot_images[new_image_id]
                    .flags
                    .insert(ImageFlagBits::BAD_OVERLAP);
            }
        }

        alias_indices
    }

    // ── Registration / tracking ────────────────────────────────────────

    /// Port of `TextureCache<P>::RegisterImage`.
    ///
    /// Inserts the image into page tables and marks it for CPU write-tracking.
    pub fn register_image(&mut self, image_id: ImageId) {
        debug_assert!(
            !self.slot_images[image_id]
                .flags
                .contains(ImageFlagBits::REGISTERED),
            "TextureCache::register_image: image already registered"
        );
        let memory_size = Self::registered_image_memory_size(&self.slot_images[image_id]);
        self.total_used_memory = self.total_used_memory.saturating_add(memory_size);
        let lru_index = self
            .lru_cache
            .insert(image_id, self.frame_tick.min(i64::MAX as u64) as i64);
        self.slot_images[image_id].lru_index = lru_index;

        let (gpu_addr, guest_size_bytes, is_sparse) = {
            let image = &self.slot_images[image_id];
            (
                image.gpu_addr,
                image.guest_size_bytes as usize,
                image.flags.contains(ImageFlagBits::SPARSE),
            )
        };
        if let Some(table_index) = self.current_gpu_page_table_index(false) {
            let table = &mut self.gpu_page_table_storage[table_index];
            Self::for_each_gpu_page(gpu_addr, guest_size_bytes, |page| {
                let entries = table.entry(page).or_default();
                if !entries.contains(&image_id) {
                    entries.push(image_id);
                }
            });
        }
        if is_sparse {
            if let Some(table_index) = self.current_gpu_page_table_index(true) {
                let table = &mut self.gpu_page_table_storage[table_index];
                Self::for_each_gpu_page(gpu_addr, guest_size_bytes, |page| {
                    let entries = table.entry(page).or_default();
                    if !entries.contains(&image_id) {
                        entries.push(image_id);
                    }
                });
            }
        }

        if is_sparse {
            let segments = self.sparse_segments_for_image(image_id, "TextureCache::register_image");
            let mut sparse_maps = Vec::new();
            for (segment_gpu_addr, cpu_addr, segment_size) in segments {
                let map_id = self.slot_map_views.insert(ImageMapView::new(
                    segment_gpu_addr,
                    cpu_addr,
                    segment_size,
                    image_id,
                ));
                Self::for_each_cpu_page(cpu_addr, segment_size, |page| {
                    let entries = self.page_table.entry(page).or_default();
                    if !entries.contains(&map_id) {
                        entries.push(map_id);
                    }
                });
                sparse_maps.push(map_id);
            }
            self.sparse_views.insert(image_id, sparse_maps);
            self.slot_images[image_id]
                .flags
                .insert(ImageFlagBits::REGISTERED);
            return;
        }

        let image = &self.slot_images[image_id];
        let map_id = self.slot_map_views.insert(ImageMapView::new(
            image.gpu_addr,
            image.cpu_addr,
            image.guest_size_bytes as usize,
            image_id,
        ));
        self.slot_images[image_id].map_view_id = map_id;
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

    /// Port of `lru_cache.Touch(image.lru_index, frame_tick)` in
    /// `TextureCache<P>::PrepareImage`.
    pub fn touch_image(&mut self, image_id: ImageId) {
        if !image_id.is_valid() || image_id == NULL_IMAGE_ID {
            return;
        }
        let lru_index = self.slot_images[image_id].lru_index;
        if lru_index != usize::MAX {
            self.lru_cache
                .touch(lru_index, self.frame_tick.min(i64::MAX as u64) as i64);
        }
    }

    /// Port of `TextureCache<P>::UnregisterImage`.
    ///
    /// Removes the image from CPU page tables and clears registration state.
    pub fn unregister_image(&mut self, image_id: ImageId) {
        let (is_sparse, gpu_addr, guest_size_bytes, map_view_id) = {
            let image = &self.slot_images[image_id];
            debug_assert!(
                image.flags.contains(ImageFlagBits::REGISTERED),
                "TextureCache::unregister_image: image not registered"
            );
            (
                image.flags.contains(ImageFlagBits::SPARSE),
                image.gpu_addr,
                image.guest_size_bytes as usize,
                image.map_view_id,
            )
        };
        if let Some(table_index) = self.current_gpu_page_table_index(false) {
            let table = &mut self.gpu_page_table_storage[table_index];
            Self::for_each_gpu_page(gpu_addr, guest_size_bytes, |page| {
                if let Some(image_ids) = table.get_mut(&page) {
                    image_ids.retain(|&id| id != image_id);
                    if image_ids.is_empty() {
                        table.remove(&page);
                    }
                }
            });
        }
        if is_sparse {
            if let Some(table_index) = self.current_gpu_page_table_index(true) {
                let table = &mut self.gpu_page_table_storage[table_index];
                Self::for_each_gpu_page(gpu_addr, guest_size_bytes, |page| {
                    if let Some(image_ids) = table.get_mut(&page) {
                        image_ids.retain(|&id| id != image_id);
                        if image_ids.is_empty() {
                            table.remove(&page);
                        }
                    }
                });
            }
        }
        let map_ids = if is_sparse {
            self.sparse_views
                .remove(&image_id)
                .unwrap_or_default()
                .into_iter()
                .collect::<Vec<_>>()
        } else {
            if map_view_id.is_valid() {
                vec![map_view_id]
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
        if !is_sparse {
            image.map_view_id = ImageMapId::default();
        }
        if image.lru_index != usize::MAX {
            self.lru_cache.free(image.lru_index);
            image.lru_index = usize::MAX;
        }
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
        if registered {
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
            return;
        }
        for (_, cpu_addr, size) in
            self.sparse_segments_for_image(image_id, "TextureCache::track_image")
        {
            self.device_memory
                .update_pages_cached_count(cpu_addr, size, 1);
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
    pub fn delete_image(&mut self, image_id: ImageId, immediate_delete: bool) {
        self.delete_image_impl(image_id, immediate_delete, true);
    }

    pub(crate) fn delete_image_after_backend_cleanup(
        &mut self,
        image_id: ImageId,
        immediate_delete: bool,
    ) {
        self.delete_image_impl(image_id, immediate_delete, false);
    }

    fn delete_image_impl(
        &mut self,
        image_id: ImageId,
        immediate_delete: bool,
        queue_backend_deletion: bool,
    ) {
        let image_view_ids = self.slot_images[image_id].image_view_ids.clone();
        let aliased_images = self.slot_images[image_id].aliased_images.clone();
        let overlapping_images = self.slot_images[image_id].overlapping_images.clone();
        let gpu_addr = self.slot_images[image_id].gpu_addr;
        let registered_size = Self::registered_image_memory_size(&self.slot_images[image_id]);
        let scaled_size = if self.slot_images[image_id].has_scaled {
            Self::scaled_image_memory_size(&self.slot_images[image_id])
        } else {
            0
        };

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

        if queue_backend_deletion && self.backend_completes_join_images {
            self.pending_backend_deletions.push(PendingBackendDeletion {
                image_id,
                image_view_ids: image_view_ids.clone(),
            });
        }

        self.mark_render_targets_dirty();
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
        self.remove_image_view_references(&image_view_ids);
        self.remove_framebuffers(&image_view_ids);

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

        for image_view_id in image_view_ids {
            if image_view_id != NULL_IMAGE_VIEW_ID && image_view_id.is_valid() {
                if immediate_delete {
                    self.slot_image_views.erase(image_view_id);
                } else {
                    let image_view = self.slot_image_views.take(image_view_id);
                    self.sentenced_image_view.push(image_view);
                }
            }
        }

        if immediate_delete {
            self.slot_images.erase(image_id);
        } else {
            let image = self.slot_images.take(image_id);
            self.sentenced_images.push(image);
        }
        self.total_used_memory = self
            .total_used_memory
            .saturating_sub(registered_size.saturating_add(scaled_size));

        if let Some(alloc_id) = self.image_allocs_table.get(&gpu_addr).copied() {
            let alloc_images = &mut self.slot_image_allocs[alloc_id].images;
            alloc_images.retain(|&id| id != image_id);
            if alloc_images.is_empty() {
                self.slot_image_allocs.erase(alloc_id);
                self.image_allocs_table.remove(&gpu_addr);
            }
        }

        self.invalidate_channel_image_views();
        self.has_deleted_images = true;
    }

    pub(crate) fn invalidate_channel_image_views(&mut self) {
        self.for_each_active_channel_state_mut(|channel| {
            channel.graphics_image_table.invalidate();
            channel.compute_image_table.invalidate();
            channel.graphics_image_view_ids.fill(CORRUPT_ID);
            channel.compute_image_view_ids.fill(CORRUPT_ID);
        });
    }

    /// Port of `TextureCache<P>::RemoveImageViewReferences`.
    pub(crate) fn remove_image_view_references(&mut self, removed_views: &[ImageViewId]) {
        self.for_each_active_channel_state_mut(|channel| {
            channel
                .image_views
                .retain(|_, id| !removed_views.contains(id));
        });
    }

    /// Port of `TextureCache<P>::RemoveFramebuffers`.
    fn remove_framebuffers(&mut self, removed_views: &[ImageViewId]) {
        self.framebuffers
            .retain(|key, _| !key.contains(removed_views));
    }

    // ── Blit ───────────────────────────────────────────────────────────

    /// Port of `TextureCache<P>::BlitImage`.
    ///
    /// Resolves source/destination image pairs, handles rescaling, and
    /// delegates to the backend blit/resolve operations.  Returns false until
    /// Fermi2D engine types and backend runtime are available.
    pub fn blit_image(&mut self, _dst: &(), _src: &(), _copy: &()) -> bool {
        panic!(
            "TextureCacheBase::blit_image cannot perform upstream Fermi2D image preparation or backend blit without a backend runtime; use the backend texture-cache wrapper"
        );
    }

    // ── Maxwell DMA image/buffer copies ────────────────────────────────

    /// Port of `TextureCache<P>::DmaImageId`.
    pub fn dma_image_id(&mut self, operand: &dma::ImageOperand, is_upload: bool) -> ImageId {
        let dst_info = ImageInfo::from_dma_operand(operand);
        let dst_id = self.find_dma_image(&dst_info, operand.address);
        if !dst_id.is_valid() {
            return NULL_IMAGE_ID;
        }

        let image = &mut self.slot_images[dst_id];
        if !image.flags.contains(ImageFlagBits::GPU_MODIFIED) {
            return NULL_IMAGE_ID;
        }
        if image.info.image_type == ImageType::E3D {
            return NULL_IMAGE_ID;
        }
        if !is_upload && !image.info.dma_downloaded {
            image.info.dma_downloaded = true;
            return NULL_IMAGE_ID;
        }
        if image.try_find_base(operand.address).is_none() {
            return NULL_IMAGE_ID;
        }
        dst_id
    }

    /// Port of `TextureCache<P>::FindDMAImage`.
    pub fn find_dma_image(&self, info: &ImageInfo, gpu_addr: GPUVAddr) -> ImageId {
        let size = super::util::calculate_guest_size_in_bytes(info) as u64;
        let Some(cpu_addr) = self.translated_cpu_addr(gpu_addr, size) else {
            return NULL_IMAGE_ID;
        };

        let mut image_id = NULL_IMAGE_ID;
        let mut image_ids = Vec::new();
        for existing_image_id in self.collect_images_in_region(cpu_addr, size as usize) {
            let existing_image = &self.slot_images[existing_image_id];
            if existing_image.flags.contains(ImageFlagBits::REMAPPED) {
                continue;
            }

            let matched = if info.image_type == ImageType::Linear
                || existing_image.info.image_type == ImageType::Linear
            {
                let strict_size = existing_image.flags.contains(ImageFlagBits::STRONG);
                let existing = &existing_image.info;
                existing_image.gpu_addr == gpu_addr
                    && existing.image_type == info.image_type
                    && existing.pitch() == info.pitch()
                    && super::util::is_pitch_linear_same_size(existing, info, strict_size)
                    && surface::is_view_compatible(existing.format, info.format, false, true)
            } else {
                super::util::is_sub_copy(info, existing_image, gpu_addr)
            };

            if matched {
                image_id = existing_image_id;
                image_ids.push(existing_image_id);
            }
        }

        if image_ids.len() <= 1 {
            return image_id;
        }
        image_ids
            .into_iter()
            .max_by_key(|&id| self.slot_images[id].modification_tick)
            .unwrap_or(NULL_IMAGE_ID)
    }

    /// Backend-independent copy descriptor half of
    /// `TextureCache<P>::DmaBufferImageCopy`.
    ///
    /// Upstream also calls `PrepareDmaImage` here; the Rust base cache cannot
    /// invoke backend `PrepareImage`, so the OpenGL wrapper performs that
    /// preparation and then uses this exact descriptor construction.
    pub fn dma_buffer_image_copy_descriptor(
        &self,
        copy_info: &dma::ImageCopy,
        buffer_operand: &dma::BufferOperand,
        image_operand: &dma::ImageOperand,
        image_id: ImageId,
    ) -> Option<DmaBufferImageCopyResult> {
        if !image_id.is_valid() {
            return None;
        }
        let image = &self.slot_images[image_id];
        let base = image.try_find_base(image_operand.address)?;
        let buffer_size = buffer_operand.pitch.saturating_mul(buffer_operand.height);
        let bpp = surface::bytes_per_block(image.info.format);
        let convert = |value: u32| (image_operand.bytes_per_pixel.saturating_mul(value)) / bpp;
        let copy = BufferImageCopy {
            buffer_offset: 0,
            buffer_size: buffer_size as usize,
            buffer_row_length: convert(buffer_operand.pitch),
            buffer_image_height: buffer_operand.height,
            image_subresource: SubresourceLayers {
                base_level: base.level,
                base_layer: base.layer,
                num_layers: 1,
            },
            image_offset: Offset3D {
                x: convert(image_operand.params.origin.x()) as i32,
                y: image_operand.params.origin.y() as i32,
                z: 0,
            },
            image_extent: Extent3D {
                width: convert(copy_info.length_x),
                height: copy_info.length_y,
                depth: 1,
            },
        };
        Some(DmaBufferImageCopyResult { image_id, copy })
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
        panic!(
            "TextureCacheBase::prepare_image cannot preserve upstream RefreshContents/SynchronizeAliases ordering without a backend runtime; use the backend texture-cache wrapper"
        );
    }

    /// Port of `TextureCache<P>::RefreshContents`.
    ///
    /// Re-uploads guest texture data for a CPU-modified image.
    fn refresh_contents(&mut self, _image_id: ImageId) {
        panic!(
            "TextureCacheBase::refresh_contents cannot upload guest texture data without a backend runtime; use the backend texture-cache wrapper"
        );
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
        image.modification_tick = self.modification_tick;
    }

    // ── GPU memory queries ─────────────────────────────────────────────

    /// Port of `TextureCache<P>::IsRegionGpuModified`.
    ///
    /// Returns true if any image overlapping the given CPU address range has
    /// been modified from the GPU and not yet downloaded to guest memory.
    pub fn is_region_gpu_modified(&self, addr: u64, size: usize) -> bool {
        self.collect_images_in_region(addr, size)
            .into_iter()
            .any(|image_id| {
                self.slot_images[image_id]
                    .flags
                    .contains(ImageFlagBits::GPU_MODIFIED)
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engines::maxwell_3d::{RenderTargetInfo, RtControlInfo};
    use crate::framebuffer_config::{AndroidPixelFormat, FramebufferConfig};
    use crate::textures::texture::{ComponentType, TextureFormat, TextureType, TicEntry, TscEntry};

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

    fn descriptor_bytes(raw: [u64; 4]) -> Vec<u8> {
        raw.into_iter()
            .flat_map(u64::to_le_bytes)
            .collect::<Vec<_>>()
    }

    fn test_cache() -> TextureCacheBase {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;
        TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()))
    }

    fn test_color_info(width: u32, height: u32) -> ImageInfo {
        ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            size: crate::texture_cache::types::Extent3D {
                width,
                height,
                depth: 1,
            },
            ..ImageInfo::default()
        }
    }

    fn dma_operand(
        address: u64,
        width: u32,
        height: u32,
        bytes_per_pixel: u32,
    ) -> dma::ImageOperand {
        dma::ImageOperand {
            bytes_per_pixel,
            params: dma::Parameters {
                width,
                height,
                depth: 1,
                ..dma::Parameters::default()
            },
            address,
        }
    }

    #[test]
    fn dma_image_id_requires_gpu_modified_image() {
        let mut cache = test_cache();
        let operand = dma_operand(0x5000_0000, 64, 64, 4);
        let info = ImageInfo::from_dma_operand(&operand);
        let image_id = cache.insert_image(&info, operand.address);

        assert_eq!(cache.dma_image_id(&operand, true), NULL_IMAGE_ID);

        cache.mark_modification_by_id(image_id);

        assert_eq!(cache.dma_image_id(&operand, true), image_id);
    }

    #[test]
    fn dma_image_id_download_first_marks_dma_downloaded_and_returns_null() {
        let mut cache = test_cache();
        let operand = dma_operand(0x5100_0000, 64, 64, 4);
        let info = ImageInfo::from_dma_operand(&operand);
        let image_id = cache.insert_image(&info, operand.address);
        cache.mark_modification_by_id(image_id);

        assert!(!cache.slot_images[image_id].info.dma_downloaded);
        assert_eq!(cache.dma_image_id(&operand, false), NULL_IMAGE_ID);
        assert!(cache.slot_images[image_id].info.dma_downloaded);
        assert_eq!(cache.dma_image_id(&operand, false), image_id);
    }

    #[test]
    fn dma_image_id_refuses_3d_images() {
        let mut cache = test_cache();
        let mut operand = dma_operand(0x5200_0000, 32, 32, 4);
        operand.params.block_size.raw = 1 << 8;
        operand.params.depth = 4;
        let info = ImageInfo::from_dma_operand(&operand);
        let image_id = cache.insert_image(&info, operand.address);
        cache.mark_modification_by_id(image_id);

        assert_eq!(cache.dma_image_id(&operand, true), NULL_IMAGE_ID);
    }

    #[test]
    fn dma_buffer_image_copy_descriptor_matches_upstream_fields() {
        let mut cache = test_cache();
        let info = test_color_info(64, 64);
        let image_id = cache
            .slot_images
            .insert(crate::texture_cache::image_base::ImageBase::new(
                info,
                0x5300_0000,
                0x9300_0000,
            ));
        let image_operand = dma::ImageOperand {
            bytes_per_pixel: 8,
            params: dma::Parameters {
                origin: dma::Origin { raw: 3 | (7 << 16) },
                ..dma::Parameters::default()
            },
            address: 0x5300_0000,
        };
        let buffer_operand = dma::BufferOperand {
            pitch: 64,
            height: 9,
            address: 0x6000_0000,
            ..dma::BufferOperand::default()
        };
        let copy_info = dma::ImageCopy {
            length_x: 5,
            length_y: 6,
        };

        let result = cache
            .dma_buffer_image_copy_descriptor(&copy_info, &buffer_operand, &image_operand, image_id)
            .expect("valid descriptor");

        assert_eq!(result.image_id, image_id);
        assert_eq!(result.copy.buffer_offset, 0);
        assert_eq!(result.copy.buffer_size, 64 * 9);
        assert_eq!(result.copy.buffer_row_length, 128);
        assert_eq!(result.copy.buffer_image_height, 9);
        assert_eq!(result.copy.image_subresource.base_level, 0);
        assert_eq!(result.copy.image_subresource.base_layer, 0);
        assert_eq!(result.copy.image_subresource.num_layers, 1);
        assert_eq!(result.copy.image_offset, Offset3D { x: 6, y: 7, z: 0 });
        assert_eq!(
            result.copy.image_extent,
            Extent3D {
                width: 10,
                height: 6,
                depth: 1,
            }
        );
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
    fn fill_compute_image_views_uses_channel_gpu_memory_for_tic_reads() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use crate::memory_manager::MemoryManager;
        use parking_lot::Mutex as ParkingMutex;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let device_memory = Arc::new(MaxwellDeviceMemoryManager::default());
        let mut backing = vec![0u8; 0x6000];
        device_memory.smmu_set_physical_base_for_test(backing.as_ptr() as usize);
        device_memory.smmu_map_with_cpu_backing(
            0x9000_0000,
            backing.as_mut_ptr(),
            0x5000_0000,
            backing.len(),
            3,
            true,
        );
        let gpu_memory = Arc::new(ParkingMutex::new(
            MemoryManager::new_with_geometry_and_device_memory(
                7,
                Arc::clone(&device_memory),
                32,
                0x1_0000_0000,
                16,
                12,
            ),
        ));
        {
            let mut gpu_memory = gpu_memory.lock();
            gpu_memory.map(0x1000, 0x9000_0000, 0x1000, 0, false);
            gpu_memory.map(0x8000, 0x9000_1000, 0x4000, 0, false);
            let tic = color_2d_tic(0x8000, 0);
            assert!(gpu_memory.write_block(0x1000, &descriptor_bytes(tic.raw)));
        }
        cache.set_channel_gpu_memory(Arc::clone(&gpu_memory));
        assert!(cache
            .channel_state
            .compute_image_table
            .synchronize(0x1000, 0));
        cache
            .channel_state
            .compute_image_view_ids
            .resize(1, CORRUPT_ID);

        let mut views = [ImageViewInOut {
            index: 0,
            blacklist: false,
            id: NULL_IMAGE_VIEW_ID,
        }];
        cache.fill_compute_image_views(&mut views);

        assert!(views[0].id.is_valid());
        assert_ne!(views[0].id, NULL_IMAGE_VIEW_ID);
        let view = &cache.slot_image_views[views[0].id];
        assert_eq!(view.gpu_addr, 0x8000);
    }

    #[test]
    fn fill_image_views_resolves_corrupt_cached_id_after_descriptor_probe() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use crate::memory_manager::MemoryManager;
        use parking_lot::Mutex as ParkingMutex;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let device_memory = Arc::new(MaxwellDeviceMemoryManager::default());
        let mut backing = vec![0u8; 0x6000];
        device_memory.smmu_set_physical_base_for_test(backing.as_ptr() as usize);
        device_memory.smmu_map_with_cpu_backing(
            0x9000_0000,
            backing.as_mut_ptr(),
            0x5000_0000,
            backing.len(),
            3,
            true,
        );
        let gpu_memory = Arc::new(ParkingMutex::new(
            MemoryManager::new_with_geometry_and_device_memory(
                7,
                Arc::clone(&device_memory),
                32,
                0x1_0000_0000,
                16,
                12,
            ),
        ));
        {
            let mut gpu_memory = gpu_memory.lock();
            gpu_memory.map(0x1000, 0x9000_0000, 0x1000, 0, false);
            gpu_memory.map(0x8000, 0x9000_1000, 0x4000, 0, false);
            let tic = color_2d_tic(0x8000, 0);
            assert!(gpu_memory.write_block(0x1000, &descriptor_bytes(tic.raw)));
        }
        cache.set_channel_gpu_memory(Arc::clone(&gpu_memory));
        assert!(cache
            .channel_state
            .compute_image_table
            .synchronize(0x1000, 0));
        cache
            .channel_state
            .compute_image_view_ids
            .resize(1, CORRUPT_ID);

        let (_, is_new) = cache
            .channel_state
            .compute_image_table
            .read_with(0, |gpu_addr, out| {
                gpu_memory.lock().read_block(gpu_addr, out)
            });
        assert!(is_new);
        assert_eq!(cache.channel_state.compute_image_view_ids[0], CORRUPT_ID);

        let mut views = [ImageViewInOut {
            index: 0,
            blacklist: false,
            id: NULL_IMAGE_VIEW_ID,
        }];
        cache.fill_compute_image_views(&mut views);

        assert!(views[0].id.is_valid());
        assert_ne!(views[0].id, NULL_IMAGE_VIEW_ID);
        assert_ne!(views[0].id, CORRUPT_ID);
        assert_eq!(cache.channel_state.compute_image_view_ids[0], views[0].id);
        assert_eq!(cache.slot_image_views[views[0].id].gpu_addr, 0x8000);
    }

    #[test]
    fn fill_image_views_defers_backend_completion_for_inserted_tic_image() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use crate::memory_manager::MemoryManager;
        use parking_lot::Mutex as ParkingMutex;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        cache.set_backend_completes_join_images(true);

        let device_memory = Arc::new(MaxwellDeviceMemoryManager::default());
        let mut backing = vec![0u8; 0x6000];
        device_memory.smmu_set_physical_base_for_test(backing.as_ptr() as usize);
        device_memory.smmu_map_with_cpu_backing(
            0x9000_0000,
            backing.as_mut_ptr(),
            0x5000_0000,
            backing.len(),
            3,
            true,
        );
        let gpu_memory = Arc::new(ParkingMutex::new(
            MemoryManager::new_with_geometry_and_device_memory(
                7,
                Arc::clone(&device_memory),
                32,
                0x1_0000_0000,
                16,
                12,
            ),
        ));
        {
            let mut gpu_memory = gpu_memory.lock();
            gpu_memory.map(0x1000, 0x9000_0000, 0x1000, 0, false);
            gpu_memory.map(0x8000, 0x9000_1000, 0x4000, 0, false);
            let tic = color_2d_tic(0x8000, 0);
            assert!(gpu_memory.write_block(0x1000, &descriptor_bytes(tic.raw)));
        }
        cache.set_channel_gpu_memory(Arc::clone(&gpu_memory));
        assert!(cache
            .channel_state
            .compute_image_table
            .synchronize(0x1000, 0));
        cache
            .channel_state
            .compute_image_view_ids
            .resize(1, CORRUPT_ID);

        let mut views = [ImageViewInOut {
            index: 0,
            blacklist: false,
            id: NULL_IMAGE_VIEW_ID,
        }];
        cache.fill_compute_image_views(&mut views);

        let image_id = cache.slot_image_views[views[0].id].image_id;
        assert_eq!(cache.pending_backend_insertions, vec![image_id]);
        assert!(!cache.slot_images[image_id]
            .flags
            .contains(ImageFlagBits::REGISTERED));
    }

    #[test]
    fn get_compute_sampler_id_uses_channel_gpu_memory_for_tsc_reads() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use crate::memory_manager::MemoryManager;
        use parking_lot::Mutex as ParkingMutex;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let device_memory = Arc::new(MaxwellDeviceMemoryManager::default());
        let mut backing = vec![0u8; 0x1000];
        device_memory.smmu_set_physical_base_for_test(backing.as_ptr() as usize);
        device_memory.smmu_map_with_cpu_backing(
            0x9000_0000,
            backing.as_mut_ptr(),
            0x5000_0000,
            backing.len(),
            3,
            true,
        );
        let gpu_memory = Arc::new(ParkingMutex::new(
            MemoryManager::new_with_geometry_and_device_memory(
                7,
                Arc::clone(&device_memory),
                32,
                0x1_0000_0000,
                16,
                12,
            ),
        ));
        let mut tsc = TscEntry::default();
        tsc.raw[0] = 0x0000_03A2_0002_6080;
        {
            let mut gpu_memory = gpu_memory.lock();
            gpu_memory.map(0x2000, 0x9000_0000, 0x1000, 0, false);
            assert!(gpu_memory.write_block(0x2000, &descriptor_bytes(tsc.raw)));
        }
        cache.set_channel_gpu_memory(Arc::clone(&gpu_memory));
        assert!(cache
            .channel_state
            .compute_sampler_table
            .synchronize(0x2000, 0));
        cache
            .channel_state
            .compute_sampler_ids
            .resize(1, CORRUPT_ID);

        let sampler_id = cache.get_compute_sampler_id(0);

        assert!(sampler_id.is_valid());
        assert_ne!(sampler_id, NULL_SAMPLER_ID);
        assert_eq!(cache.slot_samplers[sampler_id].raw, tsc.raw);
    }

    #[test]
    fn synchronize_compute_descriptors_resizes_compute_tables() {
        let mut cache = test_cache();

        cache.synchronize_compute_descriptors(
            crate::texture_cache::texture_cache_base::ComputeDescriptorSyncRegs {
                linked_tsc: false,
                tic_addr: 0x3000,
                tic_limit: 4,
                tsc_addr: 0x2000,
                tsc_limit: 2,
            },
        );

        assert_eq!(cache.channel_state.compute_image_table.limit(), 4);
        assert_eq!(cache.channel_state.compute_sampler_table.limit(), 2);
        assert_eq!(cache.channel_state.compute_image_view_ids.len(), 5);
        assert_eq!(cache.channel_state.compute_sampler_ids.len(), 3);
        assert!(cache
            .channel_state
            .compute_image_view_ids
            .iter()
            .all(|&id| id == CORRUPT_ID));
        assert!(cache
            .channel_state
            .compute_sampler_ids
            .iter()
            .all(|&id| id == CORRUPT_ID));
    }

    #[test]
    fn synchronize_compute_descriptors_uses_tic_limit_when_tsc_is_linked() {
        let mut cache = test_cache();

        cache.synchronize_compute_descriptors(
            crate::texture_cache::texture_cache_base::ComputeDescriptorSyncRegs {
                linked_tsc: true,
                tic_addr: 0x7000,
                tic_limit: 6,
                tsc_addr: 0x6000,
                tsc_limit: 1,
            },
        );

        assert_eq!(cache.channel_state.compute_image_table.limit(), 6);
        assert_eq!(cache.channel_state.compute_sampler_table.limit(), 6);
        assert_eq!(cache.channel_state.compute_image_view_ids.len(), 7);
        assert_eq!(cache.channel_state.compute_sampler_ids.len(), 7);
    }

    #[test]
    fn synchronize_graphics_descriptors_updates_bound_channel_state() {
        use crate::control::channel_state::ChannelState;
        use crate::texture_cache::texture_cache_base::DescriptorSyncRegs;

        let mut cache = test_cache();
        let channel = ChannelState::new(10);
        cache.create_channel(&channel);
        cache.bind_to_channel(10);

        cache.synchronize_graphics_descriptors(DescriptorSyncRegs {
            sampler_binding_via_header: false,
            tex_header_addr: 0x5000,
            tex_header_limit: 808,
            tex_sampler_addr: 0x3000,
            tex_sampler_limit: 64,
        });

        let bound = cache
            .channel_caches
            .channel_state_by_bind_id(10)
            .expect("bound texture-cache channel exists");
        assert_eq!(bound.graphics_image_table.limit(), 808);
        assert_eq!(bound.graphics_sampler_table.limit(), 64);
        assert_eq!(bound.graphics_image_view_ids.len(), 809);
        assert_eq!(bound.graphics_sampler_ids.len(), 65);
        assert_eq!(cache.channel_state.graphics_image_table.limit(), 0);
        assert_eq!(cache.channel_state.graphics_sampler_table.limit(), 0);
    }

    #[test]
    fn synchronize_compute_descriptors_updates_bound_channel_state() {
        use crate::control::channel_state::ChannelState;
        use crate::texture_cache::texture_cache_base::ComputeDescriptorSyncRegs;

        let mut cache = test_cache();
        let channel = ChannelState::new(10);
        cache.create_channel(&channel);
        cache.bind_to_channel(10);

        cache.synchronize_compute_descriptors(ComputeDescriptorSyncRegs {
            linked_tsc: true,
            tic_addr: 0x7000,
            tic_limit: 12,
            tsc_addr: 0x6000,
            tsc_limit: 1,
        });

        let bound = cache
            .channel_caches
            .channel_state_by_bind_id(10)
            .expect("bound texture-cache channel exists");
        assert_eq!(bound.compute_image_table.limit(), 12);
        assert_eq!(bound.compute_sampler_table.limit(), 12);
        assert_eq!(bound.compute_image_view_ids.len(), 13);
        assert_eq!(bound.compute_sampler_ids.len(), 13);
        assert_eq!(cache.channel_state.compute_image_table.limit(), 0);
        assert_eq!(cache.channel_state.compute_sampler_table.limit(), 0);
    }

    #[test]
    fn update_render_targets_from_snapshot_registers_presentable_view() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;
        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let mut render_targets = Maxwell3DRenderTargets::default();
        render_targets.surface_clip.width = 1280;
        render_targets.surface_clip.height = 720;
        render_targets.rt_control = RtControlInfo {
            count: 1,
            map: [0, 0, 0, 0, 0, 0, 0, 0],
        };
        render_targets.render_targets[0] = RenderTargetInfo {
            address: 0x4000_0000,
            width: 64,
            height: 32,
            format: 0xD5,
            tile_mode: 2 | (1 << 4),
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
            tile_mode: 2 | (1 << 4),
            array_pitch: 32,
            depth: 1,
        };

        cache.update_render_targets_from_snapshot(&render_targets, |gpu_addr, _guest_size| {
            match gpu_addr {
                0x4000_0000 => Some(0x535B_5000),
                0x5000_0000 => Some(0x535C_0000),
                _ => None,
            }
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
        assert_eq!(cache.render_targets.size.width, 1280);
        assert_eq!(cache.render_targets.size.height, 720);
        assert_eq!(cache.slot_images.size(), 3);
        assert_eq!(cache.slot_image_views.size(), 4);
    }

    #[test]
    fn update_render_targets_from_snapshot_passes_guest_size_for_range_translation() {
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
        let info = ImageInfo::from_render_target_info(
            &render_targets.render_targets[0],
            render_targets.anti_alias_samples_mode,
        );
        let expected_guest_size =
            crate::texture_cache::util::calculate_guest_size_in_bytes(&info) as u64;

        cache.update_render_targets_from_snapshot(&render_targets, |gpu_addr, guest_size| {
            if gpu_addr == 0x4000_0000 && guest_size == expected_guest_size {
                Some(0x535B_5000)
            } else {
                None
            }
        });

        assert!(cache.render_targets.color_buffer_ids[0].is_valid());
    }

    #[test]
    fn update_render_targets_from_snapshot_forwards_raw_msaa_mode_to_image_info() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let mut render_targets = Maxwell3DRenderTargets::default();
        render_targets.anti_alias_samples_mode = 3;
        render_targets.rt_control = RtControlInfo {
            count: 1,
            map: [0, 0, 0, 0, 0, 0, 0, 0],
        };
        render_targets.render_targets[0] = RenderTargetInfo {
            address: 0x4000_0000,
            width: 64,
            height: 32,
            format: 0xD5,
            tile_mode: 2 | (1 << 4),
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
            tile_mode: 2 | (1 << 4),
            array_pitch: 32,
            depth: 1,
        };

        cache.update_render_targets_from_snapshot(&render_targets, |gpu_addr, _guest_size| {
            match gpu_addr {
                0x4000_0000 => Some(0x535B_5000),
                0x5000_0000 => Some(0x535C_0000),
                _ => None,
            }
        });

        let color_id = cache.render_targets.color_buffer_ids[0];
        let zeta_id = cache.render_targets.depth_buffer_id;

        assert_eq!(cache.slot_images[color_id].info.num_samples, 8);
        assert_eq!(cache.slot_images[zeta_id].info.num_samples, 8);
    }

    #[test]
    fn update_render_targets_from_snapshot_scales_render_target_size_when_rescaling() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use common::settings;
        use std::sync::Arc;

        let previous_resolution = settings::values().resolution_info.clone();
        {
            let mut values = settings::values_mut();
            values.resolution_info.up_scale = 3;
            values.resolution_info.down_shift = 1;
            values.resolution_info.active = true;
        }

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        cache.is_rescaling = true;
        let mut render_targets = Maxwell3DRenderTargets::default();
        render_targets.surface_clip.width = 1280;
        render_targets.surface_clip.height = 720;

        cache.update_render_targets_from_snapshot(&render_targets, |_gpu_addr, _guest_size| None);

        settings::values_mut().resolution_info = previous_resolution;

        assert_eq!(cache.render_targets.size.width, 1920);
        assert_eq!(cache.render_targets.size.height, 1080);
        assert!(cache.render_targets.is_rescaled);
    }

    #[test]
    fn update_render_targets_size_uses_wrapping_scale_multiply_like_upstream() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use common::settings;
        use std::sync::Arc;

        let previous_resolution = settings::values().resolution_info.clone();
        {
            let mut values = settings::values_mut();
            values.resolution_info.up_scale = 3;
            values.resolution_info.down_shift = 1;
            values.resolution_info.active = true;
        }

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        cache.is_rescaling = true;
        let mut render_targets = Maxwell3DRenderTargets::default();
        render_targets.surface_clip.width = u32::MAX;
        render_targets.surface_clip.height = u32::MAX - 1;

        cache.update_render_targets_from_snapshot(&render_targets, |_gpu_addr, _guest_size| None);

        settings::values_mut().resolution_info = previous_resolution;

        assert_eq!(
            cache.render_targets.size.width,
            u32::MAX.wrapping_mul(3) >> 1
        );
        assert_eq!(
            cache.render_targets.size.height,
            (u32::MAX - 1).wrapping_mul(3) >> 1
        );
    }

    #[test]
    fn update_render_targets_from_snapshot_with_clean_render_targets_preserves_state_like_upstream()
    {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        cache.render_targets.draw_buffers = [7, 6, 5, 4, 3, 2, 1, 0];
        cache.render_targets.size = Extent2D {
            width: 320,
            height: 180,
        };
        cache.render_targets.is_rescaled = true;
        let initial_image_count = cache.slot_images.size();
        let initial_image_view_count = cache.slot_image_views.size();

        let mut render_targets = Maxwell3DRenderTargets::default();
        render_targets.surface_clip.width = 1280;
        render_targets.surface_clip.height = 720;
        render_targets.rt_control = RtControlInfo {
            count: 1,
            map: [0, 1, 2, 3, 4, 5, 6, 7],
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
        let mut dirty_flags = [false; 256];
        dirty_flags[crate::dirty_flags::flags::RENDER_TARGET_CONTROL as usize] = true;
        dirty_flags[crate::dirty_flags::flags::COLOR_BUFFER0 as usize] = true;

        cache.update_render_targets_from_snapshot_with_dirty_flags(
            &render_targets,
            &dirty_flags,
            |_, _| panic!("clean RenderTargets must not re-resolve views"),
        );

        assert_eq!(cache.render_targets.draw_buffers, [7, 6, 5, 4, 3, 2, 1, 0]);
        assert_eq!(cache.render_targets.size.width, 320);
        assert_eq!(cache.render_targets.size.height, 180);
        assert!(cache.render_targets.is_rescaled);
        assert_eq!(cache.slot_images.size(), initial_image_count);
        assert_eq!(cache.slot_image_views.size(), initial_image_view_count);
    }

    #[test]
    fn update_render_targets_from_snapshot_uses_virtual_invalid_fallback_for_color_translation_miss(
    ) {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let mut render_targets = Maxwell3DRenderTargets::default();
        render_targets.surface_clip.width = 1280;
        render_targets.surface_clip.height = 720;
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

        cache.update_render_targets_from_snapshot(&render_targets, |_, _| None);

        let view_id = cache.render_targets.color_buffer_ids[0];
        assert!(view_id.is_valid());
        let image_id = cache.slot_image_views[view_id].image_id;
        assert_eq!(cache.slot_images[image_id].gpu_addr, 0x4000_0000);
        assert!(cache.slot_images[image_id].cpu_addr >= !(1u64 << 40));
    }

    #[test]
    fn update_render_targets_from_snapshot_uses_virtual_invalid_fallback_for_zeta_translation_miss()
    {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let mut render_targets = Maxwell3DRenderTargets::default();
        render_targets.surface_clip.width = 1280;
        render_targets.surface_clip.height = 720;
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

        cache.update_render_targets_from_snapshot(&render_targets, |gpu_addr, _guest_size| {
            match gpu_addr {
                0x4000_0000 => Some(0x535B_5000),
                0x5000_0000 => Some(0x535C_0000),
                _ => None,
            }
        });

        let original_depth = cache.render_targets.depth_buffer_id;
        assert!(original_depth.is_valid());

        render_targets.zeta.address = 0x5100_0000;
        let mut dirty_flags = [false; 256];
        dirty_flags[crate::dirty_flags::flags::ZETA_BUFFER as usize] = true;
        cache.update_render_targets_from_snapshot_with_dirty_flags(
            &render_targets,
            &dirty_flags,
            |_, _| None,
        );

        assert!(cache.render_targets.depth_buffer_id.is_valid());
        assert_ne!(cache.render_targets.depth_buffer_id, original_depth);
        let image_id = cache.slot_image_views[cache.render_targets.depth_buffer_id].image_id;
        assert_eq!(cache.slot_images[image_id].gpu_addr, 0x5100_0000);
        assert!(cache.slot_images[image_id].cpu_addr >= !(1u64 << 40));
    }

    #[test]
    fn update_render_targets_from_snapshot_ignores_slots_past_rt_control_count() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let mut render_targets = Maxwell3DRenderTargets::default();
        render_targets.surface_clip.width = 1280;
        render_targets.surface_clip.height = 720;
        render_targets.rt_control = RtControlInfo {
            count: 1,
            map: [0, 1, 0, 0, 0, 0, 0, 0],
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
        render_targets.render_targets[1] = RenderTargetInfo {
            address: 0x4100_0000,
            width: 64,
            height: 32,
            format: 0xD5,
            tile_mode: 1 << 12,
            array_pitch: 32 * 4,
            depth: 1,
            base_layer: 0,
        };

        cache.update_render_targets_from_snapshot(&render_targets, |gpu_addr, _guest_size| {
            match gpu_addr {
                0x4000_0000 => Some(0x535B_5000),
                0x4100_0000 => Some(0x535C_5000),
                _ => None,
            }
        });

        assert!(cache.render_targets.color_buffer_ids[0].is_valid());
        assert_eq!(
            cache.render_targets.color_buffer_ids[1],
            ImageViewId::default()
        );
        assert_eq!(cache.slot_images.size(), 2);
        assert_eq!(cache.slot_image_views.size(), 2);
    }

    #[test]
    fn update_render_targets_from_snapshot_dirty_flags_preserve_clean_color_slot() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let mut render_targets = Maxwell3DRenderTargets::default();
        render_targets.surface_clip.width = 1280;
        render_targets.surface_clip.height = 720;
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

        cache.update_render_targets_from_snapshot(&render_targets, |gpu_addr, _guest_size| {
            (gpu_addr == 0x4000_0000).then_some(0x535B_5000)
        });
        let original_view = cache.render_targets.color_buffer_ids[0];
        let original_images = cache.slot_images.size();

        render_targets.render_targets[0].address = 0x4100_0000;
        let mut dirty_flags = [false; 256];
        dirty_flags[crate::dirty_flags::flags::RENDER_TARGETS as usize] = true;
        cache.update_render_targets_from_snapshot_with_dirty_flags(
            &render_targets,
            &dirty_flags,
            |gpu_addr, _guest_size| (gpu_addr == 0x4100_0000).then_some(0x535C_5000),
        );

        assert_eq!(cache.render_targets.color_buffer_ids[0], original_view);
        assert_eq!(cache.slot_images.size(), original_images);

        dirty_flags[crate::dirty_flags::flags::RENDER_TARGET_CONTROL as usize] = true;
        cache.update_render_targets_from_snapshot_with_dirty_flags(
            &render_targets,
            &dirty_flags,
            |gpu_addr, _guest_size| (gpu_addr == 0x4100_0000).then_some(0x535C_5000),
        );

        assert_ne!(cache.render_targets.color_buffer_ids[0], original_view);
        assert!(cache.slot_images.size() > original_images);
    }

    #[test]
    fn bind_same_preemptive_render_target_view_has_no_download() {
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

        cache.update_render_targets_from_snapshot(&render_targets, |gpu_addr, _guest_size| {
            (gpu_addr == 0x4000_0000).then_some(0x535B_5000)
        });
        let view_id = cache.render_targets.color_buffer_ids[0];
        cache.slot_image_views[view_id]
            .flags
            .insert(ImageViewFlagBits::PREEMTIVE_DOWNLOAD);
        cache.uncommitted_downloads.clear();

        cache.bind_color_render_target(0, view_id);

        assert_eq!(cache.render_targets.color_buffer_ids[0], view_id);
        assert!(cache.uncommitted_downloads.is_empty());
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

        cache.update_render_targets_from_snapshot(&render_targets, |gpu_addr, _guest_size| {
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
    fn try_find_framebuffer_image_view_unknown_format_uses_upstream_default() {
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

        cache.update_render_targets_from_snapshot(&render_targets, |gpu_addr, _guest_size| {
            (gpu_addr == 0x4000_0000).then_some(0x535B_5000)
        });

        let config = FramebufferConfig {
            address: 0x535B_5000,
            width: 64,
            height: 32,
            stride: 64,
            pixel_format: AndroidPixelFormat(0),
            ..Default::default()
        };
        let view = cache
            .try_find_framebuffer_image_view(&config, 0x535B_5000)
            .expect("framebuffer view");

        assert_eq!(
            cache.slot_image_views[view.view_id].format,
            surface::PixelFormat::A8B8G8R8Unorm
        );
    }

    #[test]
    fn get_flush_area_marks_gpu_modified_image_views_preemptive() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use crate::texture_cache::image_view_base::ImageViewFlagBits;
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

        cache.update_render_targets_from_snapshot(&render_targets, |gpu_addr, _guest_size| {
            (gpu_addr == 0x4000_0000).then_some(0x535B_5000)
        });
        let view_id = cache.render_targets.color_buffer_ids[0];
        let image_id = cache.slot_image_views[view_id].image_id;
        cache.slot_images[image_id].info.forced_flushed = false;
        cache.slot_image_views[view_id]
            .flags
            .remove(ImageViewFlagBits::PREEMTIVE_DOWNLOAD);
        cache.mark_modification_by_id(image_id);
        assert!(!cache.slot_images[image_id].info.forced_flushed);

        let area = cache
            .get_flush_area(0x535B_5008, 0x10)
            .expect("GPU-modified image should return a flush area");
        assert_eq!(area.start_address, cache.slot_images[image_id].cpu_addr);
        assert_eq!(area.end_address, cache.slot_images[image_id].cpu_addr_end);
        assert!(!area.preemptive);
        assert!(cache.slot_images[image_id].info.forced_flushed);
        assert!(cache.slot_image_views[view_id]
            .flags
            .contains(ImageViewFlagBits::PREEMTIVE_DOWNLOAD));

        let second = cache
            .get_flush_area(0x535B_5010, 0x10)
            .expect("forced-flushed image should still return a flush area");
        assert!(second.preemptive);

        cache.slot_images[image_id]
            .flags
            .remove(ImageFlagBits::GPU_MODIFIED);
        assert!(cache.get_flush_area(0x535B_5008, 0x10).is_none());
    }

    #[test]
    fn is_region_gpu_modified_checks_registered_overlapping_images() {
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
        let cpu_addr = cache.slot_images[image_id].cpu_addr;

        assert!(!cache.is_region_gpu_modified(cpu_addr, 0x10));

        cache.mark_modification_by_id(image_id);

        assert!(cache.is_region_gpu_modified(cpu_addr + 4, 0x10));
        assert!(
            !cache.is_region_gpu_modified(cache.slot_images[image_id].cpu_addr_end + 0x1000, 0x10)
        );
    }

    #[test]
    fn mark_modification_preserves_cpu_modified_like_upstream() {
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
        assert!(cache.slot_images[image_id]
            .flags
            .contains(ImageFlagBits::CPU_MODIFIED));

        cache.mark_modification_by_id(image_id);

        let image = &cache.slot_images[image_id];
        assert!(image.flags.contains(ImageFlagBits::CPU_MODIFIED));
        assert!(image.flags.contains(ImageFlagBits::GPU_MODIFIED));
        assert_eq!(image.modification_tick, cache.modification_tick);
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
        let cpu_addr = cache.slot_images[image_id].cpu_addr;
        cache.slot_images[image_id]
            .flags
            .remove(ImageFlagBits::CPU_MODIFIED);
        cache.track_image(image_id);

        cache.write_memory(cpu_addr, 4);

        let image = &cache.slot_images[image_id];
        assert!(image.flags.contains(ImageFlagBits::CPU_MODIFIED));
        assert!(!image.flags.contains(ImageFlagBits::TRACKED));
    }

    #[test]
    fn insert_image_uses_virtual_invalid_cpu_space_when_untranslated() {
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

        let first_id = cache.insert_image(&info, 0x4000);
        let second_id = cache.insert_image(&info, 0x8000);

        let first = &cache.slot_images[first_id];
        let second = &cache.slot_images[second_id];
        assert_eq!(first.cpu_addr, !(1u64 << 40));
        assert_ne!(first.cpu_addr, first.gpu_addr);
        assert_eq!(
            second.cpu_addr,
            !(1u64 << 40) + common::alignment::align_up(first.guest_size_bytes as u64, 32)
        );
        assert_eq!(cache.collect_images_in_region(0x4000, 4), Vec::new());
        assert_eq!(
            cache.collect_images_in_region(first.cpu_addr, 4),
            vec![first_id]
        );
    }

    #[test]
    fn set_channel_gpu_memory_rebases_virtual_invalid_images() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use crate::memory_manager::MemoryManager;
        use parking_lot::Mutex as ParkingMutex;
        use std::sync::Arc;

        let device_memory = Arc::new(MaxwellDeviceMemoryManager::default());
        let mut cache = TextureCacheBase::new(Arc::clone(&device_memory));
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
        assert_eq!(cache.slot_images[image_id].cpu_addr, !(1u64 << 40));

        let gpu_memory = Arc::new(ParkingMutex::new(
            MemoryManager::new_with_geometry_and_device_memory(
                7,
                Arc::clone(&device_memory),
                32,
                0x1_0000_0000,
                16,
                12,
            ),
        ));
        gpu_memory.lock().map(0x4000, 0x9000_0000, 0x1000, 0, false);

        cache.set_channel_gpu_memory(gpu_memory);

        assert_eq!(cache.slot_images[image_id].cpu_addr, 0x9000_0000);
        assert_eq!(
            cache.collect_images_in_region(0x9000_0000, 4),
            vec![image_id]
        );
        assert_eq!(cache.collect_images_in_region(!(1u64 << 40), 4), Vec::new());
    }

    #[test]
    fn find_image_uses_translated_cpu_region_and_rejects_unmapped_gpu_addr() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use crate::memory_manager::MemoryManager;
        use parking_lot::Mutex as ParkingMutex;
        use std::sync::Arc;

        let device_memory = Arc::new(MaxwellDeviceMemoryManager::default());
        let mut cache = TextureCacheBase::new(Arc::clone(&device_memory));
        let gpu_memory = Arc::new(ParkingMutex::new(
            MemoryManager::new_with_geometry_and_device_memory(
                7,
                Arc::clone(&device_memory),
                32,
                0x1_0000_0000,
                16,
                12,
            ),
        ));
        gpu_memory.lock().map(0x4000, 0x9000_0000, 0x1000, 0, false);
        cache.set_channel_gpu_memory(gpu_memory);

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

        assert_eq!(cache.find_image(&info, 0x4000), Some(image_id));
        assert_eq!(cache.find_image(&info, 0x8000), None);
    }

    #[test]
    fn find_image_force_broken_views_disables_compatible_format_reuse() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use crate::memory_manager::MemoryManager;
        use parking_lot::Mutex as ParkingMutex;
        use std::sync::Arc;

        let device_memory = Arc::new(MaxwellDeviceMemoryManager::default());
        let mut cache = TextureCacheBase::new(Arc::clone(&device_memory));
        let gpu_memory = Arc::new(ParkingMutex::new(
            MemoryManager::new_with_geometry_and_device_memory(
                7,
                Arc::clone(&device_memory),
                32,
                0x1_0000_0000,
                16,
                12,
            ),
        ));
        gpu_memory.lock().map(0x4000, 0x9000_0000, 0x1000, 0, false);
        cache.set_channel_gpu_memory(gpu_memory);

        let existing_info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            size: Extent3D {
                width: 16,
                height: 16,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        let existing_id = cache.insert_image(&existing_info, 0x4000);
        let view_compatible_info = ImageInfo {
            format: surface::PixelFormat::B8G8R8A8Unorm,
            ..existing_info
        };

        assert!(surface::is_view_compatible(
            existing_info.format,
            view_compatible_info.format,
            false,
            true
        ));
        assert_eq!(
            cache.find_image_with_caps(
                &view_compatible_info,
                0x4000,
                RelaxedOptions::empty(),
                false,
                true,
            ),
            Some(existing_id)
        );
        assert_eq!(
            cache.find_image_with_caps(
                &view_compatible_info,
                0x4000,
                RelaxedOptions::FORCE_BROKEN_VIEWS,
                false,
                true,
            ),
            None
        );
        assert_eq!(
            cache.find_image_in_cpu_region_with_caps(
                &view_compatible_info,
                0x4000,
                0x9000_0000,
                RelaxedOptions::FORCE_BROKEN_VIEWS,
                false,
                true,
            ),
            None
        );
    }

    #[test]
    fn find_or_insert_reuses_stable_virtual_invalid_range_without_channel_memory() {
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

        let first_id = cache.find_or_insert_image(&info, 0x4000);
        let second_id = cache.find_or_insert_image(&info, 0x4000);
        let other_id = cache.find_or_insert_image(&info, 0x8000);

        assert_eq!(first_id, second_id);
        assert_ne!(first_id, other_id);
        assert_eq!(cache.slot_images.size(), 3);
    }

    #[test]
    fn sparse_insert_without_channel_memory_does_not_allocate_virtual_state() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::panic::{catch_unwind, AssertUnwindSafe};
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let mut info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            size: Extent3D {
                width: 512,
                height: 1,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        info.is_sparse = true;

        let slot_count = cache.slot_images.size();
        let virtual_invalid_space = cache.virtual_invalid_space;
        let virtual_invalid_ranges = cache.virtual_invalid_ranges.len();

        let result = catch_unwind(AssertUnwindSafe(|| {
            cache.insert_image(&info, 0x8000);
        }));

        assert!(result.is_err());
        assert_eq!(cache.slot_images.size(), slot_count);
        assert_eq!(cache.virtual_invalid_space, virtual_invalid_space);
        assert_eq!(cache.virtual_invalid_ranges.len(), virtual_invalid_ranges);
    }

    #[test]
    fn track_sparse_unregistered_image_uses_submapped_segments_like_upstream() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use crate::memory_manager::MemoryManager;
        use parking_lot::Mutex as ParkingMutex;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let gpu_memory = Arc::new(ParkingMutex::new(MemoryManager::new_with_geometry(
            11,
            22,
            1 << 22,
            16,
            12,
        )));
        {
            let mut gpu_memory = gpu_memory.lock();
            gpu_memory.map(0x8000, 0xA000, 0x1000, 0, false);
            gpu_memory.map(0x9000, 0xC000, 0x1000, 0, false);
        }
        cache.set_channel_gpu_memory(Arc::clone(&gpu_memory));

        let mut info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            size: Extent3D {
                width: 2048,
                height: 1,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        info.is_sparse = true;
        let image_id = cache
            .slot_images
            .insert(ImageBase::new(info, 0x8000, 0xA000));
        cache.slot_images[image_id]
            .flags
            .insert(ImageFlagBits::SPARSE);

        cache.track_image(image_id);

        assert!(cache.slot_images[image_id]
            .flags
            .contains(ImageFlagBits::TRACKED));
        assert!(!cache.slot_images[image_id]
            .flags
            .contains(ImageFlagBits::REGISTERED));
        assert!(!cache.sparse_views.contains_key(&image_id));
    }

    #[test]
    fn run_garbage_collector_deletes_old_lru_images_only() {
        let mut cache = test_cache();
        let info = test_color_info(16, 16);
        let old_id = cache.insert_image(&info, 0x4000);
        let touched_id = cache.insert_image(&info, 0x8000);
        let initial_memory = cache.total_used_memory;
        assert!(initial_memory > 0);

        cache.frame_tick = 100;
        cache.touch_image(touched_id);
        cache.run_garbage_collector();

        assert!(cache
            .collect_images_in_gpu_region(0x4000, 4, false)
            .is_empty());
        assert_eq!(
            cache.collect_images_in_gpu_region(0x8000, 4, false),
            vec![touched_id]
        );
        assert!(cache.total_used_memory < initial_memory);
    }

    #[test]
    fn run_garbage_collector_preserves_gpu_modified_image_without_download_path() {
        let mut cache = test_cache();
        let info = test_color_info(16, 16);
        let image_id = cache.insert_image(&info, 0x4000);
        cache.mark_modification_by_id(image_id);
        cache.frame_tick = 100;
        cache.expected_memory = 0;
        cache.critical_memory = u64::MAX;

        cache.run_garbage_collector();

        assert_eq!(
            cache.collect_images_in_gpu_region(0x4000, 4, false),
            vec![image_id]
        );
        assert!(cache.slot_images[image_id]
            .flags
            .contains(ImageFlagBits::GPU_MODIFIED));
    }

    #[test]
    fn write_downloaded_image_prefers_channel_gpu_memory() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use crate::memory_manager::MemoryManager;
        use parking_lot::Mutex as ParkingMutex;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let gpu_memory = Arc::new(ParkingMutex::new(MemoryManager::new_with_geometry(
            7,
            22,
            1 << 22,
            16,
            12,
        )));
        {
            let mut gpu_memory = gpu_memory.lock();
            gpu_memory.map(0, 0, 0x1_0000, 0, false);
        }
        cache.set_channel_gpu_memory(Arc::clone(&gpu_memory));
        cache.set_guest_memory_writer(Arc::new(|_, _| {
            panic!("channel gpu_memory should own texture download writeback")
        }));

        let mut info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            image_type: ImageType::Linear,
            size: crate::texture_cache::types::Extent3D {
                width: 2,
                height: 2,
                depth: 1,
            },
            tiling: TilingMode::PitchLinear(16),
            ..ImageInfo::default()
        };
        info.resources.levels = 1;
        info.resources.layers = 1;
        let image = ImageBase::new(info.clone(), 0x4000, 0x8000);
        let copy = BufferImageCopy {
            buffer_offset: 0,
            buffer_size: 16,
            buffer_row_length: 4,
            buffer_image_height: 2,
            image_subresource: SubresourceLayers::default(),
            image_offset: Offset3D { x: 0, y: 0, z: 0 },
            image_extent: Extent3D {
                width: 2,
                height: 2,
                depth: 1,
            },
        };
        let staging = [
            1, 2, 3, 4, 5, 6, 7, 8, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 9, 10, 11, 12,
            13, 14, 15, 16,
        ];

        assert!(cache.write_downloaded_image(&image, &[copy], &staging));
    }

    #[test]
    fn register_image_updates_gpu_page_table_and_unregister_clears_it() {
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

        assert_eq!(
            cache.collect_images_in_gpu_region(0x4000, 4, false),
            vec![image_id]
        );

        cache.unregister_image(image_id);

        assert!(cache
            .collect_images_in_gpu_region(0x4000, 4, false)
            .is_empty());
    }

    #[test]
    fn delete_image_marks_maxwell_render_targets_dirty() {
        use crate::control::channel_state::ChannelState;
        use crate::dirty_flags;
        use crate::engines::draw_manager::Maxwell3DAccess;
        use crate::engines::maxwell_3d::Maxwell3D;
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let mut channel = ChannelState::new(10);
        channel.maxwell_3d = Some(Box::new(Maxwell3D::new()));
        cache.create_channel(&channel);
        cache.bind_to_channel(10);

        let maxwell3d = channel.maxwell_3d.as_mut().unwrap();
        maxwell3d.clear_dirty_flag(dirty_flags::flags::RENDER_TARGETS);
        maxwell3d.clear_dirty_flag(dirty_flags::flags::ZETA_BUFFER);
        for rt in 0..8 {
            maxwell3d.clear_dirty_flag(dirty_flags::flags::COLOR_BUFFER0 + rt);
        }

        let info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            size: Extent3D {
                width: 16,
                height: 16,
                depth: 1,
            },
            ..ImageInfo::default()
        };

        let image_id = cache.insert_image(&info, 0x4000);
        cache.unregister_image(image_id);
        cache.delete_image(image_id, false);

        let flags = maxwell3d.dirty_flags();
        assert!(flags[dirty_flags::flags::RENDER_TARGETS as usize]);
        assert!(flags[dirty_flags::flags::ZETA_BUFFER as usize]);
        for rt in 0..8 {
            assert!(flags[(dirty_flags::flags::COLOR_BUFFER0 + rt) as usize]);
        }
    }

    #[test]
    fn gpu_page_table_storage_is_shared_per_address_space() {
        use crate::control::channel_state::ChannelState;
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use crate::memory_manager::MemoryManager;
        use parking_lot::Mutex as ParkingMutex;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let shared_gpu_memory = Arc::new(ParkingMutex::new(MemoryManager::new(17)));
        let separate_gpu_memory = Arc::new(ParkingMutex::new(MemoryManager::new(23)));

        let mut channel_a = ChannelState::new(10);
        channel_a.memory_manager = Some(Arc::clone(&shared_gpu_memory));
        let mut channel_b = ChannelState::new(11);
        channel_b.memory_manager = Some(Arc::clone(&shared_gpu_memory));
        let mut channel_c = ChannelState::new(12);
        channel_c.memory_manager = Some(Arc::clone(&separate_gpu_memory));

        cache.create_channel(&channel_a);
        cache.create_channel(&channel_b);
        cache.create_channel(&channel_c);

        let info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            size: Extent3D {
                width: 16,
                height: 16,
                depth: 1,
            },
            ..ImageInfo::default()
        };

        cache.bind_to_channel(10);
        let image_id = cache.insert_image(&info, 0x4000);

        cache.bind_to_channel(11);
        assert_eq!(
            cache.collect_images_in_gpu_region(0x4000, 4, false),
            vec![image_id]
        );

        cache.bind_to_channel(12);
        assert!(cache
            .collect_images_in_gpu_region(0x4000, 4, false)
            .is_empty());
    }

    #[test]
    fn join_images_deletes_exact_sparse_gpu_overlap() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use crate::memory_manager::MemoryManager;
        use parking_lot::Mutex as ParkingMutex;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let gpu_memory = Arc::new(ParkingMutex::new(MemoryManager::new_with_geometry(
            7,
            22,
            1 << 22,
            16,
            12,
        )));
        {
            let mut gpu_memory = gpu_memory.lock();
            gpu_memory.map(0x8000, 0xA000, 0x1000, 0, false);
            gpu_memory.map(0x9000, 0xC000, 0x1000, 0, false);
        }
        cache.set_channel_gpu_memory(Arc::clone(&gpu_memory));
        let mut info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            size: crate::texture_cache::types::Extent3D {
                width: 512,
                height: 1,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        info.is_sparse = true;
        let old_id = cache.join_images(&info, 0x8000, 0xA000);
        assert!(cache.slot_images[old_id]
            .flags
            .contains(ImageFlagBits::SPARSE));
        assert_eq!(
            cache.sparse_views.get(&old_id).expect("sparse maps").len(),
            2
        );

        let new_id = cache.join_images(&info, 0x8000, 0xD000);

        assert_ne!(new_id, old_id);
        assert_eq!(cache.slot_images[new_id].cpu_addr, 0xD000);
        assert_eq!(
            cache.sparse_views.get(&new_id).expect("sparse maps").len(),
            2
        );
        assert_eq!(
            cache.collect_images_in_gpu_region(0x8000, 4, false),
            vec![new_id]
        );
    }

    #[test]
    fn join_images_inserts_replacement_before_deleting_ignored_overlap_like_upstream() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use crate::memory_manager::MemoryManager;
        use parking_lot::Mutex as ParkingMutex;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let gpu_memory = Arc::new(ParkingMutex::new(MemoryManager::new_with_geometry(
            7,
            22,
            1 << 22,
            16,
            12,
        )));
        {
            let mut gpu_memory = gpu_memory.lock();
            gpu_memory.map(0x8000, 0xA000, 0x1000, 0, false);
            gpu_memory.map(0x9000, 0xC000, 0x1000, 0, false);
        }
        cache.set_channel_gpu_memory(Arc::clone(&gpu_memory));
        let mut info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            size: crate::texture_cache::types::Extent3D {
                width: 512,
                height: 1,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        info.is_sparse = true;
        let old_id = cache.join_images(&info, 0x8000, 0xA000);

        let new_id = cache.join_images(&info, 0x8000, 0xD000);

        assert_ne!(
            new_id, old_id,
            "upstream inserts the replacement before deleting ignored overlaps"
        );
        assert_eq!(cache.slot_images[new_id].cpu_addr, 0xD000);
    }

    #[test]
    fn join_images_queues_backend_deletion_for_ignored_overlap() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use crate::memory_manager::MemoryManager;
        use parking_lot::Mutex as ParkingMutex;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        cache.set_backend_completes_join_images(true);
        let gpu_memory = Arc::new(ParkingMutex::new(MemoryManager::new_with_geometry(
            7,
            22,
            1 << 22,
            16,
            12,
        )));
        {
            let mut gpu_memory = gpu_memory.lock();
            gpu_memory.map(0x8000, 0xA000, 0x1000, 0, false);
            gpu_memory.map(0x9000, 0xC000, 0x1000, 0, false);
        }
        cache.set_channel_gpu_memory(Arc::clone(&gpu_memory));
        let mut info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            size: crate::texture_cache::types::Extent3D {
                width: 512,
                height: 1,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        info.is_sparse = true;
        let old_id = cache.join_images(&info, 0x8000, 0xA000);
        cache.register_image(old_id);

        let new_id = cache.join_images(&info, 0x8000, 0xD000);

        assert_ne!(new_id, old_id);
        assert_eq!(cache.pending_backend_deletions.len(), 1);
        assert_eq!(cache.pending_backend_deletions[0].image_id, old_id);
    }

    #[test]
    #[should_panic(
        expected = "TextureCache::JoinImages ignored GPU-modified overlap is unimplemented"
    )]
    fn join_images_panics_on_gpu_modified_ignored_overlap_like_upstream() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use crate::memory_manager::MemoryManager;
        use parking_lot::Mutex as ParkingMutex;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let gpu_memory = Arc::new(ParkingMutex::new(MemoryManager::new_with_geometry(
            7,
            22,
            1 << 22,
            16,
            12,
        )));
        {
            let mut gpu_memory = gpu_memory.lock();
            gpu_memory.map(0x8000, 0xA000, 0x1000, 0, false);
            gpu_memory.map(0x9000, 0xC000, 0x1000, 0, false);
        }
        cache.set_channel_gpu_memory(Arc::clone(&gpu_memory));
        let mut info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            size: crate::texture_cache::types::Extent3D {
                width: 512,
                height: 1,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        info.is_sparse = true;
        let old_id = cache.join_images(&info, 0x8000, 0xA000);
        cache.slot_images[old_id]
            .flags
            .insert(ImageFlagBits::GPU_MODIFIED);

        let _ = cache.join_images(&info, 0x8000, 0xD000);
    }

    #[test]
    #[should_panic(
        expected = "TextureCache::JoinImages ignored GPU-modified overlap is unimplemented"
    )]
    fn backend_join_panics_on_gpu_modified_ignored_overlap_like_upstream() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use crate::memory_manager::MemoryManager;
        use parking_lot::Mutex as ParkingMutex;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        cache.set_backend_completes_join_images(true);
        let gpu_memory = Arc::new(ParkingMutex::new(MemoryManager::new_with_geometry(
            7,
            22,
            1 << 22,
            16,
            12,
        )));
        {
            let mut gpu_memory = gpu_memory.lock();
            gpu_memory.map(0x8000, 0xA000, 0x1000, 0, false);
            gpu_memory.map(0x9000, 0xC000, 0x1000, 0, false);
        }
        cache.set_channel_gpu_memory(Arc::clone(&gpu_memory));
        let mut info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            size: crate::texture_cache::types::Extent3D {
                width: 512,
                height: 1,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        info.is_sparse = true;
        let old_id = cache.join_images(&info, 0x8000, 0xA000);
        cache.register_image(old_id);
        cache.register_image_alloc(old_id);
        cache.slot_images[old_id]
            .flags
            .insert(ImageFlagBits::GPU_MODIFIED);

        let _ = cache.join_images(&info, 0x8000, 0xD000);
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
        let cpu_addr = cache.slot_images[image_id].cpu_addr;
        let map_id = cache.slot_images[image_id].map_view_id;
        assert!(map_id.is_valid());
        assert_eq!(cache.collect_images_in_region(cpu_addr, 4), vec![image_id]);

        cache.track_image(image_id);
        cache.unmap_memory(cpu_addr, 4);

        assert_eq!(cache.collect_images_in_region(cpu_addr, 4), Vec::new());
        assert_eq!(cache.slot_images.size(), 1);
        assert_eq!(cache.slot_map_views.size(), 0);
        assert!(cache.page_table.is_empty());
        assert!(cache.has_deleted_images);
    }

    #[test]
    fn find_or_insert_reuses_cpu_region_view_compatible_image() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let first = ImageInfo {
            format: surface::PixelFormat::A2B10G10R10Unorm,
            image_type: ImageType::E2D,
            resources: SubresourceExtent {
                levels: 1,
                layers: 1,
            },
            size: Extent3D {
                width: 1280,
                height: 720,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        let second = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            ..first.clone()
        };

        let first_id = cache.find_or_insert_image_from_info(&first, 0x51FC_90000, 0x2AE9_E000);
        cache.mark_modification_by_id(first_id);
        let second_id = cache.find_or_insert_image_from_info(&second, 0x51FC_90000, 0x2AE9_E000);

        assert_eq!(first_id, second_id);
        assert!(cache.pending_join_copies.is_empty());
    }

    #[test]
    fn find_or_insert_result_reports_inserted_only_for_new_image() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            image_type: ImageType::E2D,
            resources: SubresourceExtent {
                levels: 1,
                layers: 1,
            },
            size: Extent3D {
                width: 64,
                height: 64,
                depth: 1,
            },
            ..ImageInfo::default()
        };

        let first = cache.find_or_insert_image_from_info_with_options_result(
            &info,
            0x8000,
            0x1000,
            RelaxedOptions::empty(),
        );
        assert!(first.inserted);
        assert!(!first.needs_backend_completion);
        assert!(!first.queued_join_tail);

        let second = cache.find_or_insert_image_from_info_with_options_result(
            &info,
            0x8000,
            0x1000,
            RelaxedOptions::empty(),
        );
        assert_eq!(second.image_id, first.image_id);
        assert!(!second.inserted);
        assert!(!second.needs_backend_completion);
        assert!(!second.queued_join_tail);
    }

    #[test]
    fn backend_completed_join_defers_registration_and_alloc_until_finish() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        cache.set_backend_completes_join_images(true);
        let info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            image_type: ImageType::E2D,
            resources: SubresourceExtent {
                levels: 1,
                layers: 1,
            },
            size: Extent3D {
                width: 64,
                height: 64,
                depth: 1,
            },
            ..ImageInfo::default()
        };

        let inserted = cache.find_or_insert_image_from_info_with_options_result(
            &info,
            0x9000,
            0x2000,
            RelaxedOptions::empty(),
        );
        assert!(inserted.inserted);
        assert!(inserted.needs_backend_completion);
        assert!(!inserted.queued_join_tail);
        assert!(!cache.slot_images[inserted.image_id]
            .flags
            .contains(ImageFlagBits::REGISTERED));
        assert!(cache.image_allocs_table.is_empty());

        cache.register_image(inserted.image_id);
        cache.register_image_alloc(inserted.image_id);
        let reused = cache.find_or_insert_image_from_info_with_options_result(
            &info,
            0x9000,
            0x2000,
            RelaxedOptions::empty(),
        );
        assert_eq!(reused.image_id, inserted.image_id);
        assert!(!reused.inserted);
        assert!(!reused.needs_backend_completion);
        assert!(!reused.queued_join_tail);
    }

    #[test]
    #[should_panic(expected = "backend completion is required")]
    fn backend_completed_join_rejects_direct_find_or_insert_helper() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        cache.set_backend_completes_join_images(true);
        let info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            image_type: ImageType::E2D,
            resources: SubresourceExtent {
                levels: 1,
                layers: 1,
            },
            size: Extent3D {
                width: 64,
                height: 64,
                depth: 1,
            },
            ..ImageInfo::default()
        };

        let _ = cache.find_or_insert_image_from_info(&info, 0x9000, 0x2000);
    }

    #[test]
    fn backend_completed_join_keeps_explicit_result_path_available() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        cache.set_backend_completes_join_images(true);
        let info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            image_type: ImageType::E2D,
            resources: SubresourceExtent {
                levels: 1,
                layers: 1,
            },
            size: Extent3D {
                width: 64,
                height: 64,
                depth: 1,
            },
            ..ImageInfo::default()
        };

        let result = cache.find_or_insert_image_from_info_with_options_result(
            &info,
            0x9000,
            0x2000,
            RelaxedOptions::empty(),
        );

        assert!(result.inserted);
        assert!(result.needs_backend_completion);
        assert!(!result.queued_join_tail);
        assert!(!cache.slot_images[result.image_id]
            .flags
            .contains(ImageFlagBits::REGISTERED));
    }

    #[test]
    #[should_panic(expected = "cannot preserve upstream RefreshContents/SynchronizeAliases")]
    fn common_prepare_image_rejects_backendless_refresh_path() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        cache.prepare_image(NULL_IMAGE_ID, false, false);
    }

    #[test]
    #[should_panic(expected = "cannot preserve upstream ScaleUp/ScaleDown ordering")]
    fn common_rescale_render_targets_rejects_backendless_path() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let _ = cache.rescale_render_targets();
    }

    #[test]
    #[should_panic(expected = "cannot prepare render-target image views")]
    fn common_update_render_targets_rejects_backendless_path() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        cache.update_render_targets(false);
    }

    #[test]
    #[should_panic(expected = "cannot perform upstream Fermi2D image preparation")]
    fn common_blit_image_rejects_backendless_path() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let _ = cache.blit_image(&(), &(), &());
    }

    #[test]
    fn find_or_insert_result_panics_on_gpu_modified_ignored_overlap_like_upstream() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use crate::memory_manager::MemoryManager;
        use parking_lot::Mutex as ParkingMutex;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        cache.set_backend_completes_join_images(true);
        let gpu_memory = Arc::new(ParkingMutex::new(MemoryManager::new_with_geometry(
            7,
            22,
            1 << 22,
            16,
            12,
        )));
        {
            let mut gpu_memory = gpu_memory.lock();
            gpu_memory.map(0x8000, 0xA000, 0x1000, 0, false);
            gpu_memory.map(0x9000, 0xC000, 0x1000, 0, false);
        }
        cache.set_channel_gpu_memory(Arc::clone(&gpu_memory));
        let mut info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            size: Extent3D {
                width: 512,
                height: 1,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        info.is_sparse = true;

        let old = cache.find_or_insert_image_from_info_with_options_result(
            &info,
            0x8000,
            0xA000,
            RelaxedOptions::empty(),
        );
        assert!(old.inserted);
        assert!(old.needs_backend_completion);
        assert!(!old.queued_join_tail);
        cache.register_image(old.image_id);
        cache.register_image_alloc(old.image_id);
        cache.slot_images[old.image_id]
            .flags
            .insert(ImageFlagBits::GPU_MODIFIED);

        let panic_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            cache.find_or_insert_image_from_info_with_options_result(
                &info,
                0x8000,
                0xD000,
                RelaxedOptions::empty(),
            )
        }));

        assert!(
            panic_result.is_err(),
            "FindOrInsertImage must stop on the upstream UNIMPLEMENTED ignored GPU-modified overlap path"
        );
    }

    #[test]
    fn find_or_insert_does_not_reuse_cpu_region_incompatible_samples_or_layers() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let existing = ImageInfo {
            format: surface::PixelFormat::B10G11R11Float,
            image_type: ImageType::E2D,
            num_samples: 4,
            resources: SubresourceExtent {
                levels: 1,
                layers: 1,
            },
            size: Extent3D {
                width: 32,
                height: 32,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        let requested_cube = ImageInfo {
            num_samples: 1,
            resources: SubresourceExtent {
                levels: 1,
                layers: 6,
            },
            ..existing.clone()
        };

        let first_id = cache.find_or_insert_image_from_info(&existing, 0x55C_BB0000, 0x5065_6000);
        let second_id =
            cache.find_or_insert_image_from_info(&requested_cube, 0x55C_BB0000, 0x5065_6000);

        assert_ne!(first_id, second_id);
        assert_eq!(cache.slot_images[first_id].info.num_samples, 4);
        assert_eq!(cache.slot_images[first_id].info.resources.layers, 1);
        assert_eq!(cache.slot_images[second_id].info.num_samples, 1);
        assert_eq!(cache.slot_images[second_id].info.resources.layers, 6);
    }

    #[test]
    fn delete_image_removes_view_references_and_framebuffers() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use crate::texture_cache::render_targets::RenderTargets;
        use common::slot_vector::SlotId;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let descriptor = color_2d_tic(0x5000_0000, 0);
        let view_id = cache.create_image_view(&descriptor);
        assert!(view_id.is_valid());
        let image_id = cache.slot_image_views[view_id].image_id;
        assert!(image_id.is_valid());

        cache.channel_state.image_views.insert(descriptor, view_id);
        cache.channel_state.graphics_image_view_ids = vec![view_id, view_id];
        cache.channel_state.compute_image_view_ids = vec![view_id];
        let mut framebuffer_key = RenderTargets::default();
        framebuffer_key.color_buffer_ids[0] = view_id;
        cache
            .framebuffers
            .insert(framebuffer_key, SlotId { index: 0x1234 });

        cache.unregister_image(image_id);
        cache.delete_image(image_id, false);

        assert!(!cache.channel_state.image_views.contains_key(&descriptor));
        assert!(cache
            .channel_state
            .graphics_image_view_ids
            .iter()
            .all(|&id| id == CORRUPT_ID));
        assert!(cache
            .channel_state
            .compute_image_view_ids
            .iter()
            .all(|&id| id == CORRUPT_ID));
        assert!(cache.framebuffers.is_empty());
        assert!(cache.has_deleted_images);
        assert_eq!(cache.sentenced_images.retained_len(), 1);
        assert_eq!(cache.sentenced_image_view.retained_len(), 1);

        for _ in 0..TICKS_TO_DESTROY {
            cache.tick_delayed_destruction_rings();
        }

        assert_eq!(cache.sentenced_images.retained_len(), 0);
        assert_eq!(cache.sentenced_image_view.retained_len(), 0);
    }

    #[test]
    fn delete_image_invalidates_all_active_channel_image_views() {
        use crate::control::channel_state::ChannelState;
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let mut channel_a = ChannelState::new(10);
        let mut channel_b = ChannelState::new(11);
        channel_a.bind_id = 10;
        channel_b.bind_id = 11;
        cache.create_channel(&channel_a);
        cache.create_channel(&channel_b);
        cache.bind_to_channel(10);

        let descriptor = color_2d_tic(0x5100_0000, 0);
        let view_id = cache.create_image_view(&descriptor);
        let image_id = cache.slot_image_views[view_id].image_id;

        {
            let channel = cache
                .channel_caches
                .channel_state_by_bind_id_mut(10)
                .expect("channel 10 exists");
            channel.image_views.insert(descriptor, view_id);
            channel.graphics_image_view_ids = vec![view_id];
            channel.compute_image_view_ids = vec![view_id];
        }
        {
            let channel = cache
                .channel_caches
                .channel_state_by_bind_id_mut(11)
                .expect("channel 11 exists");
            channel.image_views.insert(descriptor, view_id);
            channel.graphics_image_view_ids = vec![view_id];
            channel.compute_image_view_ids = vec![view_id];
        }

        cache.unregister_image(image_id);
        cache.delete_image(image_id, false);

        for bind_id in [10, 11] {
            let channel = cache
                .channel_caches
                .channel_state_by_bind_id(bind_id)
                .expect("channel exists after delete");
            assert!(!channel.image_views.contains_key(&descriptor));
            assert!(channel
                .graphics_image_view_ids
                .iter()
                .all(|&id| id == CORRUPT_ID));
            assert!(channel
                .compute_image_view_ids
                .iter()
                .all(|&id| id == CORRUPT_ID));
        }
    }

    #[test]
    fn invalidate_scale_shared_tail_invalidates_all_active_channel_image_views() {
        use crate::control::channel_state::ChannelState;
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let mut channel_a = ChannelState::new(10);
        let mut channel_b = ChannelState::new(11);
        channel_a.bind_id = 10;
        channel_b.bind_id = 11;
        cache.create_channel(&channel_a);
        cache.create_channel(&channel_b);
        cache.bind_to_channel(10);

        let descriptor = color_2d_tic(0x5100_0000, 0);
        let view_id = cache.create_image_view(&descriptor);

        for bind_id in [10, 11] {
            let channel = cache
                .channel_caches
                .channel_state_by_bind_id_mut(bind_id)
                .expect("channel exists");
            channel.image_views.insert(descriptor, view_id);
            channel.graphics_image_view_ids = vec![view_id];
            channel.compute_image_view_ids = vec![view_id];
        }

        cache.remove_image_view_references(&[view_id]);
        cache.invalidate_channel_image_views();

        for bind_id in [10, 11] {
            let channel = cache
                .channel_caches
                .channel_state_by_bind_id(bind_id)
                .expect("channel exists after invalidate");
            assert!(!channel.image_views.contains_key(&descriptor));
            assert!(channel
                .graphics_image_view_ids
                .iter()
                .all(|&id| id == CORRUPT_ID));
            assert!(channel
                .compute_image_view_ids
                .iter()
                .all(|&id| id == CORRUPT_ID));
        }
    }

    #[test]
    fn insert_image_registers_and_delete_removes_image_alloc_entry() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            image_type: ImageType::E2D,
            size: Extent3D {
                width: 64,
                height: 64,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        let image_id = cache.insert_image(&info, 0x5200_0000);
        let gpu_addr = cache.slot_images[image_id].gpu_addr;
        let alloc_id = cache.image_allocs_table[&gpu_addr];

        assert_eq!(cache.slot_image_allocs[alloc_id].images, vec![image_id]);

        cache.unregister_image(image_id);
        cache.delete_image(image_id, false);

        assert!(!cache.image_allocs_table.contains_key(&gpu_addr));
    }

    #[test]
    fn join_images_records_incompatible_overlaps() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let first = ImageInfo {
            format: surface::PixelFormat::B10G11R11Float,
            image_type: ImageType::E2D,
            size: Extent3D {
                width: 480,
                height: 272,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        let second = ImageInfo {
            format: surface::PixelFormat::A2B10G10R10Unorm,
            image_type: ImageType::E2D,
            size: Extent3D {
                width: 1920,
                height: 1080,
                depth: 1,
            },
            ..ImageInfo::default()
        };

        let first_id = cache.join_images(&first, 0x5219_F0000, 0x2CBF_E000);
        let second_id = cache.join_images(&second, 0x5219_F0000, 0x2CBF_E000);

        assert_ne!(first_id, second_id);
        assert!(cache.slot_images[first_id]
            .overlapping_images
            .contains(&second_id));
        assert!(cache.slot_images[second_id]
            .overlapping_images
            .contains(&first_id));
    }

    #[test]
    fn join_images_defers_pending_alias_relations_until_backend_tail() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let mut full = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            image_type: ImageType::E2D,
            resources: SubresourceExtent {
                levels: 2,
                layers: 1,
            },
            size: Extent3D {
                width: 64,
                height: 64,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        full.layer_stride = crate::texture_cache::util::calculate_layer_stride(&full);
        full.maybe_unaligned_layer_stride = crate::texture_cache::util::calculate_layer_size(&full);
        let sub = ImageInfo {
            resources: SubresourceExtent {
                levels: 1,
                layers: 1,
            },
            size: Extent3D {
                width: 32,
                height: 32,
                depth: 1,
            },
            ..full.clone()
        };

        let full_base = ImageBase::new(full.clone(), 0x5000, 0x9000);
        let mip_offset = full_base.mip_level_offsets[1] as u64;
        let full_id = cache.join_images(&full, 0x5000, 0x9000);
        cache.set_backend_completes_join_images(true);

        let sub_id = cache.join_images(&sub, 0x5000 + mip_offset, 0x9000 + mip_offset);

        assert_eq!(cache.pending_join_copies.len(), 1);
        let pending = &cache.pending_join_copies[0];
        assert!(!pending.alias_relations_applied);
        assert!(pending.alias_indices.is_empty());
        assert_eq!(pending.left_aliased_ids, vec![full_id]);
        assert!(pending
            .copies
            .iter()
            .any(|copy| copy.is_alias && copy.id == full_id));

        assert!(cache.slot_images[sub_id].aliased_images.is_empty());
        assert!(cache.slot_images[full_id].aliased_images.is_empty());
        assert!(!cache.slot_images[sub_id]
            .flags
            .contains(ImageFlagBits::ALIAS));
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

    #[test]
    fn create_image_view_with_gpu_to_cpu_uses_virtual_invalid_fallback_like_upstream_insert() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let mut descriptor = color_2d_tic(0, 2);
        let layer_stride = ImageInfo::from_tic_entry(&descriptor).layer_stride as u64;
        descriptor = color_2d_tic(0x5000_0000 + 2 * layer_stride, 2);

        let view_id = cache.create_image_view_with_gpu_to_cpu(&descriptor, &mut |_gpu, _size| None);

        assert!(view_id.is_valid());
        let image_id = cache.slot_image_views[view_id].image_id;
        assert_eq!(cache.slot_images[image_id].gpu_addr, 0x5000_0000);
        assert!(cache.slot_images[image_id].cpu_addr >= !(1u64 << 40));
    }

    #[test]
    fn backend_completed_create_image_view_uses_virtual_invalid_fallback_like_upstream_insert() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        cache.set_backend_completes_join_images(true);
        let mut descriptor = color_2d_tic(0, 2);
        let layer_stride = ImageInfo::from_tic_entry(&descriptor).layer_stride as u64;
        descriptor = color_2d_tic(0x5100_0000 + 2 * layer_stride, 2);

        let view_id = cache.create_image_view(&descriptor);

        assert!(view_id.is_valid());
        let image_id = cache.slot_image_views[view_id].image_id;
        assert_eq!(cache.slot_images[image_id].gpu_addr, 0x5100_0000);
        assert!(cache.slot_images[image_id].cpu_addr >= !(1u64 << 40));
        assert_eq!(cache.pending_backend_insertions, vec![image_id]);
    }

    #[test]
    #[should_panic(expected = "TextureCache::CreateImageView TryFindBase failed")]
    fn create_image_view_panics_when_try_find_base_fails_like_upstream() {
        let descriptor = color_2d_tic(0x5001, 0);
        let image = ImageBase::new(ImageInfo::from_tic_entry(&descriptor), 0x5000, 0x9000);

        let _ = TextureCacheBase::create_image_view_base(&image, &descriptor);
    }

    #[test]
    #[should_panic(expected = "TextureCache::CreateImageView base level must be zero")]
    fn create_image_view_panics_when_base_level_is_not_zero_like_upstream() {
        let mut image = ImageBase::new(
            ImageInfo {
                format: surface::PixelFormat::A8B8G8R8Unorm,
                image_type: ImageType::E2D,
                resources: SubresourceExtent {
                    levels: 2,
                    layers: 1,
                },
                size: Extent3D {
                    width: 64,
                    height: 64,
                    depth: 1,
                },
                tiling: TilingMode::BlockLinear(Extent3D {
                    width: 0,
                    height: 0,
                    depth: 0,
                }),
                layer_stride: 0x1000,
                maybe_unaligned_layer_stride: 0x1000,
                num_samples: 1,
                tile_width_spacing: 0,
                rescaleable: false,
                downscaleable: false,
                forced_flushed: false,
                dma_downloaded: false,
                is_sparse: false,
            },
            0x5000,
            0x9000,
        );
        image.mip_level_offsets[1] = 0x100;
        let mip_descriptor = color_2d_tic(0x5100, 0);

        let _ = TextureCacheBase::create_image_view_base(&image, &mip_descriptor);
    }

    #[test]
    #[should_panic(expected = "TextureCache::FindRenderTargetView TryFindBase failed")]
    fn find_render_target_view_panics_when_try_find_base_fails_like_upstream() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            image_type: ImageType::E2D,
            resources: SubresourceExtent {
                levels: 1,
                layers: 1,
            },
            size: Extent3D {
                width: 64,
                height: 64,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        let image_id = cache
            .slot_images
            .insert(ImageBase::new(info.clone(), 0x5000, 0x9000));

        let _ = cache.find_image_view_from_image_info(image_id, &info, 0x5001);
    }

    fn insert_test_image(cache: &mut TextureCacheBase, gpu_addr: u64) -> ImageId {
        let info = ImageInfo {
            format: surface::PixelFormat::A8B8G8R8Unorm,
            image_type: ImageType::E2D,
            size: Extent3D {
                width: 64,
                height: 64,
                depth: 1,
            },
            ..ImageInfo::default()
        };
        cache
            .slot_images
            .insert(crate::texture_cache::image_base::ImageBase::new(
                info, gpu_addr, gpu_addr,
            ))
    }

    fn insert_test_view(cache: &mut TextureCacheBase, image_id: ImageId) -> ImageViewId {
        let mut view =
            ImageViewBase::null(crate::texture_cache::image_view_base::NullImageViewParams);
        view.image_id = image_id;
        cache.slot_image_views.insert(view)
    }

    #[test]
    fn check_feedback_loop_detects_color_target_alias() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let image_id = insert_test_image(&mut cache, 0x6000_0000);
        let sampled_view_id = insert_test_view(&mut cache, image_id);
        let color_view_id = insert_test_view(&mut cache, image_id);
        cache.render_targets.color_buffer_ids[0] = color_view_id;

        assert!(cache.check_feedback_loop(&[ImageViewInOut {
            id: sampled_view_id,
            ..ImageViewInOut::default()
        }]));
    }

    #[test]
    fn check_feedback_loop_detects_depth_target_alias() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let image_id = insert_test_image(&mut cache, 0x6100_0000);
        let sampled_view_id = insert_test_view(&mut cache, image_id);
        cache.render_targets.depth_buffer_id = insert_test_view(&mut cache, image_id);

        assert!(cache.check_feedback_loop(&[ImageViewInOut {
            id: sampled_view_id,
            ..ImageViewInOut::default()
        }]));
    }

    #[test]
    fn check_feedback_loop_ignores_unrelated_and_null_views() {
        use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
        use std::sync::Arc;

        let mut cache = TextureCacheBase::new(Arc::new(MaxwellDeviceMemoryManager::default()));
        let sampled_image_id = insert_test_image(&mut cache, 0x6200_0000);
        let target_image_id = insert_test_image(&mut cache, 0x6300_0000);
        let sampled_view_id = insert_test_view(&mut cache, sampled_image_id);
        cache.render_targets.color_buffer_ids[0] = insert_test_view(&mut cache, target_image_id);

        assert!(!cache.check_feedback_loop(&[
            ImageViewInOut {
                id: NULL_IMAGE_VIEW_ID,
                ..ImageViewInOut::default()
            },
            ImageViewInOut {
                id: sampled_view_id,
                ..ImageViewInOut::default()
            },
        ]));
    }
}
