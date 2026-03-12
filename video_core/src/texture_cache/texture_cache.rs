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
//! signatures are ported; bodies are `todo!()` until dependent types are
//! complete.

use super::image_base::GPUVAddr;
use super::image_info::ImageInfo;
use super::texture_cache_base::*;
use super::types::*;

// All method implementations live on TextureCacheBase.

impl TextureCacheBase {
    // ── Garbage collection ─────────────────────────────────────────────

    /// Port of `TextureCache<P>::RunGarbageCollector`.
    pub fn run_garbage_collector(&mut self) {
        todo!("run_garbage_collector")
    }

    // ── Image view resolution ──────────────────────────────────────────

    /// Port of `TextureCache<P>::FillGraphicsImageViews`.
    pub fn fill_graphics_image_views(
        &mut self,
        _views: &mut [ImageViewInOut],
        _has_blacklists: bool,
    ) {
        todo!("fill_graphics_image_views")
    }

    /// Port of `TextureCache<P>::FillComputeImageViews`.
    pub fn fill_compute_image_views(&mut self, _views: &mut [ImageViewInOut]) {
        todo!("fill_compute_image_views")
    }

    /// Port of `TextureCache<P>::CheckFeedbackLoop`.
    pub fn check_feedback_loop(&self, _views: &[ImageViewInOut]) {
        todo!("check_feedback_loop")
    }

    // ── Descriptor synchronisation ─────────────────────────────────────

    /// Port of `TextureCache<P>::SynchronizeGraphicsDescriptors`.
    pub fn synchronize_graphics_descriptors(&mut self) {
        todo!("synchronize_graphics_descriptors")
    }

    /// Port of `TextureCache<P>::SynchronizeComputeDescriptors`.
    pub fn synchronize_compute_descriptors(&mut self) {
        todo!("synchronize_compute_descriptors")
    }

    // ── Render targets ─────────────────────────────────────────────────

    /// Port of `TextureCache<P>::RescaleRenderTargets`.
    pub fn rescale_render_targets(&mut self) -> bool {
        todo!("rescale_render_targets")
    }

    /// Port of `TextureCache<P>::UpdateRenderTargets`.
    pub fn update_render_targets(&mut self, _is_clear: bool) {
        todo!("update_render_targets")
    }

    // ── Image lookup / insertion ───────────────────────────────────────

    /// Port of `TextureCache<P>::FindOrInsertImage`.
    pub fn find_or_insert_image(
        &mut self,
        _info: &ImageInfo,
        _gpu_addr: GPUVAddr,
        _options: RelaxedOptions,
    ) -> ImageId {
        todo!("find_or_insert_image")
    }

    /// Port of `TextureCache<P>::FindImage`.
    pub fn find_image(
        &mut self,
        _info: &ImageInfo,
        _gpu_addr: GPUVAddr,
        _options: RelaxedOptions,
    ) -> ImageId {
        todo!("find_image")
    }

    /// Port of `TextureCache<P>::InsertImage`.
    pub fn insert_image(
        &mut self,
        _info: &ImageInfo,
        _gpu_addr: GPUVAddr,
        _options: RelaxedOptions,
    ) -> ImageId {
        todo!("insert_image")
    }

    /// Port of `TextureCache<P>::JoinImages`.
    pub fn join_images(
        &mut self,
        _info: &ImageInfo,
        _gpu_addr: GPUVAddr,
        _cpu_addr: u64,
    ) -> ImageId {
        todo!("join_images")
    }

    // ── Registration / tracking ────────────────────────────────────────

    /// Port of `TextureCache<P>::RegisterImage`.
    pub fn register_image(&mut self, _image_id: ImageId) {
        todo!("register_image")
    }

    /// Port of `TextureCache<P>::UnregisterImage`.
    pub fn unregister_image(&mut self, _image_id: ImageId) {
        todo!("unregister_image")
    }

    /// Port of `TextureCache<P>::TrackImage`.
    pub fn track_image(&mut self, _image_id: ImageId) {
        todo!("track_image")
    }

    /// Port of `TextureCache<P>::UntrackImage`.
    pub fn untrack_image(&mut self, _image_id: ImageId) {
        todo!("untrack_image")
    }

    /// Port of `TextureCache<P>::DeleteImage`.
    pub fn delete_image(&mut self, _image_id: ImageId, _immediate_delete: bool) {
        todo!("delete_image")
    }

    // ── Blit ───────────────────────────────────────────────────────────

    /// Port of `TextureCache<P>::BlitImage`.
    pub fn blit_image(&mut self, _dst: &(), _src: &(), _copy: &()) -> bool {
        todo!("blit_image")
    }

    // ── Rescaling ──────────────────────────────────────────────────────

    /// Port of `TextureCache<P>::IsRescaling`.
    pub fn is_rescaling_active(&self) -> bool {
        self.is_rescaling
    }

    // ── Prepare / refresh ──────────────────────────────────────────────

    /// Port of `TextureCache<P>::PrepareImage`.
    pub fn prepare_image(&mut self, _image_id: ImageId, _is_modification: bool, _invalidate: bool) {
        todo!("prepare_image")
    }

    /// Port of `TextureCache<P>::RefreshContents`.
    fn refresh_contents(&mut self, _image_id: ImageId) {
        todo!("refresh_contents")
    }

    // ── Modification marks ─────────────────────────────────────────────

    /// Port of `TextureCache<P>::MarkModification(ImageId)`.
    pub fn mark_modification_by_id(&mut self, _id: ImageId) {
        todo!("mark_modification_by_id")
    }

    // ── GPU memory queries ─────────────────────────────────────────────

    /// Port of `TextureCache<P>::IsRegionGpuModified`.
    pub fn is_region_gpu_modified(&self, _addr: u64, _size: usize) -> bool {
        todo!("is_region_gpu_modified")
    }
}
