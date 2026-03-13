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

use super::image_base::GPUVAddr;
use super::image_info::ImageInfo;
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
        log::warn!("TextureCacheBase::run_garbage_collector: backend types not yet available — GC skipped");
    }

    // ── Image view resolution ──────────────────────────────────────────

    /// Port of `TextureCache<P>::FillGraphicsImageViews`.
    ///
    /// Iterates `views`, resolves each TIC descriptor via `VisitImageView`,
    /// and optionally applies blacklist rescale-down logic.  Requires
    /// TICEntry, descriptor tables, and backend image types.
    pub fn fill_graphics_image_views(
        &mut self,
        _views: &mut [ImageViewInOut],
        _has_blacklists: bool,
    ) {
        log::warn!("TextureCacheBase::fill_graphics_image_views: backend types not yet available");
    }

    /// Port of `TextureCache<P>::FillComputeImageViews`.
    pub fn fill_compute_image_views(&mut self, _views: &mut [ImageViewInOut]) {
        log::warn!("TextureCacheBase::fill_compute_image_views: backend types not yet available");
    }

    /// Port of `TextureCache<P>::CheckFeedbackLoop`.
    ///
    /// Checks whether any sampled image view matches a current render target;
    /// if so, emits a barrier via `Runtime::BarrierFeedbackLoop`.
    pub fn check_feedback_loop(&self, _views: &[ImageViewInOut]) {
        log::warn!("TextureCacheBase::check_feedback_loop: backend types not yet available");
    }

    // ── Descriptor synchronisation ─────────────────────────────────────

    /// Port of `TextureCache<P>::SynchronizeGraphicsDescriptors`.
    ///
    /// Reads current Maxwell3D TIC/TSC addresses and limits; resizes cached
    /// sampler/image-view id arrays when the table limits change.
    pub fn synchronize_graphics_descriptors(&mut self) {
        log::warn!(
            "TextureCacheBase::synchronize_graphics_descriptors: Maxwell3D regs not yet ported"
        );
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
    pub fn register_image(&mut self, _image_id: ImageId) {
        log::warn!("TextureCacheBase::register_image: backend types not yet available");
    }

    /// Port of `TextureCache<P>::UnregisterImage`.
    ///
    /// Removes the image from page tables and clears any image-view references.
    pub fn unregister_image(&mut self, _image_id: ImageId) {
        log::warn!("TextureCacheBase::unregister_image: backend types not yet available");
    }

    /// Port of `TextureCache<P>::TrackImage`.
    ///
    /// Registers a CPU write-fault handler so the image is invalidated on
    /// guest writes.
    pub fn track_image(&mut self, _image_id: ImageId) {
        log::warn!("TextureCacheBase::track_image: backend types not yet available");
    }

    /// Port of `TextureCache<P>::UntrackImage`.
    ///
    /// Removes the CPU write-fault handler for the image.
    pub fn untrack_image(&mut self, _image_id: ImageId) {
        log::warn!("TextureCacheBase::untrack_image: backend types not yet available");
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
    pub fn mark_modification_by_id(&mut self, _id: ImageId) {
        log::warn!("TextureCacheBase::mark_modification_by_id: backend types not yet available");
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
