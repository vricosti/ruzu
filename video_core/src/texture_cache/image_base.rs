// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/texture_cache/image_base.h and image_base.cpp
//!
//! Defines `ImageBase`, `ImageMapView`, `ImageAllocBase`, and the
//! `add_image_alias` helper.

use super::image_info::ImageInfo;
use super::image_view_info::ImageViewInfo;
use super::types::*;

// ── Type aliases matching upstream ─────────────────────────────────────

/// GPU virtual address (u64).
pub type GPUVAddr = u64;
/// CPU virtual address (u64).
pub type VAddr = u64;

// ── ImageFlagBits ──────────────────────────────────────────────────────

bitflags::bitflags! {
    /// Flags associated with a cached image.
    ///
    /// Port of `VideoCommon::ImageFlagBits`.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct ImageFlagBits: u32 {
        const ACCELERATED_UPLOAD   = 1 << 0;
        const CONVERTED            = 1 << 1;
        const CPU_MODIFIED         = 1 << 2;
        const GPU_MODIFIED         = 1 << 3;
        const TRACKED              = 1 << 4;
        const STRONG               = 1 << 5;
        const REGISTERED           = 1 << 6;
        const PICKED               = 1 << 7;
        const REMAPPED             = 1 << 8;
        const SPARSE               = 1 << 9;
        const BAD_OVERLAP          = 1 << 10;
        const ALIAS                = 1 << 11;
        const COSTLY_LOAD          = 1 << 12;
        const RESCALED             = 1 << 13;
        const CHECKING_RESCALABLE  = 1 << 14;
        const IS_RESCALABLE        = 1 << 15;
        const ASYNCHRONOUS_DECODE  = 1 << 16;
        const IS_DECODING          = 1 << 17;
    }
}

// ── AliasedImage ───────────────────────────────────────────────────────

/// An aliased image with associated copy descriptors.
///
/// Port of `VideoCommon::AliasedImage`.
#[derive(Debug, Clone)]
pub struct AliasedImage {
    pub copies: Vec<ImageCopy>,
    pub id: ImageId,
}

// ── NullImageParams ────────────────────────────────────────────────────

/// Tag type for constructing a null/sentinel image.
pub struct NullImageParams;

// ── ImageBase ──────────────────────────────────────────────────────────

/// Base data for a cached GPU image.
///
/// Port of `VideoCommon::ImageBase`.
#[derive(Debug, Clone)]
pub struct ImageBase {
    pub info: ImageInfo,

    pub guest_size_bytes: u32,
    pub unswizzled_size_bytes: u32,
    pub converted_size_bytes: u32,
    pub scale_rating: u32,
    pub scale_tick: u64,
    pub has_scaled: bool,

    pub channel: usize,

    pub flags: ImageFlagBits,

    pub gpu_addr: GPUVAddr,
    pub cpu_addr: VAddr,
    pub cpu_addr_end: VAddr,

    pub modification_tick: u64,
    pub lru_index: usize,

    pub mip_level_offsets: [u32; MAX_MIP_LEVELS],

    pub image_view_infos: Vec<ImageViewInfo>,
    pub image_view_ids: Vec<ImageViewId>,

    pub slice_offsets: Vec<u32>,
    pub slice_subresources: Vec<SubresourceBase>,

    pub aliased_images: Vec<AliasedImage>,
    pub overlapping_images: Vec<ImageId>,
    pub map_view_id: ImageMapId,
}

impl ImageBase {
    /// Construct from image info and addresses.
    ///
    /// Port of `ImageBase::ImageBase(const ImageInfo&, GPUVAddr, VAddr)`.
    pub fn new(info: ImageInfo, gpu_addr: GPUVAddr, cpu_addr: VAddr) -> Self {
        // TODO: call calculate_guest_size_in_bytes, calculate_unswizzled_size_bytes,
        // calculate_converted_size_bytes, calculate_mip_level_offsets, etc.
        let guest_size_bytes = 0_u32; // placeholder
        Self {
            guest_size_bytes,
            unswizzled_size_bytes: 0,
            converted_size_bytes: 0,
            scale_rating: 0,
            scale_tick: 0,
            has_scaled: false,
            channel: 0,
            flags: ImageFlagBits::CPU_MODIFIED,
            gpu_addr,
            cpu_addr,
            cpu_addr_end: cpu_addr + guest_size_bytes as u64,
            modification_tick: 0,
            lru_index: usize::MAX,
            mip_level_offsets: [0; MAX_MIP_LEVELS],
            image_view_infos: Vec::new(),
            image_view_ids: Vec::new(),
            slice_offsets: if info.image_type == ImageType::E3D {
                Vec::new() // TODO: calculate_slice_offsets
            } else {
                Vec::new()
            },
            slice_subresources: if info.image_type == ImageType::E3D {
                Vec::new() // TODO: calculate_slice_subresources
            } else {
                Vec::new()
            },
            aliased_images: Vec::new(),
            overlapping_images: Vec::new(),
            map_view_id: ImageMapId::default(),
            info,
        }
    }

    /// Construct a null/sentinel image.
    ///
    /// Port of `ImageBase::ImageBase(const NullImageParams&)`.
    pub fn null(_: NullImageParams) -> Self {
        Self {
            info: ImageInfo::default(),
            guest_size_bytes: 0,
            unswizzled_size_bytes: 0,
            converted_size_bytes: 0,
            scale_rating: 0,
            scale_tick: 0,
            has_scaled: false,
            channel: 0,
            flags: ImageFlagBits::empty(),
            gpu_addr: 0,
            cpu_addr: 0,
            cpu_addr_end: 0,
            modification_tick: 0,
            lru_index: usize::MAX,
            mip_level_offsets: [0; MAX_MIP_LEVELS],
            image_view_infos: Vec::new(),
            image_view_ids: Vec::new(),
            slice_offsets: Vec::new(),
            slice_subresources: Vec::new(),
            aliased_images: Vec::new(),
            overlapping_images: Vec::new(),
            map_view_id: ImageMapId::default(),
        }
    }

    /// Try to find the base subresource at `other_addr`.
    ///
    /// Port of `ImageBase::TryFindBase`.
    pub fn try_find_base(&self, other_addr: GPUVAddr) -> Option<SubresourceBase> {
        if other_addr < self.gpu_addr {
            return None;
        }
        let diff = (other_addr - self.gpu_addr) as u32;
        if diff > self.guest_size_bytes {
            return None;
        }
        if self.info.image_type != ImageType::E3D {
            let (layer, mip_offset) = layer_mip_offset(diff as i32, self.info.layer_stride);
            let levels = self.info.resources.levels as usize;
            let it = self.mip_level_offsets[..levels]
                .iter()
                .position(|&o| o == mip_offset as u32);
            match it {
                Some(level) if layer <= self.info.resources.layers => Some(SubresourceBase {
                    level: level as i32,
                    layer,
                }),
                _ => None,
            }
        } else {
            let it = self.slice_offsets.iter().position(|&o| o == diff);
            it.map(|idx| self.slice_subresources[idx])
        }
    }

    /// Find an existing view matching `view_info`.
    ///
    /// Port of `ImageBase::FindView`.
    pub fn find_view(&self, view_info: &ImageViewInfo) -> ImageViewId {
        self.image_view_infos
            .iter()
            .position(|i| i == view_info)
            .map(|idx| self.image_view_ids[idx])
            .unwrap_or_default()
    }

    /// Insert a new view.
    ///
    /// Port of `ImageBase::InsertView`.
    pub fn insert_view(&mut self, view_info: ImageViewInfo, image_view_id: ImageViewId) {
        self.image_view_infos.push(view_info);
        self.image_view_ids.push(image_view_id);
    }

    /// Whether it is safe to download this image to the CPU.
    ///
    /// Port of `ImageBase::IsSafeDownload`.
    pub fn is_safe_download(&self) -> bool {
        if !self.flags.contains(ImageFlagBits::GPU_MODIFIED) {
            return false;
        }
        if self.flags.contains(ImageFlagBits::CPU_MODIFIED) {
            return false;
        }
        if self.info.num_samples > 1 {
            log::warn!("MSAA image downloads are not implemented");
            return false;
        }
        true
    }

    /// Whether this image overlaps a CPU address range.
    pub fn overlaps(&self, overlap_cpu_addr: VAddr, overlap_size: usize) -> bool {
        let overlap_end = overlap_cpu_addr + overlap_size as u64;
        self.cpu_addr < overlap_end && overlap_cpu_addr < self.cpu_addr_end
    }

    /// Whether this image overlaps a GPU address range.
    pub fn overlaps_gpu(&self, overlap_gpu_addr: GPUVAddr, overlap_size: usize) -> bool {
        let overlap_end = overlap_gpu_addr + overlap_size as u64;
        let gpu_addr_end = self.gpu_addr + self.guest_size_bytes as u64;
        self.gpu_addr < overlap_end && overlap_gpu_addr < gpu_addr_end
    }

    /// Port of `ImageBase::CheckBadOverlapState`.
    pub fn check_bad_overlap_state(&mut self) {
        if !self.flags.contains(ImageFlagBits::BAD_OVERLAP) {
            return;
        }
        if !self.overlapping_images.is_empty() {
            return;
        }
        self.flags.remove(ImageFlagBits::BAD_OVERLAP);
    }

    /// Port of `ImageBase::CheckAliasState`.
    pub fn check_alias_state(&mut self) {
        if !self.flags.contains(ImageFlagBits::ALIAS) {
            return;
        }
        if !self.aliased_images.is_empty() {
            return;
        }
        self.flags.remove(ImageFlagBits::ALIAS);
    }
}

// ── ImageMapView ───────────────────────────────────────────────────────

/// Describes a mapped region of an image in the GPU address space.
///
/// Port of `VideoCommon::ImageMapView`.
#[derive(Debug, Clone)]
pub struct ImageMapView {
    pub gpu_addr: GPUVAddr,
    pub cpu_addr: VAddr,
    pub size: usize,
    pub image_id: ImageId,
    pub picked: bool,
}

impl ImageMapView {
    pub fn new(gpu_addr: GPUVAddr, cpu_addr: VAddr, size: usize, image_id: ImageId) -> Self {
        Self {
            gpu_addr,
            cpu_addr,
            size,
            image_id,
            picked: false,
        }
    }

    pub fn overlaps(&self, overlap_cpu_addr: VAddr, overlap_size: usize) -> bool {
        let overlap_end = overlap_cpu_addr + overlap_size as u64;
        let cpu_addr_end = self.cpu_addr + self.size as u64;
        self.cpu_addr < overlap_end && overlap_cpu_addr < cpu_addr_end
    }

    pub fn overlaps_gpu(&self, overlap_gpu_addr: GPUVAddr, overlap_size: usize) -> bool {
        let overlap_end = overlap_gpu_addr + overlap_size as u64;
        let gpu_addr_end = self.gpu_addr + self.size as u64;
        self.gpu_addr < overlap_end && overlap_gpu_addr < gpu_addr_end
    }
}

// ── ImageAllocBase ─────────────────────────────────────────────────────

/// Base allocation record for an image.
///
/// Port of `VideoCommon::ImageAllocBase`.
#[derive(Debug, Clone, Default)]
pub struct ImageAllocBase {
    pub images: Vec<ImageId>,
}

// ── Private helpers ────────────────────────────────────────────────────

/// Returns (base_layer, mip_offset) from a byte offset and layer stride.
fn layer_mip_offset(diff: i32, layer_stride: u32) -> (i32, i32) {
    if layer_stride == 0 {
        (0, diff)
    } else {
        (diff / layer_stride as i32, diff % layer_stride as i32)
    }
}

// ── add_image_alias ────────────────────────────────────────────────────

/// Create bidirectional alias records between two images.
///
/// Port of `VideoCommon::AddImageAlias`.
/// TODO: Full implementation requires MipSize, FindSubresource,
/// DefaultBlockWidth/Height, DivCeil, ValidateCopy helpers.
pub fn add_image_alias(
    _lhs: &mut ImageBase,
    _rhs: &mut ImageBase,
    _lhs_id: ImageId,
    _rhs_id: ImageId,
) -> bool {
    todo!("add_image_alias — needs MipSize, FindSubresource, block helpers")
}
