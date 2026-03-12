// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/texture_cache/types.h
//!
//! Fundamental types, constants, and data structures used throughout the
//! texture cache subsystem.

use common::slot_vector::SlotId;

// ── Constants ──────────────────────────────────────────────────────────

pub const NUM_RT: usize = 8;
pub const MAX_MIP_LEVELS: usize = 14;

pub const CORRUPT_ID: SlotId = SlotId { index: 0xffff_fffe };

// ── Slot‑ID type aliases ───────────────────────────────────────────────

pub type ImageId = SlotId;
pub type ImageMapId = SlotId;
pub type ImageViewId = SlotId;
pub type ImageAllocId = SlotId;
pub type SamplerId = SlotId;
pub type FramebufferId = SlotId;

/// Fake image ID for null image views
pub const NULL_IMAGE_ID: ImageId = SlotId { index: 0 };
/// Image view ID for null descriptors
pub const NULL_IMAGE_VIEW_ID: ImageViewId = SlotId { index: 0 };
/// Sampler ID for bugged sampler ids
pub const NULL_SAMPLER_ID: SamplerId = SlotId { index: 0 };

// ── Enumerations ───────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum ImageType {
    E1D = 0,
    E2D = 1,
    E3D = 2,
    Linear = 3,
    Buffer = 4,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum ImageViewType {
    E1D = 0,
    E2D = 1,
    Cube = 2,
    E3D = 3,
    E1DArray = 4,
    E2DArray = 5,
    CubeArray = 6,
    Rect = 7,
    Buffer = 8,
}

pub const NUM_IMAGE_VIEW_TYPES: usize = 9;

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct RelaxedOptions: u32 {
        const SIZE              = 1 << 0;
        const FORMAT            = 1 << 1;
        const SAMPLES           = 1 << 2;
        const FORCE_BROKEN_VIEWS = 1 << 3;
    }
}

// ── Geometry primitives ────────────────────────────────────────────────

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct Offset2D {
    pub x: i32,
    pub y: i32,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct Offset3D {
    pub x: i32,
    pub y: i32,
    pub z: i32,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct Region2D {
    pub start: Offset2D,
    pub end: Offset2D,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct Extent2D {
    pub width: u32,
    pub height: u32,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct Extent3D {
    pub width: u32,
    pub height: u32,
    pub depth: u32,
}

// ── Subresource descriptors ────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct SubresourceLayers {
    pub base_level: i32,
    pub base_layer: i32,
    pub num_layers: i32,
}

impl Default for SubresourceLayers {
    fn default() -> Self {
        Self {
            base_level: 0,
            base_layer: 0,
            num_layers: 1,
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct SubresourceBase {
    pub level: i32,
    pub layer: i32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct SubresourceExtent {
    pub levels: i32,
    pub layers: i32,
}

impl Default for SubresourceExtent {
    fn default() -> Self {
        Self {
            levels: 1,
            layers: 1,
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct SubresourceRange {
    pub base: SubresourceBase,
    pub extent: SubresourceExtent,
}

// ── Copy / transfer descriptors ────────────────────────────────────────

#[derive(Debug, Default, Clone, Copy)]
#[repr(C)]
pub struct ImageCopy {
    pub src_subresource: SubresourceLayers,
    pub dst_subresource: SubresourceLayers,
    pub src_offset: Offset3D,
    pub dst_offset: Offset3D,
    pub extent: Extent3D,
}

#[derive(Debug, Default, Clone, Copy)]
#[repr(C)]
pub struct BufferImageCopy {
    pub buffer_offset: usize,
    pub buffer_size: usize,
    pub buffer_row_length: u32,
    pub buffer_image_height: u32,
    pub image_subresource: SubresourceLayers,
    pub image_offset: Offset3D,
    pub image_extent: Extent3D,
}

#[derive(Debug, Default, Clone, Copy)]
#[repr(C)]
pub struct BufferCopy {
    pub src_offset: u64,
    pub dst_offset: u64,
    pub size: usize,
}

#[derive(Debug, Default, Clone, Copy)]
#[repr(C)]
pub struct SwizzleParameters {
    pub num_tiles: Extent3D,
    pub block: Extent3D,
    pub buffer_offset: usize,
    pub level: i32,
}
