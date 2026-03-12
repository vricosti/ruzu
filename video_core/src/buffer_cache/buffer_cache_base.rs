// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `video_core/buffer_cache/buffer_cache_base.h`
//!
//! Declares the types, constants, and structures used by the buffer cache.
//! The `BufferCache<P>` template is split between this file (data structures
//! and type definitions) and `buffer_cache.rs` (method implementations).

use common::slot_vector::SlotId;
use common::types::VAddr;

// ---------------------------------------------------------------------------
// Re-export BufferId
// ---------------------------------------------------------------------------

/// Identifier for a slot in the buffer cache's `SlotVector`.
pub type BufferId = SlotId;

// ---------------------------------------------------------------------------
// Constants — match upstream buffer_cache_base.h
// ---------------------------------------------------------------------------

/// Number of vertex buffer binding slots.
///
/// Upstream: 32 on non-Apple, 16 on Apple.
/// We default to 32 (no Apple target in this port).
pub const NUM_VERTEX_BUFFERS: u32 = 32;

/// Number of transform feedback buffer slots.
pub const NUM_TRANSFORM_FEEDBACK_BUFFERS: u32 = 4;

/// Number of uniform buffers per graphics stage.
pub const NUM_GRAPHICS_UNIFORM_BUFFERS: u32 = 18;

/// Number of uniform buffers for compute.
pub const NUM_COMPUTE_UNIFORM_BUFFERS: u32 = 8;

/// Number of storage buffer slots.
pub const NUM_STORAGE_BUFFERS: u32 = 16;

/// Number of texture buffer slots.
pub const NUM_TEXTURE_BUFFERS: u32 = 32;

/// Number of shader stages (vertex, tess_ctrl, tess_eval, geometry, fragment).
pub const NUM_STAGES: u32 = 5;

/// Uniform buffer sizes per stage (graphics).
pub type UniformBufferSizes = [[u32; NUM_GRAPHICS_UNIFORM_BUFFERS as usize]; NUM_STAGES as usize];

/// Uniform buffer sizes for compute.
pub type ComputeUniformBufferSizes = [u32; NUM_COMPUTE_UNIFORM_BUFFERS as usize];

// ---------------------------------------------------------------------------
// ObtainBuffer enums
// ---------------------------------------------------------------------------

/// Synchronization mode when obtaining a buffer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ObtainBufferSynchronize {
    NoSynchronize = 0,
    FullSynchronize = 1,
    SynchronizeNoDirty = 2,
}

/// Post-obtain operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ObtainBufferOperation {
    DoNothing = 0,
    MarkAsWritten = 1,
    DiscardWrite = 2,
    MarkQuery = 3,
}

// ---------------------------------------------------------------------------
// Null buffer sentinel
// ---------------------------------------------------------------------------

/// Sentinel `BufferId` representing the null buffer (slot 0).
pub const NULL_BUFFER_ID: BufferId = SlotId { index: 0 };

/// Default size threshold below which uniform buffer skip-cache is used (4 KiB).
pub const DEFAULT_SKIP_CACHE_SIZE: u32 = 4 * 1024;

// ---------------------------------------------------------------------------
// Binding structs
// ---------------------------------------------------------------------------

/// A binding from device address to buffer slot.
#[derive(Debug, Clone, Copy)]
pub struct Binding {
    /// Device address of the binding.
    pub device_addr: VAddr,
    /// Size of the binding in bytes.
    pub size: u32,
    /// Buffer slot that backs this binding.
    pub buffer_id: BufferId,
}

impl Default for Binding {
    fn default() -> Self {
        NULL_BINDING
    }
}

/// A texture buffer binding, which extends `Binding` with a pixel format.
#[derive(Debug, Clone, Copy)]
pub struct TextureBufferBinding {
    /// Device address of the binding.
    pub device_addr: VAddr,
    /// Size of the binding in bytes.
    pub size: u32,
    /// Buffer slot that backs this binding.
    pub buffer_id: BufferId,
    /// Pixel format of the texture buffer view.
    pub format: u32, // TODO: replace with PixelFormat enum once ported
}

impl Default for TextureBufferBinding {
    fn default() -> Self {
        Self {
            device_addr: 0,
            size: 0,
            buffer_id: NULL_BUFFER_ID,
            format: 0,
        }
    }
}

/// Sentinel null binding.
pub const NULL_BINDING: Binding = Binding {
    device_addr: 0,
    size: 0,
    buffer_id: NULL_BUFFER_ID,
};

// ---------------------------------------------------------------------------
// HostBindings
// ---------------------------------------------------------------------------

/// Collected host-side vertex buffer bindings ready for the backend.
///
/// Corresponds to the C++ `HostBindings<Buffer>` template struct.
pub struct HostBindings {
    /// Indices of bound buffers (backend-specific handles obtained separately).
    pub buffer_ids: Vec<BufferId>,
    /// Offsets within each buffer.
    pub offsets: Vec<u64>,
    /// Sizes of each binding.
    pub sizes: Vec<u64>,
    /// Strides for vertex buffers.
    pub strides: Vec<u64>,
    /// Minimum bound vertex buffer index.
    pub min_index: u32,
    /// Maximum bound vertex buffer index.
    pub max_index: u32,
}

impl Default for HostBindings {
    fn default() -> Self {
        Self {
            buffer_ids: Vec::new(),
            offsets: Vec::new(),
            sizes: Vec::new(),
            strides: Vec::new(),
            min_index: NUM_VERTEX_BUFFERS,
            max_index: 0,
        }
    }
}

// ---------------------------------------------------------------------------
// BufferCacheChannelInfo
// ---------------------------------------------------------------------------

/// Per-channel state for the buffer cache.
///
/// Corresponds to the C++ `BufferCacheChannelInfo` class.
pub struct BufferCacheChannelInfo {
    // -- Graphics bindings --
    pub index_buffer: Binding,
    pub vertex_buffers: [Binding; NUM_VERTEX_BUFFERS as usize],
    pub uniform_buffers: [[Binding; NUM_GRAPHICS_UNIFORM_BUFFERS as usize]; NUM_STAGES as usize],
    pub storage_buffers: [[Binding; NUM_STORAGE_BUFFERS as usize]; NUM_STAGES as usize],
    pub texture_buffers:
        [[TextureBufferBinding; NUM_TEXTURE_BUFFERS as usize]; NUM_STAGES as usize],
    pub transform_feedback_buffers: [Binding; NUM_TRANSFORM_FEEDBACK_BUFFERS as usize],
    pub count_buffer_binding: Binding,
    pub indirect_buffer_binding: Binding,

    // -- Compute bindings --
    pub compute_uniform_buffers: [Binding; NUM_COMPUTE_UNIFORM_BUFFERS as usize],
    pub compute_storage_buffers: [Binding; NUM_STORAGE_BUFFERS as usize],
    pub compute_texture_buffers: [TextureBufferBinding; NUM_TEXTURE_BUFFERS as usize],

    // -- Enabled masks --
    pub enabled_uniform_buffer_masks: [u32; NUM_STAGES as usize],
    pub enabled_compute_uniform_buffer_mask: u32,

    // -- Uniform buffer sizes (pointers into engine state) --
    // In Rust we use Option<&'static ...> or indices; for now use owned copies.
    pub uniform_buffer_sizes: Option<Box<UniformBufferSizes>>,
    pub compute_uniform_buffer_sizes: Option<Box<ComputeUniformBufferSizes>>,

    // -- Storage buffer masks --
    pub enabled_storage_buffers: [u32; NUM_STAGES as usize],
    pub written_storage_buffers: [u32; NUM_STAGES as usize],
    pub enabled_compute_storage_buffers: u32,
    pub written_compute_storage_buffers: u32,

    // -- Texture buffer masks --
    pub enabled_texture_buffers: [u32; NUM_STAGES as usize],
    pub written_texture_buffers: [u32; NUM_STAGES as usize],
    pub image_texture_buffers: [u32; NUM_STAGES as usize],
    pub enabled_compute_texture_buffers: u32,
    pub written_compute_texture_buffers: u32,
    pub image_compute_texture_buffers: u32,

    // -- Uniform cache statistics --
    pub uniform_cache_hits: [u32; 16],
    pub uniform_cache_shots: [u32; 16],

    /// Size threshold for uniform buffer skip-cache.
    pub uniform_buffer_skip_cache_size: u32,

    /// Whether any buffers were deleted this frame.
    pub has_deleted_buffers: bool,

    // -- Dirty / fast-bound tracking --
    pub dirty_uniform_buffers: [u32; NUM_STAGES as usize],
    pub fast_bound_uniform_buffers: [u32; NUM_STAGES as usize],
    pub uniform_buffer_binding_sizes:
        [[u32; NUM_GRAPHICS_UNIFORM_BUFFERS as usize]; NUM_STAGES as usize],
}

impl Default for BufferCacheChannelInfo {
    fn default() -> Self {
        Self {
            index_buffer: Binding::default(),
            vertex_buffers: [Binding::default(); NUM_VERTEX_BUFFERS as usize],
            uniform_buffers: [[Binding::default(); NUM_GRAPHICS_UNIFORM_BUFFERS as usize];
                NUM_STAGES as usize],
            storage_buffers: [[Binding::default(); NUM_STORAGE_BUFFERS as usize];
                NUM_STAGES as usize],
            texture_buffers: [[TextureBufferBinding::default(); NUM_TEXTURE_BUFFERS as usize];
                NUM_STAGES as usize],
            transform_feedback_buffers: [Binding::default();
                NUM_TRANSFORM_FEEDBACK_BUFFERS as usize],
            count_buffer_binding: Binding::default(),
            indirect_buffer_binding: Binding::default(),

            compute_uniform_buffers: [Binding::default(); NUM_COMPUTE_UNIFORM_BUFFERS as usize],
            compute_storage_buffers: [Binding::default(); NUM_STORAGE_BUFFERS as usize],
            compute_texture_buffers: [TextureBufferBinding::default();
                NUM_TEXTURE_BUFFERS as usize],

            enabled_uniform_buffer_masks: [0; NUM_STAGES as usize],
            enabled_compute_uniform_buffer_mask: 0,

            uniform_buffer_sizes: None,
            compute_uniform_buffer_sizes: None,

            enabled_storage_buffers: [0; NUM_STAGES as usize],
            written_storage_buffers: [0; NUM_STAGES as usize],
            enabled_compute_storage_buffers: 0,
            written_compute_storage_buffers: 0,

            enabled_texture_buffers: [0; NUM_STAGES as usize],
            written_texture_buffers: [0; NUM_STAGES as usize],
            image_texture_buffers: [0; NUM_STAGES as usize],
            enabled_compute_texture_buffers: 0,
            written_compute_texture_buffers: 0,
            image_compute_texture_buffers: 0,

            uniform_cache_hits: [0; 16],
            uniform_cache_shots: [0; 16],

            uniform_buffer_skip_cache_size: DEFAULT_SKIP_CACHE_SIZE,

            has_deleted_buffers: false,

            dirty_uniform_buffers: [0; NUM_STAGES as usize],
            fast_bound_uniform_buffers: [0; NUM_STAGES as usize],
            uniform_buffer_binding_sizes: [[0; NUM_GRAPHICS_UNIFORM_BUFFERS as usize];
                NUM_STAGES as usize],
        }
    }
}

// ---------------------------------------------------------------------------
// BufferCacheParams trait — the policy template parameter
// ---------------------------------------------------------------------------

/// Trait replacing the C++ `class P` template parameter.
///
/// Each rendering backend (OpenGL, Vulkan, Null) provides a concrete
/// implementation of this trait, supplying its buffer type, runtime,
/// and various capability flags.
pub trait BufferCacheParams {
    /// Whether this is the OpenGL backend.
    const IS_OPENGL: bool;
    /// Whether persistent uniform buffer bindings are supported.
    const HAS_PERSISTENT_UNIFORM_BUFFER_BINDINGS: bool;
    /// Whether all index formats and primitive topologies are natively supported.
    const HAS_FULL_INDEX_AND_PRIMITIVE_SUPPORT: bool;
    /// Whether uniform buffers must be bound by index.
    const NEEDS_BIND_UNIFORM_INDEX: bool;
    /// Whether storage buffers must be bound by index.
    const NEEDS_BIND_STORAGE_INDEX: bool;
    /// Whether memory-mapped uploads are used.
    const USE_MEMORY_MAPS: bool;
    /// Whether image buffer bindings are separate from texture buffer bindings.
    const SEPARATE_IMAGE_BUFFER_BINDINGS: bool;
    /// Whether memory maps are used for uploads.
    const USE_MEMORY_MAPS_FOR_UPLOADS: bool;
}

// ---------------------------------------------------------------------------
// BufferCopy — copy descriptor
// ---------------------------------------------------------------------------

/// Describes a single copy operation within a buffer.
#[derive(Debug, Clone, Copy, Default)]
pub struct BufferCopy {
    /// Source offset within the source buffer.
    pub src_offset: u64,
    /// Destination offset within the destination buffer.
    pub dst_offset: u64,
    /// Number of bytes to copy.
    pub size: u64,
}

// ---------------------------------------------------------------------------
// OverlapResult (private to BufferCache, but declared here for parity)
// ---------------------------------------------------------------------------

/// Result of resolving overlapping buffers for a new allocation.
pub struct OverlapResult {
    /// Buffer IDs that overlap the requested range.
    pub ids: Vec<BufferId>,
    /// Start of the merged range.
    pub begin: VAddr,
    /// End of the merged range.
    pub end: VAddr,
    /// Whether any overlapping buffer was a stream buffer.
    pub has_stream_leap: bool,
}

impl Default for OverlapResult {
    fn default() -> Self {
        Self {
            ids: Vec::new(),
            begin: 0,
            end: 0,
            has_stream_leap: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_null_binding() {
        assert_eq!(NULL_BINDING.device_addr, 0);
        assert_eq!(NULL_BINDING.size, 0);
        assert_eq!(NULL_BINDING.buffer_id, NULL_BUFFER_ID);
    }

    #[test]
    fn test_null_buffer_id() {
        assert_eq!(NULL_BUFFER_ID.index, 0);
    }

    #[test]
    fn test_channel_info_default() {
        let info = BufferCacheChannelInfo::default();
        assert_eq!(info.enabled_compute_uniform_buffer_mask, 0);
        assert!(!info.has_deleted_buffers);
        assert_eq!(info.uniform_buffer_skip_cache_size, DEFAULT_SKIP_CACHE_SIZE);
    }

    #[test]
    fn test_host_bindings_default() {
        let bindings = HostBindings::default();
        assert_eq!(bindings.min_index, NUM_VERTEX_BUFFERS);
        assert_eq!(bindings.max_index, 0);
        assert!(bindings.buffer_ids.is_empty());
    }
}
