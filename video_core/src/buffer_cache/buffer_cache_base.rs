// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `video_core/buffer_cache/buffer_cache_base.h`
//!
//! Declares the types, constants, and structures used by the buffer cache.
//! The `BufferCache<P>` template is split between this file (data structures
//! and type definitions) and `buffer_cache.rs` (method implementations).

use common::slot_vector::SlotId;
use common::slot_vector::SlotVector;
use common::types::VAddr;

use super::buffer_base::BufferBase;

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
    /// Upstream: `PixelFormat format;` — stored as u32 here because callers pass
    /// raw format IDs from the GPU engine which are u32 throughout the buffer cache.
    pub format: u32,
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

// ---------------------------------------------------------------------------
// StagingBufferRef — staging buffer allocation handle
// ---------------------------------------------------------------------------

/// A reference to a staging buffer allocation.
///
/// Upstream: `StagingBufferRef` (Vulkan) / `StagingBufferMap` (OpenGL).
/// This is a backend-agnostic handle returned by `BufferCacheRuntime::upload_staging_buffer`
/// and `BufferCacheRuntime::download_staging_buffer`.
pub struct StagingBufferRef {
    /// Opaque buffer handle (backend interprets this as a buffer ID for copy operations).
    pub buffer: BufferId,
    /// Backend-native buffer handle used by runtimes whose staging buffers do not
    /// live in the generic slot vector.
    pub gpu_handle: u32,
    /// Offset within the staging buffer.
    pub offset: u64,
    /// Index in the backend staging allocation pool.
    pub index: usize,
    mapped_ptr: *mut u8,
    mapped_size: usize,
    sync: *mut gl::types::GLsync,
    host_mapping: Option<Vec<u8>>,
}

impl StagingBufferRef {
    /// Host-backed fallback used by non-GL test runtimes.
    pub fn host(size: usize) -> Self {
        let mut host_mapping = vec![0u8; size];
        let mapped_ptr = host_mapping.as_mut_ptr();
        Self {
            buffer: BufferId::invalid(),
            gpu_handle: 0,
            offset: 0,
            index: usize::MAX,
            mapped_ptr,
            mapped_size: size,
            sync: std::ptr::null_mut(),
            host_mapping: Some(host_mapping),
        }
    }

    /// Backend-backed mapping. The caller owns the lifetime contract for
    /// `mapped_ptr` and `sync`, matching upstream `StagingBufferMap`.
    pub unsafe fn from_mapped_backend(
        buffer: BufferId,
        gpu_handle: u32,
        offset: u64,
        index: usize,
        mapped_ptr: *mut u8,
        mapped_size: usize,
        sync: *mut gl::types::GLsync,
    ) -> Self {
        Self {
            buffer,
            gpu_handle,
            offset,
            index,
            mapped_ptr,
            mapped_size,
            sync,
            host_mapping: None,
        }
    }

    pub fn mapped_span(&self) -> &[u8] {
        if let Some(host_mapping) = &self.host_mapping {
            return host_mapping;
        }
        if self.mapped_size == 0 {
            return &[];
        }
        assert!(
            !self.mapped_ptr.is_null(),
            "staging buffer has a non-zero mapped size but no mapped pointer"
        );
        unsafe { std::slice::from_raw_parts(self.mapped_ptr, self.mapped_size) }
    }

    pub fn mapped_span_mut(&mut self) -> &mut [u8] {
        if let Some(host_mapping) = &mut self.host_mapping {
            return host_mapping;
        }
        if self.mapped_size == 0 {
            return &mut [];
        }
        assert!(
            !self.mapped_ptr.is_null(),
            "staging buffer has a non-zero mapped size but no mapped pointer"
        );
        unsafe { std::slice::from_raw_parts_mut(self.mapped_ptr, self.mapped_size) }
    }
}

impl Drop for StagingBufferRef {
    fn drop(&mut self) {
        if self.sync.is_null() {
            return;
        }
        unsafe {
            if !(*self.sync).is_null() {
                gl::DeleteSync(*self.sync);
            }
            *self.sync = gl::FenceSync(gl::SYNC_GPU_COMMANDS_COMPLETE, 0);
        }
    }
}

// ---------------------------------------------------------------------------
// BufferCacheRuntime trait — the Runtime template parameter interface
// ---------------------------------------------------------------------------

/// Trait replacing the C++ `Runtime` type parameter used by `BufferCache<P>`.
///
/// Each rendering backend (OpenGL, Vulkan, Null) provides a concrete
/// implementation of this trait. The upstream C++ uses duck-typing via the
/// template parameter `P::Runtime`; in Rust we formalize the interface as a trait.
///
/// Method signatures are derived from the union of methods called on `runtime`
/// in upstream `buffer_cache.h` (template method implementations).
pub trait BufferCacheRuntime {
    /// Initialize backend-specific buffer state after the backend handle is
    /// created.
    ///
    /// Upstream OpenGL performs this in `OpenGL::Buffer::Buffer(runtime, ...)`,
    /// including bindless GPU-address discovery.
    fn initialize_backend_buffer(&mut self, _buffer: &mut BufferBase) {}

    // -- Frame lifecycle --

    /// Called once per frame to allow the runtime to reclaim resources.
    ///
    /// Upstream: `Runtime::TickFrame(SlotVector<Buffer>&)`
    fn tick_frame(&mut self);

    /// Whether the runtime can report actual device memory usage.
    ///
    /// Upstream: `Runtime::CanReportMemoryUsage()`
    fn can_report_memory_usage(&self) -> bool;

    /// Return the amount of device-local memory available.
    ///
    /// Upstream: `Runtime::GetDeviceLocalMemory()`
    fn get_device_local_memory(&self) -> u64;

    /// Return current device memory usage in bytes.
    ///
    /// Upstream: `Runtime::GetDeviceMemoryUsage()`
    fn get_device_memory_usage(&self) -> u64;

    /// Return the alignment requirement for storage buffer offsets.
    ///
    /// Upstream: `Runtime::GetStorageBufferAlignment()`
    fn get_storage_buffer_alignment(&self) -> u32;

    /// Wait for all pending GPU operations to complete.
    ///
    /// Upstream: `Runtime::Finish()`
    fn finish(&mut self);

    // -- Staging buffers --

    /// Allocate a staging buffer for CPU→GPU upload.
    ///
    /// Upstream: `Runtime::UploadStagingBuffer(size)`
    fn upload_staging_buffer(&mut self, size: u64) -> StagingBufferRef;

    /// Allocate a staging buffer for GPU→CPU download.
    ///
    /// Upstream: `Runtime::DownloadStagingBuffer(size, deferred)`
    fn download_staging_buffer(&mut self, size: u64, deferred: bool) -> StagingBufferRef;

    /// Free a deferred staging buffer.
    ///
    /// Upstream: `Runtime::FreeDeferredStagingBuffer(ref)`
    fn free_deferred_staging_buffer(&mut self, buffer: &mut StagingBufferRef);

    /// Whether uploads to `buffer` with given `copies` can be reordered.
    ///
    /// Upstream: `Runtime::CanReorderUpload(buffer, copies)`
    fn can_reorder_upload(&self, buffer_id: BufferId, copies: &[BufferCopy]) -> bool;

    // -- Copy / Clear --

    /// Insert a barrier before a batch of copy operations.
    ///
    /// Upstream: `Runtime::PreCopyBarrier()`
    fn pre_copy_barrier(&mut self);

    /// Insert a barrier after a batch of copy operations.
    ///
    /// Upstream: `Runtime::PostCopyBarrier()`
    fn post_copy_barrier(&mut self);

    /// Copy data between two buffers.
    ///
    /// Upstream: `Runtime::CopyBuffer(dst, src, copies, barrier, can_reorder_upload)`
    fn copy_buffer(
        &mut self,
        dst_buffer: BufferId,
        dst_gpu_handle: u32,
        src_buffer: BufferId,
        src_gpu_handle: u32,
        copies: &[BufferCopy],
        barrier: bool,
        can_reorder_upload: bool,
    );

    /// Clear a buffer region to a uniform value.
    ///
    /// Upstream: `Runtime::ClearBuffer(buffer, offset, size, value)`
    fn clear_buffer(
        &mut self,
        buffer: BufferId,
        gpu_handle: u32,
        offset: u32,
        size: u64,
        value: u32,
    );

    // -- Index buffer binding --

    /// Bind an index buffer for draw calls.
    ///
    /// Upstream: `Runtime::BindIndexBuffer(buffer, offset, size)`.
    /// `gpu_handle` is the backend buffer name (GL buffer name / Vulkan
    /// buffer handle) extracted from the slot vector by the caller. Upstream
    /// receives the whole `Buffer&` object; the Rust port passes the handle
    /// separately because `BufferBase` is backend-agnostic.
    fn bind_index_buffer(&mut self, buffer: BufferId, gpu_handle: u32, offset: u32, size: u32);

    /// Return the backend index offset consumed by the rasterizer draw call.
    ///
    /// Upstream OpenGL exposes this as `BufferCacheRuntime::IndexOffset()`.
    fn index_offset(&self) -> usize {
        0
    }

    // -- Vertex buffer binding --

    /// Bind vertex buffers collected in `HostBindings`.
    ///
    /// Upstream: `Runtime::BindVertexBuffers(host_bindings)`.
    /// `buffers` provides the backend buffer objects referenced by
    /// `bindings.buffer_ids`, matching upstream's `HostBindings<Buffer>`.
    fn bind_vertex_buffers(
        &mut self,
        bindings: &HostBindings,
        buffers: &mut SlotVector<BufferBase>,
    );

    // -- Uniform buffer binding (graphics) --

    /// Bind a graphics-stage uniform buffer.
    ///
    /// Upstream (OpenGL): `Runtime::BindUniformBuffer(stage, binding_index, buffer, offset, size)`
    /// Upstream (Vulkan): `Runtime::BindUniformBuffer(buffer, offset, size)`
    fn bind_uniform_buffer(
        &mut self,
        stage: usize,
        binding_index: u32,
        buffer: BufferId,
        gpu_handle: u32,
        offset: u32,
        size: u32,
    );

    /// Set per-stage base uniform binding points.
    ///
    /// Upstream OpenGL stores this on `BufferCacheRuntime` and adds the base
    /// to each stage-local uniform binding index.
    fn set_base_uniform_bindings(&mut self, _bindings: &[u32; NUM_STAGES as usize]) {}

    /// Set per-stage base storage binding points.
    ///
    /// Upstream OpenGL stores this on `BufferCacheRuntime` and adds the base
    /// to each stage-local storage binding index.
    fn set_base_storage_bindings(&mut self, _bindings: &[u32; NUM_STAGES as usize]) {}

    /// Set the output arrays used by OpenGL texture/image-buffer binding.
    ///
    /// Upstream: `BufferCacheRuntime::SetImagePointers(GLuint*, GLuint*)`.
    /// `BindHostStageBuffers` writes TBO/image-buffer texture names through
    /// these pointers before the pipeline bulk-calls `glBindTextures` /
    /// `glBindImageTextures`.
    fn set_image_pointers(&mut self, _texture_handles: *mut u32, _image_handles: *mut u32) {}

    /// Select GL SSBO binding mode for graphics/compute storage buffers.
    ///
    /// Upstream OpenGL uses real `GL_SHADER_STORAGE_BUFFER` bindings for GLSL
    /// and bindless program-local parameters for GLASM when necessary.
    fn set_enable_storage_buffers(&mut self, _enable: bool) {}

    // -- Storage buffer binding (graphics) --

    /// Bind a graphics-stage storage buffer.
    ///
    /// Upstream (OpenGL): `Runtime::BindStorageBuffer(stage, binding_index, buffer, offset, size, is_written)`
    /// Upstream (Vulkan): `Runtime::BindStorageBuffer(buffer, offset, size, is_written)`
    fn bind_storage_buffer(
        &mut self,
        stage: usize,
        binding_index: u32,
        buffer: &mut BufferBase,
        offset: u32,
        size: u32,
        is_written: bool,
    );

    // -- Texture / Image buffer binding --

    /// Bind a texture buffer view.
    ///
    /// Upstream: `Runtime::BindTextureBuffer(buffer, offset, size, format)`
    fn bind_texture_buffer(
        &mut self,
        buffer: BufferId,
        gpu_handle: u32,
        offset: u32,
        size: u32,
        format: u32,
    );

    /// Bind an image buffer view (separate from texture on some backends).
    ///
    /// Upstream: `Runtime::BindImageBuffer(buffer, offset, size, format)`
    fn bind_image_buffer(
        &mut self,
        buffer: BufferId,
        gpu_handle: u32,
        offset: u32,
        size: u32,
        format: u32,
    );

    // -- Transform feedback --

    /// Bind transform feedback buffers.
    ///
    /// Upstream: `Runtime::BindTransformFeedbackBuffers(host_bindings)`
    fn bind_transform_feedback_buffers(&mut self, bindings: &HostBindings);

    // -- Compute buffer binding --

    /// Bind a compute-stage uniform buffer.
    ///
    /// Upstream: `Runtime::BindComputeUniformBuffer(binding_index, buffer, offset, size)`
    fn bind_compute_uniform_buffer(
        &mut self,
        binding_index: u32,
        buffer: BufferId,
        offset: u32,
        size: u32,
    );

    /// Bind a compute-stage storage buffer.
    ///
    /// Upstream: `Runtime::BindComputeStorageBuffer(binding_index, buffer, offset, size, is_written)`
    fn bind_compute_storage_buffer(
        &mut self,
        binding_index: u32,
        buffer: &mut BufferBase,
        offset: u32,
        size: u32,
        is_written: bool,
    );

    // -- OpenGL-specific fast uniform buffer path --

    /// Whether the runtime supports `glBufferSubData`-like fast path.
    ///
    /// Upstream (OpenGL): `Runtime::HasFastBufferSubData()`
    fn has_fast_buffer_sub_data(&self) -> bool {
        false
    }

    /// Whether non-zero uniform buffer offsets are supported.
    ///
    /// Upstream (OpenGL): `Runtime::SupportsNonZeroUniformOffset()`
    fn supports_non_zero_uniform_offset(&self) -> bool {
        true
    }

    /// Bind a fast uniform buffer (OpenGL assembly shader path).
    ///
    /// Upstream (OpenGL): `Runtime::BindFastUniformBuffer(stage, binding_index, size)`
    fn bind_fast_uniform_buffer(&mut self, _stage: usize, _binding_index: u32, _size: u32) {}

    /// Push data into a fast uniform buffer (OpenGL path).
    ///
    /// Upstream (OpenGL): `Runtime::PushFastUniformBuffer(stage, binding_index, data)`
    fn push_fast_uniform_buffer(&mut self, _stage: usize, _binding_index: u32, _data: &[u8]) {}

    /// Bind a mapped uniform buffer and copy uniform data into it.
    ///
    /// Upstream (OpenGL): `Runtime::BindMappedUniformBuffer(stage, binding_index, size)`
    fn bind_mapped_uniform_buffer(
        &mut self,
        _stage: usize,
        _binding_index: u32,
        _data: &[u8],
    ) -> bool {
        false
    }
}

// ---------------------------------------------------------------------------
// GpuMemoryAccess trait — GPU address translation
// ---------------------------------------------------------------------------

/// Trait for GPU virtual address translation and memory reads.
///
/// Upstream: these operations are performed via `gpu_memory` (a `Tegra::MemoryManager*`)
/// which is set per-channel. The buffer cache calls:
/// - `GpuToCpuAddress(gpu_addr)` — translate GPU VA to device/CPU address
/// - `Read<T>(gpu_addr)` — read a typed value from GPU VA space
/// - `IsWithinGPUAddressRange(gpu_addr)` — bounds check
/// - `MaxContinuousRange(gpu_addr, size)` — find max mapped range
/// - `GetMemoryLayoutSize(gpu_addr)` — get mapped size from an address
pub trait GpuMemoryAccess {
    /// Translate a GPU virtual address to a device (CPU) address.
    ///
    /// Upstream: `gpu_memory->GpuToCpuAddress(gpu_addr)`
    fn gpu_to_cpu_address(&self, gpu_addr: u64) -> Option<u64>;

    /// Read a `u64` from GPU virtual address space.
    ///
    /// Upstream: `gpu_memory->Read<u64>(gpu_addr)`
    fn read_u64(&self, gpu_addr: u64) -> Option<u64>;

    /// Read a `u32` from GPU virtual address space.
    ///
    /// Upstream: `gpu_memory->Read<u32>(gpu_addr)`
    fn read_u32(&self, gpu_addr: u64) -> Option<u32>;

    /// Check if a GPU address is within the valid address range.
    ///
    /// Upstream: `gpu_memory->IsWithinGPUAddressRange(gpu_addr)`
    fn is_within_gpu_address_range(&self, gpu_addr: u64) -> bool;

    /// Return the maximum continuous mapped range from `gpu_addr`.
    ///
    /// Upstream: `gpu_memory->MaxContinuousRange(gpu_addr, size)`
    fn max_continuous_range(&self, gpu_addr: u64, size: u64) -> u64;

    /// Return the total mapped size starting from a GPU address.
    ///
    /// Upstream: `gpu_memory->GetMemoryLayoutSize(gpu_addr)`
    fn get_memory_layout_size(&self, gpu_addr: u64) -> u64;
}

// ---------------------------------------------------------------------------
// DeviceMemoryAccess trait — host CPU memory read/write
// ---------------------------------------------------------------------------

/// Trait for reading/writing guest physical (device) memory.
///
/// Upstream: these operations are performed via `device_memory`
/// (a `Tegra::MaxwellDeviceMemoryManager&`).
pub trait DeviceMemoryAccess {
    /// Get a pointer to guest memory at `device_addr`.
    ///
    /// Upstream: `device_memory.GetPointer<u8>(device_addr)`
    /// Returns None if the address is not directly accessible.
    fn get_pointer(&self, device_addr: u64) -> Option<*const u8>;

    /// Read a block of bytes from guest memory.
    ///
    /// Upstream: `device_memory.ReadBlockUnsafe(device_addr, dst, size)`
    fn read_block_unsafe(&self, device_addr: u64, dst: &mut [u8]);

    /// Write a block of bytes to guest memory.
    ///
    /// Upstream: `device_memory.WriteBlockUnsafe(device_addr, src, size)`
    fn write_block_unsafe(&self, device_addr: u64, src: &[u8]);
}

// ---------------------------------------------------------------------------
// DrawIndirectParams — draw indirect state
// ---------------------------------------------------------------------------

/// Parameters for indirect draw calls.
///
/// Upstream: `Tegra::Engines::DrawManager::IndirectParams`
#[derive(Debug, Clone, Copy)]
pub struct DrawIndirectParams {
    /// GPU address of the indirect buffer.
    pub indirect_start_address: u64,
    /// GPU address of the count buffer.
    pub count_start_address: u64,
    /// Total size of the indirect buffer in bytes.
    pub buffer_size: u64,
    /// Maximum number of draw calls.
    pub max_draw_counts: u32,
    /// Stride between draw commands.
    pub stride: u32,
    /// Whether to include a count buffer.
    pub include_count: bool,
}

// ---------------------------------------------------------------------------
// ConstBufferInfo — per-stage constant buffer binding from engine state
// ---------------------------------------------------------------------------

/// Information about a bound constant buffer from the Maxwell3D engine.
///
/// Upstream: `Tegra::Engines::Maxwell3D::State::ConstBufferInfo`
#[derive(Debug, Clone, Copy, Default)]
pub struct ConstBufferInfo {
    /// GPU virtual address of the constant buffer.
    pub address: u64,
    /// Size of the constant buffer in bytes.
    pub size: u32,
    /// Whether the constant buffer is enabled.
    pub enabled: bool,
}

/// Information needed from the compute engine's launch description.
///
/// Upstream: `kepler_compute->launch_description`
#[derive(Debug, Clone)]
pub struct ComputeLaunchInfo {
    /// Bitmask of enabled constant buffers.
    pub const_buffer_enable_mask: u32,
    /// Constant buffer configurations (address + size).
    pub const_buffer_config: Vec<ComputeConstBufferConfig>,
}

/// A single compute constant buffer configuration.
///
/// Upstream: `Tegra::Engines::KeplerCompute::LaunchDescription::ConstBufferConfig`
#[derive(Debug, Clone, Copy, Default)]
pub struct ComputeConstBufferConfig {
    /// GPU virtual address of the constant buffer.
    pub address: u64,
    /// Size in bytes.
    pub size: u32,
}

/// Index buffer reference from the draw state.
///
/// Upstream: `maxwell3d->draw_manager->GetDrawState().index_buffer`
#[derive(Debug, Clone, Copy, Default)]
pub struct IndexBufferRef {
    /// GPU virtual address of the start of the index buffer.
    pub start_address: u64,
    /// GPU virtual address of the end of the index buffer.
    pub end_address: u64,
    /// Number of indices.
    pub count: u32,
    /// First index offset.
    pub first: u32,
    /// Bytes per index element (1, 2, or 4).
    pub format_size_in_bytes: u32,
}

/// Vertex stream register from Maxwell3D.
///
/// Upstream: `maxwell3d->regs.vertex_streams[index]`
#[derive(Debug, Clone, Copy, Default)]
pub struct VertexStreamInfo {
    /// GPU virtual address (computed from address_high:address_low).
    pub address: u64,
    /// Stride in bytes.
    pub stride: u32,
    /// Whether the stream is enabled (0 = disabled).
    pub enable: u32,
}

/// Vertex stream limit from Maxwell3D.
///
/// Upstream: `maxwell3d->regs.vertex_stream_limits[index]`
#[derive(Debug, Clone, Copy, Default)]
pub struct VertexStreamLimit {
    /// GPU virtual address of the end of the vertex buffer + 1.
    pub address: u64,
}

/// Transform feedback buffer binding from Maxwell3D.
///
/// Upstream: `maxwell3d->regs.transform_feedback.buffers[index]`
#[derive(Debug, Clone, Copy, Default)]
pub struct TransformFeedbackBufferInfo {
    /// GPU virtual address.
    pub address: u64,
    /// Start offset within the buffer.
    pub start_offset: u32,
    /// Size of the transform feedback buffer in bytes.
    pub size: u32,
    /// Whether this buffer is enabled (0 = disabled).
    pub enable: u32,
}

/// Aggregated engine state snapshot used by the buffer cache.
///
/// The buffer cache reads engine state at specific points during buffer updates.
/// Rather than holding a reference to the engine, callers provide this snapshot.
pub trait EngineState {
    // -- Index buffer --

    /// Return the current index buffer reference from the draw state.
    fn get_index_buffer(&self) -> IndexBufferRef;

    /// Return the inline index draw data, if any.
    fn get_inline_index_draw_indexes(&self) -> &[u8];

    // -- Dirty flags --

    /// Check if a dirty flag is set.
    fn is_dirty(&self, flag: DirtyFlag) -> bool;

    /// Clear a dirty flag.
    fn clear_dirty(&mut self, flag: DirtyFlag);

    // -- Vertex streams --

    /// Return vertex stream info for a given index.
    fn get_vertex_stream(&self, index: u32) -> VertexStreamInfo;

    /// Return vertex stream limit for a given index.
    fn get_vertex_stream_limit(&self, index: u32) -> VertexStreamLimit;

    // -- Transform feedback --

    /// Whether transform feedback is enabled.
    fn is_transform_feedback_enabled(&self) -> bool;

    /// Return transform feedback buffer info for a given index.
    fn get_transform_feedback_buffer(&self, index: u32) -> TransformFeedbackBufferInfo;

    // -- Const buffers (graphics) --

    /// Return const buffer info for a shader stage and cbuf index.
    fn get_const_buffer(&self, stage: usize, cbuf_index: u32) -> ConstBufferInfo;

    // -- Compute launch description --

    /// Return compute launch info (const buffer enable mask + configs).
    fn get_compute_launch_info(&self) -> ComputeLaunchInfo;
}

/// Dirty flags used by the buffer cache.
///
/// Upstream: `VideoCommon::Dirty::*` flags used in `maxwell3d->dirty.flags[]`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DirtyFlag {
    IndexBuffer,
    VertexBuffers,
    VertexBuffer(u32), // VertexBuffer0 + index
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
