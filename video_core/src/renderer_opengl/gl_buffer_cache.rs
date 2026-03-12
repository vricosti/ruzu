// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_buffer_cache.h and gl_buffer_cache.cpp
//!
//! OpenGL buffer cache -- manages GPU buffer objects for vertex, index, uniform, and storage
//! buffer access.

/// NV program stage LUT for assembly shader parameter buffer bindings.
///
/// Corresponds to `BufferCacheRuntime::PABO_LUT` in gl_buffer_cache.h.
const PABO_LUT: [u32; 5] = [
    0x8DA2, // GL_VERTEX_PROGRAM_PARAMETER_BUFFER_NV
    0x8DA3, // GL_TESS_CONTROL_PROGRAM_PARAMETER_BUFFER_NV
    0x8DA4, // GL_TESS_EVALUATION_PROGRAM_PARAMETER_BUFFER_NV
    0x8DA5, // GL_GEOMETRY_PROGRAM_PARAMETER_BUFFER_NV
    0x8DA6, // GL_FRAGMENT_PROGRAM_PARAMETER_BUFFER_NV
];

/// NV program stage LUT for bindless SSBO.
const PROGRAM_LUT: [u32; 5] = [
    0x8620, // GL_VERTEX_PROGRAM_NV
    0x891E, // GL_TESS_CONTROL_PROGRAM_NV
    0x891F, // GL_TESS_EVALUATION_PROGRAM_NV
    0x8C26, // GL_GEOMETRY_PROGRAM_NV
    0x8870, // GL_FRAGMENT_PROGRAM_NV
];

/// Number of graphics uniform buffers per stage.
pub const NUM_GRAPHICS_UNIFORM_BUFFERS: usize = 18;

/// Number of compute uniform buffers.
pub const NUM_COMPUTE_UNIFORM_BUFFERS: usize = 8;

/// Number of shader stages.
pub const NUM_STAGES: usize = 5;

/// Bindless SSBO descriptor layout.
///
/// Corresponds to the anonymous `BindlessSSBO` struct in gl_buffer_cache.cpp.
#[repr(C)]
struct BindlessSSBO {
    address: u64,
    length: i32,
    padding: i32,
}

/// A single buffer view used for texture buffer access.
struct BufferView {
    offset: u32,
    size: u32,
    format: u32,
    texture: u32,
}

/// An OpenGL buffer object tracked by the buffer cache.
///
/// Corresponds to `OpenGL::Buffer`.
pub struct Buffer {
    pub handle: u32,
    address: u64,
    current_residency_access: u32,
    views: Vec<BufferView>,
    cpu_addr: u64,
    size_bytes: u64,
}

impl Buffer {
    /// Create a new buffer.
    ///
    /// Port of `Buffer::Buffer(BufferCacheRuntime&, DAddr, u64)`.
    pub fn new(cpu_addr: u64, size_bytes: u64) -> Self {
        let mut handle: u32 = 0;
        unsafe {
            gl::CreateBuffers(1, &mut handle);
            gl::NamedBufferData(
                handle,
                size_bytes as isize,
                std::ptr::null(),
                gl::DYNAMIC_DRAW,
            );
        }

        Self {
            handle,
            address: 0,
            current_residency_access: gl::NONE,
            views: Vec::new(),
            cpu_addr,
            size_bytes,
        }
    }

    /// Create a null buffer.
    pub fn null() -> Self {
        Self {
            handle: 0,
            address: 0,
            current_residency_access: gl::NONE,
            views: Vec::new(),
            cpu_addr: 0,
            size_bytes: 0,
        }
    }

    /// Upload data to the buffer immediately.
    ///
    /// Port of `Buffer::ImmediateUpload`.
    pub fn immediate_upload(&self, offset: usize, data: &[u8]) {
        if self.handle == 0 || data.is_empty() {
            return;
        }
        unsafe {
            gl::NamedBufferSubData(
                self.handle,
                offset as isize,
                data.len() as isize,
                data.as_ptr() as *const _,
            );
        }
    }

    /// Download data from the buffer immediately.
    ///
    /// Port of `Buffer::ImmediateDownload`.
    pub fn immediate_download(&self, offset: usize, data: &mut [u8]) {
        if self.handle == 0 || data.is_empty() {
            return;
        }
        unsafe {
            gl::GetNamedBufferSubData(
                self.handle,
                offset as isize,
                data.len() as isize,
                data.as_mut_ptr() as *mut _,
            );
        }
    }

    /// Make the buffer resident for NV unified memory.
    ///
    /// Port of `Buffer::MakeResident`.
    pub fn make_resident(&mut self, access: u32) {
        if self.address == 0 || self.current_residency_access == access {
            return;
        }
        // GL_NV_shader_buffer_load
        if self.current_residency_access != gl::NONE {
            // glMakeNamedBufferNonResidentNV(handle)
        }
        self.current_residency_access = access;
        // glMakeNamedBufferResidentNV(handle, access)
    }

    /// Get or create a texture buffer view.
    ///
    /// Port of `Buffer::View`.
    pub fn view(&mut self, offset: u32, size: u32, format: u32) -> u32 {
        // Check for existing view
        for v in &self.views {
            if v.offset == offset && v.size == size && v.format == format {
                return v.texture;
            }
        }
        // Create new texture buffer view
        let mut texture: u32 = 0;
        unsafe {
            gl::CreateTextures(gl::TEXTURE_BUFFER, 1, &mut texture);
            gl::TextureBufferRange(
                texture,
                format,
                self.handle,
                offset as isize,
                size as isize,
            );
        }
        self.views.push(BufferView {
            offset,
            size,
            format,
            texture,
        });
        texture
    }

    /// Get the host GPU address (NV unified memory).
    pub fn host_gpu_addr(&self) -> u64 {
        self.address
    }

    /// CPU address of the buffer.
    pub fn cpu_addr(&self) -> u64 {
        self.cpu_addr
    }

    /// Size of the buffer in bytes.
    pub fn size_bytes(&self) -> u64 {
        self.size_bytes
    }
}

impl Drop for Buffer {
    fn drop(&mut self) {
        unsafe {
            for v in &self.views {
                if v.texture != 0 {
                    gl::DeleteTextures(1, &v.texture);
                }
            }
            if self.handle != 0 {
                gl::DeleteBuffers(1, &self.handle);
            }
        }
    }
}

/// Runtime state for the OpenGL buffer cache.
///
/// Corresponds to `OpenGL::BufferCacheRuntime`.
pub struct BufferCacheRuntime {
    pub has_fast_buffer_sub_data: bool,
    pub use_assembly_shaders: bool,
    pub has_unified_vertex_buffers: bool,
    pub use_storage_buffers: bool,
    pub max_attributes: u32,

    pub graphics_base_uniform_bindings: [u32; NUM_STAGES],
    pub graphics_base_storage_bindings: [u32; NUM_STAGES],

    pub index_buffer_offset: u32,
    pub device_access_memory: u64,
}

impl BufferCacheRuntime {
    /// Create a new buffer cache runtime.
    ///
    /// Port of `BufferCacheRuntime::BufferCacheRuntime()`.
    pub fn new(_device: &super::gl_device::Device) -> Self {
        let mut max_attributes: i32 = 16;
        unsafe {
            gl::GetIntegerv(gl::MAX_VERTEX_ATTRIBS, &mut max_attributes);
        }

        Self {
            has_fast_buffer_sub_data: false,
            use_assembly_shaders: false,
            has_unified_vertex_buffers: false,
            use_storage_buffers: false,
            max_attributes: max_attributes as u32,
            graphics_base_uniform_bindings: [0; NUM_STAGES],
            graphics_base_storage_bindings: [0; NUM_STAGES],
            index_buffer_offset: 0,
            device_access_memory: 2 * 1024 * 1024 * 1024, // 2 GiB default
        }
    }

    /// Set base uniform bindings for graphics stages.
    pub fn set_base_uniform_bindings(&mut self, bindings: &[u32; NUM_STAGES]) {
        self.graphics_base_uniform_bindings = *bindings;
    }

    /// Set base storage bindings for graphics stages.
    pub fn set_base_storage_bindings(&mut self, bindings: &[u32; NUM_STAGES]) {
        self.graphics_base_storage_bindings = *bindings;
    }

    /// Set whether to use storage buffers.
    pub fn set_enable_storage_buffers(&mut self, enable: bool) {
        self.use_storage_buffers = enable;
    }

    /// Pre-copy memory barrier.
    pub fn pre_copy_barrier(&self) {
        unsafe {
            gl::MemoryBarrier(gl::ALL_BARRIER_BITS);
        }
    }

    /// Post-copy memory barrier.
    pub fn post_copy_barrier(&self) {
        unsafe {
            gl::MemoryBarrier(gl::BUFFER_UPDATE_BARRIER_BIT | gl::CLIENT_MAPPED_BUFFER_BARRIER_BIT);
        }
    }

    /// Finish all pending GL operations.
    pub fn finish(&self) {
        unsafe {
            gl::Finish();
        }
    }

    /// Get device memory usage.
    pub fn get_device_memory_usage(&self) -> u64 {
        // GL_GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX = 0x9049
        const GL_GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX: u32 = 0x9049;
        let mut available_kb: i32 = 0;
        unsafe {
            gl::GetIntegerv(
                GL_GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX,
                &mut available_kb,
            );
        }
        if available_kb > 0 {
            self.device_access_memory - (available_kb as u64 * 1024)
        } else {
            0
        }
    }

    /// Get device local memory.
    pub fn get_device_local_memory(&self) -> u64 {
        self.device_access_memory
    }

    /// Whether non-zero uniform offsets are supported.
    pub fn supports_non_zero_uniform_offset(&self) -> bool {
        !self.use_assembly_shaders
    }

    /// Has fast buffer sub data extension.
    pub fn has_fast_buffer_sub_data(&self) -> bool {
        self.has_fast_buffer_sub_data
    }

    /// Index offset for element array draws.
    pub fn index_offset(&self) -> usize {
        self.index_buffer_offset as usize
    }
}

/// Buffer cache parameters matching upstream `BufferCacheParams`.
pub struct BufferCacheParams;

impl BufferCacheParams {
    pub const IS_OPENGL: bool = true;
    pub const HAS_PERSISTENT_UNIFORM_BUFFER_BINDINGS: bool = true;
    pub const HAS_FULL_INDEX_AND_PRIMITIVE_SUPPORT: bool = true;
    pub const NEEDS_BIND_UNIFORM_INDEX: bool = true;
    pub const NEEDS_BIND_STORAGE_INDEX: bool = true;
    pub const USE_MEMORY_MAPS: bool = true;
    pub const SEPARATE_IMAGE_BUFFER_BINDINGS: bool = true;
    pub const USE_MEMORY_MAPS_FOR_UPLOADS: bool = false;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pabo_lut_size() {
        assert_eq!(PABO_LUT.len(), 5);
        assert_eq!(PROGRAM_LUT.len(), 5);
    }

    #[test]
    fn constants() {
        assert_eq!(NUM_GRAPHICS_UNIFORM_BUFFERS, 18);
        assert_eq!(NUM_COMPUTE_UNIFORM_BUFFERS, 8);
        assert_eq!(NUM_STAGES, 5);
    }

    #[test]
    fn buffer_cache_params() {
        assert!(BufferCacheParams::IS_OPENGL);
        assert!(BufferCacheParams::HAS_PERSISTENT_UNIFORM_BUFFER_BINDINGS);
        assert!(BufferCacheParams::USE_MEMORY_MAPS);
    }

    #[test]
    fn bindless_ssbo_layout() {
        assert_eq!(std::mem::size_of::<BindlessSSBO>(), 16);
    }
}
