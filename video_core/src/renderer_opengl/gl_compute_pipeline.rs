// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_compute_pipeline.h and gl_compute_pipeline.cpp
//!
//! OpenGL compute pipeline management -- compiles and configures compute shaders.

use std::sync::{Condvar, Mutex};

use crate::buffer_cache::buffer_cache::BufferCache;
use crate::buffer_cache::buffer_cache_base::BufferCacheParams;
use crate::buffer_cache::word_manager::DeviceTracker;
use crate::engines::kepler_compute::{DispatchCall, QueueMetaData};
use crate::texture_cache::texture_cache_base::{ComputeDescriptorSyncRegs, ImageViewInOut};
use crate::texture_cache::types::SamplerId;
use crate::textures::texture::texture_pair;

use super::gl_texture_cache::TextureCache;
use shader_recompiler::shader_info::{
    num_descriptors, ImageBufferDescriptor, ImageDescriptor, Info, TextureBufferDescriptor,
    TextureDescriptor,
};

/// Maximum number of textures bound to a compute pipeline.
pub const MAX_TEXTURES: u32 = 64;

/// Maximum number of images bound to a compute pipeline.
pub const MAX_IMAGES: u32 = 16;

/// Key used to identify a unique compute pipeline configuration.
///
/// Corresponds to `OpenGL::ComputePipelineKey`.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct ComputePipelineKey {
    pub unique_hash: u64,
    pub shared_memory_size: u32,
    pub workgroup_size: [u32; 3],
}

impl ComputePipelineKey {
    /// Hash the key using FNV-1a over the raw bytes.
    ///
    /// Port of the CityHash64 call in upstream (using FNV-1a as placeholder).
    pub fn hash_key(&self) -> u64 {
        let bytes: &[u8] = unsafe {
            std::slice::from_raw_parts(
                self as *const Self as *const u8,
                std::mem::size_of::<Self>(),
            )
        };
        let mut h: u64 = 0xcbf29ce484222325;
        for &b in bytes {
            h ^= b as u64;
            h = h.wrapping_mul(0x100000001b3);
        }
        h
    }
}

/// Uniform buffer sizes for compute pipelines.
///
/// Corresponds to `VideoCommon::ComputeUniformBufferSizes`.
pub type ComputeUniformBufferSizes = [u32; 8];

/// Host-side descriptors resolved by the texture/image part of
/// `ComputePipeline::Configure`.
#[derive(Debug, Clone, Default)]
pub struct ComputeTextureBindings {
    pub views: Vec<ImageViewInOut>,
    pub samplers: Vec<SamplerId>,
    pub num_texture_buffers: u32,
    pub num_image_buffers: u32,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct ComputeTextureHandles {
    pub views: Vec<ImageViewInOut>,
    pub sampler_indices: Vec<u32>,
    pub num_texture_buffers: u32,
    pub num_image_buffers: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ComputePipelineInfoState {
    uniform_buffer_sizes: ComputeUniformBufferSizes,
    num_texture_buffers: u32,
    num_image_buffers: u32,
    use_storage_buffers: bool,
    writes_global_memory: bool,
    uses_local_memory: bool,
}

/// OpenGL compute pipeline.
///
/// Corresponds to `OpenGL::ComputePipeline`.
pub struct ComputePipeline {
    /// Shader resource metadata copied into the pipeline.
    pub info: Info,
    /// Source program handle (GLSL or SPIR-V).
    pub source_program: u32,
    /// Assembly program handle (GLASM).
    pub assembly_program: u32,
    /// Uniform buffer sizes copied from shader info.
    pub uniform_buffer_sizes: ComputeUniformBufferSizes,

    /// Number of texture buffer descriptors.
    pub num_texture_buffers: u32,
    /// Number of image buffer descriptors.
    pub num_image_buffers: u32,

    /// Whether to use storage buffers (vs bindless).
    pub use_storage_buffers: bool,
    /// Whether any storage buffer descriptor is written.
    pub writes_global_memory: bool,
    /// Whether local memory is used.
    pub uses_local_memory: bool,

    // Build synchronization
    built_mutex: Mutex<bool>,
    built_condvar: Condvar,
    built_fence: u32,
    is_built: bool,
}

impl ComputePipeline {
    /// Create a new compute pipeline.
    ///
    /// Corresponds to `ComputePipeline::ComputePipeline()`.
    pub fn new(
        device: &super::gl_device::Device,
        code: &str,
        code_v: &[u32],
        force_context_flush: bool,
    ) -> Self {
        Self::new_with_info(device, Info::default(), code, code_v, force_context_flush)
    }

    /// Create a new compute pipeline with translated shader resource metadata.
    ///
    /// This is the Rust counterpart of upstream's constructor parameters
    /// `const Shader::Info& info_`, `std::string code`, and
    /// `std::vector<u32> code_v`.
    pub fn new_with_info(
        device: &super::gl_device::Device,
        info: Info,
        _code: &str,
        _code_v: &[u32],
        _force_context_flush: bool,
    ) -> Self {
        Self::new_with_backend_state(
            info,
            _code,
            _code_v,
            device.use_assembly_shaders(),
            device.max_glasm_storage_buffer_blocks(),
            _force_context_flush,
        )
    }

    /// Create a pipeline from the backend capability snapshot owned by
    /// `ShaderCache`. This keeps compute shader-cache creation in the
    /// OpenGL shader-cache owner until `ComputePipeline` stores the same
    /// upstream cache references as C++.
    pub(crate) fn new_with_backend_state(
        info: Info,
        _code: &str,
        _code_v: &[u32],
        use_assembly_shaders: bool,
        max_glasm_storage_buffer_blocks: u32,
        _force_context_flush: bool,
    ) -> Self {
        // Full implementation would compile `_code` / `_code_v` into
        // source_program or assembly_program. This slice ports selection and
        // shader-info ownership so resource configuration can consume `info`.
        let state = Self::info_state(&info, use_assembly_shaders, max_glasm_storage_buffer_blocks);

        Self {
            info,
            source_program: 0,
            assembly_program: 0,
            uniform_buffer_sizes: state.uniform_buffer_sizes,
            num_texture_buffers: state.num_texture_buffers,
            num_image_buffers: state.num_image_buffers,
            use_storage_buffers: state.use_storage_buffers,
            writes_global_memory: state.writes_global_memory,
            uses_local_memory: state.uses_local_memory,
            built_mutex: Mutex::new(false),
            built_condvar: Condvar::new(),
            built_fence: 0,
            is_built: true,
        }
    }

    #[cfg(test)]
    fn new_for_test(info: Info, is_glasm: bool, max_glasm_storage_buffer_blocks: u32) -> Self {
        let state = Self::info_state(&info, is_glasm, max_glasm_storage_buffer_blocks);
        Self {
            info,
            source_program: 0,
            assembly_program: 0,
            uniform_buffer_sizes: state.uniform_buffer_sizes,
            num_texture_buffers: state.num_texture_buffers,
            num_image_buffers: state.num_image_buffers,
            use_storage_buffers: state.use_storage_buffers,
            writes_global_memory: state.writes_global_memory,
            uses_local_memory: state.uses_local_memory,
            built_mutex: Mutex::new(false),
            built_condvar: Condvar::new(),
            built_fence: 0,
            is_built: true,
        }
    }

    /// Configure the compute pipeline for dispatch.
    ///
    /// Port of `ComputePipeline::Configure()`.
    ///
    /// In the full implementation, this:
    /// 1. Waits for async build if needed
    /// 2. Binds the program (source or assembly)
    /// 3. Fills uniform buffer descriptors
    /// 4. Fills storage buffer descriptors
    /// 5. Fills texture/image descriptors
    /// 6. Dispatches the compute shader
    pub fn configure(&mut self) {
        self.wait_for_build();

        if self.source_program != 0 {
            unsafe {
                gl::UseProgram(self.source_program);
            }
        }
        // Full implementation requires buffer_cache and texture_cache integration
    }

    /// Port of the `texture_cache.SynchronizeComputeDescriptors()` step at the
    /// start of upstream `ComputePipeline::Configure()`.
    ///
    /// Upstream reads `kepler_compute->launch_description.linked_tsc` and
    /// `kepler_compute->regs.{tic,tsc}` through the pipeline's current engine
    /// owner. Ruzu receives the launch-boundary `DispatchCall` snapshot from
    /// `KeplerCompute` until `ComputePipeline` stores the same engine pointer.
    pub fn synchronize_texture_descriptors(
        texture_cache: &mut TextureCache,
        dispatch: &DispatchCall,
    ) {
        texture_cache
            .base
            .synchronize_compute_descriptors(Self::descriptor_sync_regs(dispatch));
    }

    /// Port of the descriptor-handle collection at the start of upstream
    /// `ComputePipeline::Configure()`.
    ///
    /// This reads compute handles from QMD constant buffers, builds the
    /// `ImageViewInOut` list in the same texture-buffer, image-buffer,
    /// sampled-texture, storage-image order, resolves compute samplers, then
    /// calls the OpenGL texture-cache wrapper for `FillComputeImageViews`.
    pub fn prepare_texture_bindings(
        texture_cache: &mut TextureCache,
        info: &Info,
        dispatch: &DispatchCall,
        mut read_u32: impl FnMut(u64) -> u32,
    ) -> ComputeTextureBindings {
        let mut handles = Self::collect_texture_handles(info, dispatch, &mut read_u32);
        let samplers = handles
            .sampler_indices
            .iter()
            .map(|&index| texture_cache.base.get_compute_sampler_id(index))
            .collect();
        texture_cache.fill_compute_image_views(&mut handles.views);
        ComputeTextureBindings {
            views: handles.views,
            samplers,
            num_texture_buffers: handles.num_texture_buffers,
            num_image_buffers: handles.num_image_buffers,
        }
    }

    pub(crate) fn collect_texture_handles(
        info: &Info,
        dispatch: &DispatchCall,
        mut read_u32: impl FnMut(u64) -> u32,
    ) -> ComputeTextureHandles {
        let mut result = ComputeTextureHandles::default();
        let qmd = &dispatch.qmd;
        let via_header_index = qmd.linked_tsc;

        for desc in &info.texture_buffer_descriptors {
            for index in 0..desc.count {
                let (tic_index, _) =
                    Self::read_texture_handle(qmd, desc, index, via_header_index, &mut read_u32);
                result.views.push(ImageViewInOut {
                    index: tic_index,
                    ..Default::default()
                });
            }
        }
        result.num_texture_buffers = result.views.len() as u32;

        for desc in &info.image_buffer_descriptors {
            Self::add_image_handles(&mut result.views, qmd, desc, false, &mut read_u32);
        }
        result.num_image_buffers = result.views.len() as u32 - result.num_texture_buffers;

        for desc in &info.texture_descriptors {
            for index in 0..desc.count {
                let (tic_index, tsc_index) =
                    Self::read_texture_handle(qmd, desc, index, via_header_index, &mut read_u32);
                result.views.push(ImageViewInOut {
                    index: tic_index,
                    ..Default::default()
                });
                result.sampler_indices.push(tsc_index);
            }
        }

        for desc in &info.image_descriptors {
            Self::add_image_handles(&mut result.views, qmd, desc, desc.is_written, &mut read_u32);
        }

        result
    }

    fn info_state(
        info: &Info,
        is_glasm: bool,
        max_glasm_storage_buffer_blocks: u32,
    ) -> ComputePipelineInfoState {
        let mut uniform_buffer_sizes = [0; 8];
        uniform_buffer_sizes.copy_from_slice(&info.constant_buffer_used_sizes[..8]);

        let num_texture_buffers = num_descriptors(&info.texture_buffer_descriptors);
        let num_image_buffers = num_descriptors(&info.image_buffer_descriptors);
        let num_textures = num_texture_buffers + num_descriptors(&info.texture_descriptors);
        let num_images = num_image_buffers + num_descriptors(&info.image_descriptors);
        assert!(
            num_textures <= MAX_TEXTURES,
            "compute texture descriptor count {} exceeds MAX_TEXTURES {}",
            num_textures,
            MAX_TEXTURES
        );
        assert!(
            num_images <= MAX_IMAGES,
            "compute image descriptor count {} exceeds MAX_IMAGES {}",
            num_images,
            MAX_IMAGES
        );

        let num_storage_buffers = num_descriptors(&info.storage_buffers_descriptors);
        let use_storage_buffers =
            !is_glasm || num_storage_buffers < max_glasm_storage_buffer_blocks;
        let writes_global_memory = !use_storage_buffers
            && info
                .storage_buffers_descriptors
                .iter()
                .any(|desc| desc.is_written);

        ComputePipelineInfoState {
            uniform_buffer_sizes,
            num_texture_buffers,
            num_image_buffers,
            use_storage_buffers,
            writes_global_memory,
            uses_local_memory: info.uses_local_memory,
        }
    }

    pub(crate) fn descriptor_sync_regs(dispatch: &DispatchCall) -> ComputeDescriptorSyncRegs {
        ComputeDescriptorSyncRegs {
            linked_tsc: dispatch.qmd.linked_tsc,
            tic_addr: dispatch.tic_address,
            tic_limit: dispatch.tic_limit,
            tsc_addr: dispatch.tsc_address,
            tsc_limit: dispatch.tsc_limit,
        }
    }

    /// Port of the resource-state front half of upstream
    /// `ComputePipeline::Configure()`.
    ///
    /// This covers the ordering through `FillComputeImageViews`: compute UBO
    /// state, storage-buffer masks/bindings, compute descriptor
    /// synchronization, QMD handle collection, sampler-id resolution, and
    /// backend image-view preparation.
    pub fn configure_resource_state<P, DT>(
        &self,
        buffer_cache: &mut BufferCache<P, DT>,
        texture_cache: &mut TextureCache,
        dispatch: &DispatchCall,
        read_u32: impl FnMut(u64) -> u32,
    ) -> ComputeTextureBindings
    where
        P: BufferCacheParams,
        DT: DeviceTracker,
    {
        self.configure_buffer_state(buffer_cache);
        Self::synchronize_texture_descriptors(texture_cache, dispatch);
        self.prepare_texture_bindings_for_dispatch(texture_cache, dispatch, read_u32)
    }

    pub fn prepare_texture_bindings_for_dispatch(
        &self,
        texture_cache: &mut TextureCache,
        dispatch: &DispatchCall,
        read_u32: impl FnMut(u64) -> u32,
    ) -> ComputeTextureBindings {
        Self::prepare_texture_bindings(texture_cache, &self.info, dispatch, read_u32)
    }

    fn configure_buffer_state<P, DT>(&self, buffer_cache: &mut BufferCache<P, DT>)
    where
        P: BufferCacheParams,
        DT: DeviceTracker,
    {
        buffer_cache.set_compute_uniform_buffer_state(
            self.info.constant_buffer_mask,
            &self.uniform_buffer_sizes,
        );
        buffer_cache.unbind_compute_storage_buffers();
        for (ssbo_index, desc) in self.info.storage_buffers_descriptors.iter().enumerate() {
            assert_eq!(
                desc.count, 1,
                "ComputePipeline::Configure expects one storage-buffer descriptor per binding"
            );
            buffer_cache.bind_compute_storage_buffer(
                ssbo_index,
                desc.cbuf_index,
                desc.cbuf_offset,
                desc.is_written,
            );
        }
    }

    fn read_texture_handle(
        qmd: &QueueMetaData,
        desc: &impl ComputeTextureHandleDescriptor,
        index: u32,
        via_header_index: bool,
        read_u32: &mut impl FnMut(u64) -> u32,
    ) -> (u32, u32) {
        assert_compute_cbuf_enabled(qmd, desc.cbuf_index());
        let index_offset = index << desc.size_shift();
        let offset = desc.cbuf_offset().wrapping_add(index_offset);
        let addr = qmd.const_buffers[desc.cbuf_index() as usize]
            .address
            .wrapping_add(offset as u64);
        let raw = if desc.has_secondary() {
            assert_compute_cbuf_enabled(qmd, desc.secondary_cbuf_index());
            let secondary_offset = desc.secondary_cbuf_offset().wrapping_add(index_offset);
            let secondary_addr = qmd.const_buffers[desc.secondary_cbuf_index() as usize]
                .address
                .wrapping_add(secondary_offset as u64);
            (read_u32(addr) << desc.shift_left())
                | (read_u32(secondary_addr) << desc.secondary_shift_left())
        } else {
            read_u32(addr)
        };
        texture_pair(raw, via_header_index)
    }

    fn read_image_handle(
        qmd: &QueueMetaData,
        desc: &impl ComputeImageHandleDescriptor,
        index: u32,
        read_u32: &mut impl FnMut(u64) -> u32,
    ) -> u32 {
        assert_compute_cbuf_enabled(qmd, desc.cbuf_index());
        let index_offset = index << desc.size_shift();
        let offset = desc.cbuf_offset().wrapping_add(index_offset);
        let addr = qmd.const_buffers[desc.cbuf_index() as usize]
            .address
            .wrapping_add(offset as u64);
        read_u32(addr)
    }

    fn add_image_handles(
        views: &mut Vec<ImageViewInOut>,
        qmd: &QueueMetaData,
        desc: &impl ComputeImageHandleDescriptor,
        blacklist: bool,
        read_u32: &mut impl FnMut(u64) -> u32,
    ) {
        for index in 0..desc.count() {
            views.push(ImageViewInOut {
                index: Self::read_image_handle(qmd, desc, index, read_u32),
                blacklist,
                ..Default::default()
            });
        }
    }

    /// Returns whether any storage buffer descriptor is written.
    pub fn writes_global_memory(&self) -> bool {
        self.writes_global_memory
    }

    /// Returns whether local memory is used.
    pub fn uses_local_memory(&self) -> bool {
        self.uses_local_memory
    }

    /// Wait for the pipeline build to complete.
    ///
    /// Port of `ComputePipeline::WaitForBuild()`.
    fn wait_for_build(&mut self) {
        if self.is_built {
            return;
        }
        if self.built_fence != 0 {
            unsafe {
                let sync = gl::FenceSync(gl::SYNC_GPU_COMMANDS_COMPLETE, 0);
                if !sync.is_null() {
                    gl::ClientWaitSync(sync, gl::SYNC_FLUSH_COMMANDS_BIT, u64::MAX);
                    gl::DeleteSync(sync);
                }
            }
            self.is_built = true;
            return;
        }
        // Wait on condvar for async build thread
        let lock = self.built_mutex.lock().unwrap();
        let _guard = self
            .built_condvar
            .wait_while(lock, |built| !*built)
            .unwrap();
        self.is_built = true;
    }
}

fn assert_compute_cbuf_enabled(qmd: &QueueMetaData, cbuf_index: u32) {
    assert!(
        cbuf_index < qmd.const_buffers.len() as u32,
        "ComputePipeline::Configure descriptor cbuf index {} exceeds QMD const buffers",
        cbuf_index
    );
    assert!(
        ((qmd.const_buffer_enable_mask >> cbuf_index) & 1) != 0,
        "ComputePipeline::Configure descriptor cbuf {} is disabled",
        cbuf_index
    );
}

trait ComputeTextureHandleDescriptor {
    fn has_secondary(&self) -> bool;
    fn cbuf_index(&self) -> u32;
    fn cbuf_offset(&self) -> u32;
    fn shift_left(&self) -> u32;
    fn secondary_cbuf_index(&self) -> u32;
    fn secondary_cbuf_offset(&self) -> u32;
    fn secondary_shift_left(&self) -> u32;
    fn size_shift(&self) -> u32;
}

impl ComputeTextureHandleDescriptor for TextureBufferDescriptor {
    fn has_secondary(&self) -> bool {
        self.has_secondary
    }

    fn cbuf_index(&self) -> u32 {
        self.cbuf_index
    }

    fn cbuf_offset(&self) -> u32 {
        self.cbuf_offset
    }

    fn shift_left(&self) -> u32 {
        self.shift_left
    }

    fn secondary_cbuf_index(&self) -> u32 {
        self.secondary_cbuf_index
    }

    fn secondary_cbuf_offset(&self) -> u32 {
        self.secondary_cbuf_offset
    }

    fn secondary_shift_left(&self) -> u32 {
        self.secondary_shift_left
    }

    fn size_shift(&self) -> u32 {
        self.size_shift
    }
}

impl ComputeTextureHandleDescriptor for TextureDescriptor {
    fn has_secondary(&self) -> bool {
        self.has_secondary
    }

    fn cbuf_index(&self) -> u32 {
        self.cbuf_index
    }

    fn cbuf_offset(&self) -> u32 {
        self.cbuf_offset
    }

    fn shift_left(&self) -> u32 {
        self.shift_left
    }

    fn secondary_cbuf_index(&self) -> u32 {
        self.secondary_cbuf_index
    }

    fn secondary_cbuf_offset(&self) -> u32 {
        self.secondary_cbuf_offset
    }

    fn secondary_shift_left(&self) -> u32 {
        self.secondary_shift_left
    }

    fn size_shift(&self) -> u32 {
        self.size_shift
    }
}

trait ComputeImageHandleDescriptor {
    fn cbuf_index(&self) -> u32;
    fn cbuf_offset(&self) -> u32;
    fn count(&self) -> u32;
    fn size_shift(&self) -> u32;
}

impl ComputeImageHandleDescriptor for ImageBufferDescriptor {
    fn cbuf_index(&self) -> u32 {
        self.cbuf_index
    }

    fn cbuf_offset(&self) -> u32 {
        self.cbuf_offset
    }

    fn count(&self) -> u32 {
        self.count
    }

    fn size_shift(&self) -> u32 {
        self.size_shift
    }
}

impl ComputeImageHandleDescriptor for ImageDescriptor {
    fn cbuf_index(&self) -> u32 {
        self.cbuf_index
    }

    fn cbuf_offset(&self) -> u32 {
        self.cbuf_offset
    }

    fn count(&self) -> u32 {
        self.count
    }

    fn size_shift(&self) -> u32 {
        self.size_shift
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::buffer_cache::buffer_cache::BufferCache;
    use crate::buffer_cache::buffer_cache_base::{BufferCacheChannelInfo, BufferCacheParams};
    use crate::buffer_cache::word_manager::DeviceTracker;
    use crate::engines::kepler_compute::{DispatchCall, QmdConstBuffer, QueueMetaData};
    use shader_recompiler::shader_info::{
        ImageBufferDescriptor, ImageDescriptor, ImageFormat, StorageBufferDescriptor,
        TextureBufferDescriptor, TextureDescriptor, TextureType,
    };

    struct DummyTracker;

    impl DeviceTracker for DummyTracker {
        fn update_pages_cached_count(&self, _addr: u64, _size: u64, _delta: i32) {}
    }

    struct TestParams;

    impl BufferCacheParams for TestParams {
        const IS_OPENGL: bool = false;
        const HAS_PERSISTENT_UNIFORM_BUFFER_BINDINGS: bool = false;
        const HAS_FULL_INDEX_AND_PRIMITIVE_SUPPORT: bool = true;
        const NEEDS_BIND_UNIFORM_INDEX: bool = false;
        const NEEDS_BIND_STORAGE_INDEX: bool = false;
        const USE_MEMORY_MAPS: bool = false;
        const SEPARATE_IMAGE_BUFFER_BINDINGS: bool = false;
        const USE_MEMORY_MAPS_FOR_UPLOADS: bool = false;
    }

    #[test]
    fn compute_pipeline_key_hash() {
        let key = ComputePipelineKey {
            unique_hash: 0x1234,
            shared_memory_size: 1024,
            workgroup_size: [32, 1, 1],
        };
        let h1 = key.hash_key();
        let h2 = key.hash_key();
        assert_eq!(h1, h2);

        let key2 = ComputePipelineKey {
            unique_hash: 0x5678,
            shared_memory_size: 1024,
            workgroup_size: [32, 1, 1],
        };
        assert_ne!(key.hash_key(), key2.hash_key());
    }

    #[test]
    fn compute_pipeline_key_size() {
        assert_eq!(
            std::mem::size_of::<ComputePipelineKey>(),
            8 + 4 + 12 // u64 + u32 + 3*u32
        );
    }

    #[test]
    fn descriptor_sync_regs_come_from_dispatch_call() {
        let mut qmd = QueueMetaData::default();
        qmd.linked_tsc = true;
        let dispatch = DispatchCall {
            qmd,
            qmd_address: 0x1000,
            code_address: 0x2000,
            tsc_address: 0x3000,
            tsc_limit: 1,
            tic_address: 0x4000,
            tic_limit: 6,
            tex_cb_index: 0,
        };

        let regs = ComputePipeline::descriptor_sync_regs(&dispatch);

        assert!(regs.linked_tsc);
        assert_eq!(regs.tic_addr, 0x4000);
        assert_eq!(regs.tic_limit, 6);
        assert_eq!(regs.tsc_addr, 0x3000);
        assert_eq!(regs.tsc_limit, 1);
    }

    #[test]
    fn compute_pipeline_info_state_matches_upstream_constructor_metadata() {
        let mut info = Info::default();
        info.constant_buffer_used_sizes[0] = 0x10;
        info.constant_buffer_used_sizes[7] = 0x80;
        info.constant_buffer_used_sizes[8] = 0x90;
        info.texture_buffer_descriptors
            .push(TextureBufferDescriptor {
                has_secondary: false,
                cbuf_index: 0,
                cbuf_offset: 0,
                shift_left: 0,
                secondary_cbuf_index: 0,
                secondary_cbuf_offset: 0,
                secondary_shift_left: 0,
                count: 2,
                size_shift: 2,
            });
        info.texture_buffer_descriptors
            .push(TextureBufferDescriptor {
                has_secondary: false,
                cbuf_index: 0,
                cbuf_offset: 0,
                shift_left: 0,
                secondary_cbuf_index: 0,
                secondary_cbuf_offset: 0,
                secondary_shift_left: 0,
                count: 3,
                size_shift: 2,
            });
        info.image_buffer_descriptors.push(ImageBufferDescriptor {
            format: ImageFormat::R32Uint,
            is_written: false,
            is_read: true,
            is_integer: true,
            cbuf_index: 0,
            cbuf_offset: 0,
            count: 4,
            size_shift: 2,
        });
        info.texture_descriptors.push(TextureDescriptor {
            texture_type: TextureType::Color2D,
            is_depth: false,
            is_multisample: false,
            has_secondary: false,
            cbuf_index: 0,
            cbuf_offset: 0,
            shift_left: 0,
            secondary_cbuf_index: 0,
            secondary_cbuf_offset: 0,
            secondary_shift_left: 0,
            count: 6,
            size_shift: 2,
        });
        info.image_descriptors.push(ImageDescriptor {
            texture_type: TextureType::Color2D,
            format: ImageFormat::R32Uint,
            is_written: false,
            is_read: true,
            is_integer: true,
            cbuf_index: 0,
            cbuf_offset: 0,
            count: 7,
            size_shift: 2,
        });
        info.storage_buffers_descriptors
            .push(StorageBufferDescriptor {
                cbuf_index: 0,
                cbuf_offset: 0x20,
                count: 2,
                is_written: true,
            });
        info.uses_local_memory = true;

        let glsl_state = ComputePipeline::info_state(&info, false, 0);
        assert_eq!(
            glsl_state.uniform_buffer_sizes,
            [0x10, 0, 0, 0, 0, 0, 0, 0x80]
        );
        assert_eq!(glsl_state.num_texture_buffers, 5);
        assert_eq!(glsl_state.num_image_buffers, 4);
        assert!(glsl_state.use_storage_buffers);
        assert!(!glsl_state.writes_global_memory);
        assert!(glsl_state.uses_local_memory);

        let glasm_state_without_capacity = ComputePipeline::info_state(&info, true, 2);
        assert!(!glasm_state_without_capacity.use_storage_buffers);
        assert!(glasm_state_without_capacity.writes_global_memory);

        let glasm_state_with_capacity = ComputePipeline::info_state(&info, true, 3);
        assert!(glasm_state_with_capacity.use_storage_buffers);
        assert!(!glasm_state_with_capacity.writes_global_memory);
    }

    #[test]
    fn configure_buffer_state_follows_upstream_compute_order() {
        let mut info = Info::default();
        info.constant_buffer_mask = 0b101;
        info.constant_buffer_used_sizes[0] = 0x20;
        info.constant_buffer_used_sizes[2] = 0x40;
        info.storage_buffers_descriptors
            .push(StorageBufferDescriptor {
                cbuf_index: 0,
                cbuf_offset: 0x100,
                count: 1,
                is_written: false,
            });
        info.storage_buffers_descriptors
            .push(StorageBufferDescriptor {
                cbuf_index: 2,
                cbuf_offset: 0x200,
                count: 1,
                is_written: true,
            });

        let pipeline = ComputePipeline::new_for_test(info, false, 0);
        let tracker = DummyTracker;
        let mut buffer_cache = BufferCache::<TestParams, DummyTracker>::new(&tracker);
        buffer_cache.channel_state = Some(Box::new(BufferCacheChannelInfo::default()));
        {
            let cs = buffer_cache.channel_state.as_mut().unwrap();
            cs.enabled_compute_storage_buffers = 0xFFFF;
            cs.written_compute_storage_buffers = 0xFFFF;
        }

        pipeline.configure_buffer_state(&mut buffer_cache);

        let cs = buffer_cache.channel_state.as_ref().unwrap();
        assert_eq!(cs.enabled_compute_uniform_buffer_mask, 0b101);
        assert_eq!(
            *cs.compute_uniform_buffer_sizes.as_ref().unwrap().as_ref(),
            [0x20, 0, 0x40, 0, 0, 0, 0, 0]
        );
        assert_eq!(cs.enabled_compute_storage_buffers, 0b11);
        assert_eq!(cs.written_compute_storage_buffers, 0b10);
    }

    #[test]
    fn collect_texture_handles_follows_upstream_compute_order_and_pairs() {
        let mut info = Info::default();
        info.texture_buffer_descriptors
            .push(TextureBufferDescriptor {
                has_secondary: false,
                cbuf_index: 0,
                cbuf_offset: 0,
                shift_left: 0,
                secondary_cbuf_index: 0,
                secondary_cbuf_offset: 0,
                secondary_shift_left: 0,
                count: 1,
                size_shift: 2,
            });
        info.image_buffer_descriptors.push(ImageBufferDescriptor {
            format: ImageFormat::R32Uint,
            is_written: false,
            is_read: true,
            is_integer: true,
            cbuf_index: 0,
            cbuf_offset: 4,
            count: 1,
            size_shift: 2,
        });
        info.texture_descriptors.push(TextureDescriptor {
            texture_type: TextureType::Color2D,
            is_depth: false,
            is_multisample: false,
            has_secondary: true,
            cbuf_index: 0,
            cbuf_offset: 8,
            shift_left: 0,
            secondary_cbuf_index: 1,
            secondary_cbuf_offset: 0,
            secondary_shift_left: 20,
            count: 2,
            size_shift: 2,
        });
        info.image_descriptors.push(ImageDescriptor {
            texture_type: TextureType::Color2D,
            format: ImageFormat::R32Uint,
            is_written: true,
            is_read: false,
            is_integer: true,
            cbuf_index: 0,
            cbuf_offset: 16,
            count: 1,
            size_shift: 2,
        });

        let mut qmd = QueueMetaData::default();
        qmd.linked_tsc = false;
        qmd.const_buffer_enable_mask = 0b11;
        qmd.const_buffers[0] = QmdConstBuffer {
            address: 0x1000,
            size: 0x100,
        };
        qmd.const_buffers[1] = QmdConstBuffer {
            address: 0x2000,
            size: 0x100,
        };
        let dispatch = DispatchCall {
            qmd,
            qmd_address: 0,
            code_address: 0,
            tsc_address: 0,
            tsc_limit: 0,
            tic_address: 0,
            tic_limit: 0,
            tex_cb_index: 0,
        };

        let handles =
            ComputePipeline::collect_texture_handles(&info, &dispatch, |addr| match addr {
                0x1000 => 0x0000_0011,
                0x1004 => 0x0000_0022,
                0x1008 => 0x0000_0033,
                0x100c => 0x0000_0044,
                0x1010 => 0x0000_0055,
                0x2000 => 0x0000_0007,
                0x2004 => 0x0000_0008,
                _ => panic!("unexpected read at 0x{addr:X}"),
            });

        assert_eq!(handles.num_texture_buffers, 1);
        assert_eq!(handles.num_image_buffers, 1);
        assert_eq!(handles.sampler_indices, vec![7, 8]);
        assert_eq!(
            handles
                .views
                .iter()
                .map(|view| view.index)
                .collect::<Vec<_>>(),
            vec![0x11, 0x22, 0x33, 0x44, 0x55]
        );
        assert_eq!(
            handles
                .views
                .iter()
                .map(|view| view.blacklist)
                .collect::<Vec<_>>(),
            vec![false, false, false, false, true]
        );
    }
}
