// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_compute_pipeline.h and gl_compute_pipeline.cpp
//!
//! OpenGL compute pipeline management -- compiles and configures compute shaders.

use std::sync::{Condvar, Mutex};

use crate::engines::kepler_compute::DispatchCall;
use crate::texture_cache::texture_cache_base::ComputeDescriptorSyncRegs;

use super::gl_texture_cache::TextureCache;

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

/// OpenGL compute pipeline.
///
/// Corresponds to `OpenGL::ComputePipeline`.
pub struct ComputePipeline {
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
        _device: &super::gl_device::Device,
        _code: &str,
        _code_v: &[u32],
        _force_context_flush: bool,
    ) -> Self {
        // Full implementation would:
        // 1. Check device.get_shader_backend() (GLSL vs GLASM)
        // 2. Compile shader from source code
        // 3. Copy uniform buffer sizes from shader info
        // 4. Count texture/image buffer descriptors
        // 5. Determine use_storage_buffers, writes_global_memory, uses_local_memory
        Self {
            source_program: 0,
            assembly_program: 0,
            uniform_buffer_sizes: [0; 8],
            num_texture_buffers: 0,
            num_image_buffers: 0,
            use_storage_buffers: false,
            writes_global_memory: false,
            uses_local_memory: false,
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

    pub(crate) fn descriptor_sync_regs(dispatch: &DispatchCall) -> ComputeDescriptorSyncRegs {
        ComputeDescriptorSyncRegs {
            linked_tsc: dispatch.qmd.linked_tsc,
            tic_addr: dispatch.tic_address,
            tic_limit: dispatch.tic_limit,
            tsc_addr: dispatch.tsc_address,
            tsc_limit: dispatch.tsc_limit,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engines::kepler_compute::{DispatchCall, QueueMetaData};

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
}
