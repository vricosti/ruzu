// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_graphics_pipeline.h and gl_graphics_pipeline.cpp
//!
//! OpenGL graphics pipeline management -- compiles and configures vertex/fragment/etc shaders.

use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Condvar, Mutex};

use crate::buffer_cache::buffer_cache::BufferCache;
use crate::buffer_cache::buffer_cache_base::{BufferCacheParams, EngineState, UniformBufferSizes};
use crate::buffer_cache::word_manager::DeviceTracker;
use crate::engines::draw_manager::Maxwell3DRenderTargets;
use crate::engines::maxwell_3d::SurfaceClipInfo;
use crate::engines::maxwell_3d::{ConstBufferBinding, MAX_CB_SLOTS};
use crate::renderer_opengl::gl_shader_manager::ProgramManager;
use crate::renderer_opengl::gl_shader_util::program_local_parameter_4f_arb;
use crate::renderer_opengl::gl_state_tracker::StateTracker;
use crate::renderer_opengl::gl_texture_cache::{
    RenderTargetDirtyFlagAccess, TextureCache as OpenGLTextureCache,
};
use crate::texture_cache::texture_cache_base::{DescriptorSyncRegs, ImageViewInOut};
use crate::texture_cache::types::{Extent2D, ImageViewId, SamplerId};
use crate::textures::texture::texture_pair;
use crate::transform_feedback::TransformFeedbackState;
use common::{cityhash::city_hash64, settings, trace};
use shader_recompiler::shader_info::{num_descriptors, Info as ShaderInfo};

/// Maximum number of textures bound to a graphics pipeline.
pub const MAX_TEXTURES: u32 = 64;

/// Maximum number of images bound to a graphics pipeline.
pub const MAX_IMAGES: u32 = 8;

/// Number of shader stages (vertex, tess control, tess eval, geometry, fragment).
pub const NUM_STAGES: usize = 5;

/// Number of transform feedback buffers.
pub const NUM_TRANSFORM_FEEDBACK_BUFFERS: usize = 4;

/// Stride of each XFB attribute entry (token, count, attrib).
pub const XFB_ENTRY_STRIDE: usize = 3;

static GLSL_ERROR_DUMP_COUNTER: AtomicU64 = AtomicU64::new(0);
static GL_PIPELINE_BUILD_TRACE_SEQ: AtomicU64 = AtomicU64::new(0);

macro_rules! trace_gl_pipeline_stall {
    ($($arg:tt)*) => {
        if std::env::var_os("RUZU_TRACE_GL_PIPELINE_STALL").is_some() {
            eprintln!($($arg)*);
        }
    };
}

/// Diagnostic snapshot for one sampled-texture descriptor resolved by
/// `GraphicsPipeline::bind_stage_sampled_textures`.
pub struct SampledTextureBinding {
    pub stage: usize,
    pub texture_binding: usize,
    pub stage_texture_binding: u32,
    pub view_id: ImageViewId,
    pub handle: u32,
    pub texture_type: u32,
    pub is_depth: bool,
    pub is_multisample: bool,
}

/// Transient texture/sampler/image binding arrays used while configuring one graphics draw.
///
/// Owns the Rust counterpart of upstream `GraphicsPipeline::ConfigureImpl` locals:
/// `texture_binding`, `image_binding`, `sampler_binding`,
/// `std::array<GLuint, MAX_TEXTURES> textures`,
/// `std::array<GLuint, MAX_IMAGES> images`, and
/// `std::array<GLuint, MAX_TEXTURES> gl_samplers`.
pub struct GraphicsTextureImageBindingState {
    pub textures: [u32; MAX_TEXTURES as usize],
    pub samplers: [u32; MAX_TEXTURES as usize],
    pub bound_texture_view_ids: [ImageViewId; MAX_TEXTURES as usize],
    pub images: [u32; MAX_IMAGES as usize],
    pub texture_binding: usize,
    pub sampler_binding: usize,
    pub image_binding: usize,
    pub sampler_it: usize,
    pub views_it: usize,
}

impl GraphicsTextureImageBindingState {
    pub fn new() -> Self {
        Self {
            textures: [0; MAX_TEXTURES as usize],
            samplers: [0; MAX_TEXTURES as usize],
            bound_texture_view_ids: [crate::texture_cache::types::NULL_IMAGE_VIEW_ID;
                MAX_TEXTURES as usize],
            images: [0; MAX_IMAGES as usize],
            texture_binding: 0,
            sampler_binding: 0,
            image_binding: 0,
            sampler_it: 0,
            views_it: 0,
        }
    }
}

fn trace_pipeline_build(stage: u64, key: &GraphicsPipelineKey, aux0: u64, aux1: u64, aux2: u64) {
    if !trace::is_enabled(trace::cat::GL_PIPELINE) {
        return;
    }
    let seq = GL_PIPELINE_BUILD_TRACE_SEQ.fetch_add(1, Ordering::Relaxed);
    let _ = trace::emit_raw(
        trace::cat::GL_PIPELINE,
        &[
            stage,
            seq,
            0,
            key.raw as u64,
            key.hash_key(),
            key.unique_hashes[0],
            key.unique_hashes[1],
            key.unique_hashes[2],
            key.unique_hashes[3],
            key.unique_hashes[4],
            key.unique_hashes[5],
            aux0,
            aux1,
            aux2,
        ],
    );
}

/// Key used to identify a unique graphics pipeline configuration.
///
/// Corresponds to `OpenGL::GraphicsPipelineKey`.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[repr(C)]
pub struct GraphicsPipelineKey {
    pub unique_hashes: [u64; 6],
    /// Packed bitfield: xfb_enabled(1), early_z(1), gs_input_topology(4),
    /// tessellation_primitive(2), tessellation_spacing(2), tessellation_clockwise(1),
    /// app_stage(3), alpha_test_func(3).
    pub raw: u32,
    pub alpha_test_ref: u32,
    pub padding: [u32; 2],
    pub xfb_state: TransformFeedbackState,
}

impl GraphicsPipelineKey {
    const XFB_ENABLED_SHIFT: u32 = 0;
    const EARLY_Z_SHIFT: u32 = 1;
    const GS_INPUT_TOPOLOGY_SHIFT: u32 = 2;
    const TESSELLATION_PRIMITIVE_SHIFT: u32 = 6;
    const TESSELLATION_SPACING_SHIFT: u32 = 8;
    const TESSELLATION_CLOCKWISE_SHIFT: u32 = 10;
    const APP_STAGE_SHIFT: u32 = 11;
    const ALPHA_TEST_FUNC_SHIFT: u32 = 14;

    const XFB_ENABLED_MASK: u32 = 0x1 << Self::XFB_ENABLED_SHIFT;
    const EARLY_Z_MASK: u32 = 0x1 << Self::EARLY_Z_SHIFT;
    const GS_INPUT_TOPOLOGY_MASK: u32 = 0xF << Self::GS_INPUT_TOPOLOGY_SHIFT;
    const TESSELLATION_PRIMITIVE_MASK: u32 = 0x3 << Self::TESSELLATION_PRIMITIVE_SHIFT;
    const TESSELLATION_SPACING_MASK: u32 = 0x3 << Self::TESSELLATION_SPACING_SHIFT;
    const TESSELLATION_CLOCKWISE_MASK: u32 = 0x1 << Self::TESSELLATION_CLOCKWISE_SHIFT;
    const APP_STAGE_MASK: u32 = 0x7 << Self::APP_STAGE_SHIFT;
    const ALPHA_TEST_FUNC_MASK: u32 = 0x7 << Self::ALPHA_TEST_FUNC_SHIFT;

    /// Hash the key, considering only relevant bytes (smaller if xfb not enabled).
    pub fn hash_key(&self) -> u64 {
        let size = self.size();
        let bytes: &[u8] =
            unsafe { std::slice::from_raw_parts(self as *const Self as *const u8, size) };
        city_hash64(bytes)
    }

    /// Returns the xfb_enabled bit.
    pub fn xfb_enabled(&self) -> bool {
        (self.raw & 1) != 0
    }

    /// Returns the early_z bit.
    pub fn early_z(&self) -> bool {
        ((self.raw >> 1) & 1) != 0
    }

    pub fn gs_input_topology(&self) -> u32 {
        (self.raw & Self::GS_INPUT_TOPOLOGY_MASK) >> Self::GS_INPUT_TOPOLOGY_SHIFT
    }

    pub fn tessellation_primitive(&self) -> u32 {
        (self.raw & Self::TESSELLATION_PRIMITIVE_MASK) >> Self::TESSELLATION_PRIMITIVE_SHIFT
    }

    pub fn tessellation_spacing(&self) -> u32 {
        (self.raw & Self::TESSELLATION_SPACING_MASK) >> Self::TESSELLATION_SPACING_SHIFT
    }

    pub fn tessellation_clockwise(&self) -> bool {
        ((self.raw & Self::TESSELLATION_CLOCKWISE_MASK) >> Self::TESSELLATION_CLOCKWISE_SHIFT) != 0
    }

    pub fn set_xfb_enabled(&mut self, enabled: bool) {
        self.raw =
            (self.raw & !Self::XFB_ENABLED_MASK) | ((enabled as u32) << Self::XFB_ENABLED_SHIFT);
    }

    pub fn set_early_z(&mut self, enabled: bool) {
        self.raw = (self.raw & !Self::EARLY_Z_MASK) | ((enabled as u32) << Self::EARLY_Z_SHIFT);
    }

    pub fn set_gs_input_topology(&mut self, topology: u32) {
        self.raw = (self.raw & !Self::GS_INPUT_TOPOLOGY_MASK)
            | ((topology & 0xF) << Self::GS_INPUT_TOPOLOGY_SHIFT);
    }

    pub fn set_tessellation_primitive(&mut self, primitive: u32) {
        self.raw = (self.raw & !Self::TESSELLATION_PRIMITIVE_MASK)
            | ((primitive & 0x3) << Self::TESSELLATION_PRIMITIVE_SHIFT);
    }

    pub fn set_tessellation_spacing(&mut self, spacing: u32) {
        self.raw = (self.raw & !Self::TESSELLATION_SPACING_MASK)
            | ((spacing & 0x3) << Self::TESSELLATION_SPACING_SHIFT);
    }

    pub fn set_tessellation_clockwise(&mut self, clockwise: bool) {
        self.raw = (self.raw & !Self::TESSELLATION_CLOCKWISE_MASK)
            | ((clockwise as u32) << Self::TESSELLATION_CLOCKWISE_SHIFT);
    }

    pub fn set_app_stage(&mut self, app_stage: u32) {
        self.raw =
            (self.raw & !Self::APP_STAGE_MASK) | ((app_stage & 0x7) << Self::APP_STAGE_SHIFT);
    }

    pub fn alpha_test_func(&self) -> u32 {
        (self.raw & Self::ALPHA_TEST_FUNC_MASK) >> Self::ALPHA_TEST_FUNC_SHIFT
    }

    pub fn set_alpha_test_func(&mut self, func: u32) {
        self.raw = (self.raw & !Self::ALPHA_TEST_FUNC_MASK)
            | ((func & 0x7) << Self::ALPHA_TEST_FUNC_SHIFT);
    }

    /// Returns the effective size in bytes for hashing/comparison.
    ///
    /// If xfb is enabled, the full key (including xfb_state) is used;
    /// otherwise only up to the padding field.
    pub fn size(&self) -> usize {
        if self.xfb_enabled() {
            std::mem::size_of::<Self>()
        } else {
            // offset of `padding` field
            std::mem::offset_of!(GraphicsPipelineKey, padding)
        }
    }
}

impl Hash for GraphicsPipelineKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash_key());
    }
}

/// OpenGL graphics pipeline.
///
/// Corresponds to `OpenGL::GraphicsPipeline`.
pub struct GraphicsPipeline {
    pub key: GraphicsPipelineKey,

    /// Per-stage GLSL source produced by the recompiler. Stages with no
    /// shader (and the placeholder pipeline created when no GPU memory
    /// reader is installed) leave the corresponding entry as `None`.
    ///
    /// Gap (4) consumes these strings to call `glCreateShader` /
    /// `glShaderSource` / `glCompileShader` / `glLinkProgram` and fill
    /// `source_programs` with the resulting GL handles.
    pub glsl_sources: [Option<String>; NUM_STAGES],

    /// Source program handles per stage (GLSL or SPIR-V).
    pub source_programs: [u32; NUM_STAGES],
    /// Assembly program handles per stage (GLASM).
    pub assembly_programs: [u32; NUM_STAGES],
    /// Bitmask of enabled stages.
    pub enabled_stages_mask: u32,

    /// Per-stage enabled uniform buffer masks.
    pub enabled_uniform_buffer_masks: [u32; NUM_STAGES],
    /// Per-stage uniform buffer used sizes.
    pub uniform_buffer_sizes: UniformBufferSizes,
    /// Per-stage base uniform bindings.
    pub base_uniform_bindings: [u32; NUM_STAGES],
    /// Per-stage base storage bindings.
    pub base_storage_bindings: [u32; NUM_STAGES],
    /// Per-stage texture buffer counts.
    pub num_texture_buffers: [u32; NUM_STAGES],
    /// Per-stage image buffer counts.
    pub num_image_buffers: [u32; NUM_STAGES],

    pub use_storage_buffers: bool,
    pub writes_global_memory: bool,
    pub uses_local_memory: bool,

    /// Transform feedback attributes array.
    pub num_xfb_attribs: i32,
    pub num_xfb_buffers_active: u32,
    pub xfb_attribs: Vec<i32>,

    // Build synchronization
    built_mutex: Mutex<bool>,
    built_condvar: Condvar,
    built_fence: gl::types::GLsync,
    is_built: bool,

    /// GL program pipeline object that aggregates the per-stage separable
    /// programs in `source_programs`. `0` means uninitialised.
    ///
    /// Created lazily in `build_from_sources` and bound by `configure()`
    /// (gap 5). Mirrors upstream `OpenGL::ProgramManager::BindGraphicsPipeline`'s
    /// per-pipeline `OGLPipeline` object.
    program_pipeline: u32,

    /// Per-stage shader translation result. Mirrors upstream
    /// `std::array<Shader::Info, NUM_STAGES> stage_infos`. Populated by
    /// `apply_shader_infos` so the rasterizer-side ConfigureImpl
    /// equivalent can iterate `texture_descriptors` /
    /// `texture_buffer_descriptors` / `image_descriptors` per stage to
    /// build `ImageViewInOut[]` for `fill_graphics_image_views`.
    pub stage_infos: [Option<ShaderInfo>; NUM_STAGES],
}

// SAFETY: The GL sync handle is only accessed while the built_mutex is held.
unsafe impl Send for GraphicsPipeline {}
unsafe impl Sync for GraphicsPipeline {}

impl Drop for GraphicsPipeline {
    fn drop(&mut self) {
        // Release any per-stage program handles created via
        // `build_from_sources`. Safe to call without a current GL context
        // because all entries are zero in that case.
        if self.has_gl_programs() {
            self.delete_gl_programs();
        }
    }
}

impl GraphicsPipeline {
    /// Synchronize graphics TIC/TSC descriptor tables before configuring the pipeline.
    ///
    /// Corresponds to the first side effect in upstream
    /// `GraphicsPipeline::ConfigureImpl`: `texture_cache.SynchronizeGraphicsDescriptors()`.
    pub fn synchronize_graphics_descriptors(
        &self,
        texture_cache: &mut OpenGLTextureCache,
        regs: DescriptorSyncRegs,
    ) {
        texture_cache.base.synchronize_graphics_descriptors(regs);
    }

    /// Create a new graphics pipeline.
    ///
    /// Corresponds to `GraphicsPipeline::GraphicsPipeline()`.
    pub fn new(key: GraphicsPipelineKey) -> Self {
        Self {
            key,
            glsl_sources: Default::default(),
            source_programs: [0; NUM_STAGES],
            assembly_programs: [0; NUM_STAGES],
            enabled_stages_mask: 0,
            enabled_uniform_buffer_masks: [0; NUM_STAGES],
            uniform_buffer_sizes: [[0;
                crate::buffer_cache::buffer_cache_base::NUM_GRAPHICS_UNIFORM_BUFFERS as usize];
                NUM_STAGES],
            base_uniform_bindings: [0; NUM_STAGES],
            base_storage_bindings: [0; NUM_STAGES],
            num_texture_buffers: [0; NUM_STAGES],
            num_image_buffers: [0; NUM_STAGES],
            use_storage_buffers: false,
            writes_global_memory: false,
            uses_local_memory: false,
            num_xfb_attribs: 0,
            num_xfb_buffers_active: 0,
            xfb_attribs: vec![0i32; 128 * XFB_ENTRY_STRIDE * NUM_TRANSFORM_FEEDBACK_BUFFERS],
            built_mutex: Mutex::new(true),
            built_condvar: Condvar::new(),
            built_fence: std::ptr::null(),
            is_built: true,
            program_pipeline: 0,
            stage_infos: Default::default(),
        }
    }

    /// Populate per-stage descriptor metadata from translated shader infos.
    ///
    /// Port of the metadata loop in upstream
    /// `GraphicsPipeline::GraphicsPipeline(...)` (`gl_graphics_pipeline.cpp`):
    /// enabled stage mask, per-stage UBO mask/sizes, and cumulative base
    /// bindings are derived from `Shader::Info`.
    pub fn apply_shader_infos(&mut self, infos: &[Option<ShaderInfo>; NUM_STAGES]) {
        self.enabled_stages_mask = 0;
        self.enabled_uniform_buffer_masks = [0; NUM_STAGES];
        self.uniform_buffer_sizes = [[0;
            crate::buffer_cache::buffer_cache_base::NUM_GRAPHICS_UNIFORM_BUFFERS as usize];
            NUM_STAGES];
        self.base_uniform_bindings = [0; NUM_STAGES];
        self.base_storage_bindings = [0; NUM_STAGES];
        self.num_texture_buffers = [0; NUM_STAGES];
        self.num_image_buffers = [0; NUM_STAGES];
        self.use_storage_buffers = false;
        self.writes_global_memory = false;
        self.uses_local_memory = false;

        // Keep a per-stage `Info` clone so the rasterizer-side ConfigureImpl
        // can iterate `texture_descriptors[]` etc. at draw time. Upstream
        // stores these inside `GraphicsPipeline` as `stage_infos`.
        self.stage_infos = std::array::from_fn(|stage| infos[stage].clone());

        let mut num_storage_buffers = 0u32;
        for stage in 0..NUM_STAGES {
            if let Some(info) = infos[stage].as_ref() {
                self.enabled_stages_mask |= 1u32 << stage;
                self.enabled_uniform_buffer_masks[stage] = info.constant_buffer_mask;
                self.uniform_buffer_sizes[stage].copy_from_slice(&info.constant_buffer_used_sizes);
                self.num_texture_buffers[stage] = num_descriptors(&info.texture_buffer_descriptors);
                self.num_image_buffers[stage] = num_descriptors(&info.image_buffer_descriptors);
                num_storage_buffers += num_descriptors(&info.storage_buffers_descriptors);
                self.writes_global_memory |= info
                    .storage_buffers_descriptors
                    .iter()
                    .any(|desc| desc.is_written);
                self.uses_local_memory |= info.uses_local_memory;
            }

            if stage < NUM_STAGES - 1 {
                self.base_uniform_bindings[stage + 1] = self.base_uniform_bindings[stage];
                self.base_storage_bindings[stage + 1] = self.base_storage_bindings[stage];
                if let Some(info) = infos[stage].as_ref() {
                    self.base_uniform_bindings[stage + 1] +=
                        num_descriptors(&info.constant_buffer_descriptors);
                    self.base_storage_bindings[stage + 1] +=
                        num_descriptors(&info.storage_buffers_descriptors);
                }
            }
        }

        // Ruzu currently emits GLSL, not GLASM, for graphics pipelines. Match
        // upstream's non-assembly path by using real GL shader-storage-buffer
        // bindings whenever storage descriptors exist.
        self.use_storage_buffers = num_storage_buffers != 0;
        if self.use_storage_buffers {
            self.writes_global_memory = false;
        }
    }

    /// Bind graphics programs during the upstream `ConfigureImpl` sequence.
    ///
    /// Corresponds to upstream:
    /// `WaitForBuild();`
    /// then `program_manager.BindAssemblyPrograms(...)` or
    /// `program_manager.BindSourcePrograms(...)`.
    pub fn bind_graphics_programs_for_configure(&mut self) {
        self.wait_for_build();

        // Gap (5): bind the per-pipeline program-pipeline object so the
        // next `glDraw*` call uses the per-stage separable programs that
        // `build_from_sources` (gap 4) compiled and aggregated. Mirrors
        // upstream `OpenGL::ProgramManager::BindGraphicsPipeline`.
        if self.program_pipeline != 0 {
            unsafe {
                // Detach any monolithic program first — separable programs
                // and a bound monolithic program are mutually exclusive.
                gl::UseProgram(0);
                gl::BindProgramPipeline(self.program_pipeline);
            }
        }

        // Full implementation also requires buffer_cache / texture_cache
        // bindings (uniform/storage/texture/image descriptors); those are
        // separate gaps.
    }

    /// Bind graphics programs through the upstream `ProgramManager` owner.
    ///
    /// Corresponds to upstream:
    /// `program_manager.BindAssemblyPrograms(assembly_programs, enabled_stages_mask)`
    /// or `program_manager.BindSourcePrograms(source_programs)`.
    pub fn bind_graphics_programs_for_configure_with_program_manager(
        &mut self,
        program_manager: &mut ProgramManager,
    ) {
        self.wait_for_build();
        if self.assembly_programs[0] != 0 {
            program_manager
                .bind_assembly_programs(&self.assembly_programs, self.enabled_stages_mask);
        } else {
            program_manager.bind_source_programs(&self.source_programs);
        }
    }

    /// Configure the pipeline for a draw call.
    ///
    /// Compatibility wrapper while the full upstream-shaped `ConfigureImpl`
    /// body is being rebuilt as smaller owner-correct helpers.
    pub fn configure(&mut self, _is_indexed: bool) {
        self.bind_graphics_programs_for_configure();
    }

    /// Configure the buffer-cache base state owned by upstream
    /// `GraphicsPipeline::ConfigureImpl`.
    pub fn configure_buffer_cache_state<P, DT>(&self, buffer_cache: &mut BufferCache<P, DT>)
    where
        P: BufferCacheParams,
        DT: DeviceTracker,
    {
        buffer_cache.set_uniform_buffers_state(
            &self.enabled_uniform_buffer_masks,
            &self.uniform_buffer_sizes,
        );
        buffer_cache.set_graphics_base_uniform_bindings(&self.base_uniform_bindings);
        buffer_cache.set_graphics_base_storage_bindings(&self.base_storage_bindings);
        buffer_cache.set_enable_storage_buffers(self.use_storage_buffers);
    }

    /// Synchronize graphics descriptors, then configure the buffer-cache base state.
    ///
    /// This owns the first adjacent upstream `GraphicsPipeline::ConfigureImpl`
    /// side effects:
    /// `texture_cache.SynchronizeGraphicsDescriptors()`, then
    /// `SetUniformBuffersState`, `SetBaseUniformBindings`,
    /// `SetBaseStorageBindings`, and `SetEnableStorageBuffers`.
    pub fn synchronize_graphics_descriptors_then_configure_buffer_cache_state<P, DT>(
        &self,
        texture_cache: &mut OpenGLTextureCache,
        regs: DescriptorSyncRegs,
        buffer_cache: &mut BufferCache<P, DT>,
    ) where
        P: BufferCacheParams,
        DT: DeviceTracker,
    {
        self.synchronize_graphics_descriptors(texture_cache, regs);
        self.configure_buffer_cache_state(buffer_cache);
    }

    /// Install the graphics engine-state bridge consumed by the buffer cache.
    ///
    /// Upstream stores live `maxwell3d` and `gpu_memory` pointers on
    /// `GraphicsPipeline` through `SetEngine`; Rust still passes a boxed
    /// draw-state adapter, but the graphics pipeline now owns the configure
    /// step that makes the state visible to subsequent descriptor and buffer
    /// binding work.
    pub fn set_graphics_engine_state<P, DT>(
        &self,
        buffer_cache: &mut BufferCache<P, DT>,
        engine_state: Box<dyn EngineState>,
    ) where
        P: BufferCacheParams,
        DT: DeviceTracker,
    {
        buffer_cache.set_engine_state(engine_state);
    }

    /// Bind enabled graphics uniform buffers from the draw-time cbuf snapshot.
    ///
    /// Corresponds to the uniform-buffer binding state consumed by upstream
    /// `GraphicsPipeline::ConfigureImpl` before `UpdateGraphicsBuffers`.
    pub fn bind_graphics_uniform_buffers<P, DT, A>(
        &self,
        buffer_cache: &mut BufferCache<P, DT>,
        cb_bindings: &[[ConstBufferBinding; MAX_CB_SLOTS]],
        mut gpu_to_cpu_address: A,
    ) where
        P: BufferCacheParams,
        DT: DeviceTracker,
        A: FnMut(u64) -> Option<u64>,
    {
        for stage in 0..self
            .enabled_uniform_buffer_masks
            .len()
            .min(cb_bindings.len())
        {
            let mut bits = self.enabled_uniform_buffer_masks[stage];
            let mut slot = 0u32;
            while bits != 0 {
                let skip = bits.trailing_zeros();
                slot += skip;
                bits >>= skip;

                let binding = cb_bindings[stage][slot as usize];
                if binding.enabled && binding.address != 0 && binding.size != 0 {
                    let device_addr =
                        gpu_to_cpu_address(binding.address).unwrap_or(binding.address);
                    buffer_cache.bind_graphics_uniform_buffer_with_device_addr(
                        stage,
                        slot,
                        device_addr,
                        binding.size,
                    );
                } else {
                    buffer_cache.disable_graphics_uniform_buffer(stage, slot);
                }

                slot += 1;
                bits >>= 1;
            }
        }
    }

    /// Bind storage-buffer descriptors for one graphics stage.
    ///
    /// Corresponds to upstream `GraphicsPipeline::ConfigureImpl`'s
    /// `config_stage` lambda before texture/image descriptor collection:
    /// `UnbindGraphicsStorageBuffers(stage)`, then
    /// `BindGraphicsStorageBuffer(stage, ssbo_index, ...)` for each storage
    /// descriptor when storage buffers are enabled.
    pub fn bind_stage_storage_buffers<P, DT>(
        &self,
        buffer_cache: &mut BufferCache<P, DT>,
        stage: usize,
        trace_ssbo_bind: bool,
    ) where
        P: BufferCacheParams,
        DT: DeviceTracker,
    {
        let Some(info) = self.stage_infos.get(stage).and_then(Option::as_ref) else {
            return;
        };

        buffer_cache.unbind_graphics_storage_buffers(stage);
        if !self.use_storage_buffers {
            return;
        }

        if trace_ssbo_bind && !info.storage_buffers_descriptors.is_empty() {
            log::info!(
                "[SSBO_CONFIG] pipeline={} stage={} descriptors={} base_binding={}",
                self.program_pipeline_handle(),
                stage,
                info.storage_buffers_descriptors.len(),
                self.base_storage_bindings[stage]
            );
        }
        for (ssbo_index, desc) in info.storage_buffers_descriptors.iter().enumerate() {
            assert_eq!(
                desc.count, 1,
                "GraphicsPipeline storage buffer descriptor count must match upstream ASSERT"
            );
            buffer_cache.bind_graphics_storage_buffer(
                stage,
                ssbo_index,
                desc.cbuf_index,
                desc.cbuf_offset,
                desc.is_written,
            );
        }
    }

    /// Bind storage-buffer descriptors using caller-provided GPU-memory
    /// helpers, preserving the Rust no-relock bridge while keeping upstream
    /// `ConfigureImpl` ownership on the graphics pipeline.
    pub fn bind_stage_storage_buffers_with_gpu_reader<P, DT, A, L, R>(
        &self,
        buffer_cache: &mut BufferCache<P, DT>,
        stage: usize,
        trace_ssbo_bind: bool,
        mut gpu_to_cpu_address: A,
        mut get_memory_layout_size: L,
        mut read_block: R,
    ) where
        P: BufferCacheParams,
        DT: DeviceTracker,
        A: FnMut(u64) -> Option<u64>,
        L: FnMut(u64) -> u64,
        R: FnMut(u64, &mut [u8]) -> bool,
    {
        let Some(info) = self.stage_infos.get(stage).and_then(Option::as_ref) else {
            return;
        };

        buffer_cache.unbind_graphics_storage_buffers(stage);
        if !self.use_storage_buffers {
            return;
        }

        if trace_ssbo_bind && !info.storage_buffers_descriptors.is_empty() {
            log::info!(
                "[SSBO_CONFIG] pipeline={} stage={} descriptors={} base_binding={}",
                self.program_pipeline_handle(),
                stage,
                info.storage_buffers_descriptors.len(),
                self.base_storage_bindings[stage]
            );
        }
        for (ssbo_index, desc) in info.storage_buffers_descriptors.iter().enumerate() {
            assert_eq!(
                desc.count, 1,
                "GraphicsPipeline storage buffer descriptor count must match upstream ASSERT"
            );
            buffer_cache.bind_graphics_storage_buffer_with_gpu_reader(
                stage,
                ssbo_index,
                desc.cbuf_index,
                desc.cbuf_offset,
                desc.is_written,
                &mut gpu_to_cpu_address,
                &mut get_memory_layout_size,
                &mut read_block,
            );
        }
    }

    /// Collect texture/image descriptors for one graphics stage.
    ///
    /// Corresponds to upstream `GraphicsPipeline::ConfigureImpl`'s
    /// `config_stage` descriptor loops after storage-buffer binding:
    /// texture-buffer descriptors, image-buffer descriptors, sampled texture
    /// descriptors plus sampler ids, then storage-image descriptors.
    pub fn collect_stage_texture_image_descriptors<R, S, D>(
        &self,
        stage: usize,
        max_desc_count: u32,
        via_header_index: bool,
        views: &mut Vec<ImageViewInOut>,
        sampler_ids: &mut Vec<SamplerId>,
        mut read_handle: R,
        mut get_graphics_sampler_id: S,
        mut record_detail: D,
        trace_texture_descriptors: bool,
    ) -> bool
    where
        R: FnMut(usize, u32, u32) -> Option<u32>,
        S: FnMut(u32) -> SamplerId,
        D: FnMut(usize, u64, u64),
    {
        let Some(info) = self.stage_infos.get(stage).and_then(Option::as_ref) else {
            return false;
        };

        if trace_texture_descriptors {
            log::warn!(
                "[TEX_DESC] stage={} texture_buffers={} image_buffers={} textures={} images={}",
                stage,
                info.texture_buffer_descriptors.len(),
                info.image_buffer_descriptors.len(),
                info.texture_descriptors.len(),
                info.image_descriptors.len(),
            );
        }

        let mut has_images = false;
        macro_rules! descriptor_offset {
            ($desc:expr, $idx:expr) => {{
                let shift = $desc.size_shift.min(31);
                $idx.checked_shl(shift)
                    .and_then(|index_offset| $desc.cbuf_offset.checked_add(index_offset))
            }};
        }

        macro_rules! resolve_texture_handle {
            ($desc:expr, $idx:expr) => {
                (|| -> Option<u32> {
                    let shift = $desc.size_shift.min(31);
                    let index_offset = $idx.checked_shl(shift)?;
                    let offset = $desc.cbuf_offset.checked_add(index_offset)?;
                    if !$desc.has_secondary {
                        read_handle(stage, $desc.cbuf_index, offset)
                    } else {
                        let second_offset =
                            $desc.secondary_cbuf_offset.checked_add(index_offset)?;
                        debug_assert!($desc.shift_left < 32);
                        debug_assert!($desc.secondary_shift_left < 32);
                        let lhs = read_handle(stage, $desc.cbuf_index, offset)
                            .map(|raw| raw << $desc.shift_left);
                        let rhs = read_handle(stage, $desc.secondary_cbuf_index, second_offset)
                            .map(|raw| raw << $desc.secondary_shift_left);
                        lhs.zip(rhs).map(|(lhs, rhs)| lhs | rhs)
                    }
                })()
            };
        }

        record_detail(
            36,
            stage as u64,
            info.texture_buffer_descriptors.len() as u64,
        );
        for desc in &info.texture_buffer_descriptors {
            let count = desc.count.min(max_desc_count);
            for idx in 0..count {
                if let Some(raw) = resolve_texture_handle!(desc, idx) {
                    let (tic_id, _) = texture_pair(raw, via_header_index);
                    views.push(ImageViewInOut {
                        index: tic_id,
                        blacklist: false,
                        id: Default::default(),
                    });
                }
            }
        }
        record_detail(37, stage as u64, 0);

        record_detail(38, stage as u64, info.image_buffer_descriptors.len() as u64);
        for desc in &info.image_buffer_descriptors {
            let count = desc.count.min(max_desc_count);
            for idx in 0..count {
                if let Some(offset) = descriptor_offset!(desc, idx) {
                    let Some(raw) = read_handle(stage, desc.cbuf_index, offset) else {
                        continue;
                    };
                    let (tic_id, _) = texture_pair(raw, via_header_index);
                    views.push(ImageViewInOut {
                        index: tic_id,
                        blacklist: false,
                        id: Default::default(),
                    });
                }
            }
        }
        record_detail(39, stage as u64, 0);

        record_detail(40, stage as u64, info.texture_descriptors.len() as u64);
        for (desc_index, desc) in info.texture_descriptors.iter().enumerate() {
            let count = desc.count.min(max_desc_count);
            for idx in 0..count {
                let packed_desc = ((desc_index as u64) << 32) | idx as u64;
                record_detail(45, stage as u64, packed_desc);
                let raw = resolve_texture_handle!(desc, idx);
                record_detail(46, stage as u64, packed_desc);
                if trace_texture_descriptors {
                    log::warn!(
                        "[TEX_DESC] sampled stage={} idx={} cbuf={} offset=0x{:X} shift={} count={} raw={:?}",
                        stage,
                        idx,
                        desc.cbuf_index,
                        desc.cbuf_offset,
                        desc.size_shift,
                        desc.count,
                        raw,
                    );
                }
                if let Some(raw) = raw {
                    let (tic_id, tsc_id) = texture_pair(raw, via_header_index);
                    record_detail(47, stage as u64, packed_desc);
                    views.push(ImageViewInOut {
                        index: tic_id,
                        blacklist: false,
                        id: Default::default(),
                    });
                    record_detail(48, stage as u64, packed_desc);
                    sampler_ids.push(get_graphics_sampler_id(tsc_id));
                    record_detail(49, stage as u64, packed_desc);
                    record_detail(50, stage as u64, packed_desc);
                }
            }
        }
        record_detail(41, stage as u64, 0);

        record_detail(42, stage as u64, info.image_descriptors.len() as u64);
        for desc in &info.image_descriptors {
            has_images = true;
            let count = desc.count.min(max_desc_count);
            for idx in 0..count {
                if let Some(offset) = descriptor_offset!(desc, idx) {
                    let Some(raw) = read_handle(stage, desc.cbuf_index, offset) else {
                        continue;
                    };
                    let (tic_id, _) = texture_pair(raw, via_header_index);
                    views.push(ImageViewInOut {
                        index: tic_id,
                        blacklist: desc.is_written,
                        id: Default::default(),
                    });
                }
            }
        }
        record_detail(43, stage as u64, 0);
        record_detail(44, stage as u64, 0);

        has_images
    }

    /// Configure storage-buffer and texture/image descriptor state for all enabled graphics stages.
    ///
    /// Corresponds to upstream `GraphicsPipeline::ConfigureImpl`'s
    /// `if constexpr (Spec::enabled_stages[N]) { config_stage(N); }` sequence.
    /// Rust still receives GPU-memory and diagnostic bridges as closures until
    /// the pipeline owns upstream-equivalent `gpu_memory` and live engine state.
    #[allow(clippy::too_many_arguments)]
    pub fn configure_enabled_stage_texture_image_descriptors<P, DT, A, L, B, R, S, D>(
        &self,
        buffer_cache: &mut BufferCache<P, DT>,
        num_shader_stages: usize,
        max_desc_count: u32,
        via_header_index: bool,
        views: &mut Vec<ImageViewInOut>,
        sampler_ids: &mut Vec<SamplerId>,
        use_gpu_reader: bool,
        trace_ssbo_bind: bool,
        mut gpu_to_cpu_address: A,
        mut get_memory_layout_size: L,
        mut read_block: B,
        mut read_handle: R,
        mut get_graphics_sampler_id: S,
        mut record_detail: D,
        trace_texture_descriptors: bool,
    ) -> bool
    where
        P: BufferCacheParams,
        DT: DeviceTracker,
        A: FnMut(u64) -> Option<u64>,
        L: FnMut(u64) -> u64,
        B: FnMut(u64, &mut [u8]) -> bool,
        R: FnMut(usize, u32, u32) -> Option<u32>,
        S: FnMut(u32) -> SamplerId,
        D: FnMut(usize, u64, u64),
    {
        let mut has_images = false;
        for stage in 0..NUM_STAGES.min(num_shader_stages) {
            record_detail(32, stage as u64, 0);
            let Some(info) = self.stage_infos[stage].as_ref() else {
                continue;
            };
            let storage_descriptor_count = info.storage_buffers_descriptors.len() as u64;
            if use_gpu_reader {
                self.bind_stage_storage_buffers_with_gpu_reader(
                    buffer_cache,
                    stage,
                    trace_ssbo_bind,
                    &mut gpu_to_cpu_address,
                    &mut get_memory_layout_size,
                    &mut read_block,
                );
            } else {
                self.bind_stage_storage_buffers(buffer_cache, stage, trace_ssbo_bind);
            }
            record_detail(33, stage as u64, 0);
            record_detail(34, stage as u64, storage_descriptor_count);
            record_detail(35, stage as u64, 0);
            has_images |= self.collect_stage_texture_image_descriptors(
                stage,
                max_desc_count,
                via_header_index,
                views,
                sampler_ids,
                &mut read_handle,
                &mut get_graphics_sampler_id,
                &mut record_detail,
                trace_texture_descriptors,
            );
        }
        has_images
    }

    /// Fill graphics image views and materialize their OpenGL backing objects.
    ///
    /// Corresponds to upstream `GraphicsPipeline::ConfigureImpl`:
    /// `texture_cache.FillGraphicsImageViews<Spec::has_images>(...)`.
    /// The extra materialization calls are the Rust OpenGL bridge for the
    /// backend-independent slot pools populated by `FillGraphicsImageViews`.
    pub fn fill_and_materialize_graphics_image_views(
        &self,
        texture_cache: &mut OpenGLTextureCache,
        views: &mut [ImageViewInOut],
        sampler_ids: &[SamplerId],
        has_images: bool,
    ) {
        trace_gl_pipeline_stall!(
            "[GL_PIPELINE_STALL] fill_and_materialize_graphics_image_views enter len={} samplers={} has_images={}",
            views.len(),
            sampler_ids.len(),
            has_images
        );
        if std::env::var_os("RUZU_TRACE_VIEW_FILL").is_some() {
            log::warn!(
                "[VIEW_FILL] before len={} samplers={} has_images={} first={:?}",
                views.len(),
                sampler_ids.len(),
                has_images,
                views.first(),
            );
        }
        texture_cache.fill_graphics_image_views(views, has_images);
        trace_gl_pipeline_stall!("[GL_PIPELINE_STALL] after_fill_graphics_image_views");
        if std::env::var_os("RUZU_TRACE_VIEW_FILL").is_some() {
            log::warn!(
                "[VIEW_FILL] after_fill len={} first={:?}",
                views.len(),
                views.first(),
            );
        }
        texture_cache.materialize_views(views);
        trace_gl_pipeline_stall!("[GL_PIPELINE_STALL] after_materialize_views");
        if std::env::var_os("RUZU_TRACE_VIEW_FILL").is_some() {
            log::warn!(
                "[VIEW_FILL] after_materialize len={} first={:?}",
                views.len(),
                views.first(),
            );
        }
        texture_cache.materialize_samplers(sampler_ids);
        trace_gl_pipeline_stall!("[GL_PIPELINE_STALL] after_materialize_samplers");
    }

    /// Fill graphics image views, then update render targets and bind the draw framebuffer.
    ///
    /// This owns the adjacent upstream `GraphicsPipeline::ConfigureImpl` slice:
    /// `texture_cache.FillGraphicsImageViews<Spec::has_images>(...)`,
    /// `texture_cache.UpdateRenderTargets(false)`, then
    /// `state_tracker.BindFramebuffer(texture_cache.GetFramebuffer()->Handle())`.
    pub fn fill_graphics_image_views_then_update_render_targets_and_bind_framebuffer<D>(
        &self,
        texture_cache: &mut OpenGLTextureCache,
        state_tracker: &mut StateTracker,
        views: &mut [ImageViewInOut],
        sampler_ids: &[SamplerId],
        has_images: bool,
        render_targets: &Maxwell3DRenderTargets,
        dirty_flags: &[bool; 256],
        dirty_access: &mut D,
        fallback_size: Extent2D,
    ) -> Option<(u32, u32, u32)>
    where
        D: RenderTargetDirtyFlagAccess,
    {
        trace_gl_pipeline_stall!("[GL_PIPELINE_STALL] fill_then_update enter");
        self.fill_and_materialize_graphics_image_views(
            texture_cache,
            views,
            sampler_ids,
            has_images,
        );
        trace_gl_pipeline_stall!("[GL_PIPELINE_STALL] before_update_render_targets_and_bind");
        self.update_render_targets_and_bind_framebuffer(
            texture_cache,
            state_tracker,
            render_targets,
            dirty_flags,
            dirty_access,
            fallback_size,
        )
    }

    /// Collect graphics texture/image descriptors, fill image views, update render targets,
    /// then bind the draw framebuffer in upstream `ConfigureImpl` order.
    ///
    /// This owns the adjacent upstream slice from the enabled-stage
    /// `config_stage(...)` calls through:
    /// `texture_cache.FillGraphicsImageViews<Spec::has_images>(...)`,
    /// `texture_cache.UpdateRenderTargets(false)`, and
    /// `state_tracker.BindFramebuffer(texture_cache.GetFramebuffer()->Handle())`.
    #[allow(clippy::too_many_arguments)]
    pub fn configure_graphics_descriptors_then_fill_and_bind_framebuffer<
        P,
        DT,
        A,
        L,
        B,
        R,
        RD,
        DA,
    >(
        &self,
        buffer_cache: &mut BufferCache<P, DT>,
        texture_cache: &mut OpenGLTextureCache,
        state_tracker: &mut StateTracker,
        num_shader_stages: usize,
        max_desc_count: u32,
        via_header_index: bool,
        views: &mut Vec<ImageViewInOut>,
        sampler_ids: &mut Vec<SamplerId>,
        use_gpu_reader: bool,
        trace_ssbo_bind: bool,
        gpu_to_cpu_address: A,
        get_memory_layout_size: L,
        read_block: B,
        read_handle: R,
        record_detail: RD,
        trace_texture_descriptors: bool,
        render_targets: &Maxwell3DRenderTargets,
        dirty_flags: &[bool; 256],
        dirty_access: &mut DA,
        fallback_size: Extent2D,
    ) -> Option<(u32, u32, u32)>
    where
        P: BufferCacheParams,
        DT: DeviceTracker,
        A: FnMut(u64) -> Option<u64>,
        L: FnMut(u64) -> u64,
        B: FnMut(u64, &mut [u8]) -> bool,
        R: FnMut(usize, u32, u32) -> Option<u32>,
        RD: FnMut(usize, u64, u64),
        DA: RenderTargetDirtyFlagAccess,
    {
        trace_gl_pipeline_stall!("[GL_PIPELINE_STALL] configure_descriptors_then_fill enter");
        let has_images = self.configure_enabled_stage_texture_image_descriptors(
            buffer_cache,
            num_shader_stages,
            max_desc_count,
            via_header_index,
            views,
            sampler_ids,
            use_gpu_reader,
            trace_ssbo_bind,
            gpu_to_cpu_address,
            get_memory_layout_size,
            read_block,
            read_handle,
            |tsc_id| texture_cache.base.get_graphics_sampler_id(tsc_id),
            record_detail,
            trace_texture_descriptors,
        );
        trace_gl_pipeline_stall!(
            "[GL_PIPELINE_STALL] after_configure_enabled_stage_texture_image_descriptors has_images={} views={} samplers={}",
            has_images,
            views.len(),
            sampler_ids.len()
        );
        self.fill_graphics_image_views_then_update_render_targets_and_bind_framebuffer(
            texture_cache,
            state_tracker,
            views,
            sampler_ids,
            has_images,
            render_targets,
            dirty_flags,
            dirty_access,
            fallback_size,
        )
    }

    /// Install graphics engine state, then collect descriptors/fill image views/update targets.
    ///
    /// This is the Rust bridge for the upstream `ConfigureImpl` sequence after
    /// the base buffer-cache state is configured and before `config_stage(...)`
    /// walks descriptors using `maxwell3d->state.shader_stages[...]`.
    #[allow(clippy::too_many_arguments)]
    pub fn set_engine_state_then_configure_graphics_descriptors_and_framebuffer<
        P,
        DT,
        A,
        L,
        B,
        R,
        RD,
        DA,
        E,
    >(
        &self,
        buffer_cache: &mut BufferCache<P, DT>,
        engine_state: Box<dyn EngineState>,
        mut after_engine_state: E,
        texture_cache: &mut OpenGLTextureCache,
        state_tracker: &mut StateTracker,
        num_shader_stages: usize,
        max_desc_count: u32,
        via_header_index: bool,
        views: &mut Vec<ImageViewInOut>,
        sampler_ids: &mut Vec<SamplerId>,
        use_gpu_reader: bool,
        trace_ssbo_bind: bool,
        gpu_to_cpu_address: A,
        get_memory_layout_size: L,
        read_block: B,
        read_handle: R,
        record_detail: RD,
        trace_texture_descriptors: bool,
        render_targets: &Maxwell3DRenderTargets,
        dirty_flags: &[bool; 256],
        dirty_access: &mut DA,
        fallback_size: Extent2D,
    ) -> Option<(u32, u32, u32)>
    where
        P: BufferCacheParams,
        DT: DeviceTracker,
        A: FnMut(u64) -> Option<u64>,
        L: FnMut(u64) -> u64,
        B: FnMut(u64, &mut [u8]) -> bool,
        R: FnMut(usize, u32, u32) -> Option<u32>,
        RD: FnMut(usize, u64, u64),
        DA: RenderTargetDirtyFlagAccess,
        E: FnMut(),
    {
        trace_gl_pipeline_stall!("[GL_PIPELINE_STALL] set_engine_then_configure enter");
        self.set_graphics_engine_state(buffer_cache, engine_state);
        after_engine_state();
        trace_gl_pipeline_stall!("[GL_PIPELINE_STALL] after_set_graphics_engine_state");
        self.configure_graphics_descriptors_then_fill_and_bind_framebuffer(
            buffer_cache,
            texture_cache,
            state_tracker,
            num_shader_stages,
            max_desc_count,
            via_header_index,
            views,
            sampler_ids,
            use_gpu_reader,
            trace_ssbo_bind,
            gpu_to_cpu_address,
            get_memory_layout_size,
            read_block,
            read_handle,
            record_detail,
            trace_texture_descriptors,
            render_targets,
            dirty_flags,
            dirty_access,
            fallback_size,
        )
    }

    /// Synchronize descriptors/base buffer state, install engine state, then
    /// collect descriptors/fill image views/update targets in upstream order.
    ///
    /// This extends the Rust bridge for upstream `ConfigureImpl` from:
    /// `texture_cache.SynchronizeGraphicsDescriptors()` through
    /// `SetUniformBuffersState`, `SetBase*Bindings`, `SetEnableStorageBuffers`,
    /// `SetEngine`, `config_stage(...)`, `FillGraphicsImageViews`,
    /// `UpdateRenderTargets(false)`, and framebuffer bind.
    #[allow(clippy::too_many_arguments)]
    pub fn synchronize_then_set_engine_state_and_configure_graphics_framebuffer<
        P,
        DT,
        A,
        L,
        B,
        R,
        RD,
        DA,
        S,
        E,
    >(
        &self,
        texture_cache: &mut OpenGLTextureCache,
        descriptor_sync_regs: DescriptorSyncRegs,
        buffer_cache: &mut BufferCache<P, DT>,
        mut after_descriptor_base_state: S,
        engine_state: Box<dyn EngineState>,
        after_engine_state: E,
        state_tracker: &mut StateTracker,
        num_shader_stages: usize,
        max_desc_count: u32,
        via_header_index: bool,
        views: &mut Vec<ImageViewInOut>,
        sampler_ids: &mut Vec<SamplerId>,
        use_gpu_reader: bool,
        trace_ssbo_bind: bool,
        gpu_to_cpu_address: A,
        get_memory_layout_size: L,
        read_block: B,
        read_handle: R,
        record_detail: RD,
        trace_texture_descriptors: bool,
        render_targets: &Maxwell3DRenderTargets,
        dirty_flags: &[bool; 256],
        dirty_access: &mut DA,
        fallback_size: Extent2D,
    ) -> Option<(u32, u32, u32)>
    where
        P: BufferCacheParams,
        DT: DeviceTracker,
        A: FnMut(u64) -> Option<u64>,
        L: FnMut(u64) -> u64,
        B: FnMut(u64, &mut [u8]) -> bool,
        R: FnMut(usize, u32, u32) -> Option<u32>,
        RD: FnMut(usize, u64, u64),
        DA: RenderTargetDirtyFlagAccess,
        S: FnMut(),
        E: FnMut(),
    {
        trace_gl_pipeline_stall!("[GL_PIPELINE_STALL] synchronize_then_set_engine enter");
        self.synchronize_graphics_descriptors_then_configure_buffer_cache_state(
            texture_cache,
            descriptor_sync_regs,
            buffer_cache,
        );
        after_descriptor_base_state();
        trace_gl_pipeline_stall!("[GL_PIPELINE_STALL] after_synchronize_descriptor_base_state");
        self.set_engine_state_then_configure_graphics_descriptors_and_framebuffer(
            buffer_cache,
            engine_state,
            after_engine_state,
            texture_cache,
            state_tracker,
            num_shader_stages,
            max_desc_count,
            via_header_index,
            views,
            sampler_ids,
            use_gpu_reader,
            trace_ssbo_bind,
            gpu_to_cpu_address,
            get_memory_layout_size,
            read_block,
            read_handle,
            record_detail,
            trace_texture_descriptors,
            render_targets,
            dirty_flags,
            dirty_access,
            fallback_size,
        )
    }

    /// Bind host buffer resources at the start of one prepare-stage pass.
    ///
    /// This is the owner-local slice of upstream
    /// `GraphicsPipeline::ConfigureImpl`'s `prepare_stage` lambda:
    /// `SetImagePointers(&textures[texture_binding], &images[image_binding])`
    /// followed by `BindHostStageBuffers(stage)`, then the texture/image-buffer
    /// binding and view iterator skips before sampled textures are prepared.
    pub fn prepare_stage_host_buffer_bindings<P, DT>(
        &self,
        buffer_cache: &mut BufferCache<P, DT>,
        stage: usize,
        textures: &mut [u32],
        samplers: &mut [u32],
        images: &mut [u32],
        bound_texture_view_ids: &mut [ImageViewId],
        texture_binding: &mut usize,
        sampler_binding: &mut usize,
        image_binding: &mut usize,
        views: &[ImageViewInOut],
        views_it: &mut usize,
    ) where
        P: BufferCacheParams,
        DT: DeviceTracker,
    {
        if self
            .stage_infos
            .get(stage)
            .and_then(Option::as_ref)
            .is_none()
        {
            return;
        }

        let texture_ptr = if *texture_binding <= textures.len() {
            unsafe { textures.as_mut_ptr().add(*texture_binding) }
        } else {
            std::ptr::null_mut()
        };
        let image_ptr = if *image_binding <= images.len() {
            unsafe { images.as_mut_ptr().add(*image_binding) }
        } else {
            std::ptr::null_mut()
        };
        buffer_cache.set_image_pointers(texture_ptr, image_ptr);
        buffer_cache.bind_host_stage_buffers(stage);

        let texture_buffer_count = self.num_texture_buffers[stage] as usize;
        let image_buffer_count = self.num_image_buffers[stage] as usize;
        for _ in 0..texture_buffer_count {
            if *sampler_binding < samplers.len() {
                samplers[*sampler_binding] = 0;
            }
            if *texture_binding < bound_texture_view_ids.len() && *views_it < views.len() {
                bound_texture_view_ids[*texture_binding] = views[*views_it].id;
            }
            *texture_binding += 1;
            *sampler_binding += 1;
            *views_it = views_it.saturating_add(1).min(views.len());
        }

        for _ in 0..image_buffer_count {
            *image_binding += 1;
            *views_it = views_it.saturating_add(1).min(views.len());
        }
    }

    /// Clear the transient host texture/image pointer bridge after prepare-stage binding.
    pub fn clear_host_stage_buffer_pointers<P, DT>(&self, buffer_cache: &mut BufferCache<P, DT>)
    where
        P: BufferCacheParams,
        DT: DeviceTracker,
    {
        buffer_cache.set_image_pointers(std::ptr::null_mut(), std::ptr::null_mut());
    }

    /// Prepare one graphics stage's host buffer, sampled-texture, storage-image, and uniform state.
    ///
    /// Corresponds to the full upstream `GraphicsPipeline::ConfigureImpl`
    /// `prepare_stage` lambda. The observer callback carries Rust-only draw
    /// diagnostics that upstream does not have.
    pub fn prepare_stage_texture_image_bindings<P, DT, F>(
        &self,
        buffer_cache: &mut BufferCache<P, DT>,
        texture_cache: &mut OpenGLTextureCache,
        stage: usize,
        textures: &mut [u32],
        samplers: &mut [u32],
        images: &mut [u32],
        bound_texture_view_ids: &mut [ImageViewId],
        texture_binding: &mut usize,
        sampler_binding: &mut usize,
        image_binding: &mut usize,
        views: &[ImageViewInOut],
        views_it: &mut usize,
        sampler_ids: &[SamplerId],
        sampler_it: &mut usize,
        max_desc_count: u32,
        surface_clip: SurfaceClipInfo,
        mut observe: F,
    ) where
        P: BufferCacheParams,
        DT: DeviceTracker,
        F: FnMut(&SampledTextureBinding, &OpenGLTextureCache),
    {
        if self
            .stage_infos
            .get(stage)
            .and_then(Option::as_ref)
            .is_none()
        {
            return;
        }

        self.prepare_stage_host_buffer_bindings(
            buffer_cache,
            stage,
            textures,
            samplers,
            images,
            bound_texture_view_ids,
            texture_binding,
            sampler_binding,
            image_binding,
            views,
            views_it,
        );

        let mut stage_texture_binding = 0u32;
        let mut stage_image_binding = 0u32;
        let texture_cache_ref: &OpenGLTextureCache = &*texture_cache;
        let texture_scaling_mask = self.bind_stage_sampled_textures(
            texture_cache_ref,
            stage,
            views,
            views_it,
            sampler_ids,
            sampler_it,
            textures,
            samplers,
            bound_texture_view_ids,
            texture_binding,
            sampler_binding,
            &mut stage_texture_binding,
            max_desc_count,
            |binding| observe(binding, texture_cache_ref),
        );
        let image_scaling_mask = self.bind_stage_storage_images(
            texture_cache,
            stage,
            views,
            views_it,
            images,
            image_binding,
            &mut stage_image_binding,
            max_desc_count,
        );
        self.upload_stage_uniforms(
            texture_cache,
            stage,
            texture_scaling_mask,
            image_scaling_mask,
            surface_clip,
        );
    }

    /// Prepare all enabled graphics stages in upstream stage order.
    ///
    /// Corresponds to the `if constexpr (Spec::enabled_stages[N]) { prepare_stage(N); }`
    /// sequence at the end of upstream `GraphicsPipeline::ConfigureImpl`'s
    /// `prepare_stage` lambda block. Rust still receives `num_shader_stages`
    /// from the rasterizer because the generated specialization shape is not
    /// represented as a Rust const generic yet.
    pub fn prepare_enabled_graphics_texture_image_bindings<P, DT, F>(
        &self,
        buffer_cache: &mut BufferCache<P, DT>,
        texture_cache: &mut OpenGLTextureCache,
        num_shader_stages: usize,
        textures: &mut [u32],
        samplers: &mut [u32],
        images: &mut [u32],
        bound_texture_view_ids: &mut [ImageViewId],
        texture_binding: &mut usize,
        sampler_binding: &mut usize,
        image_binding: &mut usize,
        views: &[ImageViewInOut],
        views_it: &mut usize,
        sampler_ids: &[SamplerId],
        sampler_it: &mut usize,
        max_desc_count: u32,
        surface_clip: SurfaceClipInfo,
        mut observe: F,
    ) where
        P: BufferCacheParams,
        DT: DeviceTracker,
        F: FnMut(&SampledTextureBinding, &OpenGLTextureCache),
    {
        for stage in 0..NUM_STAGES.min(num_shader_stages) {
            self.prepare_stage_texture_image_bindings(
                buffer_cache,
                texture_cache,
                stage,
                textures,
                samplers,
                images,
                bound_texture_view_ids,
                texture_binding,
                sampler_binding,
                image_binding,
                views,
                views_it,
                sampler_ids,
                sampler_it,
                max_desc_count,
                surface_clip,
                |binding, texture_cache| observe(binding, texture_cache),
            );
        }
    }

    /// Prepare enabled stages, run local diagnostics, then perform the final
    /// texture/sampler/image bulk binds in upstream `ConfigureImpl` order.
    pub fn prepare_and_bind_graphics_texture_image_arrays<P, DT, F, B>(
        &self,
        buffer_cache: &mut BufferCache<P, DT>,
        texture_cache: &mut OpenGLTextureCache,
        num_shader_stages: usize,
        bindings: &mut GraphicsTextureImageBindingState,
        views: &[ImageViewInOut],
        sampler_ids: &[SamplerId],
        max_desc_count: u32,
        surface_clip: SurfaceClipInfo,
        disable_sampler_bind: bool,
        observe: F,
        before_bind: B,
    ) where
        P: BufferCacheParams,
        DT: DeviceTracker,
        F: FnMut(&SampledTextureBinding, &OpenGLTextureCache),
        B: FnOnce(&GraphicsTextureImageBindingState, &OpenGLTextureCache),
    {
        self.prepare_enabled_graphics_texture_image_bindings(
            buffer_cache,
            texture_cache,
            num_shader_stages,
            &mut bindings.textures,
            &mut bindings.samplers,
            &mut bindings.images,
            &mut bindings.bound_texture_view_ids,
            &mut bindings.texture_binding,
            &mut bindings.sampler_binding,
            &mut bindings.image_binding,
            views,
            &mut bindings.views_it,
            sampler_ids,
            &mut bindings.sampler_it,
            max_desc_count,
            surface_clip,
            observe,
        );
        self.clear_host_stage_buffer_pointers(buffer_cache);
        before_bind(bindings, texture_cache);
        self.bind_graphics_texture_image_arrays(
            &bindings.textures,
            &bindings.samplers,
            bindings.texture_binding,
            bindings.sampler_binding,
            &bindings.images,
            bindings.image_binding,
            disable_sampler_bind,
        );
    }

    /// Update graphics buffers and bind host geometry buffers in upstream order.
    ///
    /// Corresponds to `GraphicsPipeline::ConfigureImpl`:
    /// `buffer_cache.UpdateGraphicsBuffers(is_indexed);`
    /// `buffer_cache.BindHostGeometryBuffers(is_indexed);`
    pub fn configure_graphics_buffers<P, DT>(
        &self,
        buffer_cache: &mut BufferCache<P, DT>,
        is_indexed: bool,
    ) where
        P: BufferCacheParams,
        DT: DeviceTracker,
    {
        buffer_cache.update_graphics_buffers(is_indexed);
        buffer_cache.bind_host_geometry_buffers(is_indexed);
    }

    /// Update graphics buffers, bind host geometry buffers, then bind the
    /// current graphics programs in upstream `ConfigureImpl` order.
    pub fn configure_graphics_buffers_and_bind_programs<P, DT>(
        &mut self,
        buffer_cache: &mut BufferCache<P, DT>,
        program_manager: &mut ProgramManager,
        is_indexed: bool,
    ) where
        P: BufferCacheParams,
        DT: DeviceTracker,
    {
        self.configure_graphics_buffers(buffer_cache, is_indexed);
        self.bind_graphics_programs_for_configure_with_program_manager(program_manager);
    }

    /// Update graphics buffers through caller-provided GPU address helpers and
    /// bind host geometry buffers in upstream order.
    pub fn configure_graphics_buffers_with_gpu_resolver<P, DT, A, R, C>(
        &self,
        buffer_cache: &mut BufferCache<P, DT>,
        is_indexed: bool,
        gpu_to_cpu_address: A,
        is_within_gpu_address_range: R,
        max_continuous_range: C,
    ) where
        P: BufferCacheParams,
        DT: DeviceTracker,
        A: FnMut(u64) -> Option<u64>,
        R: FnMut(u64) -> bool,
        C: FnMut(u64, u64) -> u64,
    {
        buffer_cache.update_graphics_buffers_with_gpu_resolver(
            is_indexed,
            gpu_to_cpu_address,
            is_within_gpu_address_range,
            max_continuous_range,
        );
        buffer_cache.bind_host_geometry_buffers(is_indexed);
    }

    /// Update graphics buffers through caller-provided GPU address helpers,
    /// bind host geometry buffers, then bind the current graphics programs in
    /// upstream `ConfigureImpl` order.
    pub fn configure_graphics_buffers_and_bind_programs_with_gpu_resolver<P, DT, A, R, C>(
        &mut self,
        buffer_cache: &mut BufferCache<P, DT>,
        program_manager: &mut ProgramManager,
        is_indexed: bool,
        gpu_to_cpu_address: A,
        is_within_gpu_address_range: R,
        max_continuous_range: C,
    ) where
        P: BufferCacheParams,
        DT: DeviceTracker,
        A: FnMut(u64) -> Option<u64>,
        R: FnMut(u64) -> bool,
        C: FnMut(u64, u64) -> u64,
    {
        self.configure_graphics_buffers_with_gpu_resolver(
            buffer_cache,
            is_indexed,
            gpu_to_cpu_address,
            is_within_gpu_address_range,
            max_continuous_range,
        );
        self.bind_graphics_programs_for_configure_with_program_manager(program_manager);
    }

    /// Bind texture-buffer views, uniform buffers, graphics buffers, and programs in order.
    ///
    /// This owns the upstream-adjacent `ConfigureImpl` slice after framebuffer
    /// binding and before `prepare_stage`: enabled-stage `bind_stage_info`,
    /// Rust's UBO binding bridge, `UpdateGraphicsBuffers(is_indexed)`,
    /// `BindHostGeometryBuffers(is_indexed)`, then program binding.
    #[allow(clippy::too_many_arguments)]
    pub fn bind_texture_buffers_uniforms_then_configure_graphics_buffers_and_programs<P, DT, A>(
        &mut self,
        buffer_cache: &mut BufferCache<P, DT>,
        texture_cache: &OpenGLTextureCache,
        program_manager: &mut ProgramManager,
        num_shader_stages: usize,
        views: &[ImageViewInOut],
        texture_buffer_views_it: &mut usize,
        max_desc_count: u32,
        cb_bindings: &[[ConstBufferBinding; MAX_CB_SLOTS]],
        gpu_to_cpu_address: A,
        is_indexed: bool,
    ) where
        P: BufferCacheParams,
        DT: DeviceTracker,
        A: FnMut(u64) -> Option<u64>,
    {
        self.bind_enabled_stage_texture_buffer_views(
            buffer_cache,
            texture_cache,
            num_shader_stages,
            views,
            texture_buffer_views_it,
            max_desc_count,
        );
        self.bind_graphics_uniform_buffers(buffer_cache, cb_bindings, gpu_to_cpu_address);
        self.configure_graphics_buffers_and_bind_programs(
            buffer_cache,
            program_manager,
            is_indexed,
        );
    }

    /// GPU-resolver variant of the post-framebuffer graphics buffer/program configure slice.
    #[allow(clippy::too_many_arguments)]
    pub fn bind_texture_buffers_uniforms_then_configure_graphics_buffers_and_programs_with_gpu_resolver<
        P,
        DT,
        A,
        G,
        R,
        C,
    >(
        &mut self,
        buffer_cache: &mut BufferCache<P, DT>,
        texture_cache: &OpenGLTextureCache,
        program_manager: &mut ProgramManager,
        num_shader_stages: usize,
        views: &[ImageViewInOut],
        texture_buffer_views_it: &mut usize,
        max_desc_count: u32,
        cb_bindings: &[[ConstBufferBinding; MAX_CB_SLOTS]],
        uniform_gpu_to_cpu_address: A,
        is_indexed: bool,
        graphics_gpu_to_cpu_address: G,
        is_within_gpu_address_range: R,
        max_continuous_range: C,
    ) where
        P: BufferCacheParams,
        DT: DeviceTracker,
        A: FnMut(u64) -> Option<u64>,
        G: FnMut(u64) -> Option<u64>,
        R: FnMut(u64) -> bool,
        C: FnMut(u64, u64) -> u64,
    {
        self.bind_enabled_stage_texture_buffer_views(
            buffer_cache,
            texture_cache,
            num_shader_stages,
            views,
            texture_buffer_views_it,
            max_desc_count,
        );
        self.bind_graphics_uniform_buffers(buffer_cache, cb_bindings, uniform_gpu_to_cpu_address);
        self.configure_graphics_buffers_and_bind_programs_with_gpu_resolver(
            buffer_cache,
            program_manager,
            is_indexed,
            graphics_gpu_to_cpu_address,
            is_within_gpu_address_range,
            max_continuous_range,
        );
    }

    /// Configure post-framebuffer buffers/programs, then prepare and bind texture/image arrays.
    ///
    /// This extends the pipeline-owned post-framebuffer `ConfigureImpl` slice
    /// through `prepare_stage(...)` and the final `glBind*` calls.
    #[allow(clippy::too_many_arguments)]
    pub fn configure_buffers_programs_then_prepare_and_bind_graphics_resources<P, DT, A, O, M, B>(
        &mut self,
        buffer_cache: &mut BufferCache<P, DT>,
        texture_cache: &mut OpenGLTextureCache,
        program_manager: &mut ProgramManager,
        num_shader_stages: usize,
        views: &[ImageViewInOut],
        texture_buffer_views_it: &mut usize,
        max_desc_count: u32,
        cb_bindings: &[[ConstBufferBinding; MAX_CB_SLOTS]],
        gpu_to_cpu_address: A,
        is_indexed: bool,
        bindings: &mut GraphicsTextureImageBindingState,
        sampler_ids: &[SamplerId],
        surface_clip: SurfaceClipInfo,
        disable_sampler_bind: bool,
        mut after_buffer_programs: M,
        observe: O,
        before_bind: B,
    ) where
        P: BufferCacheParams,
        DT: DeviceTracker,
        A: FnMut(u64) -> Option<u64>,
        O: FnMut(&SampledTextureBinding, &OpenGLTextureCache),
        M: FnMut(),
        B: FnOnce(&GraphicsTextureImageBindingState, &OpenGLTextureCache),
    {
        self.bind_texture_buffers_uniforms_then_configure_graphics_buffers_and_programs(
            buffer_cache,
            texture_cache,
            program_manager,
            num_shader_stages,
            views,
            texture_buffer_views_it,
            max_desc_count,
            cb_bindings,
            gpu_to_cpu_address,
            is_indexed,
        );
        after_buffer_programs();
        self.prepare_and_bind_graphics_texture_image_arrays(
            buffer_cache,
            texture_cache,
            num_shader_stages,
            bindings,
            views,
            sampler_ids,
            max_desc_count,
            surface_clip,
            disable_sampler_bind,
            observe,
            before_bind,
        );
    }

    /// Configure post-framebuffer buffers/programs with GPU-memory helpers, then prepare/bind.
    #[allow(clippy::too_many_arguments)]
    pub fn configure_buffers_programs_then_prepare_and_bind_graphics_resources_with_gpu_resolver<
        P,
        DT,
        A,
        R,
        C,
        U,
        O,
        M,
        B,
    >(
        &mut self,
        buffer_cache: &mut BufferCache<P, DT>,
        texture_cache: &mut OpenGLTextureCache,
        program_manager: &mut ProgramManager,
        num_shader_stages: usize,
        views: &[ImageViewInOut],
        texture_buffer_views_it: &mut usize,
        max_desc_count: u32,
        cb_bindings: &[[ConstBufferBinding; MAX_CB_SLOTS]],
        uniform_gpu_to_cpu_address: U,
        is_indexed: bool,
        gpu_to_cpu_address: A,
        is_within_gpu_address_range: R,
        max_continuous_range: C,
        bindings: &mut GraphicsTextureImageBindingState,
        sampler_ids: &[SamplerId],
        surface_clip: SurfaceClipInfo,
        disable_sampler_bind: bool,
        mut after_buffer_programs: M,
        observe: O,
        before_bind: B,
    ) where
        P: BufferCacheParams,
        DT: DeviceTracker,
        A: FnMut(u64) -> Option<u64>,
        R: FnMut(u64) -> bool,
        C: FnMut(u64, u64) -> u64,
        U: FnMut(u64) -> Option<u64>,
        O: FnMut(&SampledTextureBinding, &OpenGLTextureCache),
        M: FnMut(),
        B: FnOnce(&GraphicsTextureImageBindingState, &OpenGLTextureCache),
    {
        self.bind_texture_buffers_uniforms_then_configure_graphics_buffers_and_programs_with_gpu_resolver(
            buffer_cache,
            texture_cache,
            program_manager,
            num_shader_stages,
            views,
            texture_buffer_views_it,
            max_desc_count,
            cb_bindings,
            uniform_gpu_to_cpu_address,
            is_indexed,
            gpu_to_cpu_address,
            is_within_gpu_address_range,
            max_continuous_range,
        );
        after_buffer_programs();
        self.prepare_and_bind_graphics_texture_image_arrays(
            buffer_cache,
            texture_cache,
            num_shader_stages,
            bindings,
            views,
            sampler_ids,
            max_desc_count,
            surface_clip,
            disable_sampler_bind,
            observe,
            before_bind,
        );
    }

    /// Bind the current draw framebuffer through the OpenGL state tracker.
    ///
    /// Corresponds to `GraphicsPipeline::ConfigureImpl`:
    /// `state_tracker.BindFramebuffer(texture_cache.GetFramebuffer()->Handle());`
    pub fn bind_draw_framebuffer(&self, state_tracker: &mut StateTracker, framebuffer: u32) {
        state_tracker.bind_framebuffer(framebuffer);
    }

    /// Update render targets and bind the resulting framebuffer in upstream order.
    ///
    /// Corresponds to `GraphicsPipeline::ConfigureImpl`:
    /// `texture_cache.UpdateRenderTargets(false);`
    /// `state_tracker.BindFramebuffer(texture_cache.GetFramebuffer()->Handle());`
    pub fn update_render_targets_and_bind_framebuffer<D>(
        &self,
        texture_cache: &mut OpenGLTextureCache,
        state_tracker: &mut StateTracker,
        render_targets: &Maxwell3DRenderTargets,
        dirty_flags: &[bool; 256],
        dirty_access: &mut D,
        fallback_size: Extent2D,
    ) -> Option<(u32, u32, u32)>
    where
        D: RenderTargetDirtyFlagAccess,
    {
        trace_gl_pipeline_stall!("[GL_PIPELINE_STALL] update_render_targets_and_bind enter");
        let framebuffer = texture_cache.update_render_targets_and_get_framebuffer_from_snapshot(
            render_targets,
            dirty_flags,
            dirty_access,
            false,
            None,
            fallback_size,
        );
        trace_gl_pipeline_stall!(
            "[GL_PIPELINE_STALL] after_update_render_targets framebuffer={:?}",
            framebuffer
        );
        if let Some((framebuffer, _, _)) = framebuffer {
            self.bind_draw_framebuffer(state_tracker, framebuffer);
            trace_gl_pipeline_stall!("[GL_PIPELINE_STALL] after_bind_draw_framebuffer");
        }
        framebuffer
    }

    /// Bind collected texture, sampler, and image handles in upstream order.
    ///
    /// Corresponds to the final `GraphicsPipeline::ConfigureImpl` bindings:
    /// `glBindTextures(0, texture_binding, textures.data());`
    /// `glBindSamplers(0, sampler_binding, gl_samplers.data());`
    /// `glBindImageTextures(0, image_binding, images.data());`
    pub fn bind_graphics_texture_image_arrays(
        &self,
        textures: &[u32],
        samplers: &[u32],
        texture_binding: usize,
        sampler_binding: usize,
        images: &[u32],
        image_binding: usize,
        disable_sampler_bind: bool,
    ) {
        if texture_binding != 0 {
            debug_assert_eq!(texture_binding, sampler_binding);
            debug_assert!(texture_binding <= textures.len());
            debug_assert!(sampler_binding <= samplers.len());
            unsafe {
                gl::BindTextures(0, texture_binding as i32, textures.as_ptr());
                if disable_sampler_bind {
                    let null_samplers = [0u32; MAX_TEXTURES as usize];
                    gl::BindSamplers(0, sampler_binding as i32, null_samplers.as_ptr());
                } else {
                    gl::BindSamplers(0, sampler_binding as i32, samplers.as_ptr());
                }
            }
        }

        if image_binding != 0 {
            debug_assert!(image_binding <= images.len());
            unsafe {
                gl::BindImageTextures(0, image_binding as i32, images.as_ptr());
            }
        }
    }

    /// Bind texture-buffer and image-buffer descriptors for one graphics stage.
    ///
    /// Corresponds to upstream `GraphicsPipeline::ConfigureImpl`'s
    /// `bind_stage_info` lambda: unbind stale stage texture buffers, iterate
    /// `texture_buffer_descriptors`, then iterate `image_buffer_descriptors`,
    /// and feed `ImageView::{GpuAddr, BufferSize, format}` into
    /// `BufferCache::BindGraphicsTextureBuffer`.
    pub fn bind_stage_texture_buffer_views<P, DT>(
        &self,
        buffer_cache: &mut BufferCache<P, DT>,
        texture_cache: &OpenGLTextureCache,
        stage: usize,
        views: &[ImageViewInOut],
        views_it: &mut usize,
        max_desc_count: u32,
    ) where
        P: BufferCacheParams,
        DT: DeviceTracker,
    {
        let Some(info) = self.stage_infos.get(stage).and_then(Option::as_ref) else {
            return;
        };

        buffer_cache.unbind_graphics_texture_buffers(stage);
        let mut tbo_index = 0usize;

        for desc in &info.texture_buffer_descriptors {
            let count = desc.count.min(max_desc_count);
            for _ in 0..count {
                if *views_it >= views.len() {
                    break;
                }
                let view_id = views[*views_it].id;
                *views_it += 1;

                if view_id.is_valid() {
                    let image_view = texture_cache
                        .get_image_view(view_id)
                        .expect("valid graphics texture-buffer view must be materialized");
                    let gpu_addr = texture_cache.image_view_gpu_addr(view_id).unwrap_or(0);
                    buffer_cache.bind_graphics_texture_buffer(
                        stage,
                        tbo_index,
                        gpu_addr,
                        image_view.buffer_size(),
                        image_view.pixel_format() as u32,
                        false,
                        false,
                    );
                }
                tbo_index += 1;
            }
        }

        for desc in &info.image_buffer_descriptors {
            let count = desc.count.min(max_desc_count);
            for _ in 0..count {
                if *views_it >= views.len() {
                    break;
                }
                let view_id = views[*views_it].id;
                *views_it += 1;

                if view_id.is_valid() {
                    let image_view = texture_cache
                        .get_image_view(view_id)
                        .expect("valid graphics image-buffer view must be materialized");
                    let gpu_addr = texture_cache.image_view_gpu_addr(view_id).unwrap_or(0);
                    buffer_cache.bind_graphics_texture_buffer(
                        stage,
                        tbo_index,
                        gpu_addr,
                        image_view.buffer_size(),
                        image_view.pixel_format() as u32,
                        desc.is_written,
                        true,
                    );
                }
                tbo_index += 1;
            }
        }

        for desc in &info.texture_descriptors {
            let count = desc.count.min(max_desc_count) as usize;
            *views_it = (*views_it).saturating_add(count).min(views.len());
        }
        for desc in &info.image_descriptors {
            let count = desc.count.min(max_desc_count) as usize;
            *views_it = (*views_it).saturating_add(count).min(views.len());
        }
    }

    /// Bind texture-buffer and image-buffer descriptors for all enabled graphics stages.
    ///
    /// Corresponds to the `if constexpr (Spec::enabled_stages[N]) { bind_stage_info(N); }`
    /// sequence in upstream `GraphicsPipeline::ConfigureImpl`, immediately
    /// before `UpdateGraphicsBuffers(is_indexed)`.
    pub fn bind_enabled_stage_texture_buffer_views<P, DT>(
        &self,
        buffer_cache: &mut BufferCache<P, DT>,
        texture_cache: &OpenGLTextureCache,
        num_shader_stages: usize,
        views: &[ImageViewInOut],
        views_it: &mut usize,
        max_desc_count: u32,
    ) where
        P: BufferCacheParams,
        DT: DeviceTracker,
    {
        for stage in 0..NUM_STAGES.min(num_shader_stages) {
            self.bind_stage_texture_buffer_views(
                buffer_cache,
                texture_cache,
                stage,
                views,
                views_it,
                max_desc_count,
            );
        }
    }

    /// Fill sampled-texture and sampler handles for one graphics stage.
    ///
    /// Corresponds to the `info.texture_descriptors` loop in upstream
    /// `GraphicsPipeline::ConfigureImpl::prepare_stage`.
    pub fn bind_stage_sampled_textures<F>(
        &self,
        texture_cache: &OpenGLTextureCache,
        stage: usize,
        views: &[ImageViewInOut],
        views_it: &mut usize,
        sampler_ids: &[SamplerId],
        sampler_it: &mut usize,
        textures: &mut [u32],
        samplers: &mut [u32],
        bound_texture_view_ids: &mut [ImageViewId],
        texture_binding: &mut usize,
        sampler_binding: &mut usize,
        stage_texture_binding: &mut u32,
        max_desc_count: u32,
        mut observe: F,
    ) -> u32
    where
        F: FnMut(&SampledTextureBinding),
    {
        let Some(info) = self.stage_infos.get(stage).and_then(Option::as_ref) else {
            return 0;
        };

        let mut texture_scaling_mask = 0u32;
        for desc in &info.texture_descriptors {
            let count = desc.count.min(max_desc_count);
            for _ in 0..count {
                if *views_it >= views.len() || *texture_binding >= textures.len() {
                    break;
                }
                let view_id = views[*views_it].id;
                *views_it += 1;

                let (handle, view_supports_aniso) = texture_cache
                    .get_image_view(view_id)
                    .map(|iv| {
                        (
                            iv.handle_for_texture_type(desc.texture_type),
                            iv.supports_anisotropy(),
                        )
                    })
                    .unwrap_or((0, false));
                if texture_cache.image_view_is_rescaling(view_id) && *stage_texture_binding < 32 {
                    texture_scaling_mask |= 1u32 << *stage_texture_binding;
                }

                observe(&SampledTextureBinding {
                    stage,
                    texture_binding: *texture_binding,
                    stage_texture_binding: *stage_texture_binding,
                    view_id,
                    handle,
                    texture_type: desc.texture_type as u32,
                    is_depth: desc.is_depth,
                    is_multisample: desc.is_multisample,
                });

                textures[*texture_binding] = handle;
                if *texture_binding < bound_texture_view_ids.len() {
                    bound_texture_view_ids[*texture_binding] = view_id;
                }

                let sampler_handle = if *sampler_it < sampler_ids.len() {
                    let sampler_id = sampler_ids[*sampler_it];
                    *sampler_it += 1;
                    texture_cache
                        .get_sampler(sampler_id)
                        .map(|sampler| {
                            if sampler.has_added_anisotropy() && !view_supports_aniso {
                                sampler.handle_with_default_anisotropy()
                            } else {
                                sampler.handle()
                            }
                        })
                        .unwrap_or(0)
                } else {
                    0
                };
                if *sampler_binding < samplers.len() {
                    samplers[*sampler_binding] = sampler_handle;
                }

                *texture_binding += 1;
                *sampler_binding += 1;
                *stage_texture_binding += 1;
            }
        }
        texture_scaling_mask
    }

    /// Fill storage-image handles for one graphics stage in upstream order.
    ///
    /// Corresponds to the `info.image_descriptors` loop in
    /// `GraphicsPipeline::ConfigureImpl::prepare_stage`.
    pub fn bind_stage_storage_images(
        &self,
        texture_cache: &mut OpenGLTextureCache,
        stage: usize,
        views: &[ImageViewInOut],
        views_it: &mut usize,
        images: &mut [u32],
        image_binding: &mut usize,
        stage_image_binding: &mut u32,
        max_desc_count: u32,
    ) -> u32 {
        let Some(info) = self.stage_infos.get(stage).and_then(Option::as_ref) else {
            return 0;
        };

        let mut image_scaling_mask = 0u32;
        for desc in &info.image_descriptors {
            let count = desc.count.min(max_desc_count);
            for _ in 0..count {
                if *views_it >= views.len() || *image_binding >= images.len() {
                    break;
                }
                let view_id = views[*views_it].id;
                *views_it += 1;

                if desc.is_written && view_id.is_valid() {
                    let parent_id = texture_cache.base.slot_image_views.get(view_id).image_id;
                    if parent_id.is_valid() {
                        texture_cache.base.mark_modification_by_id(parent_id);
                    }
                }

                let handle = texture_cache
                    .get_image_view_mut(view_id)
                    .map(|iv| iv.storage_view(desc.texture_type, desc.format))
                    .unwrap_or(0);
                if texture_cache.image_view_is_rescaling(view_id) && *stage_image_binding < 32 {
                    image_scaling_mask |= 1u32 << *stage_image_binding;
                }
                images[*image_binding] = handle;
                *image_binding += 1;
                *stage_image_binding += 1;
            }
        }
        image_scaling_mask
    }

    /// Upload per-stage rescaling and render-area uniforms in upstream order.
    ///
    /// Corresponds to the `uses_rescaling_uniform` and `uses_render_area`
    /// blocks at the end of `GraphicsPipeline::ConfigureImpl::prepare_stage`.
    pub fn upload_stage_uniforms(
        &self,
        texture_cache: &OpenGLTextureCache,
        stage: usize,
        texture_scaling_mask: u32,
        image_scaling_mask: u32,
        surface_clip: SurfaceClipInfo,
    ) {
        let Some(info) = self.stage_infos.get(stage).and_then(Option::as_ref) else {
            return;
        };
        let use_assembly = self.assembly_programs[0] != 0;

        if info.uses_rescaling_uniform {
            let texture_mask = f32::from_bits(texture_scaling_mask);
            let image_mask = f32::from_bits(image_scaling_mask);
            let down_factor = if texture_cache.is_rescaling_active() {
                settings::values().resolution_info.down_factor
            } else {
                1.0
            };
            if use_assembly {
                program_local_parameter_4f_arb(
                    gl_assembly_stage(stage),
                    0,
                    texture_mask,
                    image_mask,
                    down_factor,
                    0.0,
                );
            } else {
                let program = self.source_programs[stage];
                if program != 0 {
                    unsafe {
                        gl::ProgramUniform4f(
                            program,
                            0,
                            texture_mask,
                            image_mask,
                            down_factor,
                            0.0,
                        );
                    }
                }
            }
        }

        if info.uses_render_area {
            let render_area_width = surface_clip.width as f32;
            let render_area_height = surface_clip.height as f32;
            if use_assembly {
                program_local_parameter_4f_arb(
                    gl_assembly_stage(stage),
                    1,
                    render_area_width,
                    render_area_height,
                    0.0,
                    0.0,
                );
            } else {
                let program = self.source_programs[stage];
                if program != 0 {
                    unsafe {
                        gl::ProgramUniform4f(
                            program,
                            1,
                            render_area_width,
                            render_area_height,
                            0.0,
                            0.0,
                        );
                    }
                }
            }
        }
    }

    /// Configure transform feedback if active.
    ///
    /// Corresponds to `GraphicsPipeline::ConfigureTransformFeedback()`.
    pub fn configure_transform_feedback(&self) {
        if self.num_xfb_attribs != 0 {
            self.configure_transform_feedback_impl();
        }
    }

    /// Returns whether any storage buffer is written.
    pub fn writes_global_memory(&self) -> bool {
        self.writes_global_memory
    }

    /// Returns whether local memory is used.
    pub fn uses_local_memory(&self) -> bool {
        self.uses_local_memory
    }

    /// Whether any compiled GL program has been attached to this pipeline.
    pub fn has_gl_programs(&self) -> bool {
        self.source_programs.iter().any(|h| *h != 0)
    }

    pub fn program_pipeline_handle(&self) -> u32 {
        self.program_pipeline
    }

    /// Compile and link the staged GLSL sources into per-stage separable
    /// GL program objects.
    ///
    /// Mirrors what upstream `GraphicsPipeline`'s constructor does once
    /// `EmitGLSL` has produced source strings: a `glCreateShader` /
    /// `glShaderSource` / `glCompileShader` for each enabled stage,
    /// followed by `glLinkProgram` (with `GL_PROGRAM_SEPARABLE`) to
    /// produce a separable single-stage program. The resulting GL
    /// handles land in `source_programs[stage_index]`, ready for
    /// `configure()` (gap 5) to bind via `glUseProgramStages`.
    ///
    /// Returns `Ok(())` on full success, or `Err(stage_index, message)`
    /// for the first stage that failed to compile or link. On error the
    /// already-allocated handles are deleted so the pipeline ends up in
    /// the same "no GL programs attached" state as a placeholder.
    ///
    /// Safety: must be called with a current OpenGL context. The cache
    /// never invokes this directly — `RasterizerOpenGL::draw` calls it
    /// lazily on first use, where a GL context is guaranteed.
    pub fn build_from_sources(&mut self) -> Result<(), (usize, String)> {
        // Already built — nothing to do.
        if self.has_gl_programs() {
            return Ok(());
        }

        for (stage_index, source) in self.glsl_sources.iter().enumerate() {
            let Some(source) = source else { continue };
            if source.is_empty() {
                continue;
            }
            match unsafe { compile_link_separable(stage_index, source) } {
                Ok(program) => {
                    self.source_programs[stage_index] = program;
                    self.enabled_stages_mask |= 1 << stage_index;
                    trace_pipeline_build(
                        12,
                        &self.key,
                        stage_index as u64,
                        program as u64,
                        source.len() as u64,
                    );
                    if std::env::var_os("RUZU_TRACE_PIPELINE_BUILD").is_some() {
                        let hash = city_hash64(source.as_bytes());
                        log::info!(
                            "[PIPELINE_BUILD] pipeline_key=0x{:016X} stage={} program={} source_hash=0x{:016X} source_bytes={}",
                            self.key.hash_key(),
                            stage_name(stage_index),
                            program,
                            hash,
                            source.len()
                        );
                        if std::env::var_os("RUZU_TRACE_PIPELINE_BUILD_SOURCE").is_some() {
                            eprintln!(
                                "[PIPELINE_BUILD_SOURCE stage={} program={} hash=0x{:016X}]\n{}",
                                stage_name(stage_index),
                                program,
                                hash,
                                source
                            );
                        }
                    }
                }
                Err(msg) => {
                    trace_pipeline_build(13, &self.key, stage_index as u64, 0, source.len() as u64);
                    dump_glsl_on_error(self.key.hash_key(), stage_index, source, &msg);
                    // Roll back any handles we already created.
                    self.delete_gl_programs();
                    return Err((stage_index, msg));
                }
            }
        }

        // Aggregate the per-stage separable programs into a single
        // program-pipeline object. Mirrors upstream's `OGLPipeline` setup.
        unsafe {
            gl::GenProgramPipelines(1, &mut self.program_pipeline);
            for stage_index in 0..NUM_STAGES {
                let prog = self.source_programs[stage_index];
                if prog == 0 {
                    continue;
                }
                gl::UseProgramStages(self.program_pipeline, stage_bit(stage_index), prog);
            }
        }
        if std::env::var_os("RUZU_TRACE_PIPELINE_BUILD").is_some() {
            log::info!(
                "[PIPELINE_BUILD] pipeline_key=0x{:016X} program_pipeline={} programs={:?}",
                self.key.hash_key(),
                self.program_pipeline,
                self.source_programs,
            );
        }
        trace_pipeline_build(
            14,
            &self.key,
            self.program_pipeline as u64,
            self.enabled_stages_mask as u64,
            0,
        );
        dump_glsl_for_pipeline_handle(
            self.program_pipeline,
            self.key.hash_key(),
            &self.glsl_sources,
        );
        dump_pipeline_metadata_for_handle(self);

        Ok(())
    }

    /// Delete every per-stage program handle this pipeline owns and the
    /// program-pipeline object aggregating them.
    /// Called both on `build_from_sources` rollback and on drop.
    fn delete_gl_programs(&mut self) {
        if self.program_pipeline != 0 {
            unsafe { gl::DeleteProgramPipelines(1, &self.program_pipeline) };
            self.program_pipeline = 0;
        }
        for handle in self.source_programs.iter_mut() {
            if *handle != 0 {
                unsafe { gl::DeleteProgram(*handle) };
                *handle = 0;
            }
        }
        self.enabled_stages_mask = 0;
    }

    /// Returns whether the pipeline has finished building.
    ///
    /// Port of `GraphicsPipeline::IsBuilt()`.
    pub fn is_built(&mut self) -> bool {
        if self.is_built {
            return true;
        }
        if self.built_fence.is_null() {
            return false;
        }
        // Check if the GL fence has been signaled
        let status = unsafe { gl::ClientWaitSync(self.built_fence, 0, 0) };
        if status == gl::ALREADY_SIGNALED || status == gl::CONDITION_SATISFIED {
            unsafe {
                gl::DeleteSync(self.built_fence);
            }
            self.built_fence = std::ptr::null();
            self.is_built = true;
            return true;
        }
        false
    }

    #[cfg(test)]
    pub fn set_built_for_test(&mut self, built: bool) {
        self.is_built = built;
    }

    /// Internal: configure transform feedback attributes.
    ///
    /// Port of `GraphicsPipeline::ConfigureTransformFeedbackImpl()`.
    fn configure_transform_feedback_impl(&self) {
        // In the full implementation, this would call:
        // glTransformFeedbackVaryings or glTransformFeedbackBufferRange
        // based on the xfb_attribs and num_xfb_buffers_active
        unsafe {
            for i in 0..self.num_xfb_buffers_active {
                // glBindBufferRange for each active XFB buffer
                let _ = i;
            }
        }
    }

    /// Generate transform feedback state from the pipeline key.
    ///
    /// Port of `GraphicsPipeline::GenerateTransformFeedbackState()`.
    fn generate_transform_feedback_state(&mut self) {
        // In the full implementation, this reads XFB state from the key
        // and populates xfb_attribs and num_xfb_buffers_active
        if !self.key.xfb_enabled() {
            self.num_xfb_attribs = 0;
            self.num_xfb_buffers_active = 0;
            return;
        }
        // XFB state parsing requires the full TransformFeedbackState type
    }

    /// Wait for the pipeline build to complete.
    ///
    /// Port of `GraphicsPipeline::WaitForBuild()`.
    fn wait_for_build(&mut self) {
        if self.is_built {
            return;
        }
        if !self.built_fence.is_null() {
            unsafe {
                gl::ClientWaitSync(self.built_fence, gl::SYNC_FLUSH_COMMANDS_BIT, u64::MAX);
                gl::DeleteSync(self.built_fence);
            }
            self.built_fence = std::ptr::null();
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

/// Map a `glsl_sources` slot index to the corresponding
/// `glUseProgramStages` stage bit. Mirrors upstream's per-stage bit
/// constants used to assemble program pipelines.
fn stage_bit(stage_index: usize) -> u32 {
    match stage_index {
        0 => gl::VERTEX_SHADER_BIT,
        1 => gl::TESS_CONTROL_SHADER_BIT,
        2 => gl::TESS_EVALUATION_SHADER_BIT,
        3 => gl::GEOMETRY_SHADER_BIT,
        4 => gl::FRAGMENT_SHADER_BIT,
        _ => 0,
    }
}

fn stage_name(stage_index: usize) -> &'static str {
    match stage_index {
        0 => "Vertex",
        1 => "TessControl",
        2 => "TessEval",
        3 => "Geometry",
        4 => "Fragment",
        _ => "Unknown",
    }
}

fn dump_glsl_on_error(pipeline_hash: u64, stage_index: usize, source: &str, error: &str) {
    let Some(dir) = std::env::var_os("RUZU_DUMP_GLSL_ON_ERROR") else {
        return;
    };
    let dir = std::path::PathBuf::from(dir);
    if let Err(err) = std::fs::create_dir_all(&dir) {
        log::warn!(
            "Failed to create RUZU_DUMP_GLSL_ON_ERROR dir {}: {}",
            dir.display(),
            err
        );
        return;
    }

    let counter = GLSL_ERROR_DUMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    let stage = stage_name(stage_index);
    let stem = format!(
        "{:04}_pipeline_{:016X}_stage_{}",
        counter, pipeline_hash, stage
    );
    let source_path = dir.join(format!("{}.glsl", stem));
    let log_path = dir.join(format!("{}.log", stem));

    if let Err(err) = std::fs::write(&source_path, source) {
        log::warn!(
            "Failed to dump GLSL source {}: {}",
            source_path.display(),
            err
        );
    }
    let log = format!(
        "pipeline_hash=0x{:016X}\nstage_index={}\nstage={}\nsource_bytes={}\n\n{}",
        pipeline_hash,
        stage_index,
        stage,
        source.len(),
        error
    );
    if let Err(err) = std::fs::write(&log_path, log) {
        log::warn!(
            "Failed to dump GLSL error log {}: {}",
            log_path.display(),
            err
        );
    }
}

fn should_dump_pipeline_handle(handle: u32) -> bool {
    let Some(spec) = std::env::var_os("RUZU_DUMP_GLSL_PIPELINE_HANDLES") else {
        return false;
    };
    let spec = spec.to_string_lossy();
    spec.split(',').any(|raw| {
        let value = raw.trim();
        if value == "*" {
            return true;
        }
        if let Some(hex) = value
            .strip_prefix("0x")
            .or_else(|| value.strip_prefix("0X"))
        {
            return u32::from_str_radix(hex, 16).is_ok_and(|target| target == handle);
        }
        value.parse::<u32>().is_ok_and(|target| target == handle)
    })
}

fn dump_glsl_for_pipeline_handle(
    handle: u32,
    pipeline_hash: u64,
    sources: &[Option<String>; NUM_STAGES],
) {
    if handle == 0 || !should_dump_pipeline_handle(handle) {
        return;
    }
    let dir = std::env::var_os("RUZU_DUMP_GLSL_PIPELINE_DIR")
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|| std::path::PathBuf::from("/tmp/ruzu_pipeline_glsl"));
    if let Err(err) = std::fs::create_dir_all(&dir) {
        log::warn!(
            "Failed to create RUZU_DUMP_GLSL_PIPELINE_DIR {}: {}",
            dir.display(),
            err
        );
        return;
    }
    for (stage_index, source) in sources.iter().enumerate() {
        let Some(source) = source else { continue };
        if source.is_empty() {
            continue;
        }
        let path = dir.join(format!(
            "pipeline_{handle}_key_{pipeline_hash:016X}_stage_{}.glsl",
            stage_name(stage_index)
        ));
        if let Err(err) = std::fs::write(&path, source) {
            log::warn!("Failed to dump GLSL source {}: {}", path.display(), err);
        } else {
            log::info!(
                "[GLSL_PIPELINE_DUMP] pipeline={} stage={} bytes={} path={}",
                handle,
                stage_name(stage_index),
                source.len(),
                path.display()
            );
        }
    }
}

fn dump_pipeline_metadata_for_handle(pipeline: &GraphicsPipeline) {
    let handle = pipeline.program_pipeline;
    if handle == 0 || !should_dump_pipeline_handle(handle) {
        return;
    }
    let dir = std::env::var_os("RUZU_DUMP_GLSL_PIPELINE_DIR")
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|| std::path::PathBuf::from("/tmp/ruzu_pipeline_glsl"));
    if let Err(err) = std::fs::create_dir_all(&dir) {
        log::warn!(
            "Failed to create RUZU_DUMP_GLSL_PIPELINE_DIR {}: {}",
            dir.display(),
            err
        );
        return;
    }

    let mut text = String::new();
    use std::fmt::Write as _;
    let _ = writeln!(text, "pipeline={}", handle);
    let _ = writeln!(text, "pipeline_hash=0x{:016X}", pipeline.key.hash_key());
    let _ = writeln!(
        text,
        "enabled_uniform_buffer_masks={:X?}",
        pipeline.enabled_uniform_buffer_masks
    );
    let _ = writeln!(
        text,
        "base_uniform_bindings={:?}",
        pipeline.base_uniform_bindings
    );
    let _ = writeln!(
        text,
        "base_storage_bindings={:?}",
        pipeline.base_storage_bindings
    );
    let _ = writeln!(
        text,
        "enabled_stages_mask=0x{:X}",
        pipeline.enabled_stages_mask
    );
    for stage in 0..NUM_STAGES {
        let _ = writeln!(
            text,
            "stage{} uniform_sizes={:?}",
            stage, pipeline.uniform_buffer_sizes[stage]
        );
        if let Some(info) = &pipeline.stage_infos[stage] {
            let _ = writeln!(
                text,
                "stage{} info_cbuf_mask=0x{:X} descriptors={:?}",
                stage, info.constant_buffer_mask, info.constant_buffer_descriptors
            );
        } else {
            let _ = writeln!(text, "stage{} info=None", stage);
        }
    }

    let path = dir.join(format!(
        "pipeline_{}_key_{:016X}_metadata.txt",
        handle,
        pipeline.key.hash_key()
    ));
    if let Err(err) = std::fs::write(&path, text) {
        log::warn!(
            "Failed to dump pipeline metadata {}: {}",
            path.display(),
            err
        );
    }
}

/// Compile a GLSL source for `stage_index` and link it into a separable
/// single-stage GL program.
///
/// Returns the linked program handle on success, or a descriptive error
/// string. Mirrors the upstream pattern of producing one separable
/// program per shader stage and binding them via a program-pipeline
/// object at draw time (`glUseProgramStages`).
///
/// Safety: caller must hold a current OpenGL context.
unsafe fn compile_link_separable(stage_index: usize, source: &str) -> Result<u32, String> {
    let stage_enum = gl_stage(stage_index);

    // Compile the shader.
    let shader = gl::CreateShader(stage_enum);
    if shader == 0 {
        return Err(format!(
            "glCreateShader returned 0 for stage {}",
            stage_index
        ));
    }
    let c_src = match std::ffi::CString::new(source) {
        Ok(s) => s,
        Err(_) => {
            gl::DeleteShader(shader);
            return Err("GLSL source contained interior NUL".into());
        }
    };
    let src_ptr = c_src.as_ptr();
    gl::ShaderSource(shader, 1, &src_ptr, std::ptr::null());
    gl::CompileShader(shader);
    let mut status: gl::types::GLint = 0;
    gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut status);
    if status == 0 {
        let msg = gl_info_log_shader(shader);
        gl::DeleteShader(shader);
        return Err(format!("compile failed: {}", msg));
    }

    // Link as a separable program (single shader attached, then linked).
    let program = gl::CreateProgram();
    if program == 0 {
        gl::DeleteShader(shader);
        return Err(format!(
            "glCreateProgram returned 0 for stage {}",
            stage_index
        ));
    }
    gl::ProgramParameteri(program, gl::PROGRAM_SEPARABLE, gl::TRUE as gl::types::GLint);
    gl::AttachShader(program, shader);
    gl::LinkProgram(program);
    gl::DetachShader(program, shader);
    gl::DeleteShader(shader);

    let mut link_status: gl::types::GLint = 0;
    gl::GetProgramiv(program, gl::LINK_STATUS, &mut link_status);
    if link_status == 0 {
        let msg = gl_info_log_program(program);
        gl::DeleteProgram(program);
        return Err(format!("link failed: {}", msg));
    }

    Ok(program)
}

/// Read the shader info log into a Rust `String`. Used by error paths in
/// `compile_link_separable`.
unsafe fn gl_info_log_shader(shader: u32) -> String {
    let mut len: gl::types::GLint = 0;
    gl::GetShaderiv(shader, gl::INFO_LOG_LENGTH, &mut len);
    if len <= 0 {
        return String::new();
    }
    let mut buf = vec![0u8; len as usize];
    gl::GetShaderInfoLog(
        shader,
        len,
        std::ptr::null_mut(),
        buf.as_mut_ptr() as *mut _,
    );
    String::from_utf8_lossy(&buf).into_owned()
}

/// Read the program info log into a Rust `String`. Used by error paths in
/// `compile_link_separable`.
unsafe fn gl_info_log_program(program: u32) -> String {
    let mut len: gl::types::GLint = 0;
    gl::GetProgramiv(program, gl::INFO_LOG_LENGTH, &mut len);
    if len <= 0 {
        return String::new();
    }
    let mut buf = vec![0u8; len as usize];
    gl::GetProgramInfoLog(
        program,
        len,
        std::ptr::null_mut(),
        buf.as_mut_ptr() as *mut _,
    );
    String::from_utf8_lossy(&buf).into_owned()
}

/// Helper: map a stage index to the corresponding GL shader stage enum.
///
/// Corresponds to the anonymous `Stage()` function in gl_graphics_pipeline.cpp.
pub fn gl_stage(stage_index: usize) -> u32 {
    match stage_index {
        0 => gl::VERTEX_SHADER,
        1 => gl::TESS_CONTROL_SHADER,
        2 => gl::TESS_EVALUATION_SHADER,
        3 => gl::GEOMETRY_SHADER,
        4 => gl::FRAGMENT_SHADER,
        _ => panic!("Invalid stage index: {}", stage_index),
    }
}

/// Helper: map a stage index to the corresponding NV assembly program enum.
///
/// Corresponds to the anonymous `AssemblyStage()` function in gl_graphics_pipeline.cpp.
pub fn gl_assembly_stage(stage_index: usize) -> u32 {
    const GL_VERTEX_PROGRAM_NV: u32 = 0x8620;
    const GL_TESS_CONTROL_PROGRAM_NV: u32 = 0x891E;
    const GL_TESS_EVALUATION_PROGRAM_NV: u32 = 0x891F;
    const GL_GEOMETRY_PROGRAM_NV: u32 = 0x8C26;
    const GL_FRAGMENT_PROGRAM_NV: u32 = 0x8870;

    match stage_index {
        0 => GL_VERTEX_PROGRAM_NV,
        1 => GL_TESS_CONTROL_PROGRAM_NV,
        2 => GL_TESS_EVALUATION_PROGRAM_NV,
        3 => GL_GEOMETRY_PROGRAM_NV,
        4 => GL_FRAGMENT_PROGRAM_NV,
        _ => panic!("Invalid stage index: {}", stage_index),
    }
}

/// Translate hardware transform feedback index to ARB_transform_feedback3 tokens.
///
/// Corresponds to `TransformFeedbackEnum()` in gl_graphics_pipeline.cpp.
pub fn transform_feedback_enum(location: u32) -> (i32, i32) {
    let index = location / 4;
    if (8..=39).contains(&index) {
        return (0x8C7D_i32, (index - 8) as i32); // GL_GENERIC_ATTRIB_NV
    }
    if (48..=55).contains(&index) {
        return (0x8C7A_i32, (index - 48) as i32); // GL_TEXTURE_COORD_NV
    }
    const GL_POSITION: i32 = 0x1203;
    match index {
        7 => (GL_POSITION, 0),
        40 => (0x852C_i32, 0), // GL_PRIMARY_COLOR_NV
        41 => (0x852D_i32, 0), // GL_SECONDARY_COLOR_NV
        42 => (0x8C77_i32, 0), // GL_BACK_PRIMARY_COLOR_NV
        43 => (0x8C78_i32, 0), // GL_BACK_SECONDARY_COLOR_NV
        _ => {
            log::warn!("Unimplemented transform feedback index={}", index);
            (GL_POSITION, 0)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pipeline_key_xfb_bits() {
        let mut key = GraphicsPipelineKey::default();
        assert!(!key.xfb_enabled());
        assert!(!key.early_z());

        key.raw = 0b11; // xfb_enabled=1, early_z=1
        assert!(key.xfb_enabled());
        assert!(key.early_z());
    }

    #[test]
    fn pipeline_key_alpha_test_bits_are_hashed_without_xfb() {
        let mut key = GraphicsPipelineKey::default();
        let baseline = key.hash_key();

        key.set_alpha_test_func(4);
        key.alpha_test_ref = 0x3F00_0000;

        assert_eq!(key.alpha_test_func(), 4);
        assert_ne!(key.hash_key(), baseline);
        assert!(key.size() >= std::mem::offset_of!(GraphicsPipelineKey, padding));
    }

    #[test]
    fn pipeline_key_size_varies_by_xfb() {
        let mut key = GraphicsPipelineKey::default();
        let size_no_xfb = key.size();

        key.raw = 1; // xfb_enabled
        let size_xfb = key.size();

        assert!(size_xfb > size_no_xfb);
        assert_eq!(size_xfb, std::mem::size_of::<GraphicsPipelineKey>());
    }

    #[test]
    fn graphics_pipeline_key_hash_matches_cityhash_over_effective_size() {
        let mut key = GraphicsPipelineKey::default();
        key.unique_hashes = [1, 2, 3, 4, 5, 6];
        key.set_early_z(true);
        key.set_gs_input_topology(5);
        key.set_tessellation_primitive(2);
        key.set_tessellation_spacing(1);
        key.set_tessellation_clockwise(true);
        key.set_app_stage(1);

        let size = key.size();
        let bytes = unsafe {
            std::slice::from_raw_parts((&key as *const GraphicsPipelineKey).cast::<u8>(), size)
        };
        assert_eq!(key.hash_key(), city_hash64(bytes));

        key.set_xfb_enabled(true);
        key.xfb_state.layouts[0].stream = 3;
        key.xfb_state.layouts[0].varying_count = 5;
        key.xfb_state.layouts[0].stride = 0x20;
        key.xfb_state.varyings[0][0] =
            crate::transform_feedback::StreamOutLayout::from_raw(0x0403_0201);

        let size = key.size();
        let bytes = unsafe {
            std::slice::from_raw_parts((&key as *const GraphicsPipelineKey).cast::<u8>(), size)
        };
        assert_eq!(key.hash_key(), city_hash64(bytes));
    }

    #[test]
    fn gl_stage_mapping() {
        assert_eq!(gl_stage(0), gl::VERTEX_SHADER);
        assert_eq!(gl_stage(4), gl::FRAGMENT_SHADER);
    }

    #[test]
    fn transform_feedback_generic_attrib() {
        let (token, index) = transform_feedback_enum(8 * 4);
        assert_eq!(token, 0x8C7D); // GL_GENERIC_ATTRIB_NV
        assert_eq!(index, 0);

        let (token, index) = transform_feedback_enum(39 * 4);
        assert_eq!(token, 0x8C7D);
        assert_eq!(index, 31);
    }

    #[test]
    fn transform_feedback_position() {
        let (token, _) = transform_feedback_enum(7 * 4);
        assert_eq!(token, 0x1203); // GL_POSITION
    }

    #[test]
    fn configure_does_not_configure_transform_feedback() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let bind_programs = source
            .find("pub fn bind_graphics_programs_for_configure")
            .expect("GraphicsPipeline::bind_graphics_programs_for_configure");
        let bind_programs_with_manager = source
            .find("pub fn bind_graphics_programs_for_configure_with_program_manager")
            .expect("GraphicsPipeline::bind_graphics_programs_for_configure_with_program_manager");
        let configure = source
            .find("pub fn configure(&mut self")
            .expect("GraphicsPipeline::configure");
        let transform_feedback = source[configure..]
            .find("/// Configure transform feedback")
            .expect("ConfigureTransformFeedback boundary")
            + configure;
        let configure_body = &source[configure..transform_feedback];
        let bind_programs_body = &source[bind_programs..bind_programs_with_manager];
        let bind_programs_with_manager_body = &source[bind_programs_with_manager..configure];

        assert!(bind_programs_body.contains("wait_for_build()"));
        assert!(bind_programs_body.contains("gl::BindProgramPipeline(self.program_pipeline)"));
        assert!(bind_programs_with_manager_body.contains("program_manager"));
        assert!(bind_programs_with_manager_body.contains("bind_assembly_programs"));
        assert!(bind_programs_with_manager_body.contains("program_manager.bind_source_programs"));
        assert!(configure_body.contains("self.bind_graphics_programs_for_configure()"));
        assert!(!configure_body.contains("configure_transform_feedback"));

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime.contains(
            "pipeline.configure_buffers_programs_then_prepare_and_bind_graphics_resources"
        ));
        assert!(!rasterizer_runtime.contains("pipeline.configure(is_indexed)"));
    }

    #[test]
    fn synchronize_graphics_descriptors_owns_upstream_first_configure_side_effect() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn synchronize_graphics_descriptors")
            .expect("synchronize_graphics_descriptors");
        let next = source[method..]
            .find("/// Create a new graphics pipeline")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        assert!(body.contains("texture_cache.base.synchronize_graphics_descriptors(regs)"));

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime.contains(
            "pipeline.synchronize_graphics_descriptors_then_configure_buffer_cache_state("
        ));
        assert!(!rasterizer_runtime.contains("pipeline.synchronize_graphics_descriptors("));
        assert!(
            !rasterizer_runtime.contains(".base\n            .synchronize_graphics_descriptors")
        );
    }

    #[test]
    fn configure_buffer_cache_state_owns_upstream_base_buffer_state_slice() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn configure_buffer_cache_state")
            .expect("configure_buffer_cache_state");
        let next = source[method..]
            .find("/// Configure transform feedback")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        assert!(body.contains("set_uniform_buffers_state"));
        assert!(body.contains("set_graphics_base_uniform_bindings"));
        assert!(body.contains("set_graphics_base_storage_bindings"));
        assert!(body.contains("set_enable_storage_buffers"));

        let rasterizer = include_str!("gl_rasterizer.rs");
        assert!(rasterizer.contains(
            "pipeline.synchronize_graphics_descriptors_then_configure_buffer_cache_state("
        ));
        assert!(
            !rasterizer.contains("pipeline.configure_buffer_cache_state(&mut self.buffer_cache)")
        );
    }

    #[test]
    fn descriptor_sync_then_buffer_base_state_owns_upstream_initial_configure_sequence() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn synchronize_graphics_descriptors_then_configure_buffer_cache_state")
            .expect("synchronize_graphics_descriptors_then_configure_buffer_cache_state");
        let next = source[method..]
            .find("/// Bind enabled graphics uniform buffers")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let sync = body
            .find("self.synchronize_graphics_descriptors(texture_cache, regs)")
            .expect("descriptor sync call");
        let base_state = body
            .find("self.configure_buffer_cache_state(buffer_cache)")
            .expect("buffer-cache base state call");
        assert!(sync < base_state);

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime.contains(
            "pipeline.synchronize_graphics_descriptors_then_configure_buffer_cache_state("
        ));
        assert!(!rasterizer_runtime.contains("pipeline.synchronize_graphics_descriptors("));
        assert!(!rasterizer_runtime.contains("pipeline.configure_buffer_cache_state("));
    }

    #[test]
    fn graphics_pipeline_owns_graphics_engine_state_install_bridge() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn set_graphics_engine_state")
            .expect("set_graphics_engine_state");
        let next = source[method..]
            .find("/// Bind enabled graphics uniform buffers")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        assert!(body.contains("engine_state: Box<dyn EngineState>"));
        assert!(body.contains("buffer_cache.set_engine_state(engine_state)"));

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime
            .contains(".synchronize_then_set_engine_state_and_configure_graphics_framebuffer("));
        assert!(rasterizer_runtime.contains("Box::new(DrawStateEngineAdapter"));
        assert!(!rasterizer_runtime.contains("pipeline.set_graphics_engine_state("));
        assert!(!rasterizer_runtime.contains(
            "self.buffer_cache\n            .set_engine_state(Box::new(DrawStateEngineAdapter"
        ));
    }

    #[test]
    fn graphics_pipeline_owns_engine_state_then_descriptor_framebuffer_sequence() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn set_engine_state_then_configure_graphics_descriptors_and_framebuffer")
            .expect("set_engine_state_then_configure_graphics_descriptors_and_framebuffer");
        let next = source[method..]
            .find("/// Bind host buffer resources")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let set_engine = body
            .find("self.set_graphics_engine_state(buffer_cache, engine_state)")
            .expect("engine-state installation");
        let after_engine = body[set_engine..]
            .find("after_engine_state();")
            .expect("after engine-state callback")
            + set_engine;
        let configure = body[after_engine..]
            .find("self.configure_graphics_descriptors_then_fill_and_bind_framebuffer(")
            .expect("descriptor/fill/framebuffer helper")
            + after_engine;
        assert!(set_engine < after_engine);
        assert!(after_engine < configure);

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime
            .contains(".synchronize_then_set_engine_state_and_configure_graphics_framebuffer("));
        assert!(!rasterizer_runtime
            .contains(".configure_graphics_descriptors_then_fill_and_bind_framebuffer("));
    }

    #[test]
    fn graphics_pipeline_owns_sync_engine_descriptor_framebuffer_sequence() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn synchronize_then_set_engine_state_and_configure_graphics_framebuffer")
            .expect("synchronize/set-engine/configure helper");
        let next = source[method..]
            .find("/// Bind host buffer resources")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let sync = body
            .find("self.synchronize_graphics_descriptors_then_configure_buffer_cache_state(")
            .expect("descriptor sync + base buffer state");
        let after_sync = body[sync..]
            .find("after_descriptor_base_state();")
            .expect("after descriptor/base-state callback")
            + sync;
        let set_engine = body[after_sync..]
            .find("self.set_engine_state_then_configure_graphics_descriptors_and_framebuffer(")
            .expect("set engine then descriptor/framebuffer helper")
            + after_sync;
        assert!(sync < after_sync);
        assert!(after_sync < set_engine);

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime
            .contains(".synchronize_then_set_engine_state_and_configure_graphics_framebuffer("));
        assert!(!rasterizer_runtime
            .contains(".set_engine_state_then_configure_graphics_descriptors_and_framebuffer("));
    }

    #[test]
    fn bind_graphics_uniform_buffers_owns_upstream_ubo_binding_loop() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn bind_graphics_uniform_buffers")
            .expect("bind_graphics_uniform_buffers");
        let next = source[method..]
            .find("/// Bind storage-buffer descriptors")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        assert!(body.contains("enabled_uniform_buffer_masks"));
        assert!(body.contains("cb_bindings[stage][slot as usize]"));
        assert!(body.contains("gpu_to_cpu_address(binding.address).unwrap_or(binding.address)"));
        assert!(body.contains("bind_graphics_uniform_buffer_with_device_addr"));
        assert!(body.contains("disable_graphics_uniform_buffer(stage, slot)"));

        let rasterizer = include_str!("gl_rasterizer.rs");
        assert!(rasterizer.contains(
            "pipeline.configure_buffers_programs_then_prepare_and_bind_graphics_resources"
        ));
        assert!(!rasterizer.contains("pipeline.bind_graphics_uniform_buffers("));
        assert!(!rasterizer.contains("let mut bits = pipeline.enabled_uniform_buffer_masks"));
        assert!(!rasterizer.contains(".bind_graphics_uniform_buffer_with_device_addr(\n                                stage"));
    }

    #[test]
    fn bind_stage_storage_buffers_owns_upstream_storage_buffer_loop() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn bind_stage_storage_buffers<P")
            .expect("bind_stage_storage_buffers");
        let next = source[method..]
            .find("/// Bind host buffer resources")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let unbind = body
            .find("unbind_graphics_storage_buffers(stage)")
            .expect("UnbindGraphicsStorageBuffers");
        let enabled = body[unbind..]
            .find("if !self.use_storage_buffers")
            .expect("storage buffer enable gate")
            + unbind;
        let descriptors = body[enabled..]
            .find("for (ssbo_index, desc) in info.storage_buffers_descriptors")
            .expect("storage descriptor loop")
            + enabled;
        let count_assert = body[descriptors..]
            .find("assert_eq!(\n                desc.count, 1")
            .expect("descriptor count assertion")
            + descriptors;
        let bind = body[count_assert..]
            .find("bind_graphics_storage_buffer(")
            .expect("BindGraphicsStorageBuffer")
            + count_assert;
        assert!(unbind < enabled);
        assert!(enabled < descriptors);
        assert!(descriptors < count_assert);
        assert!(count_assert < bind);
        assert!(body.contains("bind_graphics_storage_buffer_with_gpu_reader"));

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime
            .contains(".synchronize_then_set_engine_state_and_configure_graphics_framebuffer("));
        assert!(!rasterizer_runtime
            .contains("pipeline.configure_enabled_stage_texture_image_descriptors("));
        assert!(!rasterizer_runtime.contains("pipeline.bind_stage_storage_buffers("));
        assert!(
            !rasterizer_runtime.contains("pipeline.bind_stage_storage_buffers_with_gpu_reader(")
        );
        assert!(!rasterizer_runtime
            .contains("self.buffer_cache.unbind_graphics_storage_buffers(stage)"));
        assert!(!rasterizer_runtime.contains("self.buffer_cache.bind_graphics_storage_buffer("));
        assert!(!rasterizer_runtime.contains("self.buffer_cache\n                                .bind_graphics_storage_buffer_with_gpu_reader"));
    }

    #[test]
    fn collect_stage_texture_image_descriptors_owns_upstream_descriptor_walk() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn collect_stage_texture_image_descriptors")
            .expect("collect_stage_texture_image_descriptors");
        let next = source[method..]
            .find("/// Bind host buffer resources")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let texture_buffers = body
            .find("for desc in &info.texture_buffer_descriptors")
            .expect("texture-buffer descriptor loop");
        let image_buffers = body[texture_buffers..]
            .find("for desc in &info.image_buffer_descriptors")
            .expect("image-buffer descriptor loop")
            + texture_buffers;
        let textures = body[image_buffers..]
            .find("for (desc_index, desc) in info.texture_descriptors")
            .expect("sampled texture descriptor loop")
            + image_buffers;
        let sampler = body[textures..]
            .find("sampler_ids.push(get_graphics_sampler_id(tsc_id))")
            .expect("graphics sampler id collection")
            + textures;
        let images = body[sampler..]
            .find("for desc in &info.image_descriptors")
            .expect("storage-image descriptor loop")
            + sampler;
        assert!(texture_buffers < image_buffers);
        assert!(image_buffers < textures);
        assert!(textures < sampler);
        assert!(sampler < images);
        assert!(body.contains("texture_pair(raw, via_header_index)"));
        assert!(body.contains("blacklist: desc.is_written"));

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime
            .contains(".synchronize_then_set_engine_state_and_configure_graphics_framebuffer("));
        assert!(!rasterizer_runtime
            .contains("pipeline.configure_enabled_stage_texture_image_descriptors("));
        assert!(!rasterizer_runtime.contains("pipeline.collect_stage_texture_image_descriptors("));
        assert!(!rasterizer_runtime.contains("for desc in &info.texture_buffer_descriptors"));
        assert!(!rasterizer_runtime.contains("for desc in &info.image_buffer_descriptors"));
        assert!(!rasterizer_runtime.contains("for (desc_index, desc) in info.texture_descriptors"));
        assert!(!rasterizer_runtime.contains("for desc in &info.image_descriptors"));
        assert!(!rasterizer_runtime.contains("texture_pair(raw, via_header_index)"));
    }

    #[test]
    fn configure_enabled_stage_texture_image_descriptors_owns_upstream_config_stage_sequence() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn configure_enabled_stage_texture_image_descriptors")
            .expect("configure_enabled_stage_texture_image_descriptors");
        let next = source[method..]
            .find("/// Fill graphics image views")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let loop_pos = body
            .find("for stage in 0..NUM_STAGES.min(num_shader_stages)")
            .expect("enabled stage loop");
        let storage = body[loop_pos..]
            .find("bind_stage_storage_buffers")
            .expect("storage-buffer binding")
            + loop_pos;
        let collect = body[storage..]
            .find("collect_stage_texture_image_descriptors(")
            .expect("descriptor collection")
            + storage;
        assert!(loop_pos < storage);
        assert!(storage < collect);
        assert!(body.contains("record_detail(32, stage as u64, 0)"));
        assert!(body.contains("record_detail(35, stage as u64, 0)"));

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime
            .contains(".synchronize_then_set_engine_state_and_configure_graphics_framebuffer("));
        assert!(!rasterizer_runtime
            .contains("pipeline.configure_enabled_stage_texture_image_descriptors("));
        assert!(!rasterizer_runtime.contains("for stage in 0..NUM_STAGES.min(num_shader_stages)"));
    }

    #[test]
    fn fill_and_materialize_graphics_image_views_owns_upstream_fill_slice() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn fill_and_materialize_graphics_image_views")
            .expect("fill_and_materialize_graphics_image_views");
        let next = source[method..]
            .find("/// Fill graphics image views, then update render targets")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let fill = body
            .find("fill_graphics_image_views(views, has_images)")
            .expect("FillGraphicsImageViews");
        let materialize_views = body[fill..]
            .find("materialize_views(views)")
            .expect("materialize views path")
            + fill;
        let materialize_samplers = body[materialize_views..]
            .find("materialize_samplers(sampler_ids)")
            .expect("materialize samplers")
            + materialize_views;
        assert!(fill < materialize_views);
        assert!(materialize_views < materialize_samplers);
        assert!(!body.contains("fill_graphics_image_views_with_gpu_reader"));
        assert!(!body.contains("materialize_views_with_gpu_reader"));
        assert!(!body.contains("read_gpu"));

        let rasterizer = include_str!("gl_rasterizer.rs");
        assert!(rasterizer
            .contains("synchronize_then_set_engine_state_and_configure_graphics_framebuffer("));
        assert!(!rasterizer.contains("pipeline.fill_and_materialize_graphics_image_views("));
        assert!(!rasterizer.contains("let mut read_gpu = |gpu_addr, out: &mut [u8]|"));
        assert!(!rasterizer.contains("fill_and_materialize_graphics_image_views::<"));
        assert!(!rasterizer.contains(".fill_graphics_image_views("));
        assert!(!rasterizer.contains(".fill_graphics_image_views_with_gpu_reader("));
        assert!(!rasterizer.contains(".materialize_views("));
        assert!(!rasterizer.contains(".materialize_views_with_gpu_reader("));
        assert!(!rasterizer.contains(".materialize_samplers("));

        let texture_cache = include_str!("gl_texture_cache.rs");
        assert!(!texture_cache.contains("pub fn fill_graphics_image_views_with_gpu_reader"));
        assert!(!texture_cache.contains("pub fn materialize_views_with_gpu_reader"));
    }

    #[test]
    fn fill_then_update_render_targets_and_bind_framebuffer_owns_upstream_sequence() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find(
                "pub fn fill_graphics_image_views_then_update_render_targets_and_bind_framebuffer",
            )
            .expect("fill/update/bind helper");
        let next = source[method..]
            .find("/// Bind host buffer resources")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let fill = body
            .find("self.fill_and_materialize_graphics_image_views(")
            .expect("FillGraphicsImageViews bridge");
        let update = body[fill..]
            .find("self.update_render_targets_and_bind_framebuffer(")
            .expect("UpdateRenderTargets + BindFramebuffer bridge")
            + fill;
        assert!(fill < update);

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime
            .contains(".synchronize_then_set_engine_state_and_configure_graphics_framebuffer("));
        assert!(!rasterizer_runtime.contains(
            "pipeline.fill_graphics_image_views_then_update_render_targets_and_bind_framebuffer("
        ));
        assert!(!rasterizer_runtime.contains("pipeline.fill_and_materialize_graphics_image_views("));
        assert!(
            !rasterizer_runtime.contains("pipeline.update_render_targets_and_bind_framebuffer(")
        );
    }

    #[test]
    fn configure_graphics_descriptors_then_fill_and_bind_framebuffer_owns_upstream_sequence() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn configure_graphics_descriptors_then_fill_and_bind_framebuffer")
            .expect("configure_graphics_descriptors_then_fill_and_bind_framebuffer");
        let next = source[method..]
            .find("/// Bind host buffer resources")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let collect = body
            .find("self.configure_enabled_stage_texture_image_descriptors(")
            .expect("config_stage descriptor collection");
        let sampler = body[collect..]
            .find("texture_cache.base.get_graphics_sampler_id(tsc_id)")
            .expect("TextureCache-owned sampler id lookup")
            + collect;
        let fill_update_bind = body[sampler..]
            .find("self.fill_graphics_image_views_then_update_render_targets_and_bind_framebuffer(")
            .expect("FillGraphicsImageViews -> UpdateRenderTargets -> BindFramebuffer")
            + sampler;
        assert!(collect < sampler);
        assert!(sampler < fill_update_bind);

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime
            .contains(".synchronize_then_set_engine_state_and_configure_graphics_framebuffer("));
        assert!(!rasterizer_runtime
            .contains("pipeline.configure_enabled_stage_texture_image_descriptors("));
        assert!(!rasterizer_runtime.contains(
            "pipeline.fill_graphics_image_views_then_update_render_targets_and_bind_framebuffer("
        ));
    }

    #[test]
    fn bind_host_stage_buffers_owns_upstream_prepare_stage_buffer_slice() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn prepare_stage_host_buffer_bindings")
            .expect("prepare_stage_host_buffer_bindings");
        let next = source[method..]
            .find("/// Clear the transient host texture/image pointer bridge")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let set_image_pointers = body.find("set_image_pointers").expect("SetImagePointers");
        let bind_host_stage_buffers = body
            .find("bind_host_stage_buffers(stage)")
            .expect("BindHostStageBuffers");
        let texture_buffer_count = body
            .find("num_texture_buffers[stage]")
            .expect("texture-buffer slot skip");
        let image_buffer_count = body
            .find("num_image_buffers[stage]")
            .expect("image-buffer slot skip");
        assert!(set_image_pointers < bind_host_stage_buffers);
        assert!(bind_host_stage_buffers < texture_buffer_count);
        assert!(texture_buffer_count < image_buffer_count);
        assert!(body.contains(".get(stage)"));

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime.contains(
            "pipeline.configure_buffers_programs_then_prepare_and_bind_graphics_resources"
        ));
        assert!(!rasterizer_runtime.contains("pipeline.prepare_stage_host_buffer_bindings("));
    }

    #[test]
    fn prepare_stage_texture_image_bindings_owns_upstream_prepare_stage_body() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn prepare_stage_texture_image_bindings")
            .expect("prepare_stage_texture_image_bindings");
        let next = source[method..]
            .find("/// Prepare all enabled graphics stages")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let host = body
            .find("prepare_stage_host_buffer_bindings(")
            .expect("host stage buffer binding");
        let sampled = body[host..]
            .find("bind_stage_sampled_textures(")
            .expect("sampled texture binding")
            + host;
        let storage = body[sampled..]
            .find("bind_stage_storage_images(")
            .expect("storage image binding")
            + sampled;
        let uniforms = body[storage..]
            .find("upload_stage_uniforms(")
            .expect("uniform upload")
            + storage;
        assert!(host < sampled);
        assert!(sampled < storage);
        assert!(storage < uniforms);
        assert!(body.contains("observe(binding, texture_cache_ref)"));

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime.contains(
            "pipeline.configure_buffers_programs_then_prepare_and_bind_graphics_resources"
        ));
        assert!(!rasterizer_runtime.contains("pipeline.upload_stage_uniforms("));
    }

    #[test]
    fn prepare_enabled_graphics_texture_image_bindings_owns_upstream_stage_sequence() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn prepare_enabled_graphics_texture_image_bindings")
            .expect("prepare_enabled_graphics_texture_image_bindings");
        let next = source[method..]
            .find("/// Update graphics buffers")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let loop_pos = body
            .find("for stage in 0..NUM_STAGES.min(num_shader_stages)")
            .expect("enabled stage loop");
        let prepare = body[loop_pos..]
            .find("prepare_stage_texture_image_bindings(")
            .expect("prepare_stage call")
            + loop_pos;
        assert!(loop_pos < prepare);
        assert!(body.contains("|binding, texture_cache| observe(binding, texture_cache)"));

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime.contains(
            "pipeline.configure_buffers_programs_then_prepare_and_bind_graphics_resources"
        ));
        assert!(!rasterizer_runtime.contains("prepare_graphics_stage!"));
    }

    #[test]
    fn prepare_and_bind_graphics_texture_image_arrays_owns_upstream_prepare_then_bulk_bind() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn prepare_and_bind_graphics_texture_image_arrays")
            .expect("prepare_and_bind_graphics_texture_image_arrays");
        let next = source[method..]
            .find("/// Update graphics buffers")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let prepare = body
            .find("self.prepare_enabled_graphics_texture_image_bindings(")
            .expect("prepare-stage sequence");
        let clear = body[prepare..]
            .find("self.clear_host_stage_buffer_pointers(buffer_cache)")
            .expect("host pointer cleanup")
            + prepare;
        let callback = body[clear..]
            .find("before_bind(bindings, texture_cache)")
            .expect("local diagnostic callback")
            + clear;
        let bind = body[callback..]
            .find("self.bind_graphics_texture_image_arrays(")
            .expect("final bulk bind")
            + callback;
        assert!(prepare < clear);
        assert!(clear < callback);
        assert!(callback < bind);

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime.contains(
            "pipeline.configure_buffers_programs_then_prepare_and_bind_graphics_resources"
        ));
        assert!(!rasterizer_runtime
            .contains("pipeline.prepare_enabled_graphics_texture_image_bindings("));
        assert!(!rasterizer_runtime.contains("pipeline.bind_graphics_texture_image_arrays("));
    }

    #[test]
    fn texture_buffer_binding_is_separated_from_prepare_stage_slots() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn bind_stage_texture_buffer_views")
            .expect("bind_stage_texture_buffer_views");
        let next = source[method..]
            .find("/// Bind texture-buffer and image-buffer descriptors for all enabled graphics stages")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let unbind = body
            .find("unbind_graphics_texture_buffers(stage)")
            .expect("UnbindGraphicsTextureBuffers");
        let texture_buffers = body[unbind..]
            .find("for desc in &info.texture_buffer_descriptors")
            .expect("texture-buffer loop")
            + unbind;
        let image_buffers = body[texture_buffers..]
            .find("for desc in &info.image_buffer_descriptors")
            .expect("image-buffer loop")
            + texture_buffers;
        let skip_textures = body[image_buffers..]
            .find("for desc in &info.texture_descriptors")
            .expect("sampled descriptor skip")
            + image_buffers;
        assert!(unbind < texture_buffers);
        assert!(texture_buffers < image_buffers);
        assert!(image_buffers < skip_textures);
        assert!(!body.contains("texture_binding"));
        assert!(!body.contains("image_binding"));
        assert!(!body.contains("textures["));
        assert!(!body.contains("images["));

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        let first = rasterizer_runtime
            .find("let mut texture_buffer_views_it")
            .expect("bind_stage_info iterator");
        let combined = rasterizer_runtime[first..]
            .find("pipeline.configure_buffers_programs_then_prepare_and_bind_graphics_resources")
            .expect("combined configure/prepare/bind helper")
            + first;
        assert!(first < combined);
        assert!(!rasterizer_runtime.contains("stage_texture_pointer_offsets[stage]"));
        assert!(!rasterizer_runtime.contains("pipeline.bind_stage_sampled_textures("));
    }

    #[test]
    fn bind_enabled_stage_texture_buffer_views_owns_upstream_stage_sequence() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn bind_enabled_stage_texture_buffer_views")
            .expect("bind_enabled_stage_texture_buffer_views");
        let next = source[method..]
            .find("/// Fill sampled-texture")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let loop_pos = body
            .find("for stage in 0..NUM_STAGES.min(num_shader_stages)")
            .expect("enabled stage loop");
        let bind = body[loop_pos..]
            .find("bind_stage_texture_buffer_views(")
            .expect("bind_stage_info call")
            + loop_pos;
        assert!(loop_pos < bind);

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime.contains(
            "pipeline.configure_buffers_programs_then_prepare_and_bind_graphics_resources"
        ));
        assert!(!rasterizer_runtime.contains("pipeline.bind_enabled_stage_texture_buffer_views("));
        assert!(!rasterizer_runtime.contains("pipeline.bind_stage_texture_buffer_views("));
    }

    #[test]
    fn configure_graphics_buffers_owns_upstream_update_then_geometry_bind_slice() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn configure_graphics_buffers<P")
            .expect("configure_graphics_buffers");
        let next = source[method..]
            .find("/// Update graphics buffers through caller-provided")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let update = body
            .find("update_graphics_buffers(is_indexed)")
            .expect("UpdateGraphicsBuffers");
        let bind = body
            .find("bind_host_geometry_buffers(is_indexed)")
            .expect("BindHostGeometryBuffers");
        assert!(update < bind);

        let method = source
            .find("pub fn configure_graphics_buffers_and_bind_programs<P")
            .expect("configure_graphics_buffers_and_bind_programs");
        let next = source[method..]
            .find("/// Update graphics buffers through caller-provided")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];
        let configure_buffers = body
            .find("self.configure_graphics_buffers(buffer_cache, is_indexed)")
            .expect("graphics buffer helper");
        let bind_programs = body[configure_buffers..]
            .find("self.bind_graphics_programs_for_configure_with_program_manager(program_manager)")
            .expect("program bind after graphics buffers")
            + configure_buffers;
        assert!(configure_buffers < bind_programs);

        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn configure_graphics_buffers_with_gpu_resolver")
            .expect("configure_graphics_buffers_with_gpu_resolver");
        let next = source[method..]
            .find("/// Update graphics buffers through caller-provided GPU address helpers,")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];
        let update = body
            .find("update_graphics_buffers_with_gpu_resolver")
            .expect("UpdateGraphicsBuffers with resolver");
        let bind = body
            .find("bind_host_geometry_buffers(is_indexed)")
            .expect("BindHostGeometryBuffers");
        assert!(update < bind);

        let method = source
            .find("pub fn configure_graphics_buffers_and_bind_programs_with_gpu_resolver")
            .expect("configure_graphics_buffers_and_bind_programs_with_gpu_resolver");
        let next = source[method..]
            .find("/// Bind texture-buffer views")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];
        let configure_buffers = body
            .find("self.configure_graphics_buffers_with_gpu_resolver(")
            .expect("graphics buffer helper with resolver");
        let bind_programs = body[configure_buffers..]
            .find("self.bind_graphics_programs_for_configure_with_program_manager(program_manager)")
            .expect("program bind after graphics buffers with resolver")
            + configure_buffers;
        assert!(configure_buffers < bind_programs);

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime.contains(
            "pipeline.configure_buffers_programs_then_prepare_and_bind_graphics_resources"
        ));
        assert!(
            rasterizer_runtime.contains("let mut program_manager = self.program_manager.lock()")
        );
        assert!(!rasterizer_runtime.contains("pipeline.bind_graphics_programs_for_configure()"));
    }

    #[test]
    fn post_framebuffer_buffer_program_configure_slice_is_pipeline_owned() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find(
                "pub fn bind_texture_buffers_uniforms_then_configure_graphics_buffers_and_programs<",
            )
            .expect("combined post-framebuffer configure helper");
        let next = source[method..]
            .find("/// GPU-resolver variant")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let texture_buffers = body
            .find("self.bind_enabled_stage_texture_buffer_views(")
            .expect("bind_stage_info bridge");
        let uniforms = body[texture_buffers..]
            .find("self.bind_graphics_uniform_buffers(")
            .expect("uniform buffer bridge")
            + texture_buffers;
        let buffers_programs = body[uniforms..]
            .find("self.configure_graphics_buffers_and_bind_programs(")
            .expect("graphics buffer/program configure")
            + uniforms;
        assert!(texture_buffers < uniforms);
        assert!(uniforms < buffers_programs);

        let method = source
            .find("pub fn bind_texture_buffers_uniforms_then_configure_graphics_buffers_and_programs_with_gpu_resolver")
            .expect("combined post-framebuffer configure helper with resolver");
        let next = source[method..]
            .find("/// Configure post-framebuffer buffers/programs, then prepare")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];
        let texture_buffers = body
            .find("self.bind_enabled_stage_texture_buffer_views(")
            .expect("bind_stage_info bridge with resolver");
        let uniforms = body[texture_buffers..]
            .find("self.bind_graphics_uniform_buffers(")
            .expect("uniform buffer bridge with resolver")
            + texture_buffers;
        let buffers_programs = body[uniforms..]
            .find("self.configure_graphics_buffers_and_bind_programs_with_gpu_resolver(")
            .expect("graphics buffer/program configure with resolver")
            + uniforms;
        assert!(texture_buffers < uniforms);
        assert!(uniforms < buffers_programs);

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime.contains(
            "pipeline.configure_buffers_programs_then_prepare_and_bind_graphics_resources"
        ));
        assert!(!rasterizer_runtime.contains(
            "pipeline.bind_texture_buffers_uniforms_then_configure_graphics_buffers_and_programs"
        ));
        assert!(!rasterizer_runtime
            .contains("pipeline.prepare_and_bind_graphics_texture_image_arrays("));
        assert!(!rasterizer_runtime.contains("pipeline.bind_graphics_uniform_buffers("));
        assert!(!rasterizer_runtime.contains("pipeline.bind_enabled_stage_texture_buffer_views("));
        assert!(
            !rasterizer_runtime.contains("pipeline.configure_graphics_buffers_and_bind_programs(")
        );
        assert!(!rasterizer_runtime
            .contains("pipeline.configure_graphics_buffers_and_bind_programs_with_gpu_resolver("));
    }

    #[test]
    fn post_framebuffer_configure_then_prepare_bind_slice_is_pipeline_owned() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn configure_buffers_programs_then_prepare_and_bind_graphics_resources<")
            .expect("post-framebuffer configure/prepare/bind helper");
        let next = source[method..]
            .find("/// Configure post-framebuffer buffers/programs with GPU-memory helpers")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let configure = body
            .find(
                "self.bind_texture_buffers_uniforms_then_configure_graphics_buffers_and_programs(",
            )
            .expect("buffer/program configure helper");
        let after_configure = body[configure..]
            .find("after_buffer_programs();")
            .expect("after buffer/program callback")
            + configure;
        let prepare = body[after_configure..]
            .find("self.prepare_and_bind_graphics_texture_image_arrays(")
            .expect("prepare/bulk-bind helper")
            + after_configure;
        assert!(configure < after_configure);
        assert!(after_configure < prepare);

        let method = source
            .find("pub fn configure_buffers_programs_then_prepare_and_bind_graphics_resources_with_gpu_resolver")
            .expect("post-framebuffer configure/prepare/bind helper with resolver");
        let next = source[method..]
            .find("/// Bind the current draw framebuffer")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];
        let configure = body
            .find("self.bind_texture_buffers_uniforms_then_configure_graphics_buffers_and_programs_with_gpu_resolver(")
            .expect("buffer/program configure helper with resolver");
        let after_configure = body[configure..]
            .find("after_buffer_programs();")
            .expect("after buffer/program callback with resolver")
            + configure;
        let prepare = body[after_configure..]
            .find("self.prepare_and_bind_graphics_texture_image_arrays(")
            .expect("prepare/bulk-bind helper with resolver")
            + after_configure;
        assert!(configure < after_configure);
        assert!(after_configure < prepare);

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime.contains(
            "pipeline.configure_buffers_programs_then_prepare_and_bind_graphics_resources"
        ));
        assert!(!rasterizer_runtime.contains(
            "pipeline.bind_texture_buffers_uniforms_then_configure_graphics_buffers_and_programs"
        ));
        assert!(!rasterizer_runtime
            .contains("pipeline.prepare_and_bind_graphics_texture_image_arrays("));
    }

    #[test]
    fn bind_draw_framebuffer_owns_upstream_state_tracker_bind_slice() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn bind_draw_framebuffer")
            .expect("bind_draw_framebuffer");
        let next = source[method..]
            .find("/// Configure transform feedback")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        assert!(body.contains("state_tracker.bind_framebuffer(framebuffer)"));
        assert!(!body.contains("gl::BindFramebuffer"));

        let rasterizer = include_str!("gl_rasterizer.rs");
        assert!(rasterizer.contains(
            "fill_graphics_image_views_then_update_render_targets_and_bind_framebuffer("
        ));
        assert!(!rasterizer.contains("gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, framebuffer)"));
    }

    #[test]
    fn update_render_targets_and_bind_framebuffer_owns_upstream_pair() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn update_render_targets_and_bind_framebuffer")
            .expect("update_render_targets_and_bind_framebuffer");
        let next = source[method..]
            .find("/// Configure transform feedback")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let update = body
            .find("update_render_targets_and_get_framebuffer_from_snapshot")
            .expect("UpdateRenderTargets bridge");
        let is_clear_false = body[update..]
            .find("false")
            .expect("UpdateRenderTargets(false)")
            + update;
        let bind = body[is_clear_false..]
            .find("self.bind_draw_framebuffer(state_tracker, framebuffer)")
            .expect("StateTracker framebuffer bind after update")
            + is_clear_false;
        assert!(update < is_clear_false);
        assert!(is_clear_false < bind);

        let rasterizer = include_str!("gl_rasterizer.rs");
        assert!(rasterizer.contains(
            "fill_graphics_image_views_then_update_render_targets_and_bind_framebuffer("
        ));
    }

    #[test]
    fn bind_graphics_texture_image_arrays_owns_upstream_bulk_binds() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn bind_graphics_texture_image_arrays")
            .expect("bind_graphics_texture_image_arrays");
        let next = source[method..]
            .find("/// Configure transform feedback")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let assert_counts = body
            .find("debug_assert_eq!(texture_binding, sampler_binding)")
            .expect("upstream texture/sampler binding count assertion");
        let bind_textures = body.find("gl::BindTextures").expect("glBindTextures");
        let bind_samplers = body[bind_textures..]
            .find("gl::BindSamplers")
            .expect("glBindSamplers")
            + bind_textures;
        let bind_images = body[bind_samplers..]
            .find("gl::BindImageTextures")
            .expect("glBindImageTextures")
            + bind_samplers;
        assert!(assert_counts < bind_textures);
        assert!(bind_textures < bind_samplers);
        assert!(bind_samplers < bind_images);

        let rasterizer = include_str!("gl_rasterizer.rs");
        assert!(source.contains("self.bind_graphics_texture_image_arrays("));
        assert!(rasterizer.contains(
            "pipeline.configure_buffers_programs_then_prepare_and_bind_graphics_resources"
        ));
        assert!(rasterizer.contains("sampler_binding"));
        assert!(!rasterizer.contains("gl::BindTextures(0, texture_binding as i32"));
        assert!(!rasterizer.contains("gl::BindImageTextures(0, image_binding as i32"));
    }

    #[test]
    fn graphics_texture_image_binding_state_owns_upstream_configure_arrays() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let state = source
            .find("pub struct GraphicsTextureImageBindingState")
            .expect("GraphicsTextureImageBindingState");
        let next = source[state..]
            .find("fn trace_pipeline_build")
            .expect("next item boundary")
            + state;
        let body = &source[state..next];

        assert!(body.contains("pub textures: [u32; MAX_TEXTURES as usize]"));
        assert!(body.contains("pub samplers: [u32; MAX_TEXTURES as usize]"));
        assert!(body.contains("pub images: [u32; MAX_IMAGES as usize]"));
        assert!(body.contains("pub texture_binding: usize"));
        assert!(body.contains("pub sampler_binding: usize"));
        assert!(body.contains("pub image_binding: usize"));
        assert!(body.contains("pub sampler_it: usize"));
        assert!(body.contains("pub views_it: usize"));

        let rasterizer = include_str!("gl_rasterizer.rs");
        let runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(runtime.contains("GraphicsTextureImageBindingState::new()"));
        assert!(!runtime.contains("let mut textures: [u32;"));
        assert!(!runtime.contains("let mut gl_samplers: [u32;"));
        assert!(!runtime.contains("let mut images: [u32;"));
        assert!(!runtime.contains("let mut texture_binding: usize"));
        assert!(source.contains("&mut bindings.textures"));
        assert!(runtime.contains("graphics_bindings.texture_binding"));
        assert!(runtime.contains(
            "pipeline.configure_buffers_programs_then_prepare_and_bind_graphics_resources"
        ));
    }

    #[test]
    fn bind_stage_sampled_textures_owns_upstream_sampled_texture_loop() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn bind_stage_sampled_textures")
            .expect("bind_stage_sampled_textures");
        let next = source[method..]
            .find("/// Fill storage-image handles")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let descriptors = body
            .find("for desc in &info.texture_descriptors")
            .expect("texture descriptor loop");
        let image_view = body[descriptors..]
            .find("get_image_view(view_id)")
            .expect("GetImageView")
            + descriptors;
        let handle = body[image_view..]
            .find("handle_for_texture_type(desc.texture_type)")
            .expect("ImageView::Handle")
            + image_view;
        let rescaling = body[handle..]
            .find("image_view_is_rescaling")
            .expect("IsRescaling")
            + handle;
        let sampler = body[rescaling..]
            .find("get_sampler(sampler_id)")
            .expect("GetSampler")
            + rescaling;
        let fallback = body[sampler..]
            .find("handle_with_default_anisotropy")
            .expect("anisotropy fallback")
            + sampler;
        let increment = body[fallback..]
            .find("*texture_binding += 1")
            .expect("texture binding increment")
            + fallback;

        assert!(descriptors < image_view);
        assert!(image_view < handle);
        assert!(handle < rescaling);
        assert!(rescaling < sampler);
        assert!(sampler < fallback);
        assert!(fallback < increment);

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime.contains(
            "pipeline.configure_buffers_programs_then_prepare_and_bind_graphics_resources"
        ));
        assert!(!rasterizer_runtime.contains("pipeline.bind_stage_sampled_textures("));
        assert!(!rasterizer_runtime.contains("handle_for_texture_type(desc.texture_type)"));
        assert!(!rasterizer_runtime.contains("handle_with_default_anisotropy()"));
    }

    #[test]
    fn bind_stage_storage_images_owns_upstream_storage_image_loop() {
        let source = include_str!("gl_graphics_pipeline.rs");
        let method = source
            .find("pub fn bind_stage_storage_images")
            .expect("bind_stage_storage_images");
        let next = source[method..]
            .find("/// Configure transform feedback")
            .expect("next method boundary")
            + method;
        let body = &source[method..next];

        let descriptors = body
            .find("for desc in &info.image_descriptors")
            .expect("image descriptor loop");
        let mark = body[descriptors..]
            .find("mark_modification_by_id")
            .expect("MarkModification")
            + descriptors;
        let storage = body[mark..].find(".storage_view(").expect("StorageView") + mark;
        let rescaling = body[storage..]
            .find("image_view_is_rescaling")
            .expect("IsRescaling")
            + storage;
        assert!(descriptors < mark);
        assert!(mark < storage);
        assert!(storage < rescaling);

        let rasterizer = include_str!("gl_rasterizer.rs");
        let rasterizer_runtime = rasterizer
            .split(
                "
#[cfg(test)]
mod tests",
            )
            .next()
            .unwrap_or(rasterizer);
        assert!(rasterizer_runtime.contains(
            "pipeline.configure_buffers_programs_then_prepare_and_bind_graphics_resources"
        ));
        assert!(!rasterizer_runtime.contains("pipeline.bind_stage_storage_images("));
    }
}
