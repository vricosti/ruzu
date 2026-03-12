// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_graphics_pipeline.h and gl_graphics_pipeline.cpp
//!
//! OpenGL graphics pipeline management — compiles and configures vertex/fragment/etc shaders.

use std::sync::{Condvar, Mutex};

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

/// Key used to identify a unique graphics pipeline configuration.
///
/// Corresponds to `OpenGL::GraphicsPipelineKey`.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct GraphicsPipelineKey {
    pub unique_hashes: [u64; 6],
    /// Packed bitfield: xfb_enabled(1), early_z(1), gs_input_topology(4),
    /// tessellation_primitive(2), tessellation_spacing(2), tessellation_clockwise(1),
    /// app_stage(3).
    pub raw: u32,
    pub padding: [u32; 3],
    // TODO: TransformFeedbackState xfb_state when that type is ported
}

impl GraphicsPipelineKey {
    /// Hash the key, considering only relevant bytes (smaller if xfb not enabled).
    pub fn hash_key(&self) -> u64 {
        let size = self.size();
        let bytes: &[u8] = unsafe {
            std::slice::from_raw_parts(self as *const Self as *const u8, size)
        };
        // Placeholder hash until cityhash is available
        let mut h: u64 = 0xcbf29ce484222325;
        for &b in bytes {
            h ^= b as u64;
            h = h.wrapping_mul(0x100000001b3);
        }
        h
    }

    /// Returns the xfb_enabled bit.
    pub fn xfb_enabled(&self) -> bool {
        (self.raw & 1) != 0
    }

    /// Returns the early_z bit.
    pub fn early_z(&self) -> bool {
        ((self.raw >> 1) & 1) != 0
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

/// OpenGL graphics pipeline.
///
/// Corresponds to `OpenGL::GraphicsPipeline`.
pub struct GraphicsPipeline {
    pub key: GraphicsPipelineKey,

    /// Source program handles per stage (GLSL or SPIR-V).
    pub source_programs: [u32; NUM_STAGES],
    /// Assembly program handles per stage (GLASM).
    pub assembly_programs: [u32; NUM_STAGES],
    /// Bitmask of enabled stages.
    pub enabled_stages_mask: u32,

    /// Per-stage enabled uniform buffer masks.
    pub enabled_uniform_buffer_masks: [u32; NUM_STAGES],
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
    built_fence: u32,
    is_built: bool,
}

impl GraphicsPipeline {
    /// Create a new graphics pipeline.
    ///
    /// Corresponds to `GraphicsPipeline::GraphicsPipeline()`.
    pub fn new(key: GraphicsPipelineKey) -> Self {
        // TODO: Full construction matching upstream (compile shaders, set up bindings, etc.)
        Self {
            key,
            source_programs: [0; NUM_STAGES],
            assembly_programs: [0; NUM_STAGES],
            enabled_stages_mask: 0,
            enabled_uniform_buffer_masks: [0; NUM_STAGES],
            base_uniform_bindings: [0; NUM_STAGES],
            base_storage_bindings: [0; NUM_STAGES],
            num_texture_buffers: [0; NUM_STAGES],
            num_image_buffers: [0; NUM_STAGES],
            use_storage_buffers: false,
            writes_global_memory: false,
            uses_local_memory: false,
            num_xfb_attribs: 0,
            num_xfb_buffers_active: 0,
            xfb_attribs: vec![
                0i32;
                128 * XFB_ENTRY_STRIDE * NUM_TRANSFORM_FEEDBACK_BUFFERS
            ],
            built_mutex: Mutex::new(false),
            built_condvar: Condvar::new(),
            built_fence: 0,
            is_built: true,
        }
    }

    /// Configure the pipeline for a draw call.
    ///
    /// Corresponds to `GraphicsPipeline::Configure()`.
    pub fn configure(&mut self, _is_indexed: bool) {
        todo!("GraphicsPipeline::Configure")
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

    /// Returns whether the pipeline has finished building.
    ///
    /// Corresponds to `GraphicsPipeline::IsBuilt()`.
    pub fn is_built(&mut self) -> bool {
        if self.is_built {
            return true;
        }
        if self.built_fence == 0 {
            return false;
        }
        // TODO: Check GL fence signaled state
        self.is_built = true;
        true
    }

    /// Internal: configure transform feedback attributes.
    fn configure_transform_feedback_impl(&self) {
        todo!("GraphicsPipeline::ConfigureTransformFeedbackImpl")
    }

    /// Generate transform feedback state from the pipeline key.
    ///
    /// Corresponds to `GraphicsPipeline::GenerateTransformFeedbackState()`.
    fn generate_transform_feedback_state(&mut self) {
        todo!("GraphicsPipeline::GenerateTransformFeedbackState")
    }

    /// Wait for the pipeline build to complete.
    fn wait_for_build(&mut self) {
        todo!("GraphicsPipeline::WaitForBuild")
    }
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
    // GL_VERTEX_PROGRAM_NV etc. are NV extension constants not in the base gl crate.
    // Placeholder values matching upstream enum order.
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
        // GL_GENERIC_ATTRIB_NV = 0x8C7D (placeholder)
        return (0x8C7D_i32, (index - 8) as i32);
    }
    if (48..=55).contains(&index) {
        // GL_TEXTURE_COORD_NV = 0x8C7A (placeholder)
        return (0x8C7A_i32, (index - 48) as i32);
    }
    // GL_POSITION = 0x1203 (from NV extension, not in base gl crate)
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
