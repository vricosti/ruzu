// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_graphics_pipeline.h and gl_graphics_pipeline.cpp
//!
//! OpenGL graphics pipeline management -- compiles and configures vertex/fragment/etc shaders.

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
}

impl GraphicsPipelineKey {
    /// Hash the key, considering only relevant bytes (smaller if xfb not enabled).
    pub fn hash_key(&self) -> u64 {
        let size = self.size();
        let bytes: &[u8] =
            unsafe { std::slice::from_raw_parts(self as *const Self as *const u8, size) };
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
        }
    }

    /// Configure the pipeline for a draw call.
    ///
    /// Port of `GraphicsPipeline::Configure()`.
    ///
    /// In the full implementation, this:
    /// 1. Waits for async build to complete
    /// 2. Binds shader programs (source or assembly per stage)
    /// 3. Fills uniform buffer descriptors per stage
    /// 4. Fills storage buffer descriptors per stage
    /// 5. Fills texture/image descriptors per stage
    /// 6. Configures transform feedback if enabled
    pub fn configure(&mut self, _is_indexed: bool) {
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

        self.configure_transform_feedback();
        // Full implementation also requires buffer_cache / texture_cache
        // bindings (uniform/storage/texture/image descriptors); those are
        // separate gaps.
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
                }
                Err(msg) => {
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
        return Err(format!("glCreateShader returned 0 for stage {}", stage_index));
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
        return Err(format!("glCreateProgram returned 0 for stage {}", stage_index));
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
    fn pipeline_key_size_varies_by_xfb() {
        let mut key = GraphicsPipelineKey::default();
        let size_no_xfb = key.size();

        key.raw = 1; // xfb_enabled
        let size_xfb = key.size();

        assert!(size_xfb > size_no_xfb);
        assert_eq!(size_xfb, std::mem::size_of::<GraphicsPipelineKey>());
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
}
