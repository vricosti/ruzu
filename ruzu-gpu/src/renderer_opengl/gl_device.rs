// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_device.h and gl_device.cpp
//! Status: EN COURS
//!
//! Queries OpenGL device capabilities and exposes them as boolean flags.

use log::{info, warn};
use std::ffi::CStr;

/// OpenGL device capabilities, matching zuyu's `Device` class.
pub struct Device {
    vendor_name: String,
    renderer_name: String,
    gl_version: String,

    // Uniform / storage buffer limits
    max_uniform_buffers: [u32; 5], // vertex, tess_ctrl, tess_eval, geometry, fragment
    uniform_buffer_alignment: u32,
    shader_storage_buffer_alignment: u32,
    max_vertex_attributes: u32,
    max_varyings: u32,
    max_compute_shared_memory_size: u32,

    // Extension flags
    has_warp_intrinsics: bool,
    has_shader_ballot: bool,
    has_vertex_viewport_layer: bool,
    has_image_load_formatted: bool,
    has_texture_shadow_lod: bool,
    has_vertex_buffer_unified_memory: bool,
    has_astc: bool,
    has_variable_aoffi: bool,
    has_depth_buffer_float: bool,
    has_geometry_shader_passthrough: bool,
    has_nv_gpu_shader5: bool,
    has_shader_int64: bool,
    has_amd_shader_half_float: bool,
    has_sparse_texture2: bool,
    has_draw_texture: bool,
    has_derivative_control: bool,

    use_assembly_shaders: bool,
    use_asynchronous_shaders: bool,

    is_amd: bool,
    is_intel: bool,
    is_nvidia: bool,
    can_report_memory: bool,
    must_emulate_bgr565: bool,
    strict_context_required: bool,
}

impl Device {
    /// Create a new Device by querying GL state. Must be called with a current GL context.
    pub fn new() -> Self {
        let vendor_name = gl_string(gl::VENDOR);
        let renderer_name = gl_string(gl::RENDERER);
        let gl_version = gl_string(gl::VERSION);

        info!("OpenGL Vendor: {}", vendor_name);
        info!("OpenGL Renderer: {}", renderer_name);
        info!("OpenGL Version: {}", gl_version);

        let vendor_lower = vendor_name.to_lowercase();
        let is_nvidia = vendor_lower.contains("nvidia");
        let is_amd = vendor_lower.contains("amd") || vendor_lower.contains("ati");
        let is_intel = vendor_lower.contains("intel");

        // Query limits
        let max_vertex_attributes = gl_get_integer(gl::MAX_VERTEX_ATTRIBS) as u32;
        let max_varyings = gl_get_integer(gl::MAX_VARYING_VECTORS) as u32;
        let uniform_buffer_alignment = gl_get_integer(gl::UNIFORM_BUFFER_OFFSET_ALIGNMENT) as u32;
        let shader_storage_buffer_alignment =
            gl_get_integer(gl::SHADER_STORAGE_BUFFER_OFFSET_ALIGNMENT) as u32;
        let max_compute_shared_memory_size =
            gl_get_integer(gl::MAX_COMPUTE_SHARED_MEMORY_SIZE) as u32;

        let mut max_uniform_buffers = [0u32; 5];
        let stages = [
            gl::MAX_VERTEX_UNIFORM_BLOCKS,
            gl::MAX_TESS_CONTROL_UNIFORM_BLOCKS,
            gl::MAX_TESS_EVALUATION_UNIFORM_BLOCKS,
            gl::MAX_GEOMETRY_UNIFORM_BLOCKS,
            gl::MAX_FRAGMENT_UNIFORM_BLOCKS,
        ];
        for (i, &stage) in stages.iter().enumerate() {
            max_uniform_buffers[i] = gl_get_integer(stage) as u32;
        }

        // Check extensions
        let extensions = get_extensions();
        let has_ext = |name: &str| extensions.iter().any(|e| e == name);

        let has_warp_intrinsics = is_nvidia && has_ext("GL_NV_gpu_shader5");
        let has_shader_ballot = has_ext("GL_ARB_shader_ballot");
        let has_vertex_viewport_layer = has_ext("GL_ARB_shader_viewport_layer_array")
            || has_ext("GL_NV_viewport_array2");
        let has_image_load_formatted = has_ext("GL_EXT_shader_image_load_formatted");
        let has_texture_shadow_lod = has_ext("GL_EXT_texture_shadow_lod");
        let has_vertex_buffer_unified_memory = has_ext("GL_NV_vertex_buffer_unified_memory");
        let has_astc = has_ext("GL_KHR_texture_compression_astc_ldr");
        let has_variable_aoffi = has_ext("GL_AMD_gpu_shader_half_float")
            || (is_nvidia && has_ext("GL_NV_gpu_shader5"));
        let has_depth_buffer_float = has_ext("GL_NV_depth_buffer_float");
        let has_geometry_shader_passthrough = has_ext("GL_NV_geometry_shader_passthrough");
        let has_nv_gpu_shader5 = has_ext("GL_NV_gpu_shader5");
        let has_shader_int64 = has_ext("GL_ARB_gpu_shader_int64");
        let has_amd_shader_half_float = has_ext("GL_AMD_gpu_shader_half_float");
        let has_sparse_texture2 = has_ext("GL_ARB_sparse_texture2");
        let has_draw_texture = has_ext("GL_NV_draw_texture");
        let has_derivative_control = has_ext("GL_ARB_derivative_control");

        let use_assembly_shaders = is_nvidia
            && has_ext("GL_NV_gpu_program5")
            && has_ext("GL_NV_compute_program5")
            && has_ext("GL_NV_transform_feedback")
            && has_ext("GL_NV_transform_feedback2");
        let use_asynchronous_shaders = has_ext("GL_ARB_parallel_shader_compile");

        let can_report_memory = has_ext("GL_NVX_gpu_memory_info");
        let must_emulate_bgr565 = is_intel;
        let strict_context_required = false; // Set by window code if on Wayland

        // Check required extensions
        if !has_ext("GL_EXT_texture_compression_s3tc") {
            warn!("Missing required extension: GL_EXT_texture_compression_s3tc");
        }
        if !has_ext("GL_ARB_texture_compression_rgtc") {
            warn!("Missing required extension: GL_ARB_texture_compression_rgtc");
        }

        info!(
            "OpenGL caps: max_varyings={}, max_vert_attribs={}, ub_align={}, ssbo_align={}, asm_shaders={}",
            max_varyings, max_vertex_attributes, uniform_buffer_alignment,
            shader_storage_buffer_alignment, use_assembly_shaders
        );

        Device {
            vendor_name,
            renderer_name,
            gl_version,
            max_uniform_buffers,
            uniform_buffer_alignment,
            shader_storage_buffer_alignment,
            max_vertex_attributes,
            max_varyings,
            max_compute_shared_memory_size,
            has_warp_intrinsics,
            has_shader_ballot,
            has_vertex_viewport_layer,
            has_image_load_formatted,
            has_texture_shadow_lod,
            has_vertex_buffer_unified_memory,
            has_astc,
            has_variable_aoffi,
            has_depth_buffer_float,
            has_geometry_shader_passthrough,
            has_nv_gpu_shader5,
            has_shader_int64,
            has_amd_shader_half_float,
            has_sparse_texture2,
            has_draw_texture,
            has_derivative_control,
            use_assembly_shaders,
            use_asynchronous_shaders,
            is_amd,
            is_intel,
            is_nvidia,
            can_report_memory,
            must_emulate_bgr565,
            strict_context_required,
        }
    }

    // --- Accessors ---

    pub fn vendor_name(&self) -> &str {
        &self.vendor_name
    }
    pub fn renderer_name(&self) -> &str {
        &self.renderer_name
    }
    pub fn gl_version(&self) -> &str {
        &self.gl_version
    }
    pub fn max_uniform_buffers(&self, stage: usize) -> u32 {
        self.max_uniform_buffers.get(stage).copied().unwrap_or(0)
    }
    pub fn uniform_buffer_alignment(&self) -> u32 {
        self.uniform_buffer_alignment
    }
    pub fn shader_storage_buffer_alignment(&self) -> u32 {
        self.shader_storage_buffer_alignment
    }
    pub fn max_vertex_attributes(&self) -> u32 {
        self.max_vertex_attributes
    }
    pub fn max_varyings(&self) -> u32 {
        self.max_varyings
    }
    pub fn max_compute_shared_memory_size(&self) -> u32 {
        self.max_compute_shared_memory_size
    }
    pub fn has_warp_intrinsics(&self) -> bool {
        self.has_warp_intrinsics
    }
    pub fn has_shader_ballot(&self) -> bool {
        self.has_shader_ballot
    }
    pub fn has_vertex_viewport_layer(&self) -> bool {
        self.has_vertex_viewport_layer
    }
    pub fn has_image_load_formatted(&self) -> bool {
        self.has_image_load_formatted
    }
    pub fn has_texture_shadow_lod(&self) -> bool {
        self.has_texture_shadow_lod
    }
    pub fn has_vertex_buffer_unified_memory(&self) -> bool {
        self.has_vertex_buffer_unified_memory
    }
    pub fn has_astc(&self) -> bool {
        self.has_astc
    }
    pub fn has_variable_aoffi(&self) -> bool {
        self.has_variable_aoffi
    }
    pub fn has_depth_buffer_float(&self) -> bool {
        self.has_depth_buffer_float
    }
    pub fn has_geometry_shader_passthrough(&self) -> bool {
        self.has_geometry_shader_passthrough
    }
    pub fn has_nv_gpu_shader5(&self) -> bool {
        self.has_nv_gpu_shader5
    }
    pub fn has_shader_int64(&self) -> bool {
        self.has_shader_int64
    }
    pub fn has_amd_shader_half_float(&self) -> bool {
        self.has_amd_shader_half_float
    }
    pub fn has_sparse_texture2(&self) -> bool {
        self.has_sparse_texture2
    }
    pub fn has_draw_texture(&self) -> bool {
        self.has_draw_texture
    }
    pub fn has_derivative_control(&self) -> bool {
        self.has_derivative_control
    }
    pub fn use_assembly_shaders(&self) -> bool {
        self.use_assembly_shaders
    }
    pub fn use_asynchronous_shaders(&self) -> bool {
        self.use_asynchronous_shaders
    }
    pub fn is_amd(&self) -> bool {
        self.is_amd
    }
    pub fn is_intel(&self) -> bool {
        self.is_intel
    }
    pub fn is_nvidia(&self) -> bool {
        self.is_nvidia
    }
    pub fn can_report_memory(&self) -> bool {
        self.can_report_memory
    }
    pub fn must_emulate_bgr565(&self) -> bool {
        self.must_emulate_bgr565
    }
    pub fn strict_context_required(&self) -> bool {
        self.strict_context_required
    }

    pub fn set_strict_context_required(&mut self, val: bool) {
        self.strict_context_required = val;
    }
}

// --- GL helper functions ---

fn gl_string(name: gl::types::GLenum) -> String {
    unsafe {
        let ptr = gl::GetString(name);
        if ptr.is_null() {
            return String::new();
        }
        CStr::from_ptr(ptr as *const _)
            .to_string_lossy()
            .into_owned()
    }
}

fn gl_get_integer(pname: gl::types::GLenum) -> i32 {
    let mut val: i32 = 0;
    unsafe {
        gl::GetIntegerv(pname, &mut val);
    }
    val
}

fn get_extensions() -> Vec<String> {
    let num = gl_get_integer(gl::NUM_EXTENSIONS) as u32;
    let mut exts = Vec::with_capacity(num as usize);
    for i in 0..num {
        unsafe {
            let ptr = gl::GetStringi(gl::EXTENSIONS, i);
            if !ptr.is_null() {
                let s = CStr::from_ptr(ptr as *const _)
                    .to_string_lossy()
                    .into_owned();
                exts.push(s);
            }
        }
    }
    exts
}
