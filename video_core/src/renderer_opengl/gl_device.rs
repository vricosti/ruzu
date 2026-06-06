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
    max_uniform_buffers: [u32; shader_recompiler::stage::MAX_STAGE_TYPES as usize],
    uniform_buffer_alignment: u32,
    shader_storage_buffer_alignment: u32,
    max_vertex_attributes: u32,
    max_varyings: u32,
    max_compute_shared_memory_size: u32,
    max_glasm_storage_buffer_blocks: u32,

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
    has_viewport_swizzle: bool,
    has_fill_rectangle: bool,
    has_geometry_shader_passthrough: bool,
    has_nv_viewport_array2: bool,
    has_nv_gpu_shader5: bool,
    has_shader_int64: bool,
    has_amd_shader_half_float: bool,
    has_sparse_texture2: bool,
    has_draw_texture: bool,
    has_derivative_control: bool,
    has_component_indexing_bug: bool,
    has_precise_bug: bool,
    has_broken_texture_view_formats: bool,
    has_fast_buffer_sub_data: bool,
    has_cbuf_ftou_bug: bool,
    has_bool_ref_bug: bool,
    has_debugging_tool_attached: bool,
    warp_size_potentially_larger_than_guest: bool,
    needs_fastmath_off: bool,

    use_assembly_shaders: bool,
    use_asynchronous_shaders: bool,
    use_driver_cache: bool,

    is_amd: bool,
    is_intel: bool,
    is_nvidia: bool,
    can_report_memory: bool,
    must_emulate_bgr565: bool,
    strict_context_required: bool,
    supports_conditional_barriers: bool,
    has_lmem_perf_bug: bool,
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

        // Match upstream's exact vendor predicates. In particular, Mesa
        // radeonsi reports `AMD`; upstream does not treat that as `IsAmd()`
        // for shader-profile policy such as gather subpixel offsets.
        let is_nvidia = vendor_name == "NVIDIA Corporation";
        let is_amd = vendor_name == "ATI Technologies Inc.";
        let is_intel = vendor_name == "Intel";

        // Query limits
        let max_vertex_attributes = gl_get_integer(gl::MAX_VERTEX_ATTRIBS) as u32;
        let max_varyings = gl_get_integer(gl::MAX_VARYING_VECTORS) as u32;
        let uniform_buffer_alignment = gl_get_integer(gl::UNIFORM_BUFFER_OFFSET_ALIGNMENT) as u32;
        let shader_storage_buffer_alignment =
            gl_get_integer(gl::SHADER_STORAGE_BUFFER_OFFSET_ALIGNMENT) as u32;
        let max_compute_shared_memory_size =
            gl_get_integer(gl::MAX_COMPUTE_SHARED_MEMORY_SIZE) as u32;
        let max_glasm_storage_buffer_blocks =
            gl_get_integer(gl::MAX_VERTEX_SHADER_STORAGE_BLOCKS) as u32;

        let mut max_uniform_buffers = [0u32; shader_recompiler::stage::MAX_STAGE_TYPES as usize];
        let stages = [
            gl::MAX_VERTEX_UNIFORM_BLOCKS,
            gl::MAX_TESS_CONTROL_UNIFORM_BLOCKS,
            gl::MAX_TESS_EVALUATION_UNIFORM_BLOCKS,
            gl::MAX_GEOMETRY_UNIFORM_BLOCKS,
            gl::MAX_FRAGMENT_UNIFORM_BLOCKS,
            gl::MAX_COMPUTE_UNIFORM_BLOCKS,
        ];
        for (i, &stage) in stages.iter().enumerate() {
            max_uniform_buffers[i] = gl_get_integer(stage) as u32;
        }

        // Check extensions
        let extensions = get_extensions();
        let has_ext = |name: &str| extensions.iter().any(|e| e == name);

        let has_slow_software_astc =
            !is_nvidia && !is_amd && has_slow_software_astc(&vendor_name, &renderer_name);

        let has_warp_intrinsics = has_ext("GL_NV_gpu_shader5")
            && has_ext("GL_NV_shader_thread_group")
            && has_ext("GL_NV_shader_thread_shuffle");
        let has_shader_ballot = has_ext("GL_ARB_shader_ballot");
        let has_vertex_viewport_layer = has_ext("GL_ARB_shader_viewport_layer_array");
        let has_image_load_formatted = has_ext("GL_EXT_shader_image_load_formatted");
        let has_texture_shadow_lod = has_ext("GL_EXT_texture_shadow_lod");
        let has_vertex_buffer_unified_memory = has_ext("GL_NV_vertex_buffer_unified_memory");
        let has_astc = !has_slow_software_astc && is_astc_supported();
        let has_variable_aoffi = test_variable_aoffi();
        let has_depth_buffer_float = has_ext("GL_NV_depth_buffer_float");
        let has_viewport_swizzle = has_ext("GL_NV_viewport_swizzle");
        let has_fill_rectangle = has_ext("GL_NV_fill_rectangle");
        let has_geometry_shader_passthrough = has_ext("GL_NV_geometry_shader_passthrough");
        let has_nv_viewport_array2 = has_ext("GL_NV_viewport_array2");
        let has_nv_gpu_shader5 = has_ext("GL_NV_gpu_shader5");
        let has_shader_int64 = has_ext("GL_ARB_gpu_shader_int64");
        let has_amd_shader_half_float = has_ext("GL_AMD_gpu_shader_half_float");
        let has_sparse_texture2 = has_ext("GL_ARB_sparse_texture2");
        let has_draw_texture = has_ext("GL_NV_draw_texture");
        let has_derivative_control = has_ext("GL_ARB_derivative_control");
        let has_component_indexing_bug = false;
        let has_precise_bug = test_precise_bug();
        let has_broken_texture_view_formats = cfg!(not(target_family = "unix")) && is_intel;
        let disable_fast_buffer_sub_data = is_nvidia && gl_version == "4.6.0 NVIDIA 443.24";
        let has_fast_buffer_sub_data = is_nvidia && !disable_fast_buffer_sub_data;
        let version_major = nvidia_driver_major_version(&gl_version);
        let has_cbuf_ftou_bug = is_nvidia && version_major.is_some_and(|major| major >= 495);
        let has_bool_ref_bug = has_cbuf_ftou_bug;
        let has_debugging_tool_attached = std::env::var_os("NVTX_INJECTION64_PATH").is_some()
            || std::env::var_os("NSIGHT_LAUNCHED").is_some()
            || has_ext("GL_EXT_debug_tool");
        let warp_size_potentially_larger_than_guest = !is_nvidia && !is_intel;
        let needs_fastmath_off = is_nvidia;

        let use_assembly_shaders = is_nvidia
            && has_ext("GL_NV_gpu_program5")
            && has_ext("GL_NV_compute_program5")
            && has_ext("GL_NV_transform_feedback")
            && has_ext("GL_NV_transform_feedback2");
        let use_asynchronous_shaders = has_ext("GL_ARB_parallel_shader_compile");
        let use_driver_cache = is_nvidia;

        let can_report_memory = has_ext("GL_NVX_gpu_memory_info");
        let must_emulate_bgr565 = is_intel;
        let strict_context_required = false; // Set by window code if on Wayland
        let supports_conditional_barriers = !is_intel;
        let has_lmem_perf_bug = is_nvidia;

        // Check required extensions
        if !has_ext("GL_EXT_texture_compression_s3tc") {
            warn!("Missing required extension: GL_EXT_texture_compression_s3tc");
        }
        if !has_ext("GL_ARB_texture_compression_rgtc") {
            warn!("Missing required extension: GL_ARB_texture_compression_rgtc");
        }

        info!(
            "OpenGL caps: max_varyings={}, max_vert_attribs={}, ub_align={}, ssbo_align={}, asm_shaders={}",
            max_varyings,
            max_vertex_attributes,
            uniform_buffer_alignment,
            shader_storage_buffer_alignment,
            use_assembly_shaders
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
            max_glasm_storage_buffer_blocks,
            has_warp_intrinsics,
            has_shader_ballot,
            has_vertex_viewport_layer,
            has_image_load_formatted,
            has_texture_shadow_lod,
            has_vertex_buffer_unified_memory,
            has_astc,
            has_variable_aoffi,
            has_depth_buffer_float,
            has_viewport_swizzle,
            has_fill_rectangle,
            has_geometry_shader_passthrough,
            has_nv_viewport_array2,
            has_nv_gpu_shader5,
            has_shader_int64,
            has_amd_shader_half_float,
            has_sparse_texture2,
            has_draw_texture,
            has_derivative_control,
            has_component_indexing_bug,
            has_precise_bug,
            has_broken_texture_view_formats,
            has_fast_buffer_sub_data,
            has_cbuf_ftou_bug,
            has_bool_ref_bug,
            has_debugging_tool_attached,
            warp_size_potentially_larger_than_guest,
            needs_fastmath_off,
            use_assembly_shaders,
            use_asynchronous_shaders,
            use_driver_cache,
            is_amd,
            is_intel,
            is_nvidia,
            can_report_memory,
            must_emulate_bgr565,
            strict_context_required,
            supports_conditional_barriers,
            has_lmem_perf_bug,
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
    pub fn max_glasm_storage_buffer_blocks(&self) -> u32 {
        self.max_glasm_storage_buffer_blocks
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
    pub fn has_viewport_swizzle(&self) -> bool {
        self.has_viewport_swizzle
    }
    pub fn has_fill_rectangle(&self) -> bool {
        self.has_fill_rectangle
    }
    pub fn has_geometry_shader_passthrough(&self) -> bool {
        self.has_geometry_shader_passthrough
    }
    pub fn has_nv_viewport_array2(&self) -> bool {
        self.has_nv_viewport_array2
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
    pub fn has_component_indexing_bug(&self) -> bool {
        self.has_component_indexing_bug
    }
    pub fn has_precise_bug(&self) -> bool {
        self.has_precise_bug
    }
    pub fn has_broken_texture_view_formats(&self) -> bool {
        self.has_broken_texture_view_formats
    }
    pub fn has_fast_buffer_sub_data(&self) -> bool {
        self.has_fast_buffer_sub_data
    }
    pub fn has_cbuf_ftou_bug(&self) -> bool {
        self.has_cbuf_ftou_bug
    }
    pub fn has_bool_ref_bug(&self) -> bool {
        self.has_bool_ref_bug
    }
    pub fn has_debugging_tool_attached(&self) -> bool {
        self.has_debugging_tool_attached
    }
    pub fn is_warp_size_potentially_larger_than_guest(&self) -> bool {
        self.warp_size_potentially_larger_than_guest
    }
    pub fn needs_fastmath_off(&self) -> bool {
        self.needs_fastmath_off
    }
    pub fn use_assembly_shaders(&self) -> bool {
        self.use_assembly_shaders
    }
    pub fn use_asynchronous_shaders(&self) -> bool {
        self.use_asynchronous_shaders
    }
    pub fn use_driver_cache(&self) -> bool {
        self.use_driver_cache
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

    /// Port of upstream `Device::GetCurrentDedicatedVideoMemory()`
    /// (gl_device.cpp:331-335). Queries
    /// `GL_GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX = 0x9048` (the
    /// NVX-extension total-available query) and returns it as bytes.
    /// Returns 0 if the NVX extension is absent or the query fails.
    pub fn get_current_dedicated_video_memory(&self) -> u64 {
        const GL_GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX: u32 = 0x9048;
        if !self.can_report_memory {
            return 0;
        }
        let mut total_kb: i32 = 0;
        unsafe {
            gl::GetIntegerv(GL_GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX, &mut total_kb);
        }
        if total_kb <= 0 {
            0
        } else {
            (total_kb as u64) * 1024
        }
    }
    pub fn must_emulate_bgr565(&self) -> bool {
        self.must_emulate_bgr565
    }
    pub fn strict_context_required(&self) -> bool {
        self.strict_context_required
    }
    pub fn supports_conditional_barriers(&self) -> bool {
        self.supports_conditional_barriers
    }
    pub fn has_lmem_perf_bug(&self) -> bool {
        self.has_lmem_perf_bug
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

fn test_program(glsl: &'static CStr) -> bool {
    unsafe {
        let source = glsl.as_ptr();
        let program = gl::CreateShaderProgramv(gl::VERTEX_SHADER, 1, &source);
        if program == 0 {
            return false;
        }
        let mut link_status = 0;
        gl::GetProgramiv(program, gl::LINK_STATUS, &mut link_status);
        gl::DeleteProgram(program);
        link_status == gl::TRUE as i32
    }
}

fn test_variable_aoffi() -> bool {
    test_program(
        c"#version 430 core
// This is a unit test, please ignore me on apitrace bug reports.
uniform sampler2D tex;
uniform ivec2 variable_offset;
out vec4 output_attribute;
void main() {
    output_attribute = textureOffset(tex, vec2(0), variable_offset);
}
",
    )
}

fn test_precise_bug() -> bool {
    !test_program(
        c"#version 430 core
in vec3 coords;
out float out_value;
uniform sampler2DShadow tex;
void main() {
    precise float tmp_value = vec4(texture(tex, coords)).x;
    out_value = tmp_value;
}
",
    )
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

fn has_slow_software_astc(vendor_name: &str, renderer: &str) -> bool {
    if cfg!(target_family = "unix") {
        if vendor_name == "AMD" {
            return true;
        }
        if vendor_name == "Intel" {
            return renderer.contains("DG");
        }
        if vendor_name == "nouveau" || vendor_name == "X.Org" {
            return true;
        }
    }
    matches!(
        vendor_name,
        "Collabora Ltd" | "Microsoft Corporation" | "Mesa/X.org"
    )
}

fn is_astc_supported() -> bool {
    const GL_FULL_SUPPORT: i32 = 0x82B7;
    const GL_COMPRESSED_RGBA_ASTC_4X4_KHR: u32 = 0x93B0;
    const GL_COMPRESSED_RGBA_ASTC_5X4_KHR: u32 = 0x93B1;
    const GL_COMPRESSED_RGBA_ASTC_5X5_KHR: u32 = 0x93B2;
    const GL_COMPRESSED_RGBA_ASTC_6X5_KHR: u32 = 0x93B3;
    const GL_COMPRESSED_RGBA_ASTC_6X6_KHR: u32 = 0x93B4;
    const GL_COMPRESSED_RGBA_ASTC_8X5_KHR: u32 = 0x93B5;
    const GL_COMPRESSED_RGBA_ASTC_8X6_KHR: u32 = 0x93B6;
    const GL_COMPRESSED_RGBA_ASTC_8X8_KHR: u32 = 0x93B7;
    const GL_COMPRESSED_RGBA_ASTC_10X5_KHR: u32 = 0x93B8;
    const GL_COMPRESSED_RGBA_ASTC_10X6_KHR: u32 = 0x93B9;
    const GL_COMPRESSED_RGBA_ASTC_10X8_KHR: u32 = 0x93BA;
    const GL_COMPRESSED_RGBA_ASTC_10X10_KHR: u32 = 0x93BB;
    const GL_COMPRESSED_RGBA_ASTC_12X10_KHR: u32 = 0x93BC;
    const GL_COMPRESSED_RGBA_ASTC_12X12_KHR: u32 = 0x93BD;
    const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4X4_KHR: u32 = 0x93D0;
    const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5X4_KHR: u32 = 0x93D1;
    const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5X5_KHR: u32 = 0x93D2;
    const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6X5_KHR: u32 = 0x93D3;
    const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6X6_KHR: u32 = 0x93D4;
    const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8X5_KHR: u32 = 0x93D5;
    const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8X6_KHR: u32 = 0x93D6;
    const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8X8_KHR: u32 = 0x93D7;
    const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10X5_KHR: u32 = 0x93D8;
    const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10X6_KHR: u32 = 0x93D9;
    const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10X8_KHR: u32 = 0x93DA;
    const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10X10_KHR: u32 = 0x93DB;
    const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12X10_KHR: u32 = 0x93DC;
    const GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12X12_KHR: u32 = 0x93DD;
    const TARGETS: [u32; 2] = [gl::TEXTURE_2D, gl::TEXTURE_2D_ARRAY];
    const FORMATS: [u32; 28] = [
        GL_COMPRESSED_RGBA_ASTC_4X4_KHR,
        GL_COMPRESSED_RGBA_ASTC_5X4_KHR,
        GL_COMPRESSED_RGBA_ASTC_5X5_KHR,
        GL_COMPRESSED_RGBA_ASTC_6X5_KHR,
        GL_COMPRESSED_RGBA_ASTC_6X6_KHR,
        GL_COMPRESSED_RGBA_ASTC_8X5_KHR,
        GL_COMPRESSED_RGBA_ASTC_8X6_KHR,
        GL_COMPRESSED_RGBA_ASTC_8X8_KHR,
        GL_COMPRESSED_RGBA_ASTC_10X5_KHR,
        GL_COMPRESSED_RGBA_ASTC_10X6_KHR,
        GL_COMPRESSED_RGBA_ASTC_10X8_KHR,
        GL_COMPRESSED_RGBA_ASTC_10X10_KHR,
        GL_COMPRESSED_RGBA_ASTC_12X10_KHR,
        GL_COMPRESSED_RGBA_ASTC_12X12_KHR,
        GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4X4_KHR,
        GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5X4_KHR,
        GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5X5_KHR,
        GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6X5_KHR,
        GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6X6_KHR,
        GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8X5_KHR,
        GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8X6_KHR,
        GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8X8_KHR,
        GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10X5_KHR,
        GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10X6_KHR,
        GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10X8_KHR,
        GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10X10_KHR,
        GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12X10_KHR,
        GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12X12_KHR,
    ];
    const REQUIRED_SUPPORT: [u32; 6] = [
        gl::VERTEX_TEXTURE,
        gl::TESS_CONTROL_TEXTURE,
        gl::TESS_EVALUATION_TEXTURE,
        gl::GEOMETRY_TEXTURE,
        gl::FRAGMENT_TEXTURE,
        gl::COMPUTE_TEXTURE,
    ];
    for target in TARGETS {
        for format in FORMATS {
            for support in REQUIRED_SUPPORT {
                let mut value = 0;
                unsafe {
                    gl::GetInternalformativ(target, format, support, 1, &mut value);
                }
                if value != GL_FULL_SUPPORT {
                    return false;
                }
            }
        }
    }
    true
}

fn nvidia_driver_major_version(gl_version: &str) -> Option<u32> {
    let driver = gl_version.strip_prefix("4.6.0 NVIDIA ")?;
    let major = driver.split('.').next()?;
    major.parse().ok()
}
