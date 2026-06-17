// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/present/window_adapt_pass.h and
//! window_adapt_pass.cpp
//!
//! Window adapt pass -- final composition of layers onto the window framebuffer
//! using a configurable fragment shader (filter).

use super::layer::Layer;
use super::present_uniforms::{
    ScreenRectVertex, MODEL_VIEW_MATRIX_LOCATION, POSITION_LOCATION, TEX_COORD_LOCATION,
};
use crate::framebuffer_config::{BlendMode, FramebufferConfig};
use crate::host_shaders::vertex_shaders::OPENGL_PRESENT_VERT;
use crate::renderer_opengl::gl_shader_manager::ProgramManagerHandle;
use crate::renderer_opengl::Device;
use ruzu_core::frontend::framebuffer_layout::FramebufferLayout;
use std::ffi::c_void;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::OnceLock;

type GlGetNamedBufferParameterui64vNv = unsafe extern "system" fn(
    buffer: gl::types::GLuint,
    pname: gl::types::GLenum,
    params: *mut u64,
);
type GlMakeNamedBufferResidentNv =
    unsafe extern "system" fn(buffer: gl::types::GLuint, access: gl::types::GLenum);
type GlBufferAddressRangeNv = unsafe extern "system" fn(
    pname: gl::types::GLenum,
    index: gl::types::GLuint,
    address: u64,
    length: gl::types::GLsizeiptr,
);

static GL_GET_NAMED_BUFFER_PARAMETER_UI64V_NV: OnceLock<Option<GlGetNamedBufferParameterui64vNv>> =
    OnceLock::new();
static GL_MAKE_NAMED_BUFFER_RESIDENT_NV: OnceLock<Option<GlMakeNamedBufferResidentNv>> =
    OnceLock::new();
static GL_BUFFER_ADDRESS_RANGE_NV: OnceLock<Option<GlBufferAddressRangeNv>> = OnceLock::new();
static PRESENT_PHASE_TRACE_COUNT: AtomicU64 = AtomicU64::new(0);
static PRESENT_TEXTURE_TRACE_COUNT: AtomicU64 = AtomicU64::new(0);

const GL_BUFFER_GPU_ADDRESS_NV: u32 = 0x8F1D;
const GL_VERTEX_ATTRIB_ARRAY_ADDRESS_NV: u32 = 0x8F20;

fn trace_every(env_name: &str, counter: &AtomicU64) -> bool {
    let every = std::env::var(env_name)
        .ok()
        .and_then(|value| value.parse::<u64>().ok())
        .unwrap_or(1)
        .max(1);
    let count = counter.fetch_add(1, Ordering::Relaxed) + 1;
    count % every == 0
}

fn load_optional_gl_function<T, F>(load_fn: &mut F, name: &'static str) -> Option<T>
where
    F: FnMut(&'static str) -> *const c_void,
{
    let ptr = load_fn(name);
    if ptr.is_null() {
        None
    } else {
        Some(unsafe { std::mem::transmute_copy::<*const c_void, T>(&ptr) })
    }
}

pub fn load_extra_functions<F>(load_fn: &mut F)
where
    F: FnMut(&'static str) -> *const c_void,
{
    let _ = GL_GET_NAMED_BUFFER_PARAMETER_UI64V_NV.set(load_optional_gl_function(
        load_fn,
        "glGetNamedBufferParameterui64vNV",
    ));
    let _ = GL_MAKE_NAMED_BUFFER_RESIDENT_NV.set(load_optional_gl_function(
        load_fn,
        "glMakeNamedBufferResidentNV",
    ));
    let _ = GL_BUFFER_ADDRESS_RANGE_NV
        .set(load_optional_gl_function(load_fn, "glBufferAddressRangeNV"));
}

fn trace_present_phase(label: &str, layer: Option<usize>, width: u32, height: u32) {
    if std::env::var_os("RUZU_TRACE_PRESENT_PHASES").is_none() {
        return;
    }
    if !trace_every(
        "RUZU_TRACE_PRESENT_PHASES_EVERY",
        &PRESENT_PHASE_TRACE_COUNT,
    ) {
        return;
    }

    unsafe {
        let sample_width = width.min(32) as i32;
        let sample_height = height.min(32) as i32;
        let mut old_read_fb = 0;
        let mut old_pack_buffer = 0;
        let mut old_pack_alignment = 0;
        let mut old_pack_row_length = 0;
        let mut old_read_buffer = 0;
        let mut draw_fb = 0;
        gl::GetIntegerv(gl::READ_FRAMEBUFFER_BINDING, &mut old_read_fb);
        gl::GetIntegerv(gl::DRAW_FRAMEBUFFER_BINDING, &mut draw_fb);
        gl::GetIntegerv(gl::PIXEL_PACK_BUFFER_BINDING, &mut old_pack_buffer);
        gl::GetIntegerv(gl::PACK_ALIGNMENT, &mut old_pack_alignment);
        gl::GetIntegerv(gl::PACK_ROW_LENGTH, &mut old_pack_row_length);
        gl::GetIntegerv(gl::READ_BUFFER, &mut old_read_buffer);

        gl::BindFramebuffer(gl::READ_FRAMEBUFFER, draw_fb as u32);
        gl::ReadBuffer(if draw_fb == 0 {
            gl::BACK
        } else {
            gl::COLOR_ATTACHMENT0
        });
        gl::BindBuffer(gl::PIXEL_PACK_BUFFER, 0);
        gl::PixelStorei(gl::PACK_ALIGNMENT, 1);
        gl::PixelStorei(gl::PACK_ROW_LENGTH, 0);

        let max_x = width.saturating_sub(sample_width as u32) as i32;
        let max_y = height.saturating_sub(sample_height as u32) as i32;
        let origins = [
            (0, 0),
            (max_x / 2, max_y / 2),
            (max_x, max_y),
            (0, max_y),
            (max_x, 0),
        ];
        let mut summaries = Vec::with_capacity(origins.len());
        let mut gl_error = 0;
        for (origin_x, origin_y) in origins {
            let mut pixels = vec![0u8; (sample_width * sample_height * 4) as usize];
            gl::ReadPixels(
                origin_x,
                origin_y,
                sample_width,
                sample_height,
                gl::RGBA,
                gl::UNSIGNED_BYTE,
                pixels.as_mut_ptr() as *mut _,
            );
            gl_error |= gl::GetError();

            let mut rgb_nonzero = 0usize;
            let mut alpha_nonzero = 0usize;
            let mut rgba_sum = [0u64; 4];
            for px in pixels.chunks_exact(4) {
                rgb_nonzero += px[0..3].iter().filter(|&&byte| byte != 0).count();
                alpha_nonzero += usize::from(px[3] != 0);
                for component in 0..4 {
                    rgba_sum[component] += px[component] as u64;
                }
            }
            let checksum = pixels
                .iter()
                .fold(0u64, |acc, &byte| acc.wrapping_mul(16777619) ^ byte as u64);
            summaries.push(format!(
                "@{},{} rgb={} a={} sum={:?} crc=0x{:X}",
                origin_x, origin_y, rgb_nonzero, alpha_nonzero, rgba_sum, checksum
            ));
        }

        gl::BindFramebuffer(gl::READ_FRAMEBUFFER, old_read_fb as u32);
        gl::ReadBuffer(old_read_buffer as u32);
        gl::BindBuffer(gl::PIXEL_PACK_BUFFER, old_pack_buffer as u32);
        gl::PixelStorei(gl::PACK_ALIGNMENT, old_pack_alignment);
        gl::PixelStorei(gl::PACK_ROW_LENGTH, old_pack_row_length);

        log::info!(
            "[PRESENT_PHASE] label={} layer={:?} draw_fb={} {}x{} sample={}x{} regions=[{}] gl_error=0x{:X}",
            label,
            layer,
            draw_fb,
            width,
            height,
            sample_width,
            sample_height,
            summaries.join("; "),
            gl_error
        );
    }
}

fn trace_present_texture(label: &str, layer: usize, texture: u32, framebuffer: &FramebufferConfig) {
    if std::env::var_os("RUZU_TRACE_PRESENT_TEXTURE_SAMPLE").is_none() {
        return;
    }
    if !trace_every(
        "RUZU_TRACE_PRESENT_TEXTURE_SAMPLE_EVERY",
        &PRESENT_TEXTURE_TRACE_COUNT,
    ) {
        return;
    }
    if texture == 0 {
        log::warn!(
            "[PRESENT_TEXTURE_SAMPLE] label={} layer={} texture=0 fb_addr=0x{:X}",
            label,
            layer,
            framebuffer.address.wrapping_add(framebuffer.offset as u64)
        );
        return;
    }

    unsafe {
        let mut tex_width = 0;
        let mut tex_height = 0;
        let mut tex_internal_format = 0;
        gl::GetTextureLevelParameteriv(texture, 0, gl::TEXTURE_WIDTH, &mut tex_width);
        gl::GetTextureLevelParameteriv(texture, 0, gl::TEXTURE_HEIGHT, &mut tex_height);
        gl::GetTextureLevelParameteriv(
            texture,
            0,
            gl::TEXTURE_INTERNAL_FORMAT,
            &mut tex_internal_format,
        );

        if tex_width <= 0 || tex_height <= 0 {
            log::warn!(
                "[PRESENT_TEXTURE_SAMPLE] label={} layer={} texture={} invalid_size={}x{} gl_error=0x{:X}",
                label,
                layer,
                texture,
                tex_width,
                tex_height,
                gl::GetError()
            );
            return;
        }

        let sample_width = (tex_width as u32).min(32) as i32;
        let sample_height = (tex_height as u32).min(32) as i32;
        let max_x = (tex_width - sample_width).max(0);
        let max_y = (tex_height - sample_height).max(0);
        let origins = [
            (0, 0),
            (max_x / 2, max_y / 2),
            (max_x, max_y),
            (0, max_y),
            (max_x, 0),
        ];

        let mut old_read_fb = 0;
        let mut old_draw_fb = 0;
        let mut old_pack_buffer = 0;
        let mut old_pack_alignment = 0;
        let mut old_pack_row_length = 0;
        let mut old_read_buffer = 0;
        gl::GetIntegerv(gl::READ_FRAMEBUFFER_BINDING, &mut old_read_fb);
        gl::GetIntegerv(gl::DRAW_FRAMEBUFFER_BINDING, &mut old_draw_fb);
        gl::GetIntegerv(gl::PIXEL_PACK_BUFFER_BINDING, &mut old_pack_buffer);
        gl::GetIntegerv(gl::PACK_ALIGNMENT, &mut old_pack_alignment);
        gl::GetIntegerv(gl::PACK_ROW_LENGTH, &mut old_pack_row_length);
        gl::GetIntegerv(gl::READ_BUFFER, &mut old_read_buffer);

        let mut sample_fb = 0;
        gl::CreateFramebuffers(1, &mut sample_fb);
        gl::NamedFramebufferTexture(sample_fb, gl::COLOR_ATTACHMENT0, texture, 0);
        gl::BindFramebuffer(gl::READ_FRAMEBUFFER, sample_fb);
        gl::ReadBuffer(gl::COLOR_ATTACHMENT0);
        gl::BindBuffer(gl::PIXEL_PACK_BUFFER, 0);
        gl::PixelStorei(gl::PACK_ALIGNMENT, 1);
        gl::PixelStorei(gl::PACK_ROW_LENGTH, 0);

        let fb_status = gl::CheckNamedFramebufferStatus(sample_fb, gl::READ_FRAMEBUFFER);
        let mut summaries = Vec::with_capacity(origins.len());
        let mut gl_error = gl::GetError();
        if fb_status == gl::FRAMEBUFFER_COMPLETE {
            for (origin_x, origin_y) in origins {
                let mut pixels = vec![0u8; (sample_width * sample_height * 4) as usize];
                gl::ReadPixels(
                    origin_x,
                    origin_y,
                    sample_width,
                    sample_height,
                    gl::RGBA,
                    gl::UNSIGNED_BYTE,
                    pixels.as_mut_ptr() as *mut _,
                );
                gl_error |= gl::GetError();

                let mut rgb_nonzero = 0usize;
                let mut alpha_nonzero = 0usize;
                let mut rgba_sum = [0u64; 4];
                for px in pixels.chunks_exact(4) {
                    rgb_nonzero += px[0..3].iter().filter(|&&byte| byte != 0).count();
                    alpha_nonzero += usize::from(px[3] != 0);
                    for component in 0..4 {
                        rgba_sum[component] += px[component] as u64;
                    }
                }
                let checksum = pixels
                    .iter()
                    .fold(0u64, |acc, &byte| acc.wrapping_mul(16777619) ^ byte as u64);
                summaries.push(format!(
                    "@{},{} rgb={} a={} sum={:?} crc=0x{:X}",
                    origin_x, origin_y, rgb_nonzero, alpha_nonzero, rgba_sum, checksum
                ));
            }
        }

        gl::BindFramebuffer(gl::READ_FRAMEBUFFER, old_read_fb as u32);
        gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, old_draw_fb as u32);
        gl::ReadBuffer(old_read_buffer as u32);
        gl::BindBuffer(gl::PIXEL_PACK_BUFFER, old_pack_buffer as u32);
        gl::PixelStorei(gl::PACK_ALIGNMENT, old_pack_alignment);
        gl::PixelStorei(gl::PACK_ROW_LENGTH, old_pack_row_length);
        gl::DeleteFramebuffers(1, &sample_fb);

        log::info!(
            "[PRESENT_TEXTURE_SAMPLE] label={} layer={} texture={} tex={}x{} internal=0x{:X} fb_addr=0x{:X} fb={}x{} stride={} format=0x{:X} sample={}x{} status=0x{:X} regions=[{}] gl_error=0x{:X}",
            label,
            layer,
            texture,
            tex_width,
            tex_height,
            tex_internal_format,
            framebuffer.address.wrapping_add(framebuffer.offset as u64),
            framebuffer.width,
            framebuffer.height,
            framebuffer.stride,
            framebuffer.pixel_format.0,
            sample_width,
            sample_height,
            fb_status,
            summaries.join("; "),
            gl_error
        );
    }
}

/// Compile a shader from source.
fn compile_shader(source: &str, shader_type: u32) -> u32 {
    unsafe {
        let shader = gl::CreateShader(shader_type);
        let mut forced_present_frag = None;
        if shader_type == gl::FRAGMENT_SHADER
            && std::env::var_os("RUZU_PRESENT_FORCE_SOLID").is_some()
        {
            forced_present_frag = Some(
                "#version 430 core\n\
                 layout (location = 0) out vec4 color;\n\
                 void main() { color = vec4(1.0, 0.0, 0.0, 1.0); }\n",
            );
        }
        let source = forced_present_frag.unwrap_or(source);
        let source = source
            .find("#version")
            .map(|index| &source[index..])
            .unwrap_or(source);
        let c_source = std::ffi::CString::new(source).unwrap();
        let ptr = c_source.as_ptr();
        gl::ShaderSource(shader, 1, &ptr, std::ptr::null());
        gl::CompileShader(shader);

        let mut success: i32 = 0;
        gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut success);
        if success == 0 {
            let mut log_len: i32 = 0;
            gl::GetShaderiv(shader, gl::INFO_LOG_LENGTH, &mut log_len);
            let mut log_buf = vec![0u8; log_len as usize];
            gl::GetShaderInfoLog(
                shader,
                log_len,
                std::ptr::null_mut(),
                log_buf.as_mut_ptr() as *mut i8,
            );
            log::error!(
                "Shader compilation failed: {}",
                String::from_utf8_lossy(&log_buf)
            );
            gl::DeleteShader(shader);
            return 0;
        }
        shader
    }
}

/// Link one shader stage into a separable program.
///
/// Port of upstream `LinkSeparableProgram()` in `gl_shader_util.cpp`.
fn link_separable_program(shader: u32) -> u32 {
    unsafe {
        let program = gl::CreateProgram();
        gl::ProgramParameteri(program, gl::PROGRAM_SEPARABLE, gl::TRUE as i32);
        gl::AttachShader(program, shader);
        gl::LinkProgram(program);
        gl::DetachShader(program, shader);

        let mut success: i32 = 0;
        gl::GetProgramiv(program, gl::LINK_STATUS, &mut success);
        if success == 0 {
            let mut log_len: i32 = 0;
            gl::GetProgramiv(program, gl::INFO_LOG_LENGTH, &mut log_len);
            let mut log_buf = vec![0u8; log_len as usize];
            gl::GetProgramInfoLog(
                program,
                log_len,
                std::ptr::null_mut(),
                log_buf.as_mut_ptr() as *mut i8,
            );
            log::error!(
                "Program linking failed: {}",
                String::from_utf8_lossy(&log_buf)
            );
            gl::DeleteProgram(program);
            return 0;
        }
        program
    }
}

fn create_separable_program(source: &str, shader_type: u32) -> u32 {
    let shader = compile_shader(source, shader_type);
    if shader == 0 {
        return 0;
    }
    let program = link_separable_program(shader);
    unsafe {
        gl::DeleteShader(shader);
    }
    program
}

/// Window adapt pass for final framebuffer composition.
///
/// Corresponds to `OpenGL::WindowAdaptPass`.
pub struct WindowAdaptPass {
    device: *const Device,
    sampler: u32,
    vert_program: u32,
    frag_program: u32,
    vertex_buffer: u32,
    vertex_buffer_address: u64,
    vao: u32,
}

impl WindowAdaptPass {
    /// Create a new window adapt pass with the given sampler and fragment shader.
    ///
    /// Port of `WindowAdaptPass::WindowAdaptPass()`.
    pub fn new(device: *const Device, sampler: u32, frag_source: &str) -> Self {
        let vert_program = create_separable_program(OPENGL_PRESENT_VERT, gl::VERTEX_SHADER);
        let frag_program = create_separable_program(frag_source, gl::FRAGMENT_SHADER);
        if frag_program != 0 {
            unsafe {
                let name = std::ffi::CString::new("color_texture").unwrap();
                let location = gl::GetUniformLocation(frag_program, name.as_ptr());
                if location >= 0 {
                    gl::ProgramUniform1i(frag_program, location, 0);
                }
            }
        }

        // Create vertex buffer and VAO for quad vertices.
        // Port of upstream constructor: CreateBuffers, NamedBufferData, vertex attrib setup.
        let mut vertex_buffer: u32 = 0;
        let mut vao: u32 = 0;
        let mut vertex_buffer_address = 0;
        unsafe {
            gl::CreateBuffers(1, &mut vertex_buffer);
            gl::NamedBufferData(
                vertex_buffer,
                (4 * std::mem::size_of::<ScreenRectVertex>()) as isize,
                std::ptr::null(),
                gl::STREAM_DRAW,
            );

            let has_unified_vertex_buffers = device
                .as_ref()
                .is_some_and(|device| device.has_vertex_buffer_unified_memory());
            if has_unified_vertex_buffers {
                let make_resident = GL_MAKE_NAMED_BUFFER_RESIDENT_NV
                    .get()
                    .and_then(|f| *f)
                    .expect("glMakeNamedBufferResidentNV must be loaded for present bindless VBO");
                make_resident(vertex_buffer, gl::READ_ONLY);

                let get_address = GL_GET_NAMED_BUFFER_PARAMETER_UI64V_NV
                    .get()
                    .and_then(|f| *f)
                    .expect(
                        "glGetNamedBufferParameterui64vNV must be loaded for present bindless VBO",
                    );
                get_address(
                    vertex_buffer,
                    GL_BUFFER_GPU_ADDRESS_NV,
                    &mut vertex_buffer_address,
                );
            }

            gl::CreateVertexArrays(1, &mut vao);
            gl::VertexArrayVertexBuffer(
                vao,
                0,
                vertex_buffer,
                0,
                std::mem::size_of::<ScreenRectVertex>() as i32,
            );

            // position attribute (location 0)
            gl::EnableVertexArrayAttrib(vao, POSITION_LOCATION as u32);
            gl::VertexArrayAttribFormat(vao, POSITION_LOCATION as u32, 2, gl::FLOAT, gl::FALSE, 0);
            gl::VertexArrayAttribBinding(vao, POSITION_LOCATION as u32, 0);

            // tex_coord attribute (location 1)
            gl::EnableVertexArrayAttrib(vao, TEX_COORD_LOCATION as u32);
            gl::VertexArrayAttribFormat(vao, TEX_COORD_LOCATION as u32, 2, gl::FLOAT, gl::FALSE, 8);
            gl::VertexArrayAttribBinding(vao, TEX_COORD_LOCATION as u32, 0);
        }

        Self {
            device,
            sampler,
            vert_program,
            frag_program,
            vertex_buffer,
            vertex_buffer_address,
            vao,
        }
    }

    /// Draw all layers to the current framebuffer.
    ///
    /// Port of `WindowAdaptPass::DrawToFramebuffer()`.
    ///
    /// Upstream flow:
    /// 1. Save current framebuffer bindings
    /// 2. For each layer: ConfigureDraw → get texture, matrix, vertices
    /// 3. Restore framebuffer bindings
    /// 4. Bind present program, set state
    /// 5. For each layer: set blending, bind texture, upload matrix + vertices, draw
    pub fn draw_to_framebuffer(
        &self,
        layers: &mut [Layer],
        framebuffers: &[FramebufferConfig],
        layout: &FramebufferLayout,
        invert_y: bool,
        program_manager: &ProgramManagerHandle,
    ) {
        if self.vert_program == 0 || self.frag_program == 0 {
            return;
        }

        let layer_count = framebuffers.len();

        // Save current framebuffer bindings (upstream saves and restores).
        let (old_read_fb, old_draw_fb) = unsafe {
            let mut read_fb: i32 = 0;
            let mut draw_fb: i32 = 0;
            gl::GetIntegerv(gl::READ_FRAMEBUFFER_BINDING, &mut read_fb);
            gl::GetIntegerv(gl::DRAW_FRAMEBUFFER_BINDING, &mut draw_fb);
            (read_fb, draw_fb)
        };

        // Phase 1: Configure all layers (may modify framebuffer bindings internally).
        let mut textures = Vec::with_capacity(layer_count);
        let mut matrices = Vec::with_capacity(layer_count);
        let mut vertices = Vec::with_capacity(layer_count);

        for i in 0..layer_count {
            let mut matrix = [0.0f32; 6];
            let mut verts = [ScreenRectVertex::default(); 4];
            let mut program_manager = program_manager.lock();
            let texture = layers[i].configure_draw(
                &mut matrix,
                &mut verts,
                &framebuffers[i],
                layout,
                invert_y,
                &mut program_manager,
            );
            textures.push(texture);
            trace_present_texture("after_configure_draw", i, texture, &framebuffers[i]);
            matrices.push(matrix);
            vertices.push(verts);
        }

        // Restore framebuffer bindings.
        unsafe {
            gl::BindFramebuffer(gl::READ_FRAMEBUFFER, old_read_fb as u32);
            gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, old_draw_fb as u32);
        }

        // Phase 2: Render all layers.
        let mut program_manager_guard = program_manager.lock();
        program_manager_guard.bind_present_programs(self.vert_program, self.frag_program);
        unsafe {
            gl::Disable(gl::FRAMEBUFFER_SRGB);
            gl::ViewportIndexedf(0, 0.0, 0.0, layout.width as f32, layout.height as f32);

            gl::BindVertexArray(self.vao);
            gl::EnableVertexAttribArray(POSITION_LOCATION as u32);
            gl::EnableVertexAttribArray(TEX_COORD_LOCATION as u32);
            gl::VertexAttribDivisor(POSITION_LOCATION as u32, 0);
            gl::VertexAttribDivisor(TEX_COORD_LOCATION as u32, 0);
            gl::VertexAttribFormat(POSITION_LOCATION as u32, 2, gl::FLOAT, gl::FALSE, 0);
            gl::VertexAttribFormat(TEX_COORD_LOCATION as u32, 2, gl::FLOAT, gl::FALSE, 8);
            gl::VertexAttribBinding(POSITION_LOCATION as u32, 0);
            gl::VertexAttribBinding(TEX_COORD_LOCATION as u32, 0);
            let has_unified_vertex_buffers = self
                .device
                .as_ref()
                .is_some_and(|device| device.has_vertex_buffer_unified_memory());
            if has_unified_vertex_buffers {
                gl::BindVertexBuffer(0, 0, 0, std::mem::size_of::<ScreenRectVertex>() as i32);
                let buffer_address_range = GL_BUFFER_ADDRESS_RANGE_NV
                    .get()
                    .and_then(|f| *f)
                    .expect("glBufferAddressRangeNV must be loaded for present bindless VBO");
                buffer_address_range(
                    GL_VERTEX_ATTRIB_ARRAY_ADDRESS_NV,
                    0,
                    self.vertex_buffer_address,
                    std::mem::size_of::<[ScreenRectVertex; 4]>() as isize,
                );
            } else {
                gl::BindVertexBuffer(
                    0,
                    self.vertex_buffer,
                    0,
                    std::mem::size_of::<ScreenRectVertex>() as i32,
                );
            }
            gl::BindSampler(0, self.sampler);

            let settings = common::settings::values();
            let bg_red = *settings.bg_red.get_value() as f32 / 255.0;
            let bg_green = *settings.bg_green.get_value() as f32 / 255.0;
            let bg_blue = *settings.bg_blue.get_value() as f32 / 255.0;
            drop(settings);

            // Update background color before drawing.
            // Upstream: Settings::values.bg_red/green/blue.
            gl::ClearColor(bg_red, bg_green, bg_blue, 1.0);
            gl::Clear(gl::COLOR_BUFFER_BIT);
            trace_present_phase("after_clear", None, layout.width, layout.height);

            for i in 0..layer_count {
                // Set blending mode per-layer.
                // Port of upstream switch on framebuffers[i].blending.
                match framebuffers[i].blending {
                    BlendMode::Opaque => {
                        gl::Disablei(gl::BLEND, 0);
                    }
                    BlendMode::Premultiplied => {
                        gl::Enablei(gl::BLEND, 0);
                        gl::BlendFuncSeparatei(
                            0,
                            gl::ONE,
                            gl::ONE_MINUS_SRC_ALPHA,
                            gl::ONE,
                            gl::ZERO,
                        );
                    }
                    BlendMode::Coverage => {
                        gl::Enablei(gl::BLEND, 0);
                        gl::BlendFuncSeparatei(
                            0,
                            gl::SRC_ALPHA,
                            gl::ONE_MINUS_SRC_ALPHA,
                            gl::ONE,
                            gl::ZERO,
                        );
                    }
                }

                // Bind texture.
                gl::BindTextureUnit(0, textures[i]);

                // Upload orthographic matrix (location 0, mat3x2).
                gl::ProgramUniformMatrix3x2fv(
                    self.vert_program,
                    MODEL_VIEW_MATRIX_LOCATION,
                    1,
                    gl::FALSE,
                    matrices[i].as_ptr(),
                );

                // Upload vertex data to VBO.
                gl::NamedBufferSubData(
                    self.vertex_buffer,
                    0,
                    (4 * std::mem::size_of::<ScreenRectVertex>()) as isize,
                    vertices[i].as_ptr() as *const _,
                );

                // Draw quad as triangle strip.
                gl::DrawArrays(gl::TRIANGLE_STRIP, 0, 4);
                trace_present_phase("after_draw", Some(i), layout.width, layout.height);

                if std::env::var_os("RUZU_TRACE_PRESENT_DRAW").is_some() {
                    let scissor_enabled = gl::IsEnabledi(gl::SCISSOR_TEST, 0);
                    let rasterizer_discard = gl::IsEnabled(gl::RASTERIZER_DISCARD);
                    let cull_face = gl::IsEnabled(gl::CULL_FACE);
                    let depth_test = gl::IsEnabled(gl::DEPTH_TEST);
                    let stencil_test = gl::IsEnabled(gl::STENCIL_TEST);
                    let mut viewport = [0; 4];
                    let mut scissor_box = [0; 4];
                    let mut color_mask = [0; 4];
                    gl::GetIntegeri_v(gl::VIEWPORT, 0, viewport.as_mut_ptr());
                    gl::GetIntegeri_v(gl::SCISSOR_BOX, 0, scissor_box.as_mut_ptr());
                    gl::GetBooleani_v(gl::COLOR_WRITEMASK, 0, color_mask.as_mut_ptr());
                    let gl_error = gl::GetError();
                    log::info!(
                        "[PRESENT_DRAW] layer={} vert_program={} frag_program={} texture={} layout={}x{} screen=({},{}..{},{} ) blend={:?} viewport={:?} scissor_enabled={} scissor_box={:?} color_mask={:?} rasterizer_discard={} cull_face={} depth_test={} stencil_test={} matrix={:?} vertices={:?} gl_error=0x{:X}",
                        i,
                        self.vert_program,
                        self.frag_program,
                        textures[i],
                        layout.width,
                        layout.height,
                        layout.screen.left,
                        layout.screen.top,
                        layout.screen.right,
                        layout.screen.bottom,
                        framebuffers[i].blending,
                        viewport,
                        scissor_enabled,
                        scissor_box,
                        color_mask,
                        rasterizer_discard,
                        cull_face,
                        depth_test,
                        stencil_test,
                        matrices[i],
                        vertices[i],
                        gl_error
                    );
                }
            }

            gl::BindVertexArray(0);
        }
    }
}

impl Drop for WindowAdaptPass {
    fn drop(&mut self) {
        unsafe {
            if self.vert_program != 0 {
                gl::DeleteProgram(self.vert_program);
            }
            if self.frag_program != 0 {
                gl::DeleteProgram(self.frag_program);
            }
            if self.sampler != 0 {
                gl::DeleteSamplers(1, &self.sampler);
            }
            if self.vertex_buffer != 0 {
                gl::DeleteBuffers(1, &self.vertex_buffer);
            }
            if self.vao != 0 {
                gl::DeleteVertexArrays(1, &self.vao);
            }
        }
    }
}
