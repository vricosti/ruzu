// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of Eden `video_core/renderer_opengl/present/window_adapt_pass.{h,cpp}`.
//!
//! Window adapt pass -- final composition of layers onto the window framebuffer
//! using a configurable fragment shader (filter).

use super::layer::Layer;
use super::present_uniforms::{
    ScreenRectVertex, MODEL_VIEW_MATRIX_LOCATION, POSITION_LOCATION, TEX_COORD_LOCATION,
};
use crate::framebuffer_config::{BlendMode, FramebufferConfig};
use crate::host_shaders::vertex_shaders::OPENGL_PRESENT_VERT;
use crate::renderer_opengl::gl_resource_manager::{OGLBuffer, OGLProgram, OGLSampler};
use crate::renderer_opengl::gl_shader_manager::ProgramManagerHandle;
use crate::renderer_opengl::gl_shader_util::create_program_from_source;
use crate::renderer_opengl::Device;
use ruzu_core::frontend::framebuffer_layout::FramebufferLayout;
use std::collections::LinkedList;
use std::ffi::c_void;
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

const GL_BUFFER_GPU_ADDRESS_NV: u32 = 0x8F1D;
const GL_VERTEX_ATTRIB_ARRAY_ADDRESS_NV: u32 = 0x8F20;

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

/// Window adapt pass for final framebuffer composition.
///
/// Corresponds to `OpenGL::WindowAdaptPass`.
pub struct WindowAdaptPass {
    // Rust drops fields in declaration order; list resources in the reverse
    // order of Eden's C++ members.
    vertex_buffer: OGLBuffer,
    frag: OGLProgram,
    vert: OGLProgram,
    sampler: OGLSampler,
    device: *const Device,
    vertex_buffer_address: u64,
}

impl WindowAdaptPass {
    /// Create a new window adapt pass with the given sampler and fragment shader.
    ///
    /// Port of `WindowAdaptPass::WindowAdaptPass()`.
    pub fn new(device: *const Device, sampler: OGLSampler, frag_source: &str) -> Self {
        let vert = create_program_from_source(OPENGL_PRESENT_VERT, gl::VERTEX_SHADER);
        let frag = create_program_from_source(frag_source, gl::FRAGMENT_SHADER);

        // Create the vertex buffer used for the presentation quad.
        let mut vertex_buffer = OGLBuffer::new();
        vertex_buffer.create();
        let mut vertex_buffer_address = 0;
        unsafe {
            gl::NamedBufferData(
                vertex_buffer.handle,
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
                make_resident(vertex_buffer.handle, gl::READ_ONLY);

                let get_address = GL_GET_NAMED_BUFFER_PARAMETER_UI64V_NV
                    .get()
                    .and_then(|f| *f)
                    .expect(
                        "glGetNamedBufferParameterui64vNV must be loaded for present bindless VBO",
                    );
                get_address(
                    vertex_buffer.handle,
                    GL_BUFFER_GPU_ADDRESS_NV,
                    &mut vertex_buffer_address,
                );
            }
        }

        Self {
            vertex_buffer,
            frag,
            vert,
            sampler,
            device,
            vertex_buffer_address,
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
        layers: &mut LinkedList<Layer>,
        framebuffers: &[FramebufferConfig],
        layout: &FramebufferLayout,
        invert_y: bool,
        program_manager: &ProgramManagerHandle,
    ) {
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

        let mut program_manager = program_manager.lock();
        for (layer, framebuffer) in layers.iter_mut().zip(framebuffers.iter()) {
            let mut matrix = [0.0f32; 6];
            let mut verts = [ScreenRectVertex::default(); 4];
            let texture = layer.configure_draw(
                &mut matrix,
                &mut verts,
                framebuffer,
                layout,
                invert_y,
                &mut program_manager,
            );
            textures.push(texture);
            matrices.push(matrix);
            vertices.push(verts);
        }
        // Restore framebuffer bindings.
        unsafe {
            gl::BindFramebuffer(gl::READ_FRAMEBUFFER, old_read_fb as u32);
            gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, old_draw_fb as u32);
        }

        // Phase 2: Render all layers.
        program_manager.bind_present_programs(self.vert.handle, self.frag.handle);
        unsafe {
            gl::Disable(gl::FRAMEBUFFER_SRGB);
            gl::ViewportIndexedf(0, 0.0, 0.0, layout.width as f32, layout.height as f32);

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
                    self.vertex_buffer.handle,
                    0,
                    std::mem::size_of::<ScreenRectVertex>() as i32,
                );
            }
            gl::BindSampler(0, self.sampler.handle);

            let settings = common::settings::values();
            let bg_red = *settings.bg_red.get_value() as f32 / 255.0;
            let bg_green = *settings.bg_green.get_value() as f32 / 255.0;
            let bg_blue = *settings.bg_blue.get_value() as f32 / 255.0;
            drop(settings);

            // Update background color before drawing.
            // Upstream: Settings::values.bg_red/green/blue.
            gl::ClearColor(bg_red, bg_green, bg_blue, 1.0);
            gl::Clear(gl::COLOR_BUFFER_BIT);

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
                    self.vert.handle,
                    MODEL_VIEW_MATRIX_LOCATION,
                    1,
                    gl::FALSE,
                    matrices[i].as_ptr(),
                );

                // Upload vertex data to VBO.
                gl::NamedBufferSubData(
                    self.vertex_buffer.handle,
                    0,
                    (4 * std::mem::size_of::<ScreenRectVertex>()) as isize,
                    vertices[i].as_ptr() as *const _,
                );

                // Draw quad as triangle strip.
                gl::DrawArrays(gl::TRIANGLE_STRIP, 0, 4);
            }
        }
    }
}
