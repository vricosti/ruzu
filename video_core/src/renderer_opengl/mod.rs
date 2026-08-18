// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! OpenGL renderer backend.

pub mod blit_image;
pub mod gl_blit_screen;
pub mod gl_buffer_cache;
pub mod gl_buffer_cache_base;
pub mod gl_compute_pipeline;
pub mod gl_device;
pub mod gl_fence_manager;
pub mod gl_graphics_pipeline;
pub mod gl_query_cache;
pub mod gl_rasterizer;
pub mod gl_resource_manager;
pub mod gl_shader_cache;
pub mod gl_shader_context;
pub mod gl_shader_manager;
pub mod gl_shader_util;
pub mod gl_staging_buffer_pool;
pub mod gl_state_tracker;
pub mod gl_texture_cache;
pub mod gl_texture_cache_base;
pub mod maxwell_to_gl;
pub mod present;
pub mod renderer_opengl;
pub mod util_shaders;

pub use gl_device::Device;
pub use gl_rasterizer::RasterizerOpenGL;
pub use gl_state_tracker::StateTracker;
pub use renderer_opengl::{OpenGLError, RendererOpenGL};
