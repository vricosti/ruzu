// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/present/util.h
//!
//! Utility functions for the presentation pipeline (sampler creation, shader include replacement).

/// Replace an `#include "name"` directive in a shader source with the given content.
///
/// Corresponds to `OpenGL::ReplaceInclude()`.
pub fn replace_include(shader_source: &mut String, include_name: &str, include_content: &str) {
    let include_string = format!("#include \"{}\"", include_name);
    if let Some(pos) = shader_source.find(&include_string) {
        shader_source.replace_range(pos..pos + include_string.len(), include_content);
    } else {
        panic!(
            "Include directive '{}' not found in shader source",
            include_string
        );
    }
}

/// Create a bilinear sampler with clamp-to-edge wrapping.
///
/// Corresponds to `OpenGL::CreateBilinearSampler()`.
pub fn create_bilinear_sampler() -> u32 {
    let mut sampler: u32 = 0;
    unsafe {
        gl::GenSamplers(1, &mut sampler);
        gl::SamplerParameteri(sampler, gl::TEXTURE_MIN_FILTER, gl::LINEAR as i32);
        gl::SamplerParameteri(sampler, gl::TEXTURE_MAG_FILTER, gl::LINEAR as i32);
        gl::SamplerParameteri(sampler, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as i32);
        gl::SamplerParameteri(sampler, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as i32);
        gl::SamplerParameteri(sampler, gl::TEXTURE_WRAP_R, gl::CLAMP_TO_EDGE as i32);
    }
    sampler
}

/// Create a nearest-neighbor sampler with clamp-to-edge wrapping.
///
/// Corresponds to `OpenGL::CreateNearestNeighborSampler()`.
pub fn create_nearest_neighbor_sampler() -> u32 {
    let mut sampler: u32 = 0;
    unsafe {
        gl::GenSamplers(1, &mut sampler);
        gl::SamplerParameteri(sampler, gl::TEXTURE_MIN_FILTER, gl::NEAREST as i32);
        gl::SamplerParameteri(sampler, gl::TEXTURE_MAG_FILTER, gl::NEAREST as i32);
        gl::SamplerParameteri(sampler, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as i32);
        gl::SamplerParameteri(sampler, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as i32);
        gl::SamplerParameteri(sampler, gl::TEXTURE_WRAP_R, gl::CLAMP_TO_EDGE as i32);
    }
    sampler
}
