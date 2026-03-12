// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_shader_context.h
//!
//! Shader compilation context — provides per-thread GL context and object pools
//! for parallel shader compilation.

/// Shader object pools for reuse during compilation.
///
/// Corresponds to `OpenGL::ShaderContext::ShaderPools`.
pub struct ShaderPools {
    // TODO: ObjectPool<IR::Inst>, ObjectPool<IR::Block>, ObjectPool<Flow::Block>
    // These depend on the shader recompiler crate.
}

impl ShaderPools {
    /// Create new shader pools.
    pub fn new() -> Self {
        Self {}
    }

    /// Release all contents back to the pools.
    ///
    /// Corresponds to `ShaderPools::ReleaseContents()`.
    pub fn release_contents(&mut self) {
        // TODO: Release pool contents
    }
}

/// Per-thread shader compilation context.
///
/// Corresponds to `OpenGL::ShaderContext::Context`.
pub struct Context {
    pub pools: ShaderPools,
    // TODO: gl_context (shared GL context), scoped (RAII context activation)
}

impl Context {
    /// Create a new shader compilation context.
    pub fn new() -> Self {
        Self {
            pools: ShaderPools::new(),
        }
    }
}
