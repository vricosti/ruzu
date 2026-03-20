// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_shader_context.h
//!
//! Shader compilation context — provides per-thread GL context and object pools
//! for parallel shader compilation.

/// Shader object pools for reuse during compilation.
///
/// Corresponds to `OpenGL::ShaderContext::ShaderPools`.
///
/// Upstream holds three pools:
///   - `Shader::ObjectPool<Shader::IR::Inst>` (capacity 8192)
///   - `Shader::ObjectPool<Shader::IR::Block>` (capacity 32)
///   - `Shader::ObjectPool<Shader::Maxwell::Flow::Block>` (capacity 32)
///
/// These depend on the shader_recompiler crate (`Shader::IR` / `Shader::Maxwell::Flow`)
/// which is not yet ported. The pools will be added here once that crate is available.
pub struct ShaderPools {
    // Fields will be added when the shader_recompiler crate is ported:
    //   inst: ObjectPool<IR::Inst>,
    //   block: ObjectPool<IR::Block>,
    //   flow_block: ObjectPool<Flow::Block>,
}

impl ShaderPools {
    /// Create new shader pools.
    pub fn new() -> Self {
        Self {}
    }

    /// Release all contents back to the pools.
    ///
    /// Corresponds to `ShaderPools::ReleaseContents()`.
    /// Upstream calls `flow_block.ReleaseContents()`, `block.ReleaseContents()`,
    /// and `inst.ReleaseContents()` in that order. Currently a no-op because
    /// the pools depend on the shader_recompiler crate which is not yet ported.
    pub fn release_contents(&mut self) {
        // Will call pool.release_contents() on each pool once shader_recompiler is ported.
    }
}

/// Per-thread shader compilation context.
///
/// Corresponds to `OpenGL::ShaderContext::Context`.
///
/// Upstream holds:
///   - `gl_context`: a `unique_ptr<Core::Frontend::GraphicsContext>` created via
///     `emu_window.CreateSharedContext()`
///   - `scoped`: a `GraphicsContext::Scoped` RAII guard that makes the context current
///   - `pools`: `ShaderPools`
///
/// The GL context fields depend on `Core::Frontend::EmuWindow` and
/// `Core::Frontend::GraphicsContext` which are not yet integrated into the
/// OpenGL renderer pipeline. The pools field is present but empty (see `ShaderPools`).
pub struct Context {
    pub pools: ShaderPools,
    // Fields will be added when the frontend graphics context integration is available:
    //   gl_context: Box<dyn GraphicsContext>,
    //   scoped: ScopedContext (RAII activation of gl_context),
}

impl Context {
    /// Create a new shader compilation context.
    pub fn new() -> Self {
        Self {
            pools: ShaderPools::new(),
        }
    }
}
