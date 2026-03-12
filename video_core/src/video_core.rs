// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/video_core.h and video_core/video_core.cpp
//!
//! Main entry point for the GPU subsystem. Creates the GPU instance and
//! renderer based on settings.

use crate::gpu::Gpu;

/// Renderer backend selection.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RendererBackend {
    OpenGL,
    Vulkan,
    Null,
}

/// NVDEC emulation mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NvdecEmulation {
    Off,
    Cpu,
    Gpu,
}

/// Creates an emulated GPU instance.
///
/// In the full port, this reads settings for renderer backend, NVDEC mode,
/// and async GPU emulation. It creates the appropriate renderer (OpenGL, Vulkan,
/// or Null) and binds it to the GPU.
pub fn create_gpu(
    use_async: bool,
    use_nvdec: bool,
    _backend: RendererBackend,
) -> Result<Gpu, String> {
    // Settings::UpdateRescalingInfo();
    let gpu = Gpu::new(use_async, use_nvdec);

    // In the full port:
    // let context = emu_window.create_shared_context();
    // let _scope = context.acquire();
    // let renderer = create_renderer(system, emu_window, &gpu, context, backend);
    // gpu.bind_renderer(renderer);

    Ok(gpu)
}
