// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/video_core.h and video_core/video_core.cpp
//!
//! Main entry point for the GPU subsystem. Creates the GPU instance and
//! renderer based on settings.

use crate::gpu::Gpu;
use crate::renderer_base::RendererBase;

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

/// Creates an emulated GPU instance and binds a renderer to it.
///
/// Upstream: `VideoCore::CreateGPU(EmuWindow&, System&)` in video_core.cpp.
///
/// The caller provides:
/// - `renderer`: a fully constructed renderer (created by the frontend which
///   has access to the window system / graphics context).
///
/// This avoids video_core depending on SDL or any windowing library.
pub fn create_gpu(use_async: bool, use_nvdec: bool, renderer: Box<dyn RendererBase>) -> Gpu {
    // Upstream: Settings::UpdateRescalingInfo();
    let gpu = Gpu::new(use_async, use_nvdec);

    // Upstream flow:
    // auto renderer = CreateRenderer(system, emu_window, *gpu, context);
    // gpu->BindRenderer(std::move(renderer));
    gpu.bind_renderer(renderer);

    gpu
}
