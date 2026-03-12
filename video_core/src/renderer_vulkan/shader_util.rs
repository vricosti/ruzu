// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_shader_util.h` / `vk_shader_util.cpp`.
//!
//! Utility for building Vulkan shader modules from SPIR-V code.

use ash::vk;

/// Port of `BuildShader`.
///
/// Creates a `VkShaderModule` from SPIR-V `u32` words.
pub fn build_shader(_device: &ash::Device, _code: &[u32]) -> vk::ShaderModule {
    todo!("build_shader")
}
