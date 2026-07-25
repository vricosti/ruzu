// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! VkRenderPass cache keyed by render target format configuration.
//!
//! Ref: zuyu `vk_render_pass_cache.h` — caches VkRenderPass objects to avoid
//! redundant creation for identical render target configurations.

use std::collections::HashMap;

use ash::vk;
use log::debug;

use super::fixed_pipeline_state::FixedPipelineState;
use super::maxwell_to_vk;
use crate::surface::{
    get_format_type, pixel_format_from_depth_format, pixel_format_from_render_target_format,
    PixelFormat, SurfaceType,
};
use crate::textures::texture::MsaaMode;
use crate::vulkan_common::vulkan_device::format_alternatives;

/// Key for render pass lookup — color formats + depth format + samples.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RenderPassKey {
    pub color_formats: [PixelFormat; 8],
    pub depth_format: PixelFormat,
    pub samples: vk::SampleCountFlags,
}

impl Default for RenderPassKey {
    fn default() -> Self {
        Self {
            color_formats: [PixelFormat::Invalid; 8],
            depth_format: PixelFormat::Invalid,
            samples: vk::SampleCountFlags::TYPE_1,
        }
    }
}

impl RenderPassKey {
    /// Port of upstream `MakeRenderPassKey(const FixedPipelineState&)` from
    /// `vk_graphics_pipeline.cpp`.
    pub fn from_fixed_pipeline_state(state: &FixedPipelineState) -> Self {
        let mut key = RenderPassKey::default();
        for (index, &encoded_format) in state.color_formats.iter().enumerate() {
            if encoded_format == 0 {
                key.color_formats[index] = PixelFormat::Invalid;
                continue;
            }
            key.color_formats[index] =
                pixel_format_from_render_target_format(encoded_format as u32);
        }
        if state.depth_enabled() {
            key.depth_format = pixel_format_from_depth_format(state.depth_format());
        }
        let msaa_mode = MsaaMode::from_raw(state.msaa_mode_raw()).unwrap_or(MsaaMode::Msaa1x1);
        key.samples = maxwell_to_vk::msaa_mode(msaa_mode);
        key
    }
}

/// Caches VkRenderPass objects by render target configuration.
///
/// Ref: zuyu RenderPassCache — avoids re-creating VkRenderPass objects when
/// the render target format configuration hasn't changed.
pub struct RenderPassCache {
    device: ash::Device,
    instance: ash::Instance,
    physical_device: vk::PhysicalDevice,
    cache: HashMap<RenderPassKey, vk::RenderPass>,
}

impl RenderPassCache {
    pub fn new(
        device: ash::Device,
        instance: ash::Instance,
        physical_device: vk::PhysicalDevice,
    ) -> Self {
        Self {
            device,
            instance,
            physical_device,
            cache: HashMap::new(),
        }
    }

    /// Get or create a VkRenderPass for the given key.
    pub fn get(&mut self, key: &RenderPassKey) -> Result<vk::RenderPass, vk::Result> {
        if let Some(&rp) = self.cache.get(key) {
            return Ok(rp);
        }

        let rp = self.create_render_pass(key)?;
        self.cache.insert(key.clone(), rp);
        debug!(
            "RenderPassCache: created new render pass (depth={:?})",
            key.depth_format,
        );
        Ok(rp)
    }

    /// Port of `MaxwellToVK::SurfaceFormat(device, FormatType::Optimal, true, format)`.
    fn surface_format(&self, pixel_format: PixelFormat) -> vk::Format {
        let format_info = maxwell_to_vk::surface_format(pixel_format);
        let mut usage = vk::FormatFeatureFlags::SAMPLED_IMAGE
            | vk::FormatFeatureFlags::TRANSFER_DST
            | vk::FormatFeatureFlags::TRANSFER_SRC;
        if format_info.attachable {
            usage |= match get_format_type(pixel_format) {
                SurfaceType::ColorTexture => vk::FormatFeatureFlags::COLOR_ATTACHMENT,
                SurfaceType::Depth | SurfaceType::Stencil | SurfaceType::DepthStencil => {
                    vk::FormatFeatureFlags::DEPTH_STENCIL_ATTACHMENT
                }
                SurfaceType::Invalid => vk::FormatFeatureFlags::empty(),
            };
        }
        if format_info.storage {
            usage |= vk::FormatFeatureFlags::STORAGE_IMAGE;
        }
        if self.is_format_supported(format_info.format, usage) {
            return format_info.format;
        }
        format_alternatives(format_info.format)
            .into_iter()
            .flatten()
            .copied()
            .find(|&format| self.is_format_supported(format, usage))
            .unwrap_or(format_info.format)
    }

    fn is_format_supported(&self, format: vk::Format, usage: vk::FormatFeatureFlags) -> bool {
        let properties = unsafe {
            self.instance
                .get_physical_device_format_properties(self.physical_device, format)
        };
        properties.optimal_tiling_features.contains(usage)
    }

    fn create_render_pass(&self, key: &RenderPassKey) -> Result<vk::RenderPass, vk::Result> {
        let mut attachments = Vec::new();
        let mut color_refs = Vec::new();
        let mut num_attachments = 0usize;
        let mut num_colors = 0u32;

        // Color attachments. Upstream keeps the original RT slot indices in
        // pColorAttachments and uses VK_ATTACHMENT_UNUSED for holes; only the
        // VkFramebuffer attachment array is compacted to the actually-bound
        // views. Do not compact these references or Location(N) fragment
        // outputs target the wrong attachment.
        for i in 0..key.color_formats.len() {
            let pixel_format = key.color_formats[i];
            if pixel_format == PixelFormat::Invalid {
                color_refs.push(vk::AttachmentReference {
                    attachment: vk::ATTACHMENT_UNUSED,
                    layout: vk::ImageLayout::GENERAL,
                });
                continue;
            }
            // Upstream `vk_render_pass_cache.cpp` uses one AttachmentDescription
            // for every render-target attachment: LOAD/STORE (contents persist;
            // clears are explicit vkCmdClearAttachments), and GENERAL layout
            // throughout so attachments can be used, sampled and presented
            // without per-use layout transitions.
            color_refs.push(vk::AttachmentReference {
                attachment: num_colors,
                layout: vk::ImageLayout::GENERAL,
            });
            num_attachments = i + 1;
            num_colors += 1;
            attachments.push(
                vk::AttachmentDescription::builder()
                    .format(self.surface_format(pixel_format))
                    .samples(key.samples)
                    .load_op(vk::AttachmentLoadOp::LOAD)
                    .store_op(vk::AttachmentStoreOp::STORE)
                    .stencil_load_op(vk::AttachmentLoadOp::LOAD)
                    .stencil_store_op(vk::AttachmentStoreOp::STORE)
                    .initial_layout(vk::ImageLayout::GENERAL)
                    .final_layout(vk::ImageLayout::GENERAL)
                    .build(),
            );
        }

        // Depth attachment
        let depth_ref;
        let has_depth = key.depth_format != PixelFormat::Invalid;
        if has_depth {
            depth_ref = Some(vk::AttachmentReference {
                attachment: num_colors,
                layout: vk::ImageLayout::GENERAL,
            });
            // Same as the colour attachments (upstream vk_render_pass_cache.cpp):
            // LOAD/STORE with GENERAL layout, so the depth/stencil buffer
            // persists across passes and can be sampled. Guest depth clears are
            // honoured via explicit vkCmdClearAttachments in RasterizerVulkan.
            attachments.push(
                vk::AttachmentDescription::builder()
                    .format(self.surface_format(key.depth_format))
                    .samples(key.samples)
                    .load_op(vk::AttachmentLoadOp::LOAD)
                    .store_op(vk::AttachmentStoreOp::STORE)
                    .stencil_load_op(vk::AttachmentLoadOp::LOAD)
                    .stencil_store_op(vk::AttachmentStoreOp::STORE)
                    .initial_layout(vk::ImageLayout::GENERAL)
                    .final_layout(vk::ImageLayout::GENERAL)
                    .build(),
            );
        } else {
            depth_ref = None;
        }

        let mut subpass = vk::SubpassDescription::builder()
            .pipeline_bind_point(vk::PipelineBindPoint::GRAPHICS)
            .color_attachments(&color_refs[..num_attachments]);
        if let Some(ref dr) = depth_ref {
            subpass = subpass.depth_stencil_attachment(dr);
        }
        let subpass = subpass.build();

        let render_pass_info = vk::RenderPassCreateInfo::builder()
            .attachments(&attachments)
            .subpasses(std::slice::from_ref(&subpass))
            .build();

        unsafe { self.device.create_render_pass(&render_pass_info, None) }
    }
}

impl Drop for RenderPassCache {
    fn drop(&mut self) {
        for (_, rp) in self.cache.drain() {
            unsafe {
                self.device.destroy_render_pass(rp, None);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_render_pass_key_default() {
        let key = RenderPassKey::default();
        assert!(key
            .color_formats
            .iter()
            .all(|&format| format == PixelFormat::Invalid));
        assert_eq!(key.depth_format, PixelFormat::Invalid);
        assert_eq!(key.samples, vk::SampleCountFlags::TYPE_1);
    }

    #[test]
    fn test_render_pass_key_equality() {
        let mut a = RenderPassKey::default();
        let mut b = RenderPassKey::default();
        a.color_formats[0] = PixelFormat::A8B8G8R8Unorm;
        b.color_formats[0] = PixelFormat::A8B8G8R8Unorm;
        assert_eq!(a, b);
    }

    #[test]
    fn test_render_pass_key_different_format() {
        let mut a = RenderPassKey::default();
        let mut b = RenderPassKey::default();
        a.color_formats[0] = PixelFormat::A8B8G8R8Unorm;
        b.color_formats[0] = PixelFormat::B8G8R8A8Unorm;
        assert_ne!(a, b);
    }

    #[test]
    fn render_pass_key_preserves_guest_pixel_format() {
        let mut state = FixedPipelineState::default();
        state.color_formats[0] = 0xD1; // A2B10G10R10_UNORM

        let key = RenderPassKey::from_fixed_pipeline_state(&state);

        assert_eq!(key.color_formats[0], PixelFormat::A2B10G10R10Unorm);
    }
}
