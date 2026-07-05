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
use crate::surface::{pixel_format_from_depth_format, pixel_format_from_render_target_format};
use crate::textures::texture::MsaaMode;

/// Key for render pass lookup — color formats + depth format + samples.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RenderPassKey {
    pub color_formats: [vk::Format; 8],
    pub num_color_attachments: u8,
    pub depth_format: vk::Format,
    pub samples: vk::SampleCountFlags,
}

impl Default for RenderPassKey {
    fn default() -> Self {
        Self {
            color_formats: [vk::Format::UNDEFINED; 8],
            num_color_attachments: 0,
            depth_format: vk::Format::UNDEFINED,
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
                key.color_formats[index] = vk::Format::UNDEFINED;
                continue;
            }
            let pixel_format = pixel_format_from_render_target_format(encoded_format as u32);
            // Use the authoritative surface-format table (same one that creates
            // the RT images), so the render-pass key matches the attachments,
            // matching upstream `MakeRenderPassKey` → `MaxwellToVK::SurfaceFormat`.
            key.color_formats[index] = maxwell_to_vk::surface_format(pixel_format).format;
            if key.color_formats[index] != vk::Format::UNDEFINED {
                key.num_color_attachments = (index + 1) as u8;
            }
        }
        if state.depth_enabled() {
            let depth_format = pixel_format_from_depth_format(state.depth_format());
            key.depth_format = maxwell_to_vk::surface_format(depth_format).format;
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
    cache: HashMap<RenderPassKey, vk::RenderPass>,
}

impl RenderPassCache {
    pub fn new(device: ash::Device) -> Self {
        Self {
            device,
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
            "RenderPassCache: created new render pass ({} color slots, depth={:?})",
            key.num_color_attachments, key.depth_format
        );
        Ok(rp)
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
            let format = key.color_formats[i];
            if format == vk::Format::UNDEFINED {
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
                    .format(format)
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

        // If no attachments are bound, keep the legacy fallback colour
        // attachment. Depth-only render passes are valid and are used by
        // upstream helper paths such as `Image::BlitScaleHelper`.
        if num_attachments == 0 && key.depth_format == vk::Format::UNDEFINED {
            color_refs.push(vk::AttachmentReference {
                attachment: 0,
                layout: vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL,
            });
            num_attachments = 1;
            attachments.push(
                vk::AttachmentDescription::builder()
                    .format(vk::Format::R8G8B8A8_UNORM)
                    .samples(key.samples)
                    .load_op(vk::AttachmentLoadOp::CLEAR)
                    .store_op(vk::AttachmentStoreOp::STORE)
                    .stencil_load_op(vk::AttachmentLoadOp::DONT_CARE)
                    .stencil_store_op(vk::AttachmentStoreOp::DONT_CARE)
                    .initial_layout(vk::ImageLayout::UNDEFINED)
                    .final_layout(vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL)
                    .build(),
            );
        }

        // Depth attachment
        let depth_ref;
        let has_depth = key.depth_format != vk::Format::UNDEFINED;
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
                    .format(key.depth_format)
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

        let dependency = vk::SubpassDependency::builder()
            .src_subpass(vk::SUBPASS_EXTERNAL)
            .dst_subpass(0)
            .src_stage_mask(
                vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT
                    | vk::PipelineStageFlags::EARLY_FRAGMENT_TESTS,
            )
            .dst_stage_mask(
                vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT
                    | vk::PipelineStageFlags::EARLY_FRAGMENT_TESTS,
            )
            .src_access_mask(vk::AccessFlags::empty())
            .dst_access_mask(
                vk::AccessFlags::COLOR_ATTACHMENT_WRITE
                    | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_WRITE,
            )
            .build();

        let render_pass_info = vk::RenderPassCreateInfo::builder()
            .attachments(&attachments)
            .subpasses(std::slice::from_ref(&subpass))
            .dependencies(std::slice::from_ref(&dependency))
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
        assert_eq!(key.num_color_attachments, 0);
        assert_eq!(key.depth_format, vk::Format::UNDEFINED);
        assert_eq!(key.samples, vk::SampleCountFlags::TYPE_1);
    }

    #[test]
    fn test_render_pass_key_equality() {
        let mut a = RenderPassKey::default();
        let mut b = RenderPassKey::default();
        a.color_formats[0] = vk::Format::R8G8B8A8_UNORM;
        a.num_color_attachments = 1;
        b.color_formats[0] = vk::Format::R8G8B8A8_UNORM;
        b.num_color_attachments = 1;
        assert_eq!(a, b);
    }

    #[test]
    fn test_render_pass_key_different_format() {
        let mut a = RenderPassKey::default();
        let mut b = RenderPassKey::default();
        a.color_formats[0] = vk::Format::R8G8B8A8_UNORM;
        b.color_formats[0] = vk::Format::B8G8R8A8_UNORM;
        assert_ne!(a, b);
    }

    #[test]
    fn render_pass_key_uses_central_surface_format_table() {
        let mut state = FixedPipelineState::default();
        state.color_formats[0] = 0xD1; // A2B10G10R10_UNORM

        let key = RenderPassKey::from_fixed_pipeline_state(&state);

        assert_eq!(key.num_color_attachments, 1);
        assert_eq!(key.color_formats[0], vk::Format::A2B10G10R10_UNORM_PACK32);
    }
}
