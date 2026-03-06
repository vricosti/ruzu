// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! VkRenderPass cache keyed by render target format configuration.
//!
//! Ref: zuyu `vk_render_pass_cache.h` — caches VkRenderPass objects to avoid
//! redundant creation for identical render target configurations.

use std::collections::HashMap;

use ash::vk;
use log::debug;

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
            "RenderPassCache: created new render pass ({} color, depth={:?})",
            key.num_color_attachments, key.depth_format
        );
        Ok(rp)
    }

    fn create_render_pass(&self, key: &RenderPassKey) -> Result<vk::RenderPass, vk::Result> {
        let mut attachments = Vec::new();
        let mut color_refs = Vec::new();

        // Color attachments
        for i in 0..key.num_color_attachments as usize {
            let format = key.color_formats[i];
            if format == vk::Format::UNDEFINED {
                continue;
            }
            color_refs.push(vk::AttachmentReference {
                attachment: attachments.len() as u32,
                layout: vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL,
            });
            attachments.push(
                vk::AttachmentDescription::builder()
                    .format(format)
                    .samples(key.samples)
                    .load_op(vk::AttachmentLoadOp::LOAD)
                    .store_op(vk::AttachmentStoreOp::STORE)
                    .stencil_load_op(vk::AttachmentLoadOp::DONT_CARE)
                    .stencil_store_op(vk::AttachmentStoreOp::DONT_CARE)
                    .initial_layout(vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL)
                    .final_layout(vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL)
                    .build(),
            );
        }

        // If no color attachments, add a default RGBA8 one
        if color_refs.is_empty() {
            color_refs.push(vk::AttachmentReference {
                attachment: 0,
                layout: vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL,
            });
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
                attachment: attachments.len() as u32,
                layout: vk::ImageLayout::DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
            });
            attachments.push(
                vk::AttachmentDescription::builder()
                    .format(key.depth_format)
                    .samples(key.samples)
                    .load_op(vk::AttachmentLoadOp::CLEAR)
                    .store_op(vk::AttachmentStoreOp::DONT_CARE)
                    .stencil_load_op(vk::AttachmentLoadOp::DONT_CARE)
                    .stencil_store_op(vk::AttachmentStoreOp::DONT_CARE)
                    .initial_layout(vk::ImageLayout::UNDEFINED)
                    .final_layout(vk::ImageLayout::DEPTH_STENCIL_ATTACHMENT_OPTIMAL)
                    .build(),
            );
        } else {
            depth_ref = None;
        }

        let mut subpass = vk::SubpassDescription::builder()
            .pipeline_bind_point(vk::PipelineBindPoint::GRAPHICS)
            .color_attachments(&color_refs);
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
}
