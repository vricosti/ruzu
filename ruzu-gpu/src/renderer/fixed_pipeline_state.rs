// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Hashable representation of non-dynamic graphics pipeline state.
//!
//! Ref: zuyu `fixed_pipeline_state.h` — bit-packed for fast hashing, used as
//! a key in the graphics pipeline cache to avoid re-creating VkPipelines.

use std::hash::{Hash, Hasher};

use crate::engines::maxwell_3d::{BlendInfo, DrawCall};

/// Blend state for a single render target attachment.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlendAttachment {
    pub enabled: bool,
    pub color_src: u8,
    pub color_dst: u8,
    pub color_op: u8,
    pub alpha_src: u8,
    pub alpha_dst: u8,
    pub alpha_op: u8,
}

impl Default for BlendAttachment {
    fn default() -> Self {
        Self {
            enabled: false,
            color_src: 0,
            color_dst: 0,
            color_op: 0,
            alpha_src: 0,
            alpha_dst: 0,
            alpha_op: 0,
        }
    }
}

impl BlendAttachment {
    fn from_blend_info(info: &BlendInfo) -> Self {
        Self {
            enabled: info.enabled,
            color_src: info.color_src as u8,
            color_dst: info.color_dst as u8,
            color_op: info.color_op as u8,
            alpha_src: info.alpha_src as u8,
            alpha_dst: info.alpha_dst as u8,
            alpha_op: info.alpha_op as u8,
        }
    }
}

/// Vertex attribute packed for hashing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct VertexAttribute {
    pub buffer_index: u8,
    pub offset: u16,
    pub size: u8,
    pub attrib_type: u8,
    pub constant: bool,
    pub bgra: bool,
}

/// Hashable representation of all non-dynamic graphics pipeline state.
///
/// Ref: zuyu FixedPipelineState — captures the subset of GPU state that
/// affects VkPipeline compilation. Used as a HashMap key for pipeline caching.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FixedPipelineState {
    // Topology & tessellation
    pub topology: u8,
    pub polygon_mode: u8,
    pub msaa_mode: u8,

    // Depth/stencil
    pub depth_enabled: bool,
    pub depth_format: u8,
    pub early_z: bool,
    pub alpha_test_func: u8,

    // Color targets
    pub color_formats: [u32; 8],

    // Dynamic state subset (zuyu packs into DynamicState struct)
    pub cull_enable: bool,
    pub cull_face: u8,
    pub front_face: u8,
    pub depth_test_enable: bool,
    pub depth_write_enable: bool,
    pub depth_test_func: u8,
    pub stencil_enable: bool,
    pub depth_bias_enable: bool,
    pub primitive_restart_enable: bool,
    pub logic_op_enable: bool,
    pub logic_op: u8,

    // Blending per attachment (zuyu: BlendingAttachment × 8)
    pub attachments: [BlendAttachment; 8],

    // Vertex attributes (zuyu: VertexAttribute × 32)
    pub attributes: [VertexAttribute; 32],
    pub vertex_strides: [u16; 16],
}

impl Hash for FixedPipelineState {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.topology.hash(state);
        self.polygon_mode.hash(state);
        self.msaa_mode.hash(state);
        self.depth_enabled.hash(state);
        self.depth_format.hash(state);
        self.early_z.hash(state);
        self.alpha_test_func.hash(state);
        self.color_formats.hash(state);
        self.cull_enable.hash(state);
        self.cull_face.hash(state);
        self.front_face.hash(state);
        self.depth_test_enable.hash(state);
        self.depth_write_enable.hash(state);
        self.depth_test_func.hash(state);
        self.stencil_enable.hash(state);
        self.depth_bias_enable.hash(state);
        self.primitive_restart_enable.hash(state);
        self.logic_op_enable.hash(state);
        self.logic_op.hash(state);
        self.attachments.hash(state);
        self.attributes.hash(state);
        self.vertex_strides.hash(state);
    }
}

impl Default for FixedPipelineState {
    fn default() -> Self {
        Self {
            topology: 0,
            polygon_mode: 0,
            msaa_mode: 0,
            depth_enabled: false,
            depth_format: 0,
            early_z: false,
            alpha_test_func: 0,
            color_formats: [0; 8],
            cull_enable: false,
            cull_face: 0,
            front_face: 0,
            depth_test_enable: false,
            depth_write_enable: false,
            depth_test_func: 0,
            stencil_enable: false,
            depth_bias_enable: false,
            primitive_restart_enable: false,
            logic_op_enable: false,
            logic_op: 0,
            attachments: [BlendAttachment::default(); 8],
            attributes: [VertexAttribute::default(); 32],
            vertex_strides: [0; 16],
        }
    }
}

impl FixedPipelineState {
    /// Populate from a Maxwell3D DrawCall state snapshot.
    pub fn refresh(&mut self, draw: &DrawCall) {
        self.topology = draw.topology as u8;
        self.cull_enable = draw.rasterizer.cull_enable;
        self.cull_face = draw.rasterizer.cull_face as u8;
        self.front_face = draw.rasterizer.front_face as u8;
        self.depth_test_enable = draw.depth_stencil.depth_test_enable;
        self.depth_write_enable = draw.depth_stencil.depth_write_enable;
        self.depth_test_func = draw.depth_stencil.depth_func as u8;
        self.stencil_enable = draw.depth_stencil.stencil_enable;
        self.depth_bias_enable = draw.rasterizer.depth_bias != 0.0;

        // Populate color formats from render targets
        for (i, rt) in draw.render_targets.iter().enumerate() {
            self.color_formats[i] = rt.format;
        }

        // Populate blend attachments
        for (i, blend) in draw.blend.iter().enumerate() {
            self.attachments[i] = BlendAttachment::from_blend_info(blend);
        }

        // Populate vertex attributes
        for (i, attrib) in draw.vertex_attribs.iter().enumerate() {
            if i >= 32 {
                break;
            }
            self.attributes[i] = VertexAttribute {
                buffer_index: attrib.buffer_index as u8,
                offset: attrib.offset as u16,
                size: attrib.size as u8,
                attrib_type: attrib.attrib_type as u8,
                constant: attrib.constant,
                bgra: attrib.bgra,
            };
        }

        // Populate vertex strides
        for (i, stream) in draw.vertex_streams.iter().enumerate() {
            if i >= 16 {
                break;
            }
            self.vertex_strides[i] = stream.stride as u16;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::hash_map::DefaultHasher;
    use crate::engines::maxwell_3d::{
        BlendEquation, BlendFactor, PrimitiveTopology,
    };

    fn hash_state(state: &FixedPipelineState) -> u64 {
        let mut hasher = DefaultHasher::new();
        state.hash(&mut hasher);
        hasher.finish()
    }

    #[test]
    fn test_default_state_is_consistent() {
        let a = FixedPipelineState::default();
        let b = FixedPipelineState::default();
        assert_eq!(a, b);
        assert_eq!(hash_state(&a), hash_state(&b));
    }

    #[test]
    fn test_different_topology_gives_different_hash() {
        let mut a = FixedPipelineState::default();
        let mut b = FixedPipelineState::default();
        a.topology = PrimitiveTopology::Triangles as u8;
        b.topology = PrimitiveTopology::Lines as u8;
        assert_ne!(a, b);
        assert_ne!(hash_state(&a), hash_state(&b));
    }

    #[test]
    fn test_blend_attachment_from_blend_info() {
        let info = BlendInfo {
            enabled: true,
            separate_alpha: false,
            color_op: BlendEquation::Add,
            color_src: BlendFactor::SrcAlpha,
            color_dst: BlendFactor::OneMinusSrcAlpha,
            alpha_op: BlendEquation::Add,
            alpha_src: BlendFactor::One,
            alpha_dst: BlendFactor::Zero,
        };
        let att = BlendAttachment::from_blend_info(&info);
        assert!(att.enabled);
        assert_eq!(att.color_src, BlendFactor::SrcAlpha as u8);
        assert_eq!(att.color_dst, BlendFactor::OneMinusSrcAlpha as u8);
    }
}
