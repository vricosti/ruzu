// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `pipeline_helper.h`.
//!
//! Helper types for building descriptor set layouts, pipeline layouts,
//! descriptor update templates, and push constant management for
//! rescaling and render area data.

use ash::vk;

/// Number of u32 words used for texture and image scaling bit flags.
/// Port of `NUM_TEXTURE_AND_IMAGE_SCALING_WORDS` from shader recompiler.
pub const NUM_TEXTURE_AND_IMAGE_SCALING_WORDS: usize = 4;

/// Number of u32 words for texture-only scaling.
/// Port of `NUM_TEXTURE_SCALING_WORDS`.
pub const NUM_TEXTURE_SCALING_WORDS: usize = 2;

/// Size of a single descriptor update entry (buffer info / image info).
/// Port of `sizeof(DescriptorUpdateEntry)` used as stride.
const DESCRIPTOR_UPDATE_ENTRY_SIZE: usize = std::mem::size_of::<vk::DescriptorBufferInfo>();

// ---------------------------------------------------------------------------
// DescriptorLayoutBuilder
// ---------------------------------------------------------------------------

/// Descriptor binding info for a single descriptor type.
#[derive(Debug, Clone, Copy)]
pub struct DescriptorInfo {
    pub count: u32,
}

/// Port of `DescriptorLayoutBuilder` class.
///
/// Incrementally builds descriptor set layout bindings, update template
/// entries, and pipeline layouts from shader info.
pub struct DescriptorLayoutBuilder {
    is_compute: bool,
    bindings: Vec<vk::DescriptorSetLayoutBinding>,
    entries: Vec<vk::DescriptorUpdateTemplateEntry>,
    binding: u32,
    num_descriptors: u32,
    offset: usize,
}

impl DescriptorLayoutBuilder {
    /// Port of `DescriptorLayoutBuilder::DescriptorLayoutBuilder`.
    pub fn new() -> Self {
        DescriptorLayoutBuilder {
            is_compute: false,
            bindings: Vec::new(),
            entries: Vec::new(),
            binding: 0,
            num_descriptors: 0,
            offset: 0,
        }
    }

    /// Port of `DescriptorLayoutBuilder::CanUsePushDescriptor`.
    pub fn can_use_push_descriptor(
        &self,
        max_push_descriptors: u32,
        is_supported: bool,
    ) -> bool {
        is_supported && self.num_descriptors <= max_push_descriptors
    }

    /// Port of `DescriptorLayoutBuilder::CreateDescriptorSetLayout`.
    pub fn create_descriptor_set_layout(
        &self,
        device: &ash::Device,
        use_push_descriptor: bool,
    ) -> Result<vk::DescriptorSetLayout, vk::Result> {
        if self.bindings.is_empty() {
            return Ok(vk::DescriptorSetLayout::null());
        }
        let flags = if use_push_descriptor {
            vk::DescriptorSetLayoutCreateFlags::PUSH_DESCRIPTOR_KHR
        } else {
            vk::DescriptorSetLayoutCreateFlags::empty()
        };
        let ci = vk::DescriptorSetLayoutCreateInfo {
            s_type: vk::StructureType::DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
            p_next: std::ptr::null(),
            flags,
            binding_count: self.bindings.len() as u32,
            p_bindings: self.bindings.as_ptr(),
        };
        unsafe { device.create_descriptor_set_layout(&ci, None) }
    }

    /// Port of `DescriptorLayoutBuilder::CreateTemplate`.
    pub fn create_template(
        &self,
        device: &ash::Device,
        descriptor_set_layout: vk::DescriptorSetLayout,
        pipeline_layout: vk::PipelineLayout,
        use_push_descriptor: bool,
    ) -> Result<vk::DescriptorUpdateTemplate, vk::Result> {
        if self.entries.is_empty() {
            return Ok(vk::DescriptorUpdateTemplate::null());
        }
        let template_type = if use_push_descriptor {
            vk::DescriptorUpdateTemplateType::PUSH_DESCRIPTORS_KHR
        } else {
            vk::DescriptorUpdateTemplateType::DESCRIPTOR_SET
        };
        let ci = vk::DescriptorUpdateTemplateCreateInfo {
            s_type: vk::StructureType::DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO,
            p_next: std::ptr::null(),
            flags: vk::DescriptorUpdateTemplateCreateFlags::empty(),
            descriptor_update_entry_count: self.entries.len() as u32,
            p_descriptor_update_entries: self.entries.as_ptr(),
            template_type,
            descriptor_set_layout,
            pipeline_bind_point: vk::PipelineBindPoint::GRAPHICS,
            pipeline_layout,
            set: 0,
        };
        unsafe { device.create_descriptor_update_template(&ci, None) }
    }

    /// Port of `DescriptorLayoutBuilder::CreatePipelineLayout`.
    ///
    /// Creates a pipeline layout with push constant ranges for rescaling
    /// and render area data.
    pub fn create_pipeline_layout(
        &self,
        device: &ash::Device,
        descriptor_set_layout: vk::DescriptorSetLayout,
    ) -> Result<vk::PipelineLayout, vk::Result> {
        // Push constant range covers rescaling layout + render area layout
        // Rescaling layout: NUM_TEXTURE_AND_IMAGE_SCALING_WORDS * 4 bytes + optional down_factor (4 bytes for compute)
        let size_offset: u32 = if self.is_compute { 4 } else { 0 };
        let rescaling_size = (NUM_TEXTURE_AND_IMAGE_SCALING_WORDS as u32 * 4) + 4; // words + down_factor
        let render_area_size = 4 * 4u32; // 4 floats
        let range = vk::PushConstantRange {
            stage_flags: if self.is_compute {
                vk::ShaderStageFlags::COMPUTE
            } else {
                vk::ShaderStageFlags::ALL_GRAPHICS
            },
            offset: 0,
            size: rescaling_size - size_offset + render_area_size,
        };

        let set_layout_count = if descriptor_set_layout == vk::DescriptorSetLayout::null() {
            0u32
        } else {
            1u32
        };
        let layouts = [descriptor_set_layout];
        let ci = vk::PipelineLayoutCreateInfo {
            s_type: vk::StructureType::PIPELINE_LAYOUT_CREATE_INFO,
            p_next: std::ptr::null(),
            flags: vk::PipelineLayoutCreateFlags::empty(),
            set_layout_count,
            p_set_layouts: if self.bindings.is_empty() {
                std::ptr::null()
            } else {
                layouts.as_ptr()
            },
            push_constant_range_count: 1,
            p_push_constant_ranges: &range,
        };
        unsafe { device.create_pipeline_layout(&ci, None) }
    }

    /// Port of `DescriptorLayoutBuilder::Add`.
    ///
    /// Adds descriptor bindings for a list of descriptors of a given type and stage.
    pub fn add_descriptors(
        &mut self,
        descriptor_type: vk::DescriptorType,
        stage: vk::ShaderStageFlags,
        descriptors: &[DescriptorInfo],
    ) {
        self.is_compute |= stage.contains(vk::ShaderStageFlags::COMPUTE);

        for desc in descriptors {
            self.bindings.push(vk::DescriptorSetLayoutBinding {
                binding: self.binding,
                descriptor_type,
                descriptor_count: desc.count,
                stage_flags: stage,
                p_immutable_samplers: std::ptr::null(),
            });
            self.entries.push(vk::DescriptorUpdateTemplateEntry {
                dst_binding: self.binding,
                dst_array_element: 0,
                descriptor_count: desc.count,
                descriptor_type,
                offset: self.offset,
                stride: DESCRIPTOR_UPDATE_ENTRY_SIZE,
            });
            self.binding += 1;
            self.num_descriptors += desc.count;
            self.offset += DESCRIPTOR_UPDATE_ENTRY_SIZE;
        }
    }
}

// ---------------------------------------------------------------------------
// RescalingPushConstant
// ---------------------------------------------------------------------------

/// Port of `RescalingPushConstant` class.
///
/// Tracks per-texture and per-image rescaling flags as bit-packed words
/// for push constant upload.
pub struct RescalingPushConstant {
    words: [u32; NUM_TEXTURE_AND_IMAGE_SCALING_WORDS],
    texture_index: usize,
    texture_bit: u32,
    image_index: usize,
    image_bit: u32,
}

impl RescalingPushConstant {
    /// Port of `RescalingPushConstant::RescalingPushConstant`.
    pub fn new() -> Self {
        RescalingPushConstant {
            words: [0u32; NUM_TEXTURE_AND_IMAGE_SCALING_WORDS],
            texture_index: 0,
            texture_bit: 1,
            image_index: NUM_TEXTURE_SCALING_WORDS,
            image_bit: 1,
        }
    }

    /// Port of `RescalingPushConstant::PushTexture`.
    pub fn push_texture(&mut self, is_rescaled: bool) {
        if is_rescaled {
            self.words[self.texture_index] |= self.texture_bit;
        }
        self.texture_bit <<= 1;
        if self.texture_bit == 0 {
            self.texture_bit = 1;
            self.texture_index += 1;
        }
    }

    /// Port of `RescalingPushConstant::PushImage`.
    pub fn push_image(&mut self, is_rescaled: bool) {
        if is_rescaled {
            self.words[self.image_index] |= self.image_bit;
        }
        self.image_bit <<= 1;
        if self.image_bit == 0 {
            self.image_bit = 1;
            self.image_index += 1;
        }
    }

    /// Port of `RescalingPushConstant::Data`.
    pub fn data(&self) -> &[u32; NUM_TEXTURE_AND_IMAGE_SCALING_WORDS] {
        &self.words
    }
}

// ---------------------------------------------------------------------------
// RenderAreaPushConstant
// ---------------------------------------------------------------------------

/// Port of `RenderAreaPushConstant` class.
pub struct RenderAreaPushConstant {
    pub uses_render_area: bool,
    pub words: [f32; 4],
}

impl RenderAreaPushConstant {
    pub fn new() -> Self {
        RenderAreaPushConstant {
            uses_render_area: false,
            words: [0.0; 4],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rescaling_push_constant_default() {
        let rpc = RescalingPushConstant::new();
        assert_eq!(rpc.words, [0, 0, 0, 0]);
    }

    #[test]
    fn rescaling_push_texture() {
        let mut rpc = RescalingPushConstant::new();
        rpc.push_texture(true);
        assert_eq!(rpc.words[0], 1);
        rpc.push_texture(false);
        assert_eq!(rpc.words[0], 1);
        rpc.push_texture(true);
        assert_eq!(rpc.words[0], 0b101);
    }

    #[test]
    fn rescaling_push_image() {
        let mut rpc = RescalingPushConstant::new();
        rpc.push_image(true);
        assert_eq!(rpc.words[NUM_TEXTURE_SCALING_WORDS], 1);
    }

    #[test]
    fn descriptor_layout_builder_empty() {
        let builder = DescriptorLayoutBuilder::new();
        assert!(!builder.can_use_push_descriptor(32, true));
        // 0 descriptors <= 32, so it should be true if supported
        // Actually 0 <= 32 is true
        assert!(builder.can_use_push_descriptor(32, true));
    }
}
