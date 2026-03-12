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

// ---------------------------------------------------------------------------
// DescriptorLayoutBuilder
// ---------------------------------------------------------------------------

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
    pub fn can_use_push_descriptor(&self, _max_push_descriptors: u32, _supported: bool) -> bool {
        _supported && self.num_descriptors <= _max_push_descriptors
    }

    /// Port of `DescriptorLayoutBuilder::CreateDescriptorSetLayout`.
    pub fn create_descriptor_set_layout(
        &self,
        _device: &ash::Device,
        _use_push_descriptor: bool,
    ) -> vk::DescriptorSetLayout {
        todo!("DescriptorLayoutBuilder::create_descriptor_set_layout")
    }

    /// Port of `DescriptorLayoutBuilder::CreateTemplate`.
    pub fn create_template(
        &self,
        _device: &ash::Device,
        _descriptor_set_layout: vk::DescriptorSetLayout,
        _pipeline_layout: vk::PipelineLayout,
        _use_push_descriptor: bool,
    ) -> vk::DescriptorUpdateTemplate {
        todo!("DescriptorLayoutBuilder::create_template")
    }

    /// Port of `DescriptorLayoutBuilder::CreatePipelineLayout`.
    pub fn create_pipeline_layout(
        &self,
        _device: &ash::Device,
        _descriptor_set_layout: vk::DescriptorSetLayout,
    ) -> vk::PipelineLayout {
        todo!("DescriptorLayoutBuilder::create_pipeline_layout")
    }

    /// Port of `DescriptorLayoutBuilder::Add`.
    ///
    /// Adds descriptor bindings for a shader stage's info.
    pub fn add(&mut self, _stage: vk::ShaderStageFlags) {
        todo!("DescriptorLayoutBuilder::add")
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

// ---------------------------------------------------------------------------
// PushImageDescriptors (free function)
// ---------------------------------------------------------------------------

/// Port of `PushImageDescriptors` inline function.
///
/// Iterates texture and image descriptors from shader info, pushing
/// sampled image and storage image entries into the guest descriptor queue.
pub fn push_image_descriptors() {
    todo!("push_image_descriptors")
}
