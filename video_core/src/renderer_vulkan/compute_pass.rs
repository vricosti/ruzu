// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_compute_pass.h` / `vk_compute_pass.cpp`.
//!
//! Reusable compute passes for index buffer assembly, conditional rendering,
//! prefix scans, ASTC decoding, and MSAA copy.

use ash::vk;

// ---------------------------------------------------------------------------
// ComputePass (base)
// ---------------------------------------------------------------------------

/// Port of `ComputePass` base class.
///
/// Owns the shader module, pipeline, pipeline layout, and descriptor
/// allocation for a single reusable compute pass.
pub struct ComputePass {
    pub descriptor_template: vk::DescriptorUpdateTemplate,
    pub layout: vk::PipelineLayout,
    pub pipeline: vk::Pipeline,
    pub descriptor_set_layout: vk::DescriptorSetLayout,
    _module: vk::ShaderModule,
}

impl ComputePass {
    /// Port of `ComputePass::ComputePass`.
    pub fn new(
        _code: &[u32],
        _optional_subgroup_size: Option<u32>,
    ) -> Self {
        todo!("ComputePass::new")
    }
}

// ---------------------------------------------------------------------------
// Uint8Pass
// ---------------------------------------------------------------------------

/// Port of `Uint8Pass` class.
///
/// Assembles uint8 indices into a uint16 index buffer.
pub struct Uint8Pass {
    _base: ComputePass,
}

impl Uint8Pass {
    /// Port of `Uint8Pass::Uint8Pass`.
    pub fn new() -> Self {
        todo!("Uint8Pass::new")
    }

    /// Port of `Uint8Pass::Assemble`.
    ///
    /// Returns `(buffer, offset)` pair for the assembled index buffer.
    pub fn assemble(
        &mut self,
        _num_vertices: u32,
        _src_buffer: vk::Buffer,
        _src_offset: u32,
    ) -> (vk::Buffer, vk::DeviceSize) {
        todo!("Uint8Pass::assemble")
    }
}

// ---------------------------------------------------------------------------
// QuadIndexedPass
// ---------------------------------------------------------------------------

/// Port of `QuadIndexedPass` class.
///
/// Assembles quad-indexed geometry into triangle indices.
pub struct QuadIndexedPass {
    _base: ComputePass,
}

impl QuadIndexedPass {
    /// Port of `QuadIndexedPass::QuadIndexedPass`.
    pub fn new() -> Self {
        todo!("QuadIndexedPass::new")
    }

    /// Port of `QuadIndexedPass::Assemble`.
    pub fn assemble(
        &mut self,
        _index_format: u32,
        _num_vertices: u32,
        _base_vertex: u32,
        _src_buffer: vk::Buffer,
        _src_offset: u32,
        _is_strip: bool,
    ) -> (vk::Buffer, vk::DeviceSize) {
        todo!("QuadIndexedPass::assemble")
    }
}

// ---------------------------------------------------------------------------
// ConditionalRenderingResolvePass
// ---------------------------------------------------------------------------

/// Port of `ConditionalRenderingResolvePass` class.
pub struct ConditionalRenderingResolvePass {
    _base: ComputePass,
}

impl ConditionalRenderingResolvePass {
    /// Port of `ConditionalRenderingResolvePass::ConditionalRenderingResolvePass`.
    pub fn new() -> Self {
        todo!("ConditionalRenderingResolvePass::new")
    }

    /// Port of `ConditionalRenderingResolvePass::Resolve`.
    pub fn resolve(
        &mut self,
        _dst_buffer: vk::Buffer,
        _src_buffer: vk::Buffer,
        _src_offset: u32,
        _compare_to_zero: bool,
    ) {
        todo!("ConditionalRenderingResolvePass::resolve")
    }
}

// ---------------------------------------------------------------------------
// QueriesPrefixScanPass
// ---------------------------------------------------------------------------

/// Port of `QueriesPrefixScanPass` class.
pub struct QueriesPrefixScanPass {
    _base: ComputePass,
}

impl QueriesPrefixScanPass {
    /// Port of `QueriesPrefixScanPass::QueriesPrefixScanPass`.
    pub fn new() -> Self {
        todo!("QueriesPrefixScanPass::new")
    }

    /// Port of `QueriesPrefixScanPass::Run`.
    pub fn run(
        &mut self,
        _accumulation_buffer: vk::Buffer,
        _dst_buffer: vk::Buffer,
        _src_buffer: vk::Buffer,
        _number_of_sums: usize,
        _min_accumulation_limit: usize,
        _max_accumulation_limit: usize,
    ) {
        todo!("QueriesPrefixScanPass::run")
    }
}

// ---------------------------------------------------------------------------
// ASTCDecoderPass
// ---------------------------------------------------------------------------

/// Port of `ASTCDecoderPass` class.
///
/// GPU-accelerated ASTC texture decoding.
pub struct AstcDecoderPass {
    _base: ComputePass,
}

impl AstcDecoderPass {
    /// Port of `ASTCDecoderPass::ASTCDecoderPass`.
    pub fn new() -> Self {
        todo!("AstcDecoderPass::new")
    }

    /// Port of `ASTCDecoderPass::Assemble`.
    pub fn assemble(&mut self) {
        todo!("AstcDecoderPass::assemble")
    }
}

// ---------------------------------------------------------------------------
// MSAACopyPass
// ---------------------------------------------------------------------------

/// Port of `MSAACopyPass` class.
///
/// Copies between MSAA and non-MSAA images via compute.
pub struct MsaaCopyPass {
    _base: ComputePass,
    _modules: [vk::ShaderModule; 2],
    _pipelines: [vk::Pipeline; 2],
}

impl MsaaCopyPass {
    /// Port of `MSAACopyPass::MSAACopyPass`.
    pub fn new() -> Self {
        todo!("MsaaCopyPass::new")
    }

    /// Port of `MSAACopyPass::CopyImage`.
    pub fn copy_image(&mut self, _msaa_to_non_msaa: bool) {
        todo!("MsaaCopyPass::copy_image")
    }
}
