// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_compute_pass.h` / `vk_compute_pass.cpp`.
//!
//! Reusable compute passes for index buffer assembly, conditional rendering,
//! prefix scans, ASTC decoding, and MSAA copy.

use ash::vk;
use std::ptr::NonNull;

use super::descriptor_pool::{DescriptorBankInfo as PoolDescriptorBankInfo, DescriptorPool};
use super::scheduler::Scheduler;
use super::update_descriptor::ComputePassDescriptorQueue;
use crate::host_shaders::spirv_shaders::ASTC_DECODER_COMP_SPV;
use crate::host_shaders::spirv_shaders::{
    CONVERT_MSAA_TO_NON_MSAA_COMP_SPV, CONVERT_NON_MSAA_TO_MSAA_COMP_SPV,
};
use crate::texture_cache::accelerated_swizzle::make_block_linear_swizzle_2d_params;
use crate::texture_cache::image_info::ImageInfo;
use crate::texture_cache::types::SwizzleParameters;

// ---------------------------------------------------------------------------
// Constants (from vk_compute_pass.cpp anonymous namespace)
// ---------------------------------------------------------------------------

const ASTC_BINDING_INPUT_BUFFER: u32 = 0;
const ASTC_BINDING_OUTPUT_IMAGE: u32 = 1;
const ASTC_NUM_BINDINGS: usize = 2;

/// Port of `DISPATCH_SIZE` used in Uint8Pass and QuadIndexedPass.
const DISPATCH_SIZE: u32 = 1024;

/// Port of `DISPATCH_SIZE` used in QueriesPrefixScanPass.
const QUERIES_DISPATCH_SIZE: usize = 2048;

/// Port of `AstcPushConstants` from anonymous namespace.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct AstcPushConstants {
    pub blocks_dims: [u32; 2],
    pub layer_stride: u32,
    pub block_size: u32,
    pub x_shift: u32,
    pub block_height: u32,
    pub block_height_mask: u32,
}

/// Port of `QueriesPrefixScanPushConstants` from anonymous namespace.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct QueriesPrefixScanPushConstants {
    pub min_accumulation_base: u32,
    pub max_accumulation_base: u32,
    pub accumulation_limit: u32,
    pub buffer_offset: u32,
}

/// Memory barrier for shader write -> vertex attribute read.
fn write_barrier_vertex() -> vk::MemoryBarrier {
    vk::MemoryBarrier {
        s_type: vk::StructureType::MEMORY_BARRIER,
        p_next: std::ptr::null(),
        src_access_mask: vk::AccessFlags::SHADER_WRITE,
        dst_access_mask: vk::AccessFlags::VERTEX_ATTRIBUTE_READ,
    }
}

/// Memory barrier for shader write -> index read.
fn write_barrier_index() -> vk::MemoryBarrier {
    vk::MemoryBarrier {
        s_type: vk::StructureType::MEMORY_BARRIER,
        p_next: std::ptr::null(),
        src_access_mask: vk::AccessFlags::SHADER_WRITE,
        dst_access_mask: vk::AccessFlags::INDEX_READ,
    }
}

// ---------------------------------------------------------------------------
// DescriptorBankInfo
// ---------------------------------------------------------------------------

/// Port of `DescriptorBankInfo` struct used for descriptor allocation scoring.
#[derive(Debug, Clone, Copy, Default)]
pub struct DescriptorBankInfo {
    pub uniform_buffers: u32,
    pub storage_buffers: u32,
    pub texture_buffers: u32,
    pub image_buffers: u32,
    pub textures: u32,
    pub images: u32,
    pub score: u32,
}

/// Bank info for input/output storage buffer passes (Uint8, QuadIndexed).
const INPUT_OUTPUT_BANK_INFO: DescriptorBankInfo = DescriptorBankInfo {
    uniform_buffers: 0,
    storage_buffers: 2,
    texture_buffers: 0,
    image_buffers: 0,
    textures: 0,
    images: 0,
    score: 2,
};

/// Bank info for queries scan pass (3 storage buffers).
const QUERIES_SCAN_BANK_INFO: DescriptorBankInfo = DescriptorBankInfo {
    uniform_buffers: 0,
    storage_buffers: 3,
    texture_buffers: 0,
    image_buffers: 0,
    textures: 0,
    images: 0,
    score: 3,
};

/// Bank info for ASTC pass (1 storage buffer + 1 storage image).
const ASTC_BANK_INFO: DescriptorBankInfo = DescriptorBankInfo {
    uniform_buffers: 0,
    storage_buffers: 1,
    texture_buffers: 0,
    image_buffers: 0,
    textures: 0,
    images: 1,
    score: 2,
};

/// Bank info for MSAA pass (2 storage images).
const MSAA_BANK_INFO: DescriptorBankInfo = DescriptorBankInfo {
    uniform_buffers: 0,
    storage_buffers: 0,
    texture_buffers: 0,
    image_buffers: 0,
    textures: 0,
    images: 2,
    score: 2,
};

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
    module: vk::ShaderModule,
}

impl ComputePass {
    /// Port of `ComputePass::ComputePass`.
    ///
    /// Creates the descriptor set layout, pipeline layout, descriptor update
    /// template, shader module, and compute pipeline from the given bindings
    /// and SPIR-V code.
    pub fn new(
        device: &ash::Device,
        bindings: &[vk::DescriptorSetLayoutBinding],
        templates: &[vk::DescriptorUpdateTemplateEntry],
        push_constants: &[vk::PushConstantRange],
        code: &[u32],
        _optional_subgroup_size: Option<u32>,
    ) -> Result<Self, vk::Result> {
        // Create descriptor set layout
        let layout_ci = vk::DescriptorSetLayoutCreateInfo::builder()
            .bindings(bindings)
            .build();
        let descriptor_set_layout =
            unsafe { device.create_descriptor_set_layout(&layout_ci, None)? };

        // Create pipeline layout
        let set_layouts = [descriptor_set_layout];
        let pipeline_layout_ci = vk::PipelineLayoutCreateInfo::builder()
            .set_layouts(&set_layouts)
            .push_constant_ranges(push_constants)
            .build();
        let layout = unsafe { device.create_pipeline_layout(&pipeline_layout_ci, None)? };

        // Create descriptor update template
        let descriptor_template = if !templates.is_empty() {
            let template_ci = vk::DescriptorUpdateTemplateCreateInfo {
                s_type: vk::StructureType::DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO,
                p_next: std::ptr::null(),
                flags: vk::DescriptorUpdateTemplateCreateFlags::empty(),
                descriptor_update_entry_count: templates.len() as u32,
                p_descriptor_update_entries: templates.as_ptr(),
                template_type: vk::DescriptorUpdateTemplateType::DESCRIPTOR_SET,
                descriptor_set_layout,
                pipeline_bind_point: vk::PipelineBindPoint::COMPUTE,
                pipeline_layout: layout,
                set: 0,
            };
            unsafe { device.create_descriptor_update_template(&template_ci, None)? }
        } else {
            vk::DescriptorUpdateTemplate::null()
        };

        // Create shader module and pipeline
        let (module, pipeline) = if !code.is_empty() {
            let module_ci = vk::ShaderModuleCreateInfo::builder().code(code).build();
            let module = unsafe { device.create_shader_module(&module_ci, None)? };

            let main_name = std::ffi::CString::new("main").unwrap();
            let stage_ci = vk::PipelineShaderStageCreateInfo::builder()
                .stage(vk::ShaderStageFlags::COMPUTE)
                .module(module)
                .name(&main_name)
                .build();

            let pipeline_ci = vk::ComputePipelineCreateInfo::builder()
                .stage(stage_ci)
                .layout(layout)
                .build();

            let pipelines = unsafe {
                device
                    .create_compute_pipelines(vk::PipelineCache::null(), &[pipeline_ci], None)
                    .map_err(|e| e.1)?
            };

            (module, pipelines[0])
        } else {
            (vk::ShaderModule::null(), vk::Pipeline::null())
        };

        Ok(ComputePass {
            descriptor_template,
            layout,
            pipeline,
            descriptor_set_layout,
            module,
        })
    }
}

// ---------------------------------------------------------------------------
// Uint8Pass
// ---------------------------------------------------------------------------

/// Port of `Uint8Pass` class.
///
/// Assembles uint8 indices into a uint16 index buffer using a compute shader.
pub struct Uint8Pass {
    base: ComputePass,
}

impl Uint8Pass {
    /// Port of `Uint8Pass::Uint8Pass`.
    pub fn new_with_pass(base: ComputePass) -> Self {
        Uint8Pass { base }
    }

    /// Port of `Uint8Pass::Assemble`.
    ///
    /// Dispatches the uint8-to-uint16 conversion compute shader.
    /// Returns `(buffer, offset)` pair for the assembled index buffer.
    pub fn assemble(&self, device: &ash::Device, cmdbuf: vk::CommandBuffer, num_vertices: u32) {
        let num_workgroups = (num_vertices + DISPATCH_SIZE - 1) / DISPATCH_SIZE;

        unsafe {
            device.cmd_bind_pipeline(cmdbuf, vk::PipelineBindPoint::COMPUTE, self.base.pipeline);
            device.cmd_dispatch(cmdbuf, num_workgroups, 1, 1);

            let barrier = write_barrier_vertex();
            device.cmd_pipeline_barrier(
                cmdbuf,
                vk::PipelineStageFlags::COMPUTE_SHADER,
                vk::PipelineStageFlags::VERTEX_INPUT,
                vk::DependencyFlags::empty(),
                &[barrier],
                &[],
                &[],
            );
        }
    }
}

// ---------------------------------------------------------------------------
// QuadIndexedPass
// ---------------------------------------------------------------------------

/// Port of `QuadIndexedPass` class.
///
/// Assembles quad-indexed geometry into triangle indices.
pub struct QuadIndexedPass {
    base: ComputePass,
}

impl QuadIndexedPass {
    /// Port of `QuadIndexedPass::QuadIndexedPass`.
    pub fn new_with_pass(base: ComputePass) -> Self {
        QuadIndexedPass { base }
    }

    /// Port of `QuadIndexedPass::Assemble`.
    ///
    /// Converts quad indices to triangle indices via compute dispatch.
    pub fn assemble(
        &mut self,
        device: &ash::Device,
        cmdbuf: vk::CommandBuffer,
        num_tri_vertices: u32,
        base_vertex: u32,
        index_shift: u32,
        is_strip: bool,
    ) {
        let push_constants: [u32; 3] = [base_vertex, index_shift, if is_strip { 1 } else { 0 }];

        let num_workgroups = (num_tri_vertices + DISPATCH_SIZE - 1) / DISPATCH_SIZE;

        unsafe {
            device.cmd_bind_pipeline(cmdbuf, vk::PipelineBindPoint::COMPUTE, self.base.pipeline);
            device.cmd_push_constants(
                cmdbuf,
                self.base.layout,
                vk::ShaderStageFlags::COMPUTE,
                0,
                bytemuck::bytes_of(&push_constants),
            );
            device.cmd_dispatch(cmdbuf, num_workgroups, 1, 1);

            let barrier = write_barrier_index();
            device.cmd_pipeline_barrier(
                cmdbuf,
                vk::PipelineStageFlags::COMPUTE_SHADER,
                vk::PipelineStageFlags::VERTEX_INPUT,
                vk::DependencyFlags::empty(),
                &[barrier],
                &[],
                &[],
            );
        }
    }

    /// Port of index_shift calculation from QuadIndexedPass::Assemble.
    pub fn index_shift_for_format(index_format: u32) -> u32 {
        match index_format {
            0 => 0, // UnsignedByte
            1 => 1, // UnsignedShort
            2 => 2, // UnsignedInt
            _ => 2,
        }
    }
}

// ---------------------------------------------------------------------------
// ConditionalRenderingResolvePass
// ---------------------------------------------------------------------------

/// Port of `ConditionalRenderingResolvePass` class.
///
/// Resolves conditional rendering predicates via compute.
pub struct ConditionalRenderingResolvePass {
    base: ComputePass,
}

impl ConditionalRenderingResolvePass {
    /// Port of `ConditionalRenderingResolvePass::ConditionalRenderingResolvePass`.
    pub fn new_with_pass(base: ComputePass) -> Self {
        ConditionalRenderingResolvePass { base }
    }

    /// Port of `ConditionalRenderingResolvePass::Resolve`.
    ///
    /// Dispatches the conditional rendering resolve compute shader.
    pub fn resolve(&self, device: &ash::Device, cmdbuf: vk::CommandBuffer) {
        let read_barrier = vk::MemoryBarrier {
            s_type: vk::StructureType::MEMORY_BARRIER,
            p_next: std::ptr::null(),
            src_access_mask: vk::AccessFlags::TRANSFER_WRITE | vk::AccessFlags::SHADER_WRITE,
            dst_access_mask: vk::AccessFlags::SHADER_READ | vk::AccessFlags::SHADER_WRITE,
        };
        let write_barrier = vk::MemoryBarrier {
            s_type: vk::StructureType::MEMORY_BARRIER,
            p_next: std::ptr::null(),
            src_access_mask: vk::AccessFlags::SHADER_WRITE,
            dst_access_mask: vk::AccessFlags::CONDITIONAL_RENDERING_READ_EXT,
        };

        unsafe {
            device.cmd_pipeline_barrier(
                cmdbuf,
                vk::PipelineStageFlags::ALL_COMMANDS,
                vk::PipelineStageFlags::COMPUTE_SHADER,
                vk::DependencyFlags::empty(),
                &[read_barrier],
                &[],
                &[],
            );
            device.cmd_bind_pipeline(cmdbuf, vk::PipelineBindPoint::COMPUTE, self.base.pipeline);
            device.cmd_dispatch(cmdbuf, 1, 1, 1);
            device.cmd_pipeline_barrier(
                cmdbuf,
                vk::PipelineStageFlags::COMPUTE_SHADER,
                vk::PipelineStageFlags::CONDITIONAL_RENDERING_EXT,
                vk::DependencyFlags::empty(),
                &[write_barrier],
                &[],
                &[],
            );
        }
    }
}

// ---------------------------------------------------------------------------
// QueriesPrefixScanPass
// ---------------------------------------------------------------------------

/// Port of `QueriesPrefixScanPass` class.
///
/// Performs prefix sum scan over query results via compute.
pub struct QueriesPrefixScanPass {
    base: ComputePass,
}

impl QueriesPrefixScanPass {
    /// Port of `QueriesPrefixScanPass::QueriesPrefixScanPass`.
    pub fn new_with_pass(base: ComputePass) -> Self {
        QueriesPrefixScanPass { base }
    }

    /// Port of `QueriesPrefixScanPass::Run`.
    ///
    /// Runs the prefix scan in batches of up to DISPATCH_SIZE (2048) elements.
    pub fn run(
        &self,
        device: &ash::Device,
        cmdbuf: vk::CommandBuffer,
        number_of_sums: usize,
        min_accumulation_limit: usize,
        max_accumulation_limit: usize,
    ) {
        let mut current_runs = number_of_sums;
        let mut offset: usize = 0;

        let read_barrier = vk::MemoryBarrier {
            s_type: vk::StructureType::MEMORY_BARRIER,
            p_next: std::ptr::null(),
            src_access_mask: vk::AccessFlags::TRANSFER_WRITE,
            dst_access_mask: vk::AccessFlags::SHADER_READ | vk::AccessFlags::SHADER_WRITE,
        };
        let write_barrier = vk::MemoryBarrier {
            s_type: vk::StructureType::MEMORY_BARRIER,
            p_next: std::ptr::null(),
            src_access_mask: vk::AccessFlags::SHADER_WRITE,
            dst_access_mask: vk::AccessFlags::SHADER_READ
                | vk::AccessFlags::TRANSFER_READ
                | vk::AccessFlags::VERTEX_ATTRIBUTE_READ
                | vk::AccessFlags::INDIRECT_COMMAND_READ
                | vk::AccessFlags::INDEX_READ
                | vk::AccessFlags::UNIFORM_READ
                | vk::AccessFlags::CONDITIONAL_RENDERING_READ_EXT,
        };

        while current_runs != 0 {
            let runs_to_do = current_runs.min(QUERIES_DISPATCH_SIZE);
            current_runs -= runs_to_do;
            let used_offset = offset;
            offset += runs_to_do;

            let uniforms = QueriesPrefixScanPushConstants {
                min_accumulation_base: min_accumulation_limit as u32,
                max_accumulation_base: max_accumulation_limit as u32,
                accumulation_limit: (runs_to_do - 1) as u32,
                buffer_offset: used_offset as u32,
            };

            unsafe {
                device.cmd_pipeline_barrier(
                    cmdbuf,
                    vk::PipelineStageFlags::ALL_COMMANDS,
                    vk::PipelineStageFlags::COMPUTE_SHADER,
                    vk::DependencyFlags::empty(),
                    &[read_barrier],
                    &[],
                    &[],
                );
                device.cmd_bind_pipeline(
                    cmdbuf,
                    vk::PipelineBindPoint::COMPUTE,
                    self.base.pipeline,
                );
                device.cmd_push_constants(
                    cmdbuf,
                    self.base.layout,
                    vk::ShaderStageFlags::COMPUTE,
                    0,
                    bytemuck::bytes_of(&uniforms),
                );
                device.cmd_dispatch(cmdbuf, 1, 1, 1);
                device.cmd_pipeline_barrier(
                    cmdbuf,
                    vk::PipelineStageFlags::COMPUTE_SHADER,
                    vk::PipelineStageFlags::CONDITIONAL_RENDERING_EXT,
                    vk::DependencyFlags::empty(),
                    &[write_barrier],
                    &[],
                    &[],
                );
            }
        }
    }
}

// ---------------------------------------------------------------------------
// ASTCDecoderPass
// ---------------------------------------------------------------------------

/// Port of `ASTCDecoderPass` class.
///
/// GPU-accelerated ASTC texture decoding via compute shader.
pub struct AstcDecoderPass {
    base: ComputePass,
    descriptor_pool: NonNull<DescriptorPool>,
    compute_pass_descriptor_queue: NonNull<ComputePassDescriptorQueue>,
}

impl AstcDecoderPass {
    /// Port of `ASTCDecoderPass::ASTCDecoderPass`.
    pub fn new(
        device: &ash::Device,
        descriptor_pool: &mut DescriptorPool,
        compute_pass_descriptor_queue: &mut ComputePassDescriptorQueue,
    ) -> Result<Self, vk::Result> {
        let bindings = [
            vk::DescriptorSetLayoutBinding {
                binding: ASTC_BINDING_INPUT_BUFFER,
                descriptor_type: vk::DescriptorType::STORAGE_BUFFER,
                descriptor_count: 1,
                stage_flags: vk::ShaderStageFlags::COMPUTE,
                p_immutable_samplers: std::ptr::null(),
            },
            vk::DescriptorSetLayoutBinding {
                binding: ASTC_BINDING_OUTPUT_IMAGE,
                descriptor_type: vk::DescriptorType::STORAGE_IMAGE,
                descriptor_count: 1,
                stage_flags: vk::ShaderStageFlags::COMPUTE,
                p_immutable_samplers: std::ptr::null(),
            },
        ];
        let templates = [
            vk::DescriptorUpdateTemplateEntry {
                dst_binding: ASTC_BINDING_INPUT_BUFFER,
                dst_array_element: 0,
                descriptor_count: 1,
                descriptor_type: vk::DescriptorType::STORAGE_BUFFER,
                offset: 0,
                stride: std::mem::size_of::<
                    crate::renderer_vulkan::update_descriptor::DescriptorUpdateEntry,
                >(),
            },
            vk::DescriptorUpdateTemplateEntry {
                dst_binding: ASTC_BINDING_OUTPUT_IMAGE,
                dst_array_element: 0,
                descriptor_count: 1,
                descriptor_type: vk::DescriptorType::STORAGE_IMAGE,
                offset: std::mem::size_of::<
                    crate::renderer_vulkan::update_descriptor::DescriptorUpdateEntry,
                >(),
                stride: std::mem::size_of::<
                    crate::renderer_vulkan::update_descriptor::DescriptorUpdateEntry,
                >(),
            },
        ];
        let push_constants = [vk::PushConstantRange {
            stage_flags: vk::ShaderStageFlags::COMPUTE,
            offset: 0,
            size: std::mem::size_of::<AstcPushConstants>() as u32,
        }];
        let base = ComputePass::new(
            device,
            &bindings,
            &templates,
            &push_constants,
            ASTC_DECODER_COMP_SPV,
            None,
        )?;
        Ok(AstcDecoderPass {
            base,
            descriptor_pool: NonNull::from(descriptor_pool),
            compute_pass_descriptor_queue: NonNull::from(compute_pass_descriptor_queue),
        })
    }

    /// Port of `ASTCDecoderPass::Assemble`.
    pub fn assemble(
        &mut self,
        device: &ash::Device,
        scheduler: &mut Scheduler,
        image: vk::Image,
        aspect_mask: vk::ImageAspectFlags,
        is_initialized: bool,
        info: &ImageInfo,
        guest_size_bytes: usize,
        staging_buffer: vk::Buffer,
        staging_offset: vk::DeviceSize,
        swizzles: &[SwizzleParameters],
        storage_views: &[vk::ImageView],
    ) -> bool {
        let device_handle = device.clone();
        let block_dims = [
            crate::surface::default_block_width(info.format),
            crate::surface::default_block_height(info.format),
        ];
        let pipeline = self.base.pipeline;
        let layout = self.base.layout;
        scheduler.request_outside_renderpass();
        let device = device_handle.clone();
        scheduler.record(move |cmdbuf| unsafe {
            let image_barrier = vk::ImageMemoryBarrier::builder()
                .src_access_mask(if is_initialized {
                    vk::AccessFlags::SHADER_WRITE
                } else {
                    vk::AccessFlags::empty()
                })
                .dst_access_mask(vk::AccessFlags::SHADER_READ | vk::AccessFlags::SHADER_WRITE)
                .old_layout(if is_initialized {
                    vk::ImageLayout::GENERAL
                } else {
                    vk::ImageLayout::UNDEFINED
                })
                .new_layout(vk::ImageLayout::GENERAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(image)
                .subresource_range(vk::ImageSubresourceRange {
                    aspect_mask,
                    base_mip_level: 0,
                    level_count: vk::REMAINING_MIP_LEVELS,
                    base_array_layer: 0,
                    layer_count: vk::REMAINING_ARRAY_LAYERS,
                })
                .build();
            device.cmd_pipeline_barrier(
                cmdbuf,
                if is_initialized {
                    vk::PipelineStageFlags::ALL_COMMANDS
                } else {
                    vk::PipelineStageFlags::TOP_OF_PIPE
                },
                vk::PipelineStageFlags::COMPUTE_SHADER,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &[image_barrier],
            );
            device.cmd_bind_pipeline(cmdbuf, vk::PipelineBindPoint::COMPUTE, pipeline);
        });

        for swizzle in swizzles {
            let Some(&storage_view) = storage_views.get(swizzle.level as usize) else {
                log::warn!(
                    "ASTCDecoderPass::assemble: missing storage view for level {}",
                    swizzle.level
                );
                return false;
            };
            if storage_view == vk::ImageView::null() {
                return false;
            }
            let input_offset = staging_offset + swizzle.buffer_offset as vk::DeviceSize;
            let range_size =
                guest_size_bytes.saturating_sub(swizzle.buffer_offset) as vk::DeviceSize;
            let num_dispatches_x = swizzle.num_tiles.width.div_ceil(8);
            let num_dispatches_y = swizzle.num_tiles.height.div_ceil(8);
            let num_dispatches_z = info.resources.layers as u32;

            let params = make_block_linear_swizzle_2d_params(swizzle, info);
            if params.origin != [0, 0, 0]
                || params.destination != [0, 0, 0]
                || params.bytes_per_block_log2 != 4
            {
                log::warn!(
                    "ASTCDecoderPass::assemble: unsupported swizzle params origin={:?} destination={:?} bpp_log2={}",
                    params.origin,
                    params.destination,
                    params.bytes_per_block_log2
                );
                return false;
            }

            let descriptor_set = {
                let descriptor_pool = unsafe { self.descriptor_pool.as_ref() };
                match descriptor_pool.allocate(
                    self.base.descriptor_set_layout,
                    &PoolDescriptorBankInfo {
                        uniform_buffers: 0,
                        storage_buffers: 1,
                        texture_buffers: 0,
                        image_buffers: 0,
                        textures: 0,
                        images: 1,
                        score: 2,
                    },
                ) {
                    Ok(set) => set,
                    Err(err) => {
                        log::warn!(
                            "ASTCDecoderPass::assemble: failed to allocate descriptor set: {err:?}"
                        );
                        return false;
                    }
                }
            };
            let descriptor_buffer = vk::DescriptorBufferInfo {
                buffer: staging_buffer,
                offset: input_offset,
                range: range_size.max(1),
            };
            let descriptor_image = vk::DescriptorImageInfo {
                sampler: vk::Sampler::null(),
                image_view: storage_view,
                image_layout: vk::ImageLayout::GENERAL,
            };
            unsafe {
                self.compute_pass_descriptor_queue.as_mut().acquire();
                self.compute_pass_descriptor_queue.as_mut().add_buffer(
                    staging_buffer,
                    input_offset,
                    range_size.max(1),
                );
                self.compute_pass_descriptor_queue
                    .as_mut()
                    .add_image(storage_view);
            }
            let uniforms = AstcPushConstants {
                blocks_dims: block_dims,
                layer_stride: params.layer_stride,
                block_size: params.block_size,
                x_shift: params.x_shift,
                block_height: params.block_height,
                block_height_mask: params.block_height_mask,
            };
            let device = device_handle.clone();
            scheduler.record(move |cmdbuf| unsafe {
                let writes = [
                    vk::WriteDescriptorSet::builder()
                        .dst_set(descriptor_set)
                        .dst_binding(ASTC_BINDING_INPUT_BUFFER)
                        .descriptor_type(vk::DescriptorType::STORAGE_BUFFER)
                        .buffer_info(std::slice::from_ref(&descriptor_buffer))
                        .build(),
                    vk::WriteDescriptorSet::builder()
                        .dst_set(descriptor_set)
                        .dst_binding(ASTC_BINDING_OUTPUT_IMAGE)
                        .descriptor_type(vk::DescriptorType::STORAGE_IMAGE)
                        .image_info(std::slice::from_ref(&descriptor_image))
                        .build(),
                ];
                device.update_descriptor_sets(&writes, &[]);
                device.cmd_bind_descriptor_sets(
                    cmdbuf,
                    vk::PipelineBindPoint::COMPUTE,
                    layout,
                    0,
                    &[descriptor_set],
                    &[],
                );
                device.cmd_push_constants(
                    cmdbuf,
                    layout,
                    vk::ShaderStageFlags::COMPUTE,
                    0,
                    bytemuck::bytes_of(&uniforms),
                );
                device.cmd_dispatch(cmdbuf, num_dispatches_x, num_dispatches_y, num_dispatches_z);
            });
        }

        let device = device_handle;
        scheduler.record(move |cmdbuf| unsafe {
            let image_barrier = vk::ImageMemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::SHADER_WRITE)
                .dst_access_mask(vk::AccessFlags::SHADER_READ | vk::AccessFlags::SHADER_WRITE)
                .old_layout(vk::ImageLayout::GENERAL)
                .new_layout(vk::ImageLayout::GENERAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(image)
                .subresource_range(vk::ImageSubresourceRange {
                    aspect_mask,
                    base_mip_level: 0,
                    level_count: vk::REMAINING_MIP_LEVELS,
                    base_array_layer: 0,
                    layer_count: vk::REMAINING_ARRAY_LAYERS,
                })
                .build();
            device.cmd_pipeline_barrier(
                cmdbuf,
                vk::PipelineStageFlags::COMPUTE_SHADER,
                vk::PipelineStageFlags::ALL_COMMANDS,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &[image_barrier],
            );
        });
        scheduler.finish();
        true
    }
}

// ---------------------------------------------------------------------------
// MSAACopyPass
// ---------------------------------------------------------------------------

/// Port of `MSAACopyPass` class.
///
/// Copies between MSAA and non-MSAA images via compute shader.
/// Maintains two pipelines: [0] = non-msaa to msaa, [1] = msaa to non-msaa.
pub struct MsaaCopyPass {
    base: ComputePass,
    modules: [vk::ShaderModule; 2],
    pipelines: [vk::Pipeline; 2],
    descriptor_pool: NonNull<DescriptorPool>,
    compute_pass_descriptor_queue: NonNull<ComputePassDescriptorQueue>,
}

impl MsaaCopyPass {
    pub fn new(
        device: &ash::Device,
        descriptor_pool: &mut DescriptorPool,
        compute_pass_descriptor_queue: &mut ComputePassDescriptorQueue,
    ) -> Result<Self, vk::Result> {
        let bindings = [
            vk::DescriptorSetLayoutBinding {
                binding: 0,
                descriptor_type: vk::DescriptorType::STORAGE_IMAGE,
                descriptor_count: 1,
                stage_flags: vk::ShaderStageFlags::COMPUTE,
                p_immutable_samplers: std::ptr::null(),
            },
            vk::DescriptorSetLayoutBinding {
                binding: 1,
                descriptor_type: vk::DescriptorType::STORAGE_IMAGE,
                descriptor_count: 1,
                stage_flags: vk::ShaderStageFlags::COMPUTE,
                p_immutable_samplers: std::ptr::null(),
            },
        ];
        let templates = [vk::DescriptorUpdateTemplateEntry {
            dst_binding: 0,
            dst_array_element: 0,
            descriptor_count: 2,
            descriptor_type: vk::DescriptorType::STORAGE_IMAGE,
            offset: 0,
            stride: std::mem::size_of::<
                crate::renderer_vulkan::update_descriptor::DescriptorUpdateEntry,
            >(),
        }];
        let base = ComputePass::new(
            device,
            &bindings,
            &templates,
            &[],
            CONVERT_NON_MSAA_TO_MSAA_COMP_SPV,
            None,
        )?;
        Self::new_with_pass(
            device,
            base,
            CONVERT_NON_MSAA_TO_MSAA_COMP_SPV,
            CONVERT_MSAA_TO_NON_MSAA_COMP_SPV,
            descriptor_pool,
            compute_pass_descriptor_queue,
        )
    }

    /// Port of `MSAACopyPass::MSAACopyPass`.
    ///
    /// Creates both MSAA copy pipelines (to/from MSAA).
    pub fn new_with_pass(
        device: &ash::Device,
        base: ComputePass,
        non_msaa_to_msaa_code: &[u32],
        msaa_to_non_msaa_code: &[u32],
        descriptor_pool: &mut DescriptorPool,
        compute_pass_descriptor_queue: &mut ComputePassDescriptorQueue,
    ) -> Result<Self, vk::Result> {
        let make_pipeline = |code: &[u32]| -> Result<(vk::ShaderModule, vk::Pipeline), vk::Result> {
            let module_ci = vk::ShaderModuleCreateInfo::builder().code(code).build();
            let module = unsafe { device.create_shader_module(&module_ci, None)? };

            let main_name = std::ffi::CString::new("main").unwrap();
            let stage_ci = vk::PipelineShaderStageCreateInfo::builder()
                .stage(vk::ShaderStageFlags::COMPUTE)
                .module(module)
                .name(&main_name)
                .build();

            let pipeline_ci = vk::ComputePipelineCreateInfo::builder()
                .stage(stage_ci)
                .layout(base.layout)
                .build();

            let pipelines = unsafe {
                device
                    .create_compute_pipelines(vk::PipelineCache::null(), &[pipeline_ci], None)
                    .map_err(|e| e.1)?
            };

            Ok((module, pipelines[0]))
        };

        let (module0, pipeline0) = make_pipeline(non_msaa_to_msaa_code)?;
        let (module1, pipeline1) = make_pipeline(msaa_to_non_msaa_code)?;

        Ok(MsaaCopyPass {
            base,
            modules: [module0, module1],
            pipelines: [pipeline0, pipeline1],
            descriptor_pool: NonNull::from(descriptor_pool),
            compute_pass_descriptor_queue: NonNull::from(compute_pass_descriptor_queue),
        })
    }

    /// Port of `MSAACopyPass::CopyImage`.
    ///
    /// Dispatches the appropriate MSAA copy pipeline for each image copy region.
    pub fn copy_image(
        &mut self,
        device: &ash::Device,
        cmdbuf: vk::CommandBuffer,
        dst_image: vk::Image,
        src_view: vk::ImageView,
        dst_view: vk::ImageView,
        extent_width: u32,
        extent_height: u32,
        extent_depth: u32,
        msaa_to_non_msaa: bool,
    ) {
        let pipeline_idx = if msaa_to_non_msaa { 1 } else { 0 };
        let msaa_pipeline = self.pipelines[pipeline_idx];
        let descriptor_set = unsafe {
            self.descriptor_pool
                .as_ref()
                .allocate(
                    self.base.descriptor_set_layout,
                    &PoolDescriptorBankInfo {
                        uniform_buffers: 0,
                        storage_buffers: 0,
                        texture_buffers: 0,
                        image_buffers: 0,
                        textures: 0,
                        images: 2,
                        score: 2,
                    },
                )
                .expect("MSAACopyPass descriptor allocation failed")
        };
        let descriptor_images = [
            vk::DescriptorImageInfo {
                sampler: vk::Sampler::null(),
                image_view: src_view,
                image_layout: vk::ImageLayout::GENERAL,
            },
            vk::DescriptorImageInfo {
                sampler: vk::Sampler::null(),
                image_view: dst_view,
                image_layout: vk::ImageLayout::GENERAL,
            },
        ];
        unsafe {
            self.compute_pass_descriptor_queue.as_mut().acquire();
            self.compute_pass_descriptor_queue
                .as_mut()
                .add_image(src_view);
            self.compute_pass_descriptor_queue
                .as_mut()
                .add_image(dst_view);
            let writes = [vk::WriteDescriptorSet::builder()
                .dst_set(descriptor_set)
                .dst_binding(0)
                .descriptor_type(vk::DescriptorType::STORAGE_IMAGE)
                .image_info(&descriptor_images)
                .build()];
            device.update_descriptor_sets(&writes, &[]);
        }

        let num_dispatches_x = (extent_width + 7) / 8;
        let num_dispatches_y = (extent_height + 7) / 8;
        let num_dispatches_z = extent_depth;

        unsafe {
            device.cmd_bind_pipeline(cmdbuf, vk::PipelineBindPoint::COMPUTE, msaa_pipeline);
            device.cmd_bind_descriptor_sets(
                cmdbuf,
                vk::PipelineBindPoint::COMPUTE,
                self.base.layout,
                0,
                &[descriptor_set],
                &[],
            );
            device.cmd_dispatch(cmdbuf, num_dispatches_x, num_dispatches_y, num_dispatches_z);

            let write_barrier = vk::ImageMemoryBarrier {
                s_type: vk::StructureType::IMAGE_MEMORY_BARRIER,
                p_next: std::ptr::null(),
                src_access_mask: vk::AccessFlags::SHADER_WRITE,
                dst_access_mask: vk::AccessFlags::SHADER_READ,
                old_layout: vk::ImageLayout::GENERAL,
                new_layout: vk::ImageLayout::GENERAL,
                src_queue_family_index: vk::QUEUE_FAMILY_IGNORED,
                dst_queue_family_index: vk::QUEUE_FAMILY_IGNORED,
                image: dst_image,
                subresource_range: vk::ImageSubresourceRange {
                    aspect_mask: vk::ImageAspectFlags::COLOR,
                    base_mip_level: 0,
                    level_count: vk::REMAINING_MIP_LEVELS,
                    base_array_layer: 0,
                    layer_count: vk::REMAINING_ARRAY_LAYERS,
                },
            };

            device.cmd_pipeline_barrier(
                cmdbuf,
                vk::PipelineStageFlags::COMPUTE_SHADER,
                vk::PipelineStageFlags::FRAGMENT_SHADER,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &[write_barrier],
            );
        }
    }

    pub fn pipeline(&self, msaa_to_non_msaa: bool) -> vk::Pipeline {
        self.pipelines[if msaa_to_non_msaa { 1 } else { 0 }]
    }

    pub fn layout(&self) -> vk::PipelineLayout {
        self.base.layout
    }

    pub fn descriptor_set_layout(&self) -> vk::DescriptorSetLayout {
        self.base.descriptor_set_layout
    }
}

// Implement bytemuck traits for push constants that need it
unsafe impl bytemuck::Zeroable for AstcPushConstants {}
unsafe impl bytemuck::Pod for AstcPushConstants {}
unsafe impl bytemuck::Zeroable for QueriesPrefixScanPushConstants {}
unsafe impl bytemuck::Pod for QueriesPrefixScanPushConstants {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn astc_push_constants_size() {
        assert_eq!(
            std::mem::size_of::<AstcPushConstants>(),
            7 * std::mem::size_of::<u32>()
        );
    }

    #[test]
    fn queries_prefix_scan_push_constants_size() {
        assert_eq!(
            std::mem::size_of::<QueriesPrefixScanPushConstants>(),
            4 * std::mem::size_of::<u32>()
        );
    }

    #[test]
    fn bank_info_constants() {
        assert_eq!(INPUT_OUTPUT_BANK_INFO.storage_buffers, 2);
        assert_eq!(QUERIES_SCAN_BANK_INFO.storage_buffers, 3);
        assert_eq!(ASTC_BANK_INFO.images, 1);
        assert_eq!(MSAA_BANK_INFO.images, 2);
    }
}
