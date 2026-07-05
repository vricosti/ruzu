// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_compute_pipeline.h` / `vk_compute_pipeline.cpp`.
//!
//! Manages compilation and configuration of a single Vulkan compute pipeline.
//! Supports asynchronous pipeline building via a background thread worker.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Condvar, Mutex};

use ash::vk;
use shader_recompiler::shader_info::Info as ShaderInfo;

use super::pipeline_helper::{RESCALING_LAYOUT_SIZE, RESCALING_LAYOUT_WORDS_OFFSET};

// ---------------------------------------------------------------------------
// ComputePipeline
// ---------------------------------------------------------------------------

/// Port of `ComputePipeline` class.
///
/// Wraps a single Vulkan compute pipeline, handling asynchronous building,
/// descriptor set allocation, and per-dispatch configuration.
///
/// Upstream fields:
/// - `device` — reference to the Vulkan device wrapper
/// - `pipeline_cache` — shared VkPipelineCache for compilation
/// - `guest_descriptor_queue` — queue for descriptor updates
/// - `info` — shader info from the recompiler
/// - `uniform_buffer_sizes` — per-binding UBO sizes
/// - `spv_module` — SPIR-V shader module
/// - `descriptor_set_layout` — layout for the pipeline's descriptor set
/// - `descriptor_allocator` — allocates descriptor sets from the pool
/// - `pipeline_layout` — pipeline layout handle
/// - `descriptor_update_template` — template for fast descriptor updates
/// - `pipeline` — the compiled VkPipeline
/// - `build_condvar` / `build_mutex` / `is_built` — async build synchronization
pub struct ComputePipeline {
    device: ash::Device,
    /// Upstream `Shader::Info info`.
    info: ShaderInfo,

    /// SPV shader module.
    spv_module: vk::ShaderModule,

    /// Descriptor set layout for this pipeline.
    descriptor_set_layout: vk::DescriptorSetLayout,

    /// Pipeline layout.
    pipeline_layout: vk::PipelineLayout,

    /// Descriptor update template.
    descriptor_update_template: vk::DescriptorUpdateTemplate,

    /// The compiled compute pipeline handle.
    pipeline: vk::Pipeline,

    /// Uniform buffer sizes per binding (from shader info).
    uniform_buffer_sizes: [u32; ShaderInfo::MAX_CBUFS],

    /// Synchronization for async build.
    build_condvar: Condvar,
    build_mutex: Mutex<()>,
    is_built: AtomicBool,
}

// Vulkan pipeline objects are opaque device handles. Upstream queues compute
// pipeline construction on `ThreadWorker` and then transfers ownership back to
// the cache; the Rust disk preload path mirrors that transfer.
unsafe impl Send for ComputePipeline {}

impl ComputePipeline {
    /// Port of `ComputePipeline::ComputePipeline`.
    ///
    /// In the full implementation, this:
    /// 1. Creates the descriptor set layout from shader info
    /// 2. Creates the pipeline layout with push constant ranges
    /// 3. Creates the descriptor update template
    /// 4. Optionally builds the pipeline asynchronously via thread_worker
    /// 5. Notifies shader_notify when building starts/ends
    ///
    /// The pipeline can be built synchronously (if no thread_worker) or
    /// asynchronously, with `is_built` signaling completion.
    pub fn new(
        device: ash::Device,
        info: ShaderInfo,
        spv_module: vk::ShaderModule,
        pipeline_cache: vk::PipelineCache,
    ) -> Option<Self> {
        let Some(descriptor_set_layout) = create_compute_descriptor_set_layout(&device, &info)
        else {
            unsafe {
                device.destroy_shader_module(spv_module, None);
            }
            return None;
        };
        let Some(pipeline_layout) = create_compute_pipeline_layout(&device, descriptor_set_layout)
        else {
            unsafe {
                device.destroy_descriptor_set_layout(descriptor_set_layout, None);
                device.destroy_shader_module(spv_module, None);
            }
            return None;
        };
        let descriptor_update_template = vk::DescriptorUpdateTemplate::null();
        // Build the compute pipeline synchronously
        let main_name = std::ffi::CString::new("main").unwrap();
        let stage_ci = vk::PipelineShaderStageCreateInfo::builder()
            .stage(vk::ShaderStageFlags::COMPUTE)
            .module(spv_module)
            .name(&main_name)
            .build();

        let ci = vk::ComputePipelineCreateInfo::builder()
            .stage(stage_ci)
            .layout(pipeline_layout)
            .build();

        let pipeline = match unsafe { device.create_compute_pipelines(pipeline_cache, &[ci], None) }
        {
            Ok(pipelines) => pipelines[0],
            Err(_) => unsafe {
                device.destroy_pipeline_layout(pipeline_layout, None);
                device.destroy_descriptor_set_layout(descriptor_set_layout, None);
                device.destroy_shader_module(spv_module, None);
                return None;
            },
        };
        let mut uniform_buffer_sizes = [0u32; ShaderInfo::MAX_CBUFS];
        uniform_buffer_sizes.copy_from_slice(&info.constant_buffer_used_sizes);

        Some(ComputePipeline {
            device,
            info,
            spv_module,
            descriptor_set_layout,
            pipeline_layout,
            descriptor_update_template,
            pipeline,
            uniform_buffer_sizes,
            build_condvar: Condvar::new(),
            build_mutex: Mutex::new(()),
            is_built: AtomicBool::new(true),
        })
    }

    /// Port of `ComputePipeline::Configure`.
    ///
    /// Configures and dispatches the compute pipeline:
    /// 1. Waits for async build if necessary
    /// 2. Fills buffer descriptors from kepler_compute engine state
    /// 3. Fills texture/image descriptors from texture cache
    /// 4. Commits a descriptor set and updates it
    /// 5. Binds the pipeline, descriptor set, and dispatches
    ///
    /// Requires access to: kepler_compute, gpu_memory, scheduler,
    /// buffer_cache, texture_cache.
    pub fn configure(
        &self,
        device: &ash::Device,
        cmdbuf: vk::CommandBuffer,
        descriptor_set: vk::DescriptorSet,
    ) {
        // Wait for build if async
        if !self.is_built.load(Ordering::Acquire) {
            let lock = self.build_mutex.lock().unwrap();
            let _guard = self
                .build_condvar
                .wait_while(lock, |_| !self.is_built.load(Ordering::Relaxed));
        }

        if self.pipeline == vk::Pipeline::null() {
            return;
        }

        unsafe {
            device.cmd_bind_pipeline(cmdbuf, vk::PipelineBindPoint::COMPUTE, self.pipeline);
            if descriptor_set != vk::DescriptorSet::null() {
                device.cmd_bind_descriptor_sets(
                    cmdbuf,
                    vk::PipelineBindPoint::COMPUTE,
                    self.pipeline_layout,
                    0,
                    &[descriptor_set],
                    &[],
                );
            }
        }
    }

    /// Returns whether the pipeline has finished building.
    pub fn is_built(&self) -> bool {
        self.is_built.load(Ordering::Relaxed)
    }

    /// Returns the pipeline handle.
    pub fn pipeline(&self) -> vk::Pipeline {
        self.pipeline
    }

    /// Returns the pipeline layout handle.
    pub fn pipeline_layout(&self) -> vk::PipelineLayout {
        self.pipeline_layout
    }

    /// Returns the descriptor set layout.
    pub fn descriptor_set_layout(&self) -> vk::DescriptorSetLayout {
        self.descriptor_set_layout
    }

    /// Returns the descriptor update template.
    pub fn descriptor_update_template(&self) -> vk::DescriptorUpdateTemplate {
        self.descriptor_update_template
    }

    pub fn info(&self) -> &ShaderInfo {
        &self.info
    }

    pub fn uniform_buffer_sizes(&self) -> &[u32; ShaderInfo::MAX_CBUFS] {
        &self.uniform_buffer_sizes
    }

    pub fn requires_descriptor_binding(&self) -> bool {
        info_requires_descriptor_binding(&self.info)
    }
}

pub fn info_requires_descriptor_binding(info: &ShaderInfo) -> bool {
    !info.constant_buffer_descriptors.is_empty()
        || !info.storage_buffers_descriptors.is_empty()
        || !info.texture_buffer_descriptors.is_empty()
        || !info.image_buffer_descriptors.is_empty()
        || !info.texture_descriptors.is_empty()
        || !info.image_descriptors.is_empty()
}

impl Drop for ComputePipeline {
    fn drop(&mut self) {
        unsafe {
            if self.pipeline != vk::Pipeline::null() {
                self.device.destroy_pipeline(self.pipeline, None);
            }
            if self.pipeline_layout != vk::PipelineLayout::null() {
                self.device
                    .destroy_pipeline_layout(self.pipeline_layout, None);
            }
            if self.descriptor_set_layout != vk::DescriptorSetLayout::null() {
                self.device
                    .destroy_descriptor_set_layout(self.descriptor_set_layout, None);
            }
            if self.descriptor_update_template != vk::DescriptorUpdateTemplate::null() {
                self.device
                    .destroy_descriptor_update_template(self.descriptor_update_template, None);
            }
            if self.spv_module != vk::ShaderModule::null() {
                self.device.destroy_shader_module(self.spv_module, None);
            }
        }
    }
}

fn create_compute_descriptor_set_layout(
    device: &ash::Device,
    info: &ShaderInfo,
) -> Option<vk::DescriptorSetLayout> {
    let bindings = compute_descriptor_set_layout_bindings(info);
    let layout_info = vk::DescriptorSetLayoutCreateInfo::builder()
        .bindings(&bindings)
        .build();
    unsafe { device.create_descriptor_set_layout(&layout_info, None).ok() }
}

fn compute_descriptor_set_layout_bindings(
    info: &ShaderInfo,
) -> Vec<vk::DescriptorSetLayoutBinding> {
    let mut bindings = Vec::new();
    let mut binding = 0u32;
    let mut push_binding = |descriptor_type: vk::DescriptorType, count: u32| {
        if count == 0 {
            return;
        }
        bindings.push(
            vk::DescriptorSetLayoutBinding::builder()
                .binding(binding)
                .descriptor_type(descriptor_type)
                .descriptor_count(count)
                .stage_flags(vk::ShaderStageFlags::COMPUTE)
                .build(),
        );
        binding += 1;
    };

    for desc in &info.constant_buffer_descriptors {
        push_binding(vk::DescriptorType::UNIFORM_BUFFER, desc.count);
    }
    for desc in &info.storage_buffers_descriptors {
        push_binding(vk::DescriptorType::STORAGE_BUFFER, desc.count);
    }
    for desc in &info.texture_buffer_descriptors {
        push_binding(vk::DescriptorType::UNIFORM_TEXEL_BUFFER, desc.count);
    }
    for desc in &info.image_buffer_descriptors {
        push_binding(vk::DescriptorType::STORAGE_TEXEL_BUFFER, desc.count);
    }
    for desc in &info.texture_descriptors {
        push_binding(vk::DescriptorType::COMBINED_IMAGE_SAMPLER, desc.count);
    }
    for desc in &info.image_descriptors {
        push_binding(vk::DescriptorType::STORAGE_IMAGE, desc.count);
    }
    bindings
}

fn create_compute_pipeline_layout(
    device: &ash::Device,
    descriptor_set_layout: vk::DescriptorSetLayout,
) -> Option<vk::PipelineLayout> {
    let push_constant_range = vk::PushConstantRange {
        stage_flags: vk::ShaderStageFlags::COMPUTE,
        offset: RESCALING_LAYOUT_WORDS_OFFSET,
        size: RESCALING_LAYOUT_SIZE,
    };
    let layouts = [descriptor_set_layout];
    let pipeline_layout_info = vk::PipelineLayoutCreateInfo::builder()
        .set_layouts(&layouts)
        .push_constant_ranges(std::slice::from_ref(&push_constant_range))
        .build();
    unsafe {
        device
            .create_pipeline_layout(&pipeline_layout_info, None)
            .ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use shader_recompiler::shader_info::{
        ConstantBufferDescriptor, ImageBufferDescriptor, ImageDescriptor, StorageBufferDescriptor,
        TextureBufferDescriptor, TextureDescriptor,
    };
    use shader_recompiler::shader_info::{ImageFormat, TextureType};

    #[test]
    fn compute_pipeline_struct_size() {
        // Ensure the struct can be constructed (compilation test)
        let _size = std::mem::size_of::<ComputePipeline>();
    }

    #[test]
    fn compute_descriptor_bindings_follow_upstream_descriptor_order() {
        let mut info = ShaderInfo::default();
        info.constant_buffer_descriptors
            .push(ConstantBufferDescriptor { index: 0, count: 2 });
        info.storage_buffers_descriptors
            .push(StorageBufferDescriptor {
                cbuf_index: 0,
                cbuf_offset: 0,
                count: 1,
                is_written: true,
            });
        info.texture_buffer_descriptors
            .push(TextureBufferDescriptor {
                has_secondary: false,
                cbuf_index: 0,
                cbuf_offset: 0,
                shift_left: 0,
                secondary_cbuf_index: 0,
                secondary_cbuf_offset: 0,
                secondary_shift_left: 0,
                count: 3,
                size_shift: 0,
            });
        info.image_buffer_descriptors.push(ImageBufferDescriptor {
            format: ImageFormat::R32Uint,
            is_written: true,
            is_read: true,
            is_integer: true,
            cbuf_index: 0,
            cbuf_offset: 0,
            count: 4,
            size_shift: 0,
        });
        info.texture_descriptors.push(TextureDescriptor {
            texture_type: TextureType::Color2D,
            is_depth: false,
            is_multisample: false,
            has_secondary: false,
            cbuf_index: 0,
            cbuf_offset: 0,
            shift_left: 0,
            secondary_cbuf_index: 0,
            secondary_cbuf_offset: 0,
            secondary_shift_left: 0,
            count: 5,
            size_shift: 0,
        });
        info.image_descriptors.push(ImageDescriptor {
            texture_type: TextureType::Color2D,
            format: ImageFormat::R32Uint,
            is_written: true,
            is_read: true,
            is_integer: true,
            cbuf_index: 0,
            cbuf_offset: 0,
            count: 6,
            size_shift: 0,
        });

        let bindings = compute_descriptor_set_layout_bindings(&info);
        let types: Vec<_> = bindings
            .iter()
            .map(|binding| binding.descriptor_type)
            .collect();
        let counts: Vec<_> = bindings
            .iter()
            .map(|binding| binding.descriptor_count)
            .collect();

        assert_eq!(
            types,
            vec![
                vk::DescriptorType::UNIFORM_BUFFER,
                vk::DescriptorType::STORAGE_BUFFER,
                vk::DescriptorType::UNIFORM_TEXEL_BUFFER,
                vk::DescriptorType::STORAGE_TEXEL_BUFFER,
                vk::DescriptorType::COMBINED_IMAGE_SAMPLER,
                vk::DescriptorType::STORAGE_IMAGE,
            ]
        );
        assert_eq!(counts, vec![2, 1, 3, 4, 5, 6]);
        assert!(bindings
            .iter()
            .all(|binding| binding.stage_flags == vk::ShaderStageFlags::COMPUTE));
    }

    #[test]
    fn compute_descriptor_requirement_tracks_shader_resource_descriptors() {
        let mut info = ShaderInfo::default();
        assert!(!info_requires_descriptor_binding(&info));

        info.constant_buffer_descriptors
            .push(ConstantBufferDescriptor { index: 0, count: 1 });
        assert!(info_requires_descriptor_binding(&info));
    }
}
