// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Vulkan presentation layer using ash + SDL2 native Vulkan support.
//!
//! Handles instance creation, device selection, swapchain management,
//! and frame presentation. Falls back gracefully to software rendering
//! if Vulkan is not available.

use ash::vk::{self, Handle};
use log::info;
use std::ffi::{CStr, CString};
use std::ptr;
use thiserror::Error;

/// Swizzle pixels from RGBA byte order to BGRA byte order (swap R and B channels).
/// Both slices must have the same length and be a multiple of 4 bytes.
fn swizzle_rgba_to_bgra(src: &[u8], dst: &mut [u8]) {
    debug_assert_eq!(src.len(), dst.len());
    debug_assert_eq!(src.len() % 4, 0);

    for (s, d) in src.chunks_exact(4).zip(dst.chunks_exact_mut(4)) {
        d[0] = s[2]; // B <- src R position is actually B in ABGR8888? No.
        d[1] = s[1]; // G
        d[2] = s[0]; // R
        d[3] = s[3]; // A
    }
}

/// Find a memory type index matching the given type filter bits and required properties.
fn find_memory_type(
    instance: &ash::Instance,
    physical_device: vk::PhysicalDevice,
    type_filter: u32,
    properties: vk::MemoryPropertyFlags,
) -> Option<u32> {
    let mem_props = unsafe { instance.get_physical_device_memory_properties(physical_device) };
    for i in 0..mem_props.memory_type_count {
        if (type_filter & (1 << i)) != 0
            && mem_props.memory_types[i as usize]
                .property_flags
                .contains(properties)
        {
            return Some(i);
        }
    }
    None
}

#[derive(Debug, Error)]
pub enum VulkanError {
    #[error("Vulkan not available: {0}")]
    NotAvailable(String),
    #[error("No suitable GPU found")]
    NoSuitableDevice,
    #[error("Surface creation failed: {0}")]
    SurfaceCreation(String),
    #[error("Swapchain creation failed: {0}")]
    SwapchainCreation(String),
    #[error("Present failed: {0}")]
    PresentFailed(String),
}

pub struct VulkanPresenter {
    _entry: ash::Entry,
    instance: ash::Instance,
    surface: vk::SurfaceKHR,
    surface_loader: ash::extensions::khr::Surface,
    physical_device: vk::PhysicalDevice,
    device: ash::Device,
    graphics_queue: vk::Queue,
    queue_family_index: u32,
    swapchain_loader: ash::extensions::khr::Swapchain,
    swapchain: vk::SwapchainKHR,
    swapchain_images: Vec<vk::Image>,
    swapchain_format: vk::Format,
    swapchain_extent: vk::Extent2D,
    command_pool: vk::CommandPool,
    command_buffers: Vec<vk::CommandBuffer>,
    image_available_semaphore: vk::Semaphore,
    render_finished_semaphore: vk::Semaphore,
    in_flight_fence: vk::Fence,

    // Staging buffer (CPU-visible, for pixel upload)
    staging_buffer: vk::Buffer,
    staging_memory: vk::DeviceMemory,
    staging_size: u64,
    staging_mapped: *mut u8,

    // Intermediate image (device-local, guest FB dimensions)
    framebuffer_image: vk::Image,
    framebuffer_memory: vk::DeviceMemory,
    fb_width: u32,
    fb_height: u32,
}

impl VulkanPresenter {
    /// Initialize Vulkan from an SDL2 window.
    /// Returns Err if Vulkan is unavailable (graceful fallback to software blit).
    pub fn new(window: &sdl2::video::Window) -> Result<Self, VulkanError> {
        // 1. Load Vulkan entry points
        let entry = unsafe { ash::Entry::load() }
            .map_err(|e| VulkanError::NotAvailable(format!("Failed to load Vulkan: {}", e)))?;

        // 2. Get required instance extensions from SDL2
        let sdl_extensions = window
            .vulkan_instance_extensions()
            .map_err(|e| VulkanError::NotAvailable(format!("SDL2 extensions: {}", e)))?;

        info!("SDL2 Vulkan extensions: {:?}", sdl_extensions);

        let extension_cstrings: Vec<CString> = sdl_extensions
            .iter()
            .map(|s| CString::new(*s).unwrap())
            .collect();
        let extension_ptrs: Vec<*const std::os::raw::c_char> =
            extension_cstrings.iter().map(|s| s.as_ptr()).collect();

        // 3. Create Vulkan instance
        let app_name = CStr::from_bytes_with_nul(b"ruzu\0").unwrap();
        let app_info = vk::ApplicationInfo::builder()
            .application_name(app_name)
            .application_version(vk::make_api_version(0, 0, 1, 0))
            .engine_name(app_name)
            .engine_version(vk::make_api_version(0, 0, 1, 0))
            .api_version(vk::API_VERSION_1_1);

        let instance_info = vk::InstanceCreateInfo::builder()
            .application_info(&app_info)
            .enabled_extension_names(&extension_ptrs);

        let instance = unsafe { entry.create_instance(&instance_info, None) }
            .map_err(|e| VulkanError::NotAvailable(format!("Instance creation: {}", e)))?;

        info!("Vulkan instance created");

        // 4. Create surface via SDL2's native Vulkan support
        let raw_surface = window
            .vulkan_create_surface(instance.handle().as_raw() as usize)
            .map_err(|e| VulkanError::SurfaceCreation(e))?;
        let surface = vk::SurfaceKHR::from_raw(raw_surface);
        let surface_loader = ash::extensions::khr::Surface::new(&entry, &instance);

        info!("Vulkan surface created");

        // 5. Select physical device (prefer discrete GPU with graphics+present)
        let (physical_device, queue_family_index) =
            unsafe { Self::select_physical_device(&instance, &surface_loader, surface)? };

        let props = unsafe { instance.get_physical_device_properties(physical_device) };
        let device_name = unsafe { CStr::from_ptr(props.device_name.as_ptr()) };
        info!(
            "Selected GPU: {:?} (type: {:?})",
            device_name, props.device_type
        );

        // 6. Create logical device with a single graphics queue
        let queue_priorities = [1.0f32];
        let queue_info = vk::DeviceQueueCreateInfo::builder()
            .queue_family_index(queue_family_index)
            .queue_priorities(&queue_priorities);

        let swapchain_ext_name = ash::extensions::khr::Swapchain::name();
        let device_extensions = [swapchain_ext_name.as_ptr()];
        let device_info = vk::DeviceCreateInfo::builder()
            .queue_create_infos(std::slice::from_ref(&queue_info))
            .enabled_extension_names(&device_extensions);

        let device = unsafe { instance.create_device(physical_device, &device_info, None) }
            .map_err(|e| VulkanError::NotAvailable(format!("Device creation: {}", e)))?;
        let graphics_queue = unsafe { device.get_device_queue(queue_family_index, 0) };

        info!(
            "Vulkan device created (queue family: {})",
            queue_family_index
        );

        // 7. Create swapchain
        let swapchain_loader = ash::extensions::khr::Swapchain::new(&instance, &device);
        let (win_w, win_h) = window.size();
        let fallback_extent = vk::Extent2D {
            width: win_w,
            height: win_h,
        };

        let (swapchain, swapchain_images, swapchain_format, swapchain_extent) = unsafe {
            Self::create_swapchain(
                &surface_loader,
                &swapchain_loader,
                physical_device,
                surface,
                vk::SwapchainKHR::null(),
                fallback_extent,
            )?
        };

        info!(
            "Swapchain created: {:?}, {}x{}, {} images",
            swapchain_format,
            swapchain_extent.width,
            swapchain_extent.height,
            swapchain_images.len()
        );

        // 8. Create command pool and buffers
        let pool_info = vk::CommandPoolCreateInfo::builder()
            .queue_family_index(queue_family_index)
            .flags(vk::CommandPoolCreateFlags::RESET_COMMAND_BUFFER);

        let command_pool = unsafe { device.create_command_pool(&pool_info, None) }
            .map_err(|e| VulkanError::NotAvailable(format!("Command pool: {}", e)))?;

        let alloc_info = vk::CommandBufferAllocateInfo::builder()
            .command_pool(command_pool)
            .level(vk::CommandBufferLevel::PRIMARY)
            .command_buffer_count(swapchain_images.len() as u32);

        let command_buffers = unsafe { device.allocate_command_buffers(&alloc_info) }
            .map_err(|e| VulkanError::NotAvailable(format!("Command buffers: {}", e)))?;

        // 9. Create synchronization objects
        let semaphore_info = vk::SemaphoreCreateInfo::builder();
        let fence_info = vk::FenceCreateInfo::builder().flags(vk::FenceCreateFlags::SIGNALED);

        let image_available_semaphore =
            unsafe { device.create_semaphore(&semaphore_info, None) }
                .map_err(|e| VulkanError::NotAvailable(format!("Semaphore: {}", e)))?;
        let render_finished_semaphore =
            unsafe { device.create_semaphore(&semaphore_info, None) }
                .map_err(|e| VulkanError::NotAvailable(format!("Semaphore: {}", e)))?;
        let in_flight_fence = unsafe { device.create_fence(&fence_info, None) }
            .map_err(|e| VulkanError::NotAvailable(format!("Fence: {}", e)))?;

        info!("Vulkan presenter initialized successfully");

        Ok(Self {
            _entry: entry,
            instance,
            surface,
            surface_loader,
            physical_device,
            device,
            graphics_queue,
            queue_family_index,
            swapchain_loader,
            swapchain,
            swapchain_images,
            swapchain_format,
            swapchain_extent,
            command_pool,
            command_buffers,
            image_available_semaphore,
            render_finished_semaphore,
            in_flight_fence,
            staging_buffer: vk::Buffer::null(),
            staging_memory: vk::DeviceMemory::null(),
            staging_size: 0,
            staging_mapped: ptr::null_mut(),
            framebuffer_image: vk::Image::null(),
            framebuffer_memory: vk::DeviceMemory::null(),
            fb_width: 0,
            fb_height: 0,
        })
    }

    /// Clear the swapchain image to `color` and present. Called once per frame.
    pub fn present_clear(&mut self, color: [f32; 4]) -> Result<(), VulkanError> {
        unsafe {
            // Wait for previous frame to finish
            self.device
                .wait_for_fences(&[self.in_flight_fence], true, u64::MAX)
                .map_err(|e| VulkanError::PresentFailed(format!("Wait fence: {}", e)))?;
            self.device
                .reset_fences(&[self.in_flight_fence])
                .map_err(|e| VulkanError::PresentFailed(format!("Reset fence: {}", e)))?;

            // Acquire next swapchain image
            let (image_index, _suboptimal) = match self.swapchain_loader.acquire_next_image(
                self.swapchain,
                u64::MAX,
                self.image_available_semaphore,
                vk::Fence::null(),
            ) {
                Ok(result) => result,
                Err(vk::Result::ERROR_OUT_OF_DATE_KHR) => {
                    self.recreate_swapchain(
                        self.swapchain_extent.width,
                        self.swapchain_extent.height,
                    )?;
                    return Ok(());
                }
                Err(e) => {
                    return Err(VulkanError::PresentFailed(format!("Acquire image: {}", e)))
                }
            };

            let cmd = self.command_buffers[image_index as usize];
            let image = self.swapchain_images[image_index as usize];

            // Record command buffer
            self.device
                .reset_command_buffer(cmd, vk::CommandBufferResetFlags::empty())
                .map_err(|e| VulkanError::PresentFailed(format!("Reset cmd: {}", e)))?;

            let begin_info = vk::CommandBufferBeginInfo::builder()
                .flags(vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT);
            self.device
                .begin_command_buffer(cmd, &begin_info)
                .map_err(|e| VulkanError::PresentFailed(format!("Begin cmd: {}", e)))?;

            let subresource_range = vk::ImageSubresourceRange {
                aspect_mask: vk::ImageAspectFlags::COLOR,
                base_mip_level: 0,
                level_count: 1,
                base_array_layer: 0,
                layer_count: 1,
            };

            // Transition: UNDEFINED → TRANSFER_DST_OPTIMAL
            let barrier_to_clear = vk::ImageMemoryBarrier::builder()
                .old_layout(vk::ImageLayout::UNDEFINED)
                .new_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(image)
                .subresource_range(subresource_range)
                .src_access_mask(vk::AccessFlags::empty())
                .dst_access_mask(vk::AccessFlags::TRANSFER_WRITE);

            self.device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::TOP_OF_PIPE,
                vk::PipelineStageFlags::TRANSFER,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                std::slice::from_ref(&barrier_to_clear),
            );

            // Clear the image
            let clear_color = vk::ClearColorValue { float32: color };
            self.device.cmd_clear_color_image(
                cmd,
                image,
                vk::ImageLayout::TRANSFER_DST_OPTIMAL,
                &clear_color,
                &[subresource_range],
            );

            // Transition: TRANSFER_DST_OPTIMAL → PRESENT_SRC_KHR
            let barrier_to_present = vk::ImageMemoryBarrier::builder()
                .old_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
                .new_layout(vk::ImageLayout::PRESENT_SRC_KHR)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(image)
                .subresource_range(subresource_range)
                .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                .dst_access_mask(vk::AccessFlags::empty());

            self.device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::TRANSFER,
                vk::PipelineStageFlags::BOTTOM_OF_PIPE,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                std::slice::from_ref(&barrier_to_present),
            );

            self.device
                .end_command_buffer(cmd)
                .map_err(|e| VulkanError::PresentFailed(format!("End cmd: {}", e)))?;

            // Submit
            let wait_semaphores = [self.image_available_semaphore];
            let wait_stages = [vk::PipelineStageFlags::TRANSFER];
            let signal_semaphores = [self.render_finished_semaphore];
            let cmd_buffers = [cmd];

            let submit_info = vk::SubmitInfo::builder()
                .wait_semaphores(&wait_semaphores)
                .wait_dst_stage_mask(&wait_stages)
                .command_buffers(&cmd_buffers)
                .signal_semaphores(&signal_semaphores);

            self.device
                .queue_submit(
                    self.graphics_queue,
                    std::slice::from_ref(&submit_info),
                    self.in_flight_fence,
                )
                .map_err(|e| VulkanError::PresentFailed(format!("Submit: {}", e)))?;

            // Present
            let swapchains = [self.swapchain];
            let image_indices = [image_index];
            let present_info = vk::PresentInfoKHR::builder()
                .wait_semaphores(&signal_semaphores)
                .swapchains(&swapchains)
                .image_indices(&image_indices);

            match self
                .swapchain_loader
                .queue_present(self.graphics_queue, &present_info)
            {
                Ok(false) => Ok(()),
                Ok(true) | Err(vk::Result::ERROR_OUT_OF_DATE_KHR) => {
                    self.recreate_swapchain(
                        self.swapchain_extent.width,
                        self.swapchain_extent.height,
                    )?;
                    Ok(())
                }
                Err(e) => Err(VulkanError::PresentFailed(format!("Present: {}", e))),
            }
        }
    }

    /// Upload guest framebuffer pixels and present via the swapchain.
    /// `pixels` is in RGBA byte order, dimensions are the guest framebuffer size.
    pub fn present_framebuffer(
        &mut self,
        pixels: &[u8],
        width: u32,
        height: u32,
    ) -> Result<(), VulkanError> {
        let required_size = (width as u64) * (height as u64) * 4;
        if pixels.len() < required_size as usize {
            return Err(VulkanError::PresentFailed(
                "Pixel buffer too small".to_string(),
            ));
        }

        unsafe {
            // Wait for previous frame
            self.device
                .wait_for_fences(&[self.in_flight_fence], true, u64::MAX)
                .map_err(|e| VulkanError::PresentFailed(format!("Wait fence: {}", e)))?;
            self.device
                .reset_fences(&[self.in_flight_fence])
                .map_err(|e| VulkanError::PresentFailed(format!("Reset fence: {}", e)))?;

            // Ensure resources exist
            self.ensure_staging_buffer(required_size)?;
            self.ensure_framebuffer_image(width, height)?;

            // Swizzle RGBA → BGRA into the mapped staging buffer
            let dst_slice =
                std::slice::from_raw_parts_mut(self.staging_mapped, required_size as usize);
            swizzle_rgba_to_bgra(&pixels[..required_size as usize], dst_slice);

            // Acquire swapchain image
            let (image_index, _suboptimal) = match self.swapchain_loader.acquire_next_image(
                self.swapchain,
                u64::MAX,
                self.image_available_semaphore,
                vk::Fence::null(),
            ) {
                Ok(result) => result,
                Err(vk::Result::ERROR_OUT_OF_DATE_KHR) => {
                    self.recreate_swapchain(
                        self.swapchain_extent.width,
                        self.swapchain_extent.height,
                    )?;
                    return Ok(());
                }
                Err(e) => {
                    return Err(VulkanError::PresentFailed(format!("Acquire image: {}", e)));
                }
            };

            let cmd = self.command_buffers[image_index as usize];
            let swapchain_image = self.swapchain_images[image_index as usize];

            // Record command buffer
            self.device
                .reset_command_buffer(cmd, vk::CommandBufferResetFlags::empty())
                .map_err(|e| VulkanError::PresentFailed(format!("Reset cmd: {}", e)))?;

            let begin_info = vk::CommandBufferBeginInfo::builder()
                .flags(vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT);
            self.device
                .begin_command_buffer(cmd, &begin_info)
                .map_err(|e| VulkanError::PresentFailed(format!("Begin cmd: {}", e)))?;

            let subresource_range = vk::ImageSubresourceRange {
                aspect_mask: vk::ImageAspectFlags::COLOR,
                base_mip_level: 0,
                level_count: 1,
                base_array_layer: 0,
                layer_count: 1,
            };

            let subresource_layers = vk::ImageSubresourceLayers {
                aspect_mask: vk::ImageAspectFlags::COLOR,
                mip_level: 0,
                base_array_layer: 0,
                layer_count: 1,
            };

            // Barrier: framebuffer_image UNDEFINED → TRANSFER_DST_OPTIMAL
            let barrier_fb_to_dst = vk::ImageMemoryBarrier::builder()
                .old_layout(vk::ImageLayout::UNDEFINED)
                .new_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(self.framebuffer_image)
                .subresource_range(subresource_range)
                .src_access_mask(vk::AccessFlags::empty())
                .dst_access_mask(vk::AccessFlags::TRANSFER_WRITE);

            self.device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::TOP_OF_PIPE,
                vk::PipelineStageFlags::TRANSFER,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                std::slice::from_ref(&barrier_fb_to_dst),
            );

            // Copy staging buffer → framebuffer image
            let region = vk::BufferImageCopy {
                buffer_offset: 0,
                buffer_row_length: 0,
                buffer_image_height: 0,
                image_subresource: subresource_layers,
                image_offset: vk::Offset3D { x: 0, y: 0, z: 0 },
                image_extent: vk::Extent3D {
                    width,
                    height,
                    depth: 1,
                },
            };

            self.device.cmd_copy_buffer_to_image(
                cmd,
                self.staging_buffer,
                self.framebuffer_image,
                vk::ImageLayout::TRANSFER_DST_OPTIMAL,
                &[region],
            );

            // Barrier: framebuffer_image TRANSFER_DST → TRANSFER_SRC_OPTIMAL
            let barrier_fb_to_src = vk::ImageMemoryBarrier::builder()
                .old_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
                .new_layout(vk::ImageLayout::TRANSFER_SRC_OPTIMAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(self.framebuffer_image)
                .subresource_range(subresource_range)
                .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                .dst_access_mask(vk::AccessFlags::TRANSFER_READ);

            self.device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::TRANSFER,
                vk::PipelineStageFlags::TRANSFER,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                std::slice::from_ref(&barrier_fb_to_src),
            );

            // Barrier: swapchain_image UNDEFINED → TRANSFER_DST_OPTIMAL
            let barrier_sc_to_dst = vk::ImageMemoryBarrier::builder()
                .old_layout(vk::ImageLayout::UNDEFINED)
                .new_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(swapchain_image)
                .subresource_range(subresource_range)
                .src_access_mask(vk::AccessFlags::empty())
                .dst_access_mask(vk::AccessFlags::TRANSFER_WRITE);

            self.device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::TOP_OF_PIPE,
                vk::PipelineStageFlags::TRANSFER,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                std::slice::from_ref(&barrier_sc_to_dst),
            );

            // Blit framebuffer_image → swapchain_image (with scaling)
            let blit_region = vk::ImageBlit {
                src_subresource: subresource_layers,
                src_offsets: [
                    vk::Offset3D { x: 0, y: 0, z: 0 },
                    vk::Offset3D {
                        x: width as i32,
                        y: height as i32,
                        z: 1,
                    },
                ],
                dst_subresource: subresource_layers,
                dst_offsets: [
                    vk::Offset3D { x: 0, y: 0, z: 0 },
                    vk::Offset3D {
                        x: self.swapchain_extent.width as i32,
                        y: self.swapchain_extent.height as i32,
                        z: 1,
                    },
                ],
            };

            self.device.cmd_blit_image(
                cmd,
                self.framebuffer_image,
                vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
                swapchain_image,
                vk::ImageLayout::TRANSFER_DST_OPTIMAL,
                &[blit_region],
                vk::Filter::LINEAR,
            );

            // Barrier: swapchain_image TRANSFER_DST → PRESENT_SRC_KHR
            let barrier_sc_to_present = vk::ImageMemoryBarrier::builder()
                .old_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
                .new_layout(vk::ImageLayout::PRESENT_SRC_KHR)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(swapchain_image)
                .subresource_range(subresource_range)
                .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                .dst_access_mask(vk::AccessFlags::empty());

            self.device.cmd_pipeline_barrier(
                cmd,
                vk::PipelineStageFlags::TRANSFER,
                vk::PipelineStageFlags::BOTTOM_OF_PIPE,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                std::slice::from_ref(&barrier_sc_to_present),
            );

            self.device
                .end_command_buffer(cmd)
                .map_err(|e| VulkanError::PresentFailed(format!("End cmd: {}", e)))?;

            // Submit
            let wait_semaphores = [self.image_available_semaphore];
            let wait_stages = [vk::PipelineStageFlags::TRANSFER];
            let signal_semaphores = [self.render_finished_semaphore];
            let cmd_buffers = [cmd];

            let submit_info = vk::SubmitInfo::builder()
                .wait_semaphores(&wait_semaphores)
                .wait_dst_stage_mask(&wait_stages)
                .command_buffers(&cmd_buffers)
                .signal_semaphores(&signal_semaphores);

            self.device
                .queue_submit(
                    self.graphics_queue,
                    std::slice::from_ref(&submit_info),
                    self.in_flight_fence,
                )
                .map_err(|e| VulkanError::PresentFailed(format!("Submit: {}", e)))?;

            // Present
            let swapchains = [self.swapchain];
            let image_indices = [image_index];
            let present_info = vk::PresentInfoKHR::builder()
                .wait_semaphores(&signal_semaphores)
                .swapchains(&swapchains)
                .image_indices(&image_indices);

            match self
                .swapchain_loader
                .queue_present(self.graphics_queue, &present_info)
            {
                Ok(false) => Ok(()),
                Ok(true) | Err(vk::Result::ERROR_OUT_OF_DATE_KHR) => {
                    self.recreate_swapchain(
                        self.swapchain_extent.width,
                        self.swapchain_extent.height,
                    )?;
                    Ok(())
                }
                Err(e) => Err(VulkanError::PresentFailed(format!("Present: {}", e))),
            }
        }
    }

    /// Recreate the swapchain (e.g. after window resize).
    pub fn resize(&mut self, width: u32, height: u32) -> Result<(), VulkanError> {
        self.recreate_swapchain(width, height)
    }

    fn recreate_swapchain(&mut self, width: u32, height: u32) -> Result<(), VulkanError> {
        unsafe {
            self.device
                .device_wait_idle()
                .map_err(|e| VulkanError::SwapchainCreation(format!("Wait idle: {}", e)))?;

            let old_swapchain = self.swapchain;
            let fallback_extent = vk::Extent2D { width, height };

            let (new_swapchain, new_images, new_format, new_extent) = Self::create_swapchain(
                &self.surface_loader,
                &self.swapchain_loader,
                self.physical_device,
                self.surface,
                old_swapchain,
                fallback_extent,
            )?;

            // Destroy old swapchain
            self.swapchain_loader
                .destroy_swapchain(old_swapchain, None);

            self.swapchain = new_swapchain;
            self.swapchain_images = new_images;
            self.swapchain_format = new_format;
            self.swapchain_extent = new_extent;

            // Reallocate command buffers if image count changed
            if self.command_buffers.len() != self.swapchain_images.len() {
                self.device
                    .free_command_buffers(self.command_pool, &self.command_buffers);

                let alloc_info = vk::CommandBufferAllocateInfo::builder()
                    .command_pool(self.command_pool)
                    .level(vk::CommandBufferLevel::PRIMARY)
                    .command_buffer_count(self.swapchain_images.len() as u32);

                self.command_buffers = self
                    .device
                    .allocate_command_buffers(&alloc_info)
                    .map_err(|e| {
                        VulkanError::SwapchainCreation(format!("Reallocate cmd bufs: {}", e))
                    })?;
            }

            info!(
                "Swapchain recreated: {:?}, {}x{}",
                self.swapchain_format, self.swapchain_extent.width, self.swapchain_extent.height
            );
        }

        Ok(())
    }

    /// Ensure the staging buffer is large enough. Creates or recreates as needed.
    fn ensure_staging_buffer(&mut self, required_size: u64) -> Result<(), VulkanError> {
        if self.staging_buffer != vk::Buffer::null() && self.staging_size >= required_size {
            return Ok(());
        }

        // Destroy old resources
        self.destroy_staging_resources();

        unsafe {
            let buffer_info = vk::BufferCreateInfo::builder()
                .size(required_size)
                .usage(vk::BufferUsageFlags::TRANSFER_SRC)
                .sharing_mode(vk::SharingMode::EXCLUSIVE);

            let buffer = self
                .device
                .create_buffer(&buffer_info, None)
                .map_err(|e| VulkanError::PresentFailed(format!("Create staging buffer: {}", e)))?;

            let mem_reqs = self.device.get_buffer_memory_requirements(buffer);
            let mem_type = find_memory_type(
                &self.instance,
                self.physical_device,
                mem_reqs.memory_type_bits,
                vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT,
            )
            .ok_or_else(|| {
                self.device.destroy_buffer(buffer, None);
                VulkanError::PresentFailed("No suitable memory type for staging buffer".to_string())
            })?;

            let alloc_info = vk::MemoryAllocateInfo::builder()
                .allocation_size(mem_reqs.size)
                .memory_type_index(mem_type);

            let memory = self.device.allocate_memory(&alloc_info, None).map_err(|e| {
                self.device.destroy_buffer(buffer, None);
                VulkanError::PresentFailed(format!("Allocate staging memory: {}", e))
            })?;

            self.device
                .bind_buffer_memory(buffer, memory, 0)
                .map_err(|e| {
                    self.device.free_memory(memory, None);
                    self.device.destroy_buffer(buffer, None);
                    VulkanError::PresentFailed(format!("Bind staging memory: {}", e))
                })?;

            // Persistently map
            let mapped = self
                .device
                .map_memory(memory, 0, required_size, vk::MemoryMapFlags::empty())
                .map_err(|e| {
                    self.device.free_memory(memory, None);
                    self.device.destroy_buffer(buffer, None);
                    VulkanError::PresentFailed(format!("Map staging memory: {}", e))
                })?;

            self.staging_buffer = buffer;
            self.staging_memory = memory;
            self.staging_size = required_size;
            self.staging_mapped = mapped as *mut u8;
        }

        Ok(())
    }

    /// Ensure the intermediate framebuffer image matches the given dimensions.
    fn ensure_framebuffer_image(
        &mut self,
        width: u32,
        height: u32,
    ) -> Result<(), VulkanError> {
        if self.framebuffer_image != vk::Image::null()
            && self.fb_width == width
            && self.fb_height == height
        {
            return Ok(());
        }

        self.destroy_framebuffer_image();

        unsafe {
            let image_info = vk::ImageCreateInfo::builder()
                .image_type(vk::ImageType::TYPE_2D)
                .format(vk::Format::B8G8R8A8_UNORM)
                .extent(vk::Extent3D {
                    width,
                    height,
                    depth: 1,
                })
                .mip_levels(1)
                .array_layers(1)
                .samples(vk::SampleCountFlags::TYPE_1)
                .tiling(vk::ImageTiling::OPTIMAL)
                .usage(vk::ImageUsageFlags::TRANSFER_SRC | vk::ImageUsageFlags::TRANSFER_DST)
                .sharing_mode(vk::SharingMode::EXCLUSIVE)
                .initial_layout(vk::ImageLayout::UNDEFINED);

            let image = self.device.create_image(&image_info, None).map_err(|e| {
                VulkanError::PresentFailed(format!("Create framebuffer image: {}", e))
            })?;

            let mem_reqs = self.device.get_image_memory_requirements(image);
            let mem_type = find_memory_type(
                &self.instance,
                self.physical_device,
                mem_reqs.memory_type_bits,
                vk::MemoryPropertyFlags::DEVICE_LOCAL,
            )
            .ok_or_else(|| {
                self.device.destroy_image(image, None);
                VulkanError::PresentFailed(
                    "No suitable memory type for framebuffer image".to_string(),
                )
            })?;

            let alloc_info = vk::MemoryAllocateInfo::builder()
                .allocation_size(mem_reqs.size)
                .memory_type_index(mem_type);

            let memory = self.device.allocate_memory(&alloc_info, None).map_err(|e| {
                self.device.destroy_image(image, None);
                VulkanError::PresentFailed(format!("Allocate framebuffer memory: {}", e))
            })?;

            self.device
                .bind_image_memory(image, memory, 0)
                .map_err(|e| {
                    self.device.free_memory(memory, None);
                    self.device.destroy_image(image, None);
                    VulkanError::PresentFailed(format!("Bind framebuffer memory: {}", e))
                })?;

            self.framebuffer_image = image;
            self.framebuffer_memory = memory;
            self.fb_width = width;
            self.fb_height = height;
        }

        Ok(())
    }

    /// Destroy staging buffer and memory if they exist.
    fn destroy_staging_resources(&mut self) {
        unsafe {
            if self.staging_buffer != vk::Buffer::null() {
                self.device.unmap_memory(self.staging_memory);
                self.device.destroy_buffer(self.staging_buffer, None);
                self.device.free_memory(self.staging_memory, None);
                self.staging_buffer = vk::Buffer::null();
                self.staging_memory = vk::DeviceMemory::null();
                self.staging_size = 0;
                self.staging_mapped = ptr::null_mut();
            }
        }
    }

    /// Destroy framebuffer image and memory if they exist.
    fn destroy_framebuffer_image(&mut self) {
        unsafe {
            if self.framebuffer_image != vk::Image::null() {
                self.device.destroy_image(self.framebuffer_image, None);
                self.device.free_memory(self.framebuffer_memory, None);
                self.framebuffer_image = vk::Image::null();
                self.framebuffer_memory = vk::DeviceMemory::null();
                self.fb_width = 0;
                self.fb_height = 0;
            }
        }
    }

    unsafe fn select_physical_device(
        instance: &ash::Instance,
        surface_loader: &ash::extensions::khr::Surface,
        surface: vk::SurfaceKHR,
    ) -> Result<(vk::PhysicalDevice, u32), VulkanError> {
        let devices = instance
            .enumerate_physical_devices()
            .map_err(|_| VulkanError::NoSuitableDevice)?;

        if devices.is_empty() {
            return Err(VulkanError::NoSuitableDevice);
        }

        let mut best: Option<(vk::PhysicalDevice, u32, i32)> = None;

        for &pd in &devices {
            let props = instance.get_physical_device_properties(pd);
            let queue_families = instance.get_physical_device_queue_family_properties(pd);

            for (i, qf) in queue_families.iter().enumerate() {
                let i = i as u32;
                let has_graphics = qf.queue_flags.contains(vk::QueueFlags::GRAPHICS);
                let has_present = surface_loader
                    .get_physical_device_surface_support(pd, i, surface)
                    .unwrap_or(false);

                if has_graphics && has_present {
                    let score = match props.device_type {
                        vk::PhysicalDeviceType::DISCRETE_GPU => 100,
                        vk::PhysicalDeviceType::INTEGRATED_GPU => 50,
                        vk::PhysicalDeviceType::VIRTUAL_GPU => 25,
                        _ => 10,
                    };

                    if best.as_ref().map_or(true, |b| score > b.2) {
                        best = Some((pd, i, score));
                    }
                    break;
                }
            }
        }

        best.map(|(pd, qf, _)| (pd, qf))
            .ok_or(VulkanError::NoSuitableDevice)
    }

    unsafe fn create_swapchain(
        surface_loader: &ash::extensions::khr::Surface,
        swapchain_loader: &ash::extensions::khr::Swapchain,
        physical_device: vk::PhysicalDevice,
        surface: vk::SurfaceKHR,
        old_swapchain: vk::SwapchainKHR,
        fallback_extent: vk::Extent2D,
    ) -> Result<(vk::SwapchainKHR, Vec<vk::Image>, vk::Format, vk::Extent2D), VulkanError> {
        let caps = surface_loader
            .get_physical_device_surface_capabilities(physical_device, surface)
            .map_err(|e| VulkanError::SwapchainCreation(format!("Surface caps: {}", e)))?;

        let formats = surface_loader
            .get_physical_device_surface_formats(physical_device, surface)
            .map_err(|e| VulkanError::SwapchainCreation(format!("Surface formats: {}", e)))?;

        // Prefer B8G8R8A8_SRGB, then B8G8R8A8_UNORM, then first available
        let format = formats
            .iter()
            .find(|f| {
                f.format == vk::Format::B8G8R8A8_SRGB
                    && f.color_space == vk::ColorSpaceKHR::SRGB_NONLINEAR
            })
            .or_else(|| {
                formats
                    .iter()
                    .find(|f| f.format == vk::Format::B8G8R8A8_UNORM)
            })
            .unwrap_or(&formats[0]);

        // Determine swapchain extent
        let extent = if caps.current_extent.width != u32::MAX {
            caps.current_extent
        } else {
            vk::Extent2D {
                width: fallback_extent
                    .width
                    .clamp(caps.min_image_extent.width, caps.max_image_extent.width),
                height: fallback_extent
                    .height
                    .clamp(caps.min_image_extent.height, caps.max_image_extent.height),
            }
        };

        // Image count: min+1, capped by max (0 = unlimited)
        let image_count = {
            let desired = caps.min_image_count + 1;
            if caps.max_image_count > 0 {
                desired.min(caps.max_image_count)
            } else {
                desired
            }
        };

        let swapchain_info = vk::SwapchainCreateInfoKHR::builder()
            .surface(surface)
            .min_image_count(image_count)
            .image_format(format.format)
            .image_color_space(format.color_space)
            .image_extent(extent)
            .image_array_layers(1)
            .image_usage(vk::ImageUsageFlags::COLOR_ATTACHMENT | vk::ImageUsageFlags::TRANSFER_DST)
            .image_sharing_mode(vk::SharingMode::EXCLUSIVE)
            .pre_transform(caps.current_transform)
            .composite_alpha(vk::CompositeAlphaFlagsKHR::OPAQUE)
            .present_mode(vk::PresentModeKHR::FIFO)
            .clipped(true)
            .old_swapchain(old_swapchain);

        let swapchain = swapchain_loader
            .create_swapchain(&swapchain_info, None)
            .map_err(|e| VulkanError::SwapchainCreation(format!("Create swapchain: {}", e)))?;

        let images = swapchain_loader
            .get_swapchain_images(swapchain)
            .map_err(|e| VulkanError::SwapchainCreation(format!("Get images: {}", e)))?;

        Ok((swapchain, images, format.format, extent))
    }
}

impl Drop for VulkanPresenter {
    fn drop(&mut self) {
        unsafe {
            let _ = self.device.device_wait_idle();

            // Destroy Phase 23 resources first
            self.destroy_staging_resources();
            self.destroy_framebuffer_image();

            self.device.destroy_fence(self.in_flight_fence, None);
            self.device
                .destroy_semaphore(self.render_finished_semaphore, None);
            self.device
                .destroy_semaphore(self.image_available_semaphore, None);
            self.device
                .destroy_command_pool(self.command_pool, None);
            self.swapchain_loader
                .destroy_swapchain(self.swapchain, None);
            self.device.destroy_device(None);
            self.surface_loader.destroy_surface(self.surface, None);
            self.instance.destroy_instance(None);
        }

        info!("Vulkan presenter destroyed");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vulkan_error_display() {
        let err = VulkanError::NotAvailable("test reason".to_string());
        assert_eq!(err.to_string(), "Vulkan not available: test reason");

        let err = VulkanError::NoSuitableDevice;
        assert_eq!(err.to_string(), "No suitable GPU found");

        let err = VulkanError::SurfaceCreation("surface error".to_string());
        assert_eq!(err.to_string(), "Surface creation failed: surface error");

        let err = VulkanError::SwapchainCreation("swapchain error".to_string());
        assert_eq!(
            err.to_string(),
            "Swapchain creation failed: swapchain error"
        );

        let err = VulkanError::PresentFailed("present error".to_string());
        assert_eq!(err.to_string(), "Present failed: present error");
    }

    #[test]
    fn test_entry_load() {
        // ash::Entry::load() may fail on CI without Vulkan drivers — that's OK.
        let result = unsafe { ash::Entry::load() };
        match result {
            Ok(_) => log::info!("Vulkan entry loaded successfully"),
            Err(e) => log::info!("Vulkan not available (expected on CI): {}", e),
        }
    }

    #[test]
    fn test_swizzle_rgba_to_bgra() {
        // RGBA -> BGRA: R↔B swap
        let src = [255, 0, 128, 200, 10, 20, 30, 40];
        let mut dst = [0u8; 8];
        swizzle_rgba_to_bgra(&src, &mut dst);
        // Pixel 0: R=255,G=0,B=128,A=200 -> B=128,G=0,R=255,A=200
        assert_eq!(dst[0], 128); // B
        assert_eq!(dst[1], 0); // G
        assert_eq!(dst[2], 255); // R
        assert_eq!(dst[3], 200); // A
        // Pixel 1: R=10,G=20,B=30,A=40 -> B=30,G=20,R=10,A=40
        assert_eq!(dst[4], 30);
        assert_eq!(dst[5], 20);
        assert_eq!(dst[6], 10);
        assert_eq!(dst[7], 40);
    }

    #[test]
    fn test_swizzle_empty() {
        let src: &[u8] = &[];
        let mut dst: Vec<u8> = vec![];
        swizzle_rgba_to_bgra(src, &mut dst);
        assert!(dst.is_empty());
    }

    #[test]
    fn test_swizzle_single_pixel() {
        let src = [100, 150, 200, 255];
        let mut dst = [0u8; 4];
        swizzle_rgba_to_bgra(&src, &mut dst);
        assert_eq!(dst, [200, 150, 100, 255]);
    }
}
