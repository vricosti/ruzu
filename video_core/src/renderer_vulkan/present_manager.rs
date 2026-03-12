// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_present_manager.h` / `vk_present_manager.cpp`.
//!
//! Manages presentation frames, a present thread, and swapchain copies.

use std::collections::VecDeque;
use std::sync::{Condvar, Mutex};

use ash::vk;

// ---------------------------------------------------------------------------
// Helper functions (port of anonymous namespace)
// ---------------------------------------------------------------------------

/// Port of `MakeImageSubresourceLayers`.
fn make_image_subresource_layers() -> vk::ImageSubresourceLayers {
    vk::ImageSubresourceLayers {
        aspect_mask: vk::ImageAspectFlags::COLOR,
        mip_level: 0,
        base_array_layer: 0,
        layer_count: 1,
    }
}

/// Port of `MakeImageBlit`.
fn make_image_blit(
    frame_width: i32,
    frame_height: i32,
    swapchain_width: i32,
    swapchain_height: i32,
) -> vk::ImageBlit {
    vk::ImageBlit {
        src_subresource: make_image_subresource_layers(),
        src_offsets: [
            vk::Offset3D { x: 0, y: 0, z: 0 },
            vk::Offset3D {
                x: frame_width,
                y: frame_height,
                z: 1,
            },
        ],
        dst_subresource: make_image_subresource_layers(),
        dst_offsets: [
            vk::Offset3D { x: 0, y: 0, z: 0 },
            vk::Offset3D {
                x: swapchain_width,
                y: swapchain_height,
                z: 1,
            },
        ],
    }
}

/// Port of `MakeImageCopy`.
fn make_image_copy(
    frame_width: u32,
    frame_height: u32,
    swapchain_width: u32,
    swapchain_height: u32,
) -> vk::ImageCopy {
    vk::ImageCopy {
        src_subresource: make_image_subresource_layers(),
        src_offset: vk::Offset3D { x: 0, y: 0, z: 0 },
        dst_subresource: make_image_subresource_layers(),
        dst_offset: vk::Offset3D { x: 0, y: 0, z: 0 },
        extent: vk::Extent3D {
            width: frame_width.min(swapchain_width),
            height: frame_height.min(swapchain_height),
            depth: 1,
        },
    }
}

// ---------------------------------------------------------------------------
// Frame
// ---------------------------------------------------------------------------

/// Port of `Frame` struct.
///
/// A single presentation frame with its image, views, and synchronization
/// primitives.
pub struct Frame {
    pub width: u32,
    pub height: u32,
    pub image: vk::Image,
    pub image_view: vk::ImageView,
    pub framebuffer: vk::Framebuffer,
    pub cmdbuf: vk::CommandBuffer,
    pub render_ready: vk::Semaphore,
    pub present_done: vk::Fence,
}

impl Default for Frame {
    fn default() -> Self {
        Frame {
            width: 0,
            height: 0,
            image: vk::Image::null(),
            image_view: vk::ImageView::null(),
            framebuffer: vk::Framebuffer::null(),
            cmdbuf: vk::CommandBuffer::null(),
            render_ready: vk::Semaphore::null(),
            present_done: vk::Fence::null(),
        }
    }
}

// ---------------------------------------------------------------------------
// PresentManager
// ---------------------------------------------------------------------------

/// Port of `PresentManager` class.
///
/// Manages a pool of `Frame` objects, a present queue, and an optional
/// present thread that copies rendered frames to the swapchain.
pub struct PresentManager {
    device: ash::Device,
    cmdpool: vk::CommandPool,
    frames: Vec<Frame>,
    present_queue: Mutex<VecDeque<usize>>,
    free_queue: Mutex<VecDeque<usize>>,
    frame_cv: Condvar,
    free_cv: Condvar,
    swapchain_mutex: Mutex<()>,
    blit_supported: bool,
    use_present_thread: bool,
    image_count: usize,
}

/// Maximum number of images in flight.
/// Upstream caps this at 7 (FRAMES_IN_FLIGHT=8, TICKS_TO_DESTROY=8).
const MAX_IMAGES_IN_FLIGHT: usize = 7;

impl PresentManager {
    /// Port of `PresentManager::PresentManager`.
    pub fn new(
        device: ash::Device,
        graphics_family: u32,
        image_count: usize,
        blit_supported: bool,
        use_present_thread: bool,
    ) -> Self {
        let effective_count = image_count.min(MAX_IMAGES_IN_FLIGHT);

        // Create command pool
        let pool_ci = vk::CommandPoolCreateInfo::builder()
            .flags(
                vk::CommandPoolCreateFlags::TRANSIENT
                    | vk::CommandPoolCreateFlags::RESET_COMMAND_BUFFER,
            )
            .queue_family_index(graphics_family)
            .build();
        let cmdpool = unsafe {
            device
                .create_command_pool(&pool_ci, None)
                .expect("Failed to create present command pool")
        };

        // Allocate command buffers
        let alloc_info = vk::CommandBufferAllocateInfo::builder()
            .command_pool(cmdpool)
            .level(vk::CommandBufferLevel::PRIMARY)
            .command_buffer_count(effective_count as u32)
            .build();
        let cmdbufs = unsafe {
            device
                .allocate_command_buffers(&alloc_info)
                .expect("Failed to allocate present command buffers")
        };

        // Create frames
        let mut frames = Vec::with_capacity(effective_count);
        let mut free_queue = VecDeque::with_capacity(effective_count);

        let semaphore_ci = vk::SemaphoreCreateInfo::builder().build();
        let fence_ci = vk::FenceCreateInfo::builder()
            .flags(vk::FenceCreateFlags::SIGNALED)
            .build();

        for i in 0..effective_count {
            let render_ready = unsafe {
                device
                    .create_semaphore(&semaphore_ci, None)
                    .expect("Failed to create render_ready semaphore")
            };
            let present_done = unsafe {
                device
                    .create_fence(&fence_ci, None)
                    .expect("Failed to create present_done fence")
            };
            frames.push(Frame {
                width: 0,
                height: 0,
                image: vk::Image::null(),
                image_view: vk::ImageView::null(),
                framebuffer: vk::Framebuffer::null(),
                cmdbuf: cmdbufs[i],
                render_ready,
                present_done,
            });
            free_queue.push_back(i);
        }

        // NOTE: present thread would be started here in the full implementation
        // using std::thread or similar. Omitted because it requires the full
        // scheduler integration.

        PresentManager {
            device,
            cmdpool,
            frames,
            present_queue: Mutex::new(VecDeque::new()),
            free_queue: Mutex::new(free_queue),
            frame_cv: Condvar::new(),
            free_cv: Condvar::new(),
            swapchain_mutex: Mutex::new(()),
            blit_supported,
            use_present_thread,
            image_count: effective_count,
        }
    }

    /// Port of `PresentManager::GetRenderFrame`.
    ///
    /// Blocks until a free presentation frame is available, then returns it.
    pub fn get_render_frame(&mut self) -> &mut Frame {
        let index = {
            let mut free = self.free_queue.lock().unwrap();
            while free.is_empty() {
                free = self.free_cv.wait(free).unwrap();
            }
            free.pop_front().unwrap()
        };

        // Wait for the presentation to be finished
        let frame = &self.frames[index];
        if frame.present_done != vk::Fence::null() {
            unsafe {
                self.device
                    .wait_for_fences(&[frame.present_done], true, u64::MAX)
                    .expect("Failed to wait for present_done fence");
                self.device
                    .reset_fences(&[frame.present_done])
                    .expect("Failed to reset present_done fence");
            }
        }

        &mut self.frames[index]
    }

    /// Port of `PresentManager::Present`.
    ///
    /// Queues a frame for presentation, or presents directly if no present
    /// thread is active.
    pub fn present(&self, frame_index: usize) {
        if !self.use_present_thread {
            // Direct present path (no threading)
            // In the full implementation, this calls CopyToSwapchain.
            let mut free = self.free_queue.lock().unwrap();
            free.push_back(frame_index);
            self.free_cv.notify_one();
            return;
        }

        let mut queue = self.present_queue.lock().unwrap();
        queue.push_back(frame_index);
        self.frame_cv.notify_one();
    }

    /// Port of `PresentManager::RecreateFrame`.
    ///
    /// Recreates the frame's image, image view, and framebuffer to match
    /// the given dimensions and format.
    pub fn recreate_frame(
        &self,
        frame: &mut Frame,
        width: u32,
        height: u32,
        image_view_format: vk::Format,
        render_pass: vk::RenderPass,
    ) {
        frame.width = width;
        frame.height = height;

        // Create image
        let image_ci = vk::ImageCreateInfo::builder()
            .flags(vk::ImageCreateFlags::MUTABLE_FORMAT)
            .image_type(vk::ImageType::TYPE_2D)
            .format(vk::Format::B8G8R8A8_UNORM) // swapchain format
            .extent(vk::Extent3D {
                width,
                height,
                depth: 1,
            })
            .mip_levels(1)
            .array_layers(1)
            .samples(vk::SampleCountFlags::TYPE_1)
            .tiling(vk::ImageTiling::OPTIMAL)
            .usage(vk::ImageUsageFlags::TRANSFER_SRC | vk::ImageUsageFlags::COLOR_ATTACHMENT)
            .sharing_mode(vk::SharingMode::EXCLUSIVE)
            .initial_layout(vk::ImageLayout::UNDEFINED)
            .build();

        frame.image = unsafe {
            self.device
                .create_image(&image_ci, None)
                .expect("Failed to create present frame image")
        };

        // Create image view
        let view_ci = vk::ImageViewCreateInfo::builder()
            .image(frame.image)
            .view_type(vk::ImageViewType::TYPE_2D)
            .format(image_view_format)
            .components(vk::ComponentMapping::default())
            .subresource_range(vk::ImageSubresourceRange {
                aspect_mask: vk::ImageAspectFlags::COLOR,
                base_mip_level: 0,
                level_count: 1,
                base_array_layer: 0,
                layer_count: 1,
            })
            .build();

        frame.image_view = unsafe {
            self.device
                .create_image_view(&view_ci, None)
                .expect("Failed to create present frame image view")
        };

        // Create framebuffer
        let attachments = [frame.image_view];
        let fb_ci = vk::FramebufferCreateInfo::builder()
            .render_pass(render_pass)
            .attachments(&attachments)
            .width(width)
            .height(height)
            .layers(1)
            .build();

        frame.framebuffer = unsafe {
            self.device
                .create_framebuffer(&fb_ci, None)
                .expect("Failed to create present frame framebuffer")
        };
    }

    /// Port of `PresentManager::WaitPresent`.
    ///
    /// Blocks until all queued frames have been presented.
    pub fn wait_present(&self) {
        if !self.use_present_thread {
            return;
        }

        // Wait for the present queue to be empty
        {
            let mut queue = self.present_queue.lock().unwrap();
            while !queue.is_empty() {
                queue = self.frame_cv.wait(queue).unwrap();
            }
        }

        // Acquire swapchain mutex to ensure the last frame has been presented
        let _lock = self.swapchain_mutex.lock().unwrap();
    }

    /// Port of `PresentManager::CopyToSwapchainImpl`.
    ///
    /// Records and submits commands to copy a frame image to the swapchain
    /// image, using blit or copy depending on hardware support.
    pub fn copy_to_swapchain_impl(
        &self,
        frame: &Frame,
        swapchain_image: vk::Image,
        swapchain_extent: vk::Extent2D,
        present_semaphore: vk::Semaphore,
        render_semaphore: vk::Semaphore,
        graphics_queue: vk::Queue,
    ) {
        let cmdbuf = frame.cmdbuf;

        let begin_info = vk::CommandBufferBeginInfo::builder()
            .flags(vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT)
            .build();

        unsafe {
            self.device
                .begin_command_buffer(cmdbuf, &begin_info)
                .expect("Failed to begin present command buffer");
        }

        // Pre-barriers
        let pre_barriers = [
            // Swapchain image: UNDEFINED -> TRANSFER_DST_OPTIMAL
            vk::ImageMemoryBarrier {
                s_type: vk::StructureType::IMAGE_MEMORY_BARRIER,
                p_next: std::ptr::null(),
                src_access_mask: vk::AccessFlags::empty(),
                dst_access_mask: vk::AccessFlags::TRANSFER_WRITE,
                old_layout: vk::ImageLayout::UNDEFINED,
                new_layout: vk::ImageLayout::TRANSFER_DST_OPTIMAL,
                src_queue_family_index: vk::QUEUE_FAMILY_IGNORED,
                dst_queue_family_index: vk::QUEUE_FAMILY_IGNORED,
                image: swapchain_image,
                subresource_range: vk::ImageSubresourceRange {
                    aspect_mask: vk::ImageAspectFlags::COLOR,
                    base_mip_level: 0,
                    level_count: 1,
                    base_array_layer: 0,
                    layer_count: vk::REMAINING_ARRAY_LAYERS,
                },
            },
            // Frame image: GENERAL -> TRANSFER_SRC_OPTIMAL
            vk::ImageMemoryBarrier {
                s_type: vk::StructureType::IMAGE_MEMORY_BARRIER,
                p_next: std::ptr::null(),
                src_access_mask: vk::AccessFlags::COLOR_ATTACHMENT_WRITE,
                dst_access_mask: vk::AccessFlags::TRANSFER_READ,
                old_layout: vk::ImageLayout::GENERAL,
                new_layout: vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
                src_queue_family_index: vk::QUEUE_FAMILY_IGNORED,
                dst_queue_family_index: vk::QUEUE_FAMILY_IGNORED,
                image: frame.image,
                subresource_range: vk::ImageSubresourceRange {
                    aspect_mask: vk::ImageAspectFlags::COLOR,
                    base_mip_level: 0,
                    level_count: 1,
                    base_array_layer: 0,
                    layer_count: vk::REMAINING_ARRAY_LAYERS,
                },
            },
        ];

        // Post-barriers
        let post_barriers = [
            // Swapchain image: TRANSFER_DST_OPTIMAL -> PRESENT_SRC
            vk::ImageMemoryBarrier {
                s_type: vk::StructureType::IMAGE_MEMORY_BARRIER,
                p_next: std::ptr::null(),
                src_access_mask: vk::AccessFlags::TRANSFER_WRITE,
                dst_access_mask: vk::AccessFlags::MEMORY_READ,
                old_layout: vk::ImageLayout::TRANSFER_DST_OPTIMAL,
                new_layout: vk::ImageLayout::PRESENT_SRC_KHR,
                src_queue_family_index: vk::QUEUE_FAMILY_IGNORED,
                dst_queue_family_index: vk::QUEUE_FAMILY_IGNORED,
                image: swapchain_image,
                subresource_range: vk::ImageSubresourceRange {
                    aspect_mask: vk::ImageAspectFlags::COLOR,
                    base_mip_level: 0,
                    level_count: 1,
                    base_array_layer: 0,
                    layer_count: vk::REMAINING_ARRAY_LAYERS,
                },
            },
            // Frame image: TRANSFER_SRC_OPTIMAL -> GENERAL
            vk::ImageMemoryBarrier {
                s_type: vk::StructureType::IMAGE_MEMORY_BARRIER,
                p_next: std::ptr::null(),
                src_access_mask: vk::AccessFlags::TRANSFER_READ,
                dst_access_mask: vk::AccessFlags::MEMORY_WRITE,
                old_layout: vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
                new_layout: vk::ImageLayout::GENERAL,
                src_queue_family_index: vk::QUEUE_FAMILY_IGNORED,
                dst_queue_family_index: vk::QUEUE_FAMILY_IGNORED,
                image: frame.image,
                subresource_range: vk::ImageSubresourceRange {
                    aspect_mask: vk::ImageAspectFlags::COLOR,
                    base_mip_level: 0,
                    level_count: 1,
                    base_array_layer: 0,
                    layer_count: vk::REMAINING_ARRAY_LAYERS,
                },
            },
        ];

        unsafe {
            self.device.cmd_pipeline_barrier(
                cmdbuf,
                vk::PipelineStageFlags::ALL_COMMANDS,
                vk::PipelineStageFlags::TRANSFER,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &pre_barriers,
            );

            if self.blit_supported {
                let region = make_image_blit(
                    frame.width as i32,
                    frame.height as i32,
                    swapchain_extent.width as i32,
                    swapchain_extent.height as i32,
                );
                self.device.cmd_blit_image(
                    cmdbuf,
                    frame.image,
                    vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
                    swapchain_image,
                    vk::ImageLayout::TRANSFER_DST_OPTIMAL,
                    &[region],
                    vk::Filter::LINEAR,
                );
            } else {
                let region = make_image_copy(
                    frame.width,
                    frame.height,
                    swapchain_extent.width,
                    swapchain_extent.height,
                );
                self.device.cmd_copy_image(
                    cmdbuf,
                    frame.image,
                    vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
                    swapchain_image,
                    vk::ImageLayout::TRANSFER_DST_OPTIMAL,
                    &[region],
                );
            }

            self.device.cmd_pipeline_barrier(
                cmdbuf,
                vk::PipelineStageFlags::TRANSFER,
                vk::PipelineStageFlags::ALL_GRAPHICS,
                vk::DependencyFlags::empty(),
                &[],
                &[],
                &post_barriers,
            );

            self.device
                .end_command_buffer(cmdbuf)
                .expect("Failed to end present command buffer");
        }

        // Submit
        let wait_semaphores = [present_semaphore, frame.render_ready];
        let wait_stages = [
            vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
            vk::PipelineStageFlags::ALL_COMMANDS,
        ];
        let cmdbufs = [cmdbuf];
        let signal_semaphores = [render_semaphore];

        let submit_info = vk::SubmitInfo::builder()
            .wait_semaphores(&wait_semaphores)
            .wait_dst_stage_mask(&wait_stages)
            .command_buffers(&cmdbufs)
            .signal_semaphores(&signal_semaphores)
            .build();

        unsafe {
            self.device
                .queue_submit(graphics_queue, &[submit_info], frame.present_done)
                .expect("Failed to submit present commands");
        }
    }

    /// Port of `PresentManager::SetImageCount`.
    pub fn set_image_count(&mut self, swapchain_image_count: usize) {
        self.image_count = swapchain_image_count.min(MAX_IMAGES_IN_FLIGHT);
    }
}
