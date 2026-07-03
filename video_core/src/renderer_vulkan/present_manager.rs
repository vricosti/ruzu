// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_present_manager.h` / `vk_present_manager.cpp`.
//!
//! Manages presentation frames, a present thread, and swapchain copies.

use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use std::sync::{Arc, Condvar, Mutex};

use ash::vk;

use super::scheduler::Scheduler;
use super::swapchain::Swapchain;

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
    pub image_memory: vk::DeviceMemory,
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
            image_memory: vk::DeviceMemory::null(),
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
    memory_properties: vk::PhysicalDeviceMemoryProperties,
    frame_image_format: vk::Format,
    cmdpool: vk::CommandPool,
    frames: Vec<Frame>,
    present_queue: Mutex<VecDeque<usize>>,
    free_queue: Mutex<VecDeque<usize>>,
    frame_cv: Condvar,
    free_cv: Condvar,
    swapchain_mutex: Mutex<()>,
    submit_mutex: Arc<Mutex<()>>,
    blit_supported: bool,
    use_present_thread: bool,
    image_count: usize,
}

/// Maximum number of images in flight.
/// Upstream caps this at 7 (FRAMES_IN_FLIGHT=8, TICKS_TO_DESTROY=8).
const MAX_IMAGES_IN_FLIGHT: usize = 7;

static SWAPCHAIN_FRAME_DUMPED: AtomicBool = AtomicBool::new(false);
static SWAPCHAIN_FRAME_DUMP_COUNTER: AtomicU32 = AtomicU32::new(0);

impl PresentManager {
    /// Port of `PresentManager::PresentManager`.
    pub fn new(
        device: ash::Device,
        memory_properties: vk::PhysicalDeviceMemoryProperties,
        frame_image_format: vk::Format,
        graphics_family: u32,
        image_count: usize,
        blit_supported: bool,
        use_present_thread: bool,
        submit_mutex: Arc<Mutex<()>>,
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
                image_memory: vk::DeviceMemory::null(),
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
            memory_properties,
            frame_image_format,
            cmdpool,
            frames,
            present_queue: Mutex::new(VecDeque::new()),
            free_queue: Mutex::new(free_queue),
            frame_cv: Condvar::new(),
            free_cv: Condvar::new(),
            swapchain_mutex: Mutex::new(()),
            submit_mutex,
            blit_supported,
            use_present_thread,
            image_count: effective_count,
        }
    }

    /// Port of `PresentManager::GetRenderFrame`.
    ///
    /// Blocks until a free presentation frame is available, then returns it.
    pub fn get_render_frame(&mut self) -> &mut Frame {
        let index = self.get_render_frame_index();
        &mut self.frames[index]
    }

    /// Rust ownership helper for upstream `PresentManager::GetRenderFrame`.
    ///
    /// Upstream returns a `Frame*`; callers later pass the same pointer to
    /// `Present`. Rust needs the frame identity explicitly to avoid holding a
    /// mutable borrow across the whole present path.
    pub fn get_render_frame_index(&mut self) -> usize {
        let index = {
            let mut free = self.free_queue.lock().unwrap();
            while free.is_empty() {
                if std::env::var_os("RUZU_TRACE_PRESENT").is_some() {
                    log::info!("[PRESENT] PresentManager::GetRenderFrame waiting for free frame");
                }
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

        if std::env::var_os("RUZU_TRACE_PRESENT").is_some() {
            log::info!(
                "[PRESENT] PresentManager::GetRenderFrame index={} size={}x{}",
                index,
                frame.width,
                frame.height
            );
        }

        index
    }

    pub fn frame(&self, index: usize) -> &Frame {
        &self.frames[index]
    }

    pub fn frame_mut(&mut self, index: usize) -> &mut Frame {
        &mut self.frames[index]
    }

    pub fn release_frame(&self, index: usize) {
        let mut free = self.free_queue.lock().unwrap();
        free.push_back(index);
        self.free_cv.notify_one();
    }

    pub fn recreate_frame_by_index(
        &mut self,
        frame_index: usize,
        width: u32,
        height: u32,
        image_view_format: vk::Format,
        render_pass: vk::RenderPass,
    ) {
        let mut frame = std::mem::take(&mut self.frames[frame_index]);
        self.recreate_frame(&mut frame, width, height, image_view_format, render_pass);
        self.frames[frame_index] = frame;
    }

    /// Port of `PresentManager::Present`.
    ///
    /// Queues a frame for presentation, or presents directly if no present
    /// thread is active.
    pub fn present(
        &mut self,
        frame_index: usize,
        swapchain: &mut Swapchain,
        scheduler: &mut Scheduler,
        graphics_queue: vk::Queue,
    ) {
        let trace_present = std::env::var_os("RUZU_TRACE_PRESENT").is_some();
        if trace_present {
            let frame = &self.frames[frame_index];
            log::info!(
                "[PRESENT] PresentManager::Present frame_index={} size={}x{} threaded={}",
                frame_index,
                frame.width,
                frame.height,
                self.use_present_thread
            );
        }
        if !self.use_present_thread {
            self.copy_to_swapchain(frame_index, swapchain, scheduler, graphics_queue);
            self.release_frame(frame_index);
            if trace_present {
                log::info!(
                    "[PRESENT] PresentManager::Present direct complete frame_index={}",
                    frame_index
                );
            }
            return;
        }

        let mut queue = self.present_queue.lock().unwrap();
        queue.push_back(frame_index);
        self.frame_cv.notify_one();
    }

    /// Port-facing subset of `PresentManager::CopyToSwapchain`.
    fn copy_to_swapchain(
        &mut self,
        frame_index: usize,
        swapchain: &mut Swapchain,
        scheduler: &mut Scheduler,
        graphics_queue: vk::Queue,
    ) {
        let trace_present = std::env::var_os("RUZU_TRACE_PRESENT").is_some();
        let frame_width = self.frames[frame_index].width;
        let frame_height = self.frames[frame_index].height;
        let needs_recreation = swapchain.needs_recreation()
            || swapchain.get_width() != frame_width
            || swapchain.get_height() != frame_height;
        if trace_present {
            log::info!(
                "[PRESENT] PresentManager::CopyToSwapchain frame_index={} frame={}x{} swapchain={}x{} needs_recreation={}",
                frame_index,
                frame_width,
                frame_height,
                swapchain.get_width(),
                swapchain.get_height(),
                needs_recreation
            );
        }
        if needs_recreation && !self.recreate_swapchain(frame_index, swapchain) {
            return;
        }

        let mut recreate_attempts = 0;
        while swapchain.acquire_next_image(scheduler) {
            if trace_present {
                log::info!(
                    "[PRESENT] PresentManager::CopyToSwapchain acquire requested recreation attempt={}",
                    recreate_attempts + 1
                );
            }
            if !self.recreate_swapchain(frame_index, swapchain) {
                return;
            }
            recreate_attempts += 1;
            if recreate_attempts >= 8 {
                log::warn!("Vulkan swapchain acquisition remained stale after recreation");
                return;
            }
        }

        let swapchain_image = swapchain.current_image();
        let swapchain_extent = swapchain.get_extent();
        let present_semaphore = swapchain.current_present_semaphore();
        let render_semaphore = swapchain.current_render_semaphore();
        self.copy_to_swapchain_impl(
            self.frame(frame_index),
            swapchain_image,
            swapchain_extent,
            present_semaphore,
            render_semaphore,
            graphics_queue,
        );
        swapchain.present(render_semaphore);
        if trace_present {
            log::info!(
                "[PRESENT] PresentManager::CopyToSwapchain presented frame_index={} image_index={} frame_slot={}",
                frame_index,
                swapchain.get_image_index(),
                swapchain.get_frame_index()
            );
        }
    }

    /// Port-facing subset of `PresentManager::RecreateSwapchain`.
    fn recreate_swapchain(&mut self, frame_index: usize, swapchain: &mut Swapchain) -> bool {
        let frame_width = self.frames[frame_index].width;
        let frame_height = self.frames[frame_index].height;
        match swapchain.recreate(frame_width, frame_height) {
            Ok(()) => {
                self.set_image_count(swapchain.get_image_count());
                true
            }
            Err(err) => {
                log::error!("Failed to recreate Vulkan swapchain: {}", err);
                false
            }
        }
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
        self.destroy_frame_resources(frame);

        frame.width = width;
        frame.height = height;

        // Create image
        let image_ci = vk::ImageCreateInfo::builder()
            .flags(vk::ImageCreateFlags::MUTABLE_FORMAT)
            .image_type(vk::ImageType::TYPE_2D)
            .format(self.frame_image_format)
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
        let memory_requirements = unsafe { self.device.get_image_memory_requirements(frame.image) };
        let memory_type_index = find_memory_type(
            &self.memory_properties,
            memory_requirements.memory_type_bits,
            vk::MemoryPropertyFlags::DEVICE_LOCAL,
        )
        .expect("Failed to find present frame memory type");
        let allocate_info = vk::MemoryAllocateInfo::builder()
            .allocation_size(memory_requirements.size)
            .memory_type_index(memory_type_index)
            .build();
        frame.image_memory = unsafe {
            self.device
                .allocate_memory(&allocate_info, None)
                .expect("Failed to allocate present frame image memory")
        };
        unsafe {
            self.device
                .bind_image_memory(frame.image, frame.image_memory, 0)
                .expect("Failed to bind present frame image memory");
        }

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
        let swapchain_dump = self.create_swapchain_dump_buffer(swapchain_extent);

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

            if let Some(dump) = &swapchain_dump {
                let swapchain_to_transfer_src = vk::ImageMemoryBarrier {
                    s_type: vk::StructureType::IMAGE_MEMORY_BARRIER,
                    p_next: std::ptr::null(),
                    src_access_mask: vk::AccessFlags::TRANSFER_WRITE,
                    dst_access_mask: vk::AccessFlags::TRANSFER_READ,
                    old_layout: vk::ImageLayout::TRANSFER_DST_OPTIMAL,
                    new_layout: vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
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
                };
                self.device.cmd_pipeline_barrier(
                    cmdbuf,
                    vk::PipelineStageFlags::TRANSFER,
                    vk::PipelineStageFlags::TRANSFER,
                    vk::DependencyFlags::empty(),
                    &[],
                    &[],
                    &[swapchain_to_transfer_src],
                );
                let region = vk::BufferImageCopy {
                    buffer_offset: 0,
                    buffer_row_length: 0,
                    buffer_image_height: 0,
                    image_subresource: make_image_subresource_layers(),
                    image_offset: vk::Offset3D { x: 0, y: 0, z: 0 },
                    image_extent: vk::Extent3D {
                        width: swapchain_extent.width,
                        height: swapchain_extent.height,
                        depth: 1,
                    },
                };
                self.device.cmd_copy_image_to_buffer(
                    cmdbuf,
                    swapchain_image,
                    vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
                    dump.buffer,
                    &[region],
                );
                let swapchain_to_transfer_dst = vk::ImageMemoryBarrier {
                    old_layout: vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
                    new_layout: vk::ImageLayout::TRANSFER_DST_OPTIMAL,
                    src_access_mask: vk::AccessFlags::TRANSFER_READ,
                    dst_access_mask: vk::AccessFlags::TRANSFER_WRITE,
                    ..swapchain_to_transfer_src
                };
                self.device.cmd_pipeline_barrier(
                    cmdbuf,
                    vk::PipelineStageFlags::TRANSFER,
                    vk::PipelineStageFlags::TRANSFER,
                    vk::DependencyFlags::empty(),
                    &[],
                    &[],
                    &[swapchain_to_transfer_dst],
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
            let _submit_lock = self.submit_mutex.lock().unwrap();
            self.device
                .queue_submit(graphics_queue, &[submit_info], frame.present_done)
                .expect("Failed to submit present commands");
        }

        if let Some(dump) = swapchain_dump {
            self.finish_swapchain_dump(dump, frame.present_done, swapchain_extent);
        }
    }

    /// Port of `PresentManager::SetImageCount`.
    pub fn set_image_count(&mut self, swapchain_image_count: usize) {
        self.image_count = swapchain_image_count.min(MAX_IMAGES_IN_FLIGHT);
    }

    fn destroy_frame_resources(&self, frame: &mut Frame) {
        unsafe {
            if frame.framebuffer != vk::Framebuffer::null() {
                self.device.destroy_framebuffer(frame.framebuffer, None);
                frame.framebuffer = vk::Framebuffer::null();
            }
            if frame.image_view != vk::ImageView::null() {
                self.device.destroy_image_view(frame.image_view, None);
                frame.image_view = vk::ImageView::null();
            }
            if frame.image != vk::Image::null() {
                self.device.destroy_image(frame.image, None);
                frame.image = vk::Image::null();
            }
            if frame.image_memory != vk::DeviceMemory::null() {
                self.device.free_memory(frame.image_memory, None);
                frame.image_memory = vk::DeviceMemory::null();
            }
        }
        frame.width = 0;
        frame.height = 0;
    }

    fn create_swapchain_dump_buffer(&self, extent: vk::Extent2D) -> Option<SwapchainDumpBuffer> {
        let Some(path) = std::env::var_os("RUZU_DUMP_SWAPCHAIN_FRAME") else {
            return None;
        };
        let current_frame = SWAPCHAIN_FRAME_DUMP_COUNTER.fetch_add(1, Ordering::Relaxed) + 1;
        let target_frame = std::env::var("RUZU_DUMP_SWAPCHAIN_FRAME_AT")
            .ok()
            .and_then(|value| value.parse::<u32>().ok())
            .unwrap_or(300);
        if current_frame < target_frame {
            return None;
        }
        if SWAPCHAIN_FRAME_DUMPED
            .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
            .is_err()
        {
            return None;
        }

        let size = extent.width as vk::DeviceSize * extent.height as vk::DeviceSize * 4;
        if size == 0 {
            log::error!("[PRESENT] failed to dump swapchain frame: zero-sized extent");
            return None;
        }
        let buffer_info = vk::BufferCreateInfo::builder()
            .size(size)
            .usage(vk::BufferUsageFlags::TRANSFER_DST)
            .sharing_mode(vk::SharingMode::EXCLUSIVE)
            .build();
        let buffer = match unsafe { self.device.create_buffer(&buffer_info, None) } {
            Ok(buffer) => buffer,
            Err(err) => {
                log::error!("[PRESENT] failed to create swapchain dump buffer: {:?}", err);
                return None;
            }
        };
        let requirements = unsafe { self.device.get_buffer_memory_requirements(buffer) };
        let Some(memory_type_index) = find_memory_type(
            &self.memory_properties,
            requirements.memory_type_bits,
            vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT,
        ) else {
            unsafe {
                self.device.destroy_buffer(buffer, None);
            }
            log::error!("[PRESENT] failed to find host-coherent memory for swapchain dump");
            return None;
        };
        let alloc_info = vk::MemoryAllocateInfo::builder()
            .allocation_size(requirements.size)
            .memory_type_index(memory_type_index)
            .build();
        let memory = match unsafe { self.device.allocate_memory(&alloc_info, None) } {
            Ok(memory) => memory,
            Err(err) => {
                unsafe {
                    self.device.destroy_buffer(buffer, None);
                }
                log::error!("[PRESENT] failed to allocate swapchain dump memory: {:?}", err);
                return None;
            }
        };
        if let Err(err) = unsafe { self.device.bind_buffer_memory(buffer, memory, 0) } {
            unsafe {
                self.device.destroy_buffer(buffer, None);
                self.device.free_memory(memory, None);
            }
            log::error!("[PRESENT] failed to bind swapchain dump memory: {:?}", err);
            return None;
        }

        Some(SwapchainDumpBuffer {
            path: std::path::PathBuf::from(path),
            buffer,
            memory,
            size,
        })
    }

    fn finish_swapchain_dump(
        &self,
        dump: SwapchainDumpBuffer,
        present_done: vk::Fence,
        extent: vk::Extent2D,
    ) {
        let result = unsafe {
            self.device
                .wait_for_fences(&[present_done], true, u64::MAX)
        };
        if let Err(err) = result {
            log::error!("[PRESENT] failed waiting for swapchain dump fence: {:?}", err);
            self.destroy_swapchain_dump_buffer(dump);
            return;
        }

        let mapped = match unsafe {
            self.device
                .map_memory(dump.memory, 0, dump.size, vk::MemoryMapFlags::empty())
        } {
            Ok(mapped) => mapped.cast::<u8>(),
            Err(err) => {
                log::error!("[PRESENT] failed to map swapchain dump memory: {:?}", err);
                self.destroy_swapchain_dump_buffer(dump);
                return;
            }
        };
        let slice = unsafe { std::slice::from_raw_parts(mapped, dump.size as usize) };
        let write_result = write_bgra_ppm(&dump.path, slice, extent.width, extent.height);
        unsafe {
            self.device.unmap_memory(dump.memory);
        }
        match write_result {
            Ok(()) => log::info!(
                "[PRESENT] dumped swapchain frame to {}",
                dump.path.display()
            ),
            Err(err) => log::error!(
                "[PRESENT] failed to write swapchain dump {}: {}",
                dump.path.display(),
                err
            ),
        }
        self.destroy_swapchain_dump_buffer(dump);
    }

    fn destroy_swapchain_dump_buffer(&self, dump: SwapchainDumpBuffer) {
        unsafe {
            self.device.destroy_buffer(dump.buffer, None);
            self.device.free_memory(dump.memory, None);
        }
    }
}

struct SwapchainDumpBuffer {
    path: std::path::PathBuf,
    buffer: vk::Buffer,
    memory: vk::DeviceMemory,
    size: vk::DeviceSize,
}

impl Drop for PresentManager {
    fn drop(&mut self) {
        unsafe {
            let device = self.device.clone();
            for frame in &mut self.frames {
                if frame.framebuffer != vk::Framebuffer::null() {
                    device.destroy_framebuffer(frame.framebuffer, None);
                    frame.framebuffer = vk::Framebuffer::null();
                }
                if frame.image_view != vk::ImageView::null() {
                    device.destroy_image_view(frame.image_view, None);
                    frame.image_view = vk::ImageView::null();
                }
                if frame.image != vk::Image::null() {
                    device.destroy_image(frame.image, None);
                    frame.image = vk::Image::null();
                }
                if frame.image_memory != vk::DeviceMemory::null() {
                    device.free_memory(frame.image_memory, None);
                    frame.image_memory = vk::DeviceMemory::null();
                }
                if frame.render_ready != vk::Semaphore::null() {
                    device.destroy_semaphore(frame.render_ready, None);
                    frame.render_ready = vk::Semaphore::null();
                }
                if frame.present_done != vk::Fence::null() {
                    device.destroy_fence(frame.present_done, None);
                    frame.present_done = vk::Fence::null();
                }
            }
            if self.cmdpool != vk::CommandPool::null() {
                device.destroy_command_pool(self.cmdpool, None);
                self.cmdpool = vk::CommandPool::null();
            }
        }
    }
}

fn find_memory_type(
    properties: &vk::PhysicalDeviceMemoryProperties,
    type_bits: u32,
    required_flags: vk::MemoryPropertyFlags,
) -> Option<u32> {
    for index in 0..properties.memory_type_count {
        let type_supported = (type_bits & (1 << index)) != 0;
        let flags = properties.memory_types[index as usize].property_flags;
        if type_supported && flags.contains(required_flags) {
            return Some(index);
        }
    }
    None
}

fn write_bgra_ppm(
    path: &std::path::Path,
    bgra: &[u8],
    width: u32,
    height: u32,
) -> std::io::Result<()> {
    use std::io::Write;

    let pixel_count = width as usize * height as usize;
    let required_len = pixel_count * 4;
    if bgra.len() < required_len {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "BGRA buffer is smaller than framebuffer dimensions",
        ));
    }

    let mut output =
        Vec::with_capacity(format!("P6\n{} {}\n255\n", width, height).len() + pixel_count * 3);
    write!(&mut output, "P6\n{} {}\n255\n", width, height)?;
    for pixel in bgra[..required_len].chunks_exact(4) {
        output.push(pixel[2]);
        output.push(pixel[1]);
        output.push(pixel[0]);
    }
    std::fs::write(path, output)
}
