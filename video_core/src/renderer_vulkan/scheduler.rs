// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Vulkan command scheduler with command chunk batching.
//!
//! Ref: zuyu `vk_scheduler.h/.cpp` — batches Vulkan commands into chunks,
//! manages render pass state, and submits to the GPU queue.

use ash::vk;
use log::{debug, trace};
use std::ptr::NonNull;
use std::sync::atomic::{AtomicU64, Ordering};

use super::state_tracker::StateTracker;

/// Type-erased Vulkan command closure.
///
/// Replaces zuyu's placement-new CommandChunk arena with boxed closures.
/// The two `vk::CommandBuffer` args are (render_cmdbuf, upload_cmdbuf).
type VkCommand = Box<dyn FnOnce(vk::CommandBuffer, vk::CommandBuffer) + Send>;

/// Batch of recorded Vulkan commands (zuyu: CommandChunk, 32KB arena).
struct CommandChunk {
    commands: Vec<VkCommand>,
}

impl CommandChunk {
    fn new() -> Self {
        Self {
            commands: Vec::with_capacity(64),
        }
    }

    fn is_empty(&self) -> bool {
        self.commands.is_empty()
    }
}

/// Current render pass state tracked by the scheduler.
#[derive(Default)]
struct RenderPassState {
    renderpass: vk::RenderPass,
    framebuffer: vk::Framebuffer,
    render_area: vk::Rect2D,
    inside_renderpass: bool,
    images: Vec<vk::Image>,
    image_ranges: Vec<vk::ImageSubresourceRange>,
}

/// Port of upstream `Scheduler::State` fields that are independent from the
/// command-buffer render pass state.
#[derive(Default)]
struct SchedulerState {
    graphics_pipeline: vk::Pipeline,
    is_rescaling: bool,
    rescaling_defined: bool,
}

/// Command buffer scheduler with submission tracking.
///
/// Ref: zuyu Scheduler — batches commands, tracks render pass state,
/// and submits to the GPU queue with tick-based synchronization.
pub struct Scheduler {
    device: ash::Device,
    queue: vk::Queue,
    command_pool: vk::CommandPool,

    /// Current chunk being recorded to.
    current_chunk: CommandChunk,

    /// Current primary command buffer.
    current_cmdbuf: vk::CommandBuffer,

    /// Upload command buffer for staging transfers.
    upload_cmdbuf: vk::CommandBuffer,

    /// Tick-based synchronization (simplified MasterSemaphore).
    current_tick: AtomicU64,

    /// Render pass state.
    rp_state: RenderPassState,

    /// Upstream scheduler-local state invalidated by helper draws.
    state: SchedulerState,

    /// Fence for GPU synchronization.
    fence: vk::Fence,

    /// Upstream `Scheduler` owns a `StateTracker&` and invalidates command
    /// buffer state after helper draws. Some Rust construction paths still
    /// build a scheduler before a rasterizer state tracker exists, so this is
    /// installed by the rasterizer once both owners are allocated.
    state_tracker: Option<NonNull<StateTracker>>,
}

impl Scheduler {
    /// Create a new scheduler.
    pub fn new(
        device: ash::Device,
        queue: vk::Queue,
        command_pool: vk::CommandPool,
    ) -> Result<Self, vk::Result> {
        let fence_info = vk::FenceCreateInfo::builder()
            .flags(vk::FenceCreateFlags::SIGNALED)
            .build();
        let fence = unsafe { device.create_fence(&fence_info, None)? };

        let mut scheduler = Self {
            device,
            queue,
            command_pool,
            current_chunk: CommandChunk::new(),
            current_cmdbuf: vk::CommandBuffer::null(),
            upload_cmdbuf: vk::CommandBuffer::null(),
            current_tick: AtomicU64::new(0),
            rp_state: RenderPassState::default(),
            state: SchedulerState::default(),
            fence,
            state_tracker: None,
        };
        scheduler.allocate_new_context()?;
        Ok(scheduler)
    }

    pub fn set_state_tracker(&mut self, state_tracker: NonNull<StateTracker>) {
        self.state_tracker = Some(state_tracker);
    }

    /// Record a command that only needs the render command buffer.
    pub fn record(&mut self, cmd: impl FnOnce(vk::CommandBuffer) + Send + 'static) {
        self.current_chunk
            .commands
            .push(Box::new(move |render_cmd, _upload_cmd| {
                cmd(render_cmd);
            }));
    }

    /// Record a command that needs both render and upload command buffers.
    pub fn record_with_upload(
        &mut self,
        cmd: impl FnOnce(vk::CommandBuffer, vk::CommandBuffer) + Send + 'static,
    ) {
        self.current_chunk.commands.push(Box::new(cmd));
    }

    /// Begin a render pass if not already inside one with matching parameters.
    pub fn request_renderpass(
        &mut self,
        framebuffer: vk::Framebuffer,
        renderpass: vk::RenderPass,
        render_area: vk::Rect2D,
        clear_values: &[vk::ClearValue],
        images: &[vk::Image],
        image_ranges: &[vk::ImageSubresourceRange],
    ) {
        if self.rp_state.inside_renderpass {
            // Already in a render pass — check if compatible
            if self.rp_state.renderpass == renderpass
                && self.rp_state.framebuffer == framebuffer
                && self.rp_state.render_area.extent.width == render_area.extent.width
                && self.rp_state.render_area.extent.height == render_area.extent.height
            {
                return;
            }
            // Different render pass — end current one first
            self.request_outside_renderpass();
        }

        let rp_begin = vk::RenderPassBeginInfo::builder()
            .render_pass(renderpass)
            .framebuffer(framebuffer)
            .render_area(render_area)
            .clear_values(clear_values)
            .build();

        trace!("Scheduler: beginning render pass");
        unsafe {
            self.device.cmd_begin_render_pass(
                self.current_cmdbuf,
                &rp_begin,
                vk::SubpassContents::INLINE,
            );
        }

        self.rp_state = RenderPassState {
            renderpass,
            framebuffer,
            render_area,
            inside_renderpass: true,
            images: images.to_vec(),
            image_ranges: image_ranges.to_vec(),
        };
    }

    /// End the current render pass if inside one.
    pub fn request_outside_renderpass(&mut self) {
        if !self.rp_state.inside_renderpass {
            return;
        }

        trace!("Scheduler: ending render pass");
        unsafe {
            self.device.cmd_end_render_pass(self.current_cmdbuf);
            let barriers: Vec<_> = self
                .rp_state
                .images
                .iter()
                .zip(self.rp_state.image_ranges.iter())
                .filter_map(|(&image, &subresource_range)| {
                    (image != vk::Image::null()).then(|| {
                        vk::ImageMemoryBarrier::builder()
                            .src_access_mask(
                                vk::AccessFlags::COLOR_ATTACHMENT_WRITE
                                    | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_WRITE,
                            )
                            .dst_access_mask(
                                vk::AccessFlags::SHADER_READ
                                    | vk::AccessFlags::SHADER_WRITE
                                    | vk::AccessFlags::COLOR_ATTACHMENT_READ
                                    | vk::AccessFlags::COLOR_ATTACHMENT_WRITE
                                    | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_READ
                                    | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_WRITE,
                            )
                            .old_layout(vk::ImageLayout::GENERAL)
                            .new_layout(vk::ImageLayout::GENERAL)
                            .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                            .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                            .image(image)
                            .subresource_range(subresource_range)
                            .build()
                    })
                })
                .collect();
            if !barriers.is_empty() {
                self.device.cmd_pipeline_barrier(
                    self.current_cmdbuf,
                    vk::PipelineStageFlags::EARLY_FRAGMENT_TESTS
                        | vk::PipelineStageFlags::LATE_FRAGMENT_TESTS
                        | vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                    vk::PipelineStageFlags::ALL_COMMANDS,
                    vk::DependencyFlags::empty(),
                    &[],
                    &[],
                    &barriers,
                );
            }
        }
        self.rp_state = RenderPassState::default();
    }

    /// Whether we are currently inside a render pass.
    pub fn is_inside_renderpass(&self) -> bool {
        self.rp_state.inside_renderpass
    }

    /// Port of upstream `Scheduler::InvalidateState`.
    pub fn invalidate_state(&mut self) {
        self.state.graphics_pipeline = vk::Pipeline::null();
        self.state.rescaling_defined = false;
        if let Some(mut state_tracker) = self.state_tracker {
            unsafe {
                state_tracker.as_mut().invalidate_command_buffer_state();
            }
        }
    }

    /// Execute all pending commands in the current chunk directly on the command buffer.
    pub fn dispatch_work(&mut self) {
        if self.current_chunk.is_empty() {
            return;
        }

        let chunk = std::mem::replace(&mut self.current_chunk, CommandChunk::new());
        for cmd in chunk.commands {
            cmd(self.current_cmdbuf, self.upload_cmdbuf);
        }
    }

    /// Flush — end render pass, dispatch remaining work, submit to GPU, return tick.
    pub fn flush(&mut self) -> u64 {
        self.flush_impl(&[])
    }

    /// Port of upstream `Scheduler::Flush(vk::Semaphore signal_semaphore)`.
    pub fn flush_with_signal(&mut self, signal_semaphore: vk::Semaphore) -> u64 {
        if signal_semaphore == vk::Semaphore::null() {
            self.flush()
        } else {
            self.flush_impl(&[signal_semaphore])
        }
    }

    fn flush_impl(&mut self, signal_semaphores: &[vk::Semaphore]) -> u64 {
        self.request_outside_renderpass();
        self.dispatch_work();

        // End command buffers
        unsafe {
            self.device.end_command_buffer(self.upload_cmdbuf).ok();
            self.device.end_command_buffer(self.current_cmdbuf).ok();
        }

        // Submit both command buffers (upload first, then render)
        let cmd_buffers = [self.upload_cmdbuf, self.current_cmdbuf];
        let submit_info = vk::SubmitInfo::builder()
            .command_buffers(&cmd_buffers)
            .signal_semaphores(signal_semaphores)
            .build();

        unsafe {
            self.device
                .wait_for_fences(&[self.fence], true, u64::MAX)
                .ok();
            self.device.reset_fences(&[self.fence]).ok();
            self.device
                .queue_submit(self.queue, &[submit_info], self.fence)
                .ok();
        }

        let tick = self.current_tick.fetch_add(1, Ordering::SeqCst) + 1;
        debug!("Scheduler: flushed at tick {}", tick);
        self.allocate_new_context().ok();
        tick
    }

    /// Flush + wait for GPU completion.
    pub fn finish(&mut self) {
        self.flush();
        unsafe {
            self.device
                .wait_for_fences(&[self.fence], true, u64::MAX)
                .ok();
        }
    }

    fn allocate_new_context(&mut self) -> Result<(), vk::Result> {
        let alloc_info = vk::CommandBufferAllocateInfo::builder()
            .command_pool(self.command_pool)
            .level(vk::CommandBufferLevel::PRIMARY)
            .command_buffer_count(2)
            .build();
        let cmd_buffers = unsafe { self.device.allocate_command_buffers(&alloc_info)? };
        self.current_cmdbuf = cmd_buffers[0];
        self.upload_cmdbuf = cmd_buffers[1];

        let begin_info = vk::CommandBufferBeginInfo::builder()
            .flags(vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT)
            .build();
        unsafe {
            self.device
                .begin_command_buffer(self.current_cmdbuf, &begin_info)?;
            self.device
                .begin_command_buffer(self.upload_cmdbuf, &begin_info)?;
        }
        self.rp_state = RenderPassState::default();
        Ok(())
    }

    /// Get the current command buffer for direct recording.
    pub fn command_buffer(&self) -> vk::CommandBuffer {
        self.current_cmdbuf
    }

    /// Get the upload command buffer.
    pub fn upload_command_buffer(&self) -> vk::CommandBuffer {
        self.upload_cmdbuf
    }

    /// Get the current tick value.
    pub fn current_tick(&self) -> u64 {
        self.current_tick.load(Ordering::SeqCst)
    }

    /// Returns true when the GPU has completed `tick`.
    ///
    /// Port-facing subset of upstream `Scheduler::IsFree`. This simplified
    /// scheduler reuses a single fence, waiting on it before every new submit;
    /// older ticks are therefore complete once a newer tick exists.
    pub fn is_free(&self, tick: u64) -> bool {
        if tick == 0 || tick < self.current_tick() {
            return true;
        }
        if tick > self.current_tick() {
            return false;
        }
        unsafe { self.device.get_fence_status(self.fence).unwrap_or(false) }
    }

    /// Tick that will be signalled by the next `Flush`.
    pub fn pending_tick(&self) -> u64 {
        self.current_tick() + 1
    }

    /// Port-facing subset of upstream `Scheduler::Wait`.
    pub fn wait(&mut self, tick: u64) {
        if tick == 0 {
            return;
        }
        if tick >= self.current_tick() {
            self.flush();
        }
        unsafe {
            self.device
                .wait_for_fences(&[self.fence], true, u64::MAX)
                .ok();
        }
    }
}

impl Drop for Scheduler {
    fn drop(&mut self) {
        unsafe {
            self.device
                .wait_for_fences(&[self.fence], true, u64::MAX)
                .ok();
            self.device.destroy_fence(self.fence, None);
            self.device.destroy_command_pool(self.command_pool, None);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_command_chunk_new_is_empty() {
        let chunk = CommandChunk::new();
        assert!(chunk.is_empty());
    }

    #[test]
    fn test_render_pass_state_default() {
        let state = RenderPassState::default();
        assert!(!state.inside_renderpass);
        assert_eq!(state.renderpass, vk::RenderPass::null());
        assert_eq!(state.framebuffer, vk::Framebuffer::null());
    }
}
