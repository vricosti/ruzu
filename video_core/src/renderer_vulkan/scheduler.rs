// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Vulkan command scheduler with command chunk batching.
//!
//! Ref: zuyu `vk_scheduler.h/.cpp` — batches Vulkan commands into chunks,
//! manages render pass state, and submits to the GPU queue.

use ash::vk;
use log::{debug, trace};
use std::sync::atomic::{AtomicU64, Ordering};

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

    /// Fence for GPU synchronization.
    fence: vk::Fence,
}

impl Scheduler {
    /// Create a new scheduler.
    pub fn new(
        device: ash::Device,
        queue: vk::Queue,
        command_pool: vk::CommandPool,
    ) -> Result<Self, vk::Result> {
        // Allocate two command buffers: one for rendering, one for uploads
        let alloc_info = vk::CommandBufferAllocateInfo::builder()
            .command_pool(command_pool)
            .level(vk::CommandBufferLevel::PRIMARY)
            .command_buffer_count(2)
            .build();
        let cmd_buffers = unsafe { device.allocate_command_buffers(&alloc_info)? };

        let fence_info = vk::FenceCreateInfo::builder().build();
        let fence = unsafe { device.create_fence(&fence_info, None)? };

        // Begin both command buffers
        let begin_info = vk::CommandBufferBeginInfo::builder()
            .flags(vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT)
            .build();
        unsafe {
            device.begin_command_buffer(cmd_buffers[0], &begin_info)?;
            device.begin_command_buffer(cmd_buffers[1], &begin_info)?;
        }

        Ok(Self {
            device,
            queue,
            command_pool,
            current_chunk: CommandChunk::new(),
            current_cmdbuf: cmd_buffers[0],
            upload_cmdbuf: cmd_buffers[1],
            current_tick: AtomicU64::new(0),
            rp_state: RenderPassState::default(),
            fence,
        })
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
    ) {
        if self.rp_state.inside_renderpass {
            // Already in a render pass — check if compatible
            if self.rp_state.renderpass == renderpass && self.rp_state.framebuffer == framebuffer {
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
        }
        self.rp_state.inside_renderpass = false;
    }

    /// Whether we are currently inside a render pass.
    pub fn is_inside_renderpass(&self) -> bool {
        self.rp_state.inside_renderpass
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
            .build();

        unsafe {
            self.device.reset_fences(&[self.fence]).ok();
            self.device
                .queue_submit(self.queue, &[submit_info], self.fence)
                .ok();
        }

        let tick = self.current_tick.fetch_add(1, Ordering::SeqCst) + 1;
        debug!("Scheduler: flushed at tick {}", tick);
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

        // Reset and re-begin command buffers for next frame
        unsafe {
            self.device
                .reset_command_pool(self.command_pool, vk::CommandPoolResetFlags::empty())
                .ok();
        }

        let alloc_info = vk::CommandBufferAllocateInfo::builder()
            .command_pool(self.command_pool)
            .level(vk::CommandBufferLevel::PRIMARY)
            .command_buffer_count(2)
            .build();
        if let Ok(cmd_buffers) = unsafe { self.device.allocate_command_buffers(&alloc_info) } {
            self.current_cmdbuf = cmd_buffers[0];
            self.upload_cmdbuf = cmd_buffers[1];

            let begin_info = vk::CommandBufferBeginInfo::builder()
                .flags(vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT)
                .build();
            unsafe {
                self.device
                    .begin_command_buffer(self.current_cmdbuf, &begin_info)
                    .ok();
                self.device
                    .begin_command_buffer(self.upload_cmdbuf, &begin_info)
                    .ok();
            }
        }
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
}

impl Drop for Scheduler {
    fn drop(&mut self) {
        unsafe {
            self.device
                .wait_for_fences(&[self.fence], true, u64::MAX)
                .ok();
            self.device.destroy_fence(self.fence, None);
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
