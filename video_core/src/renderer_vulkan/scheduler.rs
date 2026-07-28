// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Vulkan command scheduler with command chunk batching.
//!
//! Ref: zuyu `vk_scheduler.h/.cpp` — batches Vulkan commands into chunks,
//! manages render pass state, and submits to the GPU queue.

use ash::vk;
use log::{debug, trace};
use std::collections::VecDeque;
use std::mem::{align_of, size_of, MaybeUninit};
use std::ptr::NonNull;
use std::sync::atomic::Ordering;
use std::sync::{Arc, Condvar, Mutex};

use super::command_pool::CommandPool;
use super::master_semaphore::MasterSemaphore;
use super::query_cache::QueryCache;
use super::state_tracker::StateTracker;

const COMMAND_CHUNK_CAPACITY: usize = 0x8000;
const NO_COMMAND: usize = usize::MAX;

#[repr(C)]
#[derive(Clone, Copy)]
struct CommandHeader {
    next: usize,
    payload_offset: usize,
    execute: unsafe fn(*mut u8, vk::CommandBuffer, vk::CommandBuffer),
    drop_payload: unsafe fn(*mut u8),
}

#[repr(C, align(64))]
struct CommandStorage([MaybeUninit<u8>; COMMAND_CHUNK_CAPACITY]);

/// Batch of recorded Vulkan commands (zuyu: CommandChunk, 32KB arena).
struct CommandChunk {
    storage: Box<CommandStorage>,
    first: usize,
    last: usize,
    command_offset: usize,
    submit: Option<SubmitRequest>,
}

impl CommandChunk {
    fn new() -> Self {
        // The payload is an arena of `MaybeUninit<u8>` and has no initialized
        // value invariant. Allocate it directly on the heap so creating or
        // moving a chunk never copies or zeroes 32 KiB.
        let storage = unsafe { Box::<CommandStorage>::new_uninit().assume_init() };
        Self {
            storage,
            first: NO_COMMAND,
            last: NO_COMMAND,
            command_offset: 0,
            submit: None,
        }
    }

    fn is_empty(&self) -> bool {
        self.first == NO_COMMAND && self.submit.is_none()
    }

    fn command_layout<T>(&self) -> Option<(usize, usize, usize)> {
        assert!(
            align_of::<T>() <= align_of::<CommandStorage>(),
            "Vulkan scheduler command alignment exceeds the command arena"
        );
        let header_offset = self
            .command_offset
            .next_multiple_of(align_of::<CommandHeader>());
        let payload_offset = header_offset
            .checked_add(size_of::<CommandHeader>())?
            .next_multiple_of(align_of::<T>());
        let end_offset = payload_offset.checked_add(size_of::<T>())?;
        (end_offset <= COMMAND_CHUNK_CAPACITY).then_some((
            header_offset,
            payload_offset,
            end_offset,
        ))
    }

    fn record<T>(&mut self, command: T) -> Result<(), T>
    where
        T: FnOnce(vk::CommandBuffer, vk::CommandBuffer) + Send + 'static,
    {
        let Some((header_offset, payload_offset, end_offset)) = self.command_layout::<T>() else {
            return Err(command);
        };
        let base = self.storage.0.as_mut_ptr().cast::<u8>();
        let header = CommandHeader {
            next: NO_COMMAND,
            payload_offset,
            execute: execute_command::<T>,
            drop_payload: drop_command::<T>,
        };
        unsafe {
            base.add(header_offset)
                .cast::<CommandHeader>()
                .write(header);
            base.add(payload_offset).cast::<T>().write(command);
            if self.last != NO_COMMAND {
                (*base.add(self.last).cast::<CommandHeader>()).next = header_offset;
            } else {
                self.first = header_offset;
            }
        }
        self.last = header_offset;
        self.command_offset = end_offset;
        Ok(())
    }

    fn pop_header(&mut self) -> Option<CommandHeader> {
        if self.first == NO_COMMAND {
            return None;
        }
        let header_offset = self.first;
        let header = unsafe {
            self.storage
                .0
                .as_ptr()
                .cast::<u8>()
                .add(header_offset)
                .cast::<CommandHeader>()
                .read()
        };
        self.first = header.next;
        if self.first == NO_COMMAND {
            self.last = NO_COMMAND;
        }
        Some(header)
    }

    fn execute_all(
        &mut self,
        cmdbuf: vk::CommandBuffer,
        upload_cmdbuf: vk::CommandBuffer,
    ) -> Option<SubmitRequest> {
        while let Some(header) = self.pop_header() {
            let payload = unsafe {
                self.storage
                    .0
                    .as_mut_ptr()
                    .cast::<u8>()
                    .add(header.payload_offset)
            };
            unsafe {
                (header.execute)(payload, cmdbuf, upload_cmdbuf);
            }
        }
        self.command_offset = 0;
        self.submit.take()
    }
}

impl Drop for CommandChunk {
    fn drop(&mut self) {
        while let Some(header) = self.pop_header() {
            let payload = unsafe {
                self.storage
                    .0
                    .as_mut_ptr()
                    .cast::<u8>()
                    .add(header.payload_offset)
            };
            unsafe {
                (header.drop_payload)(payload);
            }
        }
    }
}

unsafe fn execute_command<T>(
    payload: *mut u8,
    cmdbuf: vk::CommandBuffer,
    upload_cmdbuf: vk::CommandBuffer,
) where
    T: FnOnce(vk::CommandBuffer, vk::CommandBuffer),
{
    let command = unsafe { payload.cast::<T>().read() };
    command(cmdbuf, upload_cmdbuf);
}

unsafe fn drop_command<T>(payload: *mut u8) {
    unsafe {
        payload.cast::<T>().drop_in_place();
    }
}

struct SubmitRequest {
    signal_semaphore: vk::Semaphore,
    wait_semaphore: vk::Semaphore,
    tick: u64,
    on_submit: Option<Arc<dyn Fn() + Send + Sync>>,
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

    /// Current chunk being recorded to.
    current_chunk: CommandChunk,

    /// Upstream `Scheduler::master_semaphore`, shared with Vulkan fences and
    /// resource pools so every completion query uses one authoritative tick.
    master_semaphore: Arc<MasterSemaphore>,

    /// Render pass state.
    rp_state: RenderPassState,
    /// Upstream scheduler-local state invalidated by helper draws.
    state: SchedulerState,

    /// Port of upstream `Scheduler::submit_mutex`.
    submit_mutex: Arc<Mutex<()>>,

    /// Upstream `Scheduler` owns a `StateTracker&` and invalidates command
    /// buffer state after helper draws. Some Rust construction paths still
    /// build a scheduler before a rasterizer state tracker exists, so this is
    /// installed by the rasterizer once both owners are allocated.
    state_tracker: Option<NonNull<StateTracker>>,
    query_cache: Option<NonNull<QueryCache>>,
    on_submit: Option<Arc<dyn Fn() + Send + Sync>>,

    /// Port of upstream `Scheduler::WorkerThread`: owns command-buffer
    /// recording, command-pool rotation, and queue submission.
    worker: Option<Arc<SchedulerWorker>>,
    worker_thread: Option<std::thread::JoinHandle<()>>,
}

struct SchedulerWorker {
    state: Mutex<SchedulerWorkerState>,
    job_cv: Condvar,
    drained_cv: Condvar,
    stop: std::sync::atomic::AtomicBool,
}

struct SchedulerWorkerState {
    chunks: VecDeque<CommandChunk>,
    chunk_reserve: Vec<CommandChunk>,
    in_flight: usize,
}

impl SchedulerWorkerState {
    fn is_drained(&self) -> bool {
        self.chunks.is_empty() && self.in_flight == 0
    }

    fn pop_front(&mut self) -> Option<CommandChunk> {
        let chunk = self.chunks.pop_front()?;
        self.in_flight += 1;
        Some(chunk)
    }
}

struct WorkerContext {
    device: ash::Device,
    command_pool: CommandPool,
    current_cmdbuf: vk::CommandBuffer,
    upload_cmdbuf: vk::CommandBuffer,
    master_semaphore: Arc<MasterSemaphore>,
    submit_mutex: Arc<Mutex<()>>,
}

impl SchedulerWorker {
    fn new() -> Self {
        Self {
            state: Mutex::new(SchedulerWorkerState {
                chunks: VecDeque::new(),
                chunk_reserve: Vec::new(),
                in_flight: 0,
            }),
            job_cv: Condvar::new(),
            drained_cv: Condvar::new(),
            stop: std::sync::atomic::AtomicBool::new(false),
        }
    }

    fn push(&self, chunk: CommandChunk) {
        self.state.lock().unwrap().chunks.push_back(chunk);
        self.job_cv.notify_one();
    }

    fn acquire_chunk(&self) -> CommandChunk {
        self.state
            .lock()
            .unwrap()
            .chunk_reserve
            .pop()
            .unwrap_or_else(CommandChunk::new)
    }

    fn wait_drained(&self) {
        let mut state = self.state.lock().unwrap();
        while !state.is_drained() {
            state = self.drained_cv.wait(state).unwrap();
        }
    }

    fn run(&self, mut context: WorkerContext) {
        loop {
            let chunk = {
                let mut state = self.state.lock().unwrap();
                loop {
                    if let Some(chunk) = state.pop_front() {
                        break Some(chunk);
                    }
                    if self.stop.load(Ordering::Acquire) {
                        break None;
                    }
                    state = self.job_cv.wait(state).unwrap();
                }
            };
            let Some(mut chunk) = chunk else {
                break;
            };

            let submit = chunk.execute_all(context.current_cmdbuf, context.upload_cmdbuf);
            if let Some(submit) = submit {
                if let Err(error) = context.submit_execution(&submit) {
                    log::error!(
                        "Vulkan worker failed to submit tick {}: {error:?}",
                        submit.tick
                    );
                }
                if let Err(error) = context.allocate_worker_command_buffer() {
                    log::error!(
                        "Vulkan worker failed to rotate command buffers after tick {}: {error:?}",
                        submit.tick
                    );
                }
            }

            let mut state = self.state.lock().unwrap();
            state.in_flight -= 1;
            state.chunk_reserve.push(chunk);
            if state.is_drained() {
                self.drained_cv.notify_all();
            }
        }
        context.wait_for_gpu();
    }
}

impl WorkerContext {
    fn allocate_worker_command_buffer(&mut self) -> Result<(), vk::Result> {
        self.current_cmdbuf = self.command_pool.commit();
        self.upload_cmdbuf = self.command_pool.commit();
        let begin_info = vk::CommandBufferBeginInfo::builder()
            .flags(vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT)
            .build();
        unsafe {
            self.device
                .reset_command_buffer(self.current_cmdbuf, vk::CommandBufferResetFlags::empty())?;
            self.device
                .reset_command_buffer(self.upload_cmdbuf, vk::CommandBufferResetFlags::empty())?;
            self.device
                .begin_command_buffer(self.current_cmdbuf, &begin_info)?;
            self.device
                .begin_command_buffer(self.upload_cmdbuf, &begin_info)?;
        }
        Ok(())
    }

    fn submit_execution(&self, submit: &SubmitRequest) -> Result<(), vk::Result> {
        unsafe {
            let write_barrier = vk::MemoryBarrier::builder()
                .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                .dst_access_mask(vk::AccessFlags::MEMORY_READ | vk::AccessFlags::MEMORY_WRITE)
                .build();
            self.device.cmd_pipeline_barrier(
                self.upload_cmdbuf,
                vk::PipelineStageFlags::TRANSFER,
                vk::PipelineStageFlags::ALL_COMMANDS,
                vk::DependencyFlags::empty(),
                &[write_barrier],
                &[],
                &[],
            );
            self.device.end_command_buffer(self.upload_cmdbuf)?;
            self.device.end_command_buffer(self.current_cmdbuf)?;
        }

        if let Some(on_submit) = &submit.on_submit {
            on_submit();
        }
        let _submit_lock = self.submit_mutex.lock().unwrap();
        self.master_semaphore.submit_queue(
            self.current_cmdbuf,
            self.upload_cmdbuf,
            submit.signal_semaphore,
            submit.wait_semaphore,
            submit.tick,
        )
    }

    fn wait_for_gpu(&self) {
        let tick = self.master_semaphore.current_tick().saturating_sub(1);
        self.master_semaphore.wait(tick);
    }
}

impl Scheduler {
    /// Create a new scheduler.
    pub fn new(
        device: ash::Device,
        queue: vk::Queue,
        graphics_family: u32,
        timeline_semaphore_supported: bool,
    ) -> Result<Self, vk::Result> {
        if !timeline_semaphore_supported {
            log::warn!(
                "Scheduler: timeline semaphores unavailable; using upstream fence-queue fallback"
            );
        }

        let master_semaphore =
            MasterSemaphore::new(device.clone(), queue, timeline_semaphore_supported)?;
        let submit_mutex = Arc::new(Mutex::new(()));
        let worker = Arc::new(SchedulerWorker::new());
        let mut worker_context = WorkerContext {
            device: device.clone(),
            command_pool: CommandPool::new(
                Arc::clone(&master_semaphore),
                device.clone(),
                graphics_family,
            ),
            current_cmdbuf: vk::CommandBuffer::null(),
            upload_cmdbuf: vk::CommandBuffer::null(),
            master_semaphore: Arc::clone(&master_semaphore),
            submit_mutex: Arc::clone(&submit_mutex),
        };
        worker_context.allocate_worker_command_buffer()?;
        let thread_worker = Arc::clone(&worker);
        let worker_thread = std::thread::Builder::new()
            .name("VulkanWorker".into())
            .spawn(move || thread_worker.run(worker_context))
            .expect("Failed to spawn Vulkan scheduler worker");

        Ok(Self {
            device,
            current_chunk: CommandChunk::new(),
            master_semaphore,
            rp_state: RenderPassState::default(),
            state: SchedulerState::default(),
            submit_mutex,
            state_tracker: None,
            query_cache: None,
            on_submit: None,
            worker: Some(worker),
            worker_thread: Some(worker_thread),
        })
    }

    pub fn submit_mutex(&self) -> Arc<Mutex<()>> {
        Arc::clone(&self.submit_mutex)
    }

    /// Returns the master timeline/fence owner used by scheduler submissions.
    ///
    /// Rust shares this owner with `InnerFence`; upstream reaches the same
    /// object through `Scheduler&`.
    pub fn master_semaphore(&self) -> Arc<MasterSemaphore> {
        Arc::clone(&self.master_semaphore)
    }

    pub fn set_state_tracker(&mut self, state_tracker: NonNull<StateTracker>) {
        self.state_tracker = Some(state_tracker);
    }

    /// Port of upstream `Scheduler::SetQueryCache`.
    pub fn set_query_cache(&mut self, query_cache: NonNull<QueryCache>) {
        self.query_cache = Some(query_cache);
    }

    /// Port of upstream `Scheduler::RegisterOnSubmit`.
    pub fn register_on_submit(&mut self, callback: impl Fn() + Send + Sync + 'static) {
        self.on_submit = Some(Arc::new(callback));
    }

    /// Record a command that only needs the render command buffer.
    pub fn record(&mut self, cmd: impl FnOnce(vk::CommandBuffer) + Send + 'static) {
        self.record_with_upload(move |render_cmd, _upload_cmd| cmd(render_cmd));
    }

    /// Record a command that needs both render and upload command buffers.
    pub fn record_with_upload(
        &mut self,
        cmd: impl FnOnce(vk::CommandBuffer, vk::CommandBuffer) + Send + 'static,
    ) {
        let command = match self.current_chunk.record(cmd) {
            Ok(()) => return,
            Err(command) => command,
        };
        self.dispatch_work();
        if self.current_chunk.record(command).is_err() {
            panic!("Vulkan scheduler command exceeds the 32 KiB command chunk");
        }
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

        trace!("Scheduler: beginning render pass");
        let device = self.device.clone();
        let clear_values = clear_values.to_vec();
        self.record(move |cmdbuf| unsafe {
            let rp_begin = vk::RenderPassBeginInfo::builder()
                .render_pass(renderpass)
                .framebuffer(framebuffer)
                .render_area(render_area)
                .clear_values(&clear_values)
                .build();
            device.cmd_begin_render_pass(cmdbuf, &rp_begin, vk::SubpassContents::INLINE);
        });

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
        let images = std::mem::take(&mut self.rp_state.images);
        let image_ranges = std::mem::take(&mut self.rp_state.image_ranges);
        let device = self.device.clone();
        self.record(move |cmdbuf| unsafe {
            device.cmd_end_render_pass(cmdbuf);
            let barriers: Vec<_> = images
                .iter()
                .zip(image_ranges.iter())
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
                device.cmd_pipeline_barrier(
                    cmdbuf,
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
        });
        self.rp_state = RenderPassState::default();
    }

    /// Whether we are currently inside a render pass.
    pub fn is_inside_renderpass(&self) -> bool {
        self.rp_state.inside_renderpass
    }

    /// Port of upstream `Scheduler::UpdateGraphicsPipeline`.
    pub fn update_graphics_pipeline(&mut self, pipeline: vk::Pipeline) -> bool {
        if self.state.graphics_pipeline == pipeline {
            return false;
        }
        self.state.graphics_pipeline = pipeline;
        true
    }

    /// Port of upstream `Scheduler::UpdateRescaling`.
    pub fn update_rescaling(&mut self, is_rescaling: bool) -> bool {
        if self.state.rescaling_defined && self.state.is_rescaling == is_rescaling {
            return false;
        }
        self.state.rescaling_defined = true;
        self.state.is_rescaling = is_rescaling;
        true
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

    /// Port of upstream `Scheduler::DispatchWork`.
    pub fn dispatch_work(&mut self) {
        if self.current_chunk.is_empty() {
            return;
        }

        let worker = self.worker.as_ref().expect("scheduler worker must exist");
        let next_chunk = worker.acquire_chunk();
        let chunk = std::mem::replace(&mut self.current_chunk, next_chunk);
        worker.push(chunk);
    }

    /// Port of upstream `Scheduler::WaitWorker`.
    ///
    pub fn wait_worker(&mut self) {
        self.dispatch_work();
        if let Some(worker) = self.worker.as_ref() {
            worker.wait_drained();
        }
    }

    /// Flush — end render pass, dispatch remaining work, submit to GPU, return tick.
    pub fn flush(&mut self) -> u64 {
        self.flush_impl(vk::Semaphore::null(), vk::Semaphore::null())
    }

    /// Port of upstream `Scheduler::Flush(vk::Semaphore signal_semaphore)`.
    pub fn flush_with_signal(&mut self, signal_semaphore: vk::Semaphore) -> u64 {
        self.flush_impl(signal_semaphore, vk::Semaphore::null())
    }

    /// Full upstream `Scheduler::Flush(signal_semaphore, wait_semaphore)`.
    pub fn flush_with_semaphores(
        &mut self,
        signal_semaphore: vk::Semaphore,
        wait_semaphore: vk::Semaphore,
    ) -> u64 {
        self.flush_impl(signal_semaphore, wait_semaphore)
    }

    fn flush_impl(
        &mut self,
        signal_semaphore: vk::Semaphore,
        wait_semaphore: vk::Semaphore,
    ) -> u64 {
        let tick = self.submit_execution(signal_semaphore, wait_semaphore);
        self.allocate_new_context();
        tick
    }

    /// Port of upstream `Scheduler::SubmitExecution`.
    fn submit_execution(
        &mut self,
        signal_semaphore: vk::Semaphore,
        wait_semaphore: vk::Semaphore,
    ) -> u64 {
        self.end_pending_operations();
        self.invalidate_state();
        let tick = self.master_semaphore.next_tick();
        self.current_chunk.submit = Some(SubmitRequest {
            signal_semaphore,
            wait_semaphore,
            tick,
            on_submit: self.on_submit.clone(),
        });
        self.dispatch_work();
        debug!("Scheduler: flushed at tick {}", tick);
        self.rp_state = RenderPassState::default();
        tick
    }

    /// Port of upstream `Scheduler::EndPendingOperations`.
    fn end_pending_operations(&mut self) {
        if let Some(mut query_cache) = self.query_cache {
            unsafe {
                query_cache.as_mut().notify_segment(false);
            }
        }
        self.request_outside_renderpass();
    }

    /// Port of upstream `Scheduler::AllocateNewContext`.
    fn allocate_new_context(&mut self) {
        if let Some(mut query_cache) = self.query_cache {
            unsafe {
                query_cache.as_mut().notify_segment(true);
            }
        }
    }

    /// Flush + wait for GPU completion.
    pub fn finish(&mut self) {
        self.finish_with_semaphores(vk::Semaphore::null(), vk::Semaphore::null());
    }

    /// Full upstream `Scheduler::Finish(signal_semaphore, wait_semaphore)`.
    pub fn finish_with_semaphores(
        &mut self,
        signal_semaphore: vk::Semaphore,
        wait_semaphore: vk::Semaphore,
    ) {
        let presubmit_tick = self.current_tick();
        self.submit_execution(signal_semaphore, wait_semaphore);
        self.wait(presubmit_tick);
        self.allocate_new_context();
    }

    /// Get the current tick value.
    pub fn current_tick(&self) -> u64 {
        self.master_semaphore.current_tick()
    }

    /// Last tick the GPU has fully completed.
    ///
    /// Port of upstream `MasterSemaphore::KnownGpuTick`. Delayed-destruction
    /// rings must retire against this value, not against the submission tick:
    /// with pipelined submissions the CPU-side tick runs ahead of the GPU.
    pub fn known_gpu_tick(&self) -> u64 {
        self.master_semaphore.refresh();
        self.master_semaphore.known_gpu_tick()
    }

    /// Returns true when the GPU has completed `tick`.
    ///
    /// Port of upstream `Scheduler::IsFree`.
    pub fn is_free(&self, tick: u64) -> bool {
        self.master_semaphore.is_free(tick)
    }

    /// Tick that will be signalled by the next `Flush`.
    pub fn pending_tick(&self) -> u64 {
        self.current_tick()
    }

    /// Port-facing subset of upstream `Scheduler::Wait`.
    pub fn wait(&mut self, tick: u64) {
        if tick >= self.current_tick() {
            // Upstream: never wait for the current logical tick before a
            // submission has been recorded to signal it.
            self.flush();
        }
        self.master_semaphore.wait(tick);
    }
}

impl Drop for Scheduler {
    fn drop(&mut self) {
        if let Some(worker) = self.worker.take() {
            worker.wait_drained();
            worker.stop.store(true, Ordering::Release);
            worker.job_cv.notify_all();
            if let Some(handle) = self.worker_thread.take() {
                let _ = handle.join();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicU64;

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

    #[test]
    fn scheduler_worker_is_not_drained_while_chunk_is_in_flight() {
        let mut state = SchedulerWorkerState {
            chunks: VecDeque::new(),
            chunk_reserve: Vec::new(),
            in_flight: 1,
        };
        assert!(!state.is_drained());

        state.in_flight = 0;
        assert!(state.is_drained());
    }

    #[test]
    fn command_chunk_executes_commands_in_record_order() {
        let order = Arc::new(Mutex::new(Vec::new()));
        let mut chunk = CommandChunk::new();
        for value in [3, 1, 4] {
            let order = Arc::clone(&order);
            assert!(chunk
                .record(move |_, _| {
                    order.lock().unwrap().push(value);
                })
                .is_ok());
        }

        let submit = chunk.execute_all(vk::CommandBuffer::null(), vk::CommandBuffer::null());

        assert!(submit.is_none());
        assert_eq!(*order.lock().unwrap(), [3, 1, 4]);
    }

    #[test]
    fn command_chunk_drops_unexecuted_commands_once() {
        struct DropProbe(Arc<AtomicU64>);

        impl Drop for DropProbe {
            fn drop(&mut self) {
                self.0.fetch_add(1, Ordering::Relaxed);
            }
        }

        let drops = Arc::new(AtomicU64::new(0));
        {
            let mut chunk = CommandChunk::new();
            let probe = DropProbe(Arc::clone(&drops));
            assert!(chunk
                .record(move |_, _| {
                    std::hint::black_box(&probe);
                })
                .is_ok());
            assert_eq!(drops.load(Ordering::Relaxed), 0);
        }
        assert_eq!(drops.load(Ordering::Relaxed), 1);
    }

    #[test]
    fn command_chunk_reuses_arena_after_execution() {
        let executions = Arc::new(AtomicU64::new(0));
        let mut chunk = CommandChunk::new();
        for expected in 1..=2 {
            let command_executions = Arc::clone(&executions);
            assert!(chunk
                .record(move |_, _| {
                    command_executions.fetch_add(1, Ordering::Relaxed);
                })
                .is_ok());
            chunk.execute_all(vk::CommandBuffer::null(), vk::CommandBuffer::null());
            assert_eq!(executions.load(Ordering::Relaxed), expected);
            assert_eq!(chunk.command_offset, 0);
        }
    }

    #[test]
    fn command_chunk_preserves_command_alignment() {
        #[repr(align(64))]
        struct AlignedCapture(Arc<AtomicU64>);

        let executions = Arc::new(AtomicU64::new(0));
        let capture = AlignedCapture(Arc::clone(&executions));
        let mut chunk = CommandChunk::new();
        assert!(chunk
            .record(move |_, _| {
                capture.0.fetch_add(1, Ordering::Relaxed);
            })
            .is_ok());

        chunk.execute_all(vk::CommandBuffer::null(), vk::CommandBuffer::null());
        assert_eq!(executions.load(Ordering::Relaxed), 1);
    }

    #[test]
    fn worker_queue_pops_chunks_fifo_and_tracks_in_flight() {
        let mut first = CommandChunk::new();
        first.submit = Some(SubmitRequest {
            signal_semaphore: vk::Semaphore::null(),
            wait_semaphore: vk::Semaphore::null(),
            tick: 7,
            on_submit: None,
        });
        let mut second = CommandChunk::new();
        second.submit = Some(SubmitRequest {
            signal_semaphore: vk::Semaphore::null(),
            wait_semaphore: vk::Semaphore::null(),
            tick: 8,
            on_submit: None,
        });
        let mut state = SchedulerWorkerState {
            chunks: VecDeque::from([first, second]),
            chunk_reserve: Vec::new(),
            in_flight: 0,
        };

        let first = state.pop_front().unwrap();
        assert_eq!(first.submit.as_ref().unwrap().tick, 7);
        assert_eq!(state.in_flight, 1);
        let second = state.pop_front().unwrap();
        assert_eq!(second.submit.as_ref().unwrap().tick, 8);
        assert_eq!(state.in_flight, 2);
    }

    #[test]
    fn command_chunk_rejects_a_command_past_upstream_capacity() {
        let first = {
            let payload = [0u8; 0x5000];
            move |_, _| {
                std::hint::black_box(payload);
            }
        };
        let second = {
            let payload = [0u8; 0x5000];
            move |_, _| {
                std::hint::black_box(payload);
            }
        };
        let mut chunk = CommandChunk::new();

        assert!(chunk.record(first).is_ok());
        assert!(chunk.record(second).is_err());
    }
}
