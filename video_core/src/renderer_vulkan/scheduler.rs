// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Vulkan command scheduler with command chunk batching.
//!
//! Ref: zuyu `vk_scheduler.h/.cpp` — batches Vulkan commands into chunks,
//! manages render pass state, and submits to the GPU queue.

use ash::vk;
use ash::vk::Handle;
use log::{debug, trace};
use std::collections::VecDeque;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Condvar, Mutex};

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
    /// Diagnostic count accumulated across dispatches for the current submit.
    current_submit_command_count: usize,
    /// Diagnostic count of guest draws encoded into the current submit.
    current_submit_draw_count: usize,

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

    /// Fence for GPU synchronization (legacy fallback when timeline
    /// semaphores are unavailable: one submission in flight, wait-before-submit).
    fence: vk::Fence,

    /// Port of upstream `MasterSemaphore`: a timeline semaphore signalled with
    /// the tick of each submission, so submissions pipeline without waiting
    /// for the previous one and completion is queried per tick.
    timeline_semaphore: Option<vk::Semaphore>,

    /// Port of upstream `Scheduler::submit_mutex`.
    submit_mutex: Arc<Mutex<()>>,

    /// Upstream `Scheduler` owns a `StateTracker&` and invalidates command
    /// buffer state after helper draws. Some Rust construction paths still
    /// build a scheduler before a rasterizer state tracker exists, so this is
    /// installed by the rasterizer once both owners are allocated.
    state_tracker: Option<NonNull<StateTracker>>,

    /// Submission worker ("VulkanWorker"), port of the submit half of
    /// upstream `Scheduler::WorkerThread`. On MoltenVK all Metal encoding
    /// happens inside vkQueueSubmit (measured ~40% of the GPU thread during
    /// MK8D loading), so handing the submit to a worker moves that cost off
    /// the DMA-pusher critical path. Command recording (incl.
    /// end_command_buffer) stays on the GPU thread: Vulkan requires external
    /// sync on the command POOL for recording, and ending is cheap.
    /// `None` on the legacy no-timeline fence path (single submission in
    /// flight cannot pipeline).
    submit_worker: Option<Arc<SubmitWorker>>,
    submit_worker_thread: Option<std::thread::JoinHandle<()>>,
}

/// One queued submission for the worker: both command buffers are fully
/// recorded and ended; the worker only calls vkQueueSubmit.
struct SubmitJob {
    render_cmdbuf: vk::CommandBuffer,
    upload_cmdbuf: vk::CommandBuffer,
    signal_semaphores: Vec<vk::Semaphore>,
    tick: u64,
    command_count: usize,
    draw_count: usize,
}

fn submit_profile_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("RUZU_PROFILE_VK_SUBMIT").is_some())
}

struct SubmitWorker {
    state: Mutex<SubmitWorkerState>,
    job_cv: Condvar,
    /// Signalled whenever the queue drains to empty (for `wait_worker`).
    drained_cv: Condvar,
    stop: std::sync::atomic::AtomicBool,
}

struct SubmitWorkerState {
    jobs: VecDeque<SubmitJob>,
    /// Jobs removed from `jobs` whose `vkQueueSubmit` has not returned yet.
    in_flight: usize,
}

impl SubmitWorkerState {
    fn is_drained(&self) -> bool {
        self.jobs.is_empty() && self.in_flight == 0
    }
}

fn submit_worker_sync_mode() -> bool {
    static SYNC: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *SYNC.get_or_init(|| {
        std::env::var("RUZU_VK_SUBMIT_WORKER").is_ok_and(|v| v.eq_ignore_ascii_case("sync"))
    })
}

impl SubmitWorker {
    fn new() -> Self {
        Self {
            state: Mutex::new(SubmitWorkerState {
                jobs: VecDeque::new(),
                in_flight: 0,
            }),
            job_cv: Condvar::new(),
            drained_cv: Condvar::new(),
            stop: std::sync::atomic::AtomicBool::new(false),
        }
    }

    fn push(&self, job: SubmitJob) {
        self.state.lock().unwrap().jobs.push_back(job);
        self.job_cv.notify_one();
    }

    /// Block until every queued submission has been handed to the driver.
    fn wait_drained(&self) {
        let mut state = self.state.lock().unwrap();
        while !state.is_drained() {
            state = self.drained_cv.wait(state).unwrap();
        }
    }

    fn run(
        &self,
        device: ash::Device,
        queue: vk::Queue,
        timeline: vk::Semaphore,
        submit_mutex: Arc<Mutex<()>>,
    ) {
        loop {
            let job = {
                let mut state = self.state.lock().unwrap();
                loop {
                    if let Some(job) = state.jobs.pop_front() {
                        state.in_flight += 1;
                        break job;
                    }
                    if self.stop.load(Ordering::Acquire) {
                        return;
                    }
                    state = self.job_cv.wait(state).unwrap();
                }
            };

            let cmd_buffers = [job.upload_cmdbuf, job.render_cmdbuf];
            let mut all_signals = Vec::with_capacity(1 + job.signal_semaphores.len());
            all_signals.push(timeline);
            all_signals.extend_from_slice(&job.signal_semaphores);
            let mut signal_values = vec![0u64; all_signals.len()];
            signal_values[0] = job.tick;
            let mut timeline_info =
                vk::TimelineSemaphoreSubmitInfo::builder().signal_semaphore_values(&signal_values);
            let submit_info = vk::SubmitInfo::builder()
                .command_buffers(&cmd_buffers)
                .signal_semaphores(&all_signals)
                .push_next(&mut timeline_info)
                .build();
            let submit_start = submit_profile_enabled().then(std::time::Instant::now);
            let submit_result = unsafe {
                let _submit_lock = submit_mutex.lock().unwrap();
                device.queue_submit(queue, &[submit_info], vk::Fence::null())
            };
            if let Some(start) = submit_start {
                let elapsed_us = start.elapsed().as_micros() as u64;
                if elapsed_us >= 100_000 || !job.signal_semaphores.is_empty() || job.draw_count != 0
                {
                    eprintln!(
                        "[VK_SUBMIT_PROFILE] tick={} elapsed_us={} commands={} draws={} binary_signals={}",
                        job.tick,
                        elapsed_us,
                        job.command_count,
                        job.draw_count,
                        job.signal_semaphores.len(),
                    );
                }
            }
            if let Err(error) = submit_result {
                log::error!(
                    "Vulkan submit worker failed to submit tick {}: {:?}",
                    job.tick,
                    error
                );
            }

            let mut state = self.state.lock().unwrap();
            state.in_flight -= 1;
            if state.is_drained() {
                self.drained_cv.notify_all();
            }
        }
    }
}

impl Scheduler {
    /// Create a new scheduler.
    pub fn new(
        device: ash::Device,
        queue: vk::Queue,
        command_pool: vk::CommandPool,
        timeline_semaphore_supported: bool,
    ) -> Result<Self, vk::Result> {
        let fence_info = vk::FenceCreateInfo::builder()
            .flags(vk::FenceCreateFlags::SIGNALED)
            .build();
        let fence = unsafe { device.create_fence(&fence_info, None)? };

        let timeline_semaphore = if timeline_semaphore_supported {
            let mut type_info = vk::SemaphoreTypeCreateInfo::builder()
                .semaphore_type(vk::SemaphoreType::TIMELINE)
                .initial_value(0)
                .build();
            let semaphore_info = vk::SemaphoreCreateInfo::builder()
                .push_next(&mut type_info)
                .build();
            Some(unsafe { device.create_semaphore(&semaphore_info, None)? })
        } else {
            log::warn!(
                "Scheduler: timeline semaphores unavailable; falling back to                  single-submission fence synchronization"
            );
            None
        };

        let submit_mutex = Arc::new(Mutex::new(()));
        // Port of upstream's Scheduler worker thread ("VulkanWorker"):
        // vkQueueSubmit runs off the GPU thread, which on MoltenVK reclaims
        // ~40% of it (all Metal encoding happens inside submit; measured
        // vkQueueSubmit 1182→0 samples on the GPU thread). Default ON like
        // upstream; RUZU_VK_SUBMIT_WORKER=0 forces synchronous submits and
        // =sync drains after every push (debug modes).
        let worker_enabled =
            !std::env::var("RUZU_VK_SUBMIT_WORKER").is_ok_and(|v| v == "0" || v == "off");
        let (submit_worker, submit_worker_thread) =
            if let (true, Some(timeline)) = (worker_enabled, timeline_semaphore) {
                let worker = Arc::new(SubmitWorker::new());
                let thread_worker = Arc::clone(&worker);
                let thread_device = device.clone();
                let thread_mutex = Arc::clone(&submit_mutex);
                let handle = std::thread::Builder::new()
                    .name("VulkanWorker".into())
                    .spawn(move || {
                        thread_worker.run(thread_device, queue, timeline, thread_mutex);
                    })
                    .expect("Failed to spawn Vulkan submit worker");
                (Some(worker), Some(handle))
            } else {
                (None, None)
            };

        let mut scheduler = Self {
            device,
            queue,
            command_pool,
            current_chunk: CommandChunk::new(),
            current_submit_command_count: 0,
            current_submit_draw_count: 0,
            current_cmdbuf: vk::CommandBuffer::null(),
            upload_cmdbuf: vk::CommandBuffer::null(),
            current_tick: AtomicU64::new(0),
            rp_state: RenderPassState::default(),
            state: SchedulerState::default(),
            fence,
            timeline_semaphore,
            submit_mutex,
            state_tracker: None,
            submit_worker,
            submit_worker_thread,
        };
        scheduler.allocate_new_context()?;
        Ok(scheduler)
    }

    pub fn submit_mutex(&self) -> Arc<Mutex<()>> {
        Arc::clone(&self.submit_mutex)
    }

    pub fn set_state_tracker(&mut self, state_tracker: NonNull<StateTracker>) {
        self.state_tracker = Some(state_tracker);
    }

    pub fn note_guest_draw(&mut self) {
        if submit_profile_enabled() {
            self.current_submit_draw_count += 1;
        }
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
        if std::env::var_os("RUZU_TRACE_B200_SOURCE_LIFECYCLE").is_some() {
            eprintln!(
                "[B200_SCHED_BEGIN] framebuffer=0x{:X} renderpass=0x{:X} images={:?}",
                framebuffer.as_raw(),
                renderpass.as_raw(),
                images
                    .iter()
                    .map(|image| image.as_raw())
                    .collect::<Vec<_>>(),
            );
        }
    }

    /// End the current render pass if inside one.
    pub fn request_outside_renderpass(&mut self) {
        if !self.rp_state.inside_renderpass {
            return;
        }

        trace!("Scheduler: ending render pass");
        if std::env::var_os("RUZU_TRACE_B200_SOURCE_LIFECYCLE").is_some() {
            eprintln!(
                "[B200_SCHED_END] framebuffer=0x{:X} renderpass=0x{:X} images={:?}",
                self.rp_state.framebuffer.as_raw(),
                self.rp_state.renderpass.as_raw(),
                self.rp_state
                    .images
                    .iter()
                    .map(|image| image.as_raw())
                    .collect::<Vec<_>>(),
            );
        }
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
        if submit_profile_enabled() {
            self.current_submit_command_count += chunk.commands.len();
        }
        for cmd in chunk.commands {
            cmd(self.current_cmdbuf, self.upload_cmdbuf);
        }
    }

    /// Port of upstream `Scheduler::WaitWorker`.
    ///
    /// Command chunks are replayed synchronously in the current Rust
    /// scheduler, while queue submission may run on `SubmitWorker`; both
    /// stages must be drained before a synchronous presentation consumes the
    /// submitted binary semaphore.
    pub fn wait_worker(&mut self) {
        self.dispatch_work();
        if let Some(worker) = self.submit_worker.as_ref() {
            worker.wait_drained();
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
        // Port of upstream `Scheduler::SubmitExecution`: bindings and dynamic
        // state recorded in this command buffer do not carry into the next
        // one. Mark the command-buffer-scoped state dirty before submission so
        // the next draw re-emits it after `allocate_new_context`.
        self.invalidate_state();

        // End command buffers
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
            self.device.end_command_buffer(self.upload_cmdbuf).ok();
            self.device.end_command_buffer(self.current_cmdbuf).ok();
        }

        // Submit both command buffers (upload first, then render)
        let cmd_buffers = [self.upload_cmdbuf, self.current_cmdbuf];

        let tick = self.current_tick.fetch_add(1, Ordering::SeqCst) + 1;
        if let Some(worker) = self.submit_worker.as_ref() {
            // Hand the fully recorded pair to the submit worker: on MoltenVK
            // vkQueueSubmit performs all Metal encoding, so this moves ~40%
            // of the GPU thread's loading-phase cost off the critical path
            // (upstream runs its whole chunk replay + submit on
            // Scheduler::WorkerThread). Timeline waiters are unaffected
            // (wait-before-signal is legal for timeline semaphores and jobs
            // are FIFO).
            worker.push(SubmitJob {
                render_cmdbuf: self.current_cmdbuf,
                upload_cmdbuf: self.upload_cmdbuf,
                signal_semaphores: signal_semaphores.to_vec(),
                tick,
                command_count: self.current_submit_command_count,
                draw_count: self.current_submit_draw_count,
            });
            if !signal_semaphores.is_empty() || submit_worker_sync_mode() {
                // BINARY semaphores (e.g. the present manager's render_ready)
                // do NOT allow wait-before-signal: the consumer may submit a
                // batch waiting on them as soon as we return, so the signal
                // must already be queued. Drain the worker on this path only —
                // it fires per presented frame, while the high-volume
                // syncpoint-fence flushes stay fully asynchronous.
                // RUZU_VK_SUBMIT_WORKER=sync drains after every push: a
                // debugging mode that keeps the submit on the worker THREAD
                // but removes the deferral WINDOW, to bisect the corruption
                // race between "window" and "thread" causes.
                worker.wait_drained();
            }
        } else if let Some(timeline) = self.timeline_semaphore {
            // Upstream `MasterSemaphore::SubmitQueue`: signal the timeline
            // semaphore with this submission's tick and pipeline submissions
            // without waiting on the previous one. Each flush records into
            // freshly allocated command buffers, so nothing here is reused
            // while the GPU is still executing.
            let mut all_signals = Vec::with_capacity(1 + signal_semaphores.len());
            all_signals.push(timeline);
            all_signals.extend_from_slice(signal_semaphores);
            let mut signal_values = vec![0u64; all_signals.len()];
            signal_values[0] = tick;
            let mut timeline_info =
                vk::TimelineSemaphoreSubmitInfo::builder().signal_semaphore_values(&signal_values);
            let submit_info = vk::SubmitInfo::builder()
                .command_buffers(&cmd_buffers)
                .signal_semaphores(&all_signals)
                .push_next(&mut timeline_info)
                .build();
            let submit_start = submit_profile_enabled().then(std::time::Instant::now);
            let submit_result = unsafe {
                let _submit_lock = self.submit_mutex.lock().unwrap();
                self.device
                    .queue_submit(self.queue, &[submit_info], vk::Fence::null())
            };
            if let Some(start) = submit_start {
                let elapsed_us = start.elapsed().as_micros() as u64;
                if elapsed_us >= 100_000
                    || !signal_semaphores.is_empty()
                    || self.current_submit_draw_count != 0
                {
                    eprintln!(
                        "[VK_SUBMIT_PROFILE] tick={} elapsed_us={} commands={} draws={} binary_signals={}",
                        tick,
                        elapsed_us,
                        self.current_submit_command_count,
                        self.current_submit_draw_count,
                        signal_semaphores.len(),
                    );
                }
            }
            let _ = submit_result;
        } else {
            let submit_info = vk::SubmitInfo::builder()
                .command_buffers(&cmd_buffers)
                .signal_semaphores(signal_semaphores)
                .build();
            let submit_start = submit_profile_enabled().then(std::time::Instant::now);
            unsafe {
                self.device
                    .wait_for_fences(&[self.fence], true, u64::MAX)
                    .ok();
                self.device.reset_fences(&[self.fence]).ok();
                let _submit_lock = self.submit_mutex.lock().unwrap();
                self.device
                    .queue_submit(self.queue, &[submit_info], self.fence)
                    .ok();
            }
            if let Some(start) = submit_start {
                let elapsed_us = start.elapsed().as_micros() as u64;
                if elapsed_us >= 100_000
                    || !signal_semaphores.is_empty()
                    || self.current_submit_draw_count != 0
                {
                    eprintln!(
                        "[VK_SUBMIT_PROFILE] tick={} elapsed_us={} commands={} draws={} binary_signals={}",
                        tick,
                        elapsed_us,
                        self.current_submit_command_count,
                        self.current_submit_draw_count,
                        signal_semaphores.len(),
                    );
                }
            }
        }

        debug!("Scheduler: flushed at tick {}", tick);
        self.current_submit_command_count = 0;
        self.current_submit_draw_count = 0;
        self.allocate_new_context().ok();
        tick
    }

    /// Flush + wait for GPU completion.
    pub fn finish(&mut self) {
        let tick = self.flush();
        self.wait(tick);
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

    /// Last tick the GPU has fully completed.
    ///
    /// Port of upstream `MasterSemaphore::KnownGpuTick`. Delayed-destruction
    /// rings must retire against this value, not against the submission tick:
    /// with pipelined submissions the CPU-side tick runs ahead of the GPU.
    pub fn known_gpu_tick(&self) -> u64 {
        if let Some(timeline) = self.timeline_semaphore {
            return unsafe {
                self.device
                    .get_semaphore_counter_value(timeline)
                    .unwrap_or(0)
            };
        }
        // Legacy single-submission fallback: at most one submission is in
        // flight, matching the pre-timeline retirement behaviour.
        self.current_tick()
    }

    /// Returns true when the GPU has completed `tick`.
    ///
    /// Port-facing subset of upstream `Scheduler::IsFree`. This simplified
    /// scheduler reuses a single fence, waiting on it before every new submit;
    /// older ticks are therefore complete once a newer tick exists.
    pub fn is_free(&self, tick: u64) -> bool {
        if tick == 0 {
            return true;
        }
        if let Some(timeline) = self.timeline_semaphore {
            // Upstream `MasterSemaphore::IsFree`: the GPU passed `tick` once
            // the timeline counter reaches it.
            return unsafe {
                self.device
                    .get_semaphore_counter_value(timeline)
                    .map(|value| value >= tick)
                    .unwrap_or(false)
            };
        }
        if tick < self.current_tick() {
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
        if tick > self.current_tick() {
            // The tick has not been submitted yet; flush so it will signal.
            self.flush();
        }
        if let Some(timeline) = self.timeline_semaphore {
            let semaphores = [timeline];
            let values = [tick];
            let wait_info = vk::SemaphoreWaitInfo::builder()
                .semaphores(&semaphores)
                .values(&values)
                .build();
            unsafe {
                self.device.wait_semaphores(&wait_info, u64::MAX).ok();
            }
            return;
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
        // Drain and stop the submit worker before waiting on / destroying
        // the timeline semaphore: pending jobs still reference it.
        if let Some(worker) = self.submit_worker.take() {
            worker.wait_drained();
            worker.stop.store(true, Ordering::Release);
            worker.job_cv.notify_all();
            if let Some(handle) = self.submit_worker_thread.take() {
                let _ = handle.join();
            }
        }
        unsafe {
            if let Some(timeline) = self.timeline_semaphore {
                let tick = self.current_tick();
                if tick > 0 {
                    let semaphores = [timeline];
                    let values = [tick];
                    let wait_info = vk::SemaphoreWaitInfo::builder()
                        .semaphores(&semaphores)
                        .values(&values)
                        .build();
                    self.device.wait_semaphores(&wait_info, u64::MAX).ok();
                }
                self.device.destroy_semaphore(timeline, None);
            } else {
                self.device
                    .wait_for_fences(&[self.fence], true, u64::MAX)
                    .ok();
            }
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

    #[test]
    fn submit_worker_is_not_drained_while_submit_is_in_flight() {
        let mut state = SubmitWorkerState {
            jobs: VecDeque::new(),
            in_flight: 1,
        };
        assert!(!state.is_drained());

        state.in_flight = 0;
        assert!(state.is_drained());
    }
}
