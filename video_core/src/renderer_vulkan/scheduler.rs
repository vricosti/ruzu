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
use std::panic::Location;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Condvar, Mutex, OnceLock};

use super::command_pool::CommandPool;
use super::query_cache::{QueryRuntimeState, SamplesQueryState, TfbCounterState};
use super::state_tracker::StateTracker;

pub(crate) type SubmitCallback = Arc<dyn Fn() + Send + Sync>;

const COMMAND_CHUNK_CAPACITY: usize = 0x8000;
const NO_COMMAND: usize = usize::MAX;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct CommandTraceLocation {
    file: &'static str,
    line: u32,
}

fn vulkan_submit_trace_enabled() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| {
        std::env::var_os("RUZU_VK_TRACE_DRAWS")
            .is_some_and(|value| !value.is_empty() && value != "0")
    })
}

fn format_scheduler_submit_trace(
    tick: u64,
    signal_semaphore_count: usize,
    locations: &[CommandTraceLocation],
) -> String {
    let mut counts: Vec<(CommandTraceLocation, usize)> = Vec::new();
    for &location in locations {
        if let Some((_, count)) = counts.iter_mut().find(|(known, _)| *known == location) {
            *count += 1;
        } else {
            counts.push((location, 1));
        }
    }
    let callsites = counts
        .into_iter()
        .map(|(location, count)| {
            let file = location
                .file
                .rsplit_once("video_core/")
                .map_or(location.file, |(_, relative)| relative);
            format!("{file}:{}={count}", location.line)
        })
        .collect::<Vec<_>>()
        .join(",");
    format!(
        "[VK_SUBMIT_TRACE] tick={tick} commands={} signal_semaphores={signal_semaphore_count} callsites=[{callsites}]",
        locations.len()
    )
}

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
    trace_locations: Vec<CommandTraceLocation>,
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
            trace_locations: Vec::new(),
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
    signal_semaphores: Vec<vk::Semaphore>,
    tick: u64,
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

    /// Tick-based synchronization (simplified MasterSemaphore).
    current_tick: Arc<AtomicU64>,
    submitted_tick: Arc<AtomicU64>,

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

    /// Port of upstream `Scheduler::on_submit`.
    on_submit: Arc<Mutex<Option<SubmitCallback>>>,

    /// Upstream `Scheduler` owns a `StateTracker&` and invalidates command
    /// buffer state after helper draws. Some Rust construction paths still
    /// build a scheduler before a rasterizer state tracker exists, so this is
    /// installed by the rasterizer once both owners are allocated.
    state_tracker: Option<NonNull<StateTracker>>,

    /// Upstream keeps a `QueryCache*` here and closes ZPass before ending a
    /// render pass. Sharing only the samples streamer preserves that ordering
    /// without making the Rust rasterizer self-referential.
    samples_query_state: Option<Arc<parking_lot::Mutex<SamplesQueryState>>>,
    tfb_query_state: Option<Arc<parking_lot::Mutex<TfbCounterState>>>,
    query_runtime_state: Option<Arc<parking_lot::Mutex<QueryRuntimeState>>>,

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

/// Stable synchronization subset of `Scheduler` used by Vulkan fences.
///
/// Upstream `InnerFence` stores a `Scheduler&`. The Rust rasterizer owns the
/// scheduler by value, so fences retain clones of only the scheduler-owned
/// synchronization objects instead of a pointer into a movable owner.
#[derive(Clone)]
pub(crate) struct SchedulerWaitHandle {
    device: ash::Device,
    timeline_semaphore: Option<vk::Semaphore>,
    fence: vk::Fence,
    worker: Arc<SchedulerWorker>,
}

impl SchedulerWaitHandle {
    pub(crate) fn wait(&self, tick: u64) {
        if tick == 0 {
            return;
        }
        if let Some(timeline) = self.timeline_semaphore {
            let semaphores = [timeline];
            let values = [tick];
            let wait_info = vk::SemaphoreWaitInfo::builder()
                .semaphores(&semaphores)
                .values(&values)
                .build();
            if let Err(error) = unsafe { self.device.wait_semaphores(&wait_info, u64::MAX) } {
                log::error!("Vulkan fence failed waiting for scheduler tick {tick}: {error:?}");
            }
            return;
        }

        self.worker.wait_drained();
        if let Err(error) = unsafe { self.device.wait_for_fences(&[self.fence], true, u64::MAX) } {
            log::error!("Vulkan fence failed waiting for scheduler fence: {error:?}");
        }
    }
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
    device_fault: Option<vk::ExtDeviceFaultFn>,
    device_fault_reported: bool,
    queue: vk::Queue,
    command_pool: CommandPool,
    current_cmdbuf: vk::CommandBuffer,
    upload_cmdbuf: vk::CommandBuffer,
    timeline_semaphore: Option<vk::Semaphore>,
    fence: vk::Fence,
    submit_mutex: Arc<Mutex<()>>,
    current_tick: Arc<AtomicU64>,
    submitted_tick: Arc<AtomicU64>,
    on_submit: Arc<Mutex<Option<SubmitCallback>>>,
    pending_trace_locations: Vec<CommandTraceLocation>,
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
            context
                .pending_trace_locations
                .append(&mut chunk.trace_locations);
            if let Some(submit) = submit {
                if vulkan_submit_trace_enabled() {
                    log::info!(
                        "{}",
                        format_scheduler_submit_trace(
                            submit.tick,
                            submit.signal_semaphores.len(),
                            &context.pending_trace_locations,
                        )
                    );
                }
                context.pending_trace_locations.clear();
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
    fn report_device_fault(&mut self) {
        if self.device_fault_reported {
            return;
        }
        self.device_fault_reported = true;
        let Some(extension) = self.device_fault.as_ref() else {
            return;
        };
        let mut counts = vk::DeviceFaultCountsEXT::default();
        let first = unsafe {
            (extension.get_device_fault_info_ext)(
                self.device.handle(),
                &mut counts,
                std::ptr::null_mut(),
            )
        };
        if first != vk::Result::SUCCESS {
            log::error!("vkGetDeviceFaultInfoEXT count query failed: {first:?}");
            return;
        }
        let mut addresses =
            vec![vk::DeviceFaultAddressInfoEXT::default(); counts.address_info_count as usize];
        let mut vendors =
            vec![vk::DeviceFaultVendorInfoEXT::default(); counts.vendor_info_count as usize];
        let mut vendor_binary = vec![0u8; counts.vendor_binary_size as usize];
        let mut info = vk::DeviceFaultInfoEXT::default();
        info.p_address_infos = addresses.as_mut_ptr();
        info.p_vendor_infos = vendors.as_mut_ptr();
        info.p_vendor_binary_data = vendor_binary.as_mut_ptr().cast();
        let second = unsafe {
            (extension.get_device_fault_info_ext)(self.device.handle(), &mut counts, &mut info)
        };
        if second != vk::Result::SUCCESS {
            log::error!("vkGetDeviceFaultInfoEXT detail query failed: {second:?}");
            return;
        }
        let description = unsafe { std::ffi::CStr::from_ptr(info.description.as_ptr()) };
        log::error!(
            "Vulkan device fault: description={} addresses={} vendors={} vendor_binary_size={}",
            description.to_string_lossy(),
            counts.address_info_count,
            counts.vendor_info_count,
            counts.vendor_binary_size
        );
        for (index, address) in addresses.iter().enumerate() {
            log::error!(
                "Vulkan device fault address[{index}]: type={:?} reported=0x{:016X} precision=0x{:016X}",
                address.address_type,
                address.reported_address,
                address.address_precision
            );
        }
        for (index, vendor) in vendors.iter().enumerate() {
            let description = unsafe { std::ffi::CStr::from_ptr(vendor.description.as_ptr()) };
            log::error!(
                "Vulkan device fault vendor[{index}]: description={} code=0x{:016X} data=0x{:016X}",
                description.to_string_lossy(),
                vendor.vendor_fault_code,
                vendor.vendor_fault_data
            );
        }
    }

    fn known_gpu_tick(&self) -> u64 {
        if let Some(timeline) = self.timeline_semaphore {
            return unsafe {
                self.device
                    .get_semaphore_counter_value(timeline)
                    .unwrap_or(0)
            };
        }
        let submitted_tick = self.submitted_tick.load(Ordering::SeqCst);
        if submitted_tick == 0
            || unsafe { self.device.get_fence_status(self.fence).unwrap_or(false) }
        {
            submitted_tick
        } else {
            submitted_tick - 1
        }
    }

    fn allocate_worker_command_buffer(&mut self) -> Result<(), vk::Result> {
        let known_gpu_tick = self.known_gpu_tick();
        let pending_tick = self.current_tick.load(Ordering::SeqCst) + 1;
        self.current_cmdbuf = self
            .command_pool
            .commit_with_ticks(known_gpu_tick, pending_tick);
        self.upload_cmdbuf = self
            .command_pool
            .commit_with_ticks(known_gpu_tick, pending_tick);
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

    fn submit_execution(&mut self, submit: &SubmitRequest) -> Result<(), vk::Result> {
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

        let callback = self.on_submit.lock().unwrap().clone();
        if let Some(callback) = callback {
            callback();
        }

        let cmd_buffers = [self.upload_cmdbuf, self.current_cmdbuf];
        if let Some(timeline) = self.timeline_semaphore {
            let mut all_signals = Vec::with_capacity(1 + submit.signal_semaphores.len());
            all_signals.push(timeline);
            all_signals.extend_from_slice(&submit.signal_semaphores);
            let mut signal_values = vec![0u64; all_signals.len()];
            signal_values[0] = submit.tick;
            let mut timeline_info =
                vk::TimelineSemaphoreSubmitInfo::builder().signal_semaphore_values(&signal_values);
            let submit_info = vk::SubmitInfo::builder()
                .command_buffers(&cmd_buffers)
                .signal_semaphores(&all_signals)
                .push_next(&mut timeline_info)
                .build();
            let _submit_lock = self.submit_mutex.lock().unwrap();
            let result = unsafe {
                self.device
                    .queue_submit(self.queue, &[submit_info], vk::Fence::null())
            };
            drop(_submit_lock);
            if result.is_ok() {
                self.submitted_tick.store(submit.tick, Ordering::SeqCst);
            }
            if result == Err(vk::Result::ERROR_DEVICE_LOST) {
                self.report_device_fault();
            }
            result
        } else {
            let submit_info = vk::SubmitInfo::builder()
                .command_buffers(&cmd_buffers)
                .signal_semaphores(&submit.signal_semaphores)
                .build();
            let result = unsafe {
                self.device.wait_for_fences(&[self.fence], true, u64::MAX)?;
                self.device.reset_fences(&[self.fence])?;
                let _submit_lock = self.submit_mutex.lock().unwrap();
                self.device
                    .queue_submit(self.queue, &[submit_info], self.fence)
            };
            if result.is_ok() {
                self.submitted_tick.store(submit.tick, Ordering::SeqCst);
            }
            if result == Err(vk::Result::ERROR_DEVICE_LOST) {
                self.report_device_fault();
            }
            result
        }
    }

    fn wait_for_gpu(&self) {
        let tick = self.current_tick.load(Ordering::SeqCst);
        if tick == 0 {
            return;
        }
        unsafe {
            if let Some(timeline) = self.timeline_semaphore {
                let semaphores = [timeline];
                let values = [tick];
                let wait_info = vk::SemaphoreWaitInfo::builder()
                    .semaphores(&semaphores)
                    .values(&values)
                    .build();
                self.device.wait_semaphores(&wait_info, u64::MAX).ok();
            } else {
                self.device
                    .wait_for_fences(&[self.fence], true, u64::MAX)
                    .ok();
            }
        }
    }
}

impl Scheduler {
    /// Create a new scheduler.
    pub fn new(
        device: ash::Device,
        queue: vk::Queue,
        graphics_family: u32,
        timeline_semaphore_supported: bool,
        device_fault: Option<vk::ExtDeviceFaultFn>,
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
        let current_tick = Arc::new(AtomicU64::new(0));
        let submitted_tick = Arc::new(AtomicU64::new(0));
        let on_submit = Arc::new(Mutex::new(None));
        let worker = Arc::new(SchedulerWorker::new());
        let mut worker_context = WorkerContext {
            device: device.clone(),
            device_fault,
            device_fault_reported: false,
            queue,
            command_pool: CommandPool::new_with_external_ticks(device.clone(), graphics_family),
            current_cmdbuf: vk::CommandBuffer::null(),
            upload_cmdbuf: vk::CommandBuffer::null(),
            timeline_semaphore,
            fence,
            submit_mutex: Arc::clone(&submit_mutex),
            current_tick: Arc::clone(&current_tick),
            submitted_tick: Arc::clone(&submitted_tick),
            on_submit: Arc::clone(&on_submit),
            pending_trace_locations: Vec::new(),
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
            current_tick,
            submitted_tick,
            rp_state: RenderPassState::default(),
            state: SchedulerState::default(),
            fence,
            timeline_semaphore,
            submit_mutex,
            on_submit,
            state_tracker: None,
            samples_query_state: None,
            tfb_query_state: None,
            query_runtime_state: None,
            worker: Some(worker),
            worker_thread: Some(worker_thread),
        })
    }

    pub fn submit_mutex(&self) -> Arc<Mutex<()>> {
        Arc::clone(&self.submit_mutex)
    }

    /// Port of upstream `Scheduler::RegisterOnSubmit`.
    pub(crate) fn register_on_submit(&mut self, callback: Option<SubmitCallback>) {
        *self.on_submit.lock().unwrap() = callback;
    }

    pub(crate) fn wait_handle(&self) -> SchedulerWaitHandle {
        SchedulerWaitHandle {
            device: self.device.clone(),
            timeline_semaphore: self.timeline_semaphore,
            fence: self.fence,
            worker: Arc::clone(
                self.worker
                    .as_ref()
                    .expect("scheduler worker must exist while fences are active"),
            ),
        }
    }

    pub fn set_state_tracker(&mut self, state_tracker: NonNull<StateTracker>) {
        self.state_tracker = Some(state_tracker);
    }

    pub(crate) fn set_samples_query_state(
        &mut self,
        state: Arc<parking_lot::Mutex<SamplesQueryState>>,
    ) {
        self.samples_query_state = Some(state);
    }

    pub(crate) fn set_query_runtime_state(
        &mut self,
        state: Arc<parking_lot::Mutex<QueryRuntimeState>>,
    ) {
        self.query_runtime_state = Some(state);
    }

    pub(crate) fn set_tfb_query_state(&mut self, state: Arc<parking_lot::Mutex<TfbCounterState>>) {
        self.tfb_query_state = Some(state);
    }

    /// Record a command that only needs the render command buffer.
    #[track_caller]
    pub fn record(&mut self, cmd: impl FnOnce(vk::CommandBuffer) + Send + 'static) {
        self.record_with_upload(move |render_cmd, _upload_cmd| cmd(render_cmd));
    }

    /// Record a command that needs both render and upload command buffers.
    #[track_caller]
    pub fn record_with_upload(
        &mut self,
        cmd: impl FnOnce(vk::CommandBuffer, vk::CommandBuffer) + Send + 'static,
    ) {
        let trace_location = if vulkan_submit_trace_enabled() {
            let caller = Location::caller();
            Some(CommandTraceLocation {
                file: caller.file(),
                line: caller.line(),
            })
        } else {
            None
        };
        let command = match self.current_chunk.record(cmd) {
            Ok(()) => {
                self.current_chunk.trace_locations.extend(trace_location);
                return;
            }
            Err(command) => command,
        };
        self.dispatch_work();
        if self.current_chunk.record(command).is_err() {
            panic!("Vulkan scheduler command exceeds the 32 KiB command chunk");
        }
        self.current_chunk.trace_locations.extend(trace_location);
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
        if let Some(state) = self.tfb_query_state.as_ref().cloned() {
            state.lock().close_counter(self);
        }
        if let Some(state) = self.samples_query_state.as_ref().cloned() {
            state.lock().pause_counter(self);
        }
        if let Some(state) = self.query_runtime_state.as_ref().cloned() {
            let conditional_rendering = state.lock().pause_host_conditional_rendering();
            if let Some(conditional_rendering) = conditional_rendering {
                self.record(move |cmdbuf| unsafe {
                    (conditional_rendering.cmd_end_conditional_rendering_ext)(cmdbuf);
                });
            }
        }
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
        self.end_pending_operations();
        self.invalidate_state();
        let tick = self.current_tick.fetch_add(1, Ordering::SeqCst) + 1;
        self.current_chunk.submit = Some(SubmitRequest {
            signal_semaphores: signal_semaphores.to_vec(),
            tick,
        });
        self.dispatch_work();
        if !signal_semaphores.is_empty() {
            self.worker
                .as_ref()
                .expect("scheduler worker must exist")
                .wait_drained();
        }
        debug!("Scheduler: flushed at tick {}", tick);
        self.rp_state = RenderPassState::default();
        tick
    }

    /// Port of upstream `Scheduler::EndPendingOperations`.
    fn end_pending_operations(&mut self) {
        if let Some(state) = self.samples_query_state.as_ref().cloned() {
            state.lock().reset_counter(self);
        }
        self.request_outside_renderpass();
    }

    /// Flush + wait for GPU completion.
    pub fn finish(&mut self) {
        let tick = self.flush();
        self.wait(tick);
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
        // Legacy single-submission fallback: the current tick remains in
        // flight until the fence is signalled. Older ticks completed before
        // that submission because this path waits on the same fence before
        // each queue submit.
        let submitted_tick = self.submitted_tick.load(Ordering::SeqCst);
        if submitted_tick == 0
            || unsafe { self.device.get_fence_status(self.fence).unwrap_or(false) }
        {
            submitted_tick
        } else {
            submitted_tick - 1
        }
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
        let submitted_tick = self.submitted_tick.load(Ordering::SeqCst);
        if tick > submitted_tick {
            return false;
        }
        if tick < submitted_tick {
            return true;
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
        self.wait_worker();
        unsafe {
            self.device
                .wait_for_fences(&[self.fence], true, u64::MAX)
                .ok();
        }
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
    fn scheduler_submit_trace_groups_callsites_without_losing_command_count() {
        let locations = [
            CommandTraceLocation {
                file: "video_core/src/renderer_vulkan/buffer_cache.rs",
                line: 42,
            },
            CommandTraceLocation {
                file: "video_core/src/renderer_vulkan/vk_rasterizer.rs",
                line: 84,
            },
            CommandTraceLocation {
                file: "video_core/src/renderer_vulkan/buffer_cache.rs",
                line: 42,
            },
        ];

        assert_eq!(
            format_scheduler_submit_trace(1698, 1, &locations),
            "[VK_SUBMIT_TRACE] tick=1698 commands=3 signal_semaphores=1 callsites=[src/renderer_vulkan/buffer_cache.rs:42=2,src/renderer_vulkan/vk_rasterizer.rs:84=1]"
        );
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
            signal_semaphores: Vec::new(),
            tick: 7,
        });
        let mut second = CommandChunk::new();
        second.submit = Some(SubmitRequest {
            signal_semaphores: Vec::new(),
            tick: 8,
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
