// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/dma_pusher.h and video_core/dma_pusher.cpp
//!
//! DMA command submission to FIFOs, assembling pushbuffers into a command stream.
//! See https://envytools.readthedocs.io/en/latest/hw/fifo/dma-pusher.html

use std::collections::VecDeque;
use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};
use std::sync::{Arc, OnceLock};
use std::time::Instant;

use crate::engines::engine_interface::EngineTypes;
use crate::engines::engine_interface::{EngineHandle, EngineInterface};
use crate::engines::puller::{EngineID, MethodCall, Puller};
use common::settings;
use parking_lot::Mutex;
use ruzu_core::core::SystemRef;

static DMA_FLOW_TRACE_COUNT: AtomicU32 = AtomicU32::new(0);
static DMA_FLOW_DISPATCH_TRACE_COUNT: AtomicU32 = AtomicU32::new(0);
static COMMAND_WORDS_TRACE_COUNT: AtomicU32 = AtomicU32::new(0);

fn should_trace_dma_flow() -> bool {
    static CACHED: OnceLock<bool> = OnceLock::new();
    *CACHED.get_or_init(|| std::env::var_os("RUZU_TRACE_DMA_FLOW").is_some())
}

fn elapsed_us(start: Instant) -> u64 {
    start.elapsed().as_micros().min(u128::from(u64::MAX)) as u64
}

fn elapsed_us_opt(start: Option<Instant>) -> u64 {
    start.map(elapsed_us).unwrap_or(0)
}

fn dma_method_trace_min_us() -> u64 {
    static CACHED: OnceLock<u64> = OnceLock::new();
    *CACHED.get_or_init(|| {
        std::env::var("RUZU_TRACE_DMA_METHOD_MIN_US")
            .ok()
            .and_then(|value| value.parse::<u64>().ok())
            .unwrap_or(500)
    })
}

fn should_trace_dma_method_inflight() -> bool {
    static CACHED: OnceLock<bool> = OnceLock::new();
    *CACHED.get_or_init(|| std::env::var_os("RUZU_TRACE_DMA_METHOD_INFLIGHT").is_some())
}

fn trace_dma_pusher_method_inflight(
    stage: u64,
    dispatch_index: u64,
    command_index: usize,
    command_count: usize,
    subchannel: u32,
    method: u32,
    argument_or_words: u32,
    pending: u32,
    dma_segment: u64,
    elapsed_us: u64,
) {
    if !common::trace::is_enabled(common::trace::cat::DMA_PUSHER) {
        return;
    }
    if !should_trace_dma_method_inflight() {
        return;
    }
    common::trace::emit_raw(
        common::trace::cat::DMA_PUSHER,
        &[
            stage,
            dispatch_index,
            command_index as u64,
            command_count as u64,
            subchannel as u64,
            method as u64,
            argument_or_words as u64,
            pending as u64,
            dma_segment,
            elapsed_us,
        ],
    );
}

fn trace_dma_pusher_method_agg(
    command_count: usize,
    total_dispatches: u64,
    subchannel: u32,
    method: u32,
    calls: u64,
    words: u64,
    elapsed_us: u64,
    dma_get: u64,
) {
    if !common::trace::is_enabled(common::trace::cat::DMA_PUSHER) {
        return;
    }
    if elapsed_us < dma_method_trace_min_us() {
        return;
    }
    common::trace::emit_raw(
        common::trace::cat::DMA_PUSHER,
        &[
            9,
            command_count as u64,
            total_dispatches,
            subchannel as u64,
            method as u64,
            calls,
            words,
            elapsed_us,
            dma_get,
        ],
    );
}

fn record_dma_method_time(
    method_times: &mut Vec<(u32, u32, u64, u64, u64)>,
    subchannel: u32,
    method: u32,
    calls: u64,
    words: u64,
    elapsed_us: u64,
) {
    if elapsed_us == 0 {
        return;
    }
    if let Some((_, _, existing_calls, existing_words, existing_elapsed)) = method_times
        .iter_mut()
        .find(|(entry_subchannel, entry_method, _, _, _)| {
            *entry_subchannel == subchannel && *entry_method == method
        })
    {
        *existing_calls += calls;
        *existing_words += words;
        *existing_elapsed += elapsed_us;
        return;
    }
    method_times.push((subchannel, method, calls, words, elapsed_us));
}

fn trace_dma_pusher_step(
    stage: u64,
    queue_len: usize,
    subindex: usize,
    addr: u64,
    size: u32,
    non_main: bool,
    method_count: u32,
    dma_get: u64,
    elapsed_us: u64,
) {
    if !common::trace::is_enabled(common::trace::cat::DMA_PUSHER) {
        return;
    }
    common::trace::emit_raw(
        common::trace::cat::DMA_PUSHER,
        &[
            stage,
            queue_len as u64,
            subindex as u64,
            addr,
            size as u64,
            non_main as u64,
            method_count as u64,
            dma_get,
            elapsed_us,
        ],
    );
}

fn should_trace_puller() -> bool {
    static CACHED: OnceLock<bool> = OnceLock::new();
    *CACHED.get_or_init(|| std::env::var_os("RUZU_TRACE_PULLER").is_some())
}

fn should_trace_dma_count() -> bool {
    static CACHED: OnceLock<bool> = OnceLock::new();
    *CACHED.get_or_init(|| std::env::var_os("RUZU_TRACE_DMA_COUNT").is_some())
}

fn command_words_trace_limit() -> Option<u32> {
    static LIMIT: OnceLock<Option<u32>> = OnceLock::new();
    *LIMIT.get_or_init(|| {
        std::env::var("RUZU_TRACE_COMMAND_WORDS")
            .ok()
            .map(|value| value.parse::<u32>().unwrap_or(64))
    })
}

fn command_words_preview_len() -> usize {
    static LEN: OnceLock<usize> = OnceLock::new();
    *LEN.get_or_init(|| {
        std::env::var("RUZU_TRACE_COMMAND_WORDS_LEN")
            .ok()
            .and_then(|value| value.parse::<usize>().ok())
            .unwrap_or(32)
            .clamp(1, 256)
    })
}

fn command_words_find_value() -> Option<u32> {
    static VALUE: OnceLock<Option<u32>> = OnceLock::new();
    *VALUE.get_or_init(|| {
        std::env::var("RUZU_FIND_COMMAND_WORD")
            .ok()
            .and_then(|value| u32::from_str_radix(value.trim().trim_start_matches("0x"), 16).ok())
    })
}

/// Per-submit method-dispatch trace gate. `RUZU_TRACE_PULLER_SUBMITS=N` logs
/// every dispatch_method/dispatch_multi_method call for the first N submits
/// crossing `Scheduler::push`. Used to find missing GPU-method dispatches in
/// the wedge between MK8D's 2nd and 3rd SubmitGPFIFO ioctls.
pub static CURRENT_SUBMIT_INDEX: AtomicU64 = AtomicU64::new(0);

/// Index of the submit currently being dispatched by `Scheduler::push`, or
/// `i64::MIN` when no traced submit is active. Set/cleared around
/// `dma_pusher.dispatch_calls()` so `current_submit_traced()` reads the
/// correct index for the in-flight method dispatches.
pub static ACTIVE_SUBMIT_IDX: std::sync::atomic::AtomicI64 =
    std::sync::atomic::AtomicI64::new(i64::MIN);

pub fn puller_trace_submits_limit() -> Option<u64> {
    static V: OnceLock<Option<u64>> = OnceLock::new();
    *V.get_or_init(|| {
        std::env::var("RUZU_TRACE_PULLER_SUBMITS")
            .ok()
            .and_then(|s| s.parse::<u64>().ok())
    })
}

pub fn current_submit_traced() -> Option<u64> {
    let v = ACTIVE_SUBMIT_IDX.load(Ordering::Relaxed);
    if v < 0 {
        None
    } else {
        Some(v as u64)
    }
}

/// GPU virtual address type.
pub type GPUVAddr = u64;

/// DMA submission modes for command headers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SubmissionMode {
    IncreasingOld = 0,
    Increasing = 1,
    NonIncreasingOld = 2,
    NonIncreasing = 3,
    Inline = 4,
    IncreaseOnce = 5,
}

impl SubmissionMode {
    pub fn from_u32(val: u32) -> Option<Self> {
        match val {
            0 => Some(Self::IncreasingOld),
            1 => Some(Self::Increasing),
            2 => Some(Self::NonIncreasingOld),
            3 => Some(Self::NonIncreasing),
            4 => Some(Self::Inline),
            5 => Some(Self::IncreaseOnce),
            _ => None,
        }
    }
}

/// Buffer methods used by the DMA pusher (register addresses).
///
/// Note: methods are treated as 4-byte addressable locations, values here are NOT
/// multiplied by 4. Docs may show values multiplied by 4.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum BufferMethods {
    BindObject = 0x0,
    Illegal = 0x1,
    Nop = 0x2,
    SemaphoreAddressHigh = 0x4,
    SemaphoreAddressLow = 0x5,
    SemaphoreSequencePayload = 0x6,
    SemaphoreOperation = 0x7,
    NonStallInterrupt = 0x8,
    WrcacheFlush = 0x9,
    MemOpA = 0xA,
    MemOpB = 0xB,
    MemOpC = 0xC,
    MemOpD = 0xD,
    RefCnt = 0x14,
    SemaphoreAcquire = 0x1A,
    SemaphoreRelease = 0x1B,
    SyncpointPayload = 0x1C,
    SyncpointOperation = 0x1D,
    WaitForIdle = 0x1E,
    CrcCheck = 0x1F,
    Yield = 0x20,
    NonPullerMethods = 0x40,
}

/// Command list header (64-bit), packed as a bitfield.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct CommandListHeader {
    pub raw: u64,
}

impl CommandListHeader {
    /// GPU address (bits 0..40).
    pub fn addr(&self) -> GPUVAddr {
        self.raw & ((1u64 << 40) - 1)
    }

    /// Whether this is a non-main entry (bit 41).
    pub fn is_non_main(&self) -> bool {
        (self.raw >> 41) & 1 != 0
    }

    /// Size in words (bits 42..63).
    pub fn size(&self) -> u32 {
        ((self.raw >> 42) & ((1u64 << 21) - 1)) as u32
    }
}

/// Command header (32-bit), packed as a bitfield.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct CommandHeader {
    pub raw: u32,
}

impl CommandHeader {
    pub fn argument(&self) -> u32 {
        self.raw
    }

    /// Method index (bits 0..13).
    pub fn method(&self) -> u32 {
        self.raw & 0x1FFF
    }

    /// Subchannel (bits 13..16).
    pub fn subchannel(&self) -> u32 {
        (self.raw >> 13) & 0x7
    }

    /// Argument/method count (bits 16..29).
    pub fn arg_count(&self) -> u32 {
        (self.raw >> 16) & 0x1FFF
    }

    /// Method count (same field as arg_count).
    pub fn method_count(&self) -> u32 {
        self.arg_count()
    }

    /// Submission mode (bits 29..32).
    pub fn mode(&self) -> Option<SubmissionMode> {
        SubmissionMode::from_u32((self.raw >> 29) & 0x7)
    }
}

/// Build a command header from parts.
pub fn build_command_header(
    method: BufferMethods,
    arg_count: u32,
    mode: SubmissionMode,
) -> CommandHeader {
    let raw = (method as u32 & 0x1FFF) | ((arg_count & 0x1FFF) << 16) | ((mode as u32 & 0x7) << 29);
    CommandHeader { raw }
}

/// A list of commands to be submitted to the DMA pusher.
#[derive(Debug, Default)]
pub struct CommandList {
    /// Indirect buffer entries (list of GPU addresses + sizes).
    pub command_lists: Vec<CommandListHeader>,
    /// Prefetched command list (used for synchronization).
    pub prefetch_command_list: Vec<CommandHeader>,
}

impl CommandList {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_size(size: usize) -> Self {
        Self {
            command_lists: Vec::with_capacity(size),
            prefetch_command_list: Vec::new(),
        }
    }

    pub fn from_prefetch(prefetch: Vec<CommandHeader>) -> Self {
        Self {
            command_lists: Vec::new(),
            prefetch_command_list: prefetch,
        }
    }
}

/// Constants matching upstream.
const NON_PULLER_METHODS: u32 = 0x40;
#[allow(dead_code)]
const MAX_SUBCHANNELS: usize = 8;
const MACRO_REGISTERS_START: u32 = 0xE00;
#[allow(dead_code)]
const COMPUTE_INLINE: u32 = 0x6D;
static DISPATCH_TRACE_COUNT: AtomicU32 = AtomicU32::new(0);

/// Internal DMA state tracking.
#[derive(Debug, Default)]
struct DmaState {
    method: u32,
    subchannel: u32,
    method_count: u32,
    #[allow(dead_code)]
    length_pending: u32,
    dma_get: GPUVAddr,
    dma_word_offset: u64,
    non_incrementing: bool,
    is_last_call: bool,
}

/// The DmaPusher implements DMA submission to FIFOs.
///
/// The pushbuffers are assembled into a "command stream" of 32-bit words.
/// In the full GPU integration, this holds references to GPU, MemoryManager,
/// and Puller. For now, engine dispatch is performed via callback closures
/// passed through the dispatch chain.
pub struct DmaPusher {
    dma_pushbuffer: VecDeque<CommandList>,
    dma_pushbuffer_subindex: usize,
    dma_state: DmaState,
    dma_increment_once: bool,
    ib_enable: bool,
    command_headers: Vec<CommandHeader>,
    subchannels: [Option<EngineHandle>; MAX_SUBCHANNELS],
    subchannel_type: [EngineTypes; MAX_SUBCHANNELS],
    gpu: *const crate::gpu::Gpu,
    system: SystemRef,
    memory_manager: Arc<Mutex<crate::memory_manager::MemoryManager>>,
    puller: Puller,
}

// Safety: `gpu` points back to the owning `Gpu`, which outlives the `DmaPusher`
// through `ChannelState`. `memory_manager` is already synchronized by `Arc<Mutex<_>>`.
unsafe impl Send for DmaPusher {}

impl DmaPusher {
    /// Creates a new DmaPusher.
    pub fn new(
        gpu: *const crate::gpu::Gpu,
        system: SystemRef,
        memory_manager: Arc<Mutex<crate::memory_manager::MemoryManager>>,
        channel_state: *mut crate::control::channel_state::ChannelState,
    ) -> Self {
        let puller_memory_manager = Arc::clone(&memory_manager);
        Self {
            dma_pushbuffer: VecDeque::new(),
            dma_pushbuffer_subindex: 0,
            dma_state: DmaState::default(),
            dma_increment_once: false,
            ib_enable: true,
            command_headers: Vec::new(),
            subchannels: [None; MAX_SUBCHANNELS],
            subchannel_type: [EngineTypes::Maxwell3D; MAX_SUBCHANNELS],
            gpu,
            system,
            memory_manager,
            puller: Puller::new(
                gpu,
                puller_memory_manager,
                std::ptr::null_mut(),
                channel_state,
            ),
        }
    }

    /// Install the stable boxed self pointer into the embedded puller.
    ///
    /// This must be called only after the `DmaPusher` has reached its final
    /// owning address. Doing it inside `new()` would capture a pre-move
    /// address and make `Puller::ProcessBindMethod()` write subchannel
    /// bindings into stale storage.
    pub fn install_self_reference(&mut self) {
        let self_ptr: *mut DmaPusher = self;
        self.puller.set_dma_pusher(self_ptr);
    }

    pub fn bind_rasterizer(
        &mut self,
        rasterizer: &dyn crate::rasterizer_interface::RasterizerInterface,
    ) {
        self.puller.bind_rasterizer(rasterizer);
    }

    pub fn bind_subchannel(
        &mut self,
        engine: &mut dyn EngineInterface,
        subchannel_id: u32,
        engine_type: EngineTypes,
    ) {
        self.subchannels[subchannel_id as usize] = Some(EngineHandle::from_ref(engine));
        self.subchannel_type[subchannel_id as usize] = engine_type;
    }

    /// Push a command list into the DMA pushbuffer queue.
    pub fn push(&mut self, entries: CommandList) {
        self.dma_pushbuffer.push_back(entries);
    }

    pub fn dma_pushbuffer_len(&self) -> usize {
        self.dma_pushbuffer.len()
    }

    /// Dispatch all pending command lists. Matches upstream `DmaPusher::DispatchCalls`.
    ///
    /// In the full port, this takes the system/GPU context. For now, the command
    /// processing and method dispatch are fully implemented; engine dispatch goes
    /// through the `subchannel` engine interface.
    pub fn dispatch_calls(&mut self) {
        trace_dma_pusher_step(
            1,
            self.dma_pushbuffer.len(),
            self.dma_pushbuffer_subindex,
            0,
            0,
            false,
            self.dma_state.method_count,
            self.dma_state.dma_get,
            0,
        );
        self.dma_pushbuffer_subindex = 0;
        self.dma_state.is_last_call = true;

        while self.system.is_null() || self.system.get().is_powered_on() {
            if !self.step() {
                break;
            }
        }

        let gpu = unsafe { &*self.gpu };
        gpu.flush_commands();
        gpu.on_command_list_end();
        trace_dma_pusher_step(
            2,
            self.dma_pushbuffer.len(),
            self.dma_pushbuffer_subindex,
            0,
            0,
            false,
            self.dma_state.method_count,
            self.dma_state.dma_get,
            0,
        );
    }

    /// Dispatch all pending command lists with an engine to receive method calls.
    /// This is the integration entry point for GPU subsystems.
    pub fn dispatch_calls_with_engine(&mut self, engine: &mut dyn EngineInterface) {
        self.dma_pushbuffer_subindex = 0;
        self.dma_state.is_last_call = true;

        while self.step_with_engine(engine) {}
    }

    fn set_current_engine_dirty(&mut self, dirty: bool) {
        let Some(engine_id) = self.puller.bound_engines[self.dma_state.subchannel as usize] else {
            return;
        };
        match engine_id {
            EngineID::MaxwellB => {
                if let Some(engine) = self.subchannels[self.dma_state.subchannel as usize] {
                    unsafe { engine.as_mut() }.set_current_dirty(dirty);
                }
            }
            EngineID::KeplerComputeB
            | EngineID::KeplerInlineToMemoryB
            | EngineID::FermiTwodA
            | EngineID::MaxwellDmaCopyA => {
                if let Some(engine) = self.subchannels[self.dma_state.subchannel as usize] {
                    unsafe { engine.as_mut() }.set_current_dirty(dirty);
                }
            }
        }
    }

    fn update_current_dirty_for_fetch(&mut self, command_gpu_addr: GPUVAddr, word_count: u32) {
        let needs_dirty_check = self.dma_state.method >= MACRO_REGISTERS_START
            || (self.puller.bound_engines[self.dma_state.subchannel as usize]
                == Some(EngineID::KeplerComputeB)
                && self.dma_state.method == COMPUTE_INLINE);
        if !needs_dirty_check {
            return;
        }

        let dirty = self.memory_manager.lock().is_memory_dirty(
            command_gpu_addr,
            word_count as u64 * std::mem::size_of::<u32>() as u64,
        );

        self.set_current_engine_dirty(dirty);
    }

    /// Process the next step of command submission. Matches upstream `DmaPusher::Step`.
    ///
    /// Without MemoryManager integration, only prefetched command lists can be
    /// processed. GPU-memory-resident command lists are deferred until the memory
    /// manager is wired in.
    fn step(&mut self) -> bool {
        if !self.ib_enable || self.dma_pushbuffer.is_empty() {
            return false;
        }
        trace_dma_pusher_step(
            3,
            self.dma_pushbuffer.len(),
            self.dma_pushbuffer_subindex,
            0,
            0,
            false,
            self.dma_state.method_count,
            self.dma_state.dma_get,
            0,
        );

        if should_trace_dma_flow() {
            let trace_idx = DMA_FLOW_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
            if trace_idx < 64 {
                log::info!(
                    "DmaPusher::step begin queue_len={} subindex={} method_count={} dma_get=0x{:X}",
                    self.dma_pushbuffer.len(),
                    self.dma_pushbuffer_subindex,
                    self.dma_state.method_count,
                    self.dma_state.dma_get
                );
            }
        }

        let command_list = match self.dma_pushbuffer.front() {
            Some(cl) => cl,
            None => return false,
        };

        if command_list.command_lists.is_empty() && command_list.prefetch_command_list.is_empty() {
            trace_dma_pusher_step(
                8,
                self.dma_pushbuffer.len(),
                self.dma_pushbuffer_subindex,
                0,
                0,
                false,
                self.dma_state.method_count,
                self.dma_state.dma_get,
                0,
            );
            self.dma_pushbuffer.pop_front();
            self.dma_pushbuffer_subindex = 0;
            return true;
        }

        if !command_list.prefetch_command_list.is_empty() {
            // Prefetched command list from nvdrv (synchronization etc.).
            let commands: Vec<CommandHeader> = command_list.prefetch_command_list.clone();
            trace_dma_pusher_step(
                4,
                self.dma_pushbuffer.len(),
                self.dma_pushbuffer_subindex,
                0,
                commands.len() as u32,
                false,
                self.dma_state.method_count,
                self.dma_state.dma_get,
                0,
            );
            if should_trace_dma_flow() {
                let trace_idx = DMA_FLOW_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
                if trace_idx < 64 {
                    log::info!(
                        "DmaPusher::step prefetch count={} queue_len={}",
                        commands.len(),
                        self.dma_pushbuffer.len()
                    );
                }
            }
            let trace_dma_pusher = common::trace::is_enabled(common::trace::cat::DMA_PUSHER);
            let process_start = trace_dma_pusher.then(Instant::now);
            self.process_commands(&commands);
            trace_dma_pusher_step(
                5,
                self.dma_pushbuffer.len(),
                self.dma_pushbuffer_subindex,
                0,
                commands.len() as u32,
                false,
                self.dma_state.method_count,
                self.dma_state.dma_get,
                elapsed_us_opt(process_start),
            );
            self.dma_pushbuffer.pop_front();
        } else {
            let command_list_header = command_list.command_lists[self.dma_pushbuffer_subindex];
            trace_dma_pusher_step(
                6,
                self.dma_pushbuffer.len(),
                self.dma_pushbuffer_subindex,
                command_list_header.addr(),
                command_list_header.size(),
                command_list_header.is_non_main(),
                self.dma_state.method_count,
                self.dma_state.dma_get,
                0,
            );
            if should_trace_dma_flow() {
                let trace_idx = DMA_FLOW_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
                if trace_idx < 64 {
                    log::info!(
                        "DmaPusher::step command_list addr=0x{:X} size={} non_main={} queue_len={} subindex={}",
                        command_list_header.addr(),
                        command_list_header.size(),
                        command_list_header.is_non_main(),
                        self.dma_pushbuffer.len(),
                        self.dma_pushbuffer_subindex
                    );
                }
            } else {
                log::trace!(
                    "DmaPusher::step command_list addr=0x{:X} size={} non_main={}",
                    command_list_header.addr(),
                    command_list_header.size(),
                    command_list_header.is_non_main()
                );
            }
            self.dma_pushbuffer_subindex += 1;
            self.dma_state.dma_get = command_list_header.addr();

            if self.dma_pushbuffer_subindex >= command_list.command_lists.len() {
                self.dma_pushbuffer.pop_front();
                self.dma_pushbuffer_subindex = 0;
            }

            if command_list_header.size() == 0 {
                return true;
            }

            self.update_current_dirty_for_fetch(
                command_list_header.addr(),
                command_list_header.size(),
            );

            let command_count = command_list_header.size() as usize;
            let raw = self.fetch_command_bytes(command_list_header.addr(), command_count);

            self.command_headers.clear();
            self.command_headers
                .extend(raw.chunks_exact(4).map(|chunk| CommandHeader {
                    raw: u32::from_le_bytes(chunk.try_into().unwrap()),
                }));
            let non_zero = self.command_headers.iter().filter(|h| h.raw != 0).count();
            if non_zero > 0 {
                if should_trace_dma_flow() {
                    let trace_idx = DMA_FLOW_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
                    if trace_idx < 64 {
                        log::info!(
                            "DmaPusher::step fetched count={} non_zero={} first=0x{:08X}",
                            self.command_headers.len(),
                            non_zero,
                            self.command_headers.first().map_or(0, |h| h.raw),
                        );
                    }
                } else {
                    log::trace!(
                        "DmaPusher::step {} commands ({} non-zero) first={:#010x}",
                        self.command_headers.len(),
                        non_zero,
                        self.command_headers.first().map_or(0, |h| h.raw),
                    );
                }
            }
            let trace_dma_pusher = common::trace::is_enabled(common::trace::cat::DMA_PUSHER);
            let process_start = trace_dma_pusher.then(Instant::now);
            self.process_commands(&self.command_headers.clone());
            trace_dma_pusher_step(
                7,
                self.dma_pushbuffer.len(),
                self.dma_pushbuffer_subindex,
                command_list_header.addr(),
                command_list_header.size(),
                command_list_header.is_non_main(),
                self.dma_state.method_count,
                self.dma_state.dma_get,
                elapsed_us_opt(process_start),
            );
        }
        true
    }

    /// Process the next step with an engine for dispatch.
    fn step_with_engine(&mut self, engine: &mut dyn EngineInterface) -> bool {
        if !self.ib_enable || self.dma_pushbuffer.is_empty() {
            return false;
        }

        let command_list = match self.dma_pushbuffer.front() {
            Some(cl) => cl,
            None => return false,
        };

        if command_list.command_lists.is_empty() && command_list.prefetch_command_list.is_empty() {
            self.dma_pushbuffer.pop_front();
            self.dma_pushbuffer_subindex = 0;
            return true;
        }

        if !command_list.prefetch_command_list.is_empty() {
            let commands: Vec<CommandHeader> = command_list.prefetch_command_list.clone();
            self.process_commands_with_engine(&commands, engine);
            self.dma_pushbuffer.pop_front();
        } else {
            let command_list_header = command_list.command_lists[self.dma_pushbuffer_subindex];
            self.dma_pushbuffer_subindex += 1;
            self.dma_state.dma_get = command_list_header.addr();

            if self.dma_pushbuffer_subindex >= command_list.command_lists.len() {
                self.dma_pushbuffer.pop_front();
                self.dma_pushbuffer_subindex = 0;
            }

            if command_list_header.size() == 0 {
                return true;
            }

            self.update_current_dirty_for_fetch(
                command_list_header.addr(),
                command_list_header.size(),
            );

            let command_count = command_list_header.size() as usize;
            let raw = self.fetch_command_bytes(command_list_header.addr(), command_count);

            self.command_headers.clear();
            self.command_headers
                .extend(raw.chunks_exact(4).map(|chunk| CommandHeader {
                    raw: u32::from_le_bytes(chunk.try_into().unwrap()),
                }));
            let commands = self.command_headers.clone();
            self.process_commands_with_engine(&commands, engine);
        }
        true
    }

    fn should_use_unsafe_read(&self) -> bool {
        let gpu_level_high = {
            let values = settings::values();
            settings::is_gpu_level_high(&values)
        };
        if gpu_level_high {
            if self.dma_state.method >= MACRO_REGISTERS_START {
                return true;
            }
            return self.puller.bound_engines[self.dma_state.subchannel as usize]
                == Some(EngineID::KeplerComputeB)
                && self.dma_state.method == COMPUTE_INLINE;
        }
        true
    }

    fn fetch_command_bytes(&self, gpu_addr: GPUVAddr, command_count: usize) -> Vec<u8> {
        let byte_len = command_count * std::mem::size_of::<CommandHeader>();
        let mut raw = vec![0u8; byte_len];
        let gpu = unsafe { &*self.gpu };
        let mm = self.memory_manager.lock();
        let translated_cpu_addr = mm.gpu_to_cpu_address(gpu_addr);
        let read_cpu = |cpu_addr, dst: &mut [u8]| {
            if !gpu.read_guest_memory(cpu_addr, dst) {
                log::warn!(
                    "DmaPusher: read_guest_memory FAILED cpu_addr={:#x} len={}",
                    cpu_addr,
                    dst.len()
                );
            }
        };
        if self.should_use_unsafe_read() {
            mm.read_block_unsafe(gpu_addr, &mut raw, &read_cpu);
        } else {
            mm.read_block(gpu_addr, &mut raw, &read_cpu);
        }
        if let Some(limit) = command_words_trace_limit() {
            let idx = COMMAND_WORDS_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
            if let Some(needle) = command_words_find_value() {
                for (word_index, chunk) in raw.chunks_exact(4).enumerate() {
                    let word = u32::from_le_bytes(chunk.try_into().unwrap());
                    if word == needle {
                        log::info!(
                            "[COMMAND_WORD_FIND] #{} gpu=0x{:X} cpu={:?} count={} word_index={} value=0x{:08X}",
                            idx,
                            gpu_addr,
                            translated_cpu_addr.map(|addr| format!("0x{addr:X}")),
                            command_count,
                            word_index,
                            word
                        );
                    }
                }
            }
            if idx < limit {
                let preview_len = command_words_preview_len().min(command_count);
                let words: Vec<String> = raw
                    .chunks_exact(4)
                    .take(preview_len)
                    .map(|chunk| {
                        let word = u32::from_le_bytes(chunk.try_into().unwrap());
                        format!("{word:08X}")
                    })
                    .collect();
                log::info!(
                    "[COMMAND_WORDS] #{} gpu=0x{:X} cpu={:?} count={} unsafe={} words[0..{}]={}",
                    idx,
                    gpu_addr,
                    translated_cpu_addr.map(|addr| format!("0x{addr:X}")),
                    command_count,
                    self.should_use_unsafe_read(),
                    preview_len,
                    words.join(" "),
                );
            }
        }
        raw
    }

    fn process_commands(&mut self, commands: &[CommandHeader]) {
        let mut index = 0;
        let mut total_dispatches = 0u64;
        let trace_methods = common::trace::is_enabled(common::trace::cat::DMA_PUSHER);
        let mut method_times: Vec<(u32, u32, u64, u64, u64)> = Vec::new();
        while index < commands.len() {
            total_dispatches += 1;
            if should_trace_dma_flow() {
                let trace_idx = DMA_FLOW_DISPATCH_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
                if trace_idx < 96 {
                    let command_header = commands[index];
                    log::info!(
                        "DmaPusher::process_commands index={} total={} raw=0x{:08X} method_count={} method=0x{:X} subch={} mode={:?}",
                        index,
                        total_dispatches,
                        command_header.raw,
                        self.dma_state.method_count,
                        self.dma_state.method,
                        self.dma_state.subchannel,
                        command_header.mode()
                    );
                }
            }
            if total_dispatches % 100_000 == 0 {
                log::warn!(
                    "DmaPusher::process_commands heartbeat: {} dispatches, index={}/{}",
                    total_dispatches,
                    index,
                    commands.len()
                );
            }
            let command_header = commands[index];

            if self.dma_state.method_count > 0 {
                self.dma_state.dma_word_offset = (index as u64) * 4;
                if self.dma_state.non_incrementing {
                    let max_write =
                        std::cmp::min(index + self.dma_state.method_count as usize, commands.len())
                            - index;
                    let method = self.dma_state.method;
                    let subchannel = self.dma_state.subchannel;
                    let dma_segment = self
                        .dma_state
                        .dma_get
                        .wrapping_add(self.dma_state.dma_word_offset);
                    trace_dma_pusher_method_inflight(
                        12,
                        total_dispatches,
                        index,
                        commands.len(),
                        subchannel,
                        method,
                        max_write as u32,
                        self.dma_state.method_count,
                        dma_segment,
                        0,
                    );
                    let elapsed = self.dispatch_multi_method(&commands[index..index + max_write]);
                    trace_dma_pusher_method_inflight(
                        13,
                        total_dispatches,
                        index,
                        commands.len(),
                        subchannel,
                        method,
                        max_write as u32,
                        self.dma_state.method_count,
                        dma_segment,
                        elapsed,
                    );
                    if trace_methods {
                        record_dma_method_time(
                            &mut method_times,
                            subchannel,
                            method,
                            1,
                            max_write as u64,
                            elapsed,
                        );
                    }
                    self.dma_state.method_count -= max_write as u32;
                    self.dma_state.is_last_call = true;
                    index += max_write;
                    continue;
                } else {
                    self.dma_state.is_last_call = self.dma_state.method_count <= 1;
                    let method = self.dma_state.method;
                    let subchannel = self.dma_state.subchannel;
                    let dma_segment = self
                        .dma_state
                        .dma_get
                        .wrapping_add(self.dma_state.dma_word_offset);
                    trace_dma_pusher_method_inflight(
                        10,
                        total_dispatches,
                        index,
                        commands.len(),
                        subchannel,
                        method,
                        command_header.argument(),
                        self.dma_state.method_count,
                        dma_segment,
                        0,
                    );
                    let elapsed = self.dispatch_method(command_header.argument());
                    trace_dma_pusher_method_inflight(
                        11,
                        total_dispatches,
                        index,
                        commands.len(),
                        subchannel,
                        method,
                        command_header.argument(),
                        self.dma_state.method_count,
                        dma_segment,
                        elapsed,
                    );
                    if trace_methods {
                        record_dma_method_time(
                            &mut method_times,
                            subchannel,
                            method,
                            1,
                            1,
                            elapsed,
                        );
                    }
                }

                if !self.dma_state.non_incrementing {
                    self.dma_state.method += 1;
                }

                if self.dma_increment_once {
                    self.dma_state.non_incrementing = true;
                }

                self.dma_state.method_count -= 1;
            } else {
                match command_header.mode() {
                    Some(SubmissionMode::Increasing) => {
                        self.set_state(&command_header);
                        self.dma_state.non_incrementing = false;
                        self.dma_increment_once = false;
                    }
                    Some(SubmissionMode::NonIncreasing) => {
                        self.set_state(&command_header);
                        self.dma_state.non_incrementing = true;
                        self.dma_increment_once = false;
                    }
                    Some(SubmissionMode::Inline) => {
                        self.dma_state.method = command_header.method();
                        self.dma_state.subchannel = command_header.subchannel();
                        self.dma_state.dma_word_offset = (-(self.dma_state.dma_get as i64)) as u64;
                        let method = self.dma_state.method;
                        let subchannel = self.dma_state.subchannel;
                        let dma_segment = self
                            .dma_state
                            .dma_get
                            .wrapping_add(self.dma_state.dma_word_offset);
                        trace_dma_pusher_method_inflight(
                            10,
                            total_dispatches,
                            index,
                            commands.len(),
                            subchannel,
                            method,
                            command_header.arg_count(),
                            self.dma_state.method_count,
                            dma_segment,
                            0,
                        );
                        let elapsed = self.dispatch_method(command_header.arg_count());
                        trace_dma_pusher_method_inflight(
                            11,
                            total_dispatches,
                            index,
                            commands.len(),
                            subchannel,
                            method,
                            command_header.arg_count(),
                            self.dma_state.method_count,
                            dma_segment,
                            elapsed,
                        );
                        if trace_methods {
                            record_dma_method_time(
                                &mut method_times,
                                subchannel,
                                method,
                                1,
                                1,
                                elapsed,
                            );
                        }
                        self.dma_state.non_incrementing = true;
                        self.dma_increment_once = false;
                    }
                    Some(SubmissionMode::IncreaseOnce) => {
                        self.set_state(&command_header);
                        self.dma_state.non_incrementing = false;
                        self.dma_increment_once = true;
                    }
                    _ => {}
                }
            }
            index += 1;
        }
        if should_trace_dma_count() {
            log::info!(
                "DmaPusher::process_commands DONE headers={} dispatches={}",
                commands.len(),
                total_dispatches
            );
        }
        if trace_methods {
            method_times.sort_by(|lhs, rhs| rhs.4.cmp(&lhs.4));
            for (subchannel, method, calls, words, elapsed) in method_times.into_iter().take(12) {
                trace_dma_pusher_method_agg(
                    commands.len(),
                    total_dispatches,
                    subchannel,
                    method,
                    calls,
                    words,
                    elapsed,
                    self.dma_state.dma_get,
                );
            }
        }
    }

    /// Process a span of command headers, dispatching to an engine.
    /// Matches upstream `DmaPusher::ProcessCommands`.
    fn process_commands_with_engine(
        &mut self,
        commands: &[CommandHeader],
        engine: &mut dyn EngineInterface,
    ) {
        let mut index = 0;
        while index < commands.len() {
            let command_header = commands[index];

            if self.dma_state.method_count > 0 {
                // Data word of methods command
                self.dma_state.dma_word_offset = (index as u64) * 4;
                if self.dma_state.non_incrementing {
                    let max_write =
                        std::cmp::min(index + self.dma_state.method_count as usize, commands.len())
                            - index;
                    self.dispatch_multi_method(&commands[index..index + max_write]);
                    self.dma_state.method_count -= max_write as u32;
                    self.dma_state.is_last_call = true;
                    index += max_write;
                    continue;
                } else {
                    self.dma_state.is_last_call = self.dma_state.method_count <= 1;
                    self.dispatch_method(command_header.argument());
                }

                if !self.dma_state.non_incrementing {
                    self.dma_state.method += 1;
                }

                if self.dma_increment_once {
                    self.dma_state.non_incrementing = true;
                }

                self.dma_state.method_count -= 1;
            } else {
                // No command active - first word of a new one
                match command_header.mode() {
                    Some(SubmissionMode::Increasing) => {
                        self.set_state(&command_header);
                        self.dma_state.non_incrementing = false;
                        self.dma_increment_once = false;
                    }
                    Some(SubmissionMode::NonIncreasing) => {
                        self.set_state(&command_header);
                        self.dma_state.non_incrementing = true;
                        self.dma_increment_once = false;
                    }
                    Some(SubmissionMode::Inline) => {
                        self.dma_state.method = command_header.method();
                        self.dma_state.subchannel = command_header.subchannel();
                        self.dma_state.dma_word_offset = (-(self.dma_state.dma_get as i64)) as u64;
                        self.dispatch_method(command_header.arg_count());
                        self.dma_state.non_incrementing = true;
                        self.dma_increment_once = false;
                    }
                    Some(SubmissionMode::IncreaseOnce) => {
                        self.set_state(&command_header);
                        self.dma_state.non_incrementing = false;
                        self.dma_increment_once = true;
                    }
                    _ => {}
                }
            }
            index += 1;
        }
    }

    fn set_state(&mut self, command_header: &CommandHeader) {
        self.dma_state.method = command_header.method();
        self.dma_state.subchannel = command_header.subchannel();
        self.dma_state.method_count = command_header.method_count();
    }

    /// Dispatch a single method call to an engine. Matches upstream
    /// `DmaPusher::CallMethod`.
    fn dispatch_method(&mut self, argument: u32) -> u64 {
        let trace_start =
            common::trace::is_enabled(common::trace::cat::DMA_PUSHER).then(Instant::now);
        if should_trace_puller() {
            eprintln!(
                "[PULL] m=0x{:X} arg=0x{:08X} subch={} count={}",
                self.dma_state.method,
                argument,
                self.dma_state.subchannel,
                self.dma_state.method_count,
            );
        }
        if self.dma_state.method < NON_PULLER_METHODS {
            let trace_idx = DISPATCH_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
            if trace_idx < 48 {
                log::trace!(
                    "DmaPusher::dispatch_method puller method=0x{:X} arg=0x{:X} subch={} pending={}",
                    self.dma_state.method,
                    argument,
                    self.dma_state.subchannel,
                    self.dma_state.method_count
                );
            }
            if let Some(s) = current_submit_traced() {
                log::info!(
                    "[PULLER_TRACE] s#{} puller m=0x{:X} arg=0x{:08X} subch={} pending={}",
                    s,
                    self.dma_state.method,
                    argument,
                    self.dma_state.subchannel,
                    self.dma_state.method_count,
                );
            }
            self.puller.call_method(&MethodCall::new(
                self.dma_state.method,
                argument,
                self.dma_state.subchannel,
                self.dma_state.method_count,
            ));
            return elapsed_us_opt(trace_start);
        }

        let Some(subchannel) = self.subchannels[self.dma_state.subchannel as usize] else {
            let trace_idx = DISPATCH_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
            if trace_idx < 48 {
                log::warn!(
                    "DmaPusher::dispatch_method no subchannel binding method=0x{:X} arg=0x{:X} subch={}",
                    self.dma_state.method,
                    argument,
                    self.dma_state.subchannel
                );
            }
            if let Some(s) = current_submit_traced() {
                log::warn!(
                    "[PULLER_TRACE] s#{} unbound m=0x{:X} arg=0x{:08X} subch={}",
                    s,
                    self.dma_state.method,
                    argument,
                    self.dma_state.subchannel,
                );
            }
            return elapsed_us_opt(trace_start);
        };
        let subchannel = unsafe { subchannel.as_mut() };
        if !subchannel.execution_mask()[self.dma_state.method as usize] {
            let trace_idx = DISPATCH_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
            if trace_idx < 48 {
                log::trace!(
                    "DmaPusher::dispatch_method sink method=0x{:X} arg=0x{:X} subch={}",
                    self.dma_state.method,
                    argument,
                    self.dma_state.subchannel
                );
            }
            if let Some(s) = current_submit_traced() {
                log::info!(
                    "[PULLER_TRACE] s#{} sink m=0x{:X} arg=0x{:08X} subch={}",
                    s,
                    self.dma_state.method,
                    argument,
                    self.dma_state.subchannel,
                );
            }
            subchannel.push_method_sink(self.dma_state.method, argument);
            return elapsed_us_opt(trace_start);
        }
        let trace_idx = DISPATCH_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
        if trace_idx < 48 {
            log::trace!(
                "DmaPusher::dispatch_method execute method=0x{:X} arg=0x{:X} subch={} dma_seg=0x{:X}",
                self.dma_state.method,
                argument,
                self.dma_state.subchannel,
                self.dma_state.dma_get.wrapping_add(self.dma_state.dma_word_offset)
            );
        }
        if let Some(s) = current_submit_traced() {
            log::info!(
                "[PULLER_TRACE] s#{} engine m=0x{:X} arg=0x{:08X} subch={} pending={}",
                s,
                self.dma_state.method,
                argument,
                self.dma_state.subchannel,
                self.dma_state.method_count,
            );
        }
        subchannel.consume_sink();
        subchannel.set_current_dma_segment(
            self.dma_state
                .dma_get
                .wrapping_add(self.dma_state.dma_word_offset),
        );
        subchannel.call_method(self.dma_state.method, argument, self.dma_state.is_last_call);
        elapsed_us_opt(trace_start)
    }

    /// Dispatch a multi-method call to an engine. Matches upstream
    /// `DmaPusher::CallMultiMethod`.
    fn dispatch_multi_method(&mut self, commands: &[CommandHeader]) -> u64 {
        let trace_start =
            common::trace::is_enabled(common::trace::cat::DMA_PUSHER).then(Instant::now);
        let args: Vec<u32> = commands.iter().map(|c| c.argument()).collect();
        if self.dma_state.method >= 0x45 && self.dma_state.method <= 0x48 {
            let preview: Vec<String> = args.iter().take(6).map(|v| format!("{:08X}", v)).collect();
            log::trace!(
                "DmaPusher::dispatch_multi_method method=0x{:X} subch={} count={} pending={} args[0..]={:?}",
                self.dma_state.method,
                self.dma_state.subchannel,
                args.len(),
                self.dma_state.method_count,
                preview
            );
        }
        if let Some(s) = current_submit_traced() {
            let preview: Vec<String> = args.iter().take(8).map(|v| format!("{:08X}", v)).collect();
            log::info!(
                "[PULLER_TRACE] s#{} multi {} m=0x{:X} subch={} count={} pending={} args[0..]={:?}",
                s,
                if self.dma_state.method < NON_PULLER_METHODS {
                    "puller"
                } else {
                    "engine"
                },
                self.dma_state.method,
                self.dma_state.subchannel,
                args.len(),
                self.dma_state.method_count,
                preview,
            );
        }
        if self.dma_state.method < NON_PULLER_METHODS {
            self.puller.call_multi_method(
                self.dma_state.method,
                self.dma_state.subchannel,
                &args,
                args.len() as u32,
                self.dma_state.method_count,
            );
            return elapsed_us_opt(trace_start);
        }
        let Some(subchannel) = self.subchannels[self.dma_state.subchannel as usize] else {
            return elapsed_us_opt(trace_start);
        };
        let subchannel = unsafe { subchannel.as_mut() };
        subchannel.consume_sink();
        subchannel.set_current_dma_segment(
            self.dma_state
                .dma_get
                .wrapping_add(self.dma_state.dma_word_offset),
        );
        subchannel.call_multi_method(
            self.dma_state.method,
            &args,
            args.len() as u32,
            self.dma_state.method_count,
        );
        elapsed_us_opt(trace_start)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::control::channel_state::ChannelState;
    use common::settings;
    use common::settings_enums::GpuAccuracy;

    #[test]
    fn install_self_reference_uses_stable_boxed_address() {
        let mut channel_state = Box::new(ChannelState::new(7));
        let memory_manager = Arc::new(Mutex::new(crate::memory_manager::MemoryManager::new(1)));
        let channel_ptr: *mut ChannelState = &mut *channel_state;
        let mut dma = Box::new(DmaPusher::new(
            std::ptr::null(),
            SystemRef::null(),
            memory_manager,
            channel_ptr,
        ));
        let dma_ptr: *mut DmaPusher = &mut *dma;

        dma.install_self_reference();

        assert_eq!(dma.puller.dma_pusher_ptr_for_test(), dma_ptr);
    }

    #[test]
    fn should_use_unsafe_read_matches_upstream_gpu_accuracy_branching() {
        let mut channel_state = Box::new(ChannelState::new(9));
        let memory_manager = Arc::new(Mutex::new(crate::memory_manager::MemoryManager::new(1)));
        let channel_ptr: *mut ChannelState = &mut *channel_state;
        let dma = DmaPusher::new(
            std::ptr::null(),
            SystemRef::null(),
            memory_manager,
            channel_ptr,
        );

        let previous_accuracy = {
            let values = settings::values();
            values.current_gpu_accuracy
        };

        {
            let mut values = settings::values_mut();
            values.current_gpu_accuracy = GpuAccuracy::High;
        }
        assert!(!dma.should_use_unsafe_read());

        {
            let mut values = settings::values_mut();
            values.current_gpu_accuracy = GpuAccuracy::Normal;
        }
        assert!(dma.should_use_unsafe_read());

        settings::values_mut().current_gpu_accuracy = previous_accuracy;
    }

    #[test]
    fn dispatch_calls_stops_when_system_is_not_powered_on() {
        let system = ruzu_core::core::System::new();
        let gpu = crate::gpu::Gpu::new(false, false);
        gpu.set_system_ref(SystemRef::from_ref(&system));
        let mut channel_state = Box::new(ChannelState::new(11));
        let memory_manager = Arc::new(Mutex::new(crate::memory_manager::MemoryManager::new(1)));
        let channel_ptr: *mut ChannelState = &mut *channel_state;
        let mut dma = DmaPusher::new(
            &gpu as *const crate::gpu::Gpu,
            SystemRef::from_ref(&system),
            memory_manager,
            channel_ptr,
        );

        dma.push(CommandList::from_prefetch(vec![CommandHeader {
            raw: build_command_header(BufferMethods::Nop, 0, SubmissionMode::Increasing).raw,
        }]));
        dma.dispatch_calls();

        assert_eq!(dma.dma_pushbuffer.len(), 1);
    }
}
