// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/dma_pusher.h and video_core/dma_pusher.cpp
//!
//! DMA command submission to FIFOs, assembling pushbuffers into a command stream.
//! See https://envytools.readthedocs.io/en/latest/hw/fifo/dma-pusher.html

use std::collections::VecDeque;
use std::sync::Arc;
use std::sync::atomic::{AtomicU32, Ordering};

use crate::engines::engine_interface::EngineInterface;
use crate::engines::engine_interface::EngineTypes;
use crate::engines::puller::{EngineID, MethodCall, Puller};
use parking_lot::Mutex;

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
pub fn build_command_header(method: BufferMethods, arg_count: u32, mode: SubmissionMode) -> CommandHeader {
    let raw = (method as u32 & 0x1FFF)
        | ((arg_count & 0x1FFF) << 16)
        | ((mode as u32 & 0x7) << 29);
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
    subchannels: [Option<[usize; 2]>; MAX_SUBCHANNELS],
    subchannel_type: [EngineTypes; MAX_SUBCHANNELS],
    gpu: *const crate::gpu::Gpu,
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
            memory_manager,
            puller: Puller::new(gpu, puller_memory_manager, std::ptr::null_mut(), channel_state),
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
        self.subchannels[subchannel_id as usize] = Some(unsafe {
            std::mem::transmute::<*mut dyn EngineInterface, [usize; 2]>(engine)
        });
        self.subchannel_type[subchannel_id as usize] = engine_type;
    }

    /// Push a command list into the DMA pushbuffer queue.
    pub fn push(&mut self, entries: CommandList) {
        self.dma_pushbuffer.push_back(entries);
    }

    /// Dispatch all pending command lists. Matches upstream `DmaPusher::DispatchCalls`.
    ///
    /// In the full port, this takes the system/GPU context. For now, the command
    /// processing and method dispatch are fully implemented; engine dispatch goes
    /// through the `subchannel` engine interface.
    pub fn dispatch_calls(&mut self) {
        log::info!(
            "DmaPusher::dispatch_calls queued_lists={}",
            self.dma_pushbuffer.len()
        );
        self.dma_pushbuffer_subindex = 0;
        self.dma_state.is_last_call = true;

        // In the full port, this loops while system.is_powered_on().
        while self.step() {}
        log::info!("DmaPusher::dispatch_calls complete");

        let gpu = unsafe { &*self.gpu };
        gpu.flush_commands();
        gpu.on_command_list_end();
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
                if let Some(raw) = self.subchannels[self.dma_state.subchannel as usize] {
                    let engine: *mut dyn EngineInterface = unsafe { std::mem::transmute(raw) };
                    unsafe { &mut *engine }.set_current_dirty(dirty);
                }
            }
            EngineID::KeplerComputeB
            | EngineID::KeplerInlineToMemoryB
            | EngineID::FermiTwodA
            | EngineID::MaxwellDmaCopyA => {
                if let Some(raw) = self.subchannels[self.dma_state.subchannel as usize] {
                    let engine: *mut dyn EngineInterface = unsafe { std::mem::transmute(raw) };
                    unsafe { &mut *engine }.set_current_dirty(dirty);
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

        let dirty = self
            .memory_manager
            .lock()
            .is_memory_dirty(command_gpu_addr, word_count as u64 * std::mem::size_of::<u32>() as u64);

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
            // Prefetched command list from nvdrv (synchronization etc.).
            let commands: Vec<CommandHeader> = command_list.prefetch_command_list.clone();
            self.process_commands(&commands);
            self.dma_pushbuffer.pop_front();
        } else {
            let command_list_header = command_list.command_lists[self.dma_pushbuffer_subindex];
            log::info!(
                "DmaPusher::step command_list addr=0x{:X} size={} non_main={}",
                command_list_header.addr(),
                command_list_header.size(),
                command_list_header.is_non_main()
            );
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
            let byte_len = command_count * std::mem::size_of::<CommandHeader>();
            let mut raw = vec![0u8; byte_len];
            let gpu = unsafe { &*self.gpu };
            let mm = self.memory_manager.lock();
            mm.read_block_unsafe(command_list_header.addr(), &mut raw, &|cpu_addr, dst| {
                if !gpu.read_guest_memory(cpu_addr, dst) {
                    log::warn!("DmaPusher: read_guest_memory FAILED cpu_addr={:#x} len={}", cpu_addr, dst.len());
                }
            });
            drop(mm);

            self.command_headers.clear();
            self.command_headers.extend(raw.chunks_exact(4).map(|chunk| CommandHeader {
                raw: u32::from_le_bytes(chunk.try_into().unwrap()),
            }));
            let non_zero = self.command_headers.iter().filter(|h| h.raw != 0).count();
            if non_zero > 0 {
                log::info!(
                    "DmaPusher::step {} commands ({} non-zero) first={:#010x}",
                    self.command_headers.len(), non_zero,
                    self.command_headers.first().map_or(0, |h| h.raw),
                );
            }
            self.process_commands(&self.command_headers.clone());
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
            let byte_len = command_count * std::mem::size_of::<CommandHeader>();
            let mut raw = vec![0u8; byte_len];
            let gpu = unsafe { &*self.gpu };
            let mm = self.memory_manager.lock();
            mm.read_block_unsafe(command_list_header.addr(), &mut raw, &|cpu_addr, dst| {
                if !gpu.read_guest_memory(cpu_addr, dst) {
                    log::warn!("DmaPusher: read_guest_memory FAILED cpu_addr={:#x} len={}", cpu_addr, dst.len());
                }
            });
            drop(mm);

            self.command_headers.clear();
            self.command_headers.extend(raw.chunks_exact(4).map(|chunk| CommandHeader {
                raw: u32::from_le_bytes(chunk.try_into().unwrap()),
            }));
            let commands = self.command_headers.clone();
            self.process_commands_with_engine(&commands, engine);
        }
        true
    }

    fn process_commands(&mut self, commands: &[CommandHeader]) {
        let mut index = 0;
        let mut total_dispatches = 0u64;
        while index < commands.len() {
            total_dispatches += 1;
            if total_dispatches % 100_000 == 0 {
                log::warn!("DmaPusher::process_commands heartbeat: {} dispatches, index={}/{}",
                    total_dispatches, index, commands.len());
            }
            let command_header = commands[index];

            if self.dma_state.method_count > 0 {
                self.dma_state.dma_word_offset = (index as u64) * 4;
                if self.dma_state.non_incrementing {
                    let max_write =
                        std::cmp::min(index + self.dma_state.method_count as usize, commands.len()) - index;
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
                    let max_write = std::cmp::min(
                        index + self.dma_state.method_count as usize,
                        commands.len(),
                    ) - index;
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
                        self.dma_state.dma_word_offset =
                            (-(self.dma_state.dma_get as i64)) as u64;
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
    fn dispatch_method(&mut self, argument: u32) {
        if self.dma_state.method < NON_PULLER_METHODS {
            let trace_idx = DISPATCH_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
            if trace_idx < 48 {
                log::info!(
                    "DmaPusher::dispatch_method puller method=0x{:X} arg=0x{:X} subch={} pending={}",
                    self.dma_state.method,
                    argument,
                    self.dma_state.subchannel,
                    self.dma_state.method_count
                );
            }
            self.puller.call_method(&MethodCall::new(
                self.dma_state.method,
                argument,
                self.dma_state.subchannel,
                self.dma_state.method_count,
            ));
            return;
        }

        let Some(raw) = self.subchannels[self.dma_state.subchannel as usize] else {
            let trace_idx = DISPATCH_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
            if trace_idx < 48 {
                log::warn!(
                    "DmaPusher::dispatch_method no subchannel binding method=0x{:X} arg=0x{:X} subch={}",
                    self.dma_state.method,
                    argument,
                    self.dma_state.subchannel
                );
            }
            return;
        };
        let subchannel: *mut dyn EngineInterface = unsafe { std::mem::transmute(raw) };
        let subchannel = unsafe { &mut *subchannel };
        if !subchannel.execution_mask()[self.dma_state.method as usize] {
            let trace_idx = DISPATCH_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
            if trace_idx < 48 {
                log::info!(
                    "DmaPusher::dispatch_method sink method=0x{:X} arg=0x{:X} subch={}",
                    self.dma_state.method,
                    argument,
                    self.dma_state.subchannel
                );
            }
            subchannel.push_method_sink(self.dma_state.method, argument);
            return;
        }
        let trace_idx = DISPATCH_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
        if trace_idx < 48 {
            log::info!(
                "DmaPusher::dispatch_method execute method=0x{:X} arg=0x{:X} subch={} dma_seg=0x{:X}",
                self.dma_state.method,
                argument,
                self.dma_state.subchannel,
                self.dma_state.dma_get.wrapping_add(self.dma_state.dma_word_offset)
            );
        }
        subchannel.consume_sink();
        subchannel.set_current_dma_segment(self.dma_state.dma_get.wrapping_add(self.dma_state.dma_word_offset));
        subchannel.call_method(self.dma_state.method, argument, self.dma_state.is_last_call);
    }

    /// Dispatch a multi-method call to an engine. Matches upstream
    /// `DmaPusher::CallMultiMethod`.
    fn dispatch_multi_method(&mut self, commands: &[CommandHeader]) {
        let args: Vec<u32> = commands.iter().map(|c| c.argument()).collect();
        if self.dma_state.method >= 0x45 && self.dma_state.method <= 0x48 {
            let preview: Vec<String> = args.iter().take(6).map(|v| format!("{:08X}", v)).collect();
            log::info!(
                "DmaPusher::dispatch_multi_method method=0x{:X} subch={} count={} pending={} args[0..]={:?}",
                self.dma_state.method,
                self.dma_state.subchannel,
                args.len(),
                self.dma_state.method_count,
                preview
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
            return;
        }
        let Some(raw) = self.subchannels[self.dma_state.subchannel as usize] else {
            return;
        };
        let subchannel: *mut dyn EngineInterface = unsafe { std::mem::transmute(raw) };
        let subchannel = unsafe { &mut *subchannel };
        subchannel.consume_sink();
        subchannel.set_current_dma_segment(self.dma_state.dma_get.wrapping_add(self.dma_state.dma_word_offset));
        subchannel.call_multi_method(
            self.dma_state.method,
            &args,
            args.len() as u32,
            self.dma_state.method_count,
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::control::channel_state::ChannelState;

    #[test]
    fn install_self_reference_uses_stable_boxed_address() {
        let mut channel_state = Box::new(ChannelState::new(7));
        let memory_manager = Arc::new(Mutex::new(crate::memory_manager::MemoryManager::new(1)));
        let channel_ptr: *mut ChannelState = &mut *channel_state;
        let mut dma = Box::new(DmaPusher::new(std::ptr::null(), memory_manager, channel_ptr));
        let dma_ptr: *mut DmaPusher = &mut *dma;

        dma.install_self_reference();

        assert_eq!(dma.puller.dma_pusher_ptr_for_test(), dma_ptr);
    }
}
