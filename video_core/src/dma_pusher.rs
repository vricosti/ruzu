// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/dma_pusher.h and video_core/dma_pusher.cpp
//!
//! DMA command submission to FIFOs, assembling pushbuffers into a command stream.
//! See https://envytools.readthedocs.io/en/latest/hw/fifo/dma-pusher.html

use std::collections::VecDeque;

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
const MAX_SUBCHANNELS: usize = 8;
const MACRO_REGISTERS_START: u32 = 0xE00;
const COMPUTE_INLINE: u32 = 0x6D;

/// Internal DMA state tracking.
#[derive(Debug, Default)]
struct DmaState {
    method: u32,
    subchannel: u32,
    method_count: u32,
    length_pending: u32,
    dma_get: GPUVAddr,
    dma_word_offset: u64,
    non_incrementing: bool,
    is_last_call: bool,
}

/// The DmaPusher implements DMA submission to FIFOs.
///
/// The pushbuffers are assembled into a "command stream" of 32-bit words.
pub struct DmaPusher {
    dma_pushbuffer: VecDeque<CommandList>,
    dma_pushbuffer_subindex: usize,
    dma_state: DmaState,
    dma_increment_once: bool,
    ib_enable: bool,
    command_headers: Vec<CommandHeader>,
    // References to GPU subsystems would be held here in the full port:
    // gpu: &GPU,
    // system: &System,
    // memory_manager: &MemoryManager,
    // puller: Puller,
    // subchannels: [Option<&EngineInterface>; MAX_SUBCHANNELS],
    // subchannel_type: [EngineTypes; MAX_SUBCHANNELS],
}

impl DmaPusher {
    /// Creates a new DmaPusher.
    ///
    /// In the full port, this takes references to System, GPU, MemoryManager,
    /// and ChannelState.
    pub fn new() -> Self {
        Self {
            dma_pushbuffer: VecDeque::new(),
            dma_pushbuffer_subindex: 0,
            dma_state: DmaState::default(),
            dma_increment_once: false,
            ib_enable: true,
            command_headers: Vec::new(),
        }
    }

    /// Push a command list into the DMA pushbuffer queue.
    pub fn push(&mut self, entries: CommandList) {
        self.dma_pushbuffer.push_back(entries);
    }

    /// Dispatch all pending command lists.
    pub fn dispatch_calls(&mut self) {
        self.dma_pushbuffer_subindex = 0;
        self.dma_state.is_last_call = true;

        // In the full port, this loops while system.is_powered_on()
        while self.step() {}

        // NOTE: Full implementation calls gpu.flush_commands() and gpu.on_command_list_end().
        // Stubbed until GPU and System integration is complete.
        log::warn!("DmaPusher::dispatch_calls: GPU/System not integrated, skipping flush");
    }

    /// Process the next step of command submission.
    fn step(&mut self) -> bool {
        if !self.ib_enable || self.dma_pushbuffer.is_empty() {
            return false;
        }
        // NOTE: Full implementation reads command buffer entries from GPU memory via
        // the memory manager. Without memory manager integration, we cannot process
        // the pushbuffer entries.
        log::warn!("DmaPusher::step: MemoryManager not integrated, dropping pushbuffer");
        self.dma_pushbuffer.clear();
        false
    }

    /// Process a span of command headers.
    fn process_commands(&mut self, commands: &[CommandHeader]) {
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
                    self.call_multi_method(&commands[index..index + max_write]);
                    self.dma_state.method_count -= max_write as u32;
                    self.dma_state.is_last_call = true;
                    index += max_write;
                    continue;
                } else {
                    self.dma_state.is_last_call = self.dma_state.method_count <= 1;
                    self.call_method(command_header.argument());
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
                        // Negate to set address as 0
                        self.dma_state.dma_word_offset =
                            (-(self.dma_state.dma_get as i64)) as u64;
                        self.call_method(command_header.arg_count());
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

    fn call_method(&self, _argument: u32) {
        // NOTE: Full implementation dispatches to the puller or the subchannel engine
        // based on dma_state.subchannel and dma_state.method.
        // Stubbed until engine integration is complete.
        log::warn!(
            "DmaPusher::call_method: engine not integrated, ignoring method 0x{:X}",
            self.dma_state.method
        );
    }

    fn call_multi_method(&self, _commands: &[CommandHeader]) {
        // NOTE: Full implementation dispatches to the puller or subchannel engine for
        // non-incrementing multi-method writes.
        // Stubbed until engine integration is complete.
        log::warn!(
            "DmaPusher::call_multi_method: engine not integrated, ignoring method 0x{:X}",
            self.dma_state.method
        );
    }
}
