// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/engines/engine_interface.h
//!
//! Common interface that all GPU engines implement for register writes.

/// GPU virtual address type.
pub type GPUVAddr = u64;

/// Engine type identifiers, matching the C++ `EngineTypes` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum EngineTypes {
    KeplerCompute = 0,
    Maxwell3D = 1,
    Fermi2D = 2,
    MaxwellDMA = 3,
    KeplerMemory = 4,
}

/// Trait corresponding to the C++ `EngineInterface` base class.
///
/// Each engine accepts single and multi-value register writes, and maintains
/// an execution mask, a method sink for deferred writes, and dirty tracking.
pub trait EngineInterface: Send {
    /// Write a single value to the register identified by `method`.
    fn call_method(&mut self, method: u32, method_argument: u32, is_last_call: bool);

    /// Write multiple values to the register identified by `method`.
    fn call_multi_method(
        &mut self,
        method: u32,
        base_start: &[u32],
        amount: u32,
        methods_pending: u32,
    );

    /// Consume the method sink, flushing deferred writes. Default implementation
    /// calls `call_method` for each entry then clears the sink.
    fn consume_sink(&mut self) {
        // Default: process deferred writes.
        // Concrete types override `consume_sink_impl` for custom behavior.
        self.consume_sink_impl();
    }

    /// Internal sink consumption — override in concrete engines.
    fn consume_sink_impl(&mut self);

    /// Access the execution mask. The DmaPusher uses this to decide whether
    /// a method should be executed immediately or deferred to the sink.
    fn execution_mask(&self) -> &[bool];

    /// Push a (method, value) pair onto the method sink for deferred processing.
    fn push_method_sink(&mut self, method: u32, value: u32);

    /// Set the current DMA segment address.
    fn set_current_dma_segment(&mut self, segment: GPUVAddr);

    /// Get the current dirty flag.
    fn current_dirty(&self) -> bool;

    /// Set the current dirty flag.
    fn set_current_dirty(&mut self, dirty: bool);
}

/// State common to all engines that implement `EngineInterface`.
///
/// Corresponds to the C++ `EngineInterface` member fields:
/// - `execution_mask` (bitset<u16::MAX>)
/// - `method_sink` (vector of (method, value) pairs)
/// - `current_dirty` flag
/// - `current_dma_segment` address
pub struct EngineInterfaceState {
    /// Bitmask indicating which method indices trigger immediate execution.
    /// Index by method number; `true` means the method must be executed
    /// rather than deferred.
    pub execution_mask: Vec<bool>,
    /// Deferred (method, value) pairs accumulated between flushes.
    pub method_sink: Vec<(u32, u32)>,
    /// Whether the engine has dirty state that needs processing.
    pub current_dirty: bool,
    /// Current DMA segment GPU virtual address.
    pub current_dma_segment: GPUVAddr,
}

impl EngineInterfaceState {
    /// Create a new state with an execution mask sized for `u16::MAX` entries.
    pub fn new() -> Self {
        Self {
            execution_mask: vec![false; u16::MAX as usize],
            method_sink: Vec::new(),
            current_dirty: false,
            current_dma_segment: 0,
        }
    }

    /// Consume the sink by calling the provided callback for each deferred write,
    /// then clear the sink. This is the default `ConsumeSinkImpl` behavior.
    pub fn default_consume_sink(
        &mut self,
        mut call_method: impl FnMut(u32, u32, bool),
    ) {
        for &(method, value) in &self.method_sink {
            call_method(method, value, true);
        }
        self.method_sink.clear();
    }
}

impl Default for EngineInterfaceState {
    fn default() -> Self {
        Self::new()
    }
}
