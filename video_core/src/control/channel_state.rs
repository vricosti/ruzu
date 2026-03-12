// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/control/channel_state.h and channel_state.cpp
//!
//! Per-GPU-channel state including engine instances and memory manager.

use std::sync::Arc;

use parking_lot::Mutex;

use crate::rasterizer_interface::RasterizerInterface;

// ---------------------------------------------------------------------------
// Forward-declared / opaque placeholders for types not yet ported into
// ruzu-gpu.  These mirror the C++ forward declarations in channel_state.h.
// As the corresponding crates/modules are ported, replace these with real
// imports.
// ---------------------------------------------------------------------------

/// Placeholder for `Tegra::Engines::Maxwell3D`.
pub struct Maxwell3D {
    _private: (),
}

/// Placeholder for `Tegra::Engines::Fermi2D`.
pub struct Fermi2D {
    _private: (),
}

/// Placeholder for `Tegra::Engines::KeplerCompute`.
pub struct KeplerCompute {
    _private: (),
}

/// Placeholder for `Tegra::Engines::MaxwellDMA`.
pub struct MaxwellDMA {
    _private: (),
}

/// Placeholder for `Tegra::Engines::KeplerMemory`.
pub struct KeplerMemory {
    _private: (),
}

/// Placeholder for `Tegra::MemoryManager`.
pub struct MemoryManager {
    id: usize,
}

impl MemoryManager {
    /// Placeholder constructor.
    pub fn new(id: usize) -> Self {
        Self { id }
    }

    /// Return the unique address-space ID.  Upstream: `MemoryManager::GetID()`.
    pub fn get_id(&self) -> usize {
        self.id
    }
}

/// Placeholder for `Tegra::DmaPusher`.
pub struct DmaPusher {
    _private: (),
}

impl DmaPusher {
    /// Placeholder for `DmaPusher::BindRasterizer`.
    ///
    /// In the full implementation, stores the rasterizer reference for later
    /// use during command dispatch.
    pub fn bind_rasterizer(&mut self, _rasterizer: &dyn RasterizerInterface) {
        // Requires storing a reference/Arc to the rasterizer.
        // Structural placeholder until DmaPusher is fully ported.
        log::debug!("DmaPusher::bind_rasterizer");
    }

    /// Placeholder for `DmaPusher::Push`.
    ///
    /// Enqueues a command list for later processing by `dispatch_calls`.
    pub fn push(&mut self, _entries: CommandList) {
        // In the full implementation, entries are pushed into an internal
        // FIFO queue and processed during dispatch_calls.
        log::trace!("DmaPusher::push");
    }

    /// Placeholder for `DmaPusher::DispatchCalls`.
    ///
    /// Processes all queued command lists, decoding GPU methods and
    /// dispatching them to the puller engine.
    pub fn dispatch_calls(&mut self) {
        // In the full implementation, this reads GPFIFO entries,
        // decodes methods (incrementing, non-incrementing, inline),
        // and calls Puller::CallMethod / CallMultiMethod.
        log::trace!("DmaPusher::dispatch_calls");
    }
}

/// Placeholder for `Tegra::CommandList`.
pub type CommandList = Vec<Vec<u32>>;

/// Placeholder for `Tegra::GPU`.
#[derive(Default)]
pub struct Gpu {
    _private: (),
}

impl Gpu {
    /// Placeholder for `GPU::BindChannel`.
    ///
    /// In the full implementation, this activates the specified GPU channel
    /// by its bind ID, making it the current channel for command processing.
    pub fn bind_channel(&mut self, _bind_id: i32) {
        log::debug!("GPU::bind_channel({})", _bind_id);
    }
}

/// Placeholder for `Core::System`.
pub struct System {
    _private: (),
}

// ---------------------------------------------------------------------------
// ChannelState
// ---------------------------------------------------------------------------

/// Per-channel GPU state.
///
/// Corresponds to `Tegra::Control::ChannelState` in upstream.
/// Not `Clone` or `Copy` (matches C++ deleted copy constructor).
pub struct ChannelState {
    pub bind_id: i32,
    pub program_id: u64,

    /// 3D engine
    pub maxwell_3d: Option<Box<Maxwell3D>>,
    /// 2D engine
    pub fermi_2d: Option<Box<Fermi2D>>,
    /// Compute engine
    pub kepler_compute: Option<Box<KeplerCompute>>,
    /// DMA engine
    pub maxwell_dma: Option<Box<MaxwellDMA>>,
    /// Inline memory engine
    pub kepler_memory: Option<Box<KeplerMemory>>,

    pub memory_manager: Option<Arc<Mutex<MemoryManager>>>,

    pub dma_pusher: Option<Box<DmaPusher>>,

    pub initialized: bool,
}

impl ChannelState {
    /// Create a new channel state with the given bind ID.
    ///
    /// Corresponds to `ChannelState::ChannelState(s32 bind_id)`.
    pub fn new(bind_id: i32) -> Self {
        Self {
            bind_id,
            program_id: 0,
            maxwell_3d: None,
            fermi_2d: None,
            kepler_compute: None,
            maxwell_dma: None,
            kepler_memory: None,
            memory_manager: None,
            dma_pusher: None,
            initialized: false,
        }
    }

    /// Initialize the channel engines.
    ///
    /// Corresponds to `ChannelState::Init(Core::System&, GPU&, u64)`.
    /// Requires `memory_manager` to be set before calling.
    ///
    /// In the full implementation, this creates all engine instances
    /// (Maxwell3D, Fermi2D, KeplerCompute, MaxwellDMA, KeplerMemory)
    /// and the DMA pusher, passing them the memory manager and system.
    pub fn init(&mut self, _system: &System, _gpu: &Gpu, program_id: u64) {
        assert!(
            self.memory_manager.is_some(),
            "memory_manager must be set before Init"
        );
        self.program_id = program_id;

        // Upstream creates all engines here:
        //   maxwell_3d = std::make_unique<Maxwell3D>(system, *memory_manager)
        //   fermi_2d = std::make_unique<Fermi2D>()
        //   kepler_compute = std::make_unique<KeplerCompute>(system, *memory_manager)
        //   maxwell_dma = std::make_unique<MaxwellDMA>(system, *memory_manager)
        //   kepler_memory = std::make_unique<KeplerMemory>(system, *memory_manager)
        //   dma_pusher = std::make_unique<DmaPusher>(system, gpu, *memory_manager, ...)
        //
        // Engine instantiation is blocked until the engine types accept
        // MemoryManager as a parameter.

        self.initialized = true;
        log::debug!(
            "ChannelState::init bind_id={} program_id={:016x}",
            self.bind_id,
            self.program_id
        );
    }

    /// Bind a rasterizer to all engines and the memory manager.
    ///
    /// Corresponds to `ChannelState::BindRasterizer(RasterizerInterface*)`.
    ///
    /// In the full implementation, this forwards the rasterizer reference to:
    /// dma_pusher, memory_manager, maxwell_3d, fermi_2d, kepler_memory,
    /// kepler_compute, maxwell_dma.
    pub fn bind_rasterizer(&mut self, _rasterizer: &dyn RasterizerInterface) {
        log::debug!(
            "ChannelState::bind_rasterizer bind_id={}",
            self.bind_id
        );
        // Forward to DMA pusher if available
        if let Some(ref mut dma) = self.dma_pusher {
            dma.bind_rasterizer(_rasterizer);
        }
        // Forward to engines when they are instantiated
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_channel_state() {
        let cs = ChannelState::new(42);
        assert_eq!(cs.bind_id, 42);
        assert_eq!(cs.program_id, 0);
        assert!(!cs.initialized);
        assert!(cs.maxwell_3d.is_none());
        assert!(cs.memory_manager.is_none());
    }
}
