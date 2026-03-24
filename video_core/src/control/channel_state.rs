// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/control/channel_state.h and channel_state.cpp
//!
//! Per-GPU-channel state including engine instances and memory manager.

use std::sync::Arc;

use parking_lot::Mutex;

use crate::dma_pusher::{CommandList, DmaPusher};
use crate::rasterizer_interface::RasterizerInterface;

// ---------------------------------------------------------------------------
// Forward-declared / opaque placeholders for engine types not yet ported.
// These mirror the C++ forward declarations in channel_state.h.
// As the corresponding crates/modules are ported, replace with real imports.
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

    pub dma_pusher: Option<Box<crate::dma_pusher::DmaPusher>>,

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
    pub fn init(&mut self, _gpu: &crate::gpu::Gpu, program_id: u64) {
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

        // Create the DMA pusher for this channel.
        self.dma_pusher = Some(Box::new(crate::dma_pusher::DmaPusher::new()));

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
        // Forward to DMA pusher if available.
        // Upstream: dma_pusher->BindRasterizer(rasterizer)
        // The real DmaPusher will forward to its Puller.
        // DmaPusher::bind_rasterizer is not yet integrated — requires Puller port.
        if let Some(ref mut _dma) = self.dma_pusher {
            // dma.bind_rasterizer(rasterizer);
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
