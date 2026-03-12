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
    pub fn bind_rasterizer(&mut self, _rasterizer: &dyn RasterizerInterface) {
        todo!()
    }

    /// Placeholder for `DmaPusher::Push`.
    pub fn push(&mut self, _entries: CommandList) {
        todo!()
    }

    /// Placeholder for `DmaPusher::DispatchCalls`.
    pub fn dispatch_calls(&mut self) {
        todo!()
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
    pub fn bind_channel(&mut self, _bind_id: i32) {
        todo!()
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
    pub fn init(&mut self, _system: &System, _gpu: &Gpu, program_id: u64) {
        assert!(
            self.memory_manager.is_some(),
            "memory_manager must be set before Init"
        );
        self.program_id = program_id;
        // Upstream creates all engines and the DMA pusher here.
        // TODO: instantiate real engine types once they accept MemoryManager.
        todo!("ChannelState::init — instantiate engines and DmaPusher")
    }

    /// Bind a rasterizer to all engines and the memory manager.
    ///
    /// Corresponds to `ChannelState::BindRasterizer(RasterizerInterface*)`.
    pub fn bind_rasterizer(&mut self, _rasterizer: &dyn RasterizerInterface) {
        // Upstream forwards the rasterizer to every sub-component:
        //   dma_pusher, memory_manager, maxwell_3d, fermi_2d,
        //   kepler_memory, kepler_compute, maxwell_dma
        todo!("ChannelState::bind_rasterizer — forward to all engines")
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
