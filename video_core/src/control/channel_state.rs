// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/control/channel_state.h and channel_state.cpp
//!
//! Per-GPU-channel state including engine instances and memory manager.

use std::sync::Arc;

use parking_lot::Mutex;

use crate::dma_pusher::DmaPusher;
use crate::engines::fermi_2d::Fermi2D;
use crate::engines::kepler_compute::KeplerCompute;
use crate::engines::kepler_memory::KeplerMemory;
use crate::engines::maxwell_3d::Maxwell3D;
use crate::engines::maxwell_dma::MaxwellDMA;
use crate::memory_manager::MemoryManager;
use crate::rasterizer_interface::RasterizerInterface;

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

        // Upstream creates DmaPusher first, then the engine set.
        self.dma_pusher = Some(Box::new(crate::dma_pusher::DmaPusher::new(
            _gpu as *const crate::gpu::Gpu,
            Arc::clone(self.memory_manager.as_ref().unwrap()),
            self as *mut ChannelState,
        )));
        self.dma_pusher
            .as_mut()
            .expect("DmaPusher must exist immediately after construction")
            .install_self_reference();

        let mut maxwell_3d = Box::new(Maxwell3D::new());
        maxwell_3d.set_memory_manager(Arc::clone(self.memory_manager.as_ref().unwrap()));
        let gpu_ptr = _gpu as *const crate::gpu::Gpu as usize;
        maxwell_3d.set_guest_memory_reader(Arc::new(move |addr, output| unsafe {
            let gpu = &*(gpu_ptr as *const crate::gpu::Gpu);
            let _ = gpu.read_guest_memory(addr, output);
        }));
        let gpu_ptr = _gpu as *const crate::gpu::Gpu as usize;
        maxwell_3d.set_guest_memory_writer(Arc::new(move |addr, data| unsafe {
            let gpu = &*(gpu_ptr as *const crate::gpu::Gpu);
            gpu.write_guest_memory(addr, data);
        }));
        let gpu_ptr = _gpu as *const crate::gpu::Gpu as usize;
        maxwell_3d.set_gpu_ticks_getter(Arc::new(move || unsafe {
            let gpu = &*(gpu_ptr as *const crate::gpu::Gpu);
            gpu.get_ticks()
        }));
        self.maxwell_3d = Some(maxwell_3d);
        self.fermi_2d = Some(Box::new(Fermi2D::new()));
        self.kepler_compute = Some(Box::new(KeplerCompute::new()));
        self.maxwell_dma = Some(Box::new(MaxwellDMA::new()));
        self.kepler_memory = Some(Box::new(KeplerMemory::new()));

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
    /// In the current port, this forwards the rasterizer reference through the
    /// same owner list as upstream.
    pub fn bind_rasterizer(&mut self, rasterizer: &dyn RasterizerInterface) {
        log::debug!("ChannelState::bind_rasterizer bind_id={}", self.bind_id);
        if let Some(ref mut dma) = self.dma_pusher {
            dma.bind_rasterizer(rasterizer);
        }
        if let Some(ref mut mm) = self.memory_manager {
            mm.lock().bind_rasterizer(rasterizer);
        }
        if let Some(ref mut maxwell_3d) = self.maxwell_3d {
            maxwell_3d.bind_rasterizer(rasterizer);
        }
        if let Some(ref mut fermi_2d) = self.fermi_2d {
            fermi_2d.bind_rasterizer(rasterizer);
        }
        if let Some(ref mut kepler_memory) = self.kepler_memory {
            kepler_memory.bind_rasterizer();
        }
        if let Some(ref mut kepler_compute) = self.kepler_compute {
            kepler_compute.bind_rasterizer(rasterizer);
        }
        if let Some(ref mut maxwell_dma) = self.maxwell_dma {
            maxwell_dma.bind_rasterizer(rasterizer);
        }
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
