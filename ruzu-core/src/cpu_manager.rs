//! Port of zuyu/src/core/cpu_manager.h and zuyu/src/core/cpu_manager.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-05
//!
//! Manages the CPU threads. Creates a thread per core and dispatches execution.
//! Currently simplified for single-core operation; multicore dispatch will be
//! added once the kernel scheduler and physical core abstractions are in place.

use crate::hardware_properties;
use ruzu_common::thread::Barrier;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

/// Per-core data held by the CPU manager.
struct CoreData {
    /// The host thread running this core.
    host_thread: Option<std::thread::JoinHandle<()>>,
}

impl Default for CoreData {
    fn default() -> Self {
        Self { host_thread: None }
    }
}

/// Manages CPU threads for the emulated system.
///
/// In the C++ code, this owns fiber contexts and dispatches guest threads
/// through the kernel scheduler. This Rust port provides the structural
/// framework; full guest thread dispatch requires the kernel module.
pub struct CpuManager {
    /// Barrier for GPU readiness synchronization.
    gpu_barrier: Option<Arc<Barrier>>,
    /// Per-core thread data.
    core_data: [CoreData; hardware_properties::NUM_CPU_CORES as usize],
    /// Whether we're using async GPU emulation.
    is_async_gpu: bool,
    /// Whether we're in multicore mode.
    is_multicore: bool,
    /// Currently active core index.
    current_core: AtomicUsize,
    /// Count of idle iterations (single-core).
    idle_count: usize,
    /// Number of active cores.
    num_cores: usize,
}

/// Maximum number of cycle runs before preemption in single-core mode.
const _MAX_CYCLE_RUNS: usize = 5;

impl CpuManager {
    /// Creates a new CpuManager.
    pub fn new() -> Self {
        Self {
            gpu_barrier: None,
            core_data: Default::default(),
            is_async_gpu: false,
            is_multicore: false,
            current_core: AtomicUsize::new(0),
            idle_count: 0,
            num_cores: 0,
        }
    }

    /// Sets if emulation is multicore or single core. Must be set before Initialize.
    pub fn set_multicore(&mut self, is_multi: bool) {
        self.is_multicore = is_multi;
    }

    /// Sets if emulation is using an asynchronous GPU.
    pub fn set_async_gpu(&mut self, is_async: bool) {
        self.is_async_gpu = is_async;
    }

    /// Called when the GPU is ready. Synchronizes with the GPU barrier.
    pub fn on_gpu_ready(&self) {
        if let Some(ref barrier) = self.gpu_barrier {
            barrier.sync();
        }
    }

    /// Initializes the CPU manager, creating threads for each core.
    ///
    /// In the C++ code, this spawns jthreads that call RunThread for each core.
    /// The threads create fiber contexts and synchronize via a GPU barrier before
    /// entering the kernel scheduler.
    ///
    /// For the initial Rust port, we create the barrier but defer actual thread
    /// spawning until the kernel scheduler is available.
    pub fn initialize(&mut self) {
        self.num_cores = if self.is_multicore {
            hardware_properties::NUM_CPU_CORES as usize
        } else {
            1
        };

        // Create GPU barrier: num_cores + 1 (the +1 is for the GPU thread)
        self.gpu_barrier = Some(Arc::new(Barrier::new(self.num_cores + 1)));

        log::info!(
            "CpuManager: initialized for {} core(s), multicore={}",
            self.num_cores,
            self.is_multicore
        );

        // NOTE: Actual thread spawning deferred until kernel scheduler is available.
        // The C++ code spawns threads here that call RunThread(), which:
        //   1. Registers the thread with the kernel
        //   2. Waits on the GPU barrier
        //   3. Obtains the GPU context (single-core, sync GPU)
        //   4. Gets the current scheduler thread
        //   5. Yields to the guest fiber context
    }

    /// Shuts down all CPU threads.
    pub fn shutdown(&mut self) {
        for i in 0..self.num_cores {
            if let Some(thread) = self.core_data[i].host_thread.take() {
                let _ = thread.join();
            }
        }
        log::info!("CpuManager: shutdown complete");
    }

    /// Returns the currently active core index.
    pub fn current_core(&self) -> usize {
        self.current_core.load(Ordering::Relaxed)
    }

    /// Returns whether multicore mode is enabled.
    pub fn is_multicore(&self) -> bool {
        self.is_multicore
    }

    /// Returns whether async GPU mode is enabled.
    pub fn is_async_gpu(&self) -> bool {
        self.is_async_gpu
    }

    /// Preempts the current core in single-core mode.
    ///
    /// In the C++ code, this advances the core timing, rotates to the next core,
    /// and triggers the kernel scheduler preemption.
    pub fn preempt_single_core(&mut self, from_running_environment: bool) {
        if self.idle_count >= 4 || from_running_environment {
            if !from_running_environment {
                self.idle_count = 0;
            }
            // In C++: kernel.SetIsPhantomModeForSingleCore(true);
            // system.CoreTiming().Advance();
            // kernel.SetIsPhantomModeForSingleCore(false);
        }

        let next_core =
            (self.current_core.load(Ordering::Relaxed) + 1) % hardware_properties::NUM_CPU_CORES as usize;
        self.current_core.store(next_core, Ordering::Relaxed);

        // In C++: system.CoreTiming().ResetTicks();
        // kernel.Scheduler(current_core).PreemptSingleCore();
    }
}

impl Default for CpuManager {
    fn default() -> Self {
        Self::new()
    }
}
