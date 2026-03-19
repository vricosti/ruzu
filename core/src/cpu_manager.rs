//! Port of zuyu/src/core/cpu_manager.h and zuyu/src/core/cpu_manager.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-19
//!
//! Manages the CPU threads. Creates a thread per core and dispatches execution.

use crate::core_timing::CoreTiming;
use crate::hardware_properties;
use common::fiber::Fiber;
use common::thread::Barrier;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

/// Per-core data held by the CPU manager.
/// Matches upstream `CpuManager::CoreData`.
struct CoreData {
    /// Host fiber context for this core's thread.
    /// Upstream: `std::shared_ptr<Common::Fiber> host_context`
    host_context: Option<Arc<Fiber>>,
    /// The host thread running this core.
    /// Upstream: `std::jthread host_thread`
    host_thread: Option<std::thread::JoinHandle<()>>,
}

impl Default for CoreData {
    fn default() -> Self {
        Self {
            host_context: None,
            host_thread: None,
        }
    }
}

/// Manages CPU threads for the emulated system.
/// Matches upstream `CpuManager` class (cpu_manager.h).
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
    /// Upstream spawns jthreads that call RunThread for each core.
    /// Thread spawning is deferred until the kernel scheduler is available.
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
        //   2. Creates a fiber via ThreadToFiber (data.host_context)
        //   3. Waits on the GPU barrier
        //   4. Obtains the GPU context (single-core, sync GPU)
        //   5. Gets the current scheduler thread
        //   6. Yields to the guest fiber context via Fiber::YieldTo
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

    /// Returns the GuestActivate closure.
    /// Upstream: `GetGuestActivateFunc() { return [this] { GuestActivate(); }; }`
    /// The returned closure calls `KScheduler::Activate()` then unreachable.
    pub fn get_guest_activate_func(&self) -> Box<dyn FnOnce() + Send> {
        // Upstream: GuestActivate gets the current scheduler, calls Activate(), then UNREACHABLE.
        // The actual implementation requires access to the kernel, which will be wired up
        // when RunThread is fully implemented.
        Box::new(|| {
            log::info!("CpuManager::GuestActivate called");
            // TODO: kernel.CurrentScheduler()->Activate(); UNREACHABLE();
        })
    }

    /// Returns the GuestThreadFunction closure.
    /// Upstream: `GetGuestThreadFunc() { return [this] { GuestThreadFunction(); }; }`
    pub fn get_guest_thread_func(&self) -> Box<dyn FnOnce() + Send> {
        let is_multicore = self.is_multicore;
        Box::new(move || {
            if is_multicore {
                // MultiCoreRunGuestThread
                log::info!("CpuManager::MultiCoreRunGuestThread called");
                // TODO: kernel scheduler OnThreadStart, then run loop
            } else {
                // SingleCoreRunGuestThread
                log::info!("CpuManager::SingleCoreRunGuestThread called");
                // TODO: kernel scheduler OnThreadStart, then run loop with PreemptSingleCore
            }
        })
    }

    /// Returns the IdleThreadFunction closure.
    /// Upstream: `GetIdleThreadStartFunc() { return [this] { IdleThreadFunction(); }; }`
    pub fn get_idle_thread_start_func(&self) -> Box<dyn FnOnce() + Send> {
        let is_multicore = self.is_multicore;
        Box::new(move || {
            if is_multicore {
                // MultiCoreRunIdleThread
                log::info!("CpuManager::MultiCoreRunIdleThread called");
                // TODO: kernel scheduler OnThreadStart, idle loop
            } else {
                // SingleCoreRunIdleThread
                log::info!("CpuManager::SingleCoreRunIdleThread called");
                // TODO: PreemptSingleCore(false), AddTicks, idle loop
            }
        })
    }

    /// Returns the ShutdownThreadFunction closure.
    /// Upstream: `GetShutdownThreadStartFunc() { return [this] { ShutdownThreadFunction(); }; }`
    pub fn get_shutdown_thread_start_func(&self) -> Box<dyn FnOnce() + Send> {
        Box::new(|| {
            log::info!("CpuManager::ShutdownThreadFunction called");
            // TODO: ShutdownThread — yields from thread's host_context
            // to core_data[core].host_context
        })
    }

    /// Preempts the current core in single-core mode.
    /// Matches upstream `CpuManager::PreemptSingleCore`.
    pub fn preempt_single_core(
        &mut self,
        core_timing: &Arc<Mutex<CoreTiming>>,
        from_running_environment: bool,
    ) {
        if self.idle_count >= 4 || from_running_environment {
            if !from_running_environment {
                core_timing.lock().unwrap().idle();
                self.idle_count = 0;
            }
            let _ = core_timing.lock().unwrap().advance();
        }

        let next_core = (self.current_core.load(Ordering::Relaxed) + 1)
            % hardware_properties::NUM_CPU_CORES as usize;
        self.current_core.store(next_core, Ordering::Relaxed);

        core_timing.lock().unwrap().reset_ticks();
        // TODO: kernel.Scheduler(current_core).preempt_single_core();

        // Check if the new scheduler is idle
        // if !kernel.Scheduler(current_core).is_idle() { self.idle_count = 0; }
    }
}

impl Default for CpuManager {
    fn default() -> Self {
        Self::new()
    }
}
