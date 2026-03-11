//! Port of zuyu/src/core/hle/kernel/physical_core.h/.cpp
//! Status: COMPLET (stub — runtime dependencies not yet available)
//! Derniere synchro: 2026-03-11
//!
//! PhysicalCore: represents a single emulated CPU core, responsible for
//! running guest threads and handling interrupts. Full implementation
//! requires KernelCore, KThread, KProcess, ArmInterface, Debugger.

use std::sync::{Condvar, Mutex};

/// Represents a single emulated physical CPU core.
pub struct PhysicalCore {
    m_core_index: usize,
    m_guard: Mutex<PhysicalCoreState>,
    m_on_interrupt: Condvar,
}

struct PhysicalCoreState {
    m_is_interrupted: bool,
    m_is_single_core: bool,
    // m_arm_interface: Option<&mut ArmInterface>,
    // m_current_thread: Option<&mut KThread>,
}

impl PhysicalCore {
    pub fn new(core_index: usize, is_multicore: bool) -> Self {
        Self {
            m_core_index: core_index,
            m_guard: Mutex::new(PhysicalCoreState {
                m_is_interrupted: false,
                m_is_single_core: !is_multicore,
            }),
            m_on_interrupt: Condvar::new(),
        }
    }

    /// Execute guest code on the given thread.
    pub fn run_thread(&self) {
        // TODO: Implement once KThread, ArmInterface, Debugger are available.
    }

    /// Load context from thread to current core.
    pub fn load_context(&self) {
        // TODO: Implement once KThread, ArmInterface are available.
    }

    /// Save context from current core to thread.
    pub fn save_context(&self) {
        // TODO: Implement once KThread, ArmInterface are available.
    }

    /// Log backtrace of current processor state.
    pub fn log_backtrace(&self) {
        // TODO: Implement once KProcess, ArmInterface are available.
    }

    /// Wait for an interrupt.
    pub fn idle(&self) {
        let mut state = self.m_guard.lock().unwrap();
        while !state.m_is_interrupted {
            state = self.m_on_interrupt.wait(state).unwrap();
        }
    }

    /// Interrupt this core.
    pub fn interrupt(&self) {
        let mut state = self.m_guard.lock().unwrap();
        state.m_is_interrupted = true;
        self.m_on_interrupt.notify_one();
        // TODO: If arm_interface is set, signal interrupt.
    }

    /// Clear this core's interrupt flag.
    pub fn clear_interrupt(&self) {
        let mut state = self.m_guard.lock().unwrap();
        state.m_is_interrupted = false;
    }

    /// Check if this core is interrupted.
    pub fn is_interrupted(&self) -> bool {
        let state = self.m_guard.lock().unwrap();
        state.m_is_interrupted
    }

    /// Get the core index.
    pub fn core_index(&self) -> usize {
        self.m_core_index
    }
}
