// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/dynarmic/arm_dynarmic.h and arm_dynarmic.cpp
//! Common dynarmic base: halt reason translation and JIT execution scoping.

use crate::arm::arm_interface::HaltReason;

/// Dynarmic halt reason constants.
/// These map Core::HaltReason values to Dynarmic::HaltReason values.
/// Upstream has static_asserts confirming these match.
pub const STEP_THREAD: u64 = HaltReason::STEP_THREAD.bits();
pub const DATA_ABORT: u64 = HaltReason::DATA_ABORT.bits();
pub const BREAK_LOOP: u64 = HaltReason::BREAK_LOOP.bits();
pub const SUPERVISOR_CALL: u64 = HaltReason::SUPERVISOR_CALL.bits();
pub const INSTRUCTION_BREAKPOINT: u64 = HaltReason::INSTRUCTION_BREAKPOINT.bits();
pub const PREFETCH_ABORT: u64 = HaltReason::PREFETCH_ABORT.bits();

/// Translate a raw dynarmic halt reason value to our HaltReason bitflags.
///
/// Corresponds to upstream `Core::TranslateHaltReason`.
pub fn translate_halt_reason(hr: u64) -> HaltReason {
    HaltReason::from_bits_truncate(hr)
}

// ---------------------------------------------------------------------------
// ScopedJitExecution
// ---------------------------------------------------------------------------

#[cfg(target_os = "linux")]
mod linux_impl {
    use crate::memory::memory::Memory;
    use std::sync::{Arc, Mutex, Once};

    /// Thread-local pointer to the current process's Memory.
    /// Upstream: `thread_local Core::Memory::Memory* g_current_memory{};`
    std::thread_local! {
        static G_CURRENT_MEMORY: std::cell::Cell<*const Memory> =
            const { std::cell::Cell::new(std::ptr::null()) };
    }

    /// Once flag for signal handler registration.
    /// Upstream: `std::once_flag g_registered{};`
    static REGISTERED: Once = Once::new();

    /// Old SIGSEGV action to chain to if the fault is not ours.
    /// Upstream: `struct sigaction g_old_segv{};`
    static mut OLD_SEGV: libc::sigaction = unsafe { std::mem::zeroed() };

    /// SIGSEGV handler.
    /// Upstream: `HandleSigSegv(int sig, siginfo_t* info, void* ctx)`.
    ///
    /// If g_current_memory is set and the fault address belongs to the
    /// separate heap, invalidate it and return. Otherwise chain to old handler.
    unsafe extern "C" fn handle_sigsegv(
        sig: libc::c_int,
        info: *mut libc::siginfo_t,
        ctx: *mut libc::c_void,
    ) {
        let memory_ptr = G_CURRENT_MEMORY.with(|m| m.get());
        if !memory_ptr.is_null() {
            let fault_addr = (*info).si_addr();
            let memory = &*memory_ptr;
            if memory.invalidate_separate_heap(fault_addr as *const u8) {
                return;
            }
        }

        // Chain to old handler.
        let old_handler = OLD_SEGV.sa_sigaction;
        if old_handler != 0 && old_handler != libc::SIG_DFL && old_handler != libc::SIG_IGN {
            let handler_fn: unsafe extern "C" fn(libc::c_int, *mut libc::siginfo_t, *mut libc::c_void) =
                std::mem::transmute(old_handler);
            handler_fn(sig, info, ctx);
        }
    }

    /// Set the thread-local memory pointer for the current JIT execution scope.
    pub fn set_current_memory(memory: *const Memory) {
        G_CURRENT_MEMORY.with(|m| m.set(memory));
    }

    /// Clear the thread-local memory pointer.
    pub fn clear_current_memory() {
        G_CURRENT_MEMORY.with(|m| m.set(std::ptr::null()));
    }

    /// Register the SIGSEGV handler (once).
    /// Upstream: `ScopedJitExecution::RegisterHandler()`.
    pub fn register_handler() {
        REGISTERED.call_once(|| unsafe {
            let mut sa: libc::sigaction = std::mem::zeroed();
            sa.sa_sigaction = handle_sigsegv as usize;
            sa.sa_flags = libc::SA_SIGINFO | libc::SA_ONSTACK;
            libc::sigaction(libc::SIGSEGV, &sa, &mut OLD_SEGV);
        });
    }
}

/// RAII guard for JIT execution scope.
///
/// Corresponds to upstream `Core::ScopedJitExecution` (arm_dynarmic.cpp).
///
/// On Linux: constructor sets thread-local g_current_memory to the process's
/// Memory; destructor clears it. RegisterHandler installs a SIGSEGV handler
/// that calls memory->InvalidateSeparateHeap(info->si_addr).
///
/// On non-Linux: no-op (matches upstream `#else` branch).
pub struct ScopedJitExecution {
    _private: (),
}

impl ScopedJitExecution {
    /// Create a new JIT execution scope guard.
    ///
    /// Upstream (Linux): `g_current_memory = &process->GetMemory();`
    pub fn new(process: &dyn std::any::Any) -> Self {
        #[cfg(target_os = "linux")]
        {
            // Transmute the opaque process to get its Memory pointer.
            // The process is passed as &dyn Any but is actually a KProcess.
            if let Some(real_process) = process.downcast_ref::<crate::hle::kernel::k_process::KProcess>() {
                let memory = real_process.get_shared_memory();
                let mem_guard = memory.read().unwrap();
                let mem_ptr = &*mem_guard as *const _ as *const crate::memory::memory::Memory;
                // SAFETY: The Memory is behind an Arc<RwLock> and lives for the
                // duration of the process. The pointer is valid while the guard
                // is held, but we store it for the JIT to use during execution.
                // The JIT only accesses it within this scope.
                drop(mem_guard);
                // For now, set null — full integration requires storing the
                // Memory* from System::memory_shared() which outlives the scope.
                linux_impl::set_current_memory(std::ptr::null());
            }
        }
        let _ = process;
        Self { _private: () }
    }

    /// Register signal handlers for JIT execution.
    ///
    /// Upstream (Linux): installs SIGSEGV handler via std::call_once.
    /// Non-Linux: no-op.
    pub fn register_handler() {
        #[cfg(target_os = "linux")]
        {
            linux_impl::register_handler();
        }
    }
}

impl Drop for ScopedJitExecution {
    /// Upstream (Linux): `g_current_memory = nullptr;`
    fn drop(&mut self) {
        #[cfg(target_os = "linux")]
        {
            linux_impl::clear_current_memory();
        }
    }
}
