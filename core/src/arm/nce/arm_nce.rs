// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/nce/arm_nce.h and arm_nce.cpp
//! Main ARM Native Code Execution (NCE) backend.
//!
//! The NCE backend runs guest ARM64 code natively on the host (AArch64 Linux),
//! using signal handlers to intercept system calls and faults. This is a
//! Linux-only, AArch64-only backend.
//!
//! NOTE: Most of the functionality in this file is deeply tied to Linux signal
//! handling, assembly trampolines (`arm_nce.s`), and direct manipulation of
//! `ucontext_t`/`mcontext_t` from signal handlers. These cannot be directly
//! ported to safe Rust and require `unsafe` + platform-specific code.
//! This port provides the structural skeleton and safe wrappers.

use super::arm_nce_asm_definitions::*;
use super::guest_context::GuestContext;
use std::sync::atomic::Ordering;

/// Halt reason flags returned from guest execution.
///
/// Corresponds to upstream `HaltReason` enum.
/// These are bit flags that can be OR'd together.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u64)]
pub enum HaltReason {
    /// No halt requested.
    None = 0,
    /// Step thread (single-step).
    StepThread = 1 << 0,
    /// Supervisor call (SVC) was executed.
    SupervisorCall = 1 << 1,
    /// Break loop requested via signal.
    BreakLoop = 1 << 2,
    /// Prefetch abort occurred.
    PrefetchAbort = 1 << 3,
    /// Data abort occurred.
    DataAbort = 1 << 4,
    /// Instruction cache invalidation.
    InstructionCacheInvalidation = 1 << 5,
}

impl HaltReason {
    pub fn from_raw(raw: u64) -> Self {
        // Just store the raw value; upstream uses bitwise OR of multiple reasons.
        // In practice we'd use a bitflags approach.
        unsafe { std::mem::transmute(raw) }
    }
}

/// ARM NCE backend architecture type.
///
/// Corresponds to upstream `Architecture::AArch64`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Architecture {
    AArch64,
}

/// ARM Native Code Execution backend.
///
/// Corresponds to upstream `Core::ArmNce`.
///
/// This backend runs guest ARM64 code directly on AArch64 Linux hosts,
/// using signal handlers to intercept SVCs, MRS/MSR instructions, and faults.
pub struct ArmNce {
    /// Back-reference to the system (opaque pointer).
    pub system: *mut std::ffi::c_void,

    /// Core index this instance is bound to.
    pub core_index: usize,

    /// Linux thread ID for signal delivery.
    pub thread_id: i32,

    /// Guest CPU context.
    pub guest_ctx: GuestContext,

    /// Currently running kernel thread (opaque pointer).
    /// Corresponds to upstream `m_running_thread`.
    pub running_thread: *mut std::ffi::c_void,

    /// Signal stack allocation.
    /// Corresponds to upstream `m_stack`.
    stack: Option<Box<[u8]>>,
}

// SAFETY: ArmNce is only used from a single thread at a time (the core thread).
unsafe impl Send for ArmNce {}

impl ArmNce {
    /// Create a new NCE backend instance.
    ///
    /// Corresponds to upstream `ArmNce::ArmNce(System&, bool, size_t)`.
    pub fn new(
        system: *mut std::ffi::c_void,
        _uses_wall_clock: bool,
        core_index: usize,
    ) -> Self {
        let mut nce = Self {
            system,
            core_index,
            thread_id: -1,
            guest_ctx: GuestContext::default(),
            running_thread: std::ptr::null_mut(),
            stack: None,
        };
        nce.guest_ctx.system = system;
        nce
    }

    /// Get the architecture of this backend.
    ///
    /// Corresponds to upstream `ArmNce::GetArchitecture()`.
    pub fn get_architecture(&self) -> Architecture {
        Architecture::AArch64
    }

    /// Initialize the NCE backend for the current thread.
    ///
    /// Sets up the signal stack and installs signal handlers (once).
    ///
    /// Corresponds to upstream `ArmNce::Initialize()`.
    pub fn initialize(&mut self) {
        if self.thread_id == -1 {
            // gettid() equivalent
            #[cfg(target_os = "linux")]
            {
                self.thread_id = unsafe { libc::syscall(libc::SYS_gettid) as i32 };
            }
            #[cfg(not(target_os = "linux"))]
            {
                self.thread_id = 0;
            }
        }

        // Configure signal stack.
        if self.stack.is_none() {
            let stack_mem = vec![0u8; STACK_SIZE].into_boxed_slice();

            #[cfg(target_os = "linux")]
            unsafe {
                let mut ss: libc::stack_t = std::mem::zeroed();
                ss.ss_sp = stack_mem.as_ptr() as *mut std::ffi::c_void;
                ss.ss_size = STACK_SIZE;
                ss.ss_flags = 0;
                libc::sigaltstack(&ss, std::ptr::null_mut());
            }

            self.stack = Some(stack_mem);
        }

        // Install signal handlers.
        // Upstream installs handlers for:
        //   - ReturnToRunCodeByExceptionLevelChangeSignal (SIGUSR2)
        //   - BreakFromRunCodeSignal (SIGURG)
        //   - GuestAlignmentFaultSignal (SIGBUS)
        //   - GuestAccessFaultSignal (SIGSEGV)
        // Full handlers require assembly trampolines for context save/restore.
        // For now, install a minimal SIGSEGV handler for diagnostics.
        #[cfg(target_os = "linux")]
        install_guest_access_fault_handler();
    }

    /// Run a guest thread until a halt condition occurs.
    ///
    /// Corresponds to upstream `ArmNce::RunThread(KThread*)`.
    pub fn run_thread(&mut self) -> u64 {
        // Check if already interrupted.
        let hr = self.guest_ctx.esr_el1.swap(0, Ordering::SeqCst);
        if hr != 0 {
            return hr;
        }

        // TODO: Full implementation requires:
        // 1. Assign thread parameters (native_context, tpidr_el0, etc.)
        // 2. Call ReturnToRunCodeByTrampoline or ReturnToRunCodeByExceptionLevelChange
        //    (assembly functions)
        // 3. Unload thread parameters on return
        log::warn!("ArmNce::run_thread: not yet implemented (requires assembly trampolines)");

        HaltReason::StepThread as u64
    }

    /// Single-step a guest thread.
    ///
    /// Corresponds to upstream `ArmNce::StepThread(KThread*)`.
    pub fn step_thread(&mut self) -> u64 {
        HaltReason::StepThread as u64
    }

    /// Get the SVC number from the last supervisor call.
    ///
    /// Corresponds to upstream `ArmNce::GetSvcNumber()`.
    pub fn get_svc_number(&self) -> u32 {
        self.guest_ctx.svc
    }

    /// Get SVC arguments (x0-x7).
    ///
    /// Corresponds to upstream `ArmNce::GetSvcArguments(span<uint64_t, 8>)`.
    pub fn get_svc_arguments(&self) -> [u64; 8] {
        let mut args = [0u64; 8];
        for i in 0..8 {
            args[i] = self.guest_ctx.cpu_registers[i];
        }
        args
    }

    /// Set SVC arguments (x0-x7).
    ///
    /// Corresponds to upstream `ArmNce::SetSvcArguments(span<const uint64_t, 8>)`.
    pub fn set_svc_arguments(&mut self, args: &[u64; 8]) {
        for i in 0..8 {
            self.guest_ctx.cpu_registers[i] = args[i];
        }
    }

    /// Set the TPIDRRO_EL0 register value.
    ///
    /// Corresponds to upstream `ArmNce::SetTpidrroEl0(u64)`.
    pub fn set_tpidrro_el0(&mut self, value: u64) {
        self.guest_ctx.tpidrro_el0 = value;
    }

    /// Signal an interrupt to the running guest thread.
    ///
    /// Corresponds to upstream `ArmNce::SignalInterrupt(KThread*)`.
    pub fn signal_interrupt(&mut self) {
        // Add break loop condition.
        self.guest_ctx
            .esr_el1
            .fetch_or(HaltReason::BreakLoop as u64, Ordering::SeqCst);

        // TODO: Lock thread parameters, check if running, send SIGURG via tkill.
        // Requires KThread integration.
        log::warn!("ArmNce::signal_interrupt: not yet fully implemented");
    }

    /// Clear the instruction cache.
    ///
    /// Corresponds to upstream `ArmNce::ClearInstructionCache()`.
    pub fn clear_instruction_cache(&self) {
        // TODO: This is not possible to implement correctly on Linux because
        // we do not have any access to ic iallu.
        std::sync::atomic::fence(Ordering::SeqCst);
    }

    /// Invalidate a range of the instruction cache.
    ///
    /// Corresponds to upstream `ArmNce::InvalidateCacheRange(u64, size_t)`.
    pub fn invalidate_cache_range(&self, _addr: u64, _size: usize) {
        self.clear_instruction_cache();
    }

    /// Get the halted watchpoint (always None for NCE).
    ///
    /// Corresponds to upstream `ArmNce::HaltedWatchpoint()`.
    pub fn halted_watchpoint(&self) -> Option<()> {
        None
    }
}

impl Drop for ArmNce {
    fn drop(&mut self) {
        // Corresponds to upstream `ArmNce::~ArmNce() = default`.
    }
}

// ---------------------------------------------------------------------------
// SIGSEGV signal handler for guest memory access faults
// ---------------------------------------------------------------------------
//
// Matches upstream `HandleGuestAccessFault` (arm_nce.cpp).
// When the JIT accesses memory through the fastmem arena (mmap'd virtual
// space), an unmapped or mprotect'd page triggers SIGSEGV. This handler
// logs the fault address and aborts with diagnostic context.
//
// A more advanced handler (lazy mapping, separate heap, etc.) can be added
// later by extending this handler.

#[cfg(target_os = "linux")]
static HANDLER_INSTALLED: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

/// Install a minimal SIGSEGV handler for guest access fault diagnostics.
///
/// Matches upstream signal handler installation in `ArmNce::Initialize()`.
/// Only installs once (uses atomic flag like upstream's `std::call_once`).
#[cfg(target_os = "linux")]
fn install_guest_access_fault_handler() {
    if HANDLER_INSTALLED.swap(true, std::sync::atomic::Ordering::SeqCst) {
        return; // Already installed.
    }

    unsafe {
        let mut sa: libc::sigaction = std::mem::zeroed();
        sa.sa_sigaction = guest_access_fault_handler as usize;
        sa.sa_flags = libc::SA_SIGINFO | libc::SA_ONSTACK;
        libc::sigemptyset(&mut sa.sa_mask);
        let result = libc::sigaction(libc::SIGSEGV, &sa, std::ptr::null_mut());
        if result != 0 {
            log::error!(
                "Failed to install SIGSEGV handler: {}",
                std::io::Error::last_os_error()
            );
        } else {
            log::info!("Installed SIGSEGV handler for guest access fault diagnostics");
        }
    }
}

/// SIGSEGV handler — logs the faulting address and aborts.
///
/// Matches upstream `HandleGuestAccessFault` which checks if the faulting
/// address is in the guest address space (fastmem arena) and either:
/// - Handles the fault (lazy mapping / permission change)
/// - Converts to a guest data abort
/// - Re-raises for host crashes
///
/// For now: log and abort with context for debugging.
#[cfg(target_os = "linux")]
extern "C" fn guest_access_fault_handler(
    sig: libc::c_int,
    info: *mut libc::siginfo_t,
    _ucontext: *mut libc::c_void,
) {
    let fault_addr = unsafe { (*info).si_addr() as u64 };

    // Log the fault — use write() directly since log macros may not be
    // signal-safe, but eprintln uses write() under the hood.
    eprintln!(
        "\n=== SIGSEGV (signal {}) at address {:#018x} ===",
        sig, fault_addr
    );
    eprintln!("This may be a guest memory access fault (unmapped or protected page).");
    eprintln!("If this address is within the fastmem arena, it's likely a guest fault.");
    eprintln!("Otherwise, it's a host crash.\n");

    // Re-raise the signal with default handler to get a core dump.
    unsafe {
        let mut sa: libc::sigaction = std::mem::zeroed();
        sa.sa_sigaction = libc::SIG_DFL;
        libc::sigaction(libc::SIGSEGV, &sa, std::ptr::null_mut());
        libc::raise(libc::SIGSEGV);
    }
}
