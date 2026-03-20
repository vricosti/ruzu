// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/nce/arm_nce.h and arm_nce.cpp
//! Main ARM Native Code Execution (NCE) backend.
//!
//! The NCE backend runs guest ARM64 code natively on AArch64 Linux hosts,
//! using signal handlers to intercept SVCs, MRS/MSR instructions, and faults.
//!
//! This entire module is AArch64-Linux-only. On x86_64 hosts, the Dynarmic
//! JIT backend is used instead. Upstream guards this with `#ifdef HAS_NCE`
//! and `#ifdef ARCHITECTURE_arm64`.

use super::arm_nce_asm_definitions::*;
use super::guest_context::GuestContext;
use std::sync::atomic::Ordering;

/// Halt reason flags returned from guest execution.
/// Corresponds to upstream `HaltReason` enum (arm_nce.h).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u64)]
pub enum HaltReason {
    None = 0,
    StepThread = 1 << 0,
    SupervisorCall = 1 << 1,
    BreakLoop = 1 << 2,
    PrefetchAbort = 1 << 3,
    DataAbort = 1 << 4,
    InstructionCacheInvalidation = 1 << 5,
}

impl HaltReason {
    pub fn from_raw(raw: u64) -> Self {
        unsafe { std::mem::transmute(raw) }
    }
}

/// NCE backend architecture type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Architecture {
    AArch64,
}

/// ARM Native Code Execution backend.
///
/// Corresponds to upstream `Core::ArmNce`.
/// AArch64-Linux-only — on x86_64, use ArmDynarmic32/64 instead.
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
    pub running_thread: *mut std::ffi::c_void,
    /// Signal stack allocation.
    stack: Option<Box<[u8]>>,
}

// SAFETY: ArmNce is only used from a single thread at a time (the core thread).
unsafe impl Send for ArmNce {}

impl ArmNce {
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

    pub fn get_architecture(&self) -> Architecture {
        Architecture::AArch64
    }

    /// Initialize the NCE backend for the current thread.
    /// Sets up signal stack and installs signal handlers.
    /// Corresponds to upstream `ArmNce::Initialize()` (arm_nce.cpp:258-314).
    pub fn initialize(&mut self) {
        if self.thread_id == -1 {
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

        // Install signal handlers (once).
        // Upstream installs handlers for SIGUSR2, SIGURG, SIGBUS, SIGSEGV
        // with assembly trampolines for context save/restore.
        // These require AArch64-specific ucontext_t manipulation.
        #[cfg(all(target_os = "linux", target_arch = "aarch64"))]
        install_nce_signal_handlers();

        // On non-AArch64 or non-Linux, signal handlers are not installed
        // (the NCE backend is not used on these platforms).
    }

    /// Run a guest thread until a halt condition occurs.
    /// Corresponds to upstream `ArmNce::RunThread(KThread*)` (arm_nce.cpp:191-229).
    #[cfg(all(target_os = "linux", target_arch = "aarch64"))]
    pub fn run_thread(&mut self) -> u64 {
        // Check if already interrupted.
        let hr = self.guest_ctx.esr_el1.swap(0, Ordering::SeqCst);
        if hr != 0 {
            return hr;
        }

        // Upstream:
        // 1. Assigns thread parameters (native_context, tpidr_el0, etc.)
        // 2. Checks post_handlers for trampoline entry
        // 3. Calls ReturnToRunCodeByTrampoline or ReturnToRunCodeByExceptionLevelChange
        //    (ARM64 assembly functions from arm_nce.s)
        // 4. On return, unloads thread parameters
        //
        // The assembly functions are in arm_nce.s and perform:
        //   - Save host callee-saved registers
        //   - Restore guest registers from GuestContext
        //   - Change TPIDR_EL0 to guest value
        //   - Return to guest PC via ERET-like mechanism (signal return)
        //
        // This requires the arm_nce.s assembly to be compiled and linked.
        // On AArch64 Linux, we would call:
        //   ReturnToRunCodeByExceptionLevelChange(self.thread_id, thread_params)
        // which sends SIGUSR2 to self, and the signal handler performs the
        // context switch to guest code.

        // For now, return StepThread since the assembly trampolines are not
        // yet compiled into the Rust binary.
        // Upstream: arm_nce.s must be assembled with the Rust build.
        log::trace!("ArmNce::run_thread: assembly trampoline not yet linked");
        HaltReason::StepThread as u64
    }

    /// Run a guest thread — non-AArch64 stub.
    #[cfg(not(all(target_os = "linux", target_arch = "aarch64")))]
    pub fn run_thread(&mut self) -> u64 {
        // NCE is AArch64-Linux only. On x86_64, the Dynarmic backend is used.
        HaltReason::StepThread as u64
    }

    /// Single-step a guest thread.
    /// Upstream returns StepThread immediately (arm_nce.cpp:231-233).
    pub fn step_thread(&mut self) -> u64 {
        HaltReason::StepThread as u64
    }

    pub fn get_svc_number(&self) -> u32 {
        self.guest_ctx.svc
    }

    pub fn get_svc_arguments(&self) -> [u64; 8] {
        let mut args = [0u64; 8];
        for i in 0..8 {
            args[i] = self.guest_ctx.cpu_registers[i];
        }
        args
    }

    pub fn set_svc_arguments(&mut self, args: &[u64; 8]) {
        for i in 0..8 {
            self.guest_ctx.cpu_registers[i] = args[i];
        }
    }

    pub fn set_tpidrro_el0(&mut self, value: u64) {
        self.guest_ctx.tpidrro_el0 = value;
    }

    /// Signal an interrupt to the running guest thread.
    /// Corresponds to upstream `ArmNce::SignalInterrupt(KThread*)` (arm_nce.cpp:350-366).
    pub fn signal_interrupt(&mut self) {
        // Add break loop condition.
        self.guest_ctx
            .esr_el1
            .fetch_or(HaltReason::BreakLoop as u64, Ordering::SeqCst);

        // Upstream:
        // 1. LockThreadParameters(params) — spin-lock on the thread's native params
        // 2. If params->is_running, send SIGURG via tkill(thread_id, BreakFromRunCodeSignal)
        // 3. Else UnlockThreadParameters(params)
        //
        // This requires KThread::GetNativeExecutionParameters() and the
        // BreakFromRunCodeSignal constant (SIGURG).
        #[cfg(all(target_os = "linux", target_arch = "aarch64"))]
        {
            if self.thread_id > 0 {
                // Send SIGURG to interrupt the guest thread.
                // The signal handler (BreakFromRunCodeSignalHandler) will
                // save guest context and return to the host.
                unsafe {
                    libc::syscall(libc::SYS_tkill, self.thread_id as libc::c_long,
                                  BREAK_FROM_RUN_CODE_SIGNAL as libc::c_long);
                }
            }
        }
    }

    /// Clear the instruction cache.
    /// Upstream (arm_nce.cpp:368-374): has the same limitation comment.
    pub fn clear_instruction_cache(&self) {
        // Upstream TODO: This is not possible to implement correctly on Linux
        // because we do not have any access to ic iallu.
        // Require accesses to complete.
        std::sync::atomic::fence(Ordering::SeqCst);
    }

    pub fn invalidate_cache_range(&self, _addr: u64, _size: usize) {
        self.clear_instruction_cache();
    }

    pub fn halted_watchpoint(&self) -> Option<()> {
        None
    }
}

impl Drop for ArmNce {
    fn drop(&mut self) {}
}

// ---------------------------------------------------------------------------
// Signal constants matching upstream arm_nce_asm_definitions.h
// ---------------------------------------------------------------------------

/// Signal used to return to guest code via exception level change.
/// Upstream: `ReturnToRunCodeByExceptionLevelChangeSignal = SIGUSR2`.
#[cfg(target_os = "linux")]
const RETURN_TO_RUN_CODE_SIGNAL: i32 = libc::SIGUSR2;

/// Signal used to break from running guest code.
/// Upstream: `BreakFromRunCodeSignal = SIGURG`.
#[cfg(target_os = "linux")]
const BREAK_FROM_RUN_CODE_SIGNAL: i32 = libc::SIGURG;

/// Signal for guest alignment faults.
/// Upstream: `GuestAlignmentFaultSignal = SIGBUS`.
#[cfg(target_os = "linux")]
const GUEST_ALIGNMENT_FAULT_SIGNAL: i32 = libc::SIGBUS;

/// Signal for guest access faults.
/// Upstream: `GuestAccessFaultSignal = SIGSEGV`.
#[cfg(target_os = "linux")]
const GUEST_ACCESS_FAULT_SIGNAL: i32 = libc::SIGSEGV;

// ---------------------------------------------------------------------------
// Signal handler installation — AArch64 Linux only
// ---------------------------------------------------------------------------

#[cfg(all(target_os = "linux", target_arch = "aarch64"))]
static NCE_HANDLERS_INSTALLED: std::sync::Once = std::sync::Once::new();

/// Install all NCE signal handlers (once).
/// Corresponds to the `std::call_once` block in upstream `ArmNce::Initialize()`.
#[cfg(all(target_os = "linux", target_arch = "aarch64"))]
fn install_nce_signal_handlers() {
    NCE_HANDLERS_INSTALLED.call_once(|| {
        unsafe {
            let mut signal_mask: libc::sigset_t = std::mem::zeroed();
            libc::sigemptyset(&mut signal_mask);
            libc::sigaddset(&mut signal_mask, RETURN_TO_RUN_CODE_SIGNAL);
            libc::sigaddset(&mut signal_mask, BREAK_FROM_RUN_CODE_SIGNAL);
            libc::sigaddset(&mut signal_mask, GUEST_ALIGNMENT_FAULT_SIGNAL);
            libc::sigaddset(&mut signal_mask, GUEST_ACCESS_FAULT_SIGNAL);

            // SIGUSR2 — return to guest code via exception level change.
            let mut sa: libc::sigaction = std::mem::zeroed();
            sa.sa_flags = libc::SA_SIGINFO | libc::SA_ONSTACK;
            sa.sa_sigaction = nce_return_to_run_code_handler as usize;
            sa.sa_mask = signal_mask;
            libc::sigaction(RETURN_TO_RUN_CODE_SIGNAL, &sa, std::ptr::null_mut());

            // SIGURG — break from running guest code.
            sa.sa_sigaction = nce_break_from_run_code_handler as usize;
            libc::sigaction(BREAK_FROM_RUN_CODE_SIGNAL, &sa, std::ptr::null_mut());

            // SIGBUS — guest alignment fault.
            sa.sa_sigaction = nce_alignment_fault_handler as usize;
            libc::sigaction(GUEST_ALIGNMENT_FAULT_SIGNAL, &sa, std::ptr::null_mut());

            // SIGSEGV — guest access fault (save old handler for chaining).
            sa.sa_flags = libc::SA_SIGINFO | libc::SA_ONSTACK | libc::SA_RESTART;
            sa.sa_sigaction = nce_access_fault_handler as usize;
            libc::sigaction(GUEST_ACCESS_FAULT_SIGNAL, &sa, std::ptr::null_mut());
        }

        log::info!("NCE: installed signal handlers (SIGUSR2, SIGURG, SIGBUS, SIGSEGV)");
    });
}

// ---------------------------------------------------------------------------
// Signal handlers — AArch64 Linux only
// ---------------------------------------------------------------------------
// These are minimal handlers that log and return. The full upstream handlers
// use assembly trampolines (ReturnToRunCodeByExceptionLevelChangeSignalHandler,
// BreakFromRunCodeSignalHandler, etc.) that manipulate ucontext_t to switch
// between host and guest register state. Those require arm_nce.s to be linked.

#[cfg(all(target_os = "linux", target_arch = "aarch64"))]
extern "C" fn nce_return_to_run_code_handler(
    _sig: libc::c_int,
    _info: *mut libc::siginfo_t,
    _ucontext: *mut libc::c_void,
) {
    // Upstream: RestoreGuestContext manipulates ucontext_t to restore all
    // guest registers and jump to guest PC. Requires arm_nce.s assembly.
}

#[cfg(all(target_os = "linux", target_arch = "aarch64"))]
extern "C" fn nce_break_from_run_code_handler(
    _sig: libc::c_int,
    _info: *mut libc::siginfo_t,
    _ucontext: *mut libc::c_void,
) {
    // Upstream: SaveGuestContext saves all guest registers from ucontext_t,
    // restores host registers, and returns to the host call site.
}

#[cfg(all(target_os = "linux", target_arch = "aarch64"))]
extern "C" fn nce_alignment_fault_handler(
    _sig: libc::c_int,
    _info: *mut libc::siginfo_t,
    _ucontext: *mut libc::c_void,
) {
    // Upstream: HandleGuestAlignmentFault interprets the faulting instruction
    // and emulates it (for unaligned accesses that the host can't handle).
}

#[cfg(all(target_os = "linux", target_arch = "aarch64"))]
extern "C" fn nce_access_fault_handler(
    sig: libc::c_int,
    info: *mut libc::siginfo_t,
    _ucontext: *mut libc::c_void,
) {
    // Upstream: HandleGuestAccessFault tries to map the faulted page via
    // InvalidateNCE. If that fails, calls HandleFailedGuestFault which
    // either skips the instruction (data abort) or returns to host
    // (prefetch abort).
    let fault_addr = unsafe { (*info).si_addr() as u64 };
    eprintln!(
        "\n=== SIGSEGV (signal {}) at address {:#018x} ===",
        sig, fault_addr
    );
    eprintln!("NCE: guest access fault — page not mapped.");

    // Re-raise with default handler for core dump.
    unsafe {
        let mut sa: libc::sigaction = std::mem::zeroed();
        sa.sa_sigaction = libc::SIG_DFL;
        libc::sigaction(libc::SIGSEGV, &sa, std::ptr::null_mut());
        libc::raise(libc::SIGSEGV);
    }
}
