// SPDX-FileCopyrightText: Copyright 2014 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/arm_interface.h and arm_interface.cpp
//! ArmInterface abstract base class (register access, step, run, etc.)

use crate::hardware_properties;

use bitflags::bitflags;

// Forward-declared opaque types matching C++ forward declarations.
// These serve the same purpose as `class KThread;` / `class KProcess;`
// in upstream headers. The ArmInterface trait uses these as opaque
// pointers in run_thread/step_thread/signal_interrupt signatures.
// At runtime, real `hle::kernel::k_thread::KThread` /
// `hle::kernel::k_process::KProcess` instances are transmuted through
// these types. We cannot use the real types directly because that would
// create a circular dependency: KThread uses ThreadContext from this
// module, and this module would need KThread.

/// Opaque type representing Kernel::KThread (forward declaration).
pub struct KThread {
    _private: (),
}

/// Opaque type representing Kernel::KProcess (forward declaration).
pub struct KProcess {
    _private: (),
}

/// Opaque type representing Kernel::Svc::ThreadContext
/// Matches upstream: 29 GPRs (x0-x28 / r0-r28), fp, lr, sp, pc, pstate, padding,
/// 32 vector regs, fpcr, fpsr, tpidr
#[derive(Clone, Default)]
#[repr(C)]
pub struct ThreadContext {
    pub r: [u64; 29],
    pub fp: u64,
    pub lr: u64,
    pub sp: u64,
    pub pc: u64,
    pub pstate: u32,
    pub padding: u32,
    pub v: [u128; 32],
    pub fpcr: u32,
    pub fpsr: u32,
    pub tpidr: u64,
}

/// Debug watchpoint type, matching Kernel::DebugWatchpointType
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum DebugWatchpointType {
    None = 0,
    Read = 1,
    Write = 2,
    ReadOrWrite = 3,
}

impl std::ops::BitAnd for DebugWatchpointType {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        let val = (self as u8) & (rhs as u8);
        // Safety: result is always 0..=3
        unsafe { std::mem::transmute(val) }
    }
}

/// Debug watchpoint structure, matching Kernel::DebugWatchpoint
#[derive(Debug, Clone)]
pub struct DebugWatchpoint {
    pub start_address: u64,
    pub end_address: u64,
    pub type_: DebugWatchpointType,
}

/// Array of watchpoints, matching Core::WatchpointArray
pub type WatchpointArray = [DebugWatchpoint; hardware_properties::NUM_WATCHPOINTS as usize];

// NOTE: these values match the HaltReason enum in Dynarmic
bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct HaltReason: u64 {
        const STEP_THREAD           = 0x00000001;
        const DATA_ABORT            = 0x00000004;
        const BREAK_LOOP            = 0x02000000;
        const SUPERVISOR_CALL       = 0x04000000;
        const INSTRUCTION_BREAKPOINT = 0x08000000;
        const PREFETCH_ABORT        = 0x20000000;
    }
}

/// CPU architecture mode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Architecture {
    AArch64,
    AArch32,
}

/// Generic ARMv8 CPU interface
///
/// Corresponds to upstream `Core::ArmInterface`.
pub trait ArmInterface: Send {
    /// Perform any backend-specific initialization.
    fn initialize(&mut self) {}

    /// Runs the CPU until an event happens.
    fn run_thread(&mut self, thread: &mut KThread) -> HaltReason;

    /// Runs the CPU for one instruction or until an event happens.
    fn step_thread(&mut self, thread: &mut KThread) -> HaltReason;

    /// Admits a backend-specific mechanism to lock the thread context.
    fn lock_thread(&mut self, _thread: &mut KThread) {}

    /// Admits a backend-specific mechanism to unlock the thread context.
    fn unlock_thread(&mut self, _thread: &mut KThread) {}

    /// Clear the entire instruction cache for this CPU.
    fn clear_instruction_cache(&mut self);

    /// Clear a range of the instruction cache for this CPU.
    fn invalidate_cache_range(&mut self, addr: u64, size: usize);

    /// Get the current architecture.
    /// Returns AArch64 when PSTATE.nRW == 0 and AArch32 when PSTATE.nRW == 1.
    fn get_architecture(&self) -> Architecture;

    /// Context accessors. Should not be called if the CPU is running.
    fn get_context(&self, ctx: &mut ThreadContext);
    fn set_context(&mut self, ctx: &ThreadContext);
    fn set_tpidrro_el0(&mut self, value: u64);

    fn get_svc_arguments(&self, args: &mut [u64; 8]);
    fn set_svc_arguments(&mut self, args: &[u64; 8]);
    fn get_svc_number(&self) -> u32;
    fn get_last_exception_address(&self) -> Option<u64> {
        None
    }

    /// Signal an interrupt for execution to halt as soon as possible.
    /// It is safe to call this if the CPU is not running.
    fn signal_interrupt(&mut self, thread: &mut KThread);

    /// Debug functionality.
    fn halted_watchpoint(&self) -> Option<&DebugWatchpoint>;
    fn rewind_breakpoint_instruction(&mut self);
}

/// Base state shared by all ArmInterface implementations.
pub struct ArmInterfaceBase {
    pub watchpoints: Option<*const WatchpointArray>,
    pub uses_wall_clock: bool,
}

impl ArmInterfaceBase {
    pub fn new(uses_wall_clock: bool) -> Self {
        Self {
            watchpoints: None,
            uses_wall_clock,
        }
    }

    pub fn set_watchpoint_array(&mut self, watchpoints: *const WatchpointArray) {
        self.watchpoints = Some(watchpoints);
    }

    /// Stack trace generation.
    /// Corresponds to upstream `ArmInterface::LogBacktrace`.
    pub fn log_backtrace(&self, _process: &KProcess, ctx: &ThreadContext) {
        log::error!(
            "Backtrace, sp={:016X}, pc={:016X}",
            ctx.sp,
            ctx.pc
        );
        log::error!(
            "{:20}{:20}{:20}{:20}{}",
            "Module Name",
            "Address",
            "Original Address",
            "Offset",
            "Symbol"
        );
        log::error!("");
        // Upstream: calls debug::get_backtrace_from_context(process, ctx) and logs each.
        // Transmute the opaque KProcess to the real type to access memory/page table.
        let real_process = unsafe {
            &*(_process as *const KProcess
                as *const crate::hle::kernel::k_process::KProcess)
        };
        let entries = crate::arm::debug::get_backtrace_from_context(real_process, ctx);
        for entry in &entries {
            log::error!(
                "{:20}{:#20X}{:#20X}{:#20X}{}",
                entry.module,
                entry.address,
                entry.original_address,
                entry.offset,
                entry.name,
            );
        }
    }

    /// Matches upstream `ArmInterface::MatchingWatchpoint`.
    pub fn matching_watchpoint(
        &self,
        addr: u64,
        size: u64,
        access_type: DebugWatchpointType,
    ) -> Option<&DebugWatchpoint> {
        let watchpoints = match self.watchpoints {
            Some(ptr) => unsafe { &*ptr },
            None => return None,
        };

        let start_address = addr;
        let end_address = addr + size;

        for watch in watchpoints.iter() {
            if end_address <= watch.start_address {
                continue;
            }
            if start_address >= watch.end_address {
                continue;
            }
            if (access_type & watch.type_) == DebugWatchpointType::None {
                continue;
            }
            return Some(watch);
        }

        None
    }
}
