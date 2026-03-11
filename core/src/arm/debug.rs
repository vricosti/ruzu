// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/debug.h and debug.cpp
//! Debug utilities (thread naming, backtrace, module enumeration).

use crate::arm::arm_interface::ThreadContext;
use crate::hardware_properties;
use std::collections::BTreeMap;

// Forward-declared opaque types for not-yet-ported dependencies
// TODO: Replace with real types when kernel crate is wired up

/// Opaque type representing Kernel::KThread
pub struct KThread {
    _private: (),
}

/// Opaque type representing Kernel::KProcess
pub struct KProcess {
    _private: (),
}

/// Module map: base address -> module name
pub type Modules = BTreeMap<u64, String>;

/// Backtrace entry, matching upstream `Core::BacktraceEntry`.
#[derive(Debug, Clone, Default)]
pub struct BacktraceEntry {
    pub module: String,
    pub address: u64,
    pub original_address: u64,
    pub offset: u64,
    pub name: String,
}

/// Segment base addresses for 32-bit and 64-bit modes.
const SEGMENT_BASES: [u64; 2] = [0x6000_0000, 0x71_0000_0000];

/// Get the name of a thread from its nnsdk thread type structure.
///
/// Corresponds to upstream `Core::GetThreadName`.
pub fn get_thread_name(_thread: &KThread) -> Option<String> {
    // TODO: Implement when KThread and Memory are available.
    // Upstream reads from TLS to find the nnsdk thread type and extract its name.
    None
}

/// Get the wait reason string for a thread.
///
/// Corresponds to upstream `Core::GetThreadWaitReason`.
pub fn get_thread_wait_reason(_thread: &KThread) -> &'static str {
    // TODO: Implement when KThread is available.
    "Unknown"
}

/// Get the state string for a thread.
///
/// Corresponds to upstream `Core::GetThreadState`.
pub fn get_thread_state(_thread: &KThread) -> String {
    // TODO: Implement when KThread is available.
    "Unknown".to_string()
}

/// Find loaded modules in a process's address space.
///
/// Corresponds to upstream `Core::FindModules`.
pub fn find_modules(_process: &mut KProcess) -> Modules {
    // TODO: Implement when KProcess page table is available.
    // Upstream walks the page table looking for executable Code/AliasCode sections
    // and reads MOD0 headers to find module names.
    Modules::new()
}

/// Get the end address of a module starting at `base`.
///
/// Corresponds to upstream `Core::GetModuleEnd`.
pub fn get_module_end(_process: &KProcess, base: u64) -> u64 {
    // TODO: Implement when KProcess page table is available.
    // Upstream walks .text (r-x) -> .rodata (r--) -> .data (rw-) sections.
    base
}

/// Find the entrypoint of the main module.
///
/// Corresponds to upstream `Core::FindMainModuleEntrypoint`.
pub fn find_main_module_entrypoint(_process: &mut KProcess) -> u64 {
    // TODO: Implement when KProcess is available.
    // Upstream: if 2+ modules, second is main; if 1, that's main; else code region start.
    0
}

/// Invalidate instruction cache range across all CPU cores.
///
/// Corresponds to upstream `Core::InvalidateInstructionCacheRange`.
pub fn invalidate_instruction_cache_range(
    _process: &KProcess,
    _address: u64,
    _size: u64,
) {
    // TODO: Implement when KProcess arm interfaces are available.
    for _i in 0..hardware_properties::NUM_CPU_CORES {
        // process.get_arm_interface(i).invalidate_cache_range(address, size);
    }
}

/// Get a backtrace from a thread context.
///
/// Corresponds to upstream `Core::GetBacktraceFromContext`.
pub fn get_backtrace_from_context(
    _process: &mut KProcess,
    _ctx: &ThreadContext,
) -> Vec<BacktraceEntry> {
    // TODO: Implement when Memory is available.
    // Upstream dispatches to GetAArch64Backtrace or GetAArch32Backtrace based on Is64Bit.
    Vec::new()
}

/// Get a backtrace from a thread.
///
/// Corresponds to upstream `Core::GetBacktrace`.
pub fn get_backtrace(_thread: &KThread) -> Vec<BacktraceEntry> {
    // TODO: Implement when KThread context access is available.
    Vec::new()
}

/// Symbolicate a backtrace by resolving module names and symbol names.
///
/// Corresponds to upstream anonymous `SymbolicateBacktrace`.
fn symbolicate_backtrace(
    _process: &mut KProcess,
    _out: &mut Vec<BacktraceEntry>,
    _is_64: bool,
) {
    // TODO: Implement when Symbols and FindModules are fully available.
    // Upstream iterates modules to find base addresses, then looks up symbols.
}
