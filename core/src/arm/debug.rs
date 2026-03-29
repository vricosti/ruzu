// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/debug.h and debug.cpp
//! Debug utilities (thread naming, backtrace, module enumeration).

use crate::arm::arm_interface::ThreadContext;
use crate::arm::symbols;
use crate::hle::kernel::k_memory_block::{KMemoryPermission, KMemoryState};
use crate::hardware_properties;
use std::collections::BTreeMap;

// The opaque types below serve as forward declarations matching upstream's
// `Kernel::KThread*` and `Kernel::KProcess*`. At runtime, pointers to real
// `hle::kernel::k_thread::KThread` / `hle::kernel::k_process::KProcess`
// are transmuted through these types to avoid circular dependencies.

/// Opaque type representing Kernel::KThread (forward declaration).
pub struct KThread {
    _private: (),
}

/// Opaque type representing Kernel::KProcess (forward declaration).
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
/// Upstream: `constexpr std::array<u64, 2> SegmentBases{0x60000000ULL, 0x7100000000ULL};`
const SEGMENT_BASES: [u64; 2] = [0x6000_0000, 0x71_0000_0000];

fn can_walk_frame_record<F>(fp: u64, record_size: u64, is_valid_range: F) -> bool
where
    F: FnOnce(u64, u64) -> bool,
{
    fp != 0 && (fp % 4 == 0) && is_valid_range(fp, record_size)
}

fn is_mapped_process_range(
    process: &crate::hle::kernel::k_process::KProcess,
    base: u64,
    size: u64,
) -> bool {
    if size == 0 {
        return false;
    }

    let start = match usize::try_from(base) {
        Ok(start) => start,
        Err(_) => return false,
    };

    let info = match process.page_table.query_info(start) {
        Some(info) => info,
        None => return false,
    };

    range_fits_memory_info(base, size, &info)
}

fn range_fits_memory_info(
    base: u64,
    size: u64,
    info: &crate::hle::kernel::k_memory_block::KMemoryInfo,
) -> bool {
    let start = match usize::try_from(base) {
        Ok(start) => start,
        Err(_) => return false,
    };
    let end = match start.checked_add(size as usize) {
        Some(end) => end,
        None => return false,
    };

    if start < info.get_address() || end > info.get_end_address() {
        return false;
    }

    if (info.get_state() & KMemoryState::MASK).bits() == 0 {
        return false;
    }

    let permission = info.get_permission();
    permission != KMemoryPermission::NONE && !permission.contains(KMemoryPermission::NOT_MAPPED)
}

/// Helper: transmute opaque KProcess to real KProcess.
/// SAFETY: The caller must ensure the pointer actually points to a real KProcess.
unsafe fn real_process(
    opaque: &KProcess,
) -> &crate::hle::kernel::k_process::KProcess {
    &*(opaque as *const KProcess as *const crate::hle::kernel::k_process::KProcess)
}

/// Helper: transmute opaque KThread to real KThread.
unsafe fn real_thread(
    opaque: &KThread,
) -> &crate::hle::kernel::k_thread::KThread {
    &*(opaque as *const KThread as *const crate::hle::kernel::k_thread::KThread)
}

/// Get the name of a thread from its nnsdk thread type structure.
///
/// Corresponds to upstream `Core::GetThreadName` (debug.cpp).
/// Reads from TLS to find the nnsdk thread type and extract its name.
pub fn get_thread_name(thread: &KThread) -> Option<String> {
    let real = unsafe { real_thread(thread) };
    let parent = real.parent.as_ref()?.upgrade()?;
    let process = parent.lock().unwrap();
    let is_64bit = process.is_64bit();
    let tls_addr = real.tls_address.get();
    if tls_addr == 0 {
        return None;
    }
    let memory = process.get_shared_memory();
    let mem = memory.read().unwrap();

    // Upstream: reads thread type pointer from TLS+0x1F8 (64-bit) or TLS+0x1FC (32-bit)
    // then reads version and name pointer from the thread type struct.
    if is_64bit {
        let thread_type_addr = mem.read_64(tls_addr + 0x1F8);
        if thread_type_addr == 0 {
            return None;
        }
        // nnsdk ThreadType at offset 0x1A0: version (u16), then at offset 0x188: name_pointer
        let version = mem.read_16(thread_type_addr + 0x1A0);
        if version == 0 {
            return None;
        }
        let name_pointer = mem.read_64(thread_type_addr + 0x188);
        if name_pointer == 0 {
            return None;
        }
        // Read C string from name_pointer, max 256 bytes.
        let mut name = Vec::new();
        for i in 0..256u64 {
            let b = mem.read_8(name_pointer + i);
            if b == 0 {
                break;
            }
            name.push(b);
        }
        Some(String::from_utf8_lossy(&name).to_string())
    } else {
        let thread_type_addr = mem.read_32(tls_addr + 0x1FC) as u64;
        if thread_type_addr == 0 {
            return None;
        }
        let version = mem.read_16(thread_type_addr + 0xF4);
        if version == 0 {
            return None;
        }
        let name_pointer = mem.read_32(thread_type_addr + 0xE4) as u64;
        if name_pointer == 0 {
            return None;
        }
        let mut name = Vec::new();
        for i in 0..256u64 {
            let b = mem.read_8(name_pointer + i);
            if b == 0 {
                break;
            }
            name.push(b);
        }
        Some(String::from_utf8_lossy(&name).to_string())
    }
}

/// Get the wait reason string for a thread.
/// Corresponds to upstream `Core::GetThreadWaitReason`.
pub fn get_thread_wait_reason(thread: &KThread) -> &'static str {
    use crate::hle::kernel::k_thread::ThreadWaitReasonForDebugging;
    let real = unsafe { real_thread(thread) };
    match real.wait_reason_for_debugging {
        ThreadWaitReasonForDebugging::Sleep => "Sleep",
        ThreadWaitReasonForDebugging::Ipc => "IPC",
        ThreadWaitReasonForDebugging::Synchronization => "Synchronization",
        ThreadWaitReasonForDebugging::ConditionVar => "ConditionVar",
        ThreadWaitReasonForDebugging::Arbitration => "Arbitration",
        ThreadWaitReasonForDebugging::Suspended => "Suspended",
        _ => "Unknown",
    }
}

/// Get the state string for a thread.
/// Corresponds to upstream `Core::GetThreadState`.
pub fn get_thread_state(thread: &KThread) -> String {
    use crate::hle::kernel::k_thread::ThreadState;
    let real = unsafe { real_thread(thread) };
    let state = real.get_state();
    match state {
        ThreadState::INITIALIZED => "Initialized".to_string(),
        ThreadState::WAITING => {
            format!("Waiting ({})", get_thread_wait_reason(thread))
        }
        ThreadState::RUNNABLE => "Runnable".to_string(),
        ThreadState::TERMINATED => "Terminated".to_string(),
        _ => "Unknown".to_string(),
    }
}

/// Find loaded modules in a process's address space.
/// Corresponds to upstream `Core::FindModules` (debug.cpp).
///
/// Walks the page table looking for executable Code sections, reads MOD0
/// headers to extract module path names.
pub fn find_modules(process: &KProcess) -> Modules {
    let real = unsafe { real_process(process) };
    let memory = real.get_shared_memory();
    let mem = memory.read().unwrap();
    let _is_64bit = real.is_64bit();

    // Upstream iterates memory regions via page table query_info().
    // KProcessPageTable::query_info is not fully wired yet.
    // Return empty for now — the backtrace will show raw addresses.
    // When page table query is available, this should walk Code regions
    // and extract MOD0 module names.
    drop(mem);
    Modules::new()
}

/// Get the end address of a module starting at `base`.
/// Corresponds to upstream `Core::GetModuleEnd`.
pub fn get_module_end(process: &KProcess, base: u64) -> u64 {
    let _real = unsafe { real_process(process) };
    // Upstream walks consecutive memory regions: .text (r-x) → .rodata (r--) → .data (rw-).
    // Requires page table query_info.
    base
}

/// Find the entrypoint of the main module.
/// Corresponds to upstream `Core::FindMainModuleEntrypoint`.
pub fn find_main_module_entrypoint(process: &KProcess) -> u64 {
    let modules = find_modules(process);
    if modules.len() >= 2 {
        // Second module is main (first is rtld).
        *modules.keys().nth(1).unwrap()
    } else if modules.len() == 1 {
        *modules.keys().next().unwrap()
    } else {
        // Upstream: falls back to code region start.
        0
    }
}

/// Invalidate instruction cache range across all CPU cores.
/// Corresponds to upstream `Core::InvalidateInstructionCacheRange`.
pub fn invalidate_instruction_cache_range(
    _process: &KProcess,
    _address: u64,
    _size: u64,
) {
    // Upstream: process->GetArmInterface(i)->InvalidateCacheRange(address, size)
    // for i in 0..NUM_CPU_CORES.
    // KProcess does not expose arm interfaces directly in ruzu.
    // When arm interfaces are stored per-core in KProcess, iterate and invalidate.
    for _i in 0..hardware_properties::NUM_CPU_CORES {
        // Upstream: process.get_arm_interface(i).invalidate_cache_range(address, size);
    }
}

/// Get a backtrace from a thread context.
/// Corresponds to upstream `Core::GetBacktraceFromContext` (debug.cpp).
pub fn get_backtrace_from_context(
    process: &crate::hle::kernel::k_process::KProcess,
    ctx: &ThreadContext,
) -> Vec<BacktraceEntry> {
    let is_64bit = process.is_64bit();
    let memory = process.get_shared_memory();
    let mem = memory.read().unwrap();

    let mut entries = Vec::new();
    let pc = ctx.pc;
    let mut fp = ctx.fp;
    let mut lr = ctx.lr;

    entries.push(BacktraceEntry {
        module: String::new(),
        address: pc,
        original_address: pc,
        offset: 0,
        name: String::new(),
    });

    // Walk frame pointer chain.
    // AArch64: fp+0 = prev fp, fp+8 = return address
    // AArch32: fp+0 = prev fp, fp+4 = return address
    for _ in 0..256 {
        entries.push(BacktraceEntry {
            module: String::new(),
            address: lr,
            original_address: lr,
            offset: 0,
            name: String::new(),
        });

        if is_64bit {
            if !can_walk_frame_record(fp, 16, |base, size| is_mapped_process_range(process, base, size))
            {
                break;
            }
            let new_fp = mem.read_64(fp);
            lr = mem.read_64(fp + 8);
            fp = new_fp;
        } else {
            if !can_walk_frame_record(fp, 8, |base, size| is_mapped_process_range(process, base, size))
            {
                break;
            }
            let new_fp = mem.read_32(fp) as u64;
            lr = mem.read_32(fp + 4) as u64;
            fp = new_fp;
        }
    }

    // Symbolicate.
    symbolicate_backtrace(process, &mut entries, is_64bit);
    entries
}

#[cfg(test)]
mod tests {
    use super::{can_walk_frame_record, range_fits_memory_info};
    use crate::hle::kernel::k_memory_block::{
        KMemoryAttribute, KMemoryBlockDisableMergeAttribute, KMemoryInfo, KMemoryPermission,
        KMemoryState,
    };

    #[test]
    fn can_walk_frame_record_rejects_zero_and_misaligned_pointers() {
        assert!(!can_walk_frame_record(0, 8, |_, _| true));
        assert!(!can_walk_frame_record(3, 8, |_, _| true));
        assert!(can_walk_frame_record(4, 8, |_, _| true));
    }

    #[test]
    fn can_walk_frame_record_requires_valid_range() {
        assert!(!can_walk_frame_record(0x1000, 8, |_, _| false));
        assert!(can_walk_frame_record(0x1000, 8, |base, size| base == 0x1000 && size == 8));
    }

    fn mapped_info(address: usize, size: usize, permission: KMemoryPermission) -> KMemoryInfo {
        KMemoryInfo {
            m_address: address,
            m_size: size,
            m_state: KMemoryState::NORMAL,
            m_device_disable_merge_left_count: 0,
            m_device_disable_merge_right_count: 0,
            m_ipc_lock_count: 0,
            m_device_use_count: 0,
            m_ipc_disable_merge_count: 0,
            m_permission: permission,
            m_attribute: KMemoryAttribute::NONE,
            m_original_permission: permission,
            m_disable_merge_attribute: KMemoryBlockDisableMergeAttribute::NONE,
        }
    }

    #[test]
    fn range_fits_memory_info_requires_backed_readable_region() {
        let info = mapped_info(0x2000, 0x1000, KMemoryPermission::USER_READ_WRITE);

        assert!(range_fits_memory_info(0x2000, 8, &info));
        assert!(!range_fits_memory_info(0x1FFC, 8, &info));
        assert!(!range_fits_memory_info(0x3000, 8, &info));
    }

    #[test]
    fn range_fits_memory_info_rejects_unmapped_permissions() {
        let info = mapped_info(0x2000, 0x1000, KMemoryPermission::NOT_MAPPED);

        assert!(!range_fits_memory_info(0x2000, 8, &info));
    }
}

/// Get a backtrace from a thread.
/// Corresponds to upstream `Core::GetBacktrace`.
pub fn get_backtrace(thread: &KThread) -> Vec<BacktraceEntry> {
    let real = unsafe { real_thread(thread) };
    let ctx = &real.thread_context;
    let arm_ctx = ThreadContext {
        r: ctx.r,
        fp: ctx.fp,
        lr: ctx.lr,
        sp: ctx.sp,
        pc: ctx.pc,
        pstate: ctx.pstate,
        padding: ctx.padding,
        v: ctx.v,
        fpcr: ctx.fpcr,
        fpsr: ctx.fpsr,
        tpidr: ctx.tpidr,
    };
    if let Some(parent) = real.parent.as_ref().and_then(|w| w.upgrade()) {
        let process = parent.lock().unwrap();
        get_backtrace_from_context(&process, &arm_ctx)
    } else {
        Vec::new()
    }
}

/// Symbolicate a backtrace by resolving module names and symbol names.
/// Corresponds to upstream anonymous `SymbolicateBacktrace` (debug.cpp).
fn symbolicate_backtrace(
    process: &crate::hle::kernel::k_process::KProcess,
    out: &mut Vec<BacktraceEntry>,
    is_64: bool,
) {
    let modules = find_modules_for_process(process);
    let segment_base = SEGMENT_BASES[is_64 as usize];

    for entry in out.iter_mut() {
        // Find the module containing this address (reverse iteration).
        let mut found_module = None;
        for (base, name) in modules.iter().rev() {
            if entry.original_address >= *base {
                found_module = Some((*base, name.clone()));
                break;
            }
        }

        if let Some((base, name)) = found_module {
            entry.module = name;
            entry.offset = entry.original_address - base;
            entry.address = segment_base + entry.offset;
        }

        // Symbol lookup would go here via symbols::get_symbol_name.
    }
}

fn find_modules_for_process(
    process: &crate::hle::kernel::k_process::KProcess,
) -> Modules {
    let memory = process.get_shared_memory();
    let mem = memory.read().unwrap();
    let _is_64bit = process.is_64bit();

    drop(mem);
    Modules::new()
}
