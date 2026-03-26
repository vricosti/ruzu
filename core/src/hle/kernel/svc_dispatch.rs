// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/kernel/svc.h and svc.cpp
//! Status: COMPLET (full dispatch wiring)
//! Derniere synchro: 2026-03-13
//!
//! This file is auto-generated in upstream (svc_generator.py).
//! It provides:
//! - SvcId enumeration for all supervisor call numbers
//! - Argument marshalling helpers
//! - 32-bit and 64-bit dispatch tables
//! - The main `call` entry point

use crate::core::System;
use crate::hle::kernel::svc::svc_activity;
use crate::hle::kernel::svc::svc_address_arbiter;
use crate::hle::kernel::svc::svc_condition_variable;
use crate::hle::kernel::svc::svc_debug_string;
use crate::hle::kernel::svc::svc_event;
use crate::hle::kernel::svc::svc_exception;
use crate::hle::kernel::svc::svc_info;
use crate::hle::kernel::svc::svc_ipc;
use crate::hle::kernel::svc::svc_lock;
use crate::hle::kernel::svc::svc_physical_memory;
use crate::hle::kernel::svc::svc_port;
use crate::hle::kernel::svc::svc_process;
use crate::hle::kernel::svc::svc_processor;
use crate::hle::kernel::svc::svc_shared_memory;
use crate::hle::kernel::svc::svc_synchronization;
use crate::hle::kernel::svc::svc_thread;
use crate::hle::kernel::svc::svc_tick;

fn drain_current_thread_termination(system: &System) {
    let current_thread = system.current_thread();
    let Some(current_thread) = current_thread else {
        return;
    };

    if {
        let thread = current_thread.lock().unwrap();
        thread.is_termination_requested() && !thread.is_signaled()
    } {
        current_thread.lock().unwrap().exit();
        system.scheduler_arc().lock().unwrap().request_schedule();
    }
}

/// SVC identifier — maps to the immediate value in the SVC instruction.
/// Corresponds to upstream `Kernel::Svc::SvcId`.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SvcId {
    SetHeapSize = 0x01,
    SetMemoryPermission = 0x02,
    SetMemoryAttribute = 0x03,
    MapMemory = 0x04,
    UnmapMemory = 0x05,
    QueryMemory = 0x06,
    ExitProcess = 0x07,
    CreateThread = 0x08,
    StartThread = 0x09,
    ExitThread = 0x0A,
    SleepThread = 0x0B,
    GetThreadPriority = 0x0C,
    SetThreadPriority = 0x0D,
    GetThreadCoreMask = 0x0E,
    SetThreadCoreMask = 0x0F,
    GetCurrentProcessorNumber = 0x10,
    SignalEvent = 0x11,
    ClearEvent = 0x12,
    MapSharedMemory = 0x13,
    UnmapSharedMemory = 0x14,
    CreateTransferMemory = 0x15,
    CloseHandle = 0x16,
    ResetSignal = 0x17,
    WaitSynchronization = 0x18,
    CancelSynchronization = 0x19,
    ArbitrateLock = 0x1A,
    ArbitrateUnlock = 0x1B,
    WaitProcessWideKeyAtomic = 0x1C,
    SignalProcessWideKey = 0x1D,
    GetSystemTick = 0x1E,
    ConnectToNamedPort = 0x1F,
    SendSyncRequestLight = 0x20,
    SendSyncRequest = 0x21,
    SendSyncRequestWithUserBuffer = 0x22,
    SendAsyncRequestWithUserBuffer = 0x23,
    GetProcessId = 0x24,
    GetThreadId = 0x25,
    Break = 0x26,
    OutputDebugString = 0x27,
    ReturnFromException = 0x28,
    GetInfo = 0x29,
    FlushEntireDataCache = 0x2A,
    FlushDataCache = 0x2B,
    MapPhysicalMemory = 0x2C,
    UnmapPhysicalMemory = 0x2D,
    GetDebugFutureThreadInfo = 0x2E,
    GetLastThreadInfo = 0x2F,
    GetResourceLimitLimitValue = 0x30,
    GetResourceLimitCurrentValue = 0x31,
    SetThreadActivity = 0x32,
    GetThreadContext3 = 0x33,
    WaitForAddress = 0x34,
    SignalToAddress = 0x35,
    SynchronizePreemptionState = 0x36,
    GetResourceLimitPeakValue = 0x37,
    CreateIoPool = 0x39,
    CreateIoRegion = 0x3A,
    KernelDebug = 0x3C,
    ChangeKernelTraceState = 0x3D,
    CreateSession = 0x40,
    AcceptSession = 0x41,
    ReplyAndReceiveLight = 0x42,
    ReplyAndReceive = 0x43,
    ReplyAndReceiveWithUserBuffer = 0x44,
    CreateEvent = 0x45,
    MapIoRegion = 0x46,
    UnmapIoRegion = 0x47,
    MapPhysicalMemoryUnsafe = 0x48,
    UnmapPhysicalMemoryUnsafe = 0x49,
    SetUnsafeLimit = 0x4A,
    CreateCodeMemory = 0x4B,
    ControlCodeMemory = 0x4C,
    SleepSystem = 0x4D,
    ReadWriteRegister = 0x4E,
    SetProcessActivity = 0x4F,
    CreateSharedMemory = 0x50,
    MapTransferMemory = 0x51,
    UnmapTransferMemory = 0x52,
    CreateInterruptEvent = 0x53,
    QueryPhysicalAddress = 0x54,
    QueryIoMapping = 0x55,
    CreateDeviceAddressSpace = 0x56,
    AttachDeviceAddressSpace = 0x57,
    DetachDeviceAddressSpace = 0x58,
    MapDeviceAddressSpaceByForce = 0x59,
    MapDeviceAddressSpaceAligned = 0x5A,
    UnmapDeviceAddressSpace = 0x5C,
    InvalidateProcessDataCache = 0x5D,
    StoreProcessDataCache = 0x5E,
    FlushProcessDataCache = 0x5F,
    DebugActiveProcess = 0x60,
    BreakDebugProcess = 0x61,
    TerminateDebugProcess = 0x62,
    GetDebugEvent = 0x63,
    ContinueDebugEvent = 0x64,
    GetProcessList = 0x65,
    GetThreadList = 0x66,
    GetDebugThreadContext = 0x67,
    SetDebugThreadContext = 0x68,
    QueryDebugProcessMemory = 0x69,
    ReadDebugProcessMemory = 0x6A,
    WriteDebugProcessMemory = 0x6B,
    SetHardwareBreakPoint = 0x6C,
    GetDebugThreadParam = 0x6D,
    GetSystemInfo = 0x6F,
    CreatePort = 0x70,
    ManageNamedPort = 0x71,
    ConnectToPort = 0x72,
    SetProcessMemoryPermission = 0x73,
    MapProcessMemory = 0x74,
    UnmapProcessMemory = 0x75,
    QueryProcessMemory = 0x76,
    MapProcessCodeMemory = 0x77,
    UnmapProcessCodeMemory = 0x78,
    CreateProcess = 0x79,
    StartProcess = 0x7A,
    TerminateProcess = 0x7B,
    GetProcessInfo = 0x7C,
    CreateResourceLimit = 0x7D,
    SetResourceLimitLimitValue = 0x7E,
    CallSecureMonitor = 0x7F,
    MapInsecureMemory = 0x90,
    UnmapInsecureMemory = 0x91,
}

impl SvcId {
    /// Try to convert a raw u32 immediate to an SvcId.
    pub fn from_u32(imm: u32) -> Option<Self> {
        match imm {
            0x01 => Some(Self::SetHeapSize),
            0x02 => Some(Self::SetMemoryPermission),
            0x03 => Some(Self::SetMemoryAttribute),
            0x04 => Some(Self::MapMemory),
            0x05 => Some(Self::UnmapMemory),
            0x06 => Some(Self::QueryMemory),
            0x07 => Some(Self::ExitProcess),
            0x08 => Some(Self::CreateThread),
            0x09 => Some(Self::StartThread),
            0x0A => Some(Self::ExitThread),
            0x0B => Some(Self::SleepThread),
            0x0C => Some(Self::GetThreadPriority),
            0x0D => Some(Self::SetThreadPriority),
            0x0E => Some(Self::GetThreadCoreMask),
            0x0F => Some(Self::SetThreadCoreMask),
            0x10 => Some(Self::GetCurrentProcessorNumber),
            0x11 => Some(Self::SignalEvent),
            0x12 => Some(Self::ClearEvent),
            0x13 => Some(Self::MapSharedMemory),
            0x14 => Some(Self::UnmapSharedMemory),
            0x15 => Some(Self::CreateTransferMemory),
            0x16 => Some(Self::CloseHandle),
            0x17 => Some(Self::ResetSignal),
            0x18 => Some(Self::WaitSynchronization),
            0x19 => Some(Self::CancelSynchronization),
            0x1A => Some(Self::ArbitrateLock),
            0x1B => Some(Self::ArbitrateUnlock),
            0x1C => Some(Self::WaitProcessWideKeyAtomic),
            0x1D => Some(Self::SignalProcessWideKey),
            0x1E => Some(Self::GetSystemTick),
            0x1F => Some(Self::ConnectToNamedPort),
            0x20 => Some(Self::SendSyncRequestLight),
            0x21 => Some(Self::SendSyncRequest),
            0x22 => Some(Self::SendSyncRequestWithUserBuffer),
            0x23 => Some(Self::SendAsyncRequestWithUserBuffer),
            0x24 => Some(Self::GetProcessId),
            0x25 => Some(Self::GetThreadId),
            0x26 => Some(Self::Break),
            0x27 => Some(Self::OutputDebugString),
            0x28 => Some(Self::ReturnFromException),
            0x29 => Some(Self::GetInfo),
            0x2A => Some(Self::FlushEntireDataCache),
            0x2B => Some(Self::FlushDataCache),
            0x2C => Some(Self::MapPhysicalMemory),
            0x2D => Some(Self::UnmapPhysicalMemory),
            0x2E => Some(Self::GetDebugFutureThreadInfo),
            0x2F => Some(Self::GetLastThreadInfo),
            0x30 => Some(Self::GetResourceLimitLimitValue),
            0x31 => Some(Self::GetResourceLimitCurrentValue),
            0x32 => Some(Self::SetThreadActivity),
            0x33 => Some(Self::GetThreadContext3),
            0x34 => Some(Self::WaitForAddress),
            0x35 => Some(Self::SignalToAddress),
            0x36 => Some(Self::SynchronizePreemptionState),
            0x37 => Some(Self::GetResourceLimitPeakValue),
            0x39 => Some(Self::CreateIoPool),
            0x3A => Some(Self::CreateIoRegion),
            0x3C => Some(Self::KernelDebug),
            0x3D => Some(Self::ChangeKernelTraceState),
            0x40 => Some(Self::CreateSession),
            0x41 => Some(Self::AcceptSession),
            0x42 => Some(Self::ReplyAndReceiveLight),
            0x43 => Some(Self::ReplyAndReceive),
            0x44 => Some(Self::ReplyAndReceiveWithUserBuffer),
            0x45 => Some(Self::CreateEvent),
            0x46 => Some(Self::MapIoRegion),
            0x47 => Some(Self::UnmapIoRegion),
            0x48 => Some(Self::MapPhysicalMemoryUnsafe),
            0x49 => Some(Self::UnmapPhysicalMemoryUnsafe),
            0x4A => Some(Self::SetUnsafeLimit),
            0x4B => Some(Self::CreateCodeMemory),
            0x4C => Some(Self::ControlCodeMemory),
            0x4D => Some(Self::SleepSystem),
            0x4E => Some(Self::ReadWriteRegister),
            0x4F => Some(Self::SetProcessActivity),
            0x50 => Some(Self::CreateSharedMemory),
            0x51 => Some(Self::MapTransferMemory),
            0x52 => Some(Self::UnmapTransferMemory),
            0x53 => Some(Self::CreateInterruptEvent),
            0x54 => Some(Self::QueryPhysicalAddress),
            0x55 => Some(Self::QueryIoMapping),
            0x56 => Some(Self::CreateDeviceAddressSpace),
            0x57 => Some(Self::AttachDeviceAddressSpace),
            0x58 => Some(Self::DetachDeviceAddressSpace),
            0x59 => Some(Self::MapDeviceAddressSpaceByForce),
            0x5A => Some(Self::MapDeviceAddressSpaceAligned),
            0x5C => Some(Self::UnmapDeviceAddressSpace),
            0x5D => Some(Self::InvalidateProcessDataCache),
            0x5E => Some(Self::StoreProcessDataCache),
            0x5F => Some(Self::FlushProcessDataCache),
            0x60 => Some(Self::DebugActiveProcess),
            0x61 => Some(Self::BreakDebugProcess),
            0x62 => Some(Self::TerminateDebugProcess),
            0x63 => Some(Self::GetDebugEvent),
            0x64 => Some(Self::ContinueDebugEvent),
            0x65 => Some(Self::GetProcessList),
            0x66 => Some(Self::GetThreadList),
            0x67 => Some(Self::GetDebugThreadContext),
            0x68 => Some(Self::SetDebugThreadContext),
            0x69 => Some(Self::QueryDebugProcessMemory),
            0x6A => Some(Self::ReadDebugProcessMemory),
            0x6B => Some(Self::WriteDebugProcessMemory),
            0x6C => Some(Self::SetHardwareBreakPoint),
            0x6D => Some(Self::GetDebugThreadParam),
            0x6F => Some(Self::GetSystemInfo),
            0x70 => Some(Self::CreatePort),
            0x71 => Some(Self::ManageNamedPort),
            0x72 => Some(Self::ConnectToPort),
            0x73 => Some(Self::SetProcessMemoryPermission),
            0x74 => Some(Self::MapProcessMemory),
            0x75 => Some(Self::UnmapProcessMemory),
            0x76 => Some(Self::QueryProcessMemory),
            0x77 => Some(Self::MapProcessCodeMemory),
            0x78 => Some(Self::UnmapProcessCodeMemory),
            0x79 => Some(Self::CreateProcess),
            0x7A => Some(Self::StartProcess),
            0x7B => Some(Self::TerminateProcess),
            0x7C => Some(Self::GetProcessInfo),
            0x7D => Some(Self::CreateResourceLimit),
            0x7E => Some(Self::SetResourceLimitLimitValue),
            0x7F => Some(Self::CallSecureMonitor),
            0x90 => Some(Self::MapInsecureMemory),
            0x91 => Some(Self::UnmapInsecureMemory),
            _ => None,
        }
    }
}

// =============================================================================
// Argument marshalling helpers
// =============================================================================

/// SVC argument register file: 8 x u64 registers (r0-r7 / x0-x7).
pub type SvcArgs = [u64; 8];

/// Extract a u32 argument from the register file.
#[inline]
fn get_arg32(args: &SvcArgs, n: usize) -> u32 {
    args[n] as u32
}

/// Store a u32 result into the register file.
#[inline]
fn set_arg32(args: &mut SvcArgs, n: usize, value: u32) {
    args[n] = value as u64;
}

/// Extract a u64 argument from the register file.
#[inline]
fn get_arg64(args: &SvcArgs, n: usize) -> u64 {
    args[n]
}

/// Store a u64 result into the register file.
#[inline]
fn set_arg64(args: &mut SvcArgs, n: usize, value: u64) {
    args[n] = value;
}

/// Gather a u64 from two u32 registers (ARM32 ABI).
/// lo register holds the low 32 bits, hi register holds the high 32 bits.
#[inline]
fn gather64(args: &SvcArgs, lo: usize, hi: usize) -> u64 {
    (args[lo] as u32 as u64) | ((args[hi] as u32 as u64) << 32)
}

/// Scatter a u64 into two u32 registers (ARM32 ABI).
#[inline]
fn scatter64(args: &mut SvcArgs, lo: usize, hi: usize, val: u64) {
    args[lo] = val as u32 as u64;
    args[hi] = (val >> 32) as u32 as u64;
}

// =============================================================================
// Stub result for unimplemented SVCs that should return success
// =============================================================================

/// Return success (0) for stubbed SVCs. Matches upstream behavior where the
/// handler would succeed but we haven't implemented the kernel object access.
const STUB_SUCCESS: u32 = 0;

/// Allocate a stub handle registered in the process handle table.
///
/// Upstream SVCs like CreateTransferMemory create a real kernel object
/// (e.g. KTransferMemory) and add it to the handle table via
/// `handle_table.Add(out, trmem)`. When the game later calls CloseHandle,
/// it must find the handle in the table or it gets ResultInvalidHandle.
///
/// For stubbed SVCs where we don't create the real kernel object yet,
/// we still must register a handle in the process table so that
/// CloseHandle succeeds. We use the kernel's object ID allocator to
/// get a unique ID as the opaque "object".
fn alloc_stub_handle(system: &System) -> u32 {
    let object_id = system
        .kernel()
        .map(|k| k.create_new_object_id())
        .unwrap_or(1) as u64;
    system
        .current_process_arc()
        .lock()
        .unwrap()
        .handle_table
        .add(object_id)
        .unwrap_or(0)
}

// =============================================================================
// QueryMemory helper
// =============================================================================

/// Resolve memory info for a given address using the page table's KMemoryBlockManager.
/// Returns (base, size, svc_state, svc_permission).
///
/// Queries the block manager in KPageTableBase, matching upstream's
/// KPageTableBase::QueryInfoImpl() behavior.
fn query_memory_info(system: &System, query_addr: u64) -> (u64, u64, u32, u32) {
    use crate::hle::kernel::k_memory_block::KMemoryPermission;

    let process = system.current_process_arc().lock().unwrap();

    if let Some(info) = process.page_table.query_info(query_addr as usize) {
        let svc_state = info.get_svc_state();
        // Convert KMemoryPermission to SVC permission (lower 3 bits = user R/W/X).
        let svc_perm = (info.get_permission() & KMemoryPermission::USER_MASK).bits() as u32;
        (
            info.get_address() as u64,
            info.get_size() as u64,
            svc_state,
            svc_perm,
        )
    } else {
        // Address outside managed range — return Inaccessible.
        // Upstream: creates fake block from address_space_end to max.
        let addr_space_end = process.page_table.get_address_space_start().get()
            + process.page_table.get_address_space_size() as u64;
        (
            addr_space_end,
            u64::MAX - addr_space_end + 1,
            0x10, // Inaccessible
            0,    // None
        )
    }
}

// =============================================================================
// 32-bit dispatch (AArch32 / ILP32)
// =============================================================================

/// Dispatch a 32-bit (AArch32 / ILP32) SVC.
///
/// Corresponds to upstream `Call32`. Register layouts match the auto-generated
/// SvcWrap_*64From32 wrappers in svc.cpp.
fn call32(system: &System, imm: u32, args: &mut SvcArgs) {
    match SvcId::from_u32(imm) {
        // =====================================================================
        // Memory management
        // =====================================================================
        Some(SvcId::SetHeapSize) => {
            // IN: size=arg32[1]; OUT: ret=arg32[0], out_address=arg32[1]
            let size = get_arg32(args, 1) as u64;
            let mut heap_base = 0;
            let result = svc_physical_memory::set_heap_size_current_process(system, &mut heap_base, size);
            log::info!("  SetHeapSize({:#x}) -> heap at {:#x}", size, heap_base);
            set_arg32(args, 0, result.get_inner_value());
            set_arg32(args, 1, heap_base as u32);
        }
        Some(SvcId::SetMemoryPermission) => {
            // IN: address=arg32[0], size=arg32[1], perm=arg32[2]; OUT: ret=arg32[0]
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::SetMemoryAttribute) => {
            // IN: address=arg32[0], size=arg32[1], mask=arg32[2], attr=arg32[3]; OUT: ret=arg32[0]
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::MapMemory) => {
            // IN: dst=arg32[0], src=arg32[1], size=arg32[2]; OUT: ret=arg32[0]
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::UnmapMemory) => {
            // IN: dst=arg32[0], src=arg32[1], size=arg32[2]; OUT: ret=arg32[0]
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::QueryMemory) => {
            // IN: out_memory_info=arg32[0], address=arg32[2]; OUT: ret=arg32[0], page_info=arg32[1]
            let mem_info_ptr = get_arg32(args, 0) as u64;
            let query_addr = get_arg32(args, 2) as u64;
            let (base, size, state, perm) = query_memory_info(system, query_addr);
            // Write MemoryInfo structure to guest memory.
            // Upstream: current_memory.WriteBlock(out_memory_info, &svc_mem_info, sizeof(...))
            {
                if let Some(memory) = system.get_svc_memory() {
                    let m = memory.lock().unwrap();
                    m.write_64(mem_info_ptr, base);
                    m.write_64(mem_info_ptr + 8, size);
                    m.write_32(mem_info_ptr + 16, state);
                    m.write_32(mem_info_ptr + 20, 0); // attribute
                    m.write_32(mem_info_ptr + 24, perm);
                    m.write_32(mem_info_ptr + 28, 0); // ipc_count
                    m.write_32(mem_info_ptr + 32, 0); // device_count
                    m.write_32(mem_info_ptr + 36, 0); // padding
                } else {
                    let mut mem = system.shared_process_memory().write().unwrap();
                    if mem.is_valid_range(mem_info_ptr, 40) {
                        mem.write_64(mem_info_ptr, base);
                        mem.write_64(mem_info_ptr + 8, size);
                        mem.write_32(mem_info_ptr + 16, state);
                        mem.write_32(mem_info_ptr + 20, 0);
                        mem.write_32(mem_info_ptr + 24, perm);
                        mem.write_32(mem_info_ptr + 28, 0);
                        mem.write_32(mem_info_ptr + 32, 0);
                        mem.write_32(mem_info_ptr + 36, 0);
                    }
                }
            }
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, 0); // page_info
        }
        Some(SvcId::MapPhysicalMemory) => {
            // IN: address=arg32[0], size=arg32[1]; OUT: ret=arg32[0]
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::UnmapPhysicalMemory) => {
            // IN: address=arg32[0], size=arg32[1]; OUT: ret=arg32[0]
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::MapPhysicalMemoryUnsafe) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::UnmapPhysicalMemoryUnsafe) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::SetUnsafeLimit) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::MapInsecureMemory) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::UnmapInsecureMemory) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }

        // =====================================================================
        // Process management
        // =====================================================================
        Some(SvcId::ExitProcess) => {
            // IN: (none); OUT: (none)
            svc_exception::break_execution(system, 0, 0, 0);
            log::info!("  ExitProcess called");
        }
        Some(SvcId::GetProcessId) => {
            // IN: process_handle=arg32[1]; OUT: ret=arg32[0], pid=scatter64[1,2]
            set_arg32(args, 0, STUB_SUCCESS);
            scatter64(args, 1, 2, 1); // pid = 1
        }
        Some(SvcId::GetProcessList) => {
            // IN: out_ids=arg32[1], max=arg32[2]; OUT: ret=arg32[0], count=arg32[1]
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, 0);
        }
        Some(SvcId::GetProcessInfo) => {
            // IN: handle=arg32[1], type=arg32[2]; OUT: ret=arg32[0], out=scatter64[1,2]
            set_arg32(args, 0, STUB_SUCCESS);
            scatter64(args, 1, 2, 0);
        }
        Some(SvcId::CreateProcess) => {
            // IN: params=arg32[1], caps=arg32[2], num_caps=arg32[3]; OUT: ret=arg32[0], handle=arg32[1]
            log::warn!("  CreateProcess: stub");
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, alloc_stub_handle(system));
        }
        Some(SvcId::StartProcess) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::TerminateProcess) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::SetProcessActivity) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }

        // =====================================================================
        // Thread management
        // =====================================================================
        Some(SvcId::CreateThread) => {
            // IN: func=arg32[1], arg=arg32[2], stack_bottom=arg32[3], priority=arg32[0], core_id=arg32[4]
            // OUT: ret=arg32[0], out_handle=arg32[1]
            let priority = get_arg32(args, 0) as i32;
            let entry_point = get_arg32(args, 1) as u64;
            let arg = get_arg32(args, 2) as u64;
            let stack_bottom = get_arg32(args, 3) as u64;
            let core_id = get_arg32(args, 4) as i32;
            let mut out_handle = 0;
            let result = svc_thread::create_thread(
                system,
                &mut out_handle,
                entry_point,
                arg,
                stack_bottom,
                priority,
                core_id,
            );
            set_arg32(args, 0, result.get_inner_value());
            set_arg32(args, 1, out_handle);
        }
        Some(SvcId::StartThread) => {
            // IN: handle=arg32[0]; OUT: ret=arg32[0]
            let handle = get_arg32(args, 0);
            let result = svc_thread::start_thread(system, handle);
            set_arg32(args, 0, result.get_inner_value());
        }
        Some(SvcId::ExitThread) => {
            svc_thread::exit_thread(system);
        }
        Some(SvcId::SleepThread) => {
            // IN: ns=gather64[0,1]; OUT: (none)
            let ns = gather64(args, 0, 1) as i64;
            svc_thread::sleep_thread(system, ns);
        }
        Some(SvcId::GetThreadPriority) => {
            // IN: handle=arg32[1]; OUT: ret=arg32[0], priority=arg32[1]
            let handle = get_arg32(args, 1);
            let mut out_priority = 0;
            let result = svc_thread::get_thread_priority(system, &mut out_priority, handle);
            set_arg32(args, 0, result.get_inner_value());
            set_arg32(args, 1, out_priority as u32);
        }
        Some(SvcId::SetThreadPriority) => {
            // IN: handle=arg32[0], priority=arg32[1]; OUT: ret=arg32[0]
            let handle = get_arg32(args, 0);
            let priority = get_arg32(args, 1) as i32;
            let result = svc_thread::set_thread_priority(system, handle, priority);
            log::info!("  SetThreadPriority(handle={:#x}, priority={}) -> result={:#x}",
                handle, priority, result.get_inner_value());
            if result.is_error() {
                let process = system.current_process_arc().lock().unwrap();
                let prio_mask = process.get_priority_mask();
                let prio_check = process.check_thread_priority(priority);
                log::error!("  priority_mask={:#018x}, check_thread_priority({})={}",
                    prio_mask, priority, prio_check);
                log::error!("  Handle table dump: count={}, table_size={}",
                    process.handle_table.count, process.handle_table.table_size);
                for i in 0..std::cmp::min(process.handle_table.count as usize + 2, 10) {
                    let obj = process.handle_table.objects[i];
                    let li = unsafe { process.handle_table.entry_infos[i].linear_id };
                    if obj != 0 {
                        let h = crate::hle::kernel::k_handle_table::encode_handle(i as u16, li);
                        log::error!("    [{}] object_id={} handle={:#x}", i, obj, h);
                    }
                }
                // Check the specific index
                let (idx, lid, _) = crate::hle::kernel::k_handle_table::decode_handle(handle);
                log::error!("  Decoded handle: index={}, linear_id={}", idx, lid);
                log::error!("  At index {}: object_id={}, entry linear_id={}",
                    idx, process.handle_table.objects[idx as usize],
                    unsafe { process.handle_table.entry_infos[idx as usize].linear_id });
            }
            set_arg32(args, 0, result.get_inner_value());
        }
        Some(SvcId::GetThreadCoreMask) => {
            // IN: handle=arg32[2]; OUT: ret=arg32[0], core_id=arg32[1]
            let handle = get_arg32(args, 2);
            let mut out_core_id = 0;
            let mut out_affinity_mask = 0;
            let result = svc_thread::get_thread_core_mask(
                system,
                &mut out_core_id,
                &mut out_affinity_mask,
                handle,
            );
            set_arg32(args, 0, result.get_inner_value());
            set_arg32(args, 1, out_core_id as u32);
            scatter64(args, 2, 3, out_affinity_mask);
        }
        Some(SvcId::SetThreadCoreMask) => {
            // IN: handle=arg32[0], core_id=arg32[1], affinity=gather64[2,3]; OUT: ret=arg32[0]
            let handle = get_arg32(args, 0);
            let core_id = get_arg32(args, 1) as i32;
            let affinity_mask = gather64(args, 2, 3);
            let result = svc_thread::set_thread_core_mask(system, handle, core_id, affinity_mask);
            set_arg32(args, 0, result.get_inner_value());
        }
        Some(SvcId::GetCurrentProcessorNumber) => {
            // IN: (none); OUT: ret=arg32[0]
            set_arg32(args, 0, svc_processor::get_current_processor_number(system) as u32);
        }
        Some(SvcId::GetThreadId) => {
            // IN: handle=arg32[1]; OUT: ret=arg32[0], tid=scatter64[1,2]
            let handle = get_arg32(args, 1);
            let mut out_thread_id = 0;
            let result = svc_thread::get_thread_id(system, &mut out_thread_id, handle);
            set_arg32(args, 0, result.get_inner_value());
            scatter64(args, 1, 2, out_thread_id);
        }
        Some(SvcId::SetThreadActivity) => {
            let handle = get_arg32(args, 0);
            let result = match get_arg32(args, 1) {
                0 => svc_activity::set_thread_activity(
                    system,
                    handle,
                    crate::hle::kernel::svc::svc_types::ThreadActivity::Runnable,
                ),
                1 => svc_activity::set_thread_activity(
                    system,
                    handle,
                    crate::hle::kernel::svc::svc_types::ThreadActivity::Paused,
                ),
                _ => crate::hle::kernel::svc::svc_results::RESULT_INVALID_ENUM_VALUE,
            };
            set_arg32(args, 0, result.get_inner_value());
        }
        Some(SvcId::GetThreadContext3) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::GetThreadList) => {
            // IN: out=arg32[1], max=arg32[2], debug=arg32[3]; OUT: ret=arg32[0], count=arg32[1]
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, 0);
        }

        // =====================================================================
        // Synchronization
        // =====================================================================
        Some(SvcId::SignalEvent) => {
            let result = svc_event::signal_event(system, get_arg32(args, 0));
            set_arg32(args, 0, result.get_inner_value());
        }
        Some(SvcId::ClearEvent) => {
            let result = svc_event::clear_event(system, get_arg32(args, 0));
            set_arg32(args, 0, result.get_inner_value());
        }
        Some(SvcId::CloseHandle) => {
            // IN: handle=arg32[0]; OUT: ret=arg32[0]
            let result = svc_synchronization::close_handle(system, get_arg32(args, 0));
            set_arg32(args, 0, result.get_inner_value());
        }
        Some(SvcId::ResetSignal) => {
            let result = svc_synchronization::reset_signal(system, get_arg32(args, 0));
            set_arg32(args, 0, result.get_inner_value());
        }
        Some(SvcId::WaitSynchronization) => {
            // IN: handles=arg32[1], num=arg32[2], timeout=gather64[0,3]; OUT: ret=arg32[0], index=arg32[1]
            let mut out_index = -1;
            let result = svc_synchronization::wait_synchronization(
                system,
                &mut out_index,
                get_arg32(args, 1) as u64,
                get_arg32(args, 2) as i32,
                gather64(args, 0, 3) as i64,
            );
            set_arg32(args, 0, result.get_inner_value());
            set_arg32(args, 1, out_index as u32);
        }
        Some(SvcId::CancelSynchronization) => {
            let result = svc_synchronization::cancel_synchronization(system, get_arg32(args, 0));
            set_arg32(args, 0, result.get_inner_value());
        }
        Some(SvcId::ArbitrateLock) => {
            let result = svc_lock::arbitrate_lock(
                system,
                get_arg32(args, 0),
                get_arg32(args, 1) as u64,
                get_arg32(args, 2),
            );
            set_arg32(args, 0, result.get_inner_value());
        }
        Some(SvcId::ArbitrateUnlock) => {
            let result = svc_lock::arbitrate_unlock(system, get_arg32(args, 0) as u64);
            set_arg32(args, 0, result.get_inner_value());
        }
        Some(SvcId::WaitProcessWideKeyAtomic) => {
            let result = svc_condition_variable::wait_process_wide_key_atomic(
                system,
                get_arg32(args, 0) as u64,
                get_arg32(args, 1) as u64,
                get_arg32(args, 2),
                gather64(args, 3, 4) as i64,
            );
            set_arg32(args, 0, result.get_inner_value());
        }
        Some(SvcId::SignalProcessWideKey) => {
            svc_condition_variable::signal_process_wide_key(
                system,
                get_arg32(args, 0) as u64,
                get_arg32(args, 1) as i32,
            );
        }
        Some(SvcId::WaitForAddress) => {
            let result = svc_address_arbiter::wait_for_address(
                system,
                get_arg32(args, 0) as u64,
                unsafe { std::mem::transmute(get_arg32(args, 1)) },
                get_arg32(args, 2) as i32,
                gather64(args, 3, 4) as i64,
            );
            set_arg32(args, 0, result.get_inner_value());
        }
        Some(SvcId::SignalToAddress) => {
            let result = svc_address_arbiter::signal_to_address(
                system,
                get_arg32(args, 0) as u64,
                unsafe { std::mem::transmute(get_arg32(args, 1)) },
                get_arg32(args, 2) as i32,
                get_arg32(args, 3) as i32,
            );
            set_arg32(args, 0, result.get_inner_value());
        }
        Some(SvcId::SynchronizePreemptionState) => {
            // No return value
        }
        Some(SvcId::CreateEvent) => {
            // OUT: ret=arg32[0], write_handle=arg32[1], read_handle=arg32[2]
            let mut write_handle = 0;
            let mut read_handle = 0;
            let result = svc_event::create_event(system, &mut write_handle, &mut read_handle);
            set_arg32(args, 0, result.get_inner_value());
            set_arg32(args, 1, write_handle);
            set_arg32(args, 2, read_handle);
        }

        // =====================================================================
        // Timing
        // =====================================================================
        Some(SvcId::GetSystemTick) => {
            // IN: (none); OUT: tick=scatter64[0,1]
            let tick = svc_tick::get_system_tick(system);
            scatter64(args, 0, 1, tick as u64);
        }

        // =====================================================================
        // IPC
        // =====================================================================
        Some(SvcId::ConnectToNamedPort) => {
            let mut out = 0;
            let result = svc_port::connect_to_named_port(system, &mut out, get_arg32(args, 1) as u64);
            set_arg32(args, 0, result.get_inner_value());
            set_arg32(args, 1, out);
        }
        Some(SvcId::SendSyncRequestLight) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::SendSyncRequest) => {
            let session_handle = get_arg32(args, 0);
            let log_handle = std::env::var("RUZU_LOG_SVC_SYNC_HANDLE")
                .ok()
                .and_then(|value| {
                    let trimmed = value.trim_start_matches("0x").trim_start_matches("0X");
                    u32::from_str_radix(trimmed, 16)
                        .ok()
                        .or_else(|| value.parse::<u32>().ok())
                });
            if log_handle.is_some_and(|target| target == session_handle) {
                log::info!(
                    "svc::SendSyncRequest pre handle={:#x} r0={:#010x} r1={:#010x} r2={:#010x} r3={:#010x} r4={:#010x} r5={:#010x} r6={:#010x} r7={:#010x}",
                    session_handle,
                    get_arg32(args, 0),
                    get_arg32(args, 1),
                    get_arg32(args, 2),
                    get_arg32(args, 3),
                    get_arg32(args, 4),
                    get_arg32(args, 5),
                    get_arg32(args, 6),
                    get_arg32(args, 7),
                );
            }
            let result = svc_ipc::send_sync_request(system, session_handle);
            set_arg32(args, 0, result.get_inner_value());
            if log_handle.is_some_and(|target| target == session_handle) {
                log::info!(
                    "svc::SendSyncRequest post handle={:#x} r0={:#010x} r1={:#010x} r2={:#010x} r3={:#010x} r4={:#010x} r5={:#010x} r6={:#010x} r7={:#010x}",
                    session_handle,
                    get_arg32(args, 0),
                    get_arg32(args, 1),
                    get_arg32(args, 2),
                    get_arg32(args, 3),
                    get_arg32(args, 4),
                    get_arg32(args, 5),
                    get_arg32(args, 6),
                    get_arg32(args, 7),
                );
            }
        }
        Some(SvcId::SendSyncRequestWithUserBuffer) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::SendAsyncRequestWithUserBuffer) => {
            // OUT: ret=arg32[0], event_handle=arg32[1]
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, alloc_stub_handle(system));
        }
        Some(SvcId::ReplyAndReceiveLight) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::ReplyAndReceive) => {
            // OUT: ret=arg32[0], index=arg32[1]
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, 0);
        }
        Some(SvcId::ReplyAndReceiveWithUserBuffer) => {
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, 0);
        }
        Some(SvcId::CreateSession) => {
            // OUT: ret=arg32[0], server=arg32[1], client=arg32[2]
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, alloc_stub_handle(system));
            set_arg32(args, 2, alloc_stub_handle(system));
        }
        Some(SvcId::AcceptSession) => {
            // OUT: ret=arg32[0], handle=arg32[1]
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, alloc_stub_handle(system));
        }
        Some(SvcId::CreatePort) => {
            // OUT: ret=arg32[0], server=arg32[1], client=arg32[2]
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, alloc_stub_handle(system));
            set_arg32(args, 2, alloc_stub_handle(system));
        }
        Some(SvcId::ManageNamedPort) => {
            // OUT: ret=arg32[0], server=arg32[1]
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, alloc_stub_handle(system));
        }
        Some(SvcId::ConnectToPort) => {
            // OUT: ret=arg32[0], handle=arg32[1]
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, alloc_stub_handle(system));
        }

        // =====================================================================
        // Shared / Transfer memory
        // =====================================================================
        Some(SvcId::MapSharedMemory) => {
            // IN: handle=arg32[0], address=arg32[1], size=arg32[2], perm=arg32[3]; OUT: ret=arg32[0]
            let handle = get_arg32(args, 0);
            let address = get_arg32(args, 1) as u64;
            let size = get_arg32(args, 2) as u64;
            let perm = get_arg32(args, 3);
            let map_perm = unsafe { std::mem::transmute::<u32, crate::hle::kernel::svc::svc_types::MemoryPermission>(perm) };
            let result = svc_shared_memory::map_shared_memory(system, handle, address, size, map_perm);
            set_arg32(args, 0, result.get_inner_value());
        }
        Some(SvcId::UnmapSharedMemory) => {
            // IN: handle=arg32[0], address=arg32[1], size=arg32[2]; OUT: ret=arg32[0]
            let handle = get_arg32(args, 0);
            let address = get_arg32(args, 1) as u64;
            let size = get_arg32(args, 2) as u64;
            let result = svc_shared_memory::unmap_shared_memory(system, handle, address, size);
            set_arg32(args, 0, result.get_inner_value());
        }
        Some(SvcId::CreateTransferMemory) => {
            // OUT: ret=arg32[0], handle=arg32[1]
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, alloc_stub_handle(system));
        }
        Some(SvcId::CreateSharedMemory) => {
            // OUT: ret=arg32[0], handle=arg32[1]
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, alloc_stub_handle(system));
        }
        Some(SvcId::MapTransferMemory) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::UnmapTransferMemory) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }

        // =====================================================================
        // Debug / Exception
        // =====================================================================
        Some(SvcId::Break) => {
            // IN: reason=arg32[0], arg=arg32[1], size=arg32[2]; OUT: (none)
            let reason = get_arg32(args, 0);
            let info1 = get_arg32(args, 1) as u64;
            let info2 = get_arg32(args, 2) as u64;

            // Dump guest registers for diagnosis.
            log::error!("=== GUEST REGISTER DUMP AT BREAK ===");
            for i in 0..args.len() {
                log::error!("  r{:2} = {:#010x}", i, args[i]);
            }
            // Read abort message from guest memory if info1 points to a string.
            if info1 != 0 && info2 > 0 && info2 < 0x200 {
                let len = info2 as usize;
                let mut buf = vec![0u8; len];
                if let Some(memory) = system.get_svc_memory() {
                    let m = memory.lock().unwrap();
                    m.read_block(info1, &mut buf);
                } else {
                    let process = system.current_process_arc().lock().unwrap();
                    let mem = process.process_memory.read().unwrap();
                    if mem.is_valid_range(info1, len) {
                        for (i, byte) in buf.iter_mut().enumerate() {
                            *byte = mem.read_8(info1 + i as u64);
                        }
                    }
                }
                if let Ok(msg) = String::from_utf8(buf) {
                    log::error!("  Break message: {}", msg.trim_end_matches('\0'));
                }
            }

            // Dump nearby memory strings for symbol resolution debugging
            if let Some(memory) = system.get_svc_memory() {
                let m = memory.lock().unwrap();
                // Try reading strings at known addresses from the step trace
                for addr in [0x20bc7fau64, 0x20bc827, 0x2244ec7] {
                    let mut buf = vec![0u8; 128];
                    for i in 0..128u64 { buf[i as usize] = m.read_8(addr + i); }
                    if let Some(end) = buf.iter().position(|&b| b == 0) { buf.truncate(end); }
                    if let Ok(s) = String::from_utf8(buf.clone()) {
                        if !s.is_empty() && s.len() < 120 {
                            eprintln!("  [{addr:#x}] = \"{s}\"");
                        }
                    }
                }
            }
            eprintln!("!!! svcBreak(reason={:#x}, info1={:#x}, info2={:#x}) — GAME ABORTED !!!", reason, info1, info2);
            svc_exception::break_execution(system, reason, info1, info2);
            std::process::exit(1);
        }
        Some(SvcId::OutputDebugString) => {
            // IN: str=arg32[0], len=arg32[1]; OUT: ret=arg32[0]
            let str_ptr = get_arg32(args, 0) as u64;
            let str_len = get_arg32(args, 1) as u64;
            let result = svc_debug_string::output_debug_string(system, str_ptr, str_len);
            set_arg32(args, 0, result.get_inner_value());
        }
        Some(SvcId::ReturnFromException) => {
            // IN: result=arg32[0]; OUT: (none)
        }

        // =====================================================================
        // Info
        // =====================================================================
        Some(SvcId::GetInfo) => {
            // IN: info_type=arg32[1], handle=arg32[2], info_subtype=gather64[0,3]
            // OUT: ret=arg32[0], out=scatter64[1,2]
            let info_type_raw = get_arg32(args, 1);
            let handle = get_arg32(args, 2);
            let info_subtype = gather64(args, 0, 3);
            let info_type = crate::hle::kernel::svc::svc_types::InfoType::from_u32(info_type_raw);
            let mut value: u64 = 0;
            let result = svc_info::get_info(system, &mut value, info_type, handle, info_subtype);
            set_arg32(args, 0, result.get_inner_value());
            scatter64(args, 1, 2, value);
        }
        Some(SvcId::GetSystemInfo) => {
            let mut value: u64 = 0;
            let result = svc_info::get_system_info(
                &mut value,
                crate::hle::kernel::svc::svc_types::SystemInfoType::from_u32(get_arg32(args, 1)),
                get_arg32(args, 2),
                gather64(args, 0, 3),
            );
            set_arg32(args, 0, result.get_inner_value());
            scatter64(args, 1, 2, value);
        }

        // =====================================================================
        // Resource limits
        // =====================================================================
        Some(SvcId::GetResourceLimitLimitValue) => {
            // IN: handle=arg32[1], which=arg32[2]; OUT: ret=arg32[0], value=scatter64[1,2]
            set_arg32(args, 0, STUB_SUCCESS);
            scatter64(args, 1, 2, 0x1_0000_0000); // 4 GiB
        }
        Some(SvcId::GetResourceLimitCurrentValue) => {
            set_arg32(args, 0, STUB_SUCCESS);
            scatter64(args, 1, 2, 0);
        }
        Some(SvcId::GetResourceLimitPeakValue) => {
            set_arg32(args, 0, STUB_SUCCESS);
            scatter64(args, 1, 2, 0);
        }
        Some(SvcId::CreateResourceLimit) => {
            // OUT: ret=arg32[0], handle=arg32[1]
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, alloc_stub_handle(system));
        }
        Some(SvcId::SetResourceLimitLimitValue) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }

        // =====================================================================
        // Cache
        // =====================================================================
        Some(SvcId::FlushEntireDataCache) => {}
        Some(SvcId::FlushDataCache) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::InvalidateProcessDataCache) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::StoreProcessDataCache) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::FlushProcessDataCache) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }

        // =====================================================================
        // Code memory
        // =====================================================================
        Some(SvcId::CreateCodeMemory) => {
            // OUT: ret=arg32[0], handle=arg32[1]
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, alloc_stub_handle(system));
        }
        Some(SvcId::ControlCodeMemory) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }

        // =====================================================================
        // Debug SVCs
        // =====================================================================
        Some(SvcId::DebugActiveProcess) => {
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, alloc_stub_handle(system));
        }
        Some(SvcId::BreakDebugProcess) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::TerminateDebugProcess) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::GetDebugEvent) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::ContinueDebugEvent) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::GetDebugThreadContext) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::SetDebugThreadContext) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::QueryDebugProcessMemory) => {
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, 0);
        }
        Some(SvcId::ReadDebugProcessMemory) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::WriteDebugProcessMemory) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::SetHardwareBreakPoint) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::GetDebugThreadParam) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::GetDebugFutureThreadInfo) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::GetLastThreadInfo) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }

        // =====================================================================
        // IO / Device address space
        // =====================================================================
        Some(SvcId::CreateIoPool) => {
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, alloc_stub_handle(system));
        }
        Some(SvcId::CreateIoRegion) => {
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, alloc_stub_handle(system));
        }
        Some(SvcId::MapIoRegion) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::UnmapIoRegion) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::CreateInterruptEvent) => {
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, alloc_stub_handle(system));
        }
        Some(SvcId::QueryPhysicalAddress) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::QueryIoMapping) => {
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, 0);
            set_arg32(args, 2, 0);
        }
        Some(SvcId::CreateDeviceAddressSpace) => {
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, alloc_stub_handle(system));
        }
        Some(SvcId::AttachDeviceAddressSpace) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::DetachDeviceAddressSpace) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::MapDeviceAddressSpaceByForce) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::MapDeviceAddressSpaceAligned) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::UnmapDeviceAddressSpace) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }

        // =====================================================================
        // Process memory
        // =====================================================================
        Some(SvcId::SetProcessMemoryPermission) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::MapProcessMemory) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::UnmapProcessMemory) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::QueryProcessMemory) => {
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, 0);
        }
        Some(SvcId::MapProcessCodeMemory) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }
        Some(SvcId::UnmapProcessCodeMemory) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }

        // =====================================================================
        // Misc
        // =====================================================================
        Some(SvcId::KernelDebug) => {}
        Some(SvcId::ChangeKernelTraceState) => {}
        Some(SvcId::SleepSystem) => {}
        Some(SvcId::ReadWriteRegister) => {
            set_arg32(args, 0, STUB_SUCCESS);
            set_arg32(args, 1, 0);
        }
        Some(SvcId::CallSecureMonitor) => {
            set_arg32(args, 0, STUB_SUCCESS);
        }

        None => {
            log::error!("Unknown SVC 0x{:02X} in 32-bit mode", imm);
            set_arg32(args, 0, 0xF001); // Generic error
        }
    }
}

// =============================================================================
// 64-bit dispatch (AArch64 / LP64)
// =============================================================================

/// Dispatch a 64-bit (AArch64 / LP64) SVC.
///
/// Corresponds to upstream `Call64`. Register layouts match the auto-generated
/// SvcWrap_*64 wrappers in svc.cpp.
fn call64(system: &System, imm: u32, args: &mut SvcArgs) {
    match SvcId::from_u32(imm) {
        Some(SvcId::SetHeapSize) => {
            let size = get_arg64(args, 1);
            let mut heap_base = 0;
            let result = svc_physical_memory::set_heap_size_current_process(system, &mut heap_base, size);
            set_arg64(args, 0, result.get_inner_value() as u64);
            set_arg64(args, 1, heap_base);
        }
        Some(SvcId::SetMemoryPermission) => {
            set_arg64(args, 0, STUB_SUCCESS as u64);
        }
        Some(SvcId::SetMemoryAttribute) => {
            set_arg64(args, 0, STUB_SUCCESS as u64);
        }
        Some(SvcId::MapMemory) => {
            set_arg64(args, 0, STUB_SUCCESS as u64);
        }
        Some(SvcId::UnmapMemory) => {
            set_arg64(args, 0, STUB_SUCCESS as u64);
        }
        Some(SvcId::QueryMemory) => {
            let mem_info_ptr = get_arg64(args, 0);
            let query_addr = get_arg64(args, 2);
            let (base, size, state, perm) = query_memory_info(system, query_addr);
            log::info!("  QueryMemory(info_ptr={:#x}, addr={:#x}) -> base={:#x} size={:#x} state={} perm={}",
                mem_info_ptr, query_addr, base, size, state, perm);
            {
                if let Some(memory) = system.get_svc_memory() {
                    let m = memory.lock().unwrap();
                    m.write_64(mem_info_ptr, base);
                    m.write_64(mem_info_ptr + 8, size);
                    m.write_32(mem_info_ptr + 16, state);
                    m.write_32(mem_info_ptr + 20, 0);
                    m.write_32(mem_info_ptr + 24, perm);
                    m.write_32(mem_info_ptr + 28, 0);
                    m.write_32(mem_info_ptr + 32, 0);
                    m.write_32(mem_info_ptr + 36, 0);
                } else {
                    let mut mem = system.shared_process_memory().write().unwrap();
                    if mem.is_valid_range(mem_info_ptr, 40) {
                        mem.write_64(mem_info_ptr, base);
                        mem.write_64(mem_info_ptr + 8, size);
                        mem.write_32(mem_info_ptr + 16, state);
                        mem.write_32(mem_info_ptr + 20, 0);
                        mem.write_32(mem_info_ptr + 24, perm);
                        mem.write_32(mem_info_ptr + 28, 0);
                        mem.write_32(mem_info_ptr + 32, 0);
                        mem.write_32(mem_info_ptr + 36, 0);
                    }
                }
            }
            set_arg64(args, 0, STUB_SUCCESS as u64);
            set_arg64(args, 1, 0); // page_info
        }
        Some(SvcId::ExitProcess) => {
            svc_process::exit_process(system);
        }
        Some(SvcId::CreateThread) => {
            let mut out_handle = 0;
            let result = svc_thread::create_thread(
                system,
                &mut out_handle,
                get_arg64(args, 1),
                get_arg64(args, 2),
                get_arg64(args, 3),
                get_arg64(args, 4) as i32,
                get_arg64(args, 5) as i32,
            );
            set_arg64(args, 0, result.get_inner_value() as u64);
            set_arg64(args, 1, out_handle as u64);
        }
        Some(SvcId::StartThread) => {
            let result = svc_thread::start_thread(system, get_arg64(args, 0) as u32);
            set_arg64(args, 0, result.get_inner_value() as u64);
        }
        Some(SvcId::ExitThread) => {
            svc_thread::exit_thread(system);
        }
        Some(SvcId::SleepThread) => {
            let ns = get_arg64(args, 0) as i64;
            svc_thread::sleep_thread(system, ns);
        }
        Some(SvcId::GetThreadPriority) => {
            let mut out_priority = 0;
            let result =
                svc_thread::get_thread_priority(system, &mut out_priority, get_arg64(args, 1) as u32);
            set_arg64(args, 0, result.get_inner_value() as u64);
            set_arg64(args, 1, out_priority as u64);
        }
        Some(SvcId::SetThreadPriority) => {
            let result = svc_thread::set_thread_priority(
                system,
                get_arg64(args, 0) as u32,
                get_arg64(args, 1) as i32,
            );
            set_arg64(args, 0, result.get_inner_value() as u64);
        }
        Some(SvcId::GetThreadCoreMask) => {
            let mut out_core_id = 0;
            let mut out_affinity_mask = 0;
            let result = svc_thread::get_thread_core_mask(
                system,
                &mut out_core_id,
                &mut out_affinity_mask,
                get_arg64(args, 2) as u32,
            );
            set_arg64(args, 0, result.get_inner_value() as u64);
            set_arg64(args, 1, out_core_id as u64);
            set_arg64(args, 2, out_affinity_mask);
        }
        Some(SvcId::SetThreadCoreMask) => {
            let result = svc_thread::set_thread_core_mask(
                system,
                get_arg64(args, 0) as u32,
                get_arg64(args, 1) as i32,
                get_arg64(args, 2),
            );
            set_arg64(args, 0, result.get_inner_value() as u64);
        }
        Some(SvcId::GetCurrentProcessorNumber) => {
            set_arg64(args, 0, svc_processor::get_current_processor_number(system) as u64);
        }
        Some(SvcId::CloseHandle) => {
            let result = svc_synchronization::close_handle(system, get_arg64(args, 0) as u32);
            set_arg64(args, 0, result.get_inner_value() as u64);
        }
        Some(SvcId::GetSystemTick) => {
            set_arg64(args, 0, svc_tick::get_system_tick(system) as u64);
        }
        Some(SvcId::ConnectToNamedPort) => {
            let mut out = 0;
            let result = svc_port::connect_to_named_port(system, &mut out, get_arg64(args, 1));
            set_arg64(args, 0, result.get_inner_value() as u64);
            set_arg64(args, 1, out as u64);
        }
        Some(SvcId::SendSyncRequest) => {
            let result = svc_ipc::send_sync_request(system, get_arg64(args, 0) as u32);
            set_arg64(args, 0, result.get_inner_value() as u64);
        }
        Some(SvcId::GetProcessId) => {
            set_arg64(args, 0, STUB_SUCCESS as u64);
            set_arg64(args, 1, 1); // pid=1
        }
        Some(SvcId::GetThreadId) => {
            let mut out_thread_id = 0;
            let result = svc_thread::get_thread_id(system, &mut out_thread_id, get_arg64(args, 1) as u32);
            set_arg64(args, 0, result.get_inner_value() as u64);
            set_arg64(args, 1, out_thread_id);
        }
        Some(SvcId::Break) => {
            let reason = get_arg64(args, 0) as u32;
            let info1 = get_arg64(args, 1);
            let info2 = get_arg64(args, 2);
            eprintln!("!!! svcBreak(reason={:#x}, info1={:#x}, info2={:#x}) — GAME ABORTED !!!", reason, info1, info2);
            svc_exception::break_execution(system, reason, info1, info2);
            std::process::exit(1);
        }
        Some(SvcId::OutputDebugString) => {
            let str_ptr = get_arg64(args, 0);
            let str_len = get_arg64(args, 1);
            let result = svc_debug_string::output_debug_string(system, str_ptr, str_len);
            set_arg64(args, 0, result.get_inner_value() as u64);
        }
        Some(SvcId::GetInfo) => {
            let info_type_raw = get_arg64(args, 1) as u32;
            let handle = get_arg64(args, 2) as u32;
            let info_subtype = get_arg64(args, 3);
            let info_type = crate::hle::kernel::svc::svc_types::InfoType::from_u32(info_type_raw);
            let mut value: u64 = 0;
            let result = svc_info::get_info(system, &mut value, info_type, handle, info_subtype);
            set_arg64(args, 0, result.get_inner_value() as u64);
            set_arg64(args, 1, value);
        }
        Some(SvcId::MapPhysicalMemory) => {
            set_arg64(args, 0, STUB_SUCCESS as u64);
        }
        Some(SvcId::GetResourceLimitLimitValue) => {
            set_arg64(args, 0, STUB_SUCCESS as u64);
            set_arg64(args, 1, 0x1_0000_0000);
        }
        Some(SvcId::GetResourceLimitCurrentValue) => {
            set_arg64(args, 0, STUB_SUCCESS as u64);
            set_arg64(args, 1, 0);
        }
        // All remaining 64-bit SVCs return stub success
        Some(svc_id) => {
            log::warn!("SVC Call64: {:?} (0x{:02X}) stub", svc_id, imm);
            set_arg64(args, 0, STUB_SUCCESS as u64);
        }
        None => {
            log::error!("Unknown SVC 0x{:02X} in 64-bit mode", imm);
            set_arg64(args, 0, 0xF001);
        }
    }
}

// =============================================================================
// Main entry point
// =============================================================================

/// Main SVC entry point. Called by the CPU emulation core when an SVC
/// instruction is executed.
///
/// Corresponds to upstream `Kernel::Svc::Call`.
///
/// In upstream, this function:
/// 1. Saves SVC arguments from the physical core
/// 2. Enters the SVC profile
/// 3. Dispatches to Call32 or Call64 based on process bitness
/// 4. Exits the SVC profile
/// 5. Loads SVC arguments back to the physical core
pub fn call(system: &System, imm: u32, is_64bit: bool, args: &mut SvcArgs) {
    // Structured SVC trace — enabled by RUZU_SVC_TRACE=1
    let trace_enabled = {
        use std::sync::atomic::{AtomicU8, Ordering};
        static INIT: AtomicU8 = AtomicU8::new(0); // 0=unknown, 1=off, 2=on
        match INIT.load(Ordering::Relaxed) {
            2 => true,
            1 => false,
            _ => {
                let on = std::env::var("RUZU_SVC_TRACE").is_ok();
                INIT.store(if on { 2 } else { 1 }, Ordering::Relaxed);
                on
            }
        }
    };

    if trace_enabled {
        use std::sync::atomic::{AtomicU32, Ordering};
        static SVC_SEQ: AtomicU32 = AtomicU32::new(0);
        let seq = SVC_SEQ.fetch_add(1, Ordering::Relaxed);
        let svc_name = SvcId::from_u32(imm).map_or("???".to_string(), |s| format!("{:?}", s));

        // Log SVC entry
        eprintln!("SVC[{:04}] #{:#04x} ({}) IN  args=[{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x}]",
            seq, imm, svc_name,
            args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);

        // For SendSyncRequest, dump TLS before
        if imm == 0x21 {
            if let Some(memory) = system.get_svc_memory() {
                let m = memory.lock().unwrap();
                if let Some(thread) = system.current_thread() {
                    let tls = thread.lock().unwrap().get_tls_address().get();
                    let mut words = [0u32; 16];
                    for i in 0..16 { words[i] = m.read_32(tls + i as u64 * 4); }
                    eprintln!("  TLS_REQ [{:#x}]: {:08x} {:08x} {:08x} {:08x}  {:08x} {:08x} {:08x} {:08x}  {:08x} {:08x} {:08x} {:08x}  {:08x} {:08x} {:08x} {:08x}",
                        tls, words[0], words[1], words[2], words[3], words[4], words[5], words[6], words[7],
                        words[8], words[9], words[10], words[11], words[12], words[13], words[14], words[15]);
                }
            }
        }

        if is_64bit { call64(system, imm, args); } else { call32(system, imm, args); }

        // Log SVC exit
        eprintln!("SVC[{:04}] #{:#04x} ({}) OUT args=[{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x},{:#x}]",
            seq, imm, svc_name,
            args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);

        // For SendSyncRequest, dump TLS after (response)
        if imm == 0x21 {
            if let Some(memory) = system.get_svc_memory() {
                let m = memory.lock().unwrap();
                if let Some(thread) = system.current_thread() {
                    let tls = thread.lock().unwrap().get_tls_address().get();
                    let mut words = [0u32; 16];
                    for i in 0..16 { words[i] = m.read_32(tls + i as u64 * 4); }
                    eprintln!("  TLS_RSP [{:#x}]: {:08x} {:08x} {:08x} {:08x}  {:08x} {:08x} {:08x} {:08x}  {:08x} {:08x} {:08x} {:08x}  {:08x} {:08x} {:08x} {:08x}",
                        tls, words[0], words[1], words[2], words[3], words[4], words[5], words[6], words[7],
                        words[8], words[9], words[10], words[11], words[12], words[13], words[14], words[15]);
                }
            }
        }
    } else {
        if is_64bit { call64(system, imm, args); } else { call32(system, imm, args); }
    }

    // Upstream reaches the equivalent behavior when the scheduler lock is
    // released after the SVC handler. In this cooperative port, drain a
    // pending current-thread termination once per SVC return path.
    drain_current_thread_termination(system);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hle::kernel::k_process::KProcess;
    use crate::hle::kernel::k_scheduler::KScheduler;
    use crate::hle::kernel::k_thread::{KThread, ThreadState};
    use crate::hle::kernel::k_worker_task_manager::KWorkerTaskManager;
    use std::sync::{Arc, Mutex};

    fn test_system() -> System {
        let mut system = System::new_for_test();

        let mut process = KProcess::new();
        process.capabilities.core_mask = 0xF;
        process.capabilities.priority_mask = u64::MAX;
        process.flags = 0;
        process.allocate_code_memory(0x200000, 0x20000);
        process.initialize_handle_table();
        process.initialize_thread_local_region_base(0x240000);

        let process = Arc::new(Mutex::new(process));
        let current_thread = Arc::new(Mutex::new(KThread::new()));
        let scheduler = Arc::new(Mutex::new(KScheduler::new(0)));
        {
            let mut thread = current_thread.lock().unwrap();
            thread.initialize_main_thread(0x200000, 0x250000, 0, 0x23f000, &process, 1, 1, false);
            thread.set_priority(44);
            thread.set_base_priority(44);
        }
        process
            .lock()
            .unwrap()
            .register_thread_object(current_thread.clone());
        process.lock().unwrap().push_back_to_priority_queue(1);
        scheduler.lock().unwrap().initialize(1, 0, 0);

        let shared_memory = process.lock().unwrap().get_shared_memory();
        system.set_current_process_arc(process);
        system.set_scheduler_arc(scheduler);
        system.set_shared_process_memory(shared_memory);
        system.set_runtime_program_id(1);
        system.set_runtime_64bit(false);
        system
    }

    #[test]
    fn test_svc_id_roundtrip() {
        assert_eq!(SvcId::from_u32(0x01), Some(SvcId::SetHeapSize));
        assert_eq!(SvcId::from_u32(0x07), Some(SvcId::ExitProcess));
        assert_eq!(SvcId::from_u32(0x7F), Some(SvcId::CallSecureMonitor));
        assert_eq!(SvcId::from_u32(0x90), Some(SvcId::MapInsecureMemory));
        assert_eq!(SvcId::from_u32(0x91), Some(SvcId::UnmapInsecureMemory));
        // Gaps in the enum
        assert_eq!(SvcId::from_u32(0x38), None);
        assert_eq!(SvcId::from_u32(0x3B), None);
        assert_eq!(SvcId::from_u32(0x5B), None);
        assert_eq!(SvcId::from_u32(0x6E), None);
        // Unknown
        assert_eq!(SvcId::from_u32(0x00), None);
        assert_eq!(SvcId::from_u32(0xFF), None);
    }

    #[test]
    fn test_arg_helpers() {
        let mut args: SvcArgs = [0; 8];
        set_arg32(&mut args, 0, 0xDEADBEEF);
        assert_eq!(get_arg32(&args, 0), 0xDEADBEEF);
        assert_eq!(get_arg64(&args, 0), 0xDEADBEEF);

        set_arg64(&mut args, 1, 0x1234_5678_9ABC_DEF0);
        assert_eq!(get_arg64(&args, 1), 0x1234_5678_9ABC_DEF0);
        assert_eq!(get_arg32(&args, 1), 0x9ABC_DEF0);
    }

    #[test]
    fn test_gather_scatter_64() {
        let mut args: SvcArgs = [0; 8];
        args[0] = 0xDEADBEEF;
        args[3] = 0x12345678;
        assert_eq!(gather64(&args, 0, 3), 0x12345678_DEADBEEF);

        scatter64(&mut args, 1, 2, 0xAAAABBBB_CCCCDDDD);
        assert_eq!(args[1], 0xCCCCDDDD);
        assert_eq!(args[2], 0xAAAABBBB);
    }

    #[test]
    fn call_drains_current_thread_termination_on_svc_return() {
        let system = test_system();
        let current_thread = system.current_thread().expect("main thread must exist");
        current_thread.lock().unwrap().request_terminate();

        let mut args: SvcArgs = [0; 8];
        call(&system, SvcId::GetSystemTick as u32, true, &mut args);
        KWorkerTaskManager::wait_for_global_idle();

        let thread = current_thread.lock().unwrap();
        assert_eq!(thread.get_state(), ThreadState::TERMINATED);
        assert!(thread.is_signaled());
    }
}
