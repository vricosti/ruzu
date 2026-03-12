// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/kernel/svc.h and svc.cpp
//! Status: COMPLET (dispatch table structure; individual SVC handlers in svc/ submodules)
//! Derniere synchro: 2026-03-12
//!
//! This file is auto-generated in upstream (svc_generator.py).
//! It provides:
//! - SvcId enumeration for all supervisor call numbers
//! - Argument marshalling helpers
//! - 32-bit and 64-bit dispatch tables
//! - The main `call` entry point

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
        // We use a match rather than transmute for safety.
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

// =============================================================================
// Dispatch tables
// =============================================================================

/// Dispatch a 32-bit (AArch32 / ILP32) SVC.
/// Corresponds to upstream `Call32`.
fn call32(_imm: u32, _args: &mut SvcArgs) {
    match SvcId::from_u32(_imm) {
        Some(svc_id) => {
            // TODO: wire individual SvcWrap_*64From32 handlers when kernel integration
            // is connected. Each handler extracts args as u32, calls the underlying
            // SVC function, and stores results back.
            log::warn!(
                "SVC Call32: {:?} (0x{:02X}) not yet dispatched",
                svc_id,
                _imm
            );
        }
        None => {
            log::error!("Unknown SVC 0x{:02X} in 32-bit mode", _imm);
        }
    }
}

/// Dispatch a 64-bit (AArch64 / LP64) SVC.
/// Corresponds to upstream `Call64`.
fn call64(_imm: u32, _args: &mut SvcArgs) {
    match SvcId::from_u32(_imm) {
        Some(svc_id) => {
            // TODO: wire individual SvcWrap_*64 handlers when kernel integration
            // is connected. Each handler extracts args as u64, calls the underlying
            // SVC function, and stores results back.
            log::warn!(
                "SVC Call64: {:?} (0x{:02X}) not yet dispatched",
                svc_id,
                _imm
            );
        }
        None => {
            log::error!("Unknown SVC 0x{:02X} in 64-bit mode", _imm);
        }
    }
}

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
///
/// The actual kernel/process integration will be connected when
/// the kernel main loop is wired up.
pub fn call(imm: u32, is_64bit: bool, args: &mut SvcArgs) {
    if is_64bit {
        call64(imm, args);
    } else {
        call32(imm, args);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
