//! Port of zuyu/src/core/hle/kernel/svc/svc_info.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for GetInfo and GetSystemInfo.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::{Handle, INVALID_HANDLE};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Gets system/memory information for the current process.
///
/// This is a large switch over InfoType, querying process page tables,
/// resource limits, random entropy, thread tick counts, idle tick counts, etc.
pub fn get_info(
    result: &mut u64,
    info_id_type: InfoType,
    handle: Handle,
    info_sub_id: u64,
) -> ResultCode {
    log::trace!(
        "svc::GetInfo called info_id={:?}, info_sub_id=0x{:X}, handle=0x{:08X}",
        info_id_type, info_sub_id, handle
    );

    match info_id_type {
        InfoType::CoreMask
        | InfoType::PriorityMask
        | InfoType::AliasRegionAddress
        | InfoType::AliasRegionSize
        | InfoType::HeapRegionAddress
        | InfoType::HeapRegionSize
        | InfoType::AslrRegionAddress
        | InfoType::AslrRegionSize
        | InfoType::StackRegionAddress
        | InfoType::StackRegionSize
        | InfoType::TotalMemorySize
        | InfoType::UsedMemorySize
        | InfoType::SystemResourceSizeTotal
        | InfoType::SystemResourceSizeUsed
        | InfoType::ProgramId
        | InfoType::UserExceptionContextAddress
        | InfoType::TotalNonSystemMemorySize
        | InfoType::UsedNonSystemMemorySize
        | InfoType::IsApplication
        | InfoType::FreeThreadCount => {
            if info_sub_id != 0 {
                return RESULT_INVALID_ENUM_VALUE;
            }

            // TODO: Get process from handle table, query appropriate field.
            log::warn!(
                "svc::GetInfo({:?}): kernel object access not yet implemented",
                info_id_type
            );
            *result = 0;
            RESULT_NOT_IMPLEMENTED
        }

        InfoType::DebuggerAttached => {
            *result = 0;
            RESULT_SUCCESS
        }

        InfoType::ResourceLimit => {
            if handle != 0 {
                return RESULT_INVALID_HANDLE;
            }
            if info_sub_id != 0 {
                return RESULT_INVALID_COMBINATION;
            }

            // TODO: Get current process resource limit, add to handle table.
            *result = INVALID_HANDLE as u64;
            RESULT_SUCCESS
        }

        InfoType::RandomEntropy => {
            if handle != 0 {
                return RESULT_INVALID_HANDLE;
            }
            if info_sub_id >= 4 {
                return RESULT_INVALID_COMBINATION;
            }

            // TODO: GetCurrentProcess(kernel).GetRandomEntropy(info_sub_id)
            *result = 0;
            log::warn!("svc::GetInfo(RandomEntropy): kernel object access not yet implemented");
            RESULT_NOT_IMPLEMENTED
        }

        InfoType::InitialProcessIdRange => {
            log::warn!(
                "svc::GetInfo(InitialProcessIdRange): STUBBED, returned 0"
            );
            *result = 0;
            RESULT_SUCCESS
        }

        InfoType::ThreadTickCount => {
            const NUM_CPUS: u64 = 4;
            if info_sub_id != 0xFFFF_FFFF_FFFF_FFFF && info_sub_id >= NUM_CPUS {
                log::error!(
                    "Core count is out of range, expected {} but got {}",
                    NUM_CPUS, info_sub_id
                );
                return RESULT_INVALID_COMBINATION;
            }

            // TODO: Get thread from handle, compute tick count.
            *result = 0;
            log::warn!("svc::GetInfo(ThreadTickCount): kernel object access not yet implemented");
            RESULT_NOT_IMPLEMENTED
        }

        InfoType::IdleTickCount => {
            if handle != INVALID_HANDLE {
                return RESULT_INVALID_HANDLE;
            }

            // TODO: Verify core is valid, get idle tick count.
            *result = 0;
            log::warn!("svc::GetInfo(IdleTickCount): kernel object access not yet implemented");
            RESULT_NOT_IMPLEMENTED
        }

        InfoType::MesosphereCurrentProcess => {
            if handle != INVALID_HANDLE {
                return RESULT_INVALID_HANDLE;
            }
            if info_sub_id != 0 {
                return RESULT_INVALID_COMBINATION;
            }

            // TODO: Get handle for current process.
            *result = 0;
            log::warn!("svc::GetInfo(MesosphereCurrentProcess): kernel object access not yet implemented");
            RESULT_NOT_IMPLEMENTED
        }

        _ => {
            log::error!(
                "Unimplemented svcGetInfo id=0x{:X}",
                info_id_type as u32
            );
            RESULT_INVALID_ENUM_VALUE
        }
    }
}

/// Gets system info. (Unimplemented upstream.)
pub fn get_system_info(
    _out: &mut u64,
    _info_type: SystemInfoType,
    _handle: Handle,
    _info_subtype: u64,
) -> ResultCode {
    log::warn!("svc::GetSystemInfo: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}
