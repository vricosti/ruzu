//! Port of zuyu/src/core/hle/kernel/svc/svc_info.cpp
//! Status: Ported
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for GetInfo and GetSystemInfo.

use crate::core::System;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::{Handle, INVALID_HANDLE, PseudoHandle};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Gets system/memory information for the current process.
///
/// This is a large switch over InfoType, querying process page tables,
/// resource limits, random entropy, thread tick counts, idle tick counts, etc.
pub fn get_info(
    system: &System,
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

            // Get the process from the handle.
            // Upstream: GetCurrentProcess(kernel).GetHandleTable().GetObject<KProcess>(handle)
            // For pseudo-handle CurrentProcess (0xFFFF8001) or handle 0 (self), use current process.
            let process_arc = if handle == PseudoHandle::CurrentProcess as Handle || handle == 0 {
                system.current_process_arc()
            } else {
                let current = system.current_process_arc();
                let guard = current.lock().unwrap();
                match guard.handle_table.get_object(handle) {
                    Some(_) => {
                        drop(guard);
                        system.current_process_arc()
                    }
                    None => {
                        return RESULT_INVALID_HANDLE;
                    }
                }
            };

            let process = process_arc.lock().unwrap();

            match info_id_type {
                InfoType::CoreMask => {
                    *result = process.get_core_mask();
                }
                InfoType::PriorityMask => {
                    *result = process.get_priority_mask();
                }
                InfoType::AliasRegionAddress => {
                    *result = process.page_table.get_alias_region_start().get();
                }
                InfoType::AliasRegionSize => {
                    *result = process.page_table.get_alias_region_size() as u64;
                }
                InfoType::HeapRegionAddress => {
                    *result = process.page_table.get_heap_region_start().get();
                }
                InfoType::HeapRegionSize => {
                    *result = process.page_table.get_heap_region_size() as u64;
                }
                InfoType::AslrRegionAddress => {
                    *result = process.page_table.get_alias_code_region_start().get();
                }
                InfoType::AslrRegionSize => {
                    *result = process.page_table.get_alias_code_region_size() as u64;
                }
                InfoType::StackRegionAddress => {
                    *result = process.page_table.get_stack_region_start().get();
                }
                InfoType::StackRegionSize => {
                    *result = process.page_table.get_stack_region_size() as u64;
                }
                InfoType::TotalMemorySize => {
                    // Upstream: process->GetTotalUserPhysicalMemorySize()
                    // Approximate: code + heap + stack sizes
                    *result = process.page_table.get_code_region_size() as u64
                        + process.page_table.get_current_heap_size() as u64
                        + process.page_table.get_stack_region_size() as u64;
                }
                InfoType::UsedMemorySize => {
                    // Upstream: process->GetUsedUserPhysicalMemorySize()
                    *result = process.page_table.get_code_region_size() as u64
                        + process.page_table.get_current_heap_size() as u64
                        + process.page_table.get_stack_region_size() as u64;
                }
                InfoType::SystemResourceSizeTotal => {
                    // Upstream: process->GetTotalSystemResourceSize()
                    *result = 0;
                }
                InfoType::SystemResourceSizeUsed => {
                    // Upstream: process->GetUsedSystemResourceSize()
                    *result = 0;
                }
                InfoType::ProgramId => {
                    *result = process.get_program_id();
                }
                InfoType::UserExceptionContextAddress => {
                    *result = process.get_process_local_region_address().get();
                }
                InfoType::TotalNonSystemMemorySize => {
                    // Upstream: process->GetTotalNonSystemUserPhysicalMemorySize()
                    *result = process.page_table.get_code_region_size() as u64
                        + process.page_table.get_current_heap_size() as u64
                        + process.page_table.get_stack_region_size() as u64;
                }
                InfoType::UsedNonSystemMemorySize => {
                    // Upstream: process->GetUsedNonSystemUserPhysicalMemorySize()
                    *result = process.page_table.get_code_region_size() as u64
                        + process.page_table.get_current_heap_size() as u64
                        + process.page_table.get_stack_region_size() as u64;
                }
                InfoType::IsApplication => {
                    *result = if process.is_application() { 1 } else { 0 };
                }
                InfoType::FreeThreadCount => {
                    // Upstream: resource_limit->GetLimitValue(ThreadCountMax) - resource_limit->GetCurrentValue(ThreadCountMax)
                    // Approximate with a reasonable default.
                    if let Some(ref rl) = process.resource_limit {
                        let rl_guard = rl.lock().unwrap();
                        let limit = rl_guard.get_limit_value(
                            crate::hle::kernel::k_resource_limit::LimitableResource::ThreadCountMax,
                        );
                        let current = rl_guard.get_current_value(
                            crate::hle::kernel::k_resource_limit::LimitableResource::ThreadCountMax,
                        );
                        *result = (limit - current) as u64;
                    } else {
                        *result = 0;
                    }
                }
                _ => {
                    log::error!("Unimplemented svcGetInfo id=0x{:X}", info_id_type as u32);
                    return RESULT_INVALID_ENUM_VALUE;
                }
            }

            RESULT_SUCCESS
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

            // Upstream: Get current process resource limit, add to handle table, return the handle.
            // If no resource limit, return InvalidHandle (upstream considers this success).
            let process_arc = system.current_process_arc();
            let mut process = process_arc.lock().unwrap();
            if process.resource_limit.is_none() {
                *result = INVALID_HANDLE as u64;
                return RESULT_SUCCESS;
            }

            // Add the resource limit to the handle table.
            // Use object_id 0xFFFF_FFFF as a sentinel for resource limit objects.
            match process.handle_table.add(0xFFFF_FFFF) {
                Ok(resource_handle) => {
                    *result = resource_handle as u64;
                }
                Err(_) => {
                    *result = INVALID_HANDLE as u64;
                }
            }
            RESULT_SUCCESS
        }

        InfoType::RandomEntropy => {
            if handle != 0 {
                return RESULT_INVALID_HANDLE;
            }
            if info_sub_id >= 4 {
                return RESULT_INVALID_COMBINATION;
            }

            let process = system.current_process_arc().lock().unwrap();
            *result = process.get_random_entropy(info_sub_id as usize);
            RESULT_SUCCESS
        }

        InfoType::InitialProcessIdRange => {
            // Upstream: STUBBED — returns 0 for privileged process id bounds.
            log::warn!(
                "svc::GetInfo(InitialProcessIdRange): Upstream STUBBED, returned 0"
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

            // Upstream: Get the thread from the handle, compute tick count based on
            // whether this is the current thread and which core is requested.
            // For now, return 0 ticks (matching upstream when not same thread or
            // different core).
            let process = system.current_process_arc().lock().unwrap();
            let _thread_obj_id = match process.handle_table.get_object(handle) {
                Some(id) => id,
                None => {
                    log::error!(
                        "Thread handle does not exist, handle=0x{:08X}",
                        handle
                    );
                    return RESULT_INVALID_HANDLE;
                }
            };

            // Upstream computes: if same_thread && sub_id == 0xFFFF...FFFF,
            //   out = thread_ticks + (current_ticks - prev_ctx_switch)
            // else if same_thread && sub_id == current_core,
            //   out = current_ticks - prev_ctx_switch
            // else out = 0
            *result = 0;
            RESULT_SUCCESS
        }

        InfoType::IdleTickCount => {
            if handle != INVALID_HANDLE {
                return RESULT_INVALID_HANDLE;
            }

            // Upstream: Verify core is valid, get idle tick count from scheduler.
            let core_valid = info_sub_id == 0xFFFF_FFFF_FFFF_FFFF || info_sub_id == 0;
            if !core_valid {
                return RESULT_INVALID_COMBINATION;
            }

            // Upstream: kernel.CurrentScheduler()->GetIdleThread()->GetCpuTime()
            *result = 0;
            RESULT_SUCCESS
        }

        InfoType::MesosphereCurrentProcess => {
            if handle != INVALID_HANDLE {
                return RESULT_INVALID_HANDLE;
            }
            if info_sub_id != 0 {
                return RESULT_INVALID_COMBINATION;
            }

            // Upstream: Add current process to its own handle table, return the new handle.
            let process_arc = system.current_process_arc();
            let mut process = process_arc.lock().unwrap();
            let process_id = process.get_process_id();
            match process.handle_table.add(process_id) {
                Ok(h) => {
                    *result = h as u64;
                }
                Err(_) => {
                    *result = 0;
                    return RESULT_OUT_OF_HANDLES;
                }
            }
            RESULT_SUCCESS
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

/// Gets system info. (Upstream: UNIMPLEMENTED)
pub fn get_system_info(
    _out: &mut u64,
    _info_type: SystemInfoType,
    _handle: Handle,
    _info_subtype: u64,
) -> ResultCode {
    log::warn!("svc::GetSystemInfo: Upstream UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}
