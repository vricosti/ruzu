//! Port of zuyu/src/core/hle/kernel/svc/svc_device_address_space.cpp
//! Status: Ported
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for device address space operations.

use crate::core::System;
use crate::hle::kernel::k_device_address_space::KDeviceAddressSpace;
use crate::hle::kernel::k_process::ProcessLock;
use crate::hle::kernel::k_typed_address::KProcessAddress;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::{Handle, PseudoHandle};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use std::sync::{Arc, Mutex};

const DEVICE_ADDRESS_SPACE_ALIGN_MASK_LOCAL: u64 = (1u64 << 22) - 1;

fn is_process_and_device_aligned(process_address: u64, device_address: u64) -> bool {
    (process_address & DEVICE_ADDRESS_SPACE_ALIGN_MASK_LOCAL)
        == (device_address & DEVICE_ADDRESS_SPACE_ALIGN_MASK_LOCAL)
}

fn is_valid_device_memory_permission(perm: MemoryPermission) -> bool {
    matches!(
        perm,
        MemoryPermission::Read | MemoryPermission::Write | MemoryPermission::ReadWrite
    )
}

fn resolve_device_address_space_and_process(
    system: &System,
    das_handle: Handle,
    process_handle: Handle,
) -> Result<(Arc<Mutex<KDeviceAddressSpace>>, Arc<ProcessLock>), ResultCode> {
    let current_process = system.current_process_arc().clone();
    let process = current_process.lock().unwrap();

    let Some(das_object_id) = process.handle_table.get_object(das_handle) else {
        return Err(RESULT_INVALID_HANDLE);
    };
    let Some(das) = process
        .device_address_space_objects
        .get(&das_object_id)
        .cloned()
    else {
        return Err(RESULT_INVALID_HANDLE);
    };

    let target_process = if process_handle == PseudoHandle::CurrentProcess as Handle {
        current_process.clone()
    } else {
        let Some(process_object_id) = process.handle_table.get_object(process_handle) else {
            return Err(RESULT_INVALID_HANDLE);
        };
        let current_process_id = process.get_process_id();
        if process_object_id == current_process_id {
            current_process.clone()
        } else {
            let Some(kernel) = system.kernel() else {
                return Err(RESULT_INVALID_HANDLE);
            };
            let Some(target_process) = kernel.get_process_by_id(process_object_id) else {
                return Err(RESULT_INVALID_HANDLE);
            };
            target_process
        }
    };

    drop(process);
    Ok((das, target_process))
}

/// Creates a device address space.
///
/// Upstream: Creates KDeviceAddressSpace, initializes, registers, adds to handle table.
pub fn create_device_address_space(
    system: &System,
    out: &mut Handle,
    das_address: u64,
    das_size: u64,
) -> ResultCode {
    // Validate input.
    if das_address % PAGE_SIZE != 0 {
        return RESULT_INVALID_MEMORY_REGION;
    }
    if das_size % PAGE_SIZE != 0 {
        return RESULT_INVALID_MEMORY_REGION;
    }
    if das_size == 0 {
        return RESULT_INVALID_MEMORY_REGION;
    }
    if das_address >= das_address.wrapping_add(das_size) {
        return RESULT_INVALID_MEMORY_REGION;
    }

    let mut process = system.current_process_arc().lock().unwrap();

    let mut das = KDeviceAddressSpace::new();
    let init_result = das.initialize(das_address, das_size);
    if init_result != RESULT_SUCCESS {
        return init_result;
    }

    // Allocate a unique object ID for the device address space.
    static NEXT_DAS_ID: std::sync::atomic::AtomicU64 =
        std::sync::atomic::AtomicU64::new(0xE000_0000);
    let das_id = NEXT_DAS_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

    // Add to handle table.
    match process.handle_table.add(das_id) {
        Ok(h) => {
            *out = h;
            process
                .device_address_space_objects
                .insert(das_id, Arc::new(Mutex::new(das)));
        }
        Err(_) => {
            *out = 0;
            return RESULT_OUT_OF_HANDLES;
        }
    }

    RESULT_SUCCESS
}

/// Attaches a device address space.
///
/// Upstream: Gets KDeviceAddressSpace from handle, calls das->Attach(device_name).
pub fn attach_device_address_space(
    system: &System,
    device_name: DeviceName,
    das_handle: Handle,
) -> ResultCode {
    let das = {
        let process = system.current_process_arc().lock().unwrap();

        // Validate the handle.
        let object_id = match process.handle_table.get_object(das_handle) {
            Some(id) => id,
            None => return RESULT_INVALID_HANDLE,
        };
        let Some(das) = process.device_address_space_objects.get(&object_id) else {
            return RESULT_INVALID_HANDLE;
        };
        das.clone()
    };

    let result = das.lock().unwrap().attach(device_name);
    result
}

/// Detaches a device address space.
///
/// Upstream: Gets KDeviceAddressSpace from handle, calls das->Detach(device_name).
pub fn detach_device_address_space(
    system: &System,
    device_name: DeviceName,
    das_handle: Handle,
) -> ResultCode {
    let das = {
        let process = system.current_process_arc().lock().unwrap();

        // Validate the handle.
        let object_id = match process.handle_table.get_object(das_handle) {
            Some(id) => id,
            None => return RESULT_INVALID_HANDLE,
        };
        let Some(das) = process.device_address_space_objects.get(&object_id) else {
            return RESULT_INVALID_HANDLE;
        };
        das.clone()
    };

    let result = das.lock().unwrap().detach(device_name);
    result
}

/// Maps a device address space by force.
///
/// Upstream: Validates, gets DAS and process from handles, verifies process_address
/// is in range, calls das->MapByForce.
pub fn map_device_address_space_by_force(
    system: &System,
    das_handle: Handle,
    process_handle: Handle,
    process_address: u64,
    size: u64,
    device_address: u64,
    option: u32,
) -> ResultCode {
    let option_pack = MapDeviceAddressSpaceOption { raw: option };
    let device_perm = option_pack.permission();
    let reserved = option_pack.reserved();

    // Validate input.
    if process_address % PAGE_SIZE != 0 {
        return RESULT_INVALID_ADDRESS;
    }
    if device_address % PAGE_SIZE != 0 {
        return RESULT_INVALID_ADDRESS;
    }
    if size % PAGE_SIZE != 0 {
        return RESULT_INVALID_SIZE;
    }
    if size == 0 {
        return RESULT_INVALID_SIZE;
    }
    if process_address >= process_address.wrapping_add(size) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }
    if device_address >= device_address.wrapping_add(size) {
        return RESULT_INVALID_MEMORY_REGION;
    }
    if !is_valid_device_memory_permission(device_perm) {
        return RESULT_INVALID_NEW_MEMORY_PERMISSION;
    }
    if reserved != 0 {
        return RESULT_INVALID_ENUM_VALUE;
    }

    let (das, process_arc) =
        match resolve_device_address_space_and_process(system, das_handle, process_handle) {
            Ok(resolved) => resolved,
            Err(result) => return result,
        };
    let das = das.lock().unwrap();
    let mut process = process_arc.lock().unwrap();

    // Validate that the process address is within range.
    let addr_kpa = KProcessAddress::new(process_address);
    if !process.page_table.contains(addr_kpa, size as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    das.map_by_force(
        &mut process.page_table,
        process_address,
        size as usize,
        device_address,
        option,
    )
}

/// Maps a device address space with alignment check.
///
/// Upstream: Same as MapByForce but with additional alignment check between
/// process_address and device_address.
pub fn map_device_address_space_aligned(
    system: &System,
    das_handle: Handle,
    process_handle: Handle,
    process_address: u64,
    size: u64,
    device_address: u64,
    option: u32,
) -> ResultCode {
    let option_pack = MapDeviceAddressSpaceOption { raw: option };
    let device_perm = option_pack.permission();
    let reserved = option_pack.reserved();

    if process_address % PAGE_SIZE != 0 {
        return RESULT_INVALID_ADDRESS;
    }
    if device_address % PAGE_SIZE != 0 {
        return RESULT_INVALID_ADDRESS;
    }
    if !is_process_and_device_aligned(process_address, device_address) {
        return RESULT_INVALID_ADDRESS;
    }
    if size % PAGE_SIZE != 0 {
        return RESULT_INVALID_SIZE;
    }
    if size == 0 {
        return RESULT_INVALID_SIZE;
    }
    if process_address >= process_address.wrapping_add(size) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }
    if device_address >= device_address.wrapping_add(size) {
        return RESULT_INVALID_MEMORY_REGION;
    }
    if !is_valid_device_memory_permission(device_perm) {
        return RESULT_INVALID_NEW_MEMORY_PERMISSION;
    }
    if reserved != 0 {
        return RESULT_INVALID_ENUM_VALUE;
    }

    let (das, process_arc) =
        match resolve_device_address_space_and_process(system, das_handle, process_handle) {
            Ok(resolved) => resolved,
            Err(result) => return result,
        };
    let das = das.lock().unwrap();
    let mut process = process_arc.lock().unwrap();

    // Validate that the process address is within range.
    let addr_kpa = KProcessAddress::new(process_address);
    if !process.page_table.contains(addr_kpa, size as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    das.map_aligned(
        &mut process.page_table,
        process_address,
        size as usize,
        device_address,
        option,
    )
}

/// Unmaps a device address space.
///
/// Upstream: Validates, gets DAS and process from handles, verifies process_address
/// is in range, calls das->Unmap.
pub fn unmap_device_address_space(
    system: &System,
    das_handle: Handle,
    process_handle: Handle,
    process_address: u64,
    size: u64,
    device_address: u64,
) -> ResultCode {
    if process_address % PAGE_SIZE != 0 {
        return RESULT_INVALID_ADDRESS;
    }
    if device_address % PAGE_SIZE != 0 {
        return RESULT_INVALID_ADDRESS;
    }
    if size % PAGE_SIZE != 0 {
        return RESULT_INVALID_SIZE;
    }
    if size == 0 {
        return RESULT_INVALID_SIZE;
    }
    if process_address >= process_address.wrapping_add(size) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }
    if device_address >= device_address.wrapping_add(size) {
        return RESULT_INVALID_MEMORY_REGION;
    }

    let (das, process_arc) =
        match resolve_device_address_space_and_process(system, das_handle, process_handle) {
            Ok(resolved) => resolved,
            Err(result) => return result,
        };
    let das = das.lock().unwrap();
    let mut process = process_arc.lock().unwrap();

    // Validate that the process address is within range.
    let addr_kpa = KProcessAddress::new(process_address);
    if !process.page_table.contains(addr_kpa, size as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    das.unmap(
        &mut process.page_table,
        process_address,
        size as usize,
        device_address,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hle::kernel::k_process::KProcess;

    #[test]
    fn explicit_process_handle_resolves_current_process_object_id() {
        let mut system = System::new_for_test();

        let mut process = KProcess::new();
        process.process_id = 0x1234;
        process.initialize_handle_table();

        let mut das = KDeviceAddressSpace::new();
        assert_eq!(das.initialize(0, PAGE_SIZE), RESULT_SUCCESS);

        let process = Arc::new(ProcessLock::from_value(process));
        let (das_handle, process_handle) = {
            let mut process_guard = process.lock().unwrap();
            let das_object_id = 0xE000_0000;
            let das_handle = process_guard.handle_table.add(das_object_id).unwrap();
            process_guard
                .device_address_space_objects
                .insert(das_object_id, Arc::new(Mutex::new(das)));
            let process_id = process_guard.get_process_id();
            let process_handle = process_guard.handle_table.add(process_id).unwrap();
            (das_handle, process_handle)
        };

        system.set_current_process_arc(process.clone());

        let (_das, resolved_process) =
            resolve_device_address_space_and_process(&system, das_handle, process_handle).unwrap();
        assert!(Arc::ptr_eq(&resolved_process, &process));
    }
}
