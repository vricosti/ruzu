//! Port of zuyu/src/core/hle/kernel/svc/svc_device_address_space.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for device address space operations.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

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

/// Creates a device address space.
pub fn create_device_address_space(
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

    // TODO: KDeviceAddressSpace::Create, Initialize, Register, add to handle table
    log::warn!("svc::CreateDeviceAddressSpace: kernel object access not yet implemented");
    *out = 0;
    RESULT_NOT_IMPLEMENTED
}

/// Attaches a device address space.
pub fn attach_device_address_space(_device_name: DeviceName, _das_handle: Handle) -> ResultCode {
    // TODO: Get DAS from handle, call das->Attach(device_name)
    log::warn!("svc::AttachDeviceAddressSpace: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Detaches a device address space.
pub fn detach_device_address_space(_device_name: DeviceName, _das_handle: Handle) -> ResultCode {
    // TODO: Get DAS from handle, call das->Detach(device_name)
    log::warn!("svc::DetachDeviceAddressSpace: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Maps a device address space by force.
pub fn map_device_address_space_by_force(
    _das_handle: Handle,
    _process_handle: Handle,
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

    // TODO: Get DAS and process from handles, validate range, map
    log::warn!("svc::MapDeviceAddressSpaceByForce: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Maps a device address space with alignment check.
pub fn map_device_address_space_aligned(
    _das_handle: Handle,
    _process_handle: Handle,
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

    // TODO: Get DAS and process from handles, validate range, map aligned
    log::warn!("svc::MapDeviceAddressSpaceAligned: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Unmaps a device address space.
pub fn unmap_device_address_space(
    _das_handle: Handle,
    _process_handle: Handle,
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

    // TODO: Get DAS and process from handles, validate range, unmap
    log::warn!("svc::UnmapDeviceAddressSpace: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}
