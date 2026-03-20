//! Port of zuyu/src/core/hle/kernel/svc/svc_device_address_space.cpp
//! Status: Ported
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for device address space operations.

use crate::core::System;
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

    // Allocate a unique object ID for the device address space.
    static NEXT_DAS_ID: std::sync::atomic::AtomicU64 =
        std::sync::atomic::AtomicU64::new(0xE000_0000);
    let das_id = NEXT_DAS_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

    // Add to handle table.
    match process.handle_table.add(das_id) {
        Ok(h) => {
            *out = h;
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
    _device_name: DeviceName,
    das_handle: Handle,
) -> ResultCode {
    let process = system.current_process_arc().lock().unwrap();

    // Validate the handle.
    let _object_id = match process.handle_table.get_object(das_handle) {
        Some(id) => id,
        None => return RESULT_INVALID_HANDLE,
    };

    // Upstream: das->Attach(device_name)
    // Device address space attachment is a no-op in the emulator (no real SMMU).
    RESULT_SUCCESS
}

/// Detaches a device address space.
///
/// Upstream: Gets KDeviceAddressSpace from handle, calls das->Detach(device_name).
pub fn detach_device_address_space(
    system: &System,
    _device_name: DeviceName,
    das_handle: Handle,
) -> ResultCode {
    let process = system.current_process_arc().lock().unwrap();

    // Validate the handle.
    let _object_id = match process.handle_table.get_object(das_handle) {
        Some(id) => id,
        None => return RESULT_INVALID_HANDLE,
    };

    // Upstream: das->Detach(device_name)
    // Device address space detachment is a no-op in the emulator.
    RESULT_SUCCESS
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

    let process = system.current_process_arc().lock().unwrap();

    // Validate handles.
    if process.handle_table.get_object(das_handle).is_none() {
        return RESULT_INVALID_HANDLE;
    }
    if process_handle != crate::hle::kernel::svc_common::PseudoHandle::CurrentProcess as Handle
        && process.handle_table.get_object(process_handle).is_none()
    {
        return RESULT_INVALID_HANDLE;
    }

    // Validate that the process address is within range.
    let addr_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(process_address);
    if !process.page_table.contains(addr_kpa, size as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // Upstream: das->MapByForce(&page_table, process_address, size, device_address, option)
    // Device address space mapping is a no-op in the emulator (no real SMMU).
    RESULT_SUCCESS
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

    let process = system.current_process_arc().lock().unwrap();

    // Validate handles.
    if process.handle_table.get_object(das_handle).is_none() {
        return RESULT_INVALID_HANDLE;
    }
    if process_handle != crate::hle::kernel::svc_common::PseudoHandle::CurrentProcess as Handle
        && process.handle_table.get_object(process_handle).is_none()
    {
        return RESULT_INVALID_HANDLE;
    }

    // Validate that the process address is within range.
    let addr_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(process_address);
    if !process.page_table.contains(addr_kpa, size as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // Upstream: das->MapAligned(&page_table, process_address, size, device_address, option)
    // Device address space mapping is a no-op in the emulator.
    RESULT_SUCCESS
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

    let process = system.current_process_arc().lock().unwrap();

    // Validate handles.
    if process.handle_table.get_object(das_handle).is_none() {
        return RESULT_INVALID_HANDLE;
    }
    if process_handle != crate::hle::kernel::svc_common::PseudoHandle::CurrentProcess as Handle
        && process.handle_table.get_object(process_handle).is_none()
    {
        return RESULT_INVALID_HANDLE;
    }

    // Validate that the process address is within range.
    let addr_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(process_address);
    if !process.page_table.contains(addr_kpa, size as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // Upstream: das->Unmap(&page_table, process_address, size, device_address)
    // Device address space unmapping is a no-op in the emulator.
    RESULT_SUCCESS
}
