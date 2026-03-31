//! Port of zuyu/src/core/hle/kernel/svc/svc_cache.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for data cache operations.

use crate::core::System;
use crate::hle::kernel::k_typed_address::KProcessAddress;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Flushes the entire data cache.
/// Upstream: UNIMPLEMENTED() — intentionally unimplemented.
pub fn flush_entire_data_cache() {
    log::warn!("svc::FlushEntireDataCache: Upstream UNIMPLEMENTED");
}

/// Flushes a range of the data cache.
/// Upstream: UNIMPLEMENTED() — intentionally unimplemented.
pub fn flush_data_cache(_address: u64, _size: u64) -> ResultCode {
    log::warn!("svc::FlushDataCache: Upstream UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

/// Invalidates process data cache.
/// Upstream: UNIMPLEMENTED() — intentionally unimplemented.
pub fn invalidate_process_data_cache(
    _process_handle: Handle,
    _address: u64,
    _size: u64,
) -> ResultCode {
    log::warn!("svc::InvalidateProcessDataCache: Upstream UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

/// Stores process data cache.
/// Upstream: UNIMPLEMENTED() — intentionally unimplemented.
pub fn store_process_data_cache(_process_handle: Handle, _address: u64, _size: u64) -> ResultCode {
    log::warn!("svc::StoreProcessDataCache: Upstream UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

/// Flushes process data cache.
///
/// Validates address/size, gets the process from its handle, verifies the region
/// is within range, then performs the flush.
pub fn flush_process_data_cache(
    system: &System,
    process_handle: Handle,
    address: u64,
    size: u64,
) -> ResultCode {
    // Validate address/size.
    if size == 0 {
        return RESULT_INVALID_SIZE;
    }
    // Upstream checks address == static_cast<uint64_t>(address) and same for size,
    // which is always true for u64 inputs. Included for parity:
    // R_UNLESS(address == static_cast<uint64_t>(address), ResultInvalidCurrentMemory);
    // R_UNLESS(size == static_cast<uint64_t>(size), ResultInvalidCurrentMemory);

    // Get the process from its handle.
    // Upstream: GetCurrentProcess(kernel).GetHandleTable().GetObject<KProcess>(process_handle)
    let process = system.current_process_arc().lock().unwrap();
    let Some(object_id) = process.handle_table.get_object(process_handle) else {
        return RESULT_INVALID_HANDLE;
    };

    // Verify the region is within range.
    // Upstream: page_table.Contains(address, size)
    if !process
        .page_table
        .contains(KProcessAddress::new(address), size as usize)
    {
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // Perform the operation.
    // Upstream: GetCurrentMemory(kernel).FlushDataCache(address, size)
    // This is a no-op on HLE since we don't have real data caches to flush.
    // The upstream implementation calls into Memory::FlushDataCache which
    // is effectively a no-op on x86/host.
    log::debug!(
        "svc::FlushProcessDataCache: flushing address=0x{:X}, size=0x{:X} (no-op on HLE)",
        address,
        size
    );

    RESULT_SUCCESS
}
