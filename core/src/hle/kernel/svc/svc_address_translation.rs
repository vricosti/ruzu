//! Port of zuyu/src/core/hle/kernel/svc/svc_address_translation.cpp
//! Status: COMPLET (stubs matching upstream UNIMPLEMENTED)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for physical address queries and IO mapping.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::result::ResultCode;

/// Queries the physical address for a given virtual address. (Unimplemented upstream.)
pub fn query_physical_address(_out_info: &mut PhysicalMemoryInfo64, _address: u64) -> ResultCode {
    log::warn!("svc::QueryPhysicalAddress: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

/// Queries an IO mapping. (Unimplemented upstream.)
pub fn query_io_mapping(
    _out_address: &mut u64,
    _out_size: &mut u64,
    _physical_address: u64,
    _size: u64,
) -> ResultCode {
    log::warn!("svc::QueryIoMapping: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}
