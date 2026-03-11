//! Port of zuyu/src/core/hle/kernel/svc/svc_insecure_memory.cpp
//! Status: COMPLET (stubs matching upstream UNIMPLEMENTED)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for insecure memory mapping.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::result::ResultCode;

/// Maps insecure memory. (Unimplemented upstream.)
pub fn map_insecure_memory(_address: u64, _size: u64) -> ResultCode {
    log::warn!("svc::MapInsecureMemory: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

/// Unmaps insecure memory. (Unimplemented upstream.)
pub fn unmap_insecure_memory(_address: u64, _size: u64) -> ResultCode {
    log::warn!("svc::UnmapInsecureMemory: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}
