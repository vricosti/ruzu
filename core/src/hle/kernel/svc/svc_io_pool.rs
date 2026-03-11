//! Port of zuyu/src/core/hle/kernel/svc/svc_io_pool.cpp
//! Status: COMPLET (stubs matching upstream UNIMPLEMENTED)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for IO pool and IO region operations.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::ResultCode;

/// Creates an IO pool. (Unimplemented upstream.)
pub fn create_io_pool(_out: &mut Handle, _pool_type: u32) -> ResultCode {
    log::warn!("svc::CreateIoPool: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

/// Creates an IO region. (Unimplemented upstream.)
pub fn create_io_region(
    _out: &mut Handle,
    _io_pool_handle: Handle,
    _phys_addr: u64,
    _size: u64,
    _mapping: MemoryMapping,
    _perm: MemoryPermission,
) -> ResultCode {
    log::warn!("svc::CreateIoRegion: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

/// Maps an IO region. (Unimplemented upstream.)
pub fn map_io_region(
    _io_region_handle: Handle,
    _address: u64,
    _size: u64,
    _map_perm: MemoryPermission,
) -> ResultCode {
    log::warn!("svc::MapIoRegion: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

/// Unmaps an IO region. (Unimplemented upstream.)
pub fn unmap_io_region(_io_region_handle: Handle, _address: u64, _size: u64) -> ResultCode {
    log::warn!("svc::UnmapIoRegion: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}
