//! Port of zuyu/src/core/hle/kernel/svc/svc_register.cpp
//! Status: COMPLET (stubs matching upstream UNIMPLEMENTED)
//! Derniere synchro: 2026-03-11
//!
//! SVC handler for ReadWriteRegister.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::result::ResultCode;

/// Reads/writes a hardware register. (Unimplemented upstream.)
pub fn read_write_register(
    out: &mut u32,
    _address: u64,
    _mask: u32,
    _value: u32,
) -> ResultCode {
    *out = 0;
    log::warn!("svc::ReadWriteRegister: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}
