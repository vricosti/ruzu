//! Port of zuyu/src/core/hle/kernel/svc/svc_debug_string.cpp
//! Status: COMPLET (stubs for kernel memory access)
//! Derniere synchro: 2026-03-11
//!
//! SVC handler for outputting debug strings.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Used to output a message on a debug hardware unit — does nothing on a retail unit.
pub fn output_debug_string(address: u64, len: u64) -> ResultCode {
    if len == 0 {
        return RESULT_SUCCESS;
    }

    // TODO: GetCurrentMemory(kernel).ReadBlock(address, str.data(), str.size())
    // TODO: LOG_INFO(Debug_Emulated, "{}", str)
    log::info!(
        "svc::OutputDebugString called, address=0x{:X}, len={}",
        address, len
    );

    RESULT_SUCCESS
}
