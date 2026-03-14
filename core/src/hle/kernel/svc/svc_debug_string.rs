//! Port of zuyu/src/core/hle/kernel/svc/svc_debug_string.cpp
//! Status: COMPLET (stubs for kernel memory access)
//! Derniere synchro: 2026-03-11
//!
//! SVC handler for outputting debug strings.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc_dispatch::SvcContext;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Used to output a message on a debug hardware unit — does nothing on a retail unit.
pub fn output_debug_string(ctx: &SvcContext, address: u64, len: u64) -> ResultCode {
    if len == 0 {
        return RESULT_SUCCESS;
    }

    let len = len as usize;
    let msg = {
        let mem = ctx.shared_memory.read().unwrap();
        if mem.is_valid_range(address, len) {
            String::from_utf8_lossy(mem.read_block(address, len)).to_string()
        } else {
            String::new()
        }
    };
    log::info!("OutputDebugString(addr=0x{:X}, len={}): {}", address, len, msg);

    RESULT_SUCCESS
}
