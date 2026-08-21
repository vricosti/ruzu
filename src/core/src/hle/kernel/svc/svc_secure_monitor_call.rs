//! Port of zuyu/src/core/hle/kernel/svc/svc_secure_monitor_call.cpp
//! Status: COMPLET (stubs matching upstream UNIMPLEMENTED)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for secure monitor calls (SMC).

use crate::hle::kernel::svc::svc_types::*;

/// Calls the secure monitor. (Unimplemented upstream.)
pub fn call_secure_monitor(_args: &mut SecureMonitorArguments64) {
    log::warn!("svc::CallSecureMonitor: UNIMPLEMENTED");
}

/// Custom ABI wrapper for CallSecureMonitor64.
pub fn svc_wrap_call_secure_monitor64(args: &mut [u64; 8]) {
    let mut smc_args = SecureMonitorArguments64::default();
    for i in 0..8 {
        smc_args.r[i] = args[i];
    }

    call_secure_monitor(&mut smc_args);

    for i in 0..8 {
        args[i] = smc_args.r[i];
    }
}

/// Custom ABI wrapper for CallSecureMonitor64From32.
pub fn svc_wrap_call_secure_monitor64_from32(args: &mut [u64; 8]) {
    let mut smc_args = SecureMonitorArguments32::default();
    for i in 0..8 {
        smc_args.r[i] = args[i] as u32;
    }

    // CallSecureMonitor64From32 is not supported upstream.
    log::warn!("svc::CallSecureMonitor64From32: UNIMPLEMENTED");

    for i in 0..8 {
        args[i] = smc_args.r[i] as u64;
    }
}
