// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use log::warn;
use common::ResultCode;
use ruzu_cpu::CpuState;

/// SVC 0x7F: CallSecureMonitor
///
/// Stub — writes 0 to all output registers and returns success.
/// Full implementation would handle SMC calls for key generation, etc.
pub fn svc_call_secure_monitor(cpu: &mut CpuState) {
    warn!(
        "CallSecureMonitor: x0=0x{:X}, x1=0x{:X} (stub)",
        cpu.x[0], cpu.x[1]
    );
    // Clear output registers (x0-x7).
    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
    for i in 1..8 {
        cpu.x[i] = 0;
    }
}
