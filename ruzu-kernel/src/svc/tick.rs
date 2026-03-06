// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use common::CNTFRQ_HZ;

/// SVC 0x1E: GetSystemTick
/// Returns the host monotonic time converted to Switch tick frequency (19.2 MHz).
pub fn svc_get_system_tick() -> u64 {
    use std::time::{SystemTime, UNIX_EPOCH};

    let elapsed = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();

    // Convert to Switch ticks: nanoseconds * 19.2MHz / 1GHz = ns * 19.2 / 1000
    let ns = elapsed.as_nanos() as u64;
    (ns as u128 * CNTFRQ_HZ as u128 / 1_000_000_000u128) as u64
}
