// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

/// SVC 0x10: GetCurrentProcessorNumber
///
/// Returns the ID of the core the calling thread is running on.
/// Always returns 0 in single-core emulation mode.
pub fn svc_get_current_processor_number() -> u32 {
    0
}
