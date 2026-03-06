// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use log::debug;
use common::{ResultCode, VAddr};

/// SVC 0x2A: FlushEntireDataCache
///
/// No-op in emulation — host CPU caches are coherent.
pub fn svc_flush_entire_data_cache() -> ResultCode {
    debug!("FlushEntireDataCache: no-op");
    ResultCode::SUCCESS
}

/// SVC 0x2B: FlushDataCache
///
/// No-op in emulation — host CPU caches are coherent.
pub fn svc_flush_data_cache(_addr: VAddr, _size: u64) -> ResultCode {
    debug!("FlushDataCache: no-op");
    ResultCode::SUCCESS
}
