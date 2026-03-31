// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `ir_opt/global_memory_to_storage_buffer_pass.cpp`
//!
//! Converts global memory accesses to storage buffer accesses.
//! This pass identifies constant buffer addresses used as SSBO descriptors
//! and rewrites global load/store instructions to use indexed storage
//! buffer operations instead.

use crate::host_translate_info::HostTranslateInfo;
use crate::ir::program::{Program, ShaderInfo};

/// Address in constant buffers to the storage buffer descriptor.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct StorageBufferAddr {
    index: u32,
    offset: u32,
}

/// Convert global memory instructions to storage buffer instructions.
///
/// Not yet implemented: requires tracking constant-buffer-derived addresses
/// and rewriting global load/store opcodes to storage buffer ops.
pub fn global_memory_to_storage_buffer_pass(
    _program: &mut Program,
    _host_info: &HostTranslateInfo,
) {
    log::warn!(
        "GlobalMemoryToStorageBufferPass not yet implemented — global memory ops left as-is"
    );
}

/// Join storage buffer descriptors from `source` into `base`.
///
/// Upstream: `JoinStorageInfo` in `global_memory_to_storage_buffer_pass.cpp`.
///
/// Not yet implemented: requires the full storage descriptor tracking infrastructure.
pub fn join_storage_info(_base: &mut ShaderInfo, _source: &mut ShaderInfo) {
    log::warn!("JoinStorageInfo not yet implemented");
}
