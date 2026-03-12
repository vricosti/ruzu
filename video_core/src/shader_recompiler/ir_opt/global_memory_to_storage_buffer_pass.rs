// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `ir_opt/global_memory_to_storage_buffer_pass.cpp`
//!
//! Converts global memory accesses to storage buffer accesses.
//! This pass identifies constant buffer addresses used as SSBO descriptors
//! and rewrites global load/store instructions to use indexed storage
//! buffer operations instead.

use crate::shader_recompiler::host_translate_info::HostTranslateInfo;
use crate::shader_recompiler::ir::program::Program;

/// Address in constant buffers to the storage buffer descriptor.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct StorageBufferAddr {
    index: u32,
    offset: u32,
}

/// Convert global memory instructions to storage buffer instructions.
pub fn global_memory_to_storage_buffer_pass(
    _program: &mut Program,
    _host_info: &HostTranslateInfo,
) {
    todo!("GlobalMemoryToStorageBufferPass: rewrite global memory ops to storage buffer ops")
}
