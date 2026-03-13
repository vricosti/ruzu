// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `ir_opt/texture_pass.cpp`
//!
//! Resolves bindless and bound texture operations to indexed texture
//! operations. Tracks constant buffer addresses that reference texture
//! descriptors and rewrites texture instructions with concrete descriptor
//! indices.

use crate::host_translate_info::HostTranslateInfo;
use crate::ir::program::{Program, ShaderInfo};

/// Resolve bindless/bound texture operations to indexed operations.
///
/// Not yet implemented: requires tracking constant-buffer-derived texture
/// handles and rewriting texture opcodes with concrete descriptor indices.
pub fn texture_pass(_program: &mut Program, _host_info: &HostTranslateInfo) {
    log::warn!("TexturePass not yet implemented — texture handles left as-is");
}

/// Join texture descriptors from `source` into `base`.
///
/// Upstream: `JoinTextureInfo` in `texture_pass.cpp`.
///
/// Not yet implemented: requires the full texture descriptor tracking infrastructure.
pub fn join_texture_info(_base: &mut ShaderInfo, _source: &mut ShaderInfo) {
    log::warn!("JoinTextureInfo not yet implemented");
}
