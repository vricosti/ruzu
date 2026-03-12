// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `ir_opt/texture_pass.cpp`
//!
//! Resolves bindless and bound texture operations to indexed texture
//! operations. Tracks constant buffer addresses that reference texture
//! descriptors and rewrites texture instructions with concrete descriptor
//! indices.

use crate::shader_recompiler::host_translate_info::HostTranslateInfo;
use crate::shader_recompiler::ir::program::Program;

/// Resolve bindless/bound texture operations to indexed operations.
pub fn texture_pass(_program: &mut Program, _host_info: &HostTranslateInfo) {
    todo!("TexturePass: resolve bindless textures to indexed descriptors")
}
