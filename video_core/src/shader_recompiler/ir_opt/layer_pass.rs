// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `ir_opt/layer_pass.cpp`
//!
//! Emulates gl_Layer output on GPUs that don't support setting Layer
//! from non-geometry shader stages. Rewrites Layer attribute stores
//! to use an unused generic attribute instead.

use crate::shader_recompiler::host_translate_info::HostTranslateInfo;
use crate::shader_recompiler::ir::program::Program;

/// Emulate layer output by redirecting Layer writes to a generic attribute.
pub fn layer_pass(_program: &mut Program, _host_info: &HostTranslateInfo) {
    todo!("LayerPass: emulate gl_Layer on non-geometry stages")
}
