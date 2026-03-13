// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `ir_opt/layer_pass.cpp`
//!
//! Emulates gl_Layer output on GPUs that don't support setting Layer
//! from non-geometry shader stages. Rewrites Layer attribute stores
//! to use an unused generic attribute instead.

use crate::host_translate_info::HostTranslateInfo;
use crate::ir::program::Program;

/// Emulate layer output by redirecting Layer writes to a generic attribute.
///
/// Not yet implemented: requires attribute/varying state tracking and instruction mutation.
pub fn layer_pass(_program: &mut Program, _host_info: &HostTranslateInfo) {
    log::warn!("LayerPass not yet implemented — Layer attribute stores left as-is");
}
