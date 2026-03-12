// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `shader_recompiler/host_translate_info.h`
//!
//! Miscellaneous information about the host GPU that affects shader translation.

/// Misc information about the host GPU/driver.
///
/// Try to keep entries here to a minimum -- they can accidentally change
/// the cached information in a shader.
#[derive(Debug, Clone, Default)]
pub struct HostTranslateInfo {
    /// True when the device supports 64-bit floats.
    pub support_float64: bool,
    /// True when the device supports 16-bit floats.
    pub support_float16: bool,
    /// True when the device supports 64-bit integers.
    pub support_int64: bool,
    /// True when the device needs DemoteToHelperInvocation reordered.
    pub needs_demote_reorder: bool,
    /// True when the device supports SNORM render buffers.
    pub support_snorm_render_buffer: bool,
    /// True when the device supports gl_Layer in VS.
    pub support_viewport_index_layer: bool,
    /// Minimum alignment supported by the device for SSBOs.
    pub min_ssbo_alignment: u32,
    /// True when the device supports geometry passthrough shaders.
    pub support_geometry_shader_passthrough: bool,
    /// True when the device supports barriers in conditional control flow.
    pub support_conditional_barrier: bool,
}
