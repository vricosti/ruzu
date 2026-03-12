// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/engines/const_buffer_info.h
//!
//! Describes a bound constant buffer slot used by shader stages.

/// GPU virtual address type.
pub type GPUVAddr = u64;

/// Information about a single constant buffer binding.
#[derive(Debug, Clone, Copy, Default)]
pub struct ConstBufferInfo {
    /// GPU virtual address of the constant buffer.
    pub address: GPUVAddr,
    /// Size of the constant buffer in bytes.
    pub size: u32,
    /// Whether this constant buffer slot is enabled.
    pub enabled: bool,
}
