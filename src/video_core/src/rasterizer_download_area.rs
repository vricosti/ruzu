// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/rasterizer_download_area.h
//!
//! Describes a region of GPU memory to be read back to the CPU.

/// Virtual address type.
pub type VAddr = u64;

/// Describes a download (read-back) region.
#[derive(Debug, Clone, Copy)]
pub struct RasterizerDownloadArea {
    pub start_address: VAddr,
    pub end_address: VAddr,
    /// When true, this area was preemptively flushed.
    pub preemtive: bool,
}

impl Default for RasterizerDownloadArea {
    fn default() -> Self {
        Self {
            start_address: 0,
            end_address: 0,
            preemtive: false,
        }
    }
}
