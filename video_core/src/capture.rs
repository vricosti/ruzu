// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/capture.h
//!
//! Constants for frame capture configuration.

/// Block height for capture tiling.
pub const BLOCK_HEIGHT: u32 = 4;

/// Block depth for capture tiling.
pub const BLOCK_DEPTH: u32 = 0;

/// Log2 of bytes per pixel.
pub const BPP_LOG2: u32 = 2;

// PixelFormat: B8G8R8A8_UNORM (referenced from surface module)
// pub const PIXEL_FORMAT: PixelFormat = PixelFormat::B8G8R8A8Unorm;

/// Linear width (undocked screen width = 1280).
pub const LINEAR_WIDTH: u32 = 1280;

/// Linear height (undocked screen height = 720).
pub const LINEAR_HEIGHT: u32 = 720;

/// Linear depth.
pub const LINEAR_DEPTH: u32 = 1;

/// Bytes per pixel.
pub const BYTES_PER_PIXEL: u32 = 4;

/// Tiled width matches linear width.
pub const TILED_WIDTH: u32 = LINEAR_WIDTH;

/// Tiled height aligned to block parameters.
pub fn tiled_height() -> u32 {
    align_up_log2(LINEAR_HEIGHT, BLOCK_HEIGHT + BLOCK_DEPTH + BPP_LOG2)
}

/// Total tiled capture size in bytes.
pub fn tiled_size() -> u32 {
    TILED_WIDTH * tiled_height() * (1 << BPP_LOG2)
}

/// Align `value` up by `align_log2` bits (matching Common::AlignUpLog2).
fn align_up_log2(value: u32, align_log2: u32) -> u32 {
    let mask = (1u32 << align_log2) - 1;
    (value + mask) & !mask
}

/// Capture framebuffer layout.
pub struct CaptureLayout {
    pub width: u32,
    pub height: u32,
    pub screen_left: u32,
    pub screen_top: u32,
    pub screen_right: u32,
    pub screen_bottom: u32,
    pub is_srgb: bool,
}

/// Default capture layout matching upstream.
pub const CAPTURE_LAYOUT: CaptureLayout = CaptureLayout {
    width: LINEAR_WIDTH,
    height: LINEAR_HEIGHT,
    screen_left: 0,
    screen_top: 0,
    screen_right: LINEAR_WIDTH,
    screen_bottom: LINEAR_HEIGHT,
    is_srgb: false,
};
