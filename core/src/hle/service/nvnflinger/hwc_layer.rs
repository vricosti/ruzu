// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvnflinger/hwc_layer.h

use common::math_util::Rectangle;

use super::buffer_transform_flags::BufferTransformFlags;
use super::pixel_format::PixelFormat;
use super::ui::fence::Fence;

/// hwc_layer_t::blending values
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LayerBlending {
    /// No blending
    None = 0x100,
    /// ONE / ONE_MINUS_SRC_ALPHA
    Premultiplied = 0x105,
    /// SRC_ALPHA / ONE_MINUS_SRC_ALPHA
    Coverage = 0x405,
}

impl Default for LayerBlending {
    fn default() -> Self {
        LayerBlending::None
    }
}

pub struct HwcLayer {
    pub buffer_handle: u32,
    pub offset: u32,
    pub format: PixelFormat,
    pub width: u32,
    pub height: u32,
    pub stride: u32,
    pub z_index: i32,
    pub blending: LayerBlending,
    pub transform: BufferTransformFlags,
    pub crop_rect: Rectangle<i32>,
    pub acquire_fence: Fence,
}
