// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/framebuffer_config.h and video_core/framebuffer_config.cpp
//!
//! Framebuffer configuration for the display compositor.

/// Device address type.
pub type DAddr = u64;

/// Blend mode for framebuffer compositing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BlendMode {
    #[default]
    Opaque,
    Premultiplied,
    Coverage,
}

/// Buffer transform flags (matching android BufferTransformFlags).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct BufferTransformFlags(pub u32);

impl BufferTransformFlags {
    pub const FLIP_H: Self = Self(0x01);
    pub const FLIP_V: Self = Self(0x02);

    pub fn contains(&self, flag: Self) -> bool {
        self.0 & flag.0 != 0
    }

    pub fn remove(&mut self, flag: Self) {
        self.0 &= !flag.0;
    }

    pub fn is_empty(&self) -> bool {
        self.0 == 0
    }
}

/// Android pixel format (stub; full definition in nvnflinger).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct AndroidPixelFormat(pub u32);

/// A rectangle with integer coordinates.
#[derive(Debug, Clone, Copy, Default)]
pub struct RectI {
    pub left: i32,
    pub top: i32,
    pub right: i32,
    pub bottom: i32,
}

impl RectI {
    pub fn is_empty(&self) -> bool {
        self.left == 0 && self.top == 0 && self.right == 0 && self.bottom == 0
    }
}

/// A rectangle with float coordinates.
#[derive(Debug, Clone, Copy, Default)]
pub struct RectF {
    pub left: f32,
    pub top: f32,
    pub right: f32,
    pub bottom: f32,
}

/// Framebuffer configuration descriptor.
#[derive(Debug, Clone, Default)]
pub struct FramebufferConfig {
    pub address: DAddr,
    pub offset: u32,
    pub width: u32,
    pub height: u32,
    pub stride: u32,
    pub pixel_format: AndroidPixelFormat,
    pub transform_flags: BufferTransformFlags,
    pub crop_rect: RectI,
    pub blending: BlendMode,
}

/// Normalize the crop rectangle to [0..1] UV coordinates.
///
/// Applies horizontal/vertical flip based on transform flags.
pub fn normalize_crop(
    framebuffer: &FramebufferConfig,
    texture_width: u32,
    texture_height: u32,
) -> RectF {
    let (mut left, mut top, mut right, mut bottom);

    if !framebuffer.crop_rect.is_empty() {
        left = framebuffer.crop_rect.left as f32;
        top = framebuffer.crop_rect.top as f32;
        right = framebuffer.crop_rect.right as f32;
        bottom = framebuffer.crop_rect.bottom as f32;
    } else {
        left = 0.0;
        top = 0.0;
        right = framebuffer.width as f32;
        bottom = framebuffer.height as f32;
    }

    let mut transform_flags = framebuffer.transform_flags;

    if transform_flags.contains(BufferTransformFlags::FLIP_H) {
        std::mem::swap(&mut left, &mut right);
    }
    if transform_flags.contains(BufferTransformFlags::FLIP_V) {
        std::mem::swap(&mut top, &mut bottom);
    }

    transform_flags.remove(BufferTransformFlags::FLIP_H);
    transform_flags.remove(BufferTransformFlags::FLIP_V);
    if !transform_flags.is_empty() {
        log::warn!(
            "Unsupported framebuffer_transform_flags={}",
            transform_flags.0
        );
    }

    let tw = texture_width as f32;
    let th = texture_height as f32;

    RectF {
        left: left / tw,
        top: top / th,
        right: right / tw,
        bottom: bottom / th,
    }
}
