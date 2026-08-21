// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: Copyright 2010 The Android Open Source Project
// SPDX-License-Identifier: GPL-3.0-or-later
// Parts of this implementation were based on:
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/include/gui/IGraphicBufferProducer.h

//! Port of zuyu/src/core/hle/service/nvnflinger/graphic_buffer_producer.h
//! Port of zuyu/src/core/hle/service/nvnflinger/graphic_buffer_producer.cpp

use common::math_util::Rectangle;

use super::ui::fence::Fence;
use super::window::{NativeWindowScalingMode, NativeWindowTransform};

/// Input data for QueueBuffer operation.
///
/// Note: This struct is read as a flat binary blob from the parcel,
/// so its layout must match upstream exactly. We use repr(C, packed)
/// to match the upstream `#pragma pack(push, 1)`.
#[repr(C, packed)]
#[derive(Debug, Clone, Copy)]
pub struct QueueBufferInput {
    pub timestamp: i64,
    pub is_auto_timestamp: i32,
    pub crop: Rectangle<i32>,
    pub scaling_mode: NativeWindowScalingMode,
    pub transform: NativeWindowTransform,
    pub sticky_transform: u32,
    pub async_flag: i32,
    pub swap_interval: i32,
    pub fence: Fence,
}
const _: () = assert!(std::mem::size_of::<QueueBufferInput>() == 84);

impl Default for QueueBufferInput {
    fn default() -> Self {
        // Safety: All fields are integer/flag/fence types where zero initialization is valid.
        unsafe { std::mem::zeroed() }
    }
}

impl QueueBufferInput {
    /// Deflate the input into individual fields, matching upstream Deflate().
    pub fn deflate(
        &self,
    ) -> (
        i64,                     // timestamp
        bool,                    // is_auto_timestamp
        Rectangle<i32>,          // crop
        NativeWindowScalingMode, // scaling_mode
        NativeWindowTransform,   // transform
        u32,                     // sticky_transform
        bool,                    // async
        i32,                     // swap_interval
        Fence,                   // fence
    ) {
        (
            self.timestamp,
            self.is_auto_timestamp != 0,
            self.crop,
            self.scaling_mode,
            self.transform,
            self.sticky_transform,
            self.async_flag != 0,
            self.swap_interval,
            self.fence,
        )
    }
}

/// Output data from QueueBuffer operation.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct QueueBufferOutput {
    width: u32,
    height: u32,
    transform_hint: u32,
    num_pending_buffers: u32,
}
const _: () = assert!(std::mem::size_of::<QueueBufferOutput>() == 16);

impl QueueBufferOutput {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn deflate(&self) -> (u32, u32, u32, u32) {
        (
            self.width,
            self.height,
            self.transform_hint,
            self.num_pending_buffers,
        )
    }

    pub fn inflate(
        &mut self,
        width: u32,
        height: u32,
        transform_hint: u32,
        num_pending_buffers: u32,
    ) {
        self.width = width;
        self.height = height;
        self.transform_hint = transform_hint;
        self.num_pending_buffers = num_pending_buffers;
    }
}
