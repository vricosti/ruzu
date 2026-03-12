// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: Copyright 2014 The Android Open Source Project
// SPDX-License-Identifier: GPL-3.0-or-later
// Parts of this implementation were based on:
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/include/gui/BufferItem.h

//! Port of zuyu/src/core/hle/service/nvnflinger/buffer_item.h

use std::sync::Arc;

use common::math_util::Rectangle;

use super::ui::fence::Fence;
use super::ui::graphic_buffer::GraphicBuffer;
use super::window::NativeWindowTransform;

pub struct BufferItem {
    pub graphic_buffer: Option<Arc<GraphicBuffer>>,
    pub fence: Fence,
    pub crop: Rectangle<i32>,
    pub transform: NativeWindowTransform,
    pub scaling_mode: u32,
    pub timestamp: i64,
    pub is_auto_timestamp: bool,
    pub frame_number: u64,

    /// The slot index, or INVALID_BUFFER_SLOT if not assigned.
    pub slot: i32,

    pub is_droppable: bool,
    pub acquire_called: bool,
    pub transform_to_display_inverse: bool,
    pub swap_interval: i32,
}

impl BufferItem {
    /// The default value for slot, used to indicate this doesn't correspond to a slot.
    pub const INVALID_BUFFER_SLOT: i32 = -1;
}

impl Default for BufferItem {
    fn default() -> Self {
        Self {
            graphic_buffer: None,
            fence: Fence::default(),
            crop: Rectangle::default(),
            transform: NativeWindowTransform::empty(),
            scaling_mode: 0,
            timestamp: 0,
            is_auto_timestamp: false,
            frame_number: 0,
            slot: Self::INVALID_BUFFER_SLOT,
            is_droppable: false,
            acquire_called: false,
            transform_to_display_inverse: false,
            swap_interval: 0,
        }
    }
}
