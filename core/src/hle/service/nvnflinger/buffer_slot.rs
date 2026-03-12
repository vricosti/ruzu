// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: Copyright 2014 The Android Open Source Project
// SPDX-License-Identifier: GPL-3.0-or-later
// Parts of this implementation were based on:
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/include/gui/BufferSlot.h

//! Port of zuyu/src/core/hle/service/nvnflinger/buffer_slot.h

use std::sync::Arc;

use super::ui::fence::Fence;
use super::ui::graphic_buffer::GraphicBuffer;

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BufferState {
    Free = 0,
    Dequeued = 1,
    Queued = 2,
    Acquired = 3,
}

impl Default for BufferState {
    fn default() -> Self {
        BufferState::Free
    }
}

pub struct BufferSlot {
    pub graphic_buffer: Option<Arc<GraphicBuffer>>,
    pub buffer_state: BufferState,
    pub request_buffer_called: bool,
    pub frame_number: u64,
    pub fence: Fence,
    pub acquire_called: bool,
    pub needs_cleanup_on_release: bool,
    pub attached_by_consumer: bool,
    pub is_preallocated: bool,
}

impl Default for BufferSlot {
    fn default() -> Self {
        Self {
            graphic_buffer: None,
            buffer_state: BufferState::Free,
            request_buffer_called: false,
            frame_number: 0,
            fence: Fence::default(),
            acquire_called: false,
            needs_cleanup_on_release: false,
            attached_by_consumer: false,
            is_preallocated: false,
        }
    }
}
