// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: Copyright 2014 The Android Open Source Project
// SPDX-License-Identifier: GPL-3.0-or-later
// Parts of this implementation were based on:
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/include/gui/BufferQueueDefs.h

//! Port of zuyu/src/core/hle/service/nvnflinger/buffer_queue_defs.h

use super::buffer_slot::BufferSlot;

/// BufferQueue will keep track of at most this value of buffers.
pub const NUM_BUFFER_SLOTS: usize = 64;

pub type SlotsType = [BufferSlot; NUM_BUFFER_SLOTS];

/// Helper to create default-initialized slots array.
pub fn new_slots() -> Box<SlotsType> {
    let mut slots: Box<SlotsType> = Box::new(std::array::from_fn(|_| BufferSlot::default()));
    slots
}
