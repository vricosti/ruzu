// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: Copyright 2014 The Android Open Source Project
// SPDX-License-Identifier: GPL-3.0-or-later
// Parts of this implementation were based on:
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/include/gui/IConsumerListener.h

//! Port of zuyu/src/core/hle/service/nvnflinger/consumer_listener.h

use super::buffer_item::BufferItem;

/// ConsumerListener is the interface through which the BufferQueue notifies the consumer of events
/// that the consumer may wish to react to.
pub trait IConsumerListener: Send + Sync {
    fn on_frame_available(&self, item: &BufferItem);
    fn on_frame_replaced(&self, item: &BufferItem);
    fn on_buffers_released(&self);
    fn on_sideband_stream_changed(&self);
}
